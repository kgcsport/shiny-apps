# apps/excise-tax/app.R
# -----------------------------------------------------------------------
# Competitive Markets & Trade Policy -- classroom call-market experiment
#
# Based on the Experiencing Economics apple excise-tax experiment,
# abstracted to a nearly perfectly competitive imported commodity
# (default: soybeans).
#
# HOW IT WORKS
# -----------------------------------------------------------------------
# Roles assigned at join time:
#   Buyer            - domestic consumer; holds max willingness-to-pay
#   Seller (dom)     - domestic producer;  holds min acceptable price (cost)
#   Seller (imported)- foreign supplier;   cost = world price (flat)
#
# Each round is a "call market" (Walrasian auction):
#   1. Instructor opens the round (optionally with a tax/tariff)
#   2. Each player submits their price -- buyers a bid, sellers an ask
#   3. Instructor closes the round; market clears at a uniform price
#      where all bids >= asks (effective)
#   4. Results and surpluses are shown to everyone
#   5. Repeat with / without a tariff to illustrate incidence & DWL
#
# CLASSROOM FLOW
# -----------------------------------------------------------------------
# Instructor:  Create room -> write room code on board -> open/close rounds
# Students:    Go to app URL -> enter room code + name -> see private card
#
# Run locally:  R -e "shiny::runApp('apps/excise-tax')"
# -----------------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(shiny, DBI, RSQLite, jsonlite, digest)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

logf <- function(...) {
  cat(format(Sys.time(), "%H:%M:%S"), "-",
      paste(vapply(list(...), as.character, character(1)), collapse = " "),
      "\n", file = stderr())
}

# -----------------------------------------------------------------------
# DB setup  (same appdata convention as final_question_reveal)
# -----------------------------------------------------------------------

data_dir <- local({
  root <- if (dir.exists("/srv/shiny-server")) "/srv/shiny-server/appdata"
          else file.path(getwd(), "appdata")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root
})
DB_PATH <- file.path(data_dir, "excise-tax.sqlite")
logf("DB path:", DB_PATH)

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn))
    conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  conn
}
db_exec  <- function(sql, p = NULL) DBI::dbExecute(get_con(),  sql, params = p)
db_query <- function(sql, p = NULL) DBI::dbGetQuery(get_con(), sql, params = p)

init_db <- function() {
  # rooms: one row per classroom session
  db_exec("
    CREATE TABLE IF NOT EXISTS rooms (
      room_id        TEXT PRIMARY KEY,
      good_name      TEXT    DEFAULT 'soybeans',
      unit           TEXT    DEFAULT 'bushel',
      n_buyers       INTEGER DEFAULT 8,
      n_sellers_dom  INTEGER DEFAULT 4,
      n_sellers_imp  INTEGER DEFAULT 4,
      buyer_hi       REAL    DEFAULT 10,
      buyer_lo       REAL    DEFAULT 3,
      dom_cost_lo    REAL    DEFAULT 5,
      dom_cost_hi    REAL    DEFAULT 9,
      world_price    REAL    DEFAULT 2,
      current_round  INTEGER DEFAULT 1,
      instructor_pin TEXT,
      created_at     TEXT
    );
  ")

  # cards: one per player slot; player_token filled when student joins
  db_exec("
    CREATE TABLE IF NOT EXISTS cards (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      room_id       TEXT    NOT NULL,
      card_num      INTEGER NOT NULL,
      player_token  TEXT,
      display_name  TEXT,
      role          TEXT,
      origin        TEXT,
      private_value REAL,
      joined_at     TEXT,
      UNIQUE(room_id, card_num),
      UNIQUE(room_id, player_token)
    );
  ")

  # rounds: one per round per room
  db_exec("
    CREATE TABLE IF NOT EXISTS rounds (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      room_id         TEXT NOT NULL,
      round_num       INTEGER NOT NULL,
      status          TEXT DEFAULT 'pending',
      tax_amount      REAL DEFAULT 0,
      tax_type        TEXT DEFAULT 'none',
      clearing_price  REAL,
      quantity_traded INTEGER,
      results_json    TEXT,
      opened_at       TEXT,
      closed_at       TEXT,
      UNIQUE(room_id, round_num)
    );
  ")

  # orders: one per player per round (upsertable so students can revise)
  db_exec("
    CREATE TABLE IF NOT EXISTS orders (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      room_id         TEXT NOT NULL,
      round_num       INTEGER NOT NULL,
      player_token    TEXT NOT NULL,
      submitted_price REAL NOT NULL,
      submitted_at    TEXT,
      UNIQUE(room_id, round_num, player_token)
    );
  ")
}

init_db()

# -----------------------------------------------------------------------
# Token / code helpers
# -----------------------------------------------------------------------

gen_room_code    <- function() paste(sample(c(LETTERS, 0:9), 5, replace = TRUE), collapse = "")
gen_pin          <- function() sprintf("%04d", sample(1000:9999, 1))
gen_player_token <- function() paste0("p_", substr(digest::digest(runif(1)), 1, 10))

# -----------------------------------------------------------------------
# Card generation
# -----------------------------------------------------------------------

generate_cards <- function(n_buyers, n_dom, n_imp,
                            buyer_hi, buyer_lo,
                            dom_lo,   dom_hi,
                            world_price, seed = 42L) {
  set.seed(as.integer(seed))

  # Evenly-spaced values -> clean step supply/demand curves
  b_vals  <- round(seq(buyer_hi, buyer_lo, length.out = max(1L, n_buyers)), 2)
  d_costs <- round(seq(dom_lo,   dom_hi,   length.out = max(1L, n_dom)),    2)
  i_costs <- rep(round(world_price, 2), n_imp)

  df <- data.frame(
    role          = c(rep("buyer",  n_buyers), rep("seller", n_dom), rep("seller", n_imp)),
    origin        = c(rep("domestic", n_buyers + n_dom), rep("imported", n_imp)),
    private_value = c(b_vals, d_costs, i_costs),
    stringsAsFactors = FALSE
  )

  df          <- df[sample(nrow(df)), ]   # shuffle so roles aren't predictable by join order
  df$card_num <- seq_len(nrow(df))
  rownames(df) <- NULL
  df
}

insert_cards <- function(room_id, df) {
  for (i in seq_len(nrow(df))) {
    db_exec("
      INSERT OR IGNORE INTO cards(room_id, card_num, role, origin, private_value)
      VALUES(?, ?, ?, ?, ?);
    ", list(room_id, df$card_num[i], df$role[i], df$origin[i], df$private_value[i]))
  }
}

# -----------------------------------------------------------------------
# Market clearing  (Walrasian call market)
# -----------------------------------------------------------------------
# bids / asks: data.frames with columns player_token, submitted_price,
#              private_value, origin (asks only)
#
# Returns a list with clearing_price, quantity, buyer/seller_tokens,
# and surplus breakdown.
# -----------------------------------------------------------------------

market_clear <- function(bids, asks, tax_amount = 0, tax_type = "none") {
  empty <- list(clearing_price = NA_real_, quantity = 0L,
                buyer_tokens = character(0), seller_tokens = character(0),
                buyer_surplus = 0, seller_surplus = 0,
                gov_revenue = 0, total_surplus = 0)

  if (!nrow(bids) || !nrow(asks)) return(empty)

  # Effective ask = submitted price + any applicable tax
  asks$eff <- asks$submitted_price
  if (tax_type == "tariff") {
    asks$eff <- ifelse(asks$origin == "imported",
                       asks$submitted_price + tax_amount,
                       asks$submitted_price)
  } else if (tax_type == "excise") {
    asks$eff <- asks$submitted_price + tax_amount
  }

  # Sort: bids descending, asks ascending by effective price
  bids <- bids[order(-bids$submitted_price), , drop = FALSE]
  asks <- asks[order(asks$eff),              , drop = FALSE]

  # Find max k where bid_k >= effective ask_k
  n <- min(nrow(bids), nrow(asks))
  k <- 0L
  for (i in seq_len(n)) {
    if (bids$submitted_price[i] >= asks$eff[i]) k <- i else break
  }
  if (k == 0L) return(empty)

  # Uniform clearing price: midpoint of marginal bid and marginal effective ask
  cp <- round((bids$submitted_price[k] + asks$eff[k]) / 2, 2)

  b_tok <- bids$player_token[seq_len(k)]
  s_tok <- asks$player_token[seq_len(k)]

  # Surplus measured against private values (not submitted prices)
  bs <- sum(bids$private_value[seq_len(k)]  - cp, na.rm = TRUE)
  ps <- sum(cp - asks$private_value[seq_len(k)], na.rm = TRUE)

  gov <- if (tax_type == "tariff") {
    n_imp_traded <- sum(asks$origin[seq_len(k)] == "imported")
    tax_amount * n_imp_traded
  } else if (tax_type == "excise") {
    tax_amount * k
  } else 0

  list(
    clearing_price  = cp,
    quantity        = k,
    buyer_tokens    = b_tok,
    seller_tokens   = s_tok,
    buyer_surplus   = round(bs,  2),
    seller_surplus  = round(ps,  2),
    gov_revenue     = round(gov, 2),
    total_surplus   = round(bs + ps + gov, 2)
  )
}

# -----------------------------------------------------------------------
# Supply / demand step-curve data  (for plot)
# -----------------------------------------------------------------------

build_step_curves <- function(room_id, tax_amount = 0, tax_type = "none") {
  cards <- db_query(
    "SELECT role, origin, private_value FROM cards WHERE room_id = ?;",
    list(room_id))
  if (!nrow(cards)) return(NULL)

  buyers  <- cards[cards$role == "buyer",  , drop = FALSE]
  sellers <- cards[cards$role == "seller", , drop = FALSE]

  # Effective cost for sellers
  sellers$eff <- sellers$private_value
  if (tax_type == "tariff") {
    sellers$eff <- ifelse(sellers$origin == "imported",
                          sellers$private_value + tax_amount,
                          sellers$private_value)
  } else if (tax_type == "excise") {
    sellers$eff <- sellers$private_value + tax_amount
  }

  # Sort and index
  buyers  <- buyers[order(-buyers$private_value), , drop = FALSE]
  sellers <- sellers[order(sellers$eff),          , drop = FALSE]
  buyers$q  <- seq_len(nrow(buyers))
  sellers$q <- seq_len(nrow(sellers))

  list(demand = buyers, supply = sellers)
}

# -----------------------------------------------------------------------
# CSS helpers
# -----------------------------------------------------------------------

card_css <- "
  .role-card          { padding:1rem; border-radius:8px; border:2px solid #999; margin-bottom:1rem; }
  .buyer-card         { border-color:#1565C0; background:#E3F2FD; }
  .seller-dom-card    { border-color:#2E7D32; background:#E8F5E9; }
  .seller-imp-card    { border-color:#E65100; background:#FBE9E7; }
  .room-code          { font-size:2.5rem; font-family:monospace; font-weight:700; letter-spacing:0.15em; }
  .round-banner       { font-size:1.05rem; margin-bottom:0.5rem; }
  .surplus-row        { font-size:1.05rem; }
"

# -----------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML(card_css))),
  titlePanel("Competitive Markets & Trade Policy"),
  uiOutput("main_ui")
)

# -----------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    player_token  = NULL,
    room_id       = NULL,
    is_instructor = FALSE,
    view          = "landing"     # "landing" | "game"
  )

  ticker     <- reactiveTimer(2500)
  setup_open <- reactiveVal(FALSE)

  # Pre-fill room code from ?room= query param
  observeEvent(session$clientData$url_search, {
    q  <- parseQueryString(session$clientData$url_search)
    rc <- toupper(trimws(q[["room"]] %||% ""))
    if (nzchar(rc)) updateTextInput(session, "join_code", value = rc)
  }, once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Landing page
  output$main_ui <- renderUI({
    if (rv$view == "game") return(uiOutput("game_ui"))

    fluidPage(
      fluidRow(
        column(5,
          wellPanel(
            h4("Create a new room"),
            p(em("Instructor: configure parameters, create the room, then write the 5-letter code on the board.")),
            actionButton("show_setup_btn", "Set up a new room", class = "btn-success btn-lg")
          ),
          uiOutput("setup_panel")
        ),
        column(5, offset = 1,
          wellPanel(
            h4("Join a room"),
            textInput("join_code", "Room code:", placeholder = "e.g. A3BXQ"),
            textInput("join_name", "Your name:", placeholder = "e.g. Alex"),
            checkboxInput("join_as_inst", "I am the instructor (enter PIN to unlock)"),
            conditionalPanel(
              "input.join_as_inst",
              textInput("join_pin", "Instructor PIN:", placeholder = "4 digits")
            ),
            actionButton("join_btn", "Join", class = "btn-primary btn-lg"),
            tags$hr(),
            p(class = "text-muted", tags$small(
              "Students: enter the code your instructor wrote on the board and your first name.",
              "You will be assigned a private market role."
            ))
          )
        )
      )
    )
  })

  observeEvent(input$show_setup_btn, setup_open(TRUE))

  output$setup_panel <- renderUI({
    if (!setup_open()) return(NULL)
    wellPanel(
      h5("Room parameters"),
      fluidRow(
        column(4, textInput("cfg_good",  "Good name",  value = "soybeans")),
        column(4, textInput("cfg_unit",  "Unit",       value = "bushel")),
        column(4, numericInput("cfg_seed", "Seed (reproducibility)", value = 42, min = 1))
      ),
      fluidRow(
        column(4, numericInput("cfg_nb",  "# Buyers",           value = 8, min = 1)),
        column(4, numericInput("cfg_nd",  "# Domestic sellers", value = 4, min = 0)),
        column(4, numericInput("cfg_ni",  "# Imported sellers", value = 4, min = 0))
      ),
      fluidRow(
        column(3, numericInput("cfg_bhi", "Buyer WTP max ($)", value = 10, min = 1,   step = 0.5)),
        column(3, numericInput("cfg_blo", "Buyer WTP min ($)", value = 3,  min = 0,   step = 0.5)),
        column(3, numericInput("cfg_dlo", "Dom. cost min ($)", value = 5,  min = 0,   step = 0.5)),
        column(3, numericInput("cfg_dhi", "Dom. cost max ($)", value = 9,  min = 0,   step = 0.5))
      ),
      fluidRow(
        column(4, numericInput("cfg_wp", "World (import) price ($)", value = 2, min = 0, step = 0.25)),
        column(8, tags$div(style = "padding-top:8px;",
          tags$small(class = "text-muted",
            "World price is the cost for all imported sellers.",
            "A tariff raises their effective cost by the tariff amount."
          )
        ))
      ),
      actionButton("create_room_btn", "Create room & generate cards", class = "btn-primary")
    )
  })

  observeEvent(input$create_room_btn, {
    nb  <- as.integer(input$cfg_nb  %||% 8L)
    nd  <- as.integer(input$cfg_nd  %||% 4L)
    ni  <- as.integer(input$cfg_ni  %||% 4L)
    bhi <- as.numeric(input$cfg_bhi %||% 10)
    blo <- as.numeric(input$cfg_blo %||%  3)
    dlo <- as.numeric(input$cfg_dlo %||%  5)
    dhi <- as.numeric(input$cfg_dhi %||%  9)
    wp  <- as.numeric(input$cfg_wp  %||%  2)
    gn  <- trimws(input$cfg_good    %||% "soybeans")
    un  <- trimws(input$cfg_unit    %||% "bushel")
    sd  <- as.integer(input$cfg_seed %||% 42L)

    if (nb < 1)               { showNotification("Need >= 1 buyer.",               type = "error"); return() }
    if (nd + ni < 1)          { showNotification("Need >= 1 seller.",              type = "error"); return() }
    if (blo >= bhi)           { showNotification("Buyer WTP min must be < max.",   type = "error"); return() }
    if (nd > 0 && dlo >= dhi) { showNotification("Domestic cost min must be < max.", type = "error"); return() }
    if (wp >= bhi)            { showNotification("World price should be < buyer WTP max for trades to occur.", type = "warning") }

    code <- gen_room_code()
    pin  <- gen_pin()

    db_exec("
      INSERT INTO rooms(room_id, good_name, unit, n_buyers, n_sellers_dom, n_sellers_imp,
                        buyer_hi, buyer_lo, dom_cost_lo, dom_cost_hi, world_price,
                        instructor_pin, created_at)
      VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?);
    ", list(code, gn, un, nb, nd, ni, bhi, blo, dlo, dhi, wp, pin,
            format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))

    cards <- generate_cards(nb, nd, ni, bhi, blo, dlo, dhi, wp, sd)
    insert_cards(code, cards)

    rv$player_token  <- gen_player_token()
    rv$room_id       <- code
    rv$is_instructor <- TRUE
    rv$view          <- "game"

    logf("Room created:", code, "| buyers:", nb, "| dom sellers:", nd, "| imp sellers:", ni)

    showModal(modalDialog(
      title = "Room ready!",
      div(style = "text-align:center;",
        p("Write this code on the board:"),
        p(class = "room-code", code),
        tags$hr(),
        p("Your instructor PIN (keep private -- needed to rejoin as instructor):"),
        p(style = "font-size:1.8rem; font-family:monospace; color:#555;", pin)
      ),
      p(tags$small(class = "text-muted",
        "Students go to this page, enter the room code and their name.",
        "You can re-enter as instructor from the join screen using the PIN."
      )),
      easyClose = FALSE,
      footer = modalButton("Got it - go to room")
    ))
  })

  observeEvent(input$join_btn, {
    code <- toupper(trimws(input$join_code %||% ""))
    name <- trimws(input$join_name %||% "")

    if (!nzchar(code)) { showNotification("Enter the room code.",  type = "error"); return() }
    if (!nzchar(name)) { showNotification("Enter your name.",      type = "error"); return() }

    room <- db_query("SELECT * FROM rooms WHERE room_id = ?;", list(code))
    if (!nrow(room)) { showNotification("Room not found. Check the code.", type = "error"); return() }

    as_inst <- isTRUE(input$join_as_inst)
    pin_ok  <- identical(trimws(input$join_pin %||% ""), room$instructor_pin[1])

    if (as_inst && !pin_ok) {
      showNotification("Incorrect instructor PIN.", type = "error"); return()
    }

    tok <- gen_player_token()

    if (!as_inst) {
      # Claim the next unassigned card atomically
      next_card <- db_query("
        SELECT card_num FROM cards
        WHERE room_id = ? AND player_token IS NULL
        ORDER BY card_num LIMIT 1;
      ", list(code))

      if (!nrow(next_card)) {
        showNotification("This room is full -- all cards have been assigned.", type = "warning")
        return()
      }

      db_exec("
        UPDATE cards
        SET player_token = ?, display_name = ?, joined_at = ?
        WHERE room_id = ? AND card_num = ? AND player_token IS NULL;
      ", list(tok, name, format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
              code, next_card$card_num[1]))
    }

    rv$player_token  <- tok
    rv$room_id       <- code
    rv$is_instructor <- as_inst
    rv$view          <- "game"

    logf("Join:", name, "| room:", code, "| instructor:", as_inst)
  })

  # Game-screen reactives

  get_room <- reactive({
    ticker()
    if (is.null(rv$room_id)) return(NULL)
    rows <- db_query("SELECT * FROM rooms WHERE room_id = ?;", list(rv$room_id))
    if (!nrow(rows)) NULL else rows[1, ]
  })

  get_my_card <- reactive({
    ticker()
    if (is.null(rv$player_token) || isTRUE(rv$is_instructor)) return(NULL)
    rows <- db_query("SELECT * FROM cards WHERE room_id = ? AND player_token = ?;",
                     list(rv$room_id, rv$player_token))
    if (!nrow(rows)) NULL else rows[1, ]
  })

  get_round <- reactive({
    ticker()
    room <- get_room(); req(!is.null(room))
    rn   <- as.integer(room$current_round)
    rows <- db_query("SELECT * FROM rounds WHERE room_id = ? AND round_num = ?;",
                     list(rv$room_id, rn))
    if (!nrow(rows)) NULL else rows[1, ]
  })

  get_my_order <- reactive({
    ticker()
    room <- get_room(); req(!is.null(room))
    rn   <- as.integer(room$current_round)
    rows <- db_query("SELECT * FROM orders WHERE room_id = ? AND round_num = ? AND player_token = ?;",
                     list(rv$room_id, rn, rv$player_token))
    if (!nrow(rows)) NULL else rows[1, ]
  })

  # Game UI router

  output$game_ui <- renderUI({
    req(rv$view == "game", !is.null(rv$room_id))
    room <- get_room()
    if (is.null(room)) return(p(em("Loading...")))

    gn  <- tools::toTitleCase(room$good_name %||% "commodity")
    rn  <- as.integer(room$current_round)
    rnd <- get_round()

    status_lbl <- if (is.null(rnd) || rnd$status == "pending") {
      span(class = "label label-default", "Not started")
    } else if (rnd$status == "open") {
      span(class = "label label-success", "Round open - submit your price")
    } else {
      span(class = "label label-info",    "Round closed - results below")
    }

    tax_note <- if (!is.null(rnd) && rnd$tax_amount > 0) {
      p(em(class = "text-danger", sprintf(
        "%s: $%.2f per %s on %s",
        if (rnd$tax_type == "tariff") "Tariff" else "Excise tax",
        rnd$tax_amount, room$unit %||% "unit",
        if (rnd$tax_type == "tariff") "imported goods" else "all transactions"
      )))
    }

    header <- wellPanel(
      fluidRow(
        column(8,
          h4(sprintf("Market: %s", gn)),
          div(class = "round-banner", strong(sprintf("Round %d  ", rn)), status_lbl),
          tax_note
        ),
        column(4, style = "text-align:right; padding-top:8px;",
          p(tags$code(rv$room_id)),
          if (rv$is_instructor) span(class = "label label-warning", "INSTRUCTOR VIEW") else NULL
        )
      )
    )

    body <- if (rv$is_instructor) uiOutput("instructor_ui") else uiOutput("student_ui")

    tagList(header, body)
  })

  # Student UI

  output$student_ui <- renderUI({
    card <- get_my_card()
    room <- get_room()
    rnd  <- get_round()
    ord  <- get_my_order()

    if (is.null(room)) return(p(em("Loading...")))
    unit <- room$unit %||% "unit"

    if (is.null(card)) {
      return(wellPanel(
        p(em("No card assigned yet. You may be the instructor, or the room is full."))
      ))
    }

    # Role card
    card_css_class <- if (card$role == "buyer") "role-card buyer-card"
                      else if (card$origin == "imported") "role-card seller-imp-card"
                      else "role-card seller-dom-card"

    role_title  <- if (card$role == "buyer") "BUYER" else "SELLER"
    origin_note <- if (card$role == "seller") {
      if (card$origin == "imported")
        p(style = "color:#E65100; font-weight:600;", "Imported goods supplier")
      else
        p(style = "color:#2E7D32; font-weight:600;", "Domestic producer")
    }

    # Effective cost/value for this round
    eff     <- card$private_value
    tax_adj <- NULL
    if (!is.null(rnd) && rnd$tax_amount > 0 && card$role == "seller") {
      applies <- rnd$tax_type == "excise" ||
                 (rnd$tax_type == "tariff" && card$origin == "imported")
      if (applies) {
        eff <- card$private_value + rnd$tax_amount
        tax_adj <- p(style = "color:#C62828; font-weight:600;",
          sprintf("Your effective cost this round: $%.2f  (base $%.2f + $%.2f tax)",
                  eff, card$private_value, rnd$tax_amount))
      }
    }

    val_label <- if (card$role == "buyer") "Your value (max WTP)"
                 else                      "Your cost (min acceptable price)"
    reminder  <- if (card$role == "buyer")
      "Submit a bid <= your value to gain surplus if you trade."
    else
      "Submit an ask >= your effective cost to avoid a loss."

    role_panel <- div(class = card_css_class,
      h5(strong(sprintf("Your market role: %s", role_title))),
      origin_note,
      p(strong(sprintf("%s: $%.2f per %s", val_label, card$private_value, unit))),
      tax_adj,
      p(em(reminder))
    )

    # Trading section
    trading <- if (is.null(rnd) || rnd$status == "pending") {
      wellPanel(p(em("Waiting for your instructor to open the round...")))

    } else if (rnd$status == "open") {
      if (!is.null(ord)) {
        wellPanel(
          p(tags$b(sprintf("Price submitted: $%.2f", ord$submitted_price))),
          p(em("Waiting for instructor to close the round and clear the market.")),
          tags$hr(),
          p(tags$small("Changed your mind? You can re-submit until the round closes.")),
          numericInput("order_price_rev", "Revise price ($):",
                       value = ord$submitted_price, min = 0, step = 0.25),
          actionButton("submit_order", "Update price", class = "btn-warning btn-sm")
        )
      } else {
        price_label <- if (card$role == "buyer") "Your bid ($):" else "Your ask ($):"
        wellPanel(
          h5("Submit your price"),
          numericInput("order_price", price_label, value = round(eff, 2), min = 0, step = 0.25),
          actionButton("submit_order", "Submit", class = "btn-primary"),
          p(tags$small("You can revise until the instructor closes the round."))
        )
      }

    } else {  # closed
      cp  <- rnd$clearing_price
      res <- tryCatch(jsonlite::fromJSON(rnd$results_json), error = function(e) list())
      tok <- rv$player_token

      if (is.null(cp) || is.na(cp)) {
        wellPanel(h5("Round result"), p("No trades cleared this round."))
      } else {
        traded <- tok %in% (res$buyer_tokens  %||% character(0)) ||
                  tok %in% (res$seller_tokens %||% character(0))

        surplus <- if (traded) {
          if (card$role == "buyer") card$private_value - cp
          else                      cp - card$private_value
        } else 0

        wellPanel(
          h5("Round result"),
          p(strong(sprintf("Clearing price: $%.2f | Quantity traded: %d",
                            cp, rnd$quantity_traded %||% 0))),
          if (traded)
            p(class = "surplus-row", style = "color:#1B5E20;",
              tags$b(sprintf("You traded! Your surplus this round: $%.2f",
                             round(surplus, 2))))
          else
            p(class = "surplus-row", style = "color:#888;",
              "You did not trade this round.")
        )
      }
    }

    fluidRow(
      column(6, role_panel),
      column(6, trading)
    )
  })

  observeEvent(input$submit_order, {
    rnd  <- get_round()
    room <- get_room()
    if (is.null(rnd) || rnd$status != "open") {
      showNotification("The round is not open.", type = "error"); return()
    }

    # Use revised price input if it exists, else new price input
    price <- suppressWarnings(
      as.numeric(input$order_price_rev %||% input$order_price %||% NA)
    )
    if (!is.finite(price) || price < 0) {
      showNotification("Enter a valid positive price.", type = "error"); return()
    }

    rn <- as.integer(room$current_round)
    db_exec("
      INSERT OR REPLACE INTO orders(room_id, round_num, player_token, submitted_price, submitted_at)
      VALUES(?, ?, ?, ?, ?);
    ", list(rv$room_id, rn, rv$player_token, price,
            format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
    showNotification(sprintf("Submitted $%.2f", price), type = "message")
  })

  # Instructor UI

  output$instructor_ui <- renderUI({
    room <- get_room()
    rnd  <- get_round()
    if (is.null(room)) return(p(em("Loading...")))

    rn   <- as.integer(room$current_round)
    unit <- room$unit %||% "unit"

    all_cards <- db_query("
      SELECT card_num, role, origin, private_value, display_name, player_token
      FROM cards WHERE room_id = ? ORDER BY card_num;
    ", list(rv$room_id))

    n_cards  <- nrow(all_cards)
    n_joined <- sum(!is.na(all_cards$player_token))

    n_orders <- if (!is.null(rnd) && rnd$status == "open") {
      db_query("SELECT COUNT(*) AS n FROM orders WHERE room_id = ? AND round_num = ?;",
               list(rv$room_id, rn))$n[1]
    } else NA_integer_

    # Round control panel
    round_ctrl <- wellPanel(
      h5(sprintf("Round %d controls", rn)),
      p(sprintf("Players joined: %d / %d", n_joined, n_cards)),
      if (!is.na(n_orders)) p(strong(sprintf("Orders submitted: %d / %d", n_orders, n_joined))),

      if (is.null(rnd) || rnd$status == "pending") {
        tagList(
          fluidRow(
            column(4,
              numericInput("cfg_tax", sprintf("Tax per %s ($)", unit),
                           value = 0, min = 0, step = 0.25)
            ),
            column(4,
              selectInput("cfg_tax_type", "Tax type",
                choices = c(
                  "None (baseline)"          = "none",
                  "Tariff (imports only)"    = "tariff",
                  "Excise tax (all sellers)" = "excise"
                ))
            ),
            column(4, br(),
              actionButton("open_round_btn", "Open round", class = "btn-success")
            )
          ),
          p(tags$small(class = "text-muted",
            "Once open, students submit bids/asks. Close the round to clear the market."
          ))
        )
      } else if (rnd$status == "open") {
        tagList(
          p(class = "text-muted", em("Round is live. Close when you are ready to clear the market.")),
          actionButton("close_round_btn", "Close round & clear market",
                       class = "btn-danger btn-lg")
        )
      } else {  # closed
        tagList(
          uiOutput("round_results_ui"),
          tags$hr(),
          actionButton("next_round_btn",
                       sprintf("Advance to round %d", rn + 1L),
                       class = "btn-primary")
        )
      }
    )

    # Player / card table
    players_panel <- wellPanel(
      h5("Player cards"),
      p(tags$small(class = "text-muted",
        "Private values are hidden from the class projector.",
        "Role and origin are shown to help spot if any seats remain."
      )),
      tableOutput("player_table_out")
    )

    fluidRow(
      column(7, round_ctrl, players_panel),
      column(5,
        wellPanel(
          h5("Supply & demand (theoretical)"),
          p(tags$small(class = "text-muted",
            "Dashed line = clearing price when a round has closed.",
            "Supply curve reflects current round's tax."
          )),
          plotOutput("sd_plot", height = "280px")
        ),
        uiOutput("round_history_ui")
      )
    )
  })

  output$round_results_ui <- renderUI({
    rnd <- get_round()
    if (is.null(rnd) || rnd$status != "closed") return(NULL)
    res <- tryCatch(jsonlite::fromJSON(rnd$results_json), error = function(e) list())
    cp  <- rnd$clearing_price

    if (is.null(cp) || is.na(cp)) return(wellPanel(p("No trades cleared this round.")))

    wellPanel(
      h6("Market outcome"),
      tags$table(class = "table table-condensed table-bordered",
        style = "max-width:320px;",
        tags$tr(tags$td("Clearing price"),  tags$td(strong(sprintf("$%.2f", cp)))),
        tags$tr(tags$td("Units traded"),    tags$td(rnd$quantity_traded %||% 0)),
        tags$tr(tags$td("Buyer surplus"),   tags$td(sprintf("$%.2f", res$buyer_surplus  %||% 0))),
        tags$tr(tags$td("Seller surplus"),  tags$td(sprintf("$%.2f", res$seller_surplus %||% 0))),
        tags$tr(tags$td("Gov. revenue"),    tags$td(sprintf("$%.2f", res$gov_revenue    %||% 0))),
        tags$tr(tags$td(strong("Total surplus")),
                tags$td(strong(sprintf("$%.2f", res$total_surplus %||% 0))))
      )
    )
  })

  output$round_history_ui <- renderUI({
    ticker()
    closed <- db_query("
      SELECT round_num, tax_type, tax_amount, clearing_price, quantity_traded, results_json
      FROM rounds WHERE room_id = ? AND status = 'closed' ORDER BY round_num;
    ", list(rv$room_id))
    if (!nrow(closed)) return(NULL)

    rows <- lapply(seq_len(nrow(closed)), function(i) {
      r   <- closed[i, ]
      res <- tryCatch(jsonlite::fromJSON(r$results_json), error = function(e) list())
      tax_str <- if (r$tax_amount > 0)
        sprintf("%s $%.2f", r$tax_type, r$tax_amount)
      else "none"
      tags$tr(
        tags$td(r$round_num),
        tags$td(tax_str),
        tags$td(if (is.na(r$clearing_price)) "--" else sprintf("$%.2f", r$clearing_price)),
        tags$td(r$quantity_traded %||% 0),
        tags$td(sprintf("$%.2f", res$total_surplus %||% 0))
      )
    })

    wellPanel(
      h6("Round history"),
      tags$table(class = "table table-condensed",
        tags$thead(tags$tr(
          tags$th("Rnd"), tags$th("Tax"), tags$th("Price"),
          tags$th("Qty"), tags$th("Surplus")
        )),
        tags$tbody(rows)
      )
    )
  })

  output$player_table_out <- renderTable({
    ticker()
    if (is.null(rv$room_id)) return(NULL)
    room <- get_room()
    rn   <- as.integer(room$current_round %||% 1L)

    cards <- db_query("
      SELECT card_num, role, origin, private_value, display_name, player_token
      FROM cards WHERE room_id = ? ORDER BY card_num;
    ", list(rv$room_id))
    if (!nrow(cards)) return(NULL)

    submitted <- db_query("
      SELECT player_token FROM orders WHERE room_id = ? AND round_num = ?;
    ", list(rv$room_id, rn))$player_token

    cards$Name       <- ifelse(is.na(cards$display_name), "(open)", cards$display_name)
    cards$Role       <- cards$role
    cards$Origin     <- cards$origin
    cards$`WTP/Cost` <- sprintf("$%.2f", cards$private_value)
    cards$Joined     <- ifelse(!is.na(cards$player_token), "Y", "")
    cards$Submitted  <- ifelse(cards$player_token %in% submitted, "Y", "")

    cards[, c("card_num", "Name", "Role", "Origin", "WTP/Cost", "Joined", "Submitted")]
  }, rownames = FALSE)

  output$sd_plot <- renderPlot({
    room <- get_room()
    if (is.null(room)) return(NULL)

    rnd <- get_round()
    ta  <- if (!is.null(rnd)) as.numeric(rnd$tax_amount %||% 0) else 0
    tt  <- if (!is.null(rnd)) as.character(rnd$tax_type %||% "none") else "none"
    cp  <- if (!is.null(rnd) && rnd$status == "closed") rnd$clearing_price else NA_real_

    curves <- build_step_curves(rv$room_id, ta, tt)
    if (is.null(curves)) return(NULL)

    d    <- curves$demand
    s    <- curves$supply
    unit <- room$unit %||% "unit"

    # Build step-function data frames
    make_steps <- function(vals, qs) {
      do.call(rbind, lapply(seq_along(vals), function(i) {
        data.frame(x = c(qs[i] - 1, qs[i]), y = c(vals[i], vals[i]))
      }))
    }

    d_steps <- make_steps(d$private_value, d$q)
    s_steps <- make_steps(s$eff,           s$q)

    xmax <- nrow(d) + nrow(s)
    ymax <- max(c(d$private_value, s$eff), na.rm = TRUE) * 1.15

    par(mar = c(4, 4, 1, 1), bg = "white")
    plot(NULL, xlim = c(0, xmax), ylim = c(0, ymax),
         xlab = sprintf("Quantity (%ss)", unit),
         ylab = sprintf("Price per %s ($)", unit),
         bty = "l", las = 1)
    lines(d_steps$x, d_steps$y, col = "#1565C0", lwd = 2.5)
    lines(s_steps$x, s_steps$y, col = "#2E7D32", lwd = 2.5)

    if (!is.na(cp)) {
      abline(h = cp, lty = 2, col = "#B71C1C", lwd = 1.5)
    }

    legend_labels <- c("Demand",
                       sprintf("Supply (effective%s)",
                               if (ta > 0) sprintf(" + $%.2f tax", ta) else ""))
    legend_cols   <- c("#1565C0", "#2E7D32")
    if (!is.na(cp)) {
      legend_labels <- c(legend_labels, "Clearing price")
      legend_cols   <- c(legend_cols,   "#B71C1C")
    }
    legend("topright", bty = "n", lwd = 2, col = legend_cols, legend = legend_labels)
  })

  # Round event handlers

  observeEvent(input$open_round_btn, {
    room <- get_room(); req(!is.null(room))
    rn <- as.integer(room$current_round)
    ta <- max(0, suppressWarnings(as.numeric(input$cfg_tax      %||% 0)))
    tt <- as.character(input$cfg_tax_type %||% "none")
    if (ta == 0) tt <- "none"

    db_exec("
      INSERT OR REPLACE INTO rounds(room_id, round_num, status, tax_amount, tax_type, opened_at)
      VALUES(?, ?, 'open', ?, ?, ?);
    ", list(rv$room_id, rn, ta, tt, format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))

    logf("Round opened:", rn, "| tax:", ta, tt)
    showNotification(sprintf("Round %d opened.", rn), type = "message")
  })

  observeEvent(input$close_round_btn, {
    rnd  <- get_round()
    room <- get_room()
    if (is.null(rnd) || rnd$status != "open") {
      showNotification("Round is not open.", type = "warning"); return()
    }
    rn <- as.integer(room$current_round)

    # Pull all orders with role/origin/private_value from cards
    ord_data <- db_query("
      SELECT o.player_token, o.submitted_price,
             c.role, c.origin, c.private_value
      FROM orders o
      JOIN cards c ON c.room_id = o.room_id AND c.player_token = o.player_token
      WHERE o.room_id = ? AND o.round_num = ?;
    ", list(rv$room_id, rn))

    bids <- ord_data[ord_data$role == "buyer",  , drop = FALSE]
    asks <- ord_data[ord_data$role == "seller", , drop = FALSE]

    result <- market_clear(bids, asks, rnd$tax_amount, rnd$tax_type)

    db_exec("
      UPDATE rounds
      SET status = 'closed', clearing_price = ?, quantity_traded = ?,
          results_json = ?, closed_at = ?
      WHERE room_id = ? AND round_num = ?;
    ", list(result$clearing_price %||% NA_real_,
            result$quantity,
            jsonlite::toJSON(result, auto_unbox = TRUE),
            format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            rv$room_id, rn))

    logf("Round closed:", rn, "| cp:", result$clearing_price, "| qty:", result$quantity)
    showNotification(
      sprintf("Round %d closed. Price: %s | Qty: %d | Surplus: $%.2f",
              rn,
              if (is.na(result$clearing_price)) "no trade"
              else sprintf("$%.2f", result$clearing_price),
              result$quantity,
              result$total_surplus),
      type = "message", duration = 8
    )
  })

  observeEvent(input$next_round_btn, {
    room <- get_room(); req(!is.null(room))
    db_exec("UPDATE rooms SET current_round = current_round + 1 WHERE room_id = ?;",
            list(rv$room_id))
    logf("Advanced to round", as.integer(room$current_round) + 1L)
    showNotification(sprintf("Advanced to round %d.", as.integer(room$current_round) + 1L),
                     type = "message")
  })

  # Cleanup
  session$onSessionEnded(function() {
    if (!is.null(conn) && DBI::dbIsValid(conn))
      try(DBI::dbDisconnect(conn), silent = TRUE)
  })
}

shinyApp(ui, server)
