# apps/excise-tax/app.R
# -----------------------------------------------------------------------
# Tax Incidence: Classroom Call-Market Experiment
#
# Students experience three rounds in a simple commodity market:
#   Round 1 -- no tax (competitive baseline)
#   Round 2 -- tax on sellers  (excise collected from sellers)
#   Round 3 -- tax on buyers   (excise collected from buyers)
# Both tax rounds yield identical prices and quantities, demonstrating
# that statutory incidence != economic incidence.
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
# DB setup
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
  db_exec("
    CREATE TABLE IF NOT EXISTS rooms (
      room_id        TEXT PRIMARY KEY,
      good_name      TEXT    DEFAULT 'wheat',
      unit           TEXT    DEFAULT 'bushel',
      n_buyers       INTEGER DEFAULT 4,
      n_sellers_dom  INTEGER DEFAULT 2,
      n_sellers_imp  INTEGER DEFAULT 2,
      buyer_hi       REAL    DEFAULT 11,
      buyer_lo       REAL    DEFAULT 5,
      dom_cost_lo    REAL    DEFAULT 5,
      dom_cost_hi    REAL    DEFAULT 7,
      world_price    REAL    DEFAULT 1,
      current_round  INTEGER DEFAULT 1,
      instructor_pin TEXT,
      created_at     TEXT
    );
  ")

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

  b_vals  <- round(seq(buyer_hi, buyer_lo, length.out = max(1L, n_buyers)), 2)
  d_costs <- round(seq(dom_lo,   dom_hi,   length.out = max(1L, n_dom)),    2)
  i_costs <- rep(round(world_price, 2), n_imp)

  df <- data.frame(
    role          = c(rep("buyer",  n_buyers), rep("seller", n_dom), rep("seller", n_imp)),
    origin        = c(rep("domestic", n_buyers + n_dom), rep("imported", n_imp)),
    private_value = c(b_vals, d_costs, i_costs),
    stringsAsFactors = FALSE
  )

  df          <- df[sample(nrow(df)), ]
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
# tax_type: "none" | "seller_excise" | "buyer_excise"
#
# seller_excise: tax raises effective cost for all sellers (supply shifts up)
# buyer_excise:  tax lowers effective WTP for all buyers  (demand shifts down)
#
# In both cases the market-clearing quantity and the buyer/seller price
# wedge are identical -- this is the main lesson.
# -----------------------------------------------------------------------

market_clear <- function(bids, asks, tax_amount = 0, tax_type = "none") {
  empty <- list(clearing_price = NA_real_, p_buyer = NA_real_, p_seller = NA_real_,
                quantity = 0L, buyer_tokens = character(0), seller_tokens = character(0),
                buyer_surplus = 0, seller_surplus = 0, gov_revenue = 0, total_surplus = 0)

  if (!nrow(bids) || !nrow(asks)) return(empty)

  # Effective values after tax
  bids$eff <- bids$submitted_price
  asks$eff <- asks$submitted_price

  if (tax_type == "buyer_excise") {
    bids$eff <- bids$submitted_price - tax_amount   # demand shifts down
  } else if (tax_type == "seller_excise") {
    asks$eff <- asks$submitted_price + tax_amount   # supply shifts up
  }

  # Sort: effective bids descending, effective asks ascending
  bids <- bids[order(-bids$eff), , drop = FALSE]
  asks <- asks[order( asks$eff), , drop = FALSE]

  # Find max k where eff_bid_k >= eff_ask_k
  n <- min(nrow(bids), nrow(asks))
  k <- 0L
  for (i in seq_len(n)) {
    if (bids$eff[i] >= asks$eff[i]) k <- i else break
  }
  if (k == 0L) return(empty)

  # Clearing price = midpoint of marginal effective bid and effective ask
  # buyer_excise:  cp is P_seller (what seller receives); buyer pays cp + tax
  # seller_excise: cp is P_buyer  (what buyer pays);      seller keeps cp - tax
  # none:          cp is the single market price
  cp <- round((bids$eff[k] + asks$eff[k]) / 2, 2)

  p_buyer  <- if (tax_type == "buyer_excise")  round(cp + tax_amount, 2) else cp
  p_seller <- if (tax_type == "seller_excise") round(cp - tax_amount, 2) else cp

  b_tok <- bids$player_token[seq_len(k)]
  s_tok <- asks$player_token[seq_len(k)]

  # Surplus measured against private values, net of tax payments
  bs  <- sum(bids$private_value[seq_len(k)] - p_buyer,  na.rm = TRUE)
  ps  <- sum(p_seller - asks$private_value[seq_len(k)], na.rm = TRUE)
  gov <- if (tax_type %in% c("buyer_excise", "seller_excise")) tax_amount * k else 0

  list(
    clearing_price  = cp,
    p_buyer         = p_buyer,
    p_seller        = p_seller,
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

  # Shift supply up (seller excise) or demand down (buyer excise)
  buyers$eff  <- buyers$private_value
  sellers$eff <- sellers$private_value

  if (tax_type == "buyer_excise") {
    buyers$eff <- buyers$private_value - tax_amount
  } else if (tax_type == "seller_excise") {
    sellers$eff <- sellers$private_value + tax_amount
  }

  buyers  <- buyers[order(-buyers$eff),   , drop = FALSE]
  sellers <- sellers[order(sellers$eff),  , drop = FALSE]
  buyers$q  <- seq_len(nrow(buyers))
  sellers$q <- seq_len(nrow(sellers))

  list(demand = buyers, supply = sellers)
}

# -----------------------------------------------------------------------
# CSS
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

VASSAR_CSS <- "
  body { font-size: 16px; }
  .btn-primary       { background-color: #951829; border-color: #7a1221; }
  .btn-primary:hover { background-color: #7a1221; border-color: #5e0d19; }
  .btn-success       { background-color: #2d6a4f; border-color: #245c43; }
  .btn-success:hover { background-color: #245c43; border-color: #1b4d38; }
  a { color: #951829; } a:hover { color: #7a1221; }
  .shiny-notification-panel { top:16px; bottom:auto; right:16px; min-width:340px; width:auto; }
  .shiny-notification { font-size:1.1rem; padding:1rem 1.25rem; border-radius:8px; border:none;
    box-shadow:0 4px 20px rgba(0,0,0,.25); color:#fff; margin-bottom:8px; }
  .shiny-notification-message { background:#2d6a4f; }
  .shiny-notification-warning { background:#92400e; }
  .shiny-notification-error   { background:#951829; }
  .shiny-notification-close   { color:rgba(255,255,255,.8); font-size:1.2rem; }
"
ui <- fluidPage(
  tags$head(tags$style(HTML(VASSAR_CSS))),
  tags$head(tags$style(HTML(card_css))),
  titlePanel("Tax Incidence: Classroom Experiment"),
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
    view          = "landing"
  )

  ticker     <- reactiveTimer(2500)
  setup_open <- reactiveVal(FALSE)

  observeEvent(session$clientData$url_search, {
    q  <- parseQueryString(session$clientData$url_search)
    rc <- toupper(trimws(q[["room"]] %||% ""))
    if (nzchar(rc)) updateTextInput(session, "join_code", value = rc)
  }, once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE)

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
        column(4, textInput("cfg_good",  "Good name",  value = "wheat")),
        column(4, textInput("cfg_unit",  "Unit",       value = "bushel")),
        column(4, numericInput("cfg_seed", "Seed (reproducibility)", value = 42, min = 1))
      ),
      fluidRow(
        column(4, numericInput("cfg_nb",  "# Buyers",           value = 4, min = 1)),
        column(4, numericInput("cfg_nd",  "# Domestic sellers", value = 2, min = 0)),
        column(4, numericInput("cfg_ni",  "# Imported sellers", value = 2, min = 0))
      ),
      fluidRow(
        column(3, numericInput("cfg_bhi", "Buyer WTP max ($)", value = 11, min = 1,   step = 0.5)),
        column(3, numericInput("cfg_blo", "Buyer WTP min ($)", value = 5,  min = 0,   step = 0.5)),
        column(3, numericInput("cfg_dlo", "Dom. cost min ($)", value = 5,  min = 0,   step = 0.5)),
        column(3, numericInput("cfg_dhi", "Dom. cost max ($)", value = 7,  min = 0,   step = 0.5))
      ),
      fluidRow(
        column(4, numericInput("cfg_wp", "World (import) price ($)", value = 1, min = 0, step = 0.25)),
        column(8, tags$div(style = "padding-top:8px;",
          tags$small(class = "text-muted",
            "World price is the cost for all imported sellers.",
            "Import/domestic distinction is flavor; the tax rounds apply equally to all sellers."
          )
        ))
      ),
      actionButton("create_room_btn", "Create room & generate cards", class = "btn-primary")
    )
  })

  observeEvent(input$create_room_btn, {
    nb  <- as.integer(input$cfg_nb  %||% 4L)
    nd  <- as.integer(input$cfg_nd  %||% 2L)
    ni  <- as.integer(input$cfg_ni  %||% 2L)
    bhi <- as.numeric(input$cfg_bhi %||% 11)
    blo <- as.numeric(input$cfg_blo %||%  5)
    dlo <- as.numeric(input$cfg_dlo %||%  5)
    dhi <- as.numeric(input$cfg_dhi %||%  7)
    wp  <- as.numeric(input$cfg_wp  %||%  1)
    gn  <- trimws(input$cfg_good    %||% "wheat")
    un  <- trimws(input$cfg_unit    %||% "bushel")
    sd  <- as.integer(input$cfg_seed %||% 42L)

    if (nb < 1)               { showNotification("Need >= 1 buyer.",               type = "error"); return() }
    if (nd + ni < 1)          { showNotification("Need >= 1 seller.",              type = "error"); return() }
    if (blo >= bhi)           { showNotification("Buyer WTP min must be < max.",   type = "error"); return() }
    if (nd > 0 && dlo >= dhi) { showNotification("Domestic cost min must be < max.", type = "error"); return() }

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
      who <- if (rnd$tax_type == "buyer_excise") "collected from buyers"
             else                                "collected from sellers"
      p(em(class = "text-danger", sprintf(
        "Tax: $%.2f per %s, %s",
        rnd$tax_amount, room$unit %||% "unit", who
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

    # Effective value after tax
    eff     <- card$private_value
    tax_adj <- NULL

    if (!is.null(rnd) && rnd$tax_amount > 0) {
      if (rnd$tax_type == "seller_excise" && card$role == "seller") {
        eff <- card$private_value + rnd$tax_amount
        tax_adj <- p(style = "color:#C62828; font-weight:600;",
          sprintf("Your effective cost this round: $%.2f  (base $%.2f + $%.2f tax)",
                  eff, card$private_value, rnd$tax_amount))
      } else if (rnd$tax_type == "buyer_excise" && card$role == "buyer") {
        eff <- card$private_value - rnd$tax_amount
        tax_adj <- p(style = "color:#C62828; font-weight:600;",
          sprintf("Your effective value this round: $%.2f  (value $%.2f - $%.2f tax you owe)",
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

        # Use the correct price for surplus calculation
        my_price <- if (card$role == "buyer") {
          res$p_buyer %||% cp
        } else {
          res$p_seller %||% cp
        }

        surplus <- if (traded) {
          if (card$role == "buyer") card$private_value - my_price
          else                      my_price - card$private_value
        } else 0

        p_buyer_lbl  <- res$p_buyer  %||% cp
        p_seller_lbl <- res$p_seller %||% cp

        price_note <- if (!is.null(rnd) && rnd$tax_amount > 0)
          p(tags$small(sprintf("Buyers paid: $%.2f | Sellers received: $%.2f",
                               p_buyer_lbl, p_seller_lbl)))

        wellPanel(
          h5("Round result"),
          p(strong(sprintf("Quantity traded: %d", rnd$quantity_traded %||% 0))),
          price_note,
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
              selectInput("cfg_tax_type", "Tax collected from",
                choices = c(
                  "No tax (baseline)"  = "none",
                  "Sellers"            = "seller_excise",
                  "Buyers"             = "buyer_excise"
                ))
            ),
            column(4, br(),
              actionButton("open_round_btn", "Open round", class = "btn-success")
            )
          ),
          p(tags$small(class = "text-muted",
            "Both seller and buyer taxes create the same price wedge.",
            "Use one round for each to demonstrate equivalence."
          ))
        )
      } else if (rnd$status == "open") {
        tagList(
          p(class = "text-muted", em("Round is live. Close when you are ready to clear the market.")),
          actionButton("close_round_btn", "Close round & clear market",
                       class = "btn-danger btn-lg")
        )
      } else {
        tagList(
          uiOutput("round_results_ui"),
          tags$hr(),
          actionButton("next_round_btn",
                       sprintf("Advance to round %d", rn + 1L),
                       class = "btn-primary")
        )
      }
    )

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
            "Curve shifts to reflect current round's tax side."
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

    p_buyer  <- res$p_buyer  %||% cp
    p_seller <- res$p_seller %||% cp

    wellPanel(
      h6("Market outcome"),
      tags$table(class = "table table-condensed table-bordered",
        style = "max-width:340px;",
        tags$tr(tags$td("Buyers paid"),    tags$td(strong(sprintf("$%.2f", p_buyer)))),
        tags$tr(tags$td("Sellers received"), tags$td(strong(sprintf("$%.2f", p_seller)))),
        tags$tr(tags$td("Units traded"),   tags$td(rnd$quantity_traded %||% 0)),
        tags$tr(tags$td("Buyer surplus"),  tags$td(sprintf("$%.2f", res$buyer_surplus  %||% 0))),
        tags$tr(tags$td("Seller surplus"), tags$td(sprintf("$%.2f", res$seller_surplus %||% 0))),
        tags$tr(tags$td("Gov. revenue"),   tags$td(sprintf("$%.2f", res$gov_revenue    %||% 0))),
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
        sprintf("%s $%.2f", sub("_excise", "", r$tax_type), r$tax_amount)
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
          tags$th("Rnd"), tags$th("Tax"), tags$th("CP"),
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
    tt  <- if (!is.null(rnd)) as.character(rnd$tax_type  %||% "none") else "none"
    cp  <- if (!is.null(rnd) && rnd$status == "closed") rnd$clearing_price else NA_real_

    curves <- build_step_curves(rv$room_id, ta, tt)
    if (is.null(curves)) return(NULL)

    d    <- curves$demand
    s    <- curves$supply
    unit <- room$unit %||% "unit"

    make_steps <- function(vals, qs) {
      do.call(rbind, lapply(seq_along(vals), function(i) {
        data.frame(x = c(qs[i] - 1, qs[i]), y = c(vals[i], vals[i]))
      }))
    }

    d_steps <- make_steps(d$eff, d$q)
    s_steps <- make_steps(s$eff, s$q)

    xmax <- nrow(d) + nrow(s)
    ymax <- max(c(d$private_value, s$private_value), na.rm = TRUE) * 1.15

    par(mar = c(4, 4, 1, 1), bg = "white")
    plot(NULL, xlim = c(0, xmax), ylim = c(0, ymax),
         xlab = sprintf("Quantity (%ss)", unit),
         ylab = sprintf("Price per %s ($)", unit),
         bty = "l", las = 1)
    lines(d_steps$x, d_steps$y, col = "#1565C0", lwd = 2.5)
    lines(s_steps$x, s_steps$y, col = "#2E7D32", lwd = 2.5)

    if (!is.na(cp)) abline(h = cp, lty = 2, col = "#B71C1C", lwd = 1.5)

    curve_lbl <- if (tt == "buyer_excise")
      sprintf("Demand (effective, -%s tax)", sprintf("$%.2f", ta))
    else if (tt == "seller_excise")
      sprintf("Supply (effective, +%s tax)", sprintf("$%.2f", ta))
    else "Supply"

    demand_lbl <- if (tt == "buyer_excise") curve_lbl else "Demand"
    supply_lbl <- if (tt == "seller_excise") curve_lbl else "Supply"

    legend_labels <- c(demand_lbl, supply_lbl)
    legend_cols   <- c("#1565C0", "#2E7D32")
    if (!is.na(cp)) {
      legend_labels <- c(legend_labels, "Clearing price")
      legend_cols   <- c(legend_cols,   "#B71C1C")
    }
    legend("topright", bty = "n", lwd = 2, col = legend_cols, legend = legend_labels)
  })

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
      sprintf("Round %d closed. Buyers paid: %s | Sellers got: %s | Qty: %d",
              rn,
              if (is.na(result$p_buyer))  "no trade" else sprintf("$%.2f", result$p_buyer),
              if (is.na(result$p_seller)) "no trade" else sprintf("$%.2f", result$p_seller),
              result$quantity),
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

  session$onSessionEnded(function() {
    if (!is.null(conn) && DBI::dbIsValid(conn))
      try(DBI::dbDisconnect(conn), silent = TRUE)
  })
}

shinyApp(ui, server)
