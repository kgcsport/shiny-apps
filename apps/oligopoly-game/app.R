# app.R --Oligopoly Game (Integrated with Flex Pass DB + Login)
# -------------------------------------------------------------------
# Uses the SAME SQLite database and credential table as your flex-pass app:
#   - users(user_id, display_name, pw_hash, is_admin)
#   - settings(id=1, initial_fp, ...)
#   - ledger(user_id, purpose, amount, meta, created_at, ...)
#
# This app adds new tables:
#   - olig_settings (singleton)
#   - olig_rounds
#   - olig_submissions
#   - olig_payouts (optional audit)
#
# Key behavior:
# - Students must log in using existing flex-pass credentials.
# - Bonus Pot game: student "contribution" is immediately DEBITED from ledger
#   (purpose='oligopoly_contrib'), capped by their current balance.
#   On reveal, each student receives a CREDIT equal to their share of the pot
#   (purpose='grant' with negative amount), rounded to nearest 0.5.
# - PD game: no staking; on reveal, students receive a CREDIT equal to
#   (pd_payoff * pd_scale), rounded to nearest 0.5.
#
# Env vars:
# - CONNECT_CONTENT_DIR (Posit) optional; otherwise uses getwd()
# - DB_PATH_OVERRIDE (optional absolute/relative path to the shared sqlite db)
#
# Packages: shiny, DT, bcrypt, dplyr, DBI, RSQLite, stringr
# -------------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  shiny, DT, bcrypt, dplyr, tibble, DBI, RSQLite, stringr, ggplot2
)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

logf <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste(vapply(list(...), as.character, character(1)), collapse = " ")
  cat(ts, "-", msg, "\n", file = stderr()); flush(stderr())
}

# -------------------------
# DB path (match your flex-pass app default)
# -------------------------
app_data_dir <- local({
  dir <- NULL
  function() {
    if (!is.null(dir)) return(dir)
    root <- Sys.getenv("CONNECT_CONTENT_DIR", unset = getwd())
    d <- file.path(root, "data")
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(d)) stop("Data directory not created: ", d)
    tf <- file.path(d, ".writetest"); on.exit(unlink(tf, force = TRUE), add = TRUE)
    if (!isTRUE(try(file.create(tf), silent = TRUE))) stop("Data directory not writable: ", d)
    dir <<- normalizePath(d, winslash = "/", mustWork = TRUE)
    dir
  }
})

DATA_DIR <- app_data_dir()
DB_PATH  <- Sys.getenv("DB_PATH_OVERRIDE", file.path(DATA_DIR, "finalqdata.sqlite"))
logf("DB_PATH:", DB_PATH)

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  conn
}
db_exec  <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

# -------------------------
# Init new tables for oligopoly module
# -------------------------
init_olig <- function() {
  # settings singleton
  db_exec("
    CREATE TABLE IF NOT EXISTS olig_settings (
      id INTEGER PRIMARY KEY CHECK(id=1),
      current_round INTEGER,
      round_status TEXT,          -- open/closed/revealed
      current_game TEXT,          -- 'pd' or 'bonus'
      bonus_multiplier REAL,
      pd_scale REAL,
      pd_HH_A REAL, pd_HH_B REAL,
      pd_HL_A REAL, pd_HL_B REAL,
      pd_LH_A REAL, pd_LH_B REAL,
      pd_LL_A REAL, pd_LL_B REAL,
      updated_at TEXT DEFAULT (CURRENT_TIMESTAMP)
    );
  ")

  # round table (optional; mostly for audit)
  db_exec("
    CREATE TABLE IF NOT EXISTS olig_rounds (
      round INTEGER PRIMARY KEY,
      game TEXT NOT NULL,
      status TEXT NOT NULL,
      created_at TEXT DEFAULT (CURRENT_TIMESTAMP)
    );
  ")

  # submissions (one per user per round)
  db_exec("
    CREATE TABLE IF NOT EXISTS olig_submissions (
      round INTEGER NOT NULL,
      user_id TEXT NOT NULL,
      display_name TEXT,
      game TEXT NOT NULL,
      action TEXT,               -- pd: High/Low
      contribute REAL,           -- bonus: amount
      created_at TEXT DEFAULT (CURRENT_TIMESTAMP),
      PRIMARY KEY (round, user_id)
    );
  ")
  db_exec("CREATE INDEX IF NOT EXISTS ix_olig_sub_round ON olig_submissions(round);")

  # payout audit
  db_exec("
    CREATE TABLE IF NOT EXISTS olig_payouts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      round INTEGER,
      user_id TEXT,
      game TEXT,
      payout REAL,
      meta TEXT,
      created_at TEXT DEFAULT (CURRENT_TIMESTAMP)
    );
  ")

  # seed defaults if missing
  n <- db_query("SELECT COUNT(*) n FROM olig_settings WHERE id=1;")$n[1]
  if (is.na(n) || n == 0) {
    db_exec("
      INSERT INTO olig_settings(
        id, current_round, round_status, current_game,
        bonus_multiplier, pd_scale,
        pd_HH_A, pd_HH_B, pd_HL_A, pd_HL_B, pd_LH_A, pd_LH_B, pd_LL_A, pd_LL_B
      ) VALUES (
        1, 1, 'open', 'pd',
        1.5, 0.1,
        50, 50, 10, 70, 70, 10, 30, 30
      );
    ")
  }
}
init_olig()

touch_olig <- function() {
  db_exec("UPDATE olig_settings SET updated_at=CURRENT_TIMESTAMP WHERE id=1;")
}
get_olig <- function() db_query("SELECT * FROM olig_settings WHERE id=1;")

round_to_half <- function(x) round(x * 2) / 2

# -------------------------
# Flex-pass accounting (reuse the same logic)
# -------------------------
get_settings <- function() {
  s <- db_query("SELECT * FROM settings WHERE id=1;")
  if (!nrow(s)) stop("Missing settings row in shared DB.")
  s
}

spent_total <- function(uid) {
  x <- db_query("SELECT COALESCE(SUM(amount),0) AS s FROM ledger WHERE user_id=?;", list(uid))$s[1]
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) 0 else x
}

remaining_fp <- function(uid) {
  s <- get_settings()
  initial <- suppressWarnings(as.numeric(s$initial_fp[1]))
  if (!is.finite(initial)) initial <- 5
  # grants in your system are negative ledger amounts; this is already included in SUM(amount)
  pmax(0, initial - spent_total(uid))
}

# -------------------------
# Game logic
# -------------------------
pd_pair_payoffs <- function(olig, subs) {
  # Pair in submission order (created_at); ignore odd last person
  if (!nrow(subs)) return(tibble())
  s <- subs %>%
    arrange(created_at) %>%
    mutate(idx = row_number(),
           pair = ceiling(idx/2),
           role = ifelse(idx %% 2 == 1, "A", "B"))
  if (nrow(s) %% 2 == 1) s <- s %>% slice(1:(nrow(s)-1))

  payoff_for_pair <- function(a_action, b_action) {
    if (a_action == "High" && b_action == "High") return(c(A = olig$pd_HH_A, B = olig$pd_HH_B))
    if (a_action == "High" && b_action == "Low")  return(c(A = olig$pd_HL_A, B = olig$pd_HL_B))
    if (a_action == "Low"  && b_action == "High") return(c(A = olig$pd_LH_A, B = olig$pd_LH_B))
    return(c(A = olig$pd_LL_A, B = olig$pd_LL_B))
  }

  wide <- s %>%
    select(pair, role, user_id, display_name, action) %>%
    tidyr::pivot_wider(names_from = role, values_from = c(user_id, display_name, action))

  out <- wide %>%
    rowwise() %>%
    mutate(
      A_pay = payoff_for_pair(action_A, action_B)[["A"]],
      B_pay = payoff_for_pair(action_A, action_B)[["B"]]
    ) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = c(A_pay, B_pay), names_to = "role_pay", values_to = "payoff") %>%
    mutate(role = ifelse(role_pay == "A_pay", "A", "B"),
           user_id = ifelse(role == "A", user_id_A, user_id_B),
           display_name = ifelse(role == "A", display_name_A, display_name_B),
           action = ifelse(role == "A", action_A, action_B)) %>%
    select(pair, role, user_id, display_name, action, payoff)

  out
}

bonus_shares <- function(olig, subs) {
  if (!nrow(subs)) return(tibble())
  m <- as.numeric(olig$bonus_multiplier[1] %||% 1.5)
  subs <- subs %>% mutate(contribute = as.numeric(contribute %||% 0))
  total <- sum(subs$contribute, na.rm = TRUE)
  pot <- m * total
  share <- ifelse(nrow(subs) > 0, pot / nrow(subs), 0)
  subs %>%
    mutate(total_contrib = total,
           pot_total = pot,
           share_each = share)
}

# Apply payouts into the shared ledger
apply_payouts <- function(round, game) {
  olig <- get_olig()
  subs <- db_query("SELECT * FROM olig_submissions WHERE round=? AND game=?;", list(as.integer(round), as.character(game)))
  if (!nrow(subs)) return(list(ok = FALSE, msg = "No submissions."))

  if (game == "bonus") {
    # Credits: each student gets share_each (rounded to 0.5) as a grant (negative amount)
    shares <- bonus_shares(olig, subs)
    payouts <- shares %>% mutate(payout = round_to_half(share_each)) %>% select(user_id, payout)
    # write to ledger + audit
    for (i in seq_len(nrow(payouts))) {
      uid <- payouts$user_id[i]
      pay <- payouts$payout[i]
      if (!is.finite(pay) || pay <= 0) next
      db_exec("INSERT INTO ledger(user_id, round, purpose, amount, meta)
               VALUES(?, ?, 'grant', ?, ?);",
              list(uid, as.integer(round), -pay, sprintf("oligopoly_bonus_share round=%d", round)))
      db_exec("INSERT INTO olig_payouts(round, user_id, game, payout, meta)
               VALUES(?, ?, ?, ?, ?);",
              list(as.integer(round), uid, "bonus", pay, "share_each"))
    }
    touch_olig()
    return(list(ok = TRUE, msg = sprintf("Applied bonus payouts to %d students.", nrow(payouts))))
  }

  if (game == "pd") {
    pay <- pd_pair_payoffs(olig, subs) %>%
      mutate(payout = round_to_half(as.numeric(payoff) * as.numeric(olig$pd_scale[1] %||% 0.1))) %>%
      select(user_id, payout)
    for (i in seq_len(nrow(pay))) {
      uid <- pay$user_id[i]
      p  <- pay$payout[i]
      if (!is.finite(p) || p <= 0) next
      db_exec("INSERT INTO ledger(user_id, round, purpose, amount, meta)
               VALUES(?, ?, 'grant', ?, ?);",
              list(uid, as.integer(round), -p, sprintf("oligopoly_pd_payout round=%d", round)))
      db_exec("INSERT INTO olig_payouts(round, user_id, game, payout, meta)
               VALUES(?, ?, ?, ?, ?);",
              list(as.integer(round), uid, "pd", p, "pd_scale"))
    }
    touch_olig()
    return(list(ok = TRUE, msg = sprintf("Applied PD payouts to %d students (paired).", nrow(pay))))
  }

  list(ok = FALSE, msg = "Unknown game.")
}

# -------------------------
# Auth UI
# -------------------------
login_ui <- function(msg = NULL) {
  fluidPage(
    titlePanel("Oligopoly Game (Flex Pass Integrated)"),
    if (!is.null(msg)) div(style="color:#b00020; font-weight:bold;", msg),
    textInput("login_user", "Username"),
    passwordInput("login_pw", "Password"),
    actionButton("login_btn", "Sign in", class="btn-primary"),
    tags$small("Use the same username/password as the flex passes app.")
  )
}

ui <- fluidPage(
  uiOutput("auth_gate"),
  conditionalPanel("output.authed",
    tabsetPanel(
      tabPanel("Student", uiOutput("student_ui")),
      tabPanel("Projector", uiOutput("projector_ui")),
      tabPanel("Admin", uiOutput("admin_ui"))
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(authed = FALSE, user = NULL, name = NULL, is_admin = FALSE)

  output$authed <- reactive(rv$authed)
  outputOptions(output, "authed", suspendWhenHidden = FALSE)
  output$auth_gate <- renderUI({ if (!rv$authed) login_ui() else NULL })

  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pw %||% ""

    row <- db_query("SELECT user_id, display_name, is_admin, pw_hash FROM users WHERE user_id=?;", list(u))
    if (nrow(row) != 1) { showNotification("Login failed.", type="error"); return() }

    ph <- row$pw_hash[1] %||% ""
    ok <- tryCatch(bcrypt::checkpw(p, ph), error = function(e) FALSE)
    if (!isTRUE(ok)) { showNotification("Login failed.", type="error"); return() }

    rv$authed <- TRUE
    rv$user <- row$user_id[1]
    rv$name <- row$display_name[1]
    rv$is_admin <- isTRUE(as.integer(row$is_admin[1]) == 1)
  })

  authed <- reactive(rv$authed)
  user_id <- reactive(rv$user)
  name <- reactive(rv$name)
  is_admin <- reactive(rv$is_admin)

  # Poll shared state via olig_settings.updated_at
  olig_poll <- reactivePoll(
    1200, session,
    checkFunc = function() get_olig()$updated_at[1] %||% as.character(Sys.time()),
    valueFunc = function() get_olig()
  )
  subs_poll <- reactivePoll(
    1200, session,
    checkFunc = function() paste0(get_olig()$updated_at[1] %||% "", "|", db_query("SELECT COUNT(*) n FROM olig_submissions WHERE round=(SELECT current_round FROM olig_settings WHERE id=1);")$n[1]),
    valueFunc = function() {
      o <- get_olig()
      subs <- db_query("SELECT * FROM olig_submissions WHERE round=?;", list(as.integer(o$current_round[1])))
      list(olig = o, subs = subs)
    }
  )

  # ---------------- Student UI ----------------
  output$student_ui <- renderUI({
    req(authed())
    st <- subs_poll()
    o <- st$olig

    bal <- remaining_fp(user_id())
    game <- as.character(o$current_game[1])
    status <- as.character(o$round_status[1])

    game_lbl <- if (game == "pd") "Price War (High vs Low)" else "Bonus Pot (Contribute flex passes)"
    tagList(
      h4(sprintf("Logged in as: %s (%s)", user_id(), name())),
      p(sprintf("Flex passes available right now: %.2f", bal)),
      p(sprintf("Round %d | Game: %s | Status: %s", as.integer(o$current_round[1]), game_lbl, status)),
      tags$hr(),
      if (game == "pd") {
        tagList(
          radioButtons("pd_action", "Your choice", choices = c("High","Low"), inline = TRUE),
          actionButton("submit_pd", "Submit", class="btn-primary")
        )
      } else {
        tagList(
          sliderInput("bonus_c", "Contribute flex passes", min = 0, max = max(0, floor(bal*2)/2), value = 0, step = 0.5),
          actionButton("submit_bonus", "Submit (debited immediately)", class="btn-primary"),
          tags$small(sprintf("Multiplier m = %.2f. On reveal, pot = m * total contributions; split evenly.", as.numeric(o$bonus_multiplier[1])))
        )
      },
      tags$hr(),
      uiOutput("student_result")
    )
  })

  # Prevent duplicate submissions per round
  already_submitted <- function(round) {
    x <- db_query("SELECT COUNT(*) n FROM olig_submissions WHERE round=? AND user_id=?;",
                  list(as.integer(round), as.character(user_id())))
    as.integer(x$n[1]) > 0
  }

  observeEvent(input$submit_pd, {
    req(authed())
    st <- subs_poll(); o <- st$olig
    req(as.character(o$round_status[1]) == "open")
    req(as.character(o$current_game[1]) == "pd")
    req(input$pd_action %in% c("High","Low"))
    if (already_submitted(o$current_round[1])) {
      showNotification("You already submitted this round.", type="warning"); return()
    }
    db_exec("INSERT INTO olig_submissions(round, user_id, display_name, game, action, contribute)
             VALUES(?, ?, ?, 'pd', ?, NULL);",
            list(as.integer(o$current_round[1]), as.character(user_id()), as.character(name()), as.character(input$pd_action)))
    touch_olig()
    showNotification("Submitted.", type="message")
  })

  observeEvent(input$submit_bonus, {
    req(authed())
    st <- subs_poll(); o <- st$olig
    req(as.character(o$round_status[1]) == "open")
    req(as.character(o$current_game[1]) == "bonus")

    if (already_submitted(o$current_round[1])) {
      showNotification("You already submitted this round.", type="warning"); return()
    }

    c <- round_to_half(as.numeric(input$bonus_c %||% 0))
    if (!is.finite(c) || c < 0) c <- 0

    bal <- remaining_fp(user_id())
    if (c > bal + 1e-9) {
      showNotification("Not enough flex passes.", type="error"); return()
    }

    # Debit immediately
    if (c > 0) {
      db_exec("INSERT INTO ledger(user_id, round, purpose, amount, meta)
               VALUES(?, ?, 'oligopoly_contrib', ?, ?);",
              list(as.character(user_id()), as.integer(o$current_round[1]), c,
                   sprintf("oligopoly_bonus_contribution round=%d", as.integer(o$current_round[1]))))
    }

    db_exec("INSERT INTO olig_submissions(round, user_id, display_name, game, action, contribute)
             VALUES(?, ?, ?, 'bonus', NULL, ?);",
            list(as.integer(o$current_round[1]), as.character(user_id()), as.character(name()), c))
    touch_olig()
    showNotification("Submitted. Your contribution was deducted immediately.", type="message")
  })

  output$student_result <- renderUI({
    req(authed())
    st <- subs_poll(); o <- st$olig; subs <- st$subs
    if (as.character(o$round_status[1]) != "revealed") {
      return(p("Results will appear after the instructor reveals the round."))
    }
    r <- as.integer(o$current_round[1])
    game <- as.character(o$current_game[1])

    my_sub <- subs %>% filter(user_id == !!user_id())
    if (!nrow(my_sub)) return(p("No submission recorded for you this round."))

    if (game == "bonus") {
      shares <- bonus_shares(o, subs %>% filter(game == "bonus"))
      me <- shares %>% filter(user_id == !!user_id())
      if (!nrow(me)) return(p("No bonus result found."))
      payout <- round_to_half(me$share_each[1])
      tagList(
        h5("Your result"),
        p(sprintf("You contributed: %.2f", as.numeric(me$contribute[1]))),
        p(sprintf("Your share of pot: %.2f (credited to your flex pass balance; rounded to nearest 0.5)", payout))
      )
    } else {
      pay <- pd_pair_payoffs(o, subs %>% filter(game == "pd"))
      me <- pay %>% filter(user_id == !!user_id())
      if (!nrow(me)) return(p("Odd number of submissions: last unpaired student has no PD payoff this round."))
      payout <- round_to_half(as.numeric(me$payoff[1]) * as.numeric(o$pd_scale[1] %||% 0.1))
      tagList(
        h5("Your result"),
        p(sprintf("You chose: %s", as.character(me$action[1]))),
        p(sprintf("Pair payoff (points): %.2f", as.numeric(me$payoff[1]))),
        p(sprintf("Payout to flex passes: %.2f (credited; rounded to nearest 0.5)", payout))
      )
    }
  })

  # ---------------- Projector UI ----------------
  output$projector_ui <- renderUI({
    req(authed())
    st <- subs_poll(); o <- st$olig; subs <- st$subs
    r <- as.integer(o$current_round[1])
    cur_game <- as.character(o$current_game[1])
    status <- as.character(o$round_status[1])
    game_lbl <- if (cur_game == "pd") "Price War (PD)" else "Bonus Pot"
    n <- nrow(subs %>% filter(round == r, game == cur_game))

    tagList(
      h3(sprintf("Round %d --%s", r, game_lbl)),
      p(sprintf("Status: %s | Submissions: %d", status, n)),
      if (cur_game == "pd") {
        tagList(
          p(sprintf("High: %d | Low: %d",
                    sum(subs$action == "High", na.rm = TRUE),
                    sum(subs$action == "Low", na.rm = TRUE))),
          if (status == "revealed") {
            tagList(
              h4("Pair Outcome Counts"),
              tableOutput("proj_pd_counts")
            )
          }
        )
      } else {
        tagList(
          p(sprintf("Total contributed: %.2f | Multiplier m: %.2f",
                    sum(as.numeric(subs$contribute), na.rm = TRUE),
                    as.numeric(o$bonus_multiplier[1]))),
          plotOutput("proj_bonus_plot", height = "350px"),
          if (status == "revealed") {
            o_data <- get_olig()
            total_c <- sum(as.numeric(subs$contribute), na.rm = TRUE)
            m <- as.numeric(o_data$bonus_multiplier[1] %||% 1.5)
            pot <- m * total_c
            share <- if (n > 0) round_to_half(pot / n) else 0
            tagList(
              h4("Results"),
              p(sprintf("Pot total: %.2f | Each student receives: %.2f flex passes", pot, share))
            )
          }
        )
      }
    )
  })

  # PD projector: anonymous pair-outcome counts (HH, HL, LH, LL)
  output$proj_pd_counts <- renderTable({
    st <- subs_poll(); o <- st$olig; subs <- st$subs
    r <- as.integer(o$current_round[1])
    pd_subs <- subs %>% filter(round == r, game == "pd")
    if (!nrow(pd_subs)) return(data.frame(Outcome = character(), Count = integer()))

    pay <- pd_pair_payoffs(o, pd_subs)
    if (!nrow(pay)) return(data.frame(Outcome = character(), Count = integer()))

    # Build outcome labels per pair
    pair_outcomes <- pay %>%
      group_by(pair) %>%
      summarise(outcome = paste(sort(action), collapse = "-"), .groups = "drop")

    counts <- pair_outcomes %>%
      count(outcome, name = "Count") %>%
      rename(Outcome = outcome)

    counts
  }, striped = TRUE, bordered = TRUE, align = "lc")

  # Bonus Pot projector: ggplot histogram of contributions (no names)
  output$proj_bonus_plot <- renderPlot({
    st <- subs_poll(); o <- st$olig; subs <- st$subs
    r <- as.integer(o$current_round[1])
    df <- subs %>% filter(round == r, game == "bonus") %>%
      mutate(contribute = as.numeric(contribute))

    req(nrow(df) > 0)

    ggplot(df, aes(x = contribute)) +
      geom_histogram(fill = "#0073C2FF", color = "white", binwidth = 0.5, boundary = 0) +
      labs(title = "Distribution of Contributions",
           x = "Flex Passes Contributed",
           y = "Number of Students") +
      scale_x_continuous(breaks = seq(0, max(df$contribute, na.rm = TRUE) + 0.5, by = 0.5)) +
      theme_minimal(base_size = 16)
  })

  # ---------------- Admin UI ----------------
  output$admin_ui <- renderUI({
    req(authed())
    if (!is_admin()) return(fluidPage(h4("Admin"), p("You are not an admin.")))

    o <- olig_poll()
    fluidPage(
      h4("Admin controls"),
      p(sprintf("Current round: %d | Game: %s | Status: %s",
                as.integer(o$current_round[1]), as.character(o$current_game[1]), as.character(o$round_status[1]))),
      tags$hr(),
      wellPanel(
        h5("Round + status"),
        numericInput("adm_round", "Round number", value = as.integer(o$current_round[1]), min = 1, step = 1),
        selectInput("adm_game", "Game", choices = c("Price War (PD)" = "pd", "Bonus Pot" = "bonus"),
                    selected = as.character(o$current_game[1])),
        selectInput("adm_status", "Status", choices = c("open","closed","revealed"),
                    selected = as.character(o$round_status[1])),
        actionButton("adm_apply", "Apply", class="btn-primary"),
        actionButton("adm_clear", "Clear submissions (this round)", class="btn-danger")
      ),
      wellPanel(
        h5("Parameters"),
        numericInput("adm_m", "Bonus multiplier m", value = as.numeric(o$bonus_multiplier[1]), min = 1, step = 0.1),
        numericInput("adm_pd_scale", "PD scale -> flex passes per 'point'", value = as.numeric(o$pd_scale[1]), min = 0, step = 0.01),
        tags$hr(),
        h5("PD payoff matrix (A,B)"),
        fluidRow(
          column(6, numericInput("pd_HH_A", "HH: A", value = as.numeric(o$pd_HH_A[1]))),
          column(6, numericInput("pd_HH_B", "HH: B", value = as.numeric(o$pd_HH_B[1])))
        ),
        fluidRow(
          column(6, numericInput("pd_HL_A", "HL: A", value = as.numeric(o$pd_HL_A[1]))),
          column(6, numericInput("pd_HL_B", "HL: B", value = as.numeric(o$pd_HL_B[1])))
        ),
        fluidRow(
          column(6, numericInput("pd_LH_A", "LH: A", value = as.numeric(o$pd_LH_A[1]))),
          column(6, numericInput("pd_LH_B", "LH: B", value = as.numeric(o$pd_LH_B[1])))
        ),
        fluidRow(
          column(6, numericInput("pd_LL_A", "LL: A", value = as.numeric(o$pd_LL_A[1]))),
          column(6, numericInput("pd_LL_B", "LL: B", value = as.numeric(o$pd_LL_B[1])))
        ),
        actionButton("adm_save_params", "Save parameters", class="btn-secondary")
      ),
      wellPanel(
        h5("Settlement"),
        p("After you set status to REVEALED, click the button below to post payouts back to student balances."),
        actionButton("adm_payout", "Apply payouts to ledger (one-time)", class="btn-success"),
        tags$small("This will CREDIT students using purpose='grant' (negative amounts). Bonus contributions were already debited on submission.")
      ),
      tags$hr(),
      h5("Payout audit (latest 50)"),
      DTOutput("payout_audit")
    )
  })

  observeEvent(input$adm_apply, {
    req(is_admin())
    db_exec("UPDATE olig_settings SET current_round=?, current_game=?, round_status=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;",
            list(as.integer(input$adm_round), as.character(input$adm_game), as.character(input$adm_status)))
    # ensure round row exists
    db_exec("INSERT OR IGNORE INTO olig_rounds(round, game, status) VALUES(?, ?, ?);",
            list(as.integer(input$adm_round), as.character(input$adm_game), as.character(input$adm_status)))
    db_exec("UPDATE olig_rounds SET game=?, status=? WHERE round=?;",
            list(as.character(input$adm_game), as.character(input$adm_status), as.integer(input$adm_round)))
    showNotification("Updated.", type="message")
  })

  observeEvent(input$adm_clear, {
    req(is_admin())
    o <- get_olig()
    r <- as.integer(o$current_round[1])
    # IMPORTANT: do not automatically refund contributions here; clearing is "hard reset".
    db_exec("DELETE FROM olig_submissions WHERE round=?;", list(r))
    touch_olig()
    showNotification("Cleared submissions for this round. (Contrib debits remain in ledger.)", type="warning")
  })

  observeEvent(input$adm_save_params, {
    req(is_admin())
    db_exec("
      UPDATE olig_settings SET
        bonus_multiplier=?, pd_scale=?,
        pd_HH_A=?, pd_HH_B=?,
        pd_HL_A=?, pd_HL_B=?,
        pd_LH_A=?, pd_LH_B=?,
        pd_LL_A=?, pd_LL_B=?,
        updated_at=CURRENT_TIMESTAMP
      WHERE id=1;
    ", list(
      as.numeric(input$adm_m), as.numeric(input$adm_pd_scale),
      as.numeric(input$pd_HH_A), as.numeric(input$pd_HH_B),
      as.numeric(input$pd_HL_A), as.numeric(input$pd_HL_B),
      as.numeric(input$pd_LH_A), as.numeric(input$pd_LH_B),
      as.numeric(input$pd_LL_A), as.numeric(input$pd_LL_B)
    ))
    showNotification("Parameters saved.", type="message")
  })

  observeEvent(input$adm_payout, {
    req(is_admin())
    o <- get_olig()
    r <- as.integer(o$current_round[1])
    game <- as.character(o$current_game[1])
    if (as.character(o$round_status[1]) != "revealed") {
      showNotification("Set status to 'revealed' first.", type="error"); return()
    }
    # crude idempotency: block if payouts already exist for round+game
    already <- db_query("SELECT COUNT(*) n FROM olig_payouts WHERE round=? AND game=?;", list(r, game))$n[1]
    if (as.integer(already) > 0) {
      showNotification("Payouts already applied for this round/game.", type="warning"); return()
    }
    res <- apply_payouts(r, game)
    showNotification(res$msg, type = if (isTRUE(res$ok)) "message" else "error")
  })

  output$payout_audit <- DT::renderDT({
    req(authed(), is_admin())
    df <- db_query("SELECT created_at, round, game, user_id, payout, meta FROM olig_payouts ORDER BY id DESC LIMIT 50;")
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10))
  })

  session$onSessionEnded(function() {
    if (!is.null(conn) && DBI::dbIsValid(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
  })
}

shinyApp(ui, server)
