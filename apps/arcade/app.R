try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
library(shiny)
library(DBI)
library(RSQLite)
library(bcrypt)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Database ──────────────────────────────────────────────────────────────────
CONNECT_CONTENT_DIR <- Sys.getenv("CONNECT_CONTENT_DIR", getwd())
DB_PATH <- file.path(CONNECT_CONTENT_DIR, "data", "finalqdata.sqlite")

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    DBI::dbExecute(conn, "PRAGMA journal_mode = WAL;")
    DBI::dbExecute(conn, "PRAGMA busy_timeout = 5000;")
  }
  conn
}
db_query <- function(sql, params = NULL) {
  tryCatch(
    if (is.null(params)) DBI::dbGetQuery(get_con(), sql)
    else DBI::dbGetQuery(get_con(), sql, params = params),
    error = function(e) { message("db_query: ", e$message); data.frame() }
  )
}
db_exec <- function(sql, params = NULL) {
  tryCatch(
    if (is.null(params)) DBI::dbExecute(get_con(), sql)
    else DBI::dbExecute(get_con(), sql, params = params),
    error = function(e) { message("db_exec: ", e$message); -1L }
  )
}

# ── Table init ────────────────────────────────────────────────────────────────
db_exec("
  CREATE TABLE IF NOT EXISTS arcade_state (
    id          INTEGER PRIMARY KEY CHECK (id = 1),
    active_game TEXT,
    updated_at  TEXT DEFAULT CURRENT_TIMESTAMP
  );
")
if (!db_query("SELECT COUNT(*) n FROM arcade_state WHERE id=1;")$n[1])
  db_exec("INSERT INTO arcade_state(id, active_game) VALUES(1, NULL);")

# Ensure olig tables exist so the arcade works even before coordination-games runs
db_exec("
  CREATE TABLE IF NOT EXISTS olig_settings (
    id INTEGER PRIMARY KEY,
    current_round INTEGER DEFAULT 1,
    round_status TEXT DEFAULT 'pending',
    current_game TEXT DEFAULT 'bonus',
    bonus_multiplier REAL DEFAULT 1.5,
    pd_payoff_points REAL DEFAULT 10,
    pd_scale REAL DEFAULT 0.1,
    contrib_cap REAL DEFAULT 0,
    use_section_size INTEGER DEFAULT 1,
    section TEXT DEFAULT '',
    updated_at TEXT DEFAULT CURRENT_TIMESTAMP
  );
")
db_exec("CREATE TABLE IF NOT EXISTS olig_submissions (
  round     INTEGER NOT NULL,
  user_id   TEXT    NOT NULL,
  section   TEXT    DEFAULT 'default',
  choice    TEXT,
  contribute REAL,
  PRIMARY KEY (round, user_id)
);")
db_exec("CREATE TABLE IF NOT EXISTS olig_payouts (
  id      INTEGER PRIMARY KEY AUTOINCREMENT,
  round   INTEGER,
  user_id TEXT,
  game    TEXT,
  payout  REAL,
  meta    TEXT,
  section TEXT DEFAULT 'default'
);")
db_exec("CREATE TABLE IF NOT EXISTS pledges (
  user_id      TEXT,
  exam_id      TEXT DEFAULT 'exam1',
  round        INTEGER,
  pledge       REAL,
  submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, exam_id, round)
);")

# ── Game catalog ──────────────────────────────────────────────────────────────
# type:
#   "session"  — live during class only; stays visible but inactive otherwise
#   "semester" — persistent all-semester tool
#   "either"   — can be activated for a session slot OR used between classes
# embedded = TRUE  → arcade renders game UI inline when active
# embedded = FALSE → arcade shows a Launch card linking to the standalone app
GAMES <- list(
  # ── Either/or: session slot OR semester use ──────────────────────────────
  list(id = "bonus_pot",        type = "either",
       label = "Bonus Pot",     embedded = TRUE,
       desc = "Contribute flex passes to a shared pot. The group earns back more when participation is high — but individual incentives push the other way."),
  list(id = "prisoners_dilemma", type = "either",
       label = "Prisoner's Dilemma", embedded = TRUE,
       desc = "Cooperate or defect? See how individual incentives produce outcomes that are collectively worse."),
  list(id = "price_war",        type = "either",
       label = "Price War",     embedded = TRUE,
       desc = "Set prices in a duopoly. Can you sustain collusion, or does competition drive prices to the floor?"),
  list(id = "supply-auction-game", type = "either",
       label = "Supply Auction", embedded = FALSE, url = "/supply-auction-game/",
       desc = "Bid in a live ascending-price auction. Win units at the market-clearing price."),
  list(id = "review-quiz",      type = "either",
       label = "Review Quiz",   embedded = FALSE, url = "/review-quiz/",
       desc = "Answer quiz questions and see the live class histogram. Used periodically through the semester."),
  # ── Semester-long: persistent all-semester ───────────────────────────────
  list(id = "price-index",      type = "semester",
       label = "Price Index",   embedded = FALSE, url = "/price-index/",
       desc = "Build a basket of goods and track prices across waves to measure your personal inflation rate."),
  list(id = "flex-pass-app",    type = "semester",
       label = "Flex Pass Accounting", embedded = FALSE, url = "/final_question_reveal/",
       desc = "See the full exam-question unlock panel, purchase exam points, and view your complete ledger history."),
  # ── Session-only: live in class, no lasting footprint ────────────────────
  list(id = "excise-tax-game",  type = "session",
       label = "Excise Tax Market", embedded = FALSE, url = "/excise-tax-game/",
       desc = "Trade in a call market before and after an excise tax. See where the burden lands."),
  list(id = "sloman-trading-game", type = "session",
       label = "Sloman Trading Game", embedded = FALSE, url = "/sloman-trading-game/",
       desc = "Produce shapes and see how market prices respond to your team's supply decisions."),
  list(id = "airplanes-game",   type = "session",
       label = "Airplanes",     embedded = FALSE, url = "/airplanes-game/",
       desc = "Enter production data in a live classroom experiment on division of labour."),
  list(id = "club-insurance-game", type = "session",
       label = "Clubs & Insurance", embedded = FALSE, url = "/club-insurance-game/",
       desc = "Choose your insurance level in a group risk pool and explore adverse selection.")
)

game_info <- function(id) Find(function(g) g$id == id, GAMES)

# ── CSS ───────────────────────────────────────────────────────────────────────
ARCADE_CSS <- "
body { font-family: system-ui, -apple-system, sans-serif; background: #f4f5f7; margin: 0; }

/* Header */
.arc-header {
  background: #951829; color: #fff;
  padding: .7rem 1.5rem;
  display: flex; align-items: center; gap: .75rem;
  margin-bottom: 0;
}
.arc-title  { font-size: 1.3rem; font-weight: 700; flex: 1; letter-spacing: .02em; }
.arc-name   { font-size: .88rem; opacity: .85; }
.arc-bal    { font-size: .88rem; background: rgba(255,255,255,.18);
              padding: .2rem .65rem; border-radius: 999px; font-weight: 600; }
.arc-signout { background: rgba(255,255,255,.15); color: #fff;
               border: 1px solid rgba(255,255,255,.4); font-size: .82rem;
               padding: .25rem .6rem; border-radius: 6px; cursor: pointer; }
.arc-signout:hover { background: rgba(255,255,255,.28); }

/* Page body */
.arc-body { max-width: 860px; margin: 0 auto; padding: 1.25rem 1rem 3rem; }

/* Nav tabs */
.nav-tabs { border-bottom: 2px solid #e0e0e0; margin-bottom: 1.25rem; }
.nav-tabs .nav-link        { color: #555; border: none; padding: .55rem .9rem; }
.nav-tabs .nav-link.active { color: #951829; border-bottom: 2px solid #951829;
                              font-weight: 600; margin-bottom: -2px; }
.nav-tabs .nav-link:hover  { color: #951829; }

/* Active game slot */
.slot-card {
  background: #fff; border-radius: 12px; padding: 1.4rem 1.5rem;
  box-shadow: 0 2px 8px rgba(0,0,0,.07); margin-bottom: 1.4rem;
}
.slot-header { font-size: .72rem; font-weight: 700; color: #951829;
               text-transform: uppercase; letter-spacing: .1em; margin-bottom: .8rem; }
.no-game { color: #aaa; text-align: center; padding: 1.75rem 0; font-style: italic; }

/* Launch card (linked games in slot) */
.launch-card { border: 2px solid #951829; border-radius: 10px;
               padding: 1.25rem 1.5rem; display: flex; align-items: center; gap: 1.25rem; }
.launch-info  { flex: 1; }
.launch-title { font-size: 1.2rem; font-weight: 700; margin-bottom: .35rem; }
.launch-desc  { color: #555; font-size: .9rem; }
.btn-launch   { background: #951829; color: #fff; padding: .55rem 1.3rem;
                border: none; border-radius: 8px; font-size: .95rem; font-weight: 600;
                text-decoration: none; white-space: nowrap; }
.btn-launch:hover { background: #7a1320; color: #fff; text-decoration: none; }

/* Game cards — shared base */
.game-card { background: #fff; border-radius: 10px; padding: .9rem 1.1rem;
             border: 1px solid #e8e8e8; margin-bottom: .5rem;
             display: flex; align-items: center; gap: .9rem; }
.game-card-text  { flex: 1; }
.game-card-label { font-weight: 600; font-size: .98rem; }
.game-card-desc  { color: #666; font-size: .83rem; margin-top: .15rem; }
.badge-live { background: #951829; color: #fff; font-size: .7rem;
              padding: .15rem .45rem; border-radius: 999px; vertical-align: middle; margin-left: .4rem; }
.arcade-only { color: #bbb; font-size: .8rem; white-space: nowrap; }

/* Session-only cards — faded when inactive */
.game-card-session        { opacity: .55; }
.game-card-session.is-live { opacity: 1; }

/* Semester panel grid */
.semester-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
                 gap: .65rem; margin-bottom: .5rem; }
.semester-card { background: #fff; border: 1px solid #e8e8e8; border-radius: 10px;
                 padding: 1rem 1.1rem; display: flex; flex-direction: column; gap: .35rem; }
.semester-card-label { font-weight: 600; font-size: .95rem; }
.semester-card-desc  { color: #666; font-size: .82rem; flex: 1; }
.semester-card-foot  { display: flex; align-items: center; justify-content: flex-end;
                       margin-top: .4rem; gap: .5rem; }

/* Wallet */
.bal-big   { font-size: 2.4rem; font-weight: 700; color: #951829; line-height: 1.1; }
.bal-label { color: #888; font-size: .8rem; margin-bottom: .1rem; }
.pending-pledge { background: #fff8e1; border: 1px solid #ffe082; border-radius: 8px;
                  padding: .5rem .85rem; font-size: .85rem; color: #795548;
                  margin-top: .5rem; }
.cr { color: #1a6e3c; }
.dr { color: #b00020; }

/* Profile */
.profile-panel { background:#fff; border-radius:10px; padding:1.25rem;
                 border:1px solid #e8e8e8; height:100%; }

/* Section labels */
.sec-label {
  font-size: .72rem; font-weight: 700; color: #951829;
  text-transform: uppercase; letter-spacing: .08em;
  border-bottom: 1px solid #e8e8e8; padding-bottom: .3rem;
  margin: 1.1rem 0 .7rem;
}

/* Login */
.login-page { max-width: 400px; margin: 5rem auto; padding: 0 1rem; }
.login-card { background: #fff; border-radius: 12px; padding: 2rem 2.25rem;
              box-shadow: 0 3px 14px rgba(0,0,0,.1); }
.login-logo { font-size: 1.6rem; font-weight: 700; color: #951829;
              margin-bottom: 1.5rem; text-align: center; }
.login-note { font-size: .82rem; color: #999; text-align: center; margin-top: .75rem; }
.btn-block  { width: 100%; }
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$title("CORE Arcade"),
    tags$style(HTML(ARCADE_CSS))
  ),
  uiOutput("root_ui")
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    authed   = FALSE,
    user_id  = NULL,
    name     = NULL,
    section  = NULL,
    is_admin = FALSE
  )

  # ── Root UI ──────────────────────────────────────────────────────────────────
  output$root_ui <- renderUI({
    if (!rv$authed) {
      div(class = "login-page",
        div(class = "login-card",
          div(class = "login-logo", "🎮 CORE Arcade"),
          if (!is.null(input$login_err) && nzchar(input$login_err %||% ""))
            div(class = "alert alert-danger", input$login_err),
          textInput("login_user", NULL, placeholder = "Username"),
          passwordInput("login_pw", NULL, placeholder = "Password"),
          actionButton("login_btn", "Sign in", class = "btn btn-primary btn-block"),
          tags$p(class = "login-note", "Use the credentials from your instructor.")
        )
      )
    } else {
      tagList(
        # Header bar
        div(class = "arc-header",
          div(class = "arc-title", "🎮 CORE Arcade"),
          uiOutput("header_widgets", inline = TRUE),
          actionButton("logout_btn", "Sign out", class = "arc-signout")
        ),
        div(class = "arc-body",
          tabsetPanel(id = "arc_tabs", type = "tabs",
            tabPanel("Play",    br(), uiOutput("play_tab")),
            tabPanel("Wallet",  br(), uiOutput("wallet_tab")),
            tabPanel("Profile", br(), uiOutput("profile_tab")),
            tabPanel("Games",   br(), uiOutput("games_tab")),
            uiOutput("admin_tab_panel")
          )
        )
      )
    }
  })

  output$header_widgets <- renderUI({
    req(rv$authed)
    bal <- wallet_bal()
    tagList(
      span(class = "arc-name", rv$name),
      span(class = "arc-bal",  sprintf("%.1f FP", bal))
    )
  })

  # ── Auth ─────────────────────────────────────────────────────────────────────
  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pw %||% ""
    if (!nzchar(u) || !nzchar(p)) {
      showNotification("Enter username and password.", type = "error"); return()
    }
    row <- db_query(
      "SELECT user_id, display_name, pw_hash, is_admin, section
       FROM users WHERE LOWER(user_id) = LOWER(?);", list(u))
    if (!nrow(row) || !bcrypt::checkpw(p, row$pw_hash[1])) {
      showNotification("Incorrect username or password.", type = "error"); return()
    }
    rv$authed   <- TRUE
    rv$user_id  <- row$user_id[1]
    rv$name     <- coalesce_str(row$display_name[1], row$user_id[1])
    rv$section  <- row$section[1] %||% ""
    rv$is_admin <- isTRUE(as.integer(row$is_admin[1]) == 1L)
  })

  observeEvent(input$logout_btn, {
    rv$authed <- FALSE; rv$user_id <- NULL; rv$name <- NULL
    rv$section <- NULL; rv$is_admin <- FALSE
  })

  coalesce_str <- function(a, b) if (!is.na(a) && nzchar(a %||% "")) a else b

  # ── Polls ─────────────────────────────────────────────────────────────────────
  # 1 — arcade_state: which game the admin activated
  arcade_poll <- reactivePoll(3000, session,
    checkFunc = function()
      db_query("SELECT updated_at FROM arcade_state WHERE id=1;")$updated_at[1] %||% "",
    valueFunc = function()
      db_query("SELECT * FROM arcade_state WHERE id=1;")
  )

  # 2 — olig state: round status, current game (Bonus Pot / PD / Price War)
  olig_poll <- reactivePoll(3000, session,
    checkFunc = function()
      db_query("SELECT updated_at FROM olig_settings WHERE id=1;")$updated_at[1] %||% "",
    valueFunc = function() {
      list(
        settings = db_query("SELECT * FROM olig_settings WHERE id=1;"),
        my_sub   = if (!is.null(rv$user_id))
          db_query(
            "SELECT s.*, p.payout
             FROM olig_submissions s
             LEFT JOIN olig_payouts p ON p.round = s.round AND p.user_id = s.user_id
             WHERE s.user_id = ?
             ORDER BY s.round DESC LIMIT 1;",
            list(rv$user_id))
          else data.frame()
      )
    }
  )

  # 3 — wallet + pledge state
  wallet_poll <- reactivePoll(6000, session,
    checkFunc = function() {
      if (is.null(rv$user_id)) return("")
      db_query("SELECT MAX(created_at) ts FROM ledger WHERE user_id=?;",
               list(rv$user_id))$ts[1] %||% ""
    },
    valueFunc = function() {
      if (is.null(rv$user_id)) return(list(ledger = data.frame(), gs = data.frame()))
      list(
        ledger = db_query(
          "SELECT amount, purpose, meta, created_at FROM ledger
           WHERE user_id = ? ORDER BY created_at DESC LIMIT 30;",
          list(rv$user_id)),
        gs = db_query("SELECT gs.*, es.round_open, es.round AS ex_round
                        FROM game_state gs
                        LEFT JOIN exam_state es ON es.exam_id = gs.active_exam
                        WHERE gs.id = 1;"),
        step = db_query("SELECT pledge_step FROM settings WHERE id=1;")$pledge_step[1] %||% 0.5
      )
    }
  )

  wallet_bal <- reactive({
    wp <- wallet_poll()
    if (!length(wp$ledger) || !nrow(wp$ledger)) return(0)
    as.numeric(sum(wp$ledger$amount, na.rm = TRUE))
  })

  # ── Play tab — three sections ─────────────────────────────────────────────────
  output$play_tab <- renderUI({
    req(rv$authed)
    active     <- arcade_poll()$active_game[1] %||% ""
    has_active <- nzchar(active)

    semester_games <- Filter(function(g) g$type %in% c("semester", "either"), GAMES)
    session_games  <- Filter(function(g) g$type == "session",  GAMES)

    tagList(
      # ── Active slot (only shown when something is running) ──
      if (has_active)
        div(class = "slot-card",
          div(class = "slot-header", "▶ Active Now"),
          uiOutput("active_slot_inner")
        ),

      # ── Semester / either-or resources ──
      div(class = "sec-label", "Semester Resources"),
      div(class = "semester-grid",
        lapply(semester_games, function(g) {
          is_live <- identical(g$id, active)
          div(class = "semester-card",
            div(class = "semester-card-label", g$label,
                if (is_live) span(class = "badge-live", "LIVE")),
            div(class = "semester-card-desc", g$desc),
            div(class = "semester-card-foot",
              if (is_live && g$embedded)
                span(style = "color:#951829;font-size:.82rem;", "↑ Active above")
              else if (g$embedded)
                span(style = "color:#bbb;font-size:.82rem;", "Instructor-activated")
              else
                tags$a(href = g$url, target = "_blank",
                       class = "btn btn-sm btn-outline-secondary", "Open →")
            )
          )
        })
      ),

      # ── Session-only games ──
      div(class = "sec-label", "Session Games"),
      tags$p(style = "color:#888;font-size:.85rem;margin-top:-.4rem;margin-bottom:.6rem;",
             "Played live during class. Inactive until your instructor starts one."),
      tagList(lapply(session_games, function(g) {
        is_live <- identical(g$id, active)
        div(class = paste("game-card game-card-session", if (is_live) "is-live"),
          div(class = "game-card-text",
            div(class = "game-card-label", g$label,
                if (is_live) span(class = "badge-live", "LIVE")),
            div(class = "game-card-desc", g$desc)
          ),
          if (is_live)
            tags$a(href = g$url, target = "_blank",
                   class = "btn btn-sm btn-primary", "Launch →")
          else
            tags$a(href = g$url, target = "_blank",
                   class = "btn btn-sm btn-outline-secondary",
                   style = "opacity:.6;", "Preview →")
        )
      }))
    )
  })

  output$active_slot_inner <- renderUI({
    req(rv$authed)
    active <- arcade_poll()$active_game[1] %||% ""
    if (!nzchar(active)) return(div(class = "no-game", "No game active."))

    ginfo <- game_info(active)
    if (is.null(ginfo)) return(div(class = "no-game", "Unknown game."))

    if (ginfo$embedded) {
      switch(active,
        bonus_pot         = uiOutput("embedded_bonus_pot"),
        prisoners_dilemma = uiOutput("embedded_pd"),
        price_war         = uiOutput("embedded_pd"),
        div(class = "no-game", "Embedded UI coming soon.")
      )
    } else {
      div(class = "launch-card",
        div(class = "launch-info",
          div(class = "launch-title", ginfo$label),
          div(class = "launch-desc",  ginfo$desc)
        ),
        tags$a(class = "btn-launch", href = ginfo$url, target = "_blank", "Launch →")
      )
    }
  })

  # ── Embedded: Bonus Pot ───────────────────────────────────────────────────────
  output$embedded_bonus_pot <- renderUI({
    req(rv$authed)
    op     <- olig_poll()
    s      <- op$settings
    if (!nrow(s)) return(div(class = "no-game", "Bonus Pot not configured yet."))

    status <- s$round_status[1] %||% "pending"
    round  <- as.integer(s$current_round[1] %||% 1L)
    mult   <- as.numeric(s$bonus_multiplier[1] %||% 1.5)
    cap    <- as.numeric(s$contrib_cap[1] %||% 0)
    bal    <- wallet_bal()
    sub    <- op$my_sub

    prev_contrib <- if (nrow(sub) && as.integer(sub$round[1]) == round)
      as.numeric(sub$contribute[1] %||% 0) else 0
    max_c <- if (cap > 0) min(cap, floor(bal * 2) / 2) else floor(bal * 2) / 2

    tagList(
      tags$p(
        tags$strong("Round "), round, " · ",
        tags$strong("Status: "),
        span(style = if (status == "open") "color:#1a6e3c;font-weight:600;"
                     else "color:#b00020;font-weight:600;", toupper(status)),
        " · ", tags$strong("Multiplier: "), sprintf("%.1f×", mult)
      ),
      if (status == "open") {
        tagList(
          tags$p(style = "color:#555;font-size:.9em;",
            "Decide how many flex passes to contribute.",
            "If the group contributes generously, everyone earns back more than they put in — but individual incentives cut the other way."),
          fluidRow(
            column(5,
              numericInput("bp_contrib", "Your contribution (FP):",
                           value = prev_contrib, min = 0,
                           max   = max(0, max_c), step = 0.5)),
            column(4, tags$br(), tags$br(),
                   actionButton("bp_submit", "Submit", class = "btn btn-primary"))
          ),
          tags$p(style = "color:#888;font-size:.82em;",
            sprintf("Balance: %.1f FP%s",
                    bal, if (cap > 0) sprintf("  ·  Cap: %.1f FP/round", cap) else ""))
        )
      } else if (status == "closed") {
        div(class = "alert alert-warning", "Round is closed. Results coming soon.")
      } else if (status == "revealed") {
        payout <- if (nrow(sub) && as.integer(sub$round[1]) == round)
          sub$payout[1] else NA
        div(class = "alert alert-success",
          tags$strong("Round revealed! "),
          if (!is.na(payout)) sprintf("Your payout: %.1f FP.", as.numeric(payout))
          else "Check your Wallet for the credit.")
      } else {
        div(class = "no-game", "Round not open yet.")
      }
    )
  })

  observeEvent(input$bp_submit, {
    req(rv$authed, rv$user_id)
    op  <- isolate(olig_poll())
    s   <- op$settings
    if (!nrow(s) || s$round_status[1] != "open") {
      showNotification("Round is not open.", type = "error"); return()
    }
    contrib <- as.numeric(input$bp_contrib %||% 0)
    if (is.na(contrib) || contrib < 0) {
      showNotification("Enter a valid contribution.", type = "error"); return()
    }
    bal <- isolate(wallet_bal())
    if (contrib > bal) {
      showNotification(sprintf("Not enough FP (balance: %.1f).", bal), type = "error"); return()
    }
    cap <- as.numeric(s$contrib_cap[1] %||% 0)
    if (cap > 0 && contrib > cap) {
      showNotification(sprintf("Exceeds round cap of %.1f FP.", cap), type = "error"); return()
    }
    db_exec(
      "INSERT INTO olig_submissions(round, user_id, section, choice, contribute)
       VALUES(?,?,?,?,?)
       ON CONFLICT(round, user_id) DO UPDATE
         SET contribute = excluded.contribute, section = excluded.section;",
      list(as.integer(s$current_round[1]), rv$user_id, rv$section %||% "",
           "contribute", contrib))
    db_exec("UPDATE olig_settings SET updated_at = CURRENT_TIMESTAMP WHERE id=1;")
    showNotification(sprintf("Submitted %.1f FP contribution.", contrib), type = "message")
  })

  # ── Embedded: Prisoner's Dilemma + Price War ──────────────────────────────────
  output$embedded_pd <- renderUI({
    req(rv$authed)
    op     <- olig_poll()
    s      <- op$settings
    if (!nrow(s)) return(div(class = "no-game", "Game not configured."))

    active    <- isolate(arcade_poll())$active_game[1] %||% ""
    is_pw     <- identical(active, "price_war")
    status    <- s$round_status[1] %||% "pending"
    round     <- as.integer(s$current_round[1] %||% 1L)
    scale     <- as.numeric(s$pd_scale[1] %||% 0.1)
    pts       <- as.numeric(s$pd_payoff_points[1] %||% 10)
    sub       <- op$my_sub
    prev      <- if (nrow(sub) && as.integer(sub$round[1]) == round)
      as.character(sub$choice[1] %||% "") else ""

    c_lbl <- if (is_pw) "Low Price (compete)" else "Defect"
    d_lbl <- if (is_pw) "High Price (collude)" else "Cooperate"

    tagList(
      tags$p(
        tags$strong("Round "), round, " · ",
        tags$strong("Status: "),
        span(style = if (status == "open") "color:#1a6e3c;font-weight:600;"
                     else "color:#b00020;font-weight:600;", toupper(status))
      ),
      if (status == "open") {
        tagList(
          tags$p(style = "color:#555;font-size:.9em;",
            if (is_pw)
              "Choose your pricing strategy. Both firms choosing High earns more collectively — but one firm can always do better by undercutting."
            else
              "Choose to cooperate or defect. Both cooperating pays more overall — but defecting pays more for you individually."),
          radioButtons("pd_choice", "Your choice:",
                       choices  = c(d_lbl, c_lbl),
                       selected = if (nzchar(prev)) prev else character(0),
                       inline   = TRUE),
          actionButton("pd_submit", "Submit", class = "btn btn-primary"),
          tags$p(style = "color:#888;font-size:.82em; margin-top:.5rem;",
                 sprintf("Payoffs scale: %.1f × %.1f pts = %.1f FP per unit.", pts, scale, pts * scale))
        )
      } else if (status == "revealed") {
        payout <- if (nrow(sub) && as.integer(sub$round[1]) == round)
          sub$payout[1] else NA
        div(class = "alert alert-success",
          tags$strong("Round revealed! "),
          if (!is.na(payout)) sprintf("Your payout: %.1f FP.", as.numeric(payout))
          else "Check your Wallet for the credit.")
      } else {
        div(class = "alert alert-warning", "Round closed. Results coming soon.")
      }
    )
  })

  observeEvent(input$pd_submit, {
    req(rv$authed, rv$user_id)
    op <- isolate(olig_poll())
    s  <- op$settings
    if (!nrow(s) || s$round_status[1] != "open") {
      showNotification("Round is not open.", type = "error"); return()
    }
    ch <- input$pd_choice
    if (is.null(ch) || !nzchar(ch %||% "")) {
      showNotification("Make a choice first.", type = "error"); return()
    }
    db_exec(
      "INSERT INTO olig_submissions(round, user_id, section, choice)
       VALUES(?,?,?,?)
       ON CONFLICT(round, user_id) DO UPDATE
         SET choice = excluded.choice, section = excluded.section;",
      list(as.integer(s$current_round[1]), rv$user_id, rv$section %||% "", ch))
    db_exec("UPDATE olig_settings SET updated_at = CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Choice submitted.", type = "message")
  })

  # ── Wallet tab ────────────────────────────────────────────────────────────────
  output$wallet_tab <- renderUI({
    req(rv$authed)
    wp  <- wallet_poll()
    bal <- wallet_bal()
    gs  <- wp$gs
    step <- as.numeric(wp$step %||% 0.5)

    # Pending pledge for the current exam round
    pending_pledge <- if (nrow(gs)) {
      pid <- db_query(
        "SELECT pledge FROM pledges WHERE user_id=? AND exam_id=? AND round=?;",
        list(rv$user_id,
             gs$active_exam[1] %||% "exam1",
             as.integer(gs$ex_round[1] %||% 1L)))$pledge[1]
      as.numeric(pid %||% 0)
    } else 0

    round_open <- nrow(gs) && isTRUE(as.integer(gs$round_open[1]) == 1L)

    tagList(
      fluidRow(
        column(4,
          wellPanel(
            div(class = "bal-label", "Flex Pass Balance"),
            div(class = "bal-big",   sprintf("%.1f", bal)),
            tags$small(style = "color:#888;", "FP  (confirmed)"),
            if (pending_pledge > 0)
              div(class = "pending-pledge",
                  icon("clock-o"),
                  sprintf(" %.1f FP pledged — pending round close", pending_pledge))
          )
        ),
        column(8,
          wellPanel(
            tags$strong("Flex Pass Pledge"),
            if (!round_open) {
              tags$p(style = "color:#888;margin-bottom:0;",
                     if (!nrow(gs)) "Pledging not set up yet."
                     else "Pledging is not open right now.")
            } else {
              tagList(
                tags$p(style = "color:#555;font-size:.88em;",
                  "Pledge flex passes toward unlocking the next exam question.",
                  if (pending_pledge > 0)
                    sprintf(" You've already pledged %.1f FP this round.", pending_pledge)
                ),
                fluidRow(
                  column(6,
                    numericInput("pledge_amt", "Amount (FP):",
                                 value = step,
                                 min   = step,
                                 max   = max(step, floor(bal * 2) / 2),
                                 step  = step)),
                  column(6, tags$br(), tags$br(),
                         actionButton("pledge_btn", "Pledge", class = "btn btn-primary"))
                )
              )
            }
          )
        )
      ),

      div(class = "sec-label", "Transaction History"),
      if (nrow(wp$ledger)) {
        tags$table(class = "table table-sm",
          tags$thead(tags$tr(
            tags$th("Date"), tags$th("Purpose"), tags$th("Note"),
            tags$th(style = "text-align:right;", "Amount")
          )),
          tags$tbody(lapply(seq_len(nrow(wp$ledger)), function(i) {
            r   <- wp$ledger[i, ]
            cls <- if (r$amount >= 0) "cr" else "dr"
            tags$tr(class = cls,
              tags$td(format(as.POSIXct(r$created_at), "%b %d %H:%M")),
              tags$td(r$purpose %||% ""),
              tags$td(style = "color:#888;font-size:.83em;", r$meta %||% ""),
              tags$td(style = "text-align:right;font-weight:600;",
                      sprintf("%+.1f", r$amount))
            )
          }))
        )
      } else {
        div(style = "color:#999;", "No transactions yet.")
      }
    )
  })

  observeEvent(input$pledge_btn, {
    req(rv$authed, rv$user_id)
    wp  <- isolate(wallet_poll())
    gs  <- wp$gs
    if (!nrow(gs) || !isTRUE(as.integer(gs$round_open[1]) == 1L)) {
      showNotification("Pledging is not open.", type = "error"); return()
    }
    amt <- as.numeric(input$pledge_amt %||% 0)
    if (is.na(amt) || amt <= 0) {
      showNotification("Enter a pledge amount.", type = "error"); return()
    }
    bal <- isolate(wallet_bal())
    if (amt > bal) {
      showNotification(sprintf("Insufficient balance (%.1f FP).", bal), type = "error"); return()
    }
    exam_id <- gs$active_exam[1] %||% "exam1"
    round   <- as.integer(gs$ex_round[1] %||% 1L)
    # Upsert pledge (replaces existing; ledger debit happens in final_question_reveal on close)
    db_exec(
      "INSERT INTO pledges(user_id, exam_id, round, pledge)
       VALUES(?,?,?,?)
       ON CONFLICT(user_id, exam_id, round) DO UPDATE
         SET pledge = excluded.pledge, submitted_at = CURRENT_TIMESTAMP;",
      list(rv$user_id, exam_id, round, amt))
    showNotification(sprintf("Pledge of %.1f FP submitted.", amt), type = "message")
  })

  # ── Profile tab ───────────────────────────────────────────────────────────────
  output$profile_tab <- renderUI({
    req(rv$authed)
    # Most-recent job entries for this student (matched by display_name)
    job_rows <- db_query(
      "SELECT job, logged_date FROM job_log WHERE display_name = ?
       ORDER BY created_at DESC LIMIT 5;",
      list(rv$name))

    tagList(
      fluidRow(
        column(6,
          div(class = "profile-panel",
            tags$h6(style = "color:#951829;font-weight:700;", "Display Name"),
            textInput("profile_name", NULL, value = rv$name, width = "100%"),
            actionButton("save_name_btn", "Save", class = "btn btn-primary"),
            tags$p(style = "color:#888;font-size:.82em;margin-top:.5rem;margin-bottom:0;",
                   "This is the name your instructor and classmates see.")
          )
        ),
        column(6,
          div(class = "profile-panel",
            tags$h6(style = "color:#951829;font-weight:700;", "Account Info"),
            tags$p(tags$strong("Username: "), rv$user_id),
            if (nzchar(rv$section %||% ""))
              tags$p(tags$strong("Section: "), rv$section),
            if (nrow(job_rows)) {
              tagList(
                tags$hr(style = "margin:.5rem 0;"),
                tags$p(tags$strong("Class Job History:")),
                tags$ul(style = "padding-left:1.1rem;margin-bottom:0;",
                  lapply(seq_len(nrow(job_rows)), function(i)
                    tags$li(job_rows$job[i],
                            tags$small(style = "color:#999;",
                                       paste0(" — ", job_rows$logged_date[i])))
                  )
                )
              )
            } else {
              tags$p(style = "color:#999;font-size:.9em;", "No job assignments yet.")
            }
          )
        )
      )
    )
  })

  observeEvent(input$save_name_btn, {
    req(rv$authed, rv$user_id)
    nm <- trimws(input$profile_name %||% "")
    if (!nzchar(nm)) {
      showNotification("Name cannot be blank.", type = "error"); return()
    }
    db_exec("UPDATE users SET display_name = ? WHERE user_id = ?;",
            list(nm, rv$user_id))
    rv$name <- nm
    showNotification("Display name updated.", type = "message")
  })

  # ── Games tab ──────────────────────────────────────────────────────────────────
  output$games_tab <- renderUI({
    req(rv$authed)

    game_section <- function(type_id, heading, note) {
      games <- Filter(function(g) g$type == type_id, GAMES)
      tagList(
        div(class = "sec-label", heading),
        tags$p(style = "color:#888;font-size:.85rem;margin-top:-.4rem;margin-bottom:.6rem;", note),
        tagList(lapply(games, function(g) {
          wellPanel(style = "padding:.8rem 1rem;",
            fluidRow(
              column(9,
                tags$strong(g$label),
                tags$p(style = "color:#555;font-size:.87em;margin-bottom:0;", g$desc)
              ),
              column(3, style = "text-align:right;padding-top:.3rem;",
                if (!g$embedded)
                  tags$a(href = g$url, target = "_blank",
                         class = "btn btn-sm btn-outline-secondary", "Open →")
                else
                  span(style = "color:#951829;font-size:.82rem;", "Plays in arcade")
              )
            )
          )
        }))
      )
    }

    tagList(
      game_section("either",   "Either / Or",
        "Can be activated for a live session or left open for use between classes."),
      game_section("semester", "Semester-Long",
        "Persistent tools available throughout the semester."),
      game_section("session",  "Session Only",
        "Played live during class. No lasting footprint — just the experience.")
    )
  })

  # ── Admin tab ──────────────────────────────────────────────────────────────────
  output$admin_tab_panel <- renderUI({
    if (!rv$is_admin) return(NULL)
    tabPanel("Admin", br(), uiOutput("admin_content"))
  })

  output$admin_content <- renderUI({
    req(rv$is_admin)
    as     <- arcade_poll()
    active <- as$active_game[1] %||% ""
    op     <- olig_poll()
    s      <- op$settings

    make_group <- function(type_id, heading) {
      gs <- Filter(function(g) g$type == type_id, GAMES)
      if (!length(gs)) return(NULL)
      setNames(sapply(gs, `[[`, "id"),
               paste0(sapply(gs, `[[`, "label"), " [", heading, "]"))
    }
    all_game_choices <- c(
      list("(none)" = ""),
      make_group("either",   "either/or"),
      make_group("semester", "semester"),
      make_group("session",  "session")
    )

    tagList(
      fluidRow(
        # Active game selector
        column(5,
          wellPanel(
            tags$h6(style = "font-weight:700;color:#951829;", "Active Game Slot"),
            selectInput("admin_game_sel", "Which game is active now?",
                        choices = all_game_choices, selected = active, width = "100%"),
            actionButton("set_active_btn", "Set active game", class = "btn btn-warning"),
            tags$p(style = "font-size:.82em;color:#888;margin-top:.5rem;margin-bottom:0;",
                   "Students see this immediately on their Play tab.")
          )
        ),
        # Coordination game quick controls
        column(7,
          wellPanel(
            tags$h6(style = "font-weight:700;color:#951829;", "Coordination Game Controls"),
            if (!nrow(s)) {
              tags$p(style = "color:#999;", "Run coordination-games once to initialize settings.")
            } else {
              tagList(
                tags$p(
                  tags$strong("Game: "), toupper(s$current_game[1] %||% "—"), "  ",
                  tags$strong("Round: "), s$current_round[1], "  ",
                  tags$strong("Status: "),
                  span(style = if (s$round_status[1] == "open") "color:#1a6e3c;" else "color:#b00020;",
                       toupper(s$round_status[1]))
                ),
                fluidRow(
                  column(4, actionButton("adm_open",   "Open",   class = "btn btn-success btn-sm btn-block")),
                  column(4, actionButton("adm_close",  "Close",  class = "btn btn-warning btn-sm btn-block")),
                  column(4, actionButton("adm_reveal", "Reveal", class = "btn btn-danger  btn-sm btn-block"))
                ),
                tags$p(style = "font-size:.8em;color:#999;margin-top:.5rem;margin-bottom:0;",
                       "For full setup (multiplier, section, payouts) use the Coordination Games app.")
              )
            }
          )
        )
      ),

      div(class = "sec-label", "Student Wallet Balances"),
      uiOutput("admin_balances")
    )
  })

  output$admin_balances <- renderUI({
    req(rv$is_admin)
    rows <- db_query("
      SELECT u.display_name, u.user_id, u.section,
             COALESCE(SUM(l.amount), 0) balance
      FROM users u
      LEFT JOIN ledger l ON l.user_id = u.user_id
      WHERE (u.is_admin IS NULL OR u.is_admin = 0)
      GROUP BY u.user_id
      ORDER BY u.section, u.display_name;")
    if (!nrow(rows)) return(div(style = "color:#999;", "No students found."))
    tags$table(class = "table table-sm table-hover",
      tags$thead(tags$tr(
        tags$th("Name"), tags$th("Username"), tags$th("Section"),
        tags$th(style = "text-align:right;", "Balance (FP)")
      )),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        tags$tr(
          tags$td(r$display_name %||% r$user_id),
          tags$td(style = "color:#888;font-size:.85em;", r$user_id),
          tags$td(r$section %||% ""),
          tags$td(style = "text-align:right;font-weight:600;",
                  sprintf("%.1f", r$balance))
        )
      }))
    )
  })

  observeEvent(input$set_active_btn, {
    req(rv$is_admin)
    g   <- input$admin_game_sel
    val <- if (is.null(g) || !nzchar(g %||% "")) "NULL" else paste0("'", g, "'")
    db_exec(paste0(
      "UPDATE arcade_state SET active_game = ", val,
      ", updated_at = CURRENT_TIMESTAMP WHERE id = 1;"))
    lbl <- if (val == "NULL") "cleared" else g
    showNotification(paste("Active game:", lbl), type = "message")
  })

  observeEvent(input$adm_open, {
    req(rv$is_admin)
    db_exec("UPDATE olig_settings SET round_status='open', updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Round opened.", type = "message")
  })
  observeEvent(input$adm_close, {
    req(rv$is_admin)
    db_exec("UPDATE olig_settings SET round_status='closed', updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Round closed. Run payouts in the Coordination Games app.", type = "warning", duration = 6)
  })
  observeEvent(input$adm_reveal, {
    req(rv$is_admin)
    db_exec("UPDATE olig_settings SET round_status='revealed', updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Status set to revealed. Run payouts in the Coordination Games app first.", type = "warning", duration = 6)
  })

}

shinyApp(ui, server)
