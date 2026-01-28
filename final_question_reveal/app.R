# app.R — FROM SCRATCH
# Final question unlock (collective) + Flex passes + Exam points (individual)
#
# Core rules (as requested):
# - Students start with 5 points, but can earn more (admin grants)
# - 0.5 flex pass = 0.5 exam point (default prices both = 1 point)
# - Collective unlock cost rises by round using schedule: 12,20,30,45,70 (editable)
#
# Credentials CSV must have: user, name, pw_hash, is_admin
# Provide via env: CRED_B64 or CRED_CSV or CRED_PATH

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(shiny, DT, bcrypt, dplyr, tibble, readr, DBI, RSQLite, base64enc, stringr)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

logf <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste(vapply(list(...), as.character, character(1)), collapse = " ")
  cat(ts, "-", msg, "\n", file = stderr()); flush(stderr())
}

# -------------------------
# Credentials
# -------------------------
get_credentials <- function() {
  b64 <- Sys.getenv("CRED_B64", "")
  if (nzchar(b64)) {
    raw <- base64enc::base64decode(b64)
    return(readr::read_csv(raw, show_col_types = FALSE, trim_ws = TRUE))
  }
  csv <- Sys.getenv("CRED_CSV", "")
  if (nzchar(csv)) {
    con <- textConnection(csv); on.exit(close(con), add = TRUE)
    return(readr::read_csv(con, show_col_types = FALSE, trim_ws = TRUE))
  }
  path <- Sys.getenv("CRED_PATH", "")
  if (nzchar(path)) {
    stopifnot(file.exists(path))
    return(readr::read_csv(path, show_col_types = FALSE, trim_ws = TRUE))
  }
  stop("No credentials found: set CRED_B64, CRED_CSV, or CRED_PATH")
}

CRED <- get_credentials()
stopifnot(all(c("user","name","pw_hash","is_admin") %in% names(CRED)))

# -------------------------
# Questions (edit)
# -------------------------
QUESTIONS <- list(
  HTML("<b>Q1</b><br>Opportunity cost: define + example."),
  HTML("<b>Q2</b><br>Marginal thinking: define + example."),
  HTML("<b>Q3</b><br>Comparative advantage: explain."),
  HTML("<b>Q4</b><br>Consumer theory: shifts vs movements.")
)

render_unlocked_questions <- function(n_unlocked) {
  if (n_unlocked <= 0) return(NULL)
  idx <- seq_len(min(n_unlocked, length(QUESTIONS)))
  tagList(
    h5(if (n_unlocked == 1) "Unlocked Question" else sprintf("Unlocked Questions (%d)", n_unlocked)),
    lapply(rev(idx), function(i) wellPanel(div(style="font-size:1.1em; line-height:1.45;", QUESTIONS[[i]])))
  )
}

# -------------------------
# SQLite storage
# -------------------------
app_data_dir <- local({
  dir <- NULL
  function() {
    if (!is.null(dir)) return(dir)
    root <- Sys.getenv("CONNECT_CONTENT_DIR", unset = getwd())
    d <- file.path(root, "data")
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    tf <- file.path(d, ".writetest"); on.exit(unlink(tf, force = TRUE), add = TRUE)
    if (!isTRUE(try(file.create(tf), silent = TRUE))) stop("Data directory not writable: ", d)
    dir <<- normalizePath(d, winslash = "/", mustWork = TRUE)
    dir
  }
})

DATA_DIR <- app_data_dir()
DB_PATH  <- file.path(DATA_DIR, "finalq_fromscratch.sqlite")
logf("DB_PATH:", DB_PATH)

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  conn
}
db_exec  <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

init_db <- function() {
  db_exec("
    CREATE TABLE IF NOT EXISTS users (
      user_id TEXT PRIMARY KEY,
      display_name TEXT,
      is_admin INTEGER DEFAULT 0
    );
  ")

  # Settings is a single row (id=1)
  # question_cost_schedule stored as comma-separated list (e.g., '12,20,30,45,70')
  db_exec("
    CREATE TABLE IF NOT EXISTS settings (
      id INTEGER PRIMARY KEY CHECK (id=1),
      initial_points REAL,
      pledge_step REAL,
      flex_cost REAL,
      exam_point_cost REAL,
      question_cost_schedule TEXT,
      shortfall_policy TEXT
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS game_state (
      id INTEGER PRIMARY KEY CHECK (id=1),
      round INTEGER,
      round_open INTEGER,
      carryover REAL,
      unlocked_questions INTEGER,
      updated_at TEXT DEFAULT (CURRENT_TIMESTAMP)
    );
  ")

  # Collective pledges (only for questions), per user per round
  db_exec("
    CREATE TABLE IF NOT EXISTS pledges (
      user_id TEXT,
      round INTEGER,
      pledge REAL,
      submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (user_id, round)
    );
  ")
  db_exec("CREATE INDEX IF NOT EXISTS ix_pledges_round ON pledges(round);")

  # Ledger (all spending and grants)
  # purpose: 'question', 'flex', 'exam_point', 'grant'
  # amount: positive = spend, negative = grant/credit
  db_exec("
    CREATE TABLE IF NOT EXISTS ledger (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id TEXT,
      round INTEGER,
      purpose TEXT,
      amount REAL,
      meta TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")
  db_exec("CREATE INDEX IF NOT EXISTS ix_ledger_user ON ledger(user_id);")
  db_exec("CREATE INDEX IF NOT EXISTS ix_ledger_purpose ON ledger(purpose);")

  # Seed settings/state if missing
  nset <- db_query("SELECT COUNT(*) n FROM settings WHERE id=1;")$n[1]
  if (is.na(nset) || nset == 0) {
    db_exec("
      INSERT INTO settings(id, initial_points, pledge_step, flex_cost, exam_point_cost, question_cost_schedule, shortfall_policy)
      VALUES(1, 5, 0.5, 1, 1, '12,20,30,45,70', 'bank_all');
    ")
  }

  ngs <- db_query("SELECT COUNT(*) n FROM game_state WHERE id=1;")$n[1]
  if (is.na(ngs) || ngs == 0) {
    db_exec("INSERT INTO game_state(id, round, round_open, carryover, unlocked_questions) VALUES(1, 1, 0, 0, 0);")
  }

  # Upsert roster from CRED
  for (i in seq_len(nrow(CRED))) {
    db_exec("
      INSERT INTO users(user_id, display_name, is_admin)
      VALUES(?, ?, ?)
      ON CONFLICT(user_id) DO UPDATE SET
        display_name = excluded.display_name,
        is_admin     = excluded.is_admin;
    ", list(CRED$user[i], CRED$name[i], as.integer(isTRUE(CRED$is_admin[i]))))
  }
}
init_db()

get_settings <- function() {
  s <- db_query("SELECT * FROM settings WHERE id=1;")
  if (!nrow(s)) stop("Missing settings row.")
  s
}
set_settings <- function(...) {
  dots <- list(...)
  if (!length(dots)) return(invisible(TRUE))
  clauses <- c(); params <- list()
  for (nm in names(dots)) {
    clauses <- c(clauses, sprintf("%s = ?", nm))
    params <- c(params, list(dots[[nm]]))
  }
  db_exec(paste0("UPDATE settings SET ", paste(clauses, collapse=", "), " WHERE id=1;"), params)
  db_exec("UPDATE game_state SET updated_at = CURRENT_TIMESTAMP WHERE id=1;")
  invisible(TRUE)
}

get_state <- function() db_query("SELECT * FROM game_state WHERE id=1;")

set_state <- function(...) {
  dots <- list(...)
  if (!length(dots)) {
    db_exec("UPDATE game_state SET updated_at = CURRENT_TIMESTAMP WHERE id=1;")
    return(invisible(TRUE))
  }
  nm <- names(dots)
  sql <- paste0("UPDATE game_state SET ",
                paste0(nm, " = ?", collapse = ", "),
                ", updated_at = CURRENT_TIMESTAMP WHERE id=1;")
  db_exec(sql, unname(dots))
  invisible(TRUE)
}

# -------------------------
# Costs & accounting
# -------------------------
parse_cost_schedule <- function(x) {
  # Returns numeric vector; ignores empties; keeps order
  parts <- unlist(strsplit(as.character(x %||% ""), ",", fixed = TRUE))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  out <- suppressWarnings(as.numeric(parts))
  out <- out[is.finite(out) & out > 0]
  if (!length(out)) c(12,20,30,45,70) else out
}

question_cost_for_round <- function(round, schedule_text) {
  sched <- parse_cost_schedule(schedule_text)
  r <- as.integer(round %||% 1L)
  if (r <= length(sched)) sched[r] else tail(sched, 1)
}

spent_total <- function(uid) {
  x <- db_query("SELECT COALESCE(SUM(amount),0) AS s FROM ledger WHERE user_id=?;", list(uid))$s[1]
  as.numeric(x %||% 0)
}
remaining_points <- function(uid) {
  s <- get_settings()
  initial <- as.numeric(s$initial_points)
  pmax(0, initial - spent_total(uid))
}

round_pledge <- function(uid, round) {
  x <- db_query("SELECT COALESCE(pledge,0) AS p FROM pledges WHERE user_id=? AND round=?;", list(uid, round))$p[1]
  as.numeric(x %||% 0)
}
round_totals <- function(round) {
  db_query("SELECT COALESCE(SUM(pledge),0) AS pledged, COUNT(*) AS n FROM pledges WHERE round=?;", list(round))
}

compute_unlocks <- function(round, carryover, cost) {
  pledged <- as.numeric(round_totals(round)$pledged[1] %||% 0)
  eff <- pledged + as.numeric(carryover %||% 0)
  units <- floor(eff / cost)
  carry <- eff - units * cost
  list(pledged = pledged, effective = eff, units = as.integer(units), carry = as.numeric(carry))
}

# Flex pass: once per 24 hours
eligible_for_flex <- function(uid) {
  last <- db_query("SELECT MAX(created_at) AS t FROM ledger WHERE user_id=? AND purpose='flex';", list(uid))$t[1]
  if (is.na(last) || !nzchar(last)) return(TRUE)
  dt <- difftime(Sys.time(), as.POSIXct(last, tz="UTC"), units = "hours")
  isTRUE(dt >= 24)
}

admin_student_summary <- function() {
  s <- get_settings()
  init <- as.numeric(s$initial_points %||% 0)

  # Student roster (exclude admins)
  students <- db_query("
    SELECT user_id, display_name
    FROM users
    WHERE COALESCE(is_admin,0)=0
    ORDER BY display_name;
  ")

  if (!nrow(students)) return(tibble())

  # Ledger aggregates by purpose
  led <- db_query("
    SELECT
      user_id,
      purpose,
      COALESCE(SUM(amount),0) AS amt,
      COUNT(*) AS n
    FROM ledger
    GROUP BY user_id, purpose;
  ")

  # Start frame
  out <- students |>
    tibble::as_tibble() |>
    mutate(initial_points = init)

  # Helper to pull sums/counts safely
  get_amt <- function(p) {
    led |> filter(purpose == p) |> select(user_id, amt)
  }
  get_n <- function(p) {
    led |> filter(purpose == p) |> select(user_id, n)
  }

  # Grants are stored as NEGATIVE amounts; convert to positive "points_granted"
  grants_amt <- get_amt("grant") |> mutate(points_granted = -as.numeric(amt)) |> select(user_id, points_granted)

  q_amt   <- get_amt("question")    |> transmute(user_id, points_spent_questions = as.numeric(amt))
  flex_amt<- get_amt("flex")        |> transmute(user_id, points_spent_flex = as.numeric(amt))
  exam_amt<- get_amt("exam_point")  |> transmute(user_id, points_spent_exam = as.numeric(amt))

  flex_n  <- get_n("flex") |> transmute(user_id, flex_passes = as.integer(n))

  # Exam points purchased: parse meta entries like "exam_points=3"
  exam_pts <- db_query("
    SELECT user_id, meta
    FROM ledger
    WHERE purpose='exam_point';
  ")
  exam_pts_sum <- if (nrow(exam_pts)) {
    exam_pts |>
      tibble::as_tibble() |>
      mutate(exam_points = suppressWarnings(as.integer(stringr::str_match(meta, "exam_points\\s*=\\s*([0-9]+)")[,2]))) |>
      mutate(exam_points = ifelse(is.na(exam_points), 0L, exam_points)) |>
      group_by(user_id) |>
      summarise(exam_points = sum(exam_points), .groups = "drop")
  } else {
    tibble(user_id = character(), exam_points = integer())
  }

  out |>
    left_join(grants_amt, by = "user_id") |>
    left_join(q_amt,      by = "user_id") |>
    left_join(flex_amt,   by = "user_id") |>
    left_join(exam_amt,   by = "user_id") |>
    left_join(flex_n,     by = "user_id") |>
    left_join(exam_pts_sum, by = "user_id") |>
    mutate(
      points_granted        = coalesce(points_granted, 0),
      points_spent_questions= coalesce(points_spent_questions, 0),
      points_spent_flex     = coalesce(points_spent_flex, 0),
      points_spent_exam     = coalesce(points_spent_exam, 0),
      flex_passes           = coalesce(flex_passes, 0L),
      exam_points           = coalesce(exam_points, 0L),
      points_spent_total    = points_spent_questions + points_spent_flex + points_spent_exam,
      points_remaining      = initial_points + points_granted - points_spent_total
    ) |>
    select(
      user_id, display_name,
      initial_points, points_granted,
      points_spent_questions, points_spent_flex, points_spent_exam,
      flex_passes, exam_points,
      points_spent_total, points_remaining
    )
}

# -------------------------
# UI
# -------------------------
login_ui <- function(msg = NULL) {
  fluidPage(
    titlePanel("Final Questions + Flex Passes + Exam Points"),
    if (!is.null(msg)) div(style="color:#b00020; font-weight:bold;", msg),
    textInput("login_user", "Username"),
    passwordInput("login_pw", "Password"),
    actionButton("login_btn", "Sign in", class="btn-primary"),
    tags$small("Use the username and password provided by your instructor.")
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

# -------------------------
# Server
# -------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(authed = FALSE, user = NULL, name = NULL, is_admin = FALSE)

  output$authed <- reactive(rv$authed)
  outputOptions(output, "authed", suspendWhenHidden = FALSE)
  output$auth_gate <- renderUI({ if (!rv$authed) login_ui() else NULL })

  observeEvent(input$login_btn, {
    u <- trimws(input$login_user); p <- input$login_pw
    row <- CRED[CRED$user == u, , drop = FALSE]
    ok <- nrow(row) == 1 && bcrypt::checkpw(p, row$pw_hash[1])
    if (!ok) {
      showNotification("Login failed.", type="error")
      return()
    }
    rv$authed <- TRUE
    rv$user <- row$user[1]
    rv$name <- row$name[1]
    rv$is_admin <- isTRUE(row$is_admin[1])
  })

  authed   <- reactive(rv$authed)
  user_id  <- reactive(rv$user)
  name     <- reactive(rv$name)
  is_admin <- reactive(rv$is_admin)

  # Live polling via game_state.updated_at
  state_poll <- reactivePoll(
    1500, session,
    checkFunc = function() db_query("SELECT updated_at FROM game_state WHERE id=1;")$updated_at[1] %||% as.character(Sys.time()),
    valueFunc = function() get_state()
  )
  settings_poll <- reactivePoll(
    1500, session,
    checkFunc = function() db_query("SELECT updated_at FROM game_state WHERE id=1;")$updated_at[1] %||% as.character(Sys.time()),
    valueFunc = function() get_settings()
  )

  # ---------------- Student ----------------
  output$student_ui <- renderUI({
    req(authed())
    fluidPage(
      h4("Status"),
      uiOutput("whoami"),
      tags$hr(),
      uiOutput("student_status"),
      tags$hr(),
      h4("Collective: Pledge to unlock final questions"),
      uiOutput("pledge_box"),
      tags$hr(),
      h4("Individual purchases"),
      uiOutput("individual_box"),
      tags$hr(),
      h4("Your ledger"),
      DTOutput("my_ledger")
    )
  })

  output$whoami <- renderUI({
    req(authed())
    HTML(sprintf("<b>Logged in as:</b> %s (%s)", user_id(), name()))
  })

  output$student_status <- renderUI({
    st <- state_poll()
    s  <- settings_poll()
    cost <- question_cost_for_round(st$round, s$question_cost_schedule)
    ws <- compute_unlocks(st$round, st$carryover, cost)

    tagList(
      p(sprintf("You start with %.2f flex passes (and can earn more). You have %.2f remaining.",
                as.numeric(s$initial_points), remaining_points(user_id()))),
      p(sprintf("Round: %d | Round open: %s", st$round, ifelse(st$round_open == 1, "YES", "NO"))),
      p(sprintf("Cost to unlock a question THIS round: %.2f (schedule: %s)",
                cost, s$question_cost_schedule)),
      p(sprintf("Carryover into this round: %.2f", st$carryover)),
      p(sprintf("This round pledged: %.2f | Effective (pledged + carry): %.2f | Unlocks if closed now: %d",
                ws$pledged, ws$effective, ws$units)),
      p(sprintf("Unlocked so far: %d", st$unlocked_questions)),
      if (st$unlocked_questions > 0) render_unlocked_questions(st$unlocked_questions)
    )
  })

  output$pledge_box <- renderUI({
    req(authed())
    st <- state_poll()
    s  <- settings_poll()

    cap <- remaining_points(user_id())
    cur <- round_pledge(user_id(), st$round)

    if (st$round_open != 1) {
      return(wellPanel(
        strong("Round is closed."),
        p("You can’t change your pledge right now.")
      ))
    }

    wellPanel(
      p(sprintf("Remaining flex passes you can spend overall: %.2f", cap)),
      sliderInput("pledge_amt", "Your pledged flex passes this round (for question unlocks):",
                  min = 0, max = cap, value = min(cur, cap), step = as.numeric(s$cfg_step)),
      actionButton("submit_pledge", "Submit / Update pledge", class = "btn-primary")
    )
  })

  observeEvent(input$submit_pledge, {
    req(authed())
    st <- state_poll()
    s  <- settings_poll()
    if (st$round_open != 1) {
      showNotification("Round is closed.", type="error")
      return()
    }

    cap  <- remaining_points(user_id())
    step <- as.numeric(s$pledge_step)
    raw  <- as.numeric(input$pledge_amt %||% 0)
    snap <- round(raw / step) * step
    new_pledge <- max(0, min(cap, snap))

    db_exec("
      INSERT INTO pledges(user_id, round, pledge, submitted_at)
      VALUES(?, ?, ?, CURRENT_TIMESTAMP)
      ON CONFLICT(user_id, round) DO UPDATE SET
        pledge = excluded.pledge,
        submitted_at = CURRENT_TIMESTAMP;
    ", list(user_id(), as.integer(st$round), as.numeric(new_pledge)))

    set_state() # heartbeat
    showNotification("Pledge saved.", type="message")
  })

  output$individual_box <- renderUI({
    req(authed())
    st <- state_poll()
    s  <- settings_poll()
    cap <- remaining_points(user_id())

    flex_ok <- eligible_for_flex(user_id())

    wellPanel(
      p(sprintf("Remaining flex passes you can spend overall: %.2f", cap)),
      tags$div(
        tags$h5("24 hours"),
        p(sprintf("Cost: %.2f flex passes. Limit: once per 24 hours. (%.2f flex pass = %.2f exam point.)", as.numeric(s$flex_cost), as.numeric(s$flex_cost), as.numeric(s$flex_cost) * 0.5)),
        actionButton("buy_flex", "Buy Flex Pass", class = "btn-success"),
        if (!flex_ok) tags$small(style="color:#b00020;", "Cooldown active: you bought one in the last 24 hours.")
      ),
      tags$hr(),
      tags$div(
        tags$h5("Exam Point"),
        p(sprintf("Cost: %.2f flex passes each. (%.2f exam point = %.2f flex pass.)", as.numeric(s$pledge_step), as.numeric(s$pledge_step), as.numeric(s$pledge_step) * 0.5)),
        numericInput("exam_pts", "How many exam points?", value = 0, min = 0, step = as.numeric(s$pledge_step)),
        actionButton("buy_exam_pts", "Buy Exam Points", class = "btn-success")
      ),
      tags$hr()
    )
  })

  observeEvent(input$buy_flex, {
    req(authed())
    s <- settings_poll()
    cost <- as.numeric(s$flex_cost)

    if (!eligible_for_flex(user_id())) {
      showNotification("You can only buy one flex pass every 24 hours.", type="error")
      return()
    }
    if (remaining_points(user_id()) < cost) {
      showNotification("Not enough points remaining.", type="error")
      return()
    }

    db_exec("
      INSERT INTO ledger(user_id, round, purpose, amount, meta)
      VALUES(?, NULL, 'flex', ?, '24h_flex_pass');
    ", list(user_id(), cost))

    set_state()
    showNotification("Flex pass purchased.", type="message")
  })

  observeEvent(input$buy_exam_pts, {
    req(authed())
    s <- settings_poll()

    n <- as.integer(input$exam_pts %||% 0)
    n <- max(0L, n)

    if (n <= 0) {
      showNotification(sprintf("Choose at least %.2f exam points.", as.numeric(s$cfg_step)), type="warning")
      return()
    }

    unit <- as.numeric(s$exam_point_cost)
    cost <- n * unit

    if (remaining_points(user_id()) < cost) {
      showNotification("Not enough points remaining.", type="error")
      return()
    }

    db_exec("
      INSERT INTO ledger(user_id, round, purpose, amount, meta)
      VALUES(?, NULL, 'exam_point', ?, ?);
    ", list(user_id(), cost, sprintf("exam_points=%d", n)))

    set_state()
    showNotification(sprintf("Purchased %d exam point(s).", n), type="message")
  })

  output$my_ledger <- DT::renderDT({
    req(authed())
    state_poll()

    df <- db_query("
      SELECT created_at, purpose, round, amount, meta
      FROM ledger
      WHERE user_id=?
      ORDER BY datetime(created_at) DESC;
    ", list(user_id()))

    st <- get_state()
    pending <- tibble(
      created_at = NA_character_,
      purpose = "pledge (current round)",
      round = st$round,
      amount = round_pledge(user_id(), st$round),
      meta = if (st$round_open == 1) "pending (not charged yet)" else "round closed"
    )

    out <- bind_rows(pending, df)
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 10))
  })

  # ---------------- Projector ----------------
  output$projector_ui <- renderUI({
    req(authed())
    st <- state_poll()
    s  <- settings_poll()

    if (st$round_open == 1 && !is_admin()) {
      return(wellPanel(
        h4("Projector hidden while round is open"),
        p(sprintf("Round %d is currently open.", st$round))
      ))
    }

    cost <- question_cost_for_round(st$round, s$question_cost_schedule)
    ws <- compute_unlocks(st$round, st$carryover, cost)

    fluidPage(
      h3("Class Progress"),
      p(sprintf("Round: %d | Unlocked: %d | Carryover: %.2f", st$round, st$unlocked_questions, st$carryover)),
      p(sprintf("This round pledged: %.2f | Effective: %.2f | Cost this round: %.2f | Unlocks if closed now: %d",
                ws$pledged, ws$effective, cost, ws$units)),
      tags$hr(),
      if (st$unlocked_questions > 0) render_unlocked_questions(st$unlocked_questions)
    )
  })

  # ---------------- Admin ----------------
  output$admin_ui <- renderUI({
    req(authed())
    if (!is_admin()) return(fluidPage(h4("Admin"), p("You are not an admin.")))

    st <- state_poll()
    s  <- settings_poll()
    cost <- question_cost_for_round(st$round, s$question_cost_schedule)

    fluidPage(
      h4("Admin Controls"),
      tags$hr(),

      wellPanel(
        h5("Settings"),
        fluidRow(
          column(4, numericInput("cfg_initial", "Initial points per student", value = as.numeric(s$initial_points), min = 0)),
          column(4, numericInput("cfg_step", "Pledge step", value = as.numeric(s$pledge_step), min = 0.1)),
          column(4, selectInput("cfg_shortfall", "Shortfall policy", choices = c("bank_all","nocharge"),
                                selected = as.character(s$shortfall_policy)))
        ),
        fluidRow(
          column(4, numericInput("cfg_flex_cost", "Flex pass cost (points)", value = as.numeric(s$flex_cost), min = 0)),
          column(4, numericInput("cfg_exam_cost", "Exam point cost (points)", value = as.numeric(s$exam_point_cost), min = 0)),
          column(4, textInput("cfg_sched", "Question cost schedule (comma-separated by round)",
                              value = as.character(s$question_cost_schedule)))
        ),
        actionButton("apply_settings", "Apply settings", class="btn-secondary")
      ),

      wellPanel(
        h5("Round controls"),
        p(sprintf("Current round: %d | Open: %s | Cost this round: %.2f",
                  st$round, ifelse(st$round_open == 1, "YES", "NO"), cost)),
        fluidRow(
          column(3, actionButton("open_round", "Open round", class="btn-success")),
          column(3, actionButton("close_round", "Close & settle", class="btn-danger")),
          column(3, actionButton("next_round", "Next round", class="btn-primary"))
        )
      ),

      wellPanel(
        h5("Grant points (students can gain more)"),
        fluidRow(
          column(6,
            selectInput("grant_user", "Student",
              choices = {
                us <- db_query("SELECT user_id, display_name FROM users WHERE is_admin=0 ORDER BY display_name;")
                setNames(us$user_id, us$display_name)
              }
            )
          ),
          column(3, numericInput("grant_amt", "Points to grant", value = 1, min = 0, step = 0.5)),
          column(3, br(), actionButton("do_grant", "Grant", class="btn-success"))
        ),
        tags$small("Grants are recorded as negative ledger amounts (credits).")
      ),

      wellPanel(
        h5("Admin export table: balances & purchases"),
        DTOutput("admin_export_table"),
        tags$br(),
        downloadButton("dl_admin_export", "Download balances & purchases (CSV)")
      ),


      wellPanel(
        h5("Exports"),
        downloadButton("dl_pledges", "Download pledges (CSV)"),
        downloadButton("dl_ledger", "Download ledger (CSV)")
      ),

      h5("Live pledges this round"),
      DTOutput("admin_pledges_table"),

      h5("Quick totals (this round)"),
      tableOutput("admin_round_totals")
    )
  })

  observeEvent(input$apply_settings, {
    req(is_admin())
    # validate schedule loosely
    sched <- input$cfg_sched %||% ""
    if (!length(parse_cost_schedule(sched))) {
      showNotification("Bad schedule. Example: 12,20,30,45,70", type="error")
      return()
    }
    set_settings(
      initial_points = as.numeric(input$cfg_initial),
      pledge_step = as.numeric(input$cfg_step),
      flex_cost = as.numeric(input$cfg_flex_cost),
      exam_point_cost = as.numeric(input$cfg_exam_cost),
      question_cost_schedule = as.character(sched),
      shortfall_policy = as.character(input$cfg_shortfall)
    )
    showNotification("Settings updated.", type="message")
  })

  observeEvent(input$do_grant, {
    req(is_admin(), input$grant_user)
    amt <- as.numeric(input$grant_amt %||% 0)
    if (!is.finite(amt) || amt <= 0) {
      showNotification("Grant must be > 0.", type="error")
      return()
    }
    # Negative amount increases remaining points (credit)
    db_exec("
      INSERT INTO ledger(user_id, round, purpose, amount, meta)
      VALUES(?, NULL, 'grant', ?, 'admin_grant');
    ", list(as.character(input$grant_user), -amt))
    set_state()
    showNotification("Granted points.", type="message")
  })

  observeEvent(input$open_round, {
    req(is_admin())
    set_state(round_open = 1)
    showNotification("Round opened.", type="message")
  })

  observeEvent(input$next_round, {
    req(is_admin())
    st <- get_state()
    new_round <- as.integer(st$round) + 1L
    set_state(round = new_round, round_open = 0)
    showNotification(sprintf("Moved to round %d.", new_round), type="message")
  })

  observeEvent(input$close_round, {
    req(is_admin())
    st <- get_state()
    s  <- get_settings()

    if (st$round_open != 1) {
      showNotification("Round already closed.", type="warning")
      return()
    }

    cost <- question_cost_for_round(st$round, s$question_cost_schedule)
    ws <- compute_unlocks(st$round, st$carryover, cost)
    funded <- ws$units > 0

    if (funded || identical(s$shortfall_policy, "bank_all")) {
      # everyone pays their pledge this round into the ledger (spending)
      db_exec("
        INSERT INTO ledger(user_id, round, purpose, amount, meta)
        SELECT user_id, round, 'question', COALESCE(pledge,0), 'round_settlement'
        FROM pledges
        WHERE round=?;
      ", list(as.integer(st$round)))
      new_carry <- ws$carry
    } else {
      # nocharge: nobody pays, carryover unchanged
      new_carry <- as.numeric(st$carryover)
    }

    new_unlocked <- as.integer(st$unlocked_questions) + as.integer(ws$units)

    set_state(
      round_open = 0,
      carryover = as.numeric(new_carry),
      unlocked_questions = new_unlocked
    )

    showNotification(
      if (funded) sprintf("Closed: unlocked %d question(s). Carryover %.2f.", ws$units, new_carry)
      else if (identical(s$shortfall_policy, "bank_all")) sprintf("Closed (not funded): banked pledges. Carryover %.2f.", new_carry)
      else "Closed (not funded): no one charged; carryover unchanged.",
      type = if (funded) "message" else "warning"
    )
  })

  output$admin_pledges_table <- DT::renderDT({
    req(authed(), is_admin())
    st <- state_poll()
    df <- db_query("
      SELECT p.user_id, u.display_name, p.round, p.pledge, p.submitted_at
      FROM pledges p
      LEFT JOIN users u ON u.user_id = p.user_id
      WHERE p.round=?
      ORDER BY p.pledge DESC, p.submitted_at DESC;
    ", list(as.integer(st$round)))
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 12))
  })

  output$admin_round_totals <- renderTable({
    req(authed(), is_admin())
    st <- state_poll()
    s  <- settings_poll()
    cost <- question_cost_for_round(st$round, s$question_cost_schedule)
    ws <- compute_unlocks(st$round, st$carryover, cost)
    data.frame(
      Metric = c("Round", "Round open", "Cost this round", "Carryover", "Pledged", "Effective", "Unlocks if closed"),
      Value  = c(st$round, st$round_open == 1, sprintf("%.2f", cost),
                 sprintf("%.2f", st$carryover), sprintf("%.2f", ws$pledged),
                 sprintf("%.2f", ws$effective), ws$units),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$admin_export_table <- DT::renderDT({
    req(authed(), is_admin())
    state_poll()  # refresh when heartbeat changes
    df <- admin_student_summary()
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 25))
  })

  output$dl_admin_export <- downloadHandler(
    filename = function() sprintf("balances_purchases_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      df <- admin_student_summary()
      readr::write_csv(df, file)
    }
  )

  output$dl_pledges <- downloadHandler(
    filename = function() "pledges.csv",
    content = function(file) {
      df <- db_query("SELECT * FROM pledges ORDER BY round, user_id;")
      readr::write_csv(df, file)
    }
  )
  output$dl_ledger <- downloadHandler(
    filename = function() "ledger.csv",
    content = function(file) {
      df <- db_query("SELECT * FROM ledger ORDER BY datetime(created_at) DESC;")
      readr::write_csv(df, file)
    }
  )

  session$onSessionEnded(function() {
    if (!is.null(conn) && DBI::dbIsValid(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
  })
}

shinyApp(ui, server)
