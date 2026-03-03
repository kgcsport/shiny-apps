# app.R -- Coordination Games (Bonus Pot + Price War / PD)
# -------------------------------------------------------------------
# Integrated with the shared Flex Pass SQLite DB + Login.
# Section-aware: sections are stored in the `sections` DB table with
# their enrolled class size. The bonus-pot denominator is either the
# section's class size OR the submitter count -- no other values.
#
# Uses the SAME SQLite database and credential table as the flex-pass app:
#   - users(user_id, display_name, pw_hash, is_admin)
#   - settings(id=1, initial_fp, ...)
#   - ledger(user_id, purpose, amount, meta, created_at, ...)
#
# This app adds/manages:
#   - sections                     <- NEW: section_id, section_name, class_size
#   - olig_settings (singleton)   <- section + use_section_size flag
#   - olig_rounds
#   - olig_submissions             <- section-tagged
#   - olig_payouts                 <- section-tagged
#
# Key behavior:
# - Students log in with existing flex-pass credentials.
# - Bonus Pot: contribution is immediately DEBITED (purpose='oligopoly_contrib').
#   On reveal each student in the section gets a CREDIT = pot / class_size
#   (or / submitter count if class_size is 0/NULL), rounded to nearest 0.5.
# - Price War (PD): no staking; payoff = pd_payoff_points * pd_scale, rounded 0.5.
#
# Env vars:
# - CONNECT_CONTENT_DIR (Posit) optional; otherwise uses getwd()
# - DB_PATH_OVERRIDE (optional absolute/relative path to the shared sqlite db)
#
# Packages: shiny, DT, bcrypt, dplyr, DBI, RSQLite, stringr, ggplot2, tidyr
# -------------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  shiny, DT, bcrypt, dplyr, tibble, DBI, RSQLite, stringr, ggplot2, tidyr,
  googledrive, googlesheets4, digest
)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

logf <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste(vapply(list(...), as.character, character(1)), collapse = " ")
  cat(ts, "-", msg, "\n", file = stderr()); flush(stderr())
}

# -------------------------
# DB path
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
# Init tables for coordination games module
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

  # Migrate: add section + use_section_size if not present (safe to run repeatedly)
  tryCatch(
    db_exec("ALTER TABLE olig_settings ADD COLUMN section TEXT DEFAULT '';"),
    error = function(e) NULL
  )
  # use_section_size: 1 = divide pot by section's enrolled count from users table
  #                   0 = divide pot by number of students who submitted
  tryCatch(
    db_exec("ALTER TABLE olig_settings ADD COLUMN use_section_size INTEGER DEFAULT 1;"),
    error = function(e) NULL
  )
  # legacy class_size column kept for DB compat but no longer used by UI
  tryCatch(
    db_exec("ALTER TABLE olig_settings ADD COLUMN class_size INTEGER;"),
    error = function(e) NULL
  )
  # contrib_cap: max flex passes a student can wager per bonus round (0 = no cap)
  tryCatch(
    db_exec("ALTER TABLE olig_settings ADD COLUMN contrib_cap REAL DEFAULT 0;"),
    error = function(e) NULL
  )

  # round audit
  db_exec("
    CREATE TABLE IF NOT EXISTS olig_rounds (
      round INTEGER PRIMARY KEY,
      game TEXT NOT NULL,
      status TEXT NOT NULL,
      created_at TEXT DEFAULT (CURRENT_TIMESTAMP)
    );
  ")

  # submissions (one per user per round; section-tagged)
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

  # Migrate: add section to submissions
  tryCatch(
    db_exec("ALTER TABLE olig_submissions ADD COLUMN section TEXT DEFAULT 'default';"),
    error = function(e) NULL
  )

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

  # Migrate: add section to payouts (for per-section idempotency)
  tryCatch(
    db_exec("ALTER TABLE olig_payouts ADD COLUMN section TEXT DEFAULT 'default';"),
    error = function(e) NULL
  )

  # Seed defaults if missing
  n <- db_query("SELECT COUNT(*) n FROM olig_settings WHERE id=1;")$n[1]
  if (is.na(n) || n == 0) {
    db_exec("
      INSERT INTO olig_settings(
        id, current_round, round_status, current_game,
        bonus_multiplier, pd_scale,
        pd_HH_A, pd_HH_B, pd_HL_A, pd_HL_B, pd_LH_A, pd_LH_B, pd_LL_A, pd_LL_B,
        section, use_section_size
      ) VALUES (
        1, 1, 'open', 'pd',
        1.5, 0.1,
        50, 50, 10, 70, 70, 10, 30, 30,
        '', 1
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

# Current section id stored in settings (empty string = none selected)
olig_section <- function(olig) as.character(olig$section[1] %||% "")

# Does the shared users table have a 'section' column?
users_has_section_col <- function() {
  cols <- tryCatch(db_query("PRAGMA table_info(users);"), error = function(e) data.frame())
  "section" %in% (if (nrow(cols) > 0) cols$name else character(0))
}

# Distinct sections + enrolled counts from the users table
sections_from_users <- function() {
  db_query("
    SELECT section, COUNT(*) AS class_size
    FROM users
    WHERE section IS NOT NULL AND section != ''
    GROUP BY section
    ORDER BY section;
  ")
}

# Named vector suitable for selectInput (value = section id, name = display label)
sections_choices <- function() {
  secs <- sections_from_users()
  if (!nrow(secs)) return(setNames("", "(no sections in users table)"))
  setNames(secs$section,
           sprintf("%s  (%d enrolled)", secs$section, secs$class_size))
}

# Enrolled count for a section from the users table; NA if not found
section_class_size <- function(section_id) {
  if (!nzchar(section_id %||% "")) return(NA_integer_)
  x <- db_query("SELECT COUNT(*) AS n FROM users WHERE section=?;", list(section_id))
  cs <- suppressWarnings(as.integer(x$n[1]))
  if (is.na(cs) || cs <= 0) NA_integer_ else cs
}

# -------------------------
# Google backup (Sheets + Drive)
# -------------------------
BACKUP_SHEET_ID <- Sys.getenv("FLEX_PASS_SHEET_ID", "")
DRIVE_FOLDER_ID <- Sys.getenv("FLEX_PASS_FOLDER_ID", "")

get_gs_cred_path <- function() {
  p <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (nzchar(p) && file.exists(p)) return(p)
  js <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
  if (nzchar(js)) {
    tf <- tempfile(fileext = ".json")
    writeLines(js, tf)
    return(tf)
  }
  ""
}

google_auth <- function() {
  cred <- get_gs_cred_path()
  if (!nzchar(cred) || !file.exists(cred)) return(FALSE)
  tryCatch({
    googledrive::drive_auth(path = cred, scopes = c(
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/spreadsheets"
    ))
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    TRUE
  }, error = function(e) {
    logf("google_auth():", conditionMessage(e))
    FALSE
  })
}

overwrite_ws <- function(ss, sheet_name, df) {
  tabs <- tryCatch(googlesheets4::sheet_names(ss), error = function(e) character(0))
  if (sheet_name %in% tabs) {
    tryCatch(googlesheets4::sheet_delete(ss, sheet_name), error = function(e) NULL)
  }
  googlesheets4::sheet_add(ss, sheet_name)
  googlesheets4::range_write(ss, data = df, sheet = sheet_name, range = "A1")
}

backup_to_sheets <- function() {
  if (!nzchar(BACKUP_SHEET_ID)) return(list(ok = FALSE, msg = "FLEX_PASS_SHEET_ID not set."))
  if (!google_auth()) return(list(ok = FALSE, msg = "Google auth failed."))

  tryCatch(googledrive::drive_get(googledrive::as_id(BACKUP_SHEET_ID)), error = function(e) {
    stop("No access to FLEX_PASS_SHEET_ID: ", conditionMessage(e))
  })

  overwrite_ws(BACKUP_SHEET_ID, "olig_settings",
    db_query("SELECT * FROM olig_settings;"))
  overwrite_ws(BACKUP_SHEET_ID, "olig_rounds",
    db_query("SELECT * FROM olig_rounds ORDER BY round;"))
  overwrite_ws(BACKUP_SHEET_ID, "olig_submissions",
    db_query("SELECT * FROM olig_submissions ORDER BY round, section, user_id;"))
  overwrite_ws(BACKUP_SHEET_ID, "olig_payouts",
    db_query("SELECT * FROM olig_payouts ORDER BY id DESC;"))

  list(ok = TRUE, msg = "Sheets backup complete (4 coordination-games tabs).")
}

.last_db_hash <- NULL
hash_db <- function() {
  parts <- c(DB_PATH, paste0(DB_PATH, "-wal"), paste0(DB_PATH, "-shm"))
  parts <- parts[file.exists(parts)]
  if (!length(parts)) return(NA_character_)
  raw <- unlist(lapply(parts, function(f) readBin(f, what = "raw", n = file.info(f)$size)))
  digest::digest(raw, algo = "xxhash64")
}

db_changed_since_last_backup <- function() {
  new_hash <- hash_db()
  if (is.null(.last_db_hash) || !identical(new_hash, .last_db_hash)) {
    .last_db_hash <<- new_hash
    TRUE
  } else {
    FALSE
  }
}

backup_db_to_drive <- function() {
  if (!google_auth()) return(list(ok = FALSE, msg = "Google auth failed."))
  if (!nzchar(DRIVE_FOLDER_ID)) return(list(ok = FALSE, msg = "FLEX_PASS_FOLDER_ID not set."))

  tryCatch(googledrive::drive_get(googledrive::as_id(DRIVE_FOLDER_ID)), error = function(e) {
    stop("drive_get(folder) failed: ", conditionMessage(e))
  })

  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  try(DBI::dbExecute(con, "PRAGMA wal_checkpoint(FULL);"), silent = TRUE)

  files <- c(DB_PATH, paste0(DB_PATH, "-wal"), paste0(DB_PATH, "-shm"))
  files <- files[file.exists(files)]
  if (!length(files)) return(list(ok = FALSE, msg = "No DB files found."))

  db_name <- gsub(".sqlite", "", basename(DB_PATH))
  zipfile <- file.path(tempdir(), sprintf("%s_%s.zip", db_name, format(Sys.time(), "%Y%m%d_%H%M%S")))
  utils::zip(zipfile, files = files, flags = "-j")

  latest_name <- sprintf("%s_latest_backup.zip", db_name)
  latest_zip  <- file.path(tempdir(), latest_name)
  file.copy(zipfile, latest_zip, overwrite = TRUE)

  tryCatch(googledrive::drive_upload(
    media = zipfile, path = googledrive::as_id(DRIVE_FOLDER_ID),
    name = basename(zipfile), type = "application/zip", overwrite = FALSE
  ), error = function(e) logf("timestamped upload failed:", conditionMessage(e)))

  ok <- tryCatch({
    googledrive::drive_upload(
      media = latest_zip, path = googledrive::as_id(DRIVE_FOLDER_ID),
      name = latest_name, type = "application/zip", overwrite = TRUE
    )
    TRUE
  }, error = function(e) FALSE)

  if (ok) list(ok = TRUE, msg = paste("Uploaded", latest_name))
  else list(ok = FALSE, msg = "Latest zip upload failed.")
}

# -------------------------
# Flex-pass accounting
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
  pmax(0, initial - spent_total(uid))
}

# -------------------------
# Game logic
# -------------------------
pd_pair_payoffs <- function(olig, subs) {
  if (!nrow(subs)) return(tibble())
  s <- subs %>%
    arrange(created_at) %>%
    mutate(idx  = row_number(),
           pair = ceiling(idx/2),
           role = ifelse(idx %% 2 == 1, "A", "B"))
  if (nrow(s) == 1) {
    return(tibble(pair = 1, role = "A", user_id = s$user_id[1],
                  display_name = s$display_name[1], action = s$action[1], payoff = 0))
  }
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
    mutate(role         = ifelse(role_pay == "A_pay", "A", "B"),
           user_id      = ifelse(role == "A", user_id_A, user_id_B),
           display_name = ifelse(role == "A", display_name_A, display_name_B),
           action       = ifelse(role == "A", action_A, action_B)) %>%
    select(pair, role, user_id, display_name, action, payoff)

  out
}

# bonus_shares: denominator is EITHER the section's enrolled count (from users table)
# OR the number of students who actually submitted. No other values.
bonus_shares <- function(olig, subs) {
  if (!nrow(subs)) return(tibble())
  m    <- as.numeric(olig$bonus_multiplier[1] %||% 1.5)
  subs <- subs %>% mutate(contribute = ifelse(is.na(contribute) | !nzchar(as.character(contribute)), 0, as.numeric(contribute)))
  total <- sum(subs$contribute, na.rm = TRUE)
  pot   <- m * total

  use_sec <- isTRUE(as.integer(olig$use_section_size[1] %||% 1) == 1)
  cs      <- if (use_sec) section_class_size(olig_section(olig)) else NA_integer_
  n_denom <- if (!is.na(cs)) cs else nrow(subs)
  denom_source <- if (!is.na(cs))
    sprintf("section class size (%d enrolled)", cs)
  else
    sprintf("submitter count (%d)", nrow(subs))
  share <- if (n_denom > 0) pot / n_denom else 0

  subs %>%
    mutate(total_contrib = total,
           pot_total     = pot,
           n_denom       = n_denom,
           denom_source  = denom_source,
           share_each    = share)
}

# Apply payouts into the shared ledger -- filtered to current section
apply_payouts <- function(round, game) {
  olig <- get_olig()
  sec  <- olig_section(olig)

  # Filter submissions to current section (also accept legacy NULL section rows)
  subs <- db_query(
    "SELECT * FROM olig_submissions WHERE round=? AND game=? AND (section=? OR section IS NULL);",
    list(as.integer(round), as.character(game), sec)
  )
  if (!nrow(subs)) {
    return(list(ok = FALSE, msg = sprintf("No submissions for section '%s'.", sec)))
  }

  if (game == "bonus") {
    shares       <- bonus_shares(olig, subs)
    payouts      <- shares %>% mutate(payout = round_to_half(share_each)) %>% select(user_id, payout)
    denom_source <- if (nrow(shares) > 0) shares$denom_source[1] else sprintf("submitter count (%d)", nrow(subs))
    for (i in seq_len(nrow(payouts))) {
      uid <- payouts$user_id[i]
      pay <- payouts$payout[i]
      if (!is.finite(pay) || pay <= 0) next
      db_exec(
        "INSERT INTO ledger(user_id, round, purpose, amount, meta) VALUES(?, ?, 'grant', ?, ?);",
        list(uid, as.integer(round), -pay,
             sprintf("coordination_bonus_share round=%d section=%s denom=%s", round, sec, denom_source))
      )
      db_exec(
        "INSERT INTO olig_payouts(round, user_id, game, payout, meta, section) VALUES(?, ?, ?, ?, ?, ?);",
        list(as.integer(round), uid, "bonus", pay, sprintf("denom=%s", denom_source), sec)
      )
    }
    touch_olig()
    return(list(ok = TRUE, msg = sprintf(
      "Applied bonus payouts to %d students (section '%s', %s).",
      nrow(payouts), sec, denom_source
    )))
  }

  if (game == "pd") {
    pay <- pd_pair_payoffs(olig, subs) %>%
      mutate(payout = round_to_half(as.numeric(payoff) * as.numeric(olig$pd_scale[1] %||% 0.1))) %>%
      select(user_id, payout)
    for (i in seq_len(nrow(pay))) {
      uid <- pay$user_id[i]
      p   <- pay$payout[i]
      if (!is.finite(p) || p <= 0) next
      db_exec(
        "INSERT INTO ledger(user_id, round, purpose, amount, meta) VALUES(?, ?, 'grant', ?, ?);",
        list(uid, as.integer(round), -p,
             sprintf("coordination_pd_payout round=%d section=%s", round, sec))
      )
      db_exec(
        "INSERT INTO olig_payouts(round, user_id, game, payout, meta, section) VALUES(?, ?, ?, ?, ?, ?);",
        list(as.integer(round), uid, "pd", p, "pd_scale", sec)
      )
    }
    touch_olig()
    return(list(ok = TRUE, msg = sprintf(
      "Applied PD payouts to %d students in section '%s' (paired).", nrow(pay), sec
    )))
  }

  list(ok = FALSE, msg = "Unknown game.")
}

# -------------------------
# Auth UI
# -------------------------
login_ui <- function(msg = NULL) {
  fluidPage(
    titlePanel("Coordination Games (Flex Pass Integrated)"),
    if (!is.null(msg)) div(style = "color:#b00020; font-weight:bold;", msg),
    textInput("login_user", "Username"),
    passwordInput("login_pw", "Password"),
    actionButton("login_btn", "Sign in", class = "btn-primary"),
    tags$small("Use the same username/password as the flex passes app.")
  )
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    /* --- Base font --- */
    body { font-size: 16px; }

    /* --- Vassar palette overrides for Bootstrap elements --- */
    .btn-primary  { background-color: #951829; border-color: #7a1221; }
    .btn-primary:hover { background-color: #7a1221; border-color: #5e0d19; }
    .btn-success  { background-color: #2d6a4f; border-color: #245c43; }
    .btn-success:hover { background-color: #245c43; border-color: #1b4d38; }
    a { color: #951829; }
    a:hover { color: #7a1221; }

    /* --- Notifications: top-right, large, bold colors --- */
    .shiny-notification-panel {
      top: 16px; bottom: auto;
      right: 16px;
      min-width: 340px; width: auto;
    }
    .shiny-notification {
      font-size: 1.1rem;
      padding: 1rem 1.25rem;
      border-radius: 8px;
      border: none;
      box-shadow: 0 4px 20px rgba(0,0,0,0.25);
      color: #fff;
      margin-bottom: 8px;
    }
    .shiny-notification-message { background: #2d6a4f; }
    .shiny-notification-warning { background: #92400e; }
    .shiny-notification-error   { background: #951829; }
    .shiny-notification-close   { color: rgba(255,255,255,0.8); font-size: 1.2rem; }
  "))),
  uiOutput("auth_gate"),
  conditionalPanel("output.authed",
    tabsetPanel(
      tabPanel("Student",   uiOutput("student_ui")),
      tabPanel("Projector", uiOutput("projector_ui")),
      tabPanel("Admin",     uiOutput("admin_ui"))
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
    if (nrow(row) != 1) { showNotification("Login failed.", type = "error"); return() }

    ph <- row$pw_hash[1] %||% ""
    ok <- tryCatch(bcrypt::checkpw(p, ph), error = function(e) FALSE)
    if (!isTRUE(ok)) { showNotification("Login failed.", type = "error"); return() }

    rv$authed   <- TRUE
    rv$user     <- row$user_id[1]
    rv$name     <- row$display_name[1]
    rv$is_admin <- isTRUE(as.integer(row$is_admin[1]) == 1)
  })

  authed   <- reactive(rv$authed)
  user_id  <- reactive(rv$user)
  name     <- reactive(rv$name)
  is_admin <- reactive(rv$is_admin)

  # Poll shared state via olig_settings.updated_at
  olig_poll <- reactivePoll(
    1200, session,
    checkFunc = function() get_olig()$updated_at[1] %||% as.character(Sys.time()),
    valueFunc  = function() get_olig()
  )

  # Poll submissions -- filtered to the current section
  subs_poll <- reactivePoll(
    1200, session,
    checkFunc = function() {
      o <- get_olig()
      paste0(
        o$updated_at[1] %||% "", "|",
        db_query(
          "SELECT COUNT(*) n FROM olig_submissions WHERE round=? AND (section=? OR section IS NULL);",
          list(as.integer(o$current_round[1]), olig_section(o))
        )$n[1]
      )
    },
    valueFunc = function() {
      o   <- get_olig()
      sec <- olig_section(o)
      subs <- db_query(
        "SELECT * FROM olig_submissions WHERE round=? AND (section=? OR section IS NULL);",
        list(as.integer(o$current_round[1]), sec)
      )
      list(olig = o, subs = subs, section = sec)
    }
  )

  # ---------------- Student UI ----------------
  # Deliberately uses olig_poll() (game state only), NOT subs_poll().
  # subs_poll fires whenever any classmate submits, which would re-render the
  # sliderInput and reset it to value=0 mid-session. Game-state (round/status/
  # game-type) changes come only from admin actions, so olig_poll is the right
  # dependency here. student_result (below) uses subs_poll for live result display.
  output$student_ui <- renderUI({
    req(authed())
    o   <- olig_poll()
    sec <- olig_section(o)

    bal    <- remaining_fp(user_id())
    game   <- as.character(o$current_game[1])
    status <- as.character(o$round_status[1])

    game_lbl <- if (game == "pd") "Price War (High vs Low)" else "Bonus Pot (Contribute flex passes)"
    tagList(
      h4(sprintf("Logged in as: %s (%s)", user_id(), name())),
      p(sprintf("Section: %s | Flex passes available: %.2f", sec, bal)),
      p(sprintf("Round %d | Game: %s | Status: %s",
                as.integer(o$current_round[1]), game_lbl, status)),
      tags$hr(),
      if (game == "pd") {
        tagList(
          radioButtons("pd_action", "Your choice", choices = c("High", "Low"), inline = TRUE),
          actionButton("submit_pd", "Submit", class = "btn-primary")
        )
      } else {
        use_sec <- isTRUE(as.integer(o$use_section_size[1] %||% 1) == 1)
        cs      <- if (use_sec) section_class_size(olig_section(o)) else NA_integer_
        cs_note <- if (!is.na(cs))
          sprintf("Pot divided among all %d enrolled students in this section.", cs)
        else
          "Pot divided among students who submit."
        tagList(
          {
            cap <- as.numeric(o$contrib_cap[1] %||% 0)
            eff_max <- if (is.finite(cap) && cap > 0) min(bal, cap) else bal
            sliderInput("bonus_c", "Contribute flex passes",
                        min = 0, max = max(0, floor(eff_max * 2) / 2), value = 0, step = 0.5)
          },
          actionButton("submit_bonus", "Submit (debited immediately)", class = "btn-primary"),
          tags$small(sprintf("Multiplier m = %.2f. %s", as.numeric(o$bonus_multiplier[1]), cs_note))
        )
      },
      tags$hr(),
      uiOutput("student_result")
    )
  })

  already_submitted <- function(round) {
    x <- db_query("SELECT COUNT(*) n FROM olig_submissions WHERE round=? AND user_id=?;",
                  list(as.integer(round), as.character(user_id())))
    as.integer(x$n[1]) > 0
  }

  observeEvent(input$submit_pd, {
    req(authed())
    # Read game state fresh (not via subs_poll) to avoid a reactive dependency
    # that could re-trigger the student UI render on every classmate submission.
    o   <- get_olig()
    sec <- olig_section(o)

    if (as.character(o$round_status[1]) != "open") {
      showNotification("Round is not open for submissions.", type = "error"); return()
    }
    if (as.character(o$current_game[1]) != "pd") {
      showNotification("Current game is not Price War.", type = "error"); return()
    }
    if (!(input$pd_action %in% c("High", "Low"))) {
      showNotification("Please select High or Low.", type = "error"); return()
    }
    if (already_submitted(o$current_round[1])) {
      showNotification("You already submitted this round.", type = "warning"); return()
    }

    db_exec(
      "INSERT INTO olig_submissions(round, user_id, display_name, game, action, contribute, section)
       VALUES(?, ?, ?, 'pd', ?, NULL, ?);",
      list(as.integer(o$current_round[1]), as.character(user_id()),
           as.character(name()), as.character(input$pd_action), sec)
    )
    # Do NOT call touch_olig() here. subs_poll detects new submissions via the
    # COUNT query in its checkFunc. Calling touch_olig() would invalidate olig_poll
    # which re-renders every connected student's form, bouncing their sliders to 0.
    showNotification("Submitted.", type = "message")
  })

  observeEvent(input$submit_bonus, {
    req(authed())
    o   <- get_olig()
    sec <- olig_section(o)

    if (as.character(o$round_status[1]) != "open") {
      showNotification("Round is not open for submissions.", type = "error"); return()
    }
    if (as.character(o$current_game[1]) != "bonus") {
      showNotification("Current game is not Bonus Pot.", type = "error"); return()
    }
    if (already_submitted(o$current_round[1])) {
      showNotification("You already submitted this round.", type = "warning"); return()
    }

    c_val <- round_to_half(as.numeric(input$bonus_c %||% 0))
    if (!is.finite(c_val) || c_val < 0) c_val <- 0

    bal <- remaining_fp(user_id())
    if (c_val > bal + 1e-9) {
      showNotification("Not enough flex passes.", type = "error"); return()
    }
    cap <- as.numeric(o$contrib_cap[1] %||% 0)
    if (is.finite(cap) && cap > 0 && c_val > cap + 1e-9) {
      showNotification(sprintf("Contribution exceeds the cap of %.1f flex passes.", cap), type = "error"); return()
    }

    # Debit contribution immediately
    if (c_val > 0) {
      db_exec(
        "INSERT INTO ledger(user_id, round, purpose, amount, meta) VALUES(?, ?, 'oligopoly_contrib', ?, ?);",
        list(as.character(user_id()), as.integer(o$current_round[1]), c_val,
             sprintf("coordination_bonus_contribution round=%d section=%s",
                     as.integer(o$current_round[1]), sec))
      )
    }

    db_exec(
      "INSERT INTO olig_submissions(round, user_id, display_name, game, action, contribute, section)
       VALUES(?, ?, ?, 'bonus', NULL, ?, ?);",
      list(as.integer(o$current_round[1]), as.character(user_id()),
           as.character(name()), c_val, sec)
    )

    # Reset slider in-place without re-rendering the whole form.
    # Do NOT call touch_olig() -- that would fire olig_poll for every connected
    # client and bounce all their sliders back to 0.
    new_bal <- remaining_fp(user_id())
    updateSliderInput(session, "bonus_c", value = 0,
                      max = max(0, floor(new_bal * 2) / 2))
    showNotification(
      sprintf("Submitted. Contributed %.1f flex pass(es); deducted immediately.", c_val),
      type = "message"
    )
  })

  output$student_result <- renderUI({
    req(authed())
    st <- subs_poll(); o <- st$olig; subs <- st$subs
    if (as.character(o$round_status[1]) != "revealed") {
      return(p("Results will appear after the instructor reveals the round."))
    }
    game   <- as.character(o$current_game[1])
    my_sub <- subs %>% filter(user_id == !!user_id())
    if (!nrow(my_sub)) return(p("No submission recorded for you this round."))

    if (game == "bonus") {
      shares <- bonus_shares(o, subs %>% filter(game == "bonus"))
      me     <- shares %>% filter(user_id == !!user_id())
      if (!nrow(me)) return(p("No bonus result found."))
      payout <- round_to_half(me$share_each[1])
      tagList(
        h5("Your result"),
        p(sprintf("You contributed: %.2f", as.numeric(me$contribute[1]))),
        p(sprintf("Denominator: %s", me$denom_source[1])),
        p(sprintf("Your share of pot: %.2f (credited; rounded to nearest 0.5)", payout))
      )
    } else {
      pay <- pd_pair_payoffs(o, subs %>% filter(game == "pd"))
      me  <- pay %>% filter(user_id == !!user_id())
      if (!nrow(me)) {
        return(p("Odd number of submissions: last unpaired student has no PD payoff this round."))
      }
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
    st       <- subs_poll(); o <- st$olig; subs <- st$subs; sec <- st$section
    r        <- as.integer(o$current_round[1])
    cur_game <- as.character(o$current_game[1])
    status   <- as.character(o$round_status[1])
    game_lbl <- if (cur_game == "pd") "Price War (PD)" else "Bonus Pot"
    n        <- nrow(subs %>% filter(round == r, game == cur_game))

    tagList(
      h3(sprintf("Round %d -- %s  |  Section: %s", r, game_lbl, sec)),
      p(sprintf("Status: %s | Submissions (this section): %d", status, n)),
      if (cur_game == "pd") {
        tagList(
          p(sprintf("High: %d | Low: %d",
                    sum(subs$action == "High", na.rm = TRUE),
                    sum(subs$action == "Low",  na.rm = TRUE))),
          if (status == "revealed")
            tagList(h4("Pair Outcome Counts"), tableOutput("proj_pd_counts"))
        )
      } else {
        use_sec  <- isTRUE(as.integer(o$use_section_size[1] %||% 1) == 1)
        cs       <- if (use_sec) section_class_size(olig_section(o)) else NA_integer_
        cs_note  <- if (!is.na(cs))
          sprintf("Section class size: %d enrolled", cs)
        else
          sprintf("Submitter count: %d", n)
        tagList(
          p(sprintf("Total contributed: %.2f | Multiplier m: %.2f | %s",
                    sum(as.numeric(subs$contribute), na.rm = TRUE),
                    as.numeric(o$bonus_multiplier[1]), cs_note)),
          plotOutput("proj_bonus_plot", height = "350px"),
          if (status == "revealed") {
            shares <- bonus_shares(o, subs %>% filter(game == "bonus"))
            if (nrow(shares) > 0) {
              tagList(
                h4("Results"),
                p(sprintf("Pot total: %.2f | %s | Each student receives: %.2f flex passes",
                          shares$pot_total[1], shares$denom_source[1],
                          round_to_half(shares$share_each[1])))
              )
            }
          }
        )
      }
    )
  })

  output$proj_pd_counts <- renderTable({
    st      <- subs_poll(); o <- st$olig; subs <- st$subs
    r       <- as.integer(o$current_round[1])
    pd_subs <- subs %>% filter(round == r, game == "pd")
    if (!nrow(pd_subs)) return(data.frame(Outcome = character(), Count = integer()))

    pay <- pd_pair_payoffs(o, pd_subs)
    if (!nrow(pay)) return(data.frame(Outcome = character(), Count = integer()))

    pay %>%
      group_by(pair) %>%
      summarise(outcome = paste(sort(action), collapse = "-"), .groups = "drop") %>%
      count(outcome, name = "Count") %>%
      rename(Outcome = outcome)
  }, striped = TRUE, bordered = TRUE, align = "lc")

  output$proj_bonus_plot <- renderPlot({
    st <- subs_poll(); o <- st$olig; subs <- st$subs
    r  <- as.integer(o$current_round[1])
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

    o       <- olig_poll()
    sec     <- olig_section(o)
    use_sec <- as.integer(o$use_section_size[1] %||% 1)
    has_col <- users_has_section_col()
    cs      <- if (has_col && use_sec == 1L) section_class_size(sec) else NA_integer_
    denom_lbl <- if (!is.na(cs)) sprintf("section class size (%d)", cs) else "submitter count"

    # Section choices derived from users table (re-read on each render)
    sec_choices <- if (has_col) sections_choices() else setNames("", "(users table has no section column)")

    # Radio button labels -- only two options, no free input
    cs_radio_lbl <- if (!is.na(cs))
      sprintf("Section class size  (%d enrolled)", cs)
    else if (has_col && nzchar(sec))
      sprintf("Section class size  (section '%s' not found in users)", sec)
    else
      "Section class size  (no section selected or column missing)"
    denom_choices <- setNames(c("1", "0"), c(cs_radio_lbl, "Submitter count"))

    fluidPage(
      h4("Admin controls"),
      p(sprintf("Round: %d | Section: %s | Denominator: %s | Game: %s | Status: %s",
                as.integer(o$current_round[1]),
                if (nzchar(sec)) sec else "(none)",
                denom_lbl,
                as.character(o$current_game[1]),
                as.character(o$round_status[1]))),

      # Warning banner when users table has no section column
      if (!has_col) div(
        style = "background:#fff3cd; border:1px solid #ffc107; padding:10px; margin-bottom:10px; border-radius:4px;",
        tags$strong("Warning:"),
        " The ", tags$code("users"), " table does not have a ",
        tags$code("section"), " column. Add a ",
        tags$code("section TEXT"), " column to the ", tags$code("users"),
        " table in ", tags$code("finalqdata.sqlite"),
        " and assign each student to their section before using section-based class sizes."
      ),

      tags$hr(),

      # --- Round / Section / Status ---
      wellPanel(
        h5("Round + Section + Status"),
        fluidRow(
          column(3,
            numericInput("adm_round", "Round number",
                         value = as.integer(o$current_round[1]), min = 1, step = 1)
          ),
          column(4,
            selectInput("adm_section", "Active Section",
                        choices  = sec_choices,
                        selected = sec)
          ),
          column(5,
            radioButtons("adm_use_section_size", "Bonus pot denominator",
                         choices  = denom_choices,
                         selected = as.character(use_sec),
                         inline   = TRUE)
          )
        ),
        fluidRow(
          column(4,
            selectInput("adm_game", "Game",
                        choices  = c("Price War (PD)" = "pd", "Bonus Pot" = "bonus"),
                        selected = as.character(o$current_game[1]))
          ),
          column(4,
            selectInput("adm_status", "Status",
                        choices  = c("open", "closed", "revealed"),
                        selected = as.character(o$round_status[1]))
          )
        ),
        actionButton("adm_apply", "Apply round settings", class = "btn-primary"),
        actionButton("adm_clear", "Clear submissions (this round + section)", class = "btn-danger"),
        tags$small(
          "Switch Active Section when moving between class sections. ",
          "Clearing removes submissions for the current section and reverses ",
          "any bonus contribution debits from the ledger."
        )
      ),

      # --- Game Parameters ---
      wellPanel(
        h5("Game Parameters"),
        numericInput("adm_m", "Bonus multiplier m",
                     value = as.numeric(o$bonus_multiplier[1]), min = 1, step = 0.1),
        numericInput("adm_contrib_cap", "Max flex passes students can wager (0 = no cap)",
                     value = as.numeric(o$contrib_cap[1] %||% 0), min = 0, step = 0.5),
        numericInput("adm_pd_scale", "PD scale (flex passes per payoff point)",
                     value = as.numeric(o$pd_scale[1]), min = 0, step = 0.01),
        tags$hr(),
        h5("PD payoff matrix (A, B)"),
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
        actionButton("adm_save_params", "Save parameters", class = "btn-secondary")
      ),

      # --- Settlement ---
      wellPanel(
        h5("Settlement"),
        p("Set status to REVEALED, then click below to post payouts for the current section."),
        actionButton("adm_payout", "Apply payouts to ledger (one-time per section)", class = "btn-success"),
        tags$small(
          "Credits students in the current section via purpose='grant' (negative amount). ",
          "Bonus contributions were already debited on submission."
        )
      ),

      # --- Backup ---
      wellPanel(
        h5("Backup"),
        actionButton("adm_backup_sheets", "Backup tables to Sheets", class = "btn-secondary"),
        actionButton("adm_backup_drive",  "Backup SQLite to Drive",  class = "btn-secondary"),
        tags$small("Uses FLEX_PASS_SHEET_ID (Sheets) and FLEX_PASS_FOLDER_ID (Drive zip).")
      ),

      tags$hr(),
      h5("Payout audit (latest 100)"),
      DTOutput("payout_audit")
    )
  })

  observeEvent(input$adm_apply, {
    req(is_admin())
    use_sec <- suppressWarnings(as.integer(input$adm_use_section_size %||% "1"))
    if (is.na(use_sec)) use_sec <- 1L
    sec     <- as.character(input$adm_section %||% "")
    cs      <- if (use_sec == 1L) section_class_size(sec) else NA_integer_
    denom_lbl <- if (!is.na(cs)) sprintf("section class size (%d)", cs) else "submitter count"

    db_exec(
      "UPDATE olig_settings SET current_round=?, current_game=?, round_status=?,
       section=?, use_section_size=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;",
      list(as.integer(input$adm_round), as.character(input$adm_game),
           as.character(input$adm_status), sec, use_sec)
    )
    db_exec(
      "INSERT OR IGNORE INTO olig_rounds(round, game, status) VALUES(?, ?, ?);",
      list(as.integer(input$adm_round), as.character(input$adm_game), as.character(input$adm_status))
    )
    db_exec(
      "UPDATE olig_rounds SET game=?, status=? WHERE round=?;",
      list(as.character(input$adm_game), as.character(input$adm_status), as.integer(input$adm_round))
    )
    showNotification(
      sprintf("Updated: Round %d | Section '%s' | Denominator: %s | Game: %s | Status: %s.",
              as.integer(input$adm_round),
              if (nzchar(sec)) sec else "(none)",
              denom_lbl, input$adm_game, input$adm_status),
      type = "message"
    )
  })

  observeEvent(input$adm_clear, {
    req(is_admin())
    o   <- get_olig()
    r   <- as.integer(o$current_round[1])
    sec <- olig_section(o)

    # Reverse contribution debits before removing submissions, so student
    # balances are restored as if the round never happened.
    n_debits <- db_exec(
      "DELETE FROM ledger
       WHERE round=? AND purpose='oligopoly_contrib'
         AND user_id IN (
           SELECT user_id FROM olig_submissions
           WHERE round=? AND (section=? OR section IS NULL)
         );",
      list(r, r, sec)
    )

    db_exec(
      "DELETE FROM olig_submissions WHERE round=? AND (section=? OR section IS NULL);",
      list(r, sec)
    )
    touch_olig()
    showNotification(
      sprintf("Cleared submissions for round %d, section '%s'. Reversed %d contribution debit(s).",
              r, sec, as.integer(n_debits)),
      type = "warning"
    )
  })

  observeEvent(input$adm_save_params, {
    req(is_admin())
    db_exec("
      UPDATE olig_settings SET
        bonus_multiplier=?, contrib_cap=?, pd_scale=?,
        pd_HH_A=?, pd_HH_B=?,
        pd_HL_A=?, pd_HL_B=?,
        pd_LH_A=?, pd_LH_B=?,
        pd_LL_A=?, pd_LL_B=?,
        updated_at=CURRENT_TIMESTAMP
      WHERE id=1;
    ", list(
      as.numeric(input$adm_m),     as.numeric(input$adm_contrib_cap), as.numeric(input$adm_pd_scale),
      as.numeric(input$pd_HH_A),   as.numeric(input$pd_HH_B),
      as.numeric(input$pd_HL_A),   as.numeric(input$pd_HL_B),
      as.numeric(input$pd_LH_A),   as.numeric(input$pd_LH_B),
      as.numeric(input$pd_LL_A),   as.numeric(input$pd_LL_B)
    ))
    showNotification("Parameters saved.", type = "message")
  })

  observeEvent(input$adm_payout, {
    req(is_admin())
    o    <- get_olig()
    r    <- as.integer(o$current_round[1])
    game <- as.character(o$current_game[1])
    sec  <- olig_section(o)
    if (as.character(o$round_status[1]) != "revealed") {
      showNotification("Set status to 'revealed' first.", type = "error"); return()
    }
    # Per-section idempotency check
    already <- db_query(
      "SELECT COUNT(*) n FROM olig_payouts WHERE round=? AND game=? AND (section=? OR section IS NULL);",
      list(r, game, sec)
    )$n[1]
    if (as.integer(already) > 0) {
      showNotification(
        sprintf("Payouts already applied for round %d, game '%s', section '%s'.", r, game, sec),
        type = "warning"
      ); return()
    }
    res <- apply_payouts(r, game)
    showNotification(res$msg, type = if (isTRUE(res$ok)) "message" else "error")
  })

  output$payout_audit <- DT::renderDT({
    req(authed(), is_admin())
    df <- db_query(
      "SELECT created_at, round, section, game, user_id, payout, meta
       FROM olig_payouts ORDER BY id DESC LIMIT 100;"
    )
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 15))
  })

  observeEvent(input$adm_backup_sheets, {
    req(is_admin())
    res <- tryCatch(backup_to_sheets(), error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    showNotification(res$msg, type = if (isTRUE(res$ok)) "message" else "error")
  })

  observeEvent(input$adm_backup_drive, {
    req(is_admin())
    res <- tryCatch(backup_db_to_drive(), error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    showNotification(res$msg, type = if (isTRUE(res$ok)) "message" else "error")
  })

  session$onSessionEnded(function() {
    if (db_changed_since_last_backup()) {
      logf("session ended: backing up to Drive...")
      tryCatch(backup_db_to_drive(),
               error = function(e) logf("session-end backup FAILED:", conditionMessage(e)))
    }
    if (!is.null(conn) && DBI::dbIsValid(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
  })
}

shinyApp(ui, server)
