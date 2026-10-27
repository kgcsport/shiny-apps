try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
# app.R -- Coordination Games (Bonus Pot + Price War / PD)
# -------------------------------------------------------------------
# Uses the shared DB (class-job-market.sqlite) for auth and token accounting.
# Section-aware: sections are stored in the `sections` DB table with
# their enrolled class size. The bonus-pot denominator is either the
# section's class size OR the submitter count -- no other values.
#
# Shared game records are initialized and controlled by Class Job Market.
# This module reads that state and records student gameplay only.
#
# Class Job Market owns:
#   - olig_settings, olig_rounds, and olig_payouts
#   - session lifecycle, token debits, and payout settlement
# This module owns:
#   - the student-facing interaction against the active shared session
# Key behavior:
# - Students log in with class-job-market credentials.
# - Bonus Pot: students UPSERT their contribution while the round is open (no debit yet).
#   When admin sets status → 'closed', contributions are DEBITED (source_type='coordination_contrib').
#   On reveal each student in the section gets a CREDIT = pot / class_size
#   (or / submitter count if class_size is 0/NULL), rounded to nearest 0.5.
# - Price War (PD): no staking; payoff = pd_payoff_points * pd_scale, rounded 0.5.
#   PD choices can also be changed while the round is open (upsert).
#
# Env vars:
# - CONNECT_CONTENT_DIR (Posit) optional; otherwise uses /srv/shiny-server/appdata or sibling dir
#
# Packages: shiny, DT, bcrypt, dplyr, DBI, RSQLite, stringr, ggplot2, tidyr
# -------------------------------------------------------------------

library(shiny); library(DT); library(bcrypt); library(dplyr); library(tibble)
library(DBI); library(RSQLite); library(stringr); library(ggplot2); library(tidyr)


this_file <- ""
for (i in rev(seq_len(sys.nframe()))) {
  candidate_file <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
  if (!is.null(candidate_file) && nzchar(candidate_file)) {
    this_file <- normalizePath(candidate_file, winslash = "/", mustWork = TRUE)
    break
  }
}
this_dir <- if (nzchar(this_file)) dirname(this_file) else getwd()
shared_sqlite_candidates <- c(
  file.path(this_dir, "..", "_shared", "sqlite.R"),
  file.path("apps", "_shared", "sqlite.R"),
  file.path("_shared", "sqlite.R"),
  file.path("..", "_shared", "sqlite.R"),
  file.path("/srv/shiny-server", "_shared", "sqlite.R")
)
shared_sqlite <- Filter(file.exists, shared_sqlite_candidates)
if (!length(shared_sqlite)) {
  stop("Cannot find shared SQLite helper from ", getwd(), ". Tried: ", paste(shared_sqlite_candidates, collapse = ", "))
}
shared_sqlite <- shared_sqlite[[1]]
source(shared_sqlite)
source(file.path(dirname(shared_sqlite), "demo_login.R"))

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

logf <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste(vapply(list(...), as.character, character(1)), collapse = " ")
  cat(ts, "-", msg, "\n", file = stderr()); flush(stderr())
}

# -------------------------
# DB path — shared DB owned by class-job-market
# -------------------------
DB_PATH <- shared_db_path()
logf("DB_PATH:", DB_PATH)

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <<- connect_sqlite(DB_PATH)
  }
  conn
}
db_exec  <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

# -------------------------
# Init tables for coordination games module
# Class Job Market is the schema and lifecycle owner. The player module must
# not seed defaults or migrate a separate active-game state on startup.
init_olig <- function() invisible(TRUE)
init_olig()
touch_olig <- function() invisible(FALSE)
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
      AND COALESCE(active,1) = 1
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
  x <- db_query("SELECT COUNT(*) AS n FROM users WHERE section=? AND COALESCE(active,1)=1;", list(section_id))
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

  con <- connect_sqlite(DB_PATH)
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
# Participation token accounting (token_ledger in class-job-market.sqlite)
# -------------------------
token_balance <- function(uid) {
  r <- db_query(
    "SELECT COALESCE(SUM(amount),0) AS spendable_balance FROM token_ledger WHERE user_id=?;",
    list(uid)
  )
  x <- suppressWarnings(as.numeric(r$spendable_balance[1]))
  if (!is.finite(x)) 0 else x
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
    denom_source <- if (nrow(shares) > 0) shares$denom_source[1] else sprintf("submitter count (%d)", nrow(subs))
    use_sec      <- isTRUE(as.integer(olig$use_section_size[1] %||% 1L) == 1L)
    recipients   <- if (use_sec && !is.na(section_class_size(sec))) db_query(
      "SELECT user_id FROM users WHERE section=? AND COALESCE(active,1)=1 AND COALESCE(is_admin,0)=0 AND COALESCE(is_demo,0)=0;", list(sec)
    ) else tibble(user_id=unique(as.character(subs$user_id)))
    if (!nrow(recipients)) recipients <- tibble(user_id=unique(as.character(subs$user_id)))
    payouts <- recipients %>% mutate(payout=round_to_half(shares$share_each[1]))
    for (i in seq_len(nrow(payouts))) {
      uid <- payouts$user_id[i]
      pay <- payouts$payout[i]
      if (!is.finite(pay) || pay <= 0) next
      db_exec(
        "INSERT INTO token_ledger(user_id, display_name, round_id, source_type, amount, earning, note)
         SELECT user_id, display_name, ?, 'coordination_grant', ?, 1, ?
         FROM users WHERE user_id=?;",
        list(as.integer(round), pay,
             sprintf("coordination_bonus_share round=%d section=%s denom=%s", round, sec, denom_source),
             uid)
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
        "INSERT INTO token_ledger(user_id, display_name, round_id, source_type, amount, earning, note)
         SELECT user_id, display_name, ?, 'coordination_grant', ?, 1, ?
         FROM users WHERE user_id=?;",
        list(as.integer(round), p,
             sprintf("coordination_pd_payout round=%d section=%s", round, sec),
             uid)
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

# Debit bonus contributions that haven't been charged yet (idempotent).
# Called when admin closes the round. Returns count of new debits posted.
debit_pending_contributions <- function(round, sec) {
  subs <- db_query(
    "SELECT * FROM olig_submissions WHERE round=? AND game='bonus' AND (section=? OR section IS NULL);",
    list(as.integer(round), sec)
  )
  debited <- 0L
  for (i in seq_len(nrow(subs))) {
    uid   <- subs$user_id[i]
    c_val <- suppressWarnings(as.numeric(subs$contribute[i]))
    if (!is.finite(c_val) || c_val <= 0) next
    already <- db_query(
      "SELECT COUNT(*) n FROM token_ledger WHERE user_id=? AND round_id=? AND source_type='coordination_contrib';",
      list(uid, as.integer(round))
    )$n[1]
    if (as.integer(already) > 0) next
    db_exec(
      "INSERT INTO token_ledger(user_id, display_name, round_id, source_type, amount, earning, note)
       SELECT user_id, display_name, ?, 'coordination_contrib', ?, 0, ?
       FROM users WHERE user_id=?;",
      list(as.integer(round), -c_val,
           sprintf("coordination_bonus_contribution round=%d section=%s", as.integer(round), sec),
           uid)
    )
    debited <- debited + 1L
  }
  debited
}

# -------------------------
# Auth UI
# -------------------------
login_ui <- function(msg = NULL) {
  fluidPage(
    titlePanel("Bonus Pot"),
    if (!is.null(msg)) div(style = "color:#b00020; font-weight:bold;", msg),
    tags$p("Checking your Class Job Market sign-in…"),
    tags$p(tags$small("If needed, you will be sent to Class Job Market and returned here.")),
    demo_login_ui
  )
}

shared_auth_js <- tags$script(HTML("
(function() {
  function sendAuthCookie() { Shiny.setInputValue('auth_cookie', (document.cookie.match(/(?:^|; )arcade_token=([^;]*)/) || ['', ''])[1], {priority: 'event'}); }
  $(document).on('shiny:sessioninitialized', sendAuthCookie);
  // Shiny emits this as a jQuery event; the native listener above never fired,
  // leaving a visitor stranded on the checking-sign-in screen.
  setTimeout(sendAuthCookie, 300);
  Shiny.addCustomMessageHandler('redirect_to_job_market', function(msg) { window.location.replace(msg.url); });
})();
"))

ui <- fluidPage(
  tags$head(shared_auth_js, tags$style(HTML("
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
  uiOutput("demo_banner"),
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
  dm       <- demo_server_init(session, DB_PATH)
  db_exec  <- dm$db_exec
  db_query <- dm$db_query
  .is_demo <- dm$is_demo

  rv <- reactiveValues(authed = FALSE, user = NULL, name = NULL, is_admin = FALSE,
                       impersonate = FALSE, impersonate_uid = NULL, impersonate_name = NULL)
  output$demo_banner <- renderUI(demo_banner_ui(.is_demo, rv$is_admin))
  output$authed <- reactive(rv$authed)
  outputOptions(output, "authed", suspendWhenHidden = FALSE)
  output$auth_gate <- renderUI({ if (!rv$authed) login_ui() else NULL })

  lookup_shared_session <- function(token) {
    if (!nzchar(token %||% "")) return(data.frame())
    tryCatch(db_query("SELECT u.user_id, u.display_name, u.is_admin, u.active FROM arcade_sessions s JOIN users u ON u.user_id=s.user_id WHERE s.token=? AND datetime(s.expires_at)>datetime('now') LIMIT 1;", list(token)), error=function(e) data.frame())
  }
  finish_login <- function(row) {
    rv$authed <- TRUE; rv$user <- row$user_id[1]; rv$name <- row$display_name[1]; rv$is_admin <- isTRUE(as.integer(row$is_admin[1] %||% 0L) == 1L)
  }
  observeEvent(input$auth_cookie, {
    if (rv$authed || .is_demo) return()
    row <- lookup_shared_session(input$auth_cookie %||% "")
    if (nrow(row) && !isTRUE(as.integer(row$active[1] %||% 1L) == 0L)) { finish_login(row); return() }
    session$sendCustomMessage("redirect_to_job_market", list(url="../class-job-market/?return_to=%2Fcoordination-games%2F"))
  }, ignoreInit=FALSE)


  authed   <- reactive(rv$authed)
  user_id  <- reactive({
    if (isTRUE(rv$impersonate) && !is.null(rv$impersonate_uid)) rv$impersonate_uid else rv$user
  })
  name     <- reactive({
    if (isTRUE(rv$impersonate) && !is.null(rv$impersonate_name)) rv$impersonate_name else rv$name
  })
  is_admin <- reactive(rv$is_admin)

  # Poll every 1200ms: shared state via olig_settings.updated_at for in-class games.
  olig_poll <- reactivePoll(
    1200, session,
    checkFunc = function() get_olig()$updated_at[1] %||% as.character(Sys.time()),
    valueFunc  = function() get_olig()
  )

  # Poll every 1200ms: submissions filtered to the current section.
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

    bal    <- token_balance(user_id())
    game   <- as.character(o$current_game[1])
    status <- as.character(o$round_status[1])

    game_lbl <- if (game == "pd") "Price War (High vs Low)" else "Bonus Pot (Contribute participation tokens)"
    tagList(
      h4(sprintf("Logged in as: %s (%s)", rv$user, rv$name)),
      if (isTRUE(rv$impersonate)) div(
        style = "background:#92400e; color:#fff; padding:6px 12px; border-radius:6px; margin-bottom:8px;",
        strong(sprintf("IMPERSONATING: %s (%s) — submissions recorded under this student", rv$impersonate_name, rv$impersonate_uid))
      ),
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
            sliderInput("bonus_c", "Contribute participation tokens",
                        min = 0, max = max(0, floor(eff_max * 2) / 2), value = 0, step = 0.5)
          },
          actionButton("submit_bonus", "Save contribution", class = "btn-primary"),
          tags$small(sprintf("Multiplier m = %.2f. %s Contributions are debited when the round closes.", as.numeric(o$bonus_multiplier[1]), cs_note))
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

    # Upsert -- allow re-submission while round is open
    db_exec(
      "INSERT OR REPLACE INTO olig_submissions(round, user_id, display_name, game, action, contribute, section)
       VALUES(?, ?, ?, 'pd', ?, NULL, ?);",
      list(as.integer(o$current_round[1]), as.character(user_id()),
           as.character(name()), as.character(input$pd_action), sec)
    )
    # Do NOT call touch_olig() here. subs_poll detects new submissions via the
    # COUNT query in its checkFunc. Calling touch_olig() would invalidate olig_poll
    # which re-renders every connected student's form, bouncing their sliders to 0.
    showNotification("Choice saved. You can change it until the round closes.", type = "message")
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

    c_val <- round_to_half(as.numeric(input$bonus_c %||% 0))
    if (!is.finite(c_val) || c_val < 0) c_val <- 0

    bal <- token_balance(user_id())
    if (c_val > bal + 1e-9) {
      showNotification("Not enough participation tokens.", type = "error"); return()
    }
    cap <- as.numeric(o$contrib_cap[1] %||% 0)
    if (is.finite(cap) && cap > 0 && c_val > cap + 1e-9) {
      showNotification(sprintf("Contribution exceeds the cap of %.1f participation tokens.", cap), type = "error"); return()
    }

    # Upsert -- no debit yet; debit happens when admin closes the round.
    # Do NOT call touch_olig() -- that would fire olig_poll for every connected
    # client and bounce all their sliders back to 0.
    db_exec(
      "INSERT OR REPLACE INTO olig_submissions(round, user_id, display_name, game, action, contribute, section)
       VALUES(?, ?, ?, 'bonus', NULL, ?, ?);",
      list(as.integer(o$current_round[1]), as.character(user_id()),
           as.character(name()), c_val, sec)
    )
    showNotification(
      sprintf("Saved %.1f participation token(s) as your contribution. You can change it until the round closes.", c_val),
      type = "message"
    )
  })

  output$student_result <- renderUI({
    req(authed())
    st <- subs_poll(); o <- st$olig; subs <- st$subs
    status <- as.character(o$round_status[1])
    game   <- as.character(o$current_game[1])

    if (status != "revealed") {
      my_sub <- subs %>% filter(user_id == !!user_id())
      if (nrow(my_sub) > 0 && game == "bonus") {
        c_saved <- suppressWarnings(as.numeric(my_sub$contribute[1]))
        c_saved <- if (is.finite(c_saved)) c_saved else 0
        return(tagList(
          p(strong(sprintf("Saved contribution: %.1f participation token(s).", c_saved))),
          p(if (status == "open")
              "You can update your contribution until the round closes."
            else
              "Round closed. Contributions are being debited.")
        ))
      }
      if (nrow(my_sub) > 0 && game == "pd") {
        return(tagList(
          p(strong(sprintf("Saved choice: %s.", as.character(my_sub$action[1])))),
          p(if (status == "open") "You can change your choice until the round closes."
            else "Round closed.")
        ))
      }
      return(p("Results will appear after the instructor reveals the round."))
    }
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
        p(sprintf("Payout to participation tokens: %.2f (credited; rounded to nearest 0.5)", payout))
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
                p(sprintf("Pot total: %.2f | %s | Each student receives: %.2f participation tokens",
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
  # This is a player module. Class Job Market owns all session setup, state
  # transitions, token debits, and payouts; keeping write controls out of this
  # app prevents two instructor workflows from racing over the same records.
  output$admin_ui <- renderUI({
    req(authed())
    if (!is_admin()) return(fluidPage(h4("Admin"), p("You are not an admin.")))
    wellPanel(
      h4("Bonus Pot is controlled from Class Job Market"),
      p("Use Games & Demos → Active Now in Class Job Market to configure the round, open or close submissions, reveal results, and apply payouts."),
      tags$a(href="../class-job-market/", class="btn btn-primary", "Open Class Job Market →")
    )
  })
  observeEvent(input$adm_apply, {
    req(is_admin(), identical(Sys.getenv("ALLOW_LEGACY_COORDINATION_ADMIN"), "1"))
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

    # When closing a bonus round, debit all pending contributions.
    if (as.character(input$adm_status) == "closed" &&
        as.character(input$adm_game) == "bonus") {
      n_debited <- debit_pending_contributions(as.integer(input$adm_round), sec)
      if (n_debited > 0) {
        showNotification(
          sprintf("Debited bonus contributions for %d student(s) in section '%s'.", n_debited, sec),
          type = "message"
        )
      }
    }
  })

  observeEvent(input$adm_clear, {
    req(is_admin(), identical(Sys.getenv("ALLOW_LEGACY_COORDINATION_ADMIN"), "1"))
    o   <- get_olig()
    r   <- as.integer(o$current_round[1])
    sec <- olig_section(o)

    # Reverse contribution debits before removing submissions, so student
    # balances are restored as if the round never happened.
    n_debits <- db_exec(
      "DELETE FROM token_ledger
       WHERE round_id=? AND source_type='coordination_contrib'
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
    req(is_admin(), identical(Sys.getenv("ALLOW_LEGACY_COORDINATION_ADMIN"), "1"))
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
    req(is_admin(), identical(Sys.getenv("ALLOW_LEGACY_COORDINATION_ADMIN"), "1"))
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
    req(is_admin(), identical(Sys.getenv("ALLOW_LEGACY_COORDINATION_ADMIN"), "1"))
    res <- tryCatch(backup_to_sheets(), error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    showNotification(res$msg, type = if (isTRUE(res$ok)) "message" else "error")
  })

  observeEvent(input$adm_backup_drive, {
    req(is_admin(), identical(Sys.getenv("ALLOW_LEGACY_COORDINATION_ADMIN"), "1"))
    res <- tryCatch(backup_db_to_drive(), error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    showNotification(res$msg, type = if (isTRUE(res$ok)) "message" else "error")
  })

  # ---------------- Impersonation ----------------
  observeEvent(input$impersonate_start, {
    req(is_admin(), identical(Sys.getenv("ALLOW_LEGACY_COORDINATION_ADMIN"), "1"), input$impersonate_select)
    uid <- as.character(input$impersonate_select)
    row <- db_query("SELECT display_name FROM users WHERE user_id=?;", list(uid))
    if (!nrow(row)) { showNotification("User not found.", type = "error"); return() }
    rv$impersonate      <- TRUE
    rv$impersonate_uid  <- uid
    rv$impersonate_name <- row$display_name[1]
    showNotification(
      sprintf("Now impersonating %s. Switch to Student tab to submit as them.", row$display_name[1]),
      type = "warning"
    )
  })

  observeEvent(input$impersonate_stop, {
    req(is_admin(), identical(Sys.getenv("ALLOW_LEGACY_COORDINATION_ADMIN"), "1"))
    rv$impersonate      <- FALSE
    rv$impersonate_uid  <- NULL
    rv$impersonate_name <- NULL
    showNotification("Stopped impersonating.", type = "message")
  })

  session$onSessionEnded(function() {
    if (!is.null(conn) && DBI::dbIsValid(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
  })
}

shinyApp(ui, server)
