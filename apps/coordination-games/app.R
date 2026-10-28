try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
# app.R -- Coordination Games (Bonus Pot + Price War / PD)
# -------------------------------------------------------------------
# Uses the shared DB (class-job-market.sqlite) for auth and token accounting.
# Section-aware: sections are stored in the `sections` DB table with
# their enrolled class size. The bonus-pot denominator is either the
# section's class size OR the submitter count -- no other values.
#
# Shared game records live in the Class Job Market database. The standalone
# Bonus Pot app and Class Job Market are supported control surfaces over the
# same state, submissions, and payouts.
#
# Class Job Market owns schema initialization and embeds the player when useful.
# This standalone module provides:
#   - direct student access to the active shared game
#   - optional instructor administration of that same shared game state
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
GOOGLE_AUTH_ENABLED <- nzchar(Sys.getenv("GOOGLE_CLIENT_ID", "")) && nzchar(Sys.getenv("GOOGLE_CLIENT_SECRET", "")) && nzchar(Sys.getenv("SHINY_APP_URL", ""))

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
# Class Job Market initializes the shared schema. This module deliberately
# does not create a second set of tables or defaults when launched directly.
init_olig <- function() invisible(TRUE)
init_olig()
# Class Job Market owns the shared schema, but this idempotent migration lets
# a direct Bonus Pot visit work immediately after deployment as well.
ensure_olig_column <- function(name, definition) {
  cols <- tryCatch(db_query("PRAGMA table_info(olig_settings);")$name,
                   error = function(e) character(0))
  if (!name %in% cols) db_exec(paste0("ALTER TABLE olig_settings ADD COLUMN ", definition, ";"))
}
ensure_olig_column("show_contributions", "show_contributions INTEGER DEFAULT 0")
touch_olig <- function() db_exec("UPDATE olig_settings SET updated_at=CURRENT_TIMESTAMP WHERE id=1;")
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
        "INSERT INTO token_ledger(user_id, display_name, source_type, source_id, amount, earning, note)
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
        "INSERT INTO token_ledger(user_id, display_name, source_type, source_id, amount, earning, note)
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
      "SELECT COUNT(*) n FROM token_ledger WHERE user_id=? AND source_id=? AND source_type='coordination_contrib';",
      list(uid, as.integer(round))
    )$n[1]
    if (as.integer(already) > 0) next
    db_exec(
      "INSERT INTO token_ledger(user_id, display_name, source_type, source_id, amount, earning, note)
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
login_ui <- function(msg = NULL) {
  div(class = "login-page",
    div(class = "login-card",
      div(class = "login-logo", "🎓 Bonus Pot"),
      div(class = "login-tagline", if (GOOGLE_AUTH_ENABLED) "Sign in with your school Google account." else "Log in with credentials from your instructor."),
      if (!is.null(msg)) div(style = "color:#b00020;font-weight:bold;margin-bottom:.75rem;", msg),
      if (GOOGLE_AUTH_ENABLED) tags$a(href = "/auth/login?return_to=%2Fcoordination-games%2F", class = "btn-google",
        tags$svg(xmlns = "http://www.w3.org/2000/svg", viewBox = "0 0 24 24", height = "18", width = "18",
          tags$path(fill = "#4285F4", d = "M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92c-.26 1.37-1.04 2.53-2.21 3.31v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.09z"),
          tags$path(fill = "#34A853", d = "M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z"),
          tags$path(fill = "#FBBC05", d = "M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z"),
          tags$path(fill = "#EA4335", d = "M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 5.29-4.53 6.16-4.53z")),
        "Sign in with Google"),
      if (GOOGLE_AUTH_ENABLED) tags$details(class = "admin-login-toggle", tags$summary("Sign in with password"), textInput("login_user", NULL, placeholder = "Username"), passwordInput("login_pw", NULL, placeholder = "Password"), actionButton("login_btn", "Sign In →", class = "btn btn-primary btn-block")) else tagList(textInput("login_user", NULL, placeholder = "Username"), passwordInput("login_pw", NULL, placeholder = "Password"), actionButton("login_btn", "Sign In →", class = "btn btn-primary btn-block")),
      tags$hr(style = "margin:.9rem 0 .5rem;"),
      tags$p(style = "font-size:.75rem;color:#aaa;text-align:center;margin-bottom:.4rem;", "What's here"),
      div(class = "preview-grid", div(class = "preview-card", div(class = "preview-card-icon", "📋"), div(class = "preview-card-label", "Today"), div(class = "preview-card-desc", "Jobs, wages & active game")), div(class = "preview-card", div(class = "preview-card-icon", "🏪"), div(class = "preview-card-label", "Job Market"), div(class = "preview-card-desc", "Bid for or apply to jobs")), div(class = "preview-card", div(class = "preview-card-icon", "🎮"), div(class = "preview-card-label", "Games & Demos"), div(class = "preview-card-desc", "Live games + interactive demos"))),
      tags$details(class = "login-howto", tags$summary("How to get started"), tags$ul(tags$li(tags$strong("Sign in"), " using your school Google account. Your instructor must add your email to the class roster first."), tags$li(tags$strong("Bonus Pot"), " lets you make a contribution and follow the shared outcome."), tags$li(tags$strong("Class Job Market"), " has your assignments, bids, and account."))),
      demo_login_ui
    )
  )
}

shared_auth_js <- tags$script(HTML("
(function() {
  Shiny.addCustomMessageHandler('set_arcade_cookie', function(msg) {
    if (msg.token) {
      document.cookie = 'arcade_token=' + encodeURIComponent(msg.token) +
        '; expires=' + new Date(msg.expires).toUTCString() + '; path=/; SameSite=Lax';
    }
  });
  function sendAuthCookie() { Shiny.setInputValue('auth_cookie', (document.cookie.match(/(?:^|; )arcade_token=([^;]*)/) || ['', ''])[1], {priority: 'event'}); }
  $(document).on('shiny:sessioninitialized', sendAuthCookie);
  // Shiny emits this as a jQuery event; the native listener above never fired,
  // leaving a visitor stranded on the checking-sign-in screen.
  setTimeout(sendAuthCookie, 300);
  Shiny.addCustomMessageHandler('redirect_to_job_market', function(msg) { window.location.replace(msg.url || '/class-job-market/?return_to=%2Fcoordination-games%2F'); });
  function applyFontScale(scale) { localStorage.setItem('bonusPotFontScale', scale); document.documentElement.style.setProperty('--bp-font-scale', scale); }
  document.addEventListener('click', function(e) {
    var button = e.target.closest('#bp-font-down, #bp-font-reset, #bp-font-up');
    if (!button) return;
    var current = parseFloat(localStorage.getItem('bonusPotFontScale') || '1');
    applyFontScale(button.id === 'bp-font-up' ? Math.min(1.35, current + .1) : button.id === 'bp-font-down' ? Math.max(.85, current - .1) : 1);
  });
  applyFontScale(parseFloat(localStorage.getItem('bonusPotFontScale') || '1'));
})();
"))

ui <- fluidPage(class = "bp-app",
  tags$head(shared_auth_js, tags$style(HTML("
    /* --- Base font --- */
    :root { --bp-font-scale: 1; }
    body { font-size: calc(16px * var(--bp-font-scale)); background: #f7f7f5; color: #222; }

    /* --- Vassar palette overrides for Bootstrap elements --- */
    .btn-primary  { background-color: #951829; border-color: #7a1221; }
    .btn-primary:hover { background-color: #7a1221; border-color: #5e0d19; }
    .btn-success  { background-color: #2d6a4f; border-color: #245c43; }
    .btn-success:hover { background-color: #245c43; border-color: #1b4d38; }
    a { color: #951829; }
    a:hover { color: #7a1221; }
    .login-page { max-width:420px; margin:4rem auto; padding:0 1rem; }
    .login-card { background:#fff; border-radius:12px; padding:2rem 2.25rem; box-shadow:0 3px 14px rgba(0,0,0,.1); }
    .login-logo { font-size:1.6rem; font-weight:700; color:#951829; margin-bottom:.35rem; text-align:center; }
    .login-tagline { font-size:.83rem; color:#888; text-align:center; margin-bottom:1.25rem; }
    .login-card .btn { min-height:44px; padding:.55rem .9rem; font-size:.96rem; }
    .login-card .form-control { min-height:44px; font-size:1rem; }
    .btn-block { width:100%; }
    .preview-grid { display:grid; grid-template-columns:1fr 1fr; gap:.5rem; margin:1rem 0 .75rem; }
    .preview-card { background:#f8f8f8; border:1px solid #e8e8e8; border-radius:6px; padding:.55rem .65rem; }
    .preview-card-icon { font-size:1.1rem; }
    .preview-card-label { font-weight:600; font-size:.85rem; }
    .preview-card-desc { color:#888; font-size:.74rem; }
    .login-howto { margin-top:1rem; }
    .login-howto summary { cursor:pointer; font-weight:600; color:#951829; font-size:.85rem; text-align:center; }
    .login-howto ul { margin:.5rem 0 0; padding-left:1.1rem; font-size:.84rem; color:#555; }
    .login-howto li { margin-bottom:.3rem; }
    .btn-google { display:flex; align-items:center; justify-content:center; gap:.55rem; width:100%; padding:.65rem 1rem; background:#fff; border:1.5px solid #dadce0; border-radius:6px; font-size:.95rem; font-weight:500; color:#3c4043; cursor:pointer; transition:background .15s, box-shadow .15s; margin-bottom:.75rem; }
    .btn-google:hover { background:#f8f9fa; box-shadow:0 1px 4px rgba(0,0,0,.15); }
    .btn-google svg { flex-shrink:0; }
    .admin-login-toggle { margin-top:.6rem; }
    .admin-login-toggle summary { cursor:pointer; font-size:.82rem; color:#888; text-align:center; padding:.3rem 0; }
    .admin-login-toggle summary:hover { color:#555; }
    .admin-login-toggle .form-group { margin-top:.6rem; }

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
    /* --- Bonus Pot player surface --- */
    .bp-app { max-width: 980px; margin: 0 auto; padding-bottom: 2rem; }
    .bp-header { background:#951829; color:#fff; padding:.7rem 1.5rem; display:flex; align-items:center; gap:.75rem; margin:0 calc((980px - 100vw) / 2) 1rem; padding-left:max(1.5rem, calc((100vw - 980px) / 2)); padding-right:max(1.5rem, calc((100vw - 980px) / 2)); }
    .bp-header-title { font-size:1.3rem; font-weight:700; flex:1; letter-spacing:.02em; }
    .bp-header-name { font-size:.88rem; opacity:.85; }
    .bp-env-picker { position:relative; font-size:.82rem; }
    .bp-env-picker summary { list-style:none; cursor:pointer; white-space:nowrap; padding:.35rem .55rem; border:1px solid rgba(255,255,255,.45); border-radius:6px; background:rgba(255,255,255,.12); }
    .bp-env-picker summary::-webkit-details-marker { display:none; }
    .bp-env-picker summary::after { content:\" ▾\"; }
    .bp-env-picker[open] summary::after { content:\" ▴\"; }
    .bp-env-menu { position:absolute; right:0; top:calc(100% + .35rem); z-index:1050; min-width:320px; display:flex; gap:.45rem; padding:.55rem; border-radius:7px; background:#fff; color:#222; box-shadow:0 4px 16px rgba(0,0,0,.28); }
    .bp-env-field { flex:1; min-width:0; }
    .bp-env-field > span { display:block; font-size:.72rem; font-weight:700; color:#555; margin:0 0 .15rem .1rem; }
    .bp-env-field .form-group { margin:0; }
    .bp-env-field .selectize-control { margin:0; }
    .bp-env-field .selectize-input { min-height:34px; padding:6px 8px; font-size:.83rem; }
    .bp-topbar { display:flex; align-items:center; justify-content:space-between; gap:1rem; margin:1rem 0 .5rem; }
    .bp-topbar h2 { margin:0; color:#67111e; font-weight:700; }
    .bp-topbar p { margin:.2rem 0 0; color:#5e5e5e; }
    .bp-tools { display:flex; align-items:center; gap:.35rem; white-space:nowrap; }
    .bp-tools button { min-width:2.5rem; min-height:2.5rem; border-radius:6px; border:1px solid #bbb; background:#fff; font-weight:700; }
    .bp-tools button:hover { border-color:#951829; color:#951829; }
    .bp-app .nav-tabs { border-bottom:2px solid #ded8d8; margin-bottom:1.25rem; }
    .bp-app .nav-tabs > li > a { font-weight:650; font-size:1.05em; padding:.75rem 1rem; }
    .bp-app .nav-tabs > li.active > a { color:#751222; border-color:#ded8d8 #ded8d8 transparent; }
    .bp-card { background:#fff; border:1px solid #e3dddd; border-radius:10px; padding:1.25rem; margin-bottom:1rem; box-shadow:0 1px 4px rgba(45,20,20,.06); }
    .bp-card h3, .bp-card h4 { margin-top:0; color:#67111e; }
    .bp-meta { display:flex; flex-wrap:wrap; gap:.55rem; margin:.75rem 0 1rem; }
    .bp-pill { display:inline-block; background:#f3eeee; border-radius:999px; padding:.4rem .7rem; font-weight:600; }
    .bp-status-open { background:#dcefe2; color:#175c32; }
    .bp-status-closed, .bp-status-revealed { background:#f5e5e7; color:#761423; }
    .bp-submit { width:100%; min-height:3.1rem; font-size:1.1em; font-weight:700; border-radius:7px; margin-top:.5rem; }
    .bp-app .irs { margin:1rem .45rem 1.4rem; }
    .bp-app .radio-inline { min-height:3rem; padding:.65rem 1rem .65rem 2.1rem; margin:.35rem .6rem .35rem 0; border:1px solid #ddd; border-radius:7px; font-size:1.05em; }
    .bp-app .radio-inline:has(input:checked) { border-color:#951829; background:#fbf4f5; }
    .bp-note { color:#565656; line-height:1.45; }
    .bp-result { border-left:4px solid #951829; padding:.85rem 1rem; background:#fbf7f7; border-radius:0 7px 7px 0; }
    .bp-projector { font-size:1.15em; }
    @media (max-width: 640px) { .bp-header { flex-wrap:wrap; margin-left:-15px; margin-right:-15px; padding:.6rem .75rem; } .bp-header-title { flex-basis:100%; } .bp-env-menu { right:auto; left:0; min-width:min(320px, calc(100vw - 1.5rem)); } .bp-topbar { align-items:flex-start; flex-direction:column; } .bp-tools { align-self:flex-end; } .bp-card { padding:1rem; } }
    .shiny-notification-close   { color: rgba(255,255,255,0.8); font-size: 1.2rem; }
  "))),
  uiOutput("demo_banner"),
  uiOutput("auth_gate"),
  conditionalPanel("output.authed",
    div(class = "bp-header",
      div(class = "bp-header-title", "🎓 Classroom Economy · Bonus Pot"),
      uiOutput("bp_header_widgets", inline = TRUE)
    ),
    div(class = "bp-topbar",
      div(h2("Bonus Pot"), p("Make a contribution, follow the round, and see the shared outcome.")),
      div(class = "bp-tools", tags$a(href="/class-job-market/", class="btn btn-default", "Class Job Market"), tags$span("Text size"),
        tags$button(type="button", id="bp-font-down", "A−"),
        tags$button(type="button", id="bp-font-reset", "A"),
        tags$button(type="button", id="bp-font-up", "A+"))
    ),
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
                       home_course = "", home_section = "", active_course = "", active_section = "", environment_ver = 0L,
                       impersonate = FALSE, impersonate_uid = NULL, impersonate_name = NULL,
                       auth_message = NULL)
  output$demo_banner <- renderUI(demo_banner_ui(.is_demo, rv$is_admin))
  output$authed <- reactive(rv$authed)
  outputOptions(output, "authed", suspendWhenHidden = FALSE)
  output$auth_gate <- renderUI({ if (!rv$authed) login_ui(rv$auth_message) else NULL })

  norm_key <- function(x) tolower(trimws(as.character(x)))
  unique_ci <- function(x) {
    x <- trimws(as.character(x))
    x <- x[!is.na(x) & nzchar(x)]
    x[!duplicated(tolower(x))]
  }
  admin_assignments_for <- function(user_id) tryCatch(
    db_query("SELECT role, course, section FROM admin_assignments WHERE LOWER(user_id)=LOWER(?) ORDER BY CASE role WHEN 'super_admin' THEN 1 WHEN 'course_admin' THEN 2 ELSE 3 END, course, section;", list(user_id)),
    error = function(e) data.frame(role=character(0), course=character(0), section=character(0))
  )
  session_environment_rows <- function() {
    if (!isTRUE(rv$authed)) return(data.frame(course=character(0), section=character(0)))
    if (!isTRUE(rv$is_admin)) {
      out <- tryCatch(db_query("SELECT ue.course, ue.section FROM user_environments ue JOIN users u ON LOWER(u.user_id)=LOWER(ue.user_id) WHERE LOWER(ue.user_id)=LOWER(?) AND COALESCE(u.active,1)=1 AND COALESCE(u.is_demo,0)=0 ORDER BY ue.course, ue.section;", list(rv$user)), error=function(e) data.frame())
      if (!nrow(out) && nzchar(rv$home_course)) out <- data.frame(course=rv$home_course, section=rv$home_section, stringsAsFactors=FALSE)
      return(out)
    }
    roles <- admin_assignments_for(rv$user)
    if (!nrow(roles)) return(data.frame(course=character(0), section=character(0)))
    if (any(roles$role == "super_admin")) {
      return(tryCatch(db_query("SELECT DISTINCT course, section FROM users WHERE COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0 AND TRIM(COALESCE(course,''))<>'' ORDER BY course, section;"), error=function(e) data.frame(course=character(0), section=character(0))))
    }
    out <- lapply(seq_len(nrow(roles)), function(i) {
      course <- trimws(as.character(roles$course[i] %||% ""))
      if (!nzchar(course)) return(data.frame(course=character(0), section=character(0)))
      if (identical(as.character(roles$role[i]), "course_admin")) {
        return(tryCatch(db_query("SELECT DISTINCT course, section FROM users WHERE COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0 AND LOWER(COALESCE(course,''))=LOWER(?) AND TRIM(COALESCE(section,''))<>'' ORDER BY section;", list(course)), error=function(e) data.frame(course=character(0), section=character(0))))
      }
      data.frame(course=course, section=trimws(as.character(roles$section[i] %||% "")), stringsAsFactors=FALSE)
    })
    out <- do.call(rbind, out)
    if (is.null(out) || !nrow(out)) return(data.frame(course=character(0), section=character(0)))
    out[!duplicated(paste(norm_key(out$course), norm_key(out$section))), , drop=FALSE]
  }
  pin_bonus_environment <- function() {
    env <- session_environment_rows()
    if (!nrow(env)) { rv$active_course <- ""; rv$active_section <- ""; return(invisible(FALSE)) }
    home <- which(norm_key(env$course) == norm_key(rv$home_course) & norm_key(env$section) == norm_key(rv$home_section))
    pick <- if (length(home)) home[1] else 1L
    rv$active_course <- env$course[pick]; rv$active_section <- env$section[pick]
    invisible(TRUE)
  }
  switch_bonus_environment <- function(course, section) {
    env <- session_environment_rows()
    hit <- which(norm_key(env$course) == norm_key(course) & norm_key(env$section) == norm_key(section))
    if (!length(hit)) return(FALSE)
    rv$active_course <- env$course[hit[1]]; rv$active_section <- env$section[hit[1]]
    rv$environment_ver <- rv$environment_ver + 1L
    updateSelectInput(session, "adm_section", selected=rv$active_section)
    TRUE
  }
  lookup_shared_session <- function(token) {
    if (!nzchar(token %||% "")) return(data.frame())
    tryCatch(db_query("SELECT u.user_id, u.display_name, u.is_admin, u.active, u.course, u.section FROM arcade_sessions s JOIN users u ON u.user_id=s.user_id WHERE s.token=? AND datetime(s.expires_at)>datetime('now') LIMIT 1;", list(token)), error=function(e) data.frame())
  }
  finish_login <- function(row) {
    rv$authed <- TRUE; rv$user <- row$user_id[1]; rv$name <- row$display_name[1]; rv$is_admin <- isTRUE(as.integer(row$is_admin[1] %||% 0L) == 1L)
    rv$home_course <- as.character(row$course[1] %||% ""); rv$home_section <- as.character(row$section[1] %||% "")
    pin_bonus_environment(); rv$auth_message <- NULL
  }
  issue_shared_cookie <- function(user_id) {
    token <- paste(sample(c(letters, LETTERS, 0:9), 48L, replace=TRUE), collapse="")
    db_exec("INSERT INTO arcade_sessions(token,user_id,expires_at) VALUES(?,?,datetime('now','+14 days'));", list(token, user_id))
    session$sendCustomMessage("set_arcade_cookie", list(token=token, expires=format(Sys.time() + 14*86400, "%Y-%m-%dT%H:%M:%SZ", tz="UTC")))
  }
  observeEvent(input$auth_cookie, {
    if (rv$authed || .is_demo) return()
    token <- input$auth_cookie %||% ""
    if (!nzchar(token)) return()
    row <- lookup_shared_session(token)
    if (nrow(row) && !isTRUE(as.integer(row$active[1] %||% 1L) == 0L)) { finish_login(row); return() }
    rv$auth_message <- "Your previous sign-in has expired. Please sign in below."
  }, ignoreInit=FALSE)
  observeEvent(input$login_btn, {
    user_id <- tolower(trimws(as.character(input$login_user %||% "")))
    password <- as.character(input$login_pw %||% "")
    if (!nzchar(user_id) || !nzchar(password)) { rv$auth_message <- "Enter your username or email and password."; return() }
    row <- tryCatch(db_query("SELECT user_id,display_name,pw_hash,is_admin,active,course,section FROM users WHERE LOWER(user_id)=LOWER(?) LIMIT 1;", list(user_id)), error=function(e) data.frame())
    valid <- nrow(row) == 1L && isTRUE(nzchar(row$pw_hash[1])) && isTRUE(tryCatch(bcrypt::checkpw(password, row$pw_hash[1]), error=function(e) FALSE))
    if (!valid) { rv$auth_message <- "Incorrect username or password."; return() }
    if (isTRUE(as.integer(row$active[1] %||% 1L) == 0L)) { rv$auth_message <- "This account has been archived. Contact your instructor."; return() }
    finish_login(row)
    if (!.is_demo) issue_shared_cookie(row$user_id[1])
  })


  authed   <- reactive(rv$authed)
  user_id  <- reactive({
    if (isTRUE(rv$impersonate) && !is.null(rv$impersonate_uid)) rv$impersonate_uid else rv$user
  })
  name     <- reactive({
    if (isTRUE(rv$impersonate) && !is.null(rv$impersonate_name)) rv$impersonate_name else rv$name
  })
  is_admin <- reactive(rv$is_admin)

  output$bp_header_widgets <- renderUI({
    req(authed())
    env <- session_environment_rows()
    courses <- unique_ci(env$course)
    course <- rv$active_course
    if (!course %in% courses && length(courses)) course <- courses[1]
    sections <- unique_ci(env$section[norm_key(env$course) == norm_key(course)])
    section <- rv$active_section
    if (!section %in% sections && length(sections)) section <- sections[1]
    tagList(
      span(class="bp-header-name", rv$name),
      if (nrow(env) > 1L)
        tags$details(class="bp-env-picker",
          tags$summary("Class / section"),
          div(class="bp-env-menu",
            div(class="bp-env-field", tags$span("Class"),
              selectInput("environment_course_sel", NULL, choices=setNames(courses, courses), selected=course, width="100%")),
            div(class="bp-env-field", tags$span("Section"),
              selectInput("environment_section_sel", NULL, choices=setNames(sections, sections), selected=section, width="100%"))
          )
        )
    )
  })
  observeEvent(input$environment_course_sel, {
    req(authed())
    env <- session_environment_rows()
    course <- as.character(input$environment_course_sel %||% "")
    rows <- env[norm_key(env$course) == norm_key(course), , drop=FALSE]
    if (!nrow(rows)) return()
    section <- rv$active_section
    if (!norm_key(section) %in% norm_key(rows$section)) section <- rows$section[1]
    switch_bonus_environment(course, section)
  }, ignoreInit=TRUE)
  observeEvent(input$environment_section_sel, {
    req(authed())
    switch_bonus_environment(input$environment_course_sel %||% rv$active_course,
                             input$environment_section_sel %||% "")
  }, ignoreInit=TRUE)
  bonus_game_in_selected_environment <- function(o) {
    selected <- trimws(as.character(rv$active_section %||% ""))
    live <- trimws(olig_section(o))
    if (nzchar(live) && !identical(norm_key(selected), norm_key(live))) return(FALSE)
    scope_key <- paste0(norm_key(rv$active_course), "::", norm_key(selected))
    state <- tryCatch(db_query("SELECT active_game FROM scope_arcade_state WHERE scope_key=? LIMIT 1;", list(scope_key)), error=function(e) data.frame())
    !nrow(state) || !nzchar(as.character(state$active_game[1] %||% "")) || identical(as.character(state$active_game[1]), "bonus_pot")
  }

  # Poll every 1200ms: shared state via olig_settings.updated_at for in-class games.
  olig_poll <- reactivePoll(
    1200, session,
    checkFunc = function() get_olig()$updated_at[1] %||% as.character(Sys.time()),
    valueFunc  = function() get_olig()
  )

  # Poll every 1200ms: submissions filtered to the current section. The
  # signature includes values, not merely row count: a student may replace an
  # existing Bonus Pot contribution, including replacing it with zero.
  subs_poll <- reactivePoll(
    1200, session,
    checkFunc = function() {
      o <- get_olig()
      paste0(
        o$updated_at[1] %||% "", "|",
        db_query(
          "SELECT COUNT(*) || '|' || COALESCE(SUM(COALESCE(contribute,0)),0) || '|' ||
                  COALESCE(GROUP_CONCAT(user_id || ':' || COALESCE(contribute,'') || ':' || COALESCE(action,''), '|'),'') AS sig
             FROM olig_submissions
             WHERE round=? AND (section=? OR section IS NULL);",
          list(as.integer(o$current_round[1]), olig_section(o))
        )$sig[1] %||% ""
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
    rv$environment_ver
    if (!bonus_game_in_selected_environment(o)) return(div(class="bp-card", h3("No active Bonus Pot game"), p(class="bp-note", sprintf("Bonus Pot is not active for %s right now.", rv$active_section))))
    sec <- olig_section(o)

    bal    <- token_balance(user_id())
    game   <- as.character(o$current_game[1])
    status <- as.character(o$round_status[1])

    game_lbl <- if (game == "pd") "Price War" else "Bonus Pot"
    status_class <- paste0("bp-pill bp-status-", status)
    use_sec <- isTRUE(as.integer(o$use_section_size[1] %||% 1) == 1)
    cs <- if (use_sec) section_class_size(sec) else NA_integer_
    cs_note <- if (!is.na(cs)) sprintf("The pot is shared with all %d enrolled students in this section.", cs) else "The pot is shared among students who submit."
    cap <- as.numeric(o$contrib_cap[1] %||% 0)
    eff_max <- if (is.finite(cap) && cap > 0) min(bal, cap) else bal

    tagList(
      div(class = "bp-card",
        h3(game_lbl),
        div(class = "bp-meta",
          span(class = "bp-pill", sprintf("Round %d", as.integer(o$current_round[1]))),
          span(class = "bp-pill", sprintf("Section %s", sec)),
          span(class = "bp-pill", sprintf("%.1f participation tokens available", bal)),
          span(class = status_class, toupper(status))
        ),
        if (isTRUE(rv$impersonate)) div(class = "alert alert-warning", strong(sprintf("IMPERSONATING: %s — submissions are recorded for this student.", rv$impersonate_name)))
      ),
      div(class = "bp-card",
        h4(if (status == "open") "Make or update your choice" else "This round is not accepting submissions"),
        if (status != "open") div(class = "bp-note", "Wait for the instructor to open the next round. Results appear here after the round is revealed."),
        if (game == "pd") {
          tagList(
            radioButtons("pd_action", "Choose a price", choiceNames = c("High price", "Low price"), choiceValues = c("High", "Low"), inline = TRUE),
            if (status == "open") actionButton("submit_pd", "Save my choice", class = "btn-primary bp-submit")
          )
        } else {
          tagList(
            sliderInput("bonus_c", "Contribution (participation tokens)", min = 0, max = max(0, floor(eff_max * 2) / 2), value = 0, step = 0.5),
            if (status == "open") div(class="bp-actions",
              actionButton("submit_bonus", "Save / replace contribution", class = "btn-primary bp-submit"),
              actionButton("reset_bonus", "Reset to 0", class = "btn-default bp-submit")
            ),
            p(class = "bp-note", sprintf("Multiplier: %.2f×. %s Contributions are debited only when the round closes.", as.numeric(o$bonus_multiplier[1]), cs_note))
          )
        }
      ),
      div(class = "bp-card bp-result", uiOutput("student_result")),
      uiOutput("public_contribution_list")
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
    if (!is_admin() && !bonus_game_in_selected_environment(o)) { showNotification("Bonus Pot is not active in the selected class section.", type="error"); return() }
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
    if (!is_admin() && !bonus_game_in_selected_environment(o)) { showNotification("Bonus Pot is not active in the selected class section.", type="error"); return() }
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
      sprintf("Saved %.1f participation token(s) as your contribution. You can replace or reset it until the round closes.", c_val),
      type = "message"
    )
  })

  observeEvent(input$reset_bonus, {
    req(authed())
    o <- get_olig()
    if (!is_admin() && !bonus_game_in_selected_environment(o)) {
      showNotification("Bonus Pot is not active in the selected class section.", type="error")
      return()
    }
    if (as.character(o$round_status[1]) != "open" ||
        as.character(o$current_game[1]) != "bonus") {
      showNotification("This Bonus Pot round is not open for changes.", type="error")
      return()
    }
    sec <- olig_section(o)
    db_exec(
      "INSERT OR REPLACE INTO olig_submissions(round, user_id, display_name, game, action, contribute, section)
       VALUES(?, ?, ?, 'bonus', NULL, 0, ?);",
      list(as.integer(o$current_round[1]), as.character(user_id()),
           as.character(name()), sec)
    )
    updateSliderInput(session, "bonus_c", value=0)
    showNotification("Contribution reset to 0. You can still replace it until the round closes.", type="message")
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
          p(strong(if (c_saved > 0)
            sprintf("Saved contribution: %.1f participation token(s).", c_saved)
            else "Your contribution is set to 0.")),
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

  # Contributions remain sealed for students until Reveal by default. An
  # instructor may explicitly enable the existing public-contribution setting
  # to show the live named list before then.
  output$public_contribution_list <- renderUI({
    req(authed())
    st <- subs_poll(); o <- st$olig
    visible <- identical(as.character(o$round_status[1] %||% ""), "revealed") ||
      isTRUE(as.integer(o$show_contributions[1] %||% 0L) == 1L)
    if (!visible || !identical(as.character(o$current_game[1] %||% ""), "bonus")) return(NULL)
    rows <- st$subs %>%
      filter(game == "bonus") %>%
      transmute(
        Student = ifelse(!is.na(display_name) & nzchar(trimws(as.character(display_name))),
                         as.character(display_name), "Student"),
        Contribution = sprintf("%.1f", suppressWarnings(as.numeric(contribute)))
      ) %>%
      arrange(Student)
    if (!nrow(rows)) {
      return(div(class="bp-card bp-result", h4("Class contributions"),
                 p(class="bp-note", "No contributions have been submitted yet.")))
    }
    div(class="bp-card bp-result",
        h4("Class contributions"),
        p(class="bp-note", if (identical(as.character(o$round_status[1] %||% ""), "revealed"))
          "The round has been revealed."
          else "Your instructor has chosen to show live named contributions for this round."),
        tags$table(class="table table-sm table-striped", style="margin-bottom:0;",
          tags$thead(tags$tr(tags$th("Student"), tags$th(style="text-align:right;", "Contribution"))),
          tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
            tags$tr(tags$td(rows$Student[i]),
                    tags$td(style="text-align:right;", rows$Contribution[i]))
          }))
        )
    )
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
      div(class = "bp-card bp-projector",
        h3(sprintf("Round %d · %s", r, game_lbl)),
        div(class = "bp-meta",
          span(class = "bp-pill", sprintf("Section %s", sec)),
          span(class = paste0("bp-pill bp-status-", status), toupper(status)),
          span(class = "bp-pill", sprintf("%d submission%s", n, if (n == 1) "" else "s"))
        ),
        if (cur_game == "pd") {
          tagList(
            h4("Class choices"),
            p(sprintf("High price: %d · Low price: %d", sum(subs$action == "High", na.rm = TRUE), sum(subs$action == "Low", na.rm = TRUE))),
            if (status == "revealed") tagList(h4("Pair outcomes"), tableOutput("proj_pd_counts"))
          )
        } else {
          show_live_bids <- isTRUE(as.integer(o$show_contributions[1] %||% 0L) == 1L)
          bids_visible <- identical(status, "revealed") || show_live_bids
          if (!bids_visible) {
            tagList(
              h4("Class contribution"),
              p(class="bp-note", "Contributions are sealed until the instructor reveals the round.")
            )
          } else {
            use_sec <- isTRUE(as.integer(o$use_section_size[1] %||% 1) == 1)
            cs <- if (use_sec) section_class_size(olig_section(o)) else NA_integer_
            cs_note <- if (!is.na(cs)) sprintf("Shared with %d enrolled students", cs) else sprintf("Shared among %d submitters", n)
            tagList(
              h4("Class contribution"),
              p(sprintf("Total: %.2f participation tokens · Multiplier: %.2f× · %s", sum(as.numeric(subs$contribute), na.rm = TRUE), as.numeric(o$bonus_multiplier[1]), cs_note)),
              plotOutput("proj_bonus_plot", height = "350px"),
              if (status == "revealed") {
                shares <- bonus_shares(o, subs %>% filter(game == "bonus"))
                if (nrow(shares) > 0) div(class = "bp-result", h4("Round result"), p(sprintf("Pot total: %.2f · Each student receives: %.2f participation tokens", shares$pot_total[1], round_to_half(shares$share_each[1]))))
              }
            )
        }
        }
      )
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
    bids_visible <- identical(as.character(o$round_status[1] %||% ""), "revealed") ||
      isTRUE(as.integer(o$show_contributions[1] %||% 0L) == 1L)
    req(bids_visible)
    df <- subs %>% filter(round == r, game == "bonus") %>%
      mutate(contribute = as.numeric(contribute))
    req(nrow(df) > 0)

    ggplot(df, aes(x = contribute)) +
      geom_histogram(fill = "#0073C2FF", color = "white", binwidth = 0.5, boundary = 0) +
      labs(title = "Distribution of Contributions",
           x = "Participation Tokens Contributed",
           y = "Number of Students") +
      scale_x_continuous(breaks = seq(0, max(df$contribute, na.rm = TRUE) + 0.5, by = 0.5)) +
      theme_minimal(base_size = 16)
  })

  # Standalone admin controls use the same shared Bonus Pot tables as Class Job Market.
  output$admin_ui <- renderUI({
    req(authed())
    if (!is_admin()) return(fluidPage(h4("Admin"), p("You are not an admin.")))

    o       <- olig_poll()
    rv$environment_ver
    sec     <- olig_section(o)
    use_sec <- as.integer(o$use_section_size[1] %||% 1)
    has_col <- users_has_section_col()
    cs      <- if (has_col && use_sec == 1L) section_class_size(sec) else NA_integer_
    denom_lbl <- if (!is.na(cs)) sprintf("section class size (%d)", cs) else "submitter count"

    # Section choices derived from users table (re-read on each render)
    allowed_sections <- unique_ci(session_environment_rows()$section)
    all_sec_choices <- if (has_col) sections_choices() else setNames("", "(users table has no section column)")
    sec_choices <- all_sec_choices[norm_key(as.character(all_sec_choices)) %in% norm_key(allowed_sections)]
    if (!length(sec_choices)) sec_choices <- setNames("", "(no authorized sections)")

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
        " table in ", tags$code("class-job-market.sqlite"),
        " and assign each student to their section before using section-based class sizes."
      ),

      tags$hr(),

      if (isTRUE(rv$impersonate)) div(
        style = "background:#92400e; color:#fff; padding:8px 16px; border-radius:6px; margin-bottom:12px;",
        strong(sprintf("IMPERSONATING: %s (%s)", rv$impersonate_name, rv$impersonate_uid)),
        " — Student tab actions (PD choice, bonus contribution) are attributed to this student."
      ),

      wellPanel(
        h5("Impersonate student (act as any student)"),
        p("As admin, simulate game participation as any student. The Student tab will show their view and all submissions are recorded under their identity."),
        fluidRow(
          column(6, selectInput("impersonate_select", "Student to impersonate",
            choices = {
              us <- db_query("SELECT user_id, display_name FROM users WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND LOWER(COALESCE(course,''))=LOWER(?) AND LOWER(COALESCE(section,''))=LOWER(?) ORDER BY display_name;", list(rv$active_course, rv$active_section))
              setNames(us$user_id, us$display_name)
            }
          )),
          column(3, br(), actionButton("impersonate_start", "Start impersonating", class = "btn-warning")),
          column(3, br(), actionButton("impersonate_stop",  "Stop impersonating",  class = "btn-secondary"))
        )
      ),

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
                        selected = if (nzchar(rv$active_section)) rv$active_section else sec)
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
          ),
          column(4,
            checkboxInput("adm_show_contributions",
                          "Show named contributions publicly",
                          value=isTRUE(as.integer(o$show_contributions[1] %||% 0L) == 1L))
          )
        ),
        actionButton("adm_apply", "Apply round settings", class = "btn-primary"),
        actionButton("adm_clear", "Clear submissions (this round + section)", class = "btn-danger"),
        tags$small(
          "Switch Active Section when moving between class sections. ",
          "Clearing removes submissions for the current section and reverses ",
          "any bonus contribution debits from the token ledger."
        )
      ),

      # --- Game Parameters ---
      wellPanel(
        h5("Game Parameters"),
        numericInput("adm_m", "Bonus multiplier m",
                     value = as.numeric(o$bonus_multiplier[1]), min = 1, step = 0.1),
        numericInput("adm_contrib_cap", "Max participation tokens students can wager (0 = no cap)",
                     value = as.numeric(o$contrib_cap[1] %||% 0), min = 0, step = 0.5),
        numericInput("adm_pd_scale", "PD scale (participation tokens per payoff point)",
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
        actionButton("adm_payout", "Apply payouts to token ledger (one-time per section)", class = "btn-success"),
        tags$small(
          "Credits students in the current section via source_type='coordination_grant'. ",
          "Bonus contributions are debited when the round is closed."
        )
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
    allowed_env <- session_environment_rows()
    allowed_hit <- which(norm_key(allowed_env$course) == norm_key(rv$active_course) & norm_key(allowed_env$section) == norm_key(sec))
    if (!length(allowed_hit)) { showNotification("Choose a class section you are authorized to manage.", type="error"); return() }
    cs      <- if (use_sec == 1L) section_class_size(sec) else NA_integer_
    denom_lbl <- if (!is.na(cs)) sprintf("section class size (%d)", cs) else "submitter count"

    db_exec(
      "UPDATE olig_settings SET current_round=?, current_game=?, round_status=?,
       section=?, use_section_size=?, show_contributions=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;",
      list(as.integer(input$adm_round), as.character(input$adm_game),
           as.character(input$adm_status), sec, use_sec,
           if (isTRUE(input$adm_show_contributions)) 1L else 0L)
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
    req(is_admin())
    o   <- get_olig()
    if (!is_admin() && !bonus_game_in_selected_environment(o)) { showNotification("Bonus Pot is not active in the selected class section.", type="error"); return() }
    r   <- as.integer(o$current_round[1])
    sec <- olig_section(o)

    # Reverse contribution debits before removing submissions, so student
    # balances are restored as if the round never happened.
    n_debits <- db_exec(
      "DELETE FROM token_ledger
       WHERE source_id=? AND source_type='coordination_contrib'
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

  # ---------------- Impersonation ----------------
  observeEvent(input$impersonate_start, {
    req(is_admin(), input$impersonate_select)
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
    req(is_admin())
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
