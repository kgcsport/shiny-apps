`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

logf <- function(...) cat(format(Sys.time()), "-", paste(..., collapse=" "), "\n", file=stderr())

get_credentials <- function() {
  b64 <- Sys.getenv("CRED_B64", "")
  if (nzchar(b64)) {
    logf("Loading credentials from CRED_B64")
    raw <- base64decode(b64)
    return(read_csv(raw, show_col_types = FALSE, trim_ws = TRUE))
  }
  csv <- Sys.getenv("CRED_CSV", "")
  if (nzchar(csv)) {
    logf("Loading credentials from CRED_CSV")
    con <- textConnection(csv); on.exit(close(con))
    return(read_csv(con, show_col_types = FALSE, trim_ws = TRUE))
  }
  path <- Sys.getenv("CRED_PATH", "")
  if (nzchar(path)) {
    logf("Loading credentials from CRED_PATH:", path)
    stopifnot(file.exists(path))
    return(read_csv(path, show_col_types = FALSE, trim_ws = TRUE))
  }
  stop("No credentials found: set CRED_B64 (preferred), or CRED_CSV, or CRED_PATH")
}

title_from_html <- function(x) {
  s <- as.character(x)
  t <- stringr::str_extract(s, "(?s)(?<=<b>).+?(?=</b>)")
  if (is.na(t)) {
    t <- stringr::str_remove_all(s, "<[^>]+>")
    t <- stringr::str_squish(t)
    t <- stringr::str_trunc(t, 120)
  }
  t
}

app_data_dir <- local({
  dir <- NULL
  function() {
    if (!is.null(dir)) return(dir)
    root <- Sys.getenv("CONNECT_CONTENT_DIR", unset = getwd())
    if (!nzchar(root)) stop("CONNECT_CONTENT_DIR not set; configure Posit Connect app data.")
    d <- file.path(root, "data")
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(d)) stop(sprintf("Data directory not writable: %s", d))
    tf <- file.path(d, ".writetest"); on.exit(unlink(tf, force=TRUE), add=TRUE)
    if (!isTRUE(try(file.create(tf), silent = TRUE))) {
      stop(sprintf("Data directory not writable: %s", d))
    }
    dir <<- normalizePath(d, winslash = "/", mustWork = TRUE)
    dir
  }
})

DATA_DIR <- app_data_dir()
DB_PATH  <- file.path(DATA_DIR, "appdata.sqlite")

get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  }
  conn
}

db_exec <- function(sql, params = NULL, con = NULL) {
  con <- if (is.null(con)) get_con() else con
  DBI::dbExecute(con, sql, params = params)
}

db_query <- function(sql, params = NULL, con = NULL) {
  con <- if (is.null(con)) get_con() else con
  DBI::dbGetQuery(con, sql, params = params)
}

touch_heartbeat <- function() db_exec("UPDATE game_state SET updated_at = CURRENT_TIMESTAMP WHERE id=1;")
is_open         <- function(x) isTRUE(as.logical(x))

q_settings   <- function() db_query("SELECT * FROM settings WHERE id=1;")
q_state      <- function() db_query("SELECT * FROM game_state WHERE id=1;")
q_round_tot  <- function(r) db_query("SELECT COALESCE(SUM(pledge),0) AS pledged,
                                      COUNT(DISTINCT user_id) AS n_users
                                      FROM pledges WHERE round=?;", list(as.integer(r)))
q_user_round <- function(uid, r) db_query("SELECT COALESCE(pledge,0) AS p
                                           FROM pledges WHERE user_id=? AND round=?;",
                                           list(uid, as.integer(r)))

init_db <- function() {
  logf("Creating users table...")
  db_exec("
    CREATE TABLE IF NOT EXISTS users (
      user_id TEXT PRIMARY KEY,
      display_name TEXT,
      is_admin INTEGER DEFAULT 0
    );")
  db_exec("
    CREATE TABLE IF NOT EXISTS settings (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      cost REAL,
      max_per_student REAL,
      slider_step REAL,
      max_for_admin REAL,
      round_timeout_sec REAL,
      shortfall_policy TEXT
    );")
  db_exec("
    CREATE TABLE IF NOT EXISTS game_state (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      round INTEGER,
      round_open INTEGER,
      carryover REAL,
      unlocked_units INTEGER,
      scale_factor REAL,
      question_text TEXT,
      started_at TEXT,
      updated_at TEXT DEFAULT (CURRENT_TIMESTAMP)
    );")
  db_exec("
    CREATE TABLE IF NOT EXISTS pledges (
      round INTEGER,
      user_id TEXT,
      name TEXT,
      pledge REAL,
      charged REAL DEFAULT 0,
      submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (user_id, round)
    );")
  db_exec("CREATE INDEX IF NOT EXISTS ix_pledges_round ON pledges(round);")
  db_exec("
    CREATE TABLE IF NOT EXISTS charges (
      round INTEGER,
      user_id TEXT,
      amount REAL,
      charged_at TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (user_id, round)
    );")
  
  nset <- db_query("SELECT COUNT(*) n FROM settings WHERE id=1;")$n[1]
  if (is.na(nset) || nset == 0) {
    db_exec("
      INSERT INTO settings(id, cost, max_per_student, slider_step, max_for_admin, round_timeout_sec, shortfall_policy)
      VALUES(1, 24, 7.5, 0.5, 100, NULL, 'bank_all');")
  }
  ngs <- db_query("SELECT COUNT(*) n FROM game_state WHERE id=1;")$n[1]
  if (is.na(ngs) || ngs == 0) {
    db_exec(
     "INSERT INTO game_state(id, round, round_open, carryover, unlocked_units, scale_factor, question_text, started_at)
       VALUES(1, 1, 0, 0, 0, NULL, ?, NULL);",
      params = list(as.character(QUESTIONS[[1]]))
    )
  }

  purrr::walk(seq_len(nrow(CRED)), function(i){
    db_exec("
      INSERT INTO users(user_id, display_name, is_admin)
      VALUES(?, ?, ?)
      ON CONFLICT(user_id) DO UPDATE
        SET display_name = excluded.display_name,
            is_admin    = excluded.is_admin;",
      params = list(CRED$user[i], CRED$name[i], as.integer(isTRUE(CRED$is_admin[i]))))
  })
}

drive_folder_id <- function() Sys.getenv("PUB_ECON_FOLDER_ID", "")

overwrite_ws <- function(ss, sheet_name, df) {
  tabs <- tryCatch(googlesheets4::sheet_names(ss), error = function(e) character(0))
  if (sheet_name %in% tabs) {
    googlesheets4::sheet_delete(ss, sheet_name)
  }
  googlesheets4::sheet_add(ss, sheet_name)
  googlesheets4::range_write(ss, data = df, sheet = sheet_name, range = "A1")
}

google_auth <- function() {
  logf("Initializing Google Sheets authentication...")
  already_authed <- tryCatch(!is.null(googledrive::drive_token()), error = function(e) FALSE)
  if (already_authed) {
    logf("Already authenticated — nothing to do")
    return(invisible(TRUE))
  }
  logf("Not authenticated, continuing...")

  cred <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (!nzchar(cred) || !file.exists(cred)) {
    json_txt <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
    if (nzchar(json_txt)) {
      cred <- tempfile(fileext = ".json")
      writeLines(json_txt, cred)
    }
  }
  if (!nzchar(cred) || !file.exists(cred)) {
    message("google_auth(): No valid GOOGLE_APPLICATION_CREDENTIALS found.")
    return(FALSE)
  }
  logf(sprintf("GOOGLE_APPLICATION_CREDENTIALS: %s", cred))

  tryCatch({
    googledrive::drive_auth(
      path   = cred,
      scopes = c(
        "https://www.googleapis.com/auth/drive",
        "https://www.googleapis.com/auth/drive.file",
        "https://www.googleapis.com/auth/spreadsheets"
      )
    )
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    message("google_auth(): authenticated successfully.")
    TRUE
  }, error = function(e) {
    message("google_auth(): FAILED — ", conditionMessage(e))
    FALSE
  })
}

backup_db_to_drive <- function() {
  google_auth()
  folder_id <- drive_folder_id()
  googledrive::drive_get(googledrive::as_id(folder_id))

  con <- get_con()
  try(DBI::dbExecute(con, "PRAGMA wal_checkpoint(FULL);"), silent = TRUE)

  files <- c(DB_PATH,
             paste0(DB_PATH, "-wal"),
             paste0(DB_PATH, "-shm"))
  files <- files[file.exists(files)]

  zipfile <- file.path(tempdir(), sprintf("appdata_%s.zip",
                     format(Sys.time(), "%Y%m%d_%H%M%S")))
  utils::zip(zipfile, files = files, flags = "-j")
  file.copy(zipfile, file.path(tempdir(), "appdata_latest_backup.zip"))

  nm <- basename(zipfile)
  googledrive::drive_upload(media = zipfile,
                            path  = googledrive::as_id(folder_id),
                            name  = nm,
                            type  = "application/zip",
                            overwrite = FALSE)

  googledrive::drive_upload(media = file.path(tempdir(), "appdata_latest_backup.zip"),
                            path  = googledrive::as_id(folder_id),
                            name  = "appdata_latest_backup.zip",
                            type  = "application/zip",
                            overwrite = TRUE)

  logf(sprintf("DB backup uploaded: %s", nm))

  ss_id <- Sys.getenv("FINALQ_SHEET_ID", "")
  if (!nzchar(ss_id)) {
    logf("Backup skipped: FINALQ_SHEET_ID not set")
    return(invisible(FALSE))
  }

  det <- tryCatch(
    db_query("SELECT * FROM pledges ORDER BY COALESCE(submitted_at, '')"),
    error = function(e) { logf(paste("backup: pledges query failed:", e$message)); NULL }
  )

  summ <- tryCatch(
    db_query("
      SELECT
        round,
        COUNT(*)                         AS n_pledges,
        COUNT(DISTINCT user_id)             AS n_users,
        ROUND(SUM(COALESCE(pledge,0)),2) AS total_points,
        ROUND(SUM(COALESCE(charged,0)),2) AS total_charged
      FROM pledges
      GROUP BY 1
      ORDER BY 1
    "),
    error = function(e) { logf(paste("backup: summary query failed:", e$message)); NULL }
  )

  gsnap <- tryCatch(
    db_query("
      SELECT id, round, round_open, carryover, unlocked_units,
             scale_factor, question_text, started_at, updated_at
      FROM game_state WHERE id = 1
    "),
    error = function(e) { logf(paste("backup: game_state snapshot failed:", e$message)); NULL }
  )

  if (is.null(det) || nrow(det) == 0) {
    logf("Backup skipped: no pledges found.")
    return(invisible(FALSE))
  }

  tabs <- tryCatch(googlesheets4::sheet_names(ss_id), error = function(e) character(0))

  if (!"pledges" %in% tabs)        googlesheets4::sheet_add(ss_id, "pledges")
  if (!"round_summary" %in% tabs)  googlesheets4::sheet_add(ss_id, "round_summary")
  if (!is.null(gsnap) && nrow(gsnap) > 0 && !"game_state_snapshot" %in% tabs)
    googlesheets4::sheet_add(ss_id, "game_state_snapshot")

  overwrite_ws(ss_id, "pledges", det)

  if (is.null(summ) || !is.data.frame(summ)) {
    summ <- tibble::tibble(round = integer(), n_pledges = integer(),
                          n_users = integer(), total_points = numeric(),
                          total_charged = numeric())
  }
  overwrite_ws(ss_id, "round_summary", summ)

  if (!is.null(gsnap) && nrow(gsnap) == 1) {
    overwrite_ws(ss_id, "game_state_snapshot", gsnap)
  } else {
    logf("backup: no valid game_state snapshot to write (nrow=%s)", if (is.null(gsnap)) NA else nrow(gsnap))
  }

  logf("Backup complete: pledges=%s rows; summary rows=%s; snapshot rows=%s",
       nrow(det), if (is.null(summ)) 0 else nrow(summ), if (is.null(gsnap)) 0 else nrow(gsnap))
  invisible(TRUE)
}

backup_db_async <- function(label = "async backup") {
  logf(sprintf("Starting %s (async)...", label))
  promises::future_promise({
    backup_db_to_drive()
  }) %...>% (function(ok) {
    logf(sprintf("%s completed with result: %s", label, as.character(ok)))
  }) %...!% (function(e) {
    logf(sprintf("%s FAILED: %s", label, conditionMessage(e)))
  }) -> .ignored
  invisible(TRUE)
}

cached_poll <- function(interval_ms, session, check_sql, value_sql, default_df) {
  cache <- shiny::reactiveVal(default_df)
  shiny::reactivePoll(
    interval_ms, session,
    checkFunc = function() {
      res <- try(db_query(check_sql), silent = TRUE)
      if (inherits(res, "try-error") || !is.data.frame(res) || !nrow(res)) {
        as.character(Sys.time())
      } else {
        as.character(res[[1]][1])
      }
    },
    valueFunc = function() {
      val <- try(db_query(value_sql), silent = TRUE)
      if (!inherits(val, "try-error") && is.data.frame(val) && nrow(val)) cache(val)
      cache()
    }
  )
}

recompute_state_from_pledges <- function(cost) {
  P <- tryCatch(
    db_query("SELECT round, COALESCE(SUM(pledge),0) AS pledged
              FROM pledges GROUP BY round ORDER BY round;"),
    error = function(e) data.frame()
  )
  if (!nrow(P)) return(list(unlocked = 0L, carry = 0))
  carry <- 0
  unlocked_total <- 0L
  for (i in seq_len(nrow(P))) {
    eff  <- carry + as.numeric(P$pledged[i])
    units <- as.integer(floor(eff / cost))
    carry <- eff - units * cost
    unlocked_total <- unlocked_total + units
  }
  list(unlocked = unlocked_total, carry = carry)
}

# Global connection variable
conn <- NULL

restore_db_from_drive <- function(filename = "appdata_latest_backup.zip") {
  folder_id <- drive_folder_id()
  googledrive::drive_get(googledrive::as_id(folder_id))
  id <- googledrive::drive_ls(googledrive::as_id(folder_id)) |>
    dplyr::filter(name == filename) |>
    dplyr::pull(id)
  zipfile <- file.path(tempdir(), filename)
  googledrive::drive_download(file = id, path = zipfile, overwrite = TRUE)
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    try(DBI::dbDisconnect(conn), silent = TRUE)
  }
  conn <<- NULL
  for (f in c(DB_PATH, paste0(DB_PATH, "-wal"), paste0(DB_PATH, "-shm"))) {
    if (file.exists(f)) try(unlink(f, force = TRUE), silent = TRUE)
  }
  utils::unzip(zipfile, exdir = dirname(DB_PATH))
  try(touch_heartbeat(), silent = TRUE)
  invisible(TRUE)
}

init_gs4 <- function() {
  google_auth()
  ss <- Sys.getenv("FINALQ_SHEET_ID", "")
  if (nzchar(ss)) {
    invisible(googledrive::drive_get(googledrive::as_id(ss)))
  }
  tabs <- googlesheets4::sheet_names(ss)
  logf("tabs: %s", paste(tabs, collapse = ", "))
  if (length(tabs) == 0) {
    stop("No tabs found in the sheet. Please check the sheet ID and permissions.")
  }
  if (!file.exists(DB_PATH)) {
    logf("No local DB found; attempting restore from Drive...")
    ok <- tryCatch(restore_db_from_drive(), error = function(e) {
      logf("Restore from drive failed: %s", e$message)
      FALSE
    })
    if (isTRUE(ok)) {
      logf("Restore from drive complete (cold start).")
    } else {
      logf("Restore from drive unavailable; starting with fresh DB.")
    }
  } else {
    logf(sprintf("Local DB already present at %s; skipping auto-restore.", DB_PATH))
  }
}

blank_settings <- function() tibble(
  id=1, cost=24, max_per_student=7.5, slider_step=0.5,
   max_for_admin=100, round_timeout_sec=NA_real_, shortfall_policy="bank_all"
)

blank_state <- function() tibble(
  id=1, round=1, round_open=0, carryover=0, unlocked_units=0,
  scale_factor=NA_real_, question_text=as.character(QUESTIONS[[1]]),
  started_at=NA_character_, updated_at=as.character(Sys.time())
)

get_settings <- function() { out <- q_settings(); if (nrow(out)) out else blank_settings() }
get_state    <- function() { out <- q_state();    if (nrow(out)) out else blank_state() }

set_settings <- function(...) {
  dots <- list(...)
  if (!length(dots)) return(invisible(TRUE))
  clauses <- c(); params <- list()
  for (nm in names(dots)) {
    val <- dots[[nm]]
    if (length(val) != 1) stop(sprintf("`%s` must be length 1", nm))
    if (is.na(val)) {
      clauses <- c(clauses, sprintf("%s = NULL", nm))
    } else {
      clauses <- c(clauses, sprintf("%s = ?", nm)); params <- c(params, list(val))
    }
  }
  db_exec(paste0("UPDATE settings SET ", paste(clauses, collapse=", "), " WHERE id=1;"), params)
  touch_heartbeat()
  invisible(TRUE)
}

set_state <- function(...) {
  dots <- list(...)
  if (!length(dots)) return(invisible(touch_heartbeat()))
  nm <- names(dots)
  sql <- paste0("UPDATE game_state SET ", paste0(nm, " = ?", collapse=", "),
                ", updated_at = CURRENT_TIMESTAMP WHERE id=1;")
  db_exec(sql, unname(dots))
  invisible(TRUE)
}

upsert_pledge <- function(user_id, round, pledge, name=NULL){
  if (!is.null(name)) {
    db_exec("UPDATE users SET display_name=? WHERE user_id=?;", params = list(name, user_id))
  }
  db_exec("
    INSERT INTO pledges(user_id, round, name, pledge, submitted_at)
    VALUES(?, ?, ?, ?, CURRENT_TIMESTAMP)
    ON CONFLICT(user_id, round) DO UPDATE SET pledge=excluded.pledge, submitted_at=CURRENT_TIMESTAMP;",
    params = list(user_id, as.integer(round), name, as.numeric(pledge)))
  set_state()
}

round_totals <- function(round){
  tryCatch(
    db_query("SELECT COALESCE(SUM(pledge),0) AS pledged, COUNT(DISTINCT user_id) AS n_users
                         FROM pledges WHERE round = ?;", params = list(as.integer(round))),
    error = function(e) tibble(pledged = 0, n_users = 0)
  )
}

user_cumulative_charged <- function(user_id){
  tryCatch(
    db_query("SELECT COALESCE(SUM(amount),0) AS total FROM charges WHERE user_id=?;", params = list(user_id))$total[1],
    error = function(e) 0
  )
}

charge_round_bank_all <- function(rnd) {
  con <- get_con()
  DBI::dbExecute(con, "DELETE FROM charges WHERE round = ?;", params = list(as.integer(rnd)))
  DBI::dbExecute(con, "INSERT INTO charges(user_id, round, amount)
     SELECT user_id, round, pledge FROM pledges WHERE round = ?;", params = list(as.integer(rnd)))
}

.compute_wtp <- function(st, s) {
  cost      <- as.numeric(s$cost %||% 24)
  carryover <- as.numeric(st$carryover %||% 0)
  rt <- db_query("SELECT COALESCE(SUM(pledge),0) AS pledged
                  FROM pledges WHERE round = ?;",
                params = list(as.integer(st$round)))
  pledged <- as.numeric(rt$pledged[1] %||% 0)
  eff   <- pledged + carryover
  units <- as.integer(floor(eff / cost))
  carry <- max(0, eff - units * cost)
  list(
    pledged = pledged,
    carryover = carryover,
    cost = cost,
    effective_total = eff,
    units_now = units,
    carryforward = carry
  )
}

refund_carryover_proportionally <- function(carry){
  if (carry <= 0) return(invisible(FALSE))
  tc <- db_query("SELECT user_id, COALESCE(SUM(amount),0) AS charged FROM charges GROUP BY user_id;")
  tot <- sum(tc$charged)
  if (tot <= 0) return(invisible(FALSE))
  tc$refund <- carry * (tc$charged / tot)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  con <- get_con()
  purrr::pwalk(tc[, c("user_id","refund")], function(user_id, refund){
    db_exec("INSERT INTO charges(user_id, round, amount, charged_at) VALUES(?, 9999, ?, ?);", params = list(user_id, -as.numeric(refund), now), con=con)
  })
  db_exec("UPDATE game_state SET carryover=0, round_open=0, updated_at=CURRENT_TIMESTAMP WHERE id=1;", con=con)
  TRUE
}

render_unlocked_questions <- function(units) {
  idx <- seq_len(min(units, length(QUESTIONS)))
  tagList(
    h5(if (units == 1) "Unlocked Question" else sprintf("Unlocked Questions (%d)", units)),
    lapply(rev(idx), function(i) {
      wellPanel(div(style="font-size:1.15em; line-height:1.5;", HTML(as.character(QUESTIONS[[i]]))))
    })
  )
}
