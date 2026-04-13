try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
# app.R — streamlined + bug-fixed
# ------------------------------------------------------------
# Exam Question unlock (collective) + Flex passes + Exam points (individual)
#
# Key rules:
# - Students start with `initial_fp` flex passes, but can earn more via admin grants
# - 24-hour extensions: no purchase cooldown; capped by count per pset (max_per_pset setting)
# - Exam points can be purchased in integer units
# - Collective question unlock cost rises by round using a schedule (editable)
#
# Credentials:
# - Expect columns: user, name, pw_hash, is_admin
# - You can seed users from env vars (CRED_B64/CRED_CSV/CRED_PATH) or (locally) a file.
# - Passwords are stored as bcrypt hashes in SQLite (pw_hash).
#
# Notes:
# - Question "pledges" are allocations (not charged) until admin closes the round.
# - Ledger records actual spending/grants. Grants are stored as negative amounts.
# Deploy to 3838 port
# shiny::runApp(appDir = "C:/Users/kgcsp/OneDrive/Documents/Education/Teaching/shiny-apps/final_question_reveal", port = 3838, host = "127.0.0.1")

library(shiny); library(DT); library(bcrypt); library(dplyr); library(tibble)
library(readr); library(DBI); library(RSQLite); library(stringr)
library(googlesheets4); library(googledrive); library(future); library(promises); library(digest)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

logf <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste(vapply(list(...), as.character, character(1)), collapse = " ")
  cat(ts, "-", msg, "\n", file = stderr()); flush(stderr())
}

APP_CONFIG <- list(
  folder_id = Sys.getenv("FLEX_PASS_FOLDER_ID", ""),
  # if you use a Drive folder id var with a different name, use that here
  gsa_json  = Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", ""),
  gsa_path  = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
)

logf("CONFIG: folder_id present:", nzchar(APP_CONFIG$folder_id),
     "| gsa_json present:", nzchar(APP_CONFIG$gsa_json),
     "| gsa_path:", APP_CONFIG$gsa_path)

# -------------------------
# Optional Google Sheets backup (service account)
# -------------------------
gs_backup_sheet_id <- function() Sys.getenv("FLEX_PASS_SHEET_ID", "")

get_gs_cred_path <- function() {
  # Prefer a file path; fall back to inline JSON.
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

google_auth_ok <- function() {
  ss <- gs_backup_sheet_id()
  if (!nzchar(ss)) return(FALSE)

  # If already authed, accept.
  already <- tryCatch(!is.null(googlesheets4::gs4_token()), error = function(e) FALSE)
  if (isTRUE(already)) return(TRUE)

  cred <- get_gs_cred_path()
  if (!nzchar(cred) || !file.exists(cred)) return(FALSE)

  scopes <- c(
    "https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/drive.file"
  )

  ok <- tryCatch({
    googlesheets4::gs4_auth(path = cred, scopes = scopes)
    googledrive::drive_auth(path = cred, scopes = scopes)
    TRUE
  }, error = function(e) {
    logf("google_auth_ok():", conditionMessage(e))
    FALSE
  })

  ok
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
  ss <- gs_backup_sheet_id()
  if (!nzchar(ss)) return(invisible(FALSE))
  if (!google_auth_ok()) {
    logf("backup_to_sheets(): auth not available")
    return(invisible(FALSE))
  }

  # Fail fast if no access
  tryCatch(googledrive::drive_get(googledrive::as_id(ss)), error = function(e) {
    stop("No access to FLEX_PASS_SHEET_ID: ", conditionMessage(e))
  })

  users_df   <- db_query("SELECT * FROM users ORDER BY is_admin DESC, display_name;")
  settings_df<- db_query("SELECT * FROM settings WHERE id=1;")
  state_df   <- db_query("SELECT * FROM game_state WHERE id=1;")
  pledges_df <- db_query("SELECT * FROM pledges ORDER BY round, user_id;")
  ledger_df  <- db_query("SELECT * FROM ledger ORDER BY datetime(created_at) DESC;")
  export_df  <- tryCatch(admin_student_summary(), error = function(e) tibble())

  overwrite_ws(ss, "users", users_df)
  overwrite_ws(ss, "settings", settings_df)
  overwrite_ws(ss, "game_state", state_df)
  overwrite_ws(ss, "pledges", pledges_df)
  overwrite_ws(ss, "ledger", ledger_df)
  overwrite_ws(ss, "admin_export", export_df)

  job_state_df <- db_query("SELECT * FROM job_state ORDER BY section, job;")
  job_log_df   <- db_query("SELECT * FROM job_log   ORDER BY created_at DESC;")
  overwrite_ws(ss, "job_state", job_state_df)
  overwrite_ws(ss, "job_log",   job_log_df)

  invisible(TRUE)
}


# Robust admin coercion for varied CSV formats (TRUE/FALSE, 1/0, yes/no)
coerce_is_admin <- function(x) {
  x <- tolower(as.character(x %||% "false"))
  as.integer(x %in% c("true","t","1","yes","y"))
}

# -------------------------
# Questions (edit)
# -------------------------
QUESTIONS <- list(
  HTML("<b>Q1.</b><br>True/False/Uncertain: The economic cost and accounting cost of attending class are both zero."),
  HTML("<b>Q2.</b> True/False/Uncertain: People should specialize in the good for which they have an absolute advantage."),
  HTML("<b>Q3.</b> An accountant pays $80,000 in explicit costs and could earn $30,000 working elsewhere. Her total revenue is $95,000. Is she earning an economic profit, an economic loss, or breaking even? Briefly explain."),
  HTML("<b>Q4.</b> Suppose the price of lattes increases while all else remains constant. What happens to the <i>quantity demanded</i>? Explain briefly using the law of demand."),
  HTML("<b>Q5.</b> The demand for gasoline is relatively inelastic and supply is relatively elastic. If the government imposes a small tax, who bears most of the tax burden—buyers or sellers? Briefly explain."),
  HTML("<b>Q6.</b> True/False/Uncertain: Making the wealthy worse off to improve the condition of the poor is not Pareto efficient. Therefore, we should not increase taxes on the wealthy."),
  HTML("<b>Q7.</b> A bakery hires workers. The first worker produces 200 loaves. The second worker increases output by 100 loaves. The third increases output by 60 loaves. What economic principle does this demonstrate?"),
  HTML("<b>Q8.</b> Suppose demand for movie tickets is elastic. If theaters raise prices by 10%, what happens to total revenue? Briefly explain."),
  HTML("<b>Q9.</b> For each statement, label it <i>positive</i> or <i>normative</i> and briefly explain:<br> a) 'Raising the minimum wage will reduce employment among teenagers.'<br> b) 'The government should raise the minimum wage to reduce inequality.'<br> c) 'Rent control decreases the quantity of housing supplied.'"),
  HTML("<b>Q10.</b> A factory emits pollution that harms nearby residents. Is the market equilibrium socially efficient? If not, name one policy that could improve efficiency and briefly explain why."),
  HTML("<b>Q11.</b> True/False/Uncertain: A firm's marginal revenue is equal to the price for every unit it sells."),
  HTML("<b>Q12.</b> You notice that sellers at a fish market charge different prices to different buyers. Is this market operating under perfect competition? Briefly explain."),
  HTML("<b>Q13.</b> After patents expire, many firms produce chemically identical generic drugs. Which market structure best describes this industry: perfect competition, monopolistic competition, oligopoly, or monopoly? Briefly justify."),
  HTML("<b>Q14.</b> Suppose the most you would be willing to pay for a plane ticket home is $250. If you buy one for $175, what is your consumer surplus (in dollars)?"),
  HTML("<b>Q15.</b> Assume all firms have identical cost curves and the market is perfectly competitive. The minimum of average total cost is $5. What is the long-run equilibrium price?"),
  HTML("<b>Q16.</b> For each good, state whether it is <i>rival</i> and whether it is <i>excludable</i>:<br> a) A free public lecture held in a university lecture hall<br> b) A public park<br> c) Bicycles available to rent around a city<br> d) A forest used by people to collect firewood"),
  HTML("<b>Q17.</b> The government shifts a tax on the sale of cars from car buyers to car manufacturers. Will this save car buyers money <i>in equilibrium</i>? Briefly explain."),
  HTML("<b>Q18.</b> For each event below, state whether the <b>demand for new cars</b> shifts right, shifts left, or does not shift. Give a one-sentence reason:<br> a) ____________________<br> b) ____________________<br> c) ____________________"),
  HTML("<b>Q19.</b> A movie ticket price rises from $10 to $20 and sales fall from 80 to 40. Using the midpoint method, compute the price elasticity of demand and state what happens to total revenue. Show your work."),
  HTML("<b>Q20.</b> Lia's marginal rate of substitution (MRS) of tea for coffee is 3 (she'd give up 3 teas for 1 coffee). Coffee costs $4 and tea costs $2. Is Lia at her utility-maximizing bundle? If not, what should she do? Explain in 2–3 sentences."),
  HTML("<b>Q21.</b> A firm has fixed costs of $60. At Q = 4, average variable cost is $9. What is average total cost at Q = 4? Show your reasoning."),
  HTML("<b>Q22.</b> A firm has fixed costs of $80. Total revenue is $90 and variable costs are $110. Should it operate or shut down in the short run? Compare the loss under each option and state the shutdown rule."),
  HTML("<b>Q23.</b> A competitive firm faces P = $22. At Q = 7, MC = $22; at Q = 8, MC = $26. Should the firm produce an 8th unit? Briefly justify."),
  HTML("<b>Q24.</b> True/False/Uncertain: After a patent expires and other firms enter the market, total surplus will rise.")
)

render_unlocked_questions <- function(n_unlocked, questions = QUESTIONS) {
  if (!is.finite(n_unlocked) || n_unlocked <= 0) return(NULL)
  idx <- seq_len(min(as.integer(n_unlocked), length(questions)))
  tagList(
    h5(if (length(idx) == 1) "Unlocked Question" else sprintf("Unlocked Questions (%d)", length(idx))),
    lapply(rev(idx), function(i) wellPanel(div(style="font-size:1.1em; line-height:1.45;", questions[[i]])))
  )
}

# Load questions from DB for a given exam id; falls back to built-in QUESTIONS list
get_exam_questions <- function(exam_id) {
  if (!nzchar(exam_id %||% "")) return(QUESTIONS)
  rows <- tryCatch(
    db_query(
      "SELECT question_html FROM exam_questions WHERE exam_id=? ORDER BY question_num;",
      list(exam_id)
    ),
    error = function(e) data.frame()
  )
  if (!nrow(rows)) return(QUESTIONS)
  lapply(rows$question_html, HTML)
}

# Distinct exam ids that have questions loaded in the DB
list_exams_with_questions <- function() {
  ids <- tryCatch(
    db_query("SELECT DISTINCT exam_id FROM exam_questions ORDER BY exam_id;")$exam_id,
    error = function(e) character(0)
  )
  if (!length(ids)) character(0) else ids
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
    # check if directory was created
    if (!dir.exists(d)) stop("Data directory not created: ", d)
    tf <- file.path(d, ".writetest"); on.exit(unlink(tf, force = TRUE), add = TRUE)
    if (!isTRUE(try(file.create(tf), silent = TRUE))) stop("Data directory not writable: ", d)
    dir <<- normalizePath(d, winslash = "/", mustWork = TRUE)
    dir
  }
})

DATA_DIR <- app_data_dir()
DB_PATH  <- file.path(DATA_DIR, "finalqdata.sqlite")  # keep stable name for Drive snapshots

# -------------------------
# Persistent storage on Posit Cloud via Google Drive snapshots
# -------------------------
# Posit Connect Cloud instances may discard local files when the app goes idle.
# We therefore treat Google Drive as the source of truth and:
#   - RESTORE the latest DB snapshot on cold start (only if no local DB)
#   - upload a fresh snapshot on session end, daily, and on admin command

drive_folder_id <- function() APP_CONFIG$folder_id
latest_zip_name <- function() "finalqdata_latest_backup.zip"

google_auth <- function() {
  # Idempotent auth for both googledrive + googlesheets4
  already_authed <- tryCatch(!is.null(googledrive::drive_token()), error = function(e) FALSE)
  if (already_authed) return(invisible(TRUE))

  cred <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (!nzchar(cred) || !file.exists(cred)) {
    json_txt <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
    if (nzchar(json_txt)) {
      cred <- tempfile(fileext = ".json")
      writeLines(json_txt, cred)
    }
  }
  if (!nzchar(cred) || !file.exists(cred)) {
    logf("google_auth(): no service-account credentials available.")
    logf("cred:", cred, "and wd", getwd())
    return(invisible(FALSE))
  }

  # confirm that cred exists
  if (!file.exists(cred)) {
    logf("google_auth(): credentials file does not exist:", cred)
    return(invisible(FALSE))
  }

  ok <- tryCatch({
    googledrive::drive_auth(
      path   = cred,
      scopes = c(
        "https://www.googleapis.com/auth/drive",
        "https://www.googleapis.com/auth/drive.file",
        "https://www.googleapis.com/auth/spreadsheets"
      )
    )
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    TRUE
  }, error = function(e) {
    logf("google_auth(): failed:", conditionMessage(e))
    FALSE
  })

  invisible(ok)
}

# Hash DB + WAL + SHM to avoid redundant uploads
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

zip_db_files <- function() {
  files <- c(DB_PATH, paste0(DB_PATH, "-wal"), paste0(DB_PATH, "-shm"))
  files <- files[file.exists(files)]
  if (!length(files)) return(NULL)

  zipfile <- file.path(tempdir(), sprintf("finalqdata_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")))
  utils::zip(zipfile, files = files, flags = "-j")
  zipfile
}

ensure_latest_file_id <- function(folder_id) {
  files <- googledrive::drive_ls(googledrive::as_id(folder_id))
  hit <- files[files$name == latest_zip_name(), , drop = FALSE]

  if (nrow(hit) >= 1) return(hit$id[1])

  tmp <- tempfile()
  writeLines("", tmp)
  new_file <- googledrive::drive_upload(
    media = tmp,
    path  = googledrive::as_id(folder_id),
    name  = latest_zip_name()
  )
  new_file$id
}

backup_db_to_drive <- function() {
  if (!google_auth()) {
    return(list(ok = FALSE, msg = "google_auth() returned FALSE (no credentials / auth failed)"))
  }

  folder_id <- drive_folder_id()
  if (!nzchar(folder_id)) {
    return(list(ok = FALSE, msg = "FLEX_PASS_FOLDER_ID not set"))
  }

  # Access check
  tryCatch(
    googledrive::drive_get(googledrive::as_id(folder_id)),
    error = function(e) {
      stop("drive_get(folder) failed: ", conditionMessage(e))
    }
  )

  # checkpoint WAL
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  try(DBI::dbExecute(con, "PRAGMA wal_checkpoint(FULL);"), silent = TRUE)

  zipfile <- zip_db_files()
  if (is.null(zipfile)) {
    return(list(ok = FALSE, msg = "zip_db_files() returned NULL (DB files missing?)"))
  }

  latest_zip <- file.path(tempdir(), latest_zip_name())
  ok_copy <- file.copy(zipfile, latest_zip, overwrite = TRUE)
  if (!isTRUE(ok_copy)) {
    return(list(ok = FALSE, msg = "file.copy() to latest_zip failed"))
  }

  # Optional timestamped upload (ignore failures but keep message if it fails)
  ts_err <- NULL
  tryCatch({
    googledrive::drive_upload(
      media     = zipfile,
      path      = googledrive::as_id(folder_id),
      name      = basename(zipfile),
      type      = "application/zip",
      overwrite = FALSE
    )
  }, error = function(e) {
    ts_err <<- paste0("timestamped upload failed: ", conditionMessage(e))
  })

  # Latest overwrite upload (this is the one you care about)
  ok_latest <- tryCatch({
    googledrive::drive_upload(
      media     = latest_zip,
      path      = googledrive::as_id(folder_id),
      name      = latest_zip_name(),
      type      = "application/zip",
      overwrite = TRUE
    )
    TRUE
  }, error = function(e) {
    return(FALSE)
  })

  if (isTRUE(ok_latest)) {
    msg <- paste0("uploaded ", latest_zip_name(), if (!is.null(ts_err)) paste0(" (", ts_err, ")") else "")
    return(list(ok = TRUE, msg = msg))
  } else {
    # IMPORTANT: your old code had conditionMessage(e) here, but e doesn't exist in this scope.
    msg <- paste0("overwrite upload failed (no error surfaced here).",
                  if (!is.null(ts_err)) paste0(" Also: ", ts_err) else "")
    return(list(ok = FALSE, msg = msg))
  }
}

restore_db_from_drive <- function(filename = latest_zip_name()) {
  if (!google_auth()) return(invisible(FALSE))

  folder_id <- drive_folder_id()
  if (!nzchar(folder_id)) {
    logf("restore_db_from_drive(): FLEX_PASS_FOLDER_ID not set")
    return(invisible(FALSE))
  }
  googledrive::drive_get(googledrive::as_id(folder_id))

  id <- googledrive::drive_ls(googledrive::as_id(folder_id)) |>
    dplyr::filter(name == filename) |>
    dplyr::pull(id)

  if (!length(id)) {
    logf("restore_db_from_drive(): no snapshot found on Drive:", filename)
    return(invisible(FALSE))
  }

  zipfile <- file.path(tempdir(), filename)
  googledrive::drive_download(file = id[1], path = zipfile, overwrite = TRUE)

  # close existing connection
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    try(DBI::dbDisconnect(conn), silent = TRUE)
  }
  conn <<- NULL

  # remove current db files
  for (f in c(DB_PATH, paste0(DB_PATH, "-wal"), paste0(DB_PATH, "-shm"))) {
    if (file.exists(f)) try(unlink(f, force = TRUE), silent = TRUE)
  }

  utils::unzip(zipfile, exdir = dirname(DB_PATH))

  # If the snapshot contained a different DB basename (e.g., older app used finalqdata.sqlite),
  # normalize to our current DB_PATH so the app actually uses the restored DB.
  snap_base <- file.path(dirname(DB_PATH), "finalqdata.sqlite")
  if (!file.exists(DB_PATH) && file.exists(snap_base)) {
    file.rename(snap_base, DB_PATH)
    if (file.exists(paste0(snap_base, "-wal"))) file.rename(paste0(snap_base, "-wal"), paste0(DB_PATH, "-wal"))
    if (file.exists(paste0(snap_base, "-shm"))) file.rename(paste0(snap_base, "-shm"), paste0(DB_PATH, "-shm"))
    logf("restore_db_from_drive(): renamed restored DB to", basename(DB_PATH))
  }
  logf("restore_db_from_drive(): restored DB snapshot from Drive")
  invisible(TRUE)
}

backup_db_async <- function(label = "async backup") {
  cfg <- APP_CONFIG
  db_path <- DB_PATH

  # Temporarily spin up a single worker, then restore previous plan
  old_plan <- future::plan()
  future::plan(future::multisession, workers = 1)
  on.exit(future::plan(old_plan), add = TRUE)

  promises::future_promise({
    APP_CONFIG <<- cfg
    DB_PATH <<- db_path
    backup_db_to_drive()
  }) %...>% (function(res) {
    if (is.list(res)) {
      logf(label, "completed:", as.character(res$ok), "|", res$msg %||% "")
    } else {
      logf(label, "completed: unexpected return type:", paste(class(res), collapse = ","))
    }
    invisible(NULL)
  }) %...!% (function(e) {
    logf(label, "FAILED:", conditionMessage(e))
  }) -> .ignored

  invisible(TRUE)
}

logf("DB_PATH:", DB_PATH)
logf("FLEX_PASS_FOLDER_ID present:", nzchar(APP_CONFIG$folder_id))
logf("GOOGLE_APPLICATION_CREDENTIALS:", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS",""))
logf("GOOGLE_SERVICE_ACCOUNT_JSON nchar:", nchar(Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON","")))

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  conn
}
db_exec  <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

# Extra safety: try to disconnect at process exit too
reg.finalizer(.GlobalEnv, function(e) {
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    try(DBI::dbDisconnect(conn), silent = TRUE)
  }
}, onexit = TRUE)

DB_BROKEN <- FALSE  # set TRUE by init_db() if users table is missing PRIMARY KEY

init_db <- function() {
  # Users
  db_exec("
    CREATE TABLE IF NOT EXISTS users (
      user_id TEXT PRIMARY KEY,
      display_name TEXT,
      is_admin INTEGER DEFAULT 0
    );
  ")
  # Detect if users table lost PRIMARY KEY (e.g. from dbWriteTable overwrite).
  # Do NOT attempt self-repair — flag it and let the admin restore from Drive backup.
  tbl_def <- db_query("SELECT sql FROM sqlite_master WHERE type='table' AND name='users';")
  if (nrow(tbl_def) && !grepl("PRIMARY KEY", tbl_def$sql[1], ignore.case = TRUE)) {
    logf("ERROR: users table is missing PRIMARY KEY — DB is broken.",
         "The app will prompt for a Drive restore. No data has been deleted.")
    DB_BROKEN <<- TRUE
    return()  # skip remaining setup; server will show restore modal
  }

  # Ensure optional columns exist (safe no-ops if already present)
  try(db_exec("ALTER TABLE users ADD COLUMN pw_hash TEXT;"), silent = TRUE)
  try(db_exec("ALTER TABLE users ADD COLUMN section TEXT;"), silent = TRUE)

  # Settings (single row)
  db_exec("
    CREATE TABLE IF NOT EXISTS settings (
      id INTEGER PRIMARY KEY CHECK (id=1),
      initial_fp REAL,
      pledge_step REAL,
      flex_cost REAL,
      exam_point_cost REAL,
      question_cost_schedule TEXT,
      shortfall_policy TEXT,
      roundless_mode INTEGER DEFAULT 0,
      max_per_pset REAL DEFAULT 3,
      max_fp_held  REAL DEFAULT 8,
      extension_hours REAL DEFAULT 24,
      question_chance_pct REAL DEFAULT 50,
      question_point_value REAL DEFAULT 5,
      question_total_points REAL DEFAULT 100
    );
  ")

  # Ensure roundless_mode exists for older DBs
  try(db_exec("ALTER TABLE settings ADD COLUMN roundless_mode INTEGER DEFAULT 0;"), silent = TRUE)
  try(db_exec("ALTER TABLE settings ADD COLUMN exam_names TEXT;"), silent = TRUE)
  try(db_exec("ALTER TABLE settings ADD COLUMN pset_names TEXT;"), silent = TRUE)
  # add delay for older DBs
  try(db_exec("ALTER TABLE settings ADD COLUMN max_per_pset REAL DEFAULT 3;"), silent = TRUE)
  try(db_exec("ALTER TABLE settings ADD COLUMN max_fp_held  REAL DEFAULT 8;"), silent = TRUE)
  try(db_exec("ALTER TABLE settings ADD COLUMN extension_hours REAL DEFAULT 24;"), silent = TRUE)
  try(db_exec("ALTER TABLE settings ADD COLUMN question_chance_pct REAL DEFAULT 50;"), silent = TRUE)
  try(db_exec("ALTER TABLE settings ADD COLUMN question_point_value REAL DEFAULT 5;"), silent = TRUE)
  try(db_exec("ALTER TABLE settings ADD COLUMN question_total_points REAL DEFAULT 100;"), silent = TRUE)

  # Game state (single row)
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
  # Migrate: active exam column (which exam set is currently in use)
  try(db_exec("ALTER TABLE game_state ADD COLUMN active_exam TEXT DEFAULT 'exam1';"), silent = TRUE)

  # Exam questions bank: one row per question per exam
  db_exec("
    CREATE TABLE IF NOT EXISTS exam_questions (
      exam_id      TEXT NOT NULL,
      question_num INTEGER NOT NULL,
      question_html TEXT NOT NULL,
      created_at   TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (exam_id, question_num)
    );
  ")

  # Pledges (allocations for questions; charged only on close)
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

  # Ledger (all spending/grants; grants stored as negative amounts)
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

  # Resource targets (where students apply already-purchased passes)
  db_exec("
    CREATE TABLE IF NOT EXISTS resource_targets (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id       TEXT,
      resource_type TEXT,
      target        TEXT,
      quantity      INTEGER DEFAULT 1,
      created_at    TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at    TEXT DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(user_id, resource_type, target)
    );
  ")
  db_exec("CREATE INDEX IF NOT EXISTS ix_rt_user ON resource_targets(user_id);")

  # Job state for class-job-picker (shared DB)
  db_exec("
    CREATE TABLE IF NOT EXISTS job_state (
      section      TEXT NOT NULL,
      job          TEXT NOT NULL,
      cycle_id     INTEGER DEFAULT 1,
      bag_json     TEXT DEFAULT '[]',
      last_updated TEXT DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (section, job)
    );
  ")
  db_exec("
    CREATE TABLE IF NOT EXISTS job_log (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      logged_date  TEXT,
      section      TEXT,
      job          TEXT,
      display_name TEXT,
      cycle_id     INTEGER DEFAULT 0,
      created_at   TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")
  db_exec("CREATE INDEX IF NOT EXISTS ix_job_log_section ON job_log(section);")
  db_exec("CREATE INDEX IF NOT EXISTS ix_job_log_date    ON job_log(logged_date);")

  # Seed settings/state if missing
  nset <- db_query("SELECT COUNT(*) n FROM settings WHERE id=1;")$n[1]
  if (is.na(nset) || nset == 0) {
    db_exec("
      INSERT INTO settings(id, initial_fp, pledge_step, flex_cost, exam_point_cost, question_cost_schedule, shortfall_policy, roundless_mode, exam_names, pset_names, extension_hours, question_chance_pct, question_point_value, question_total_points)
      VALUES(1, 5, 0.5, 1, 1, '12,20,30,45,70', 'bank_all', 0, 'Midterm 1,Midterm 2,Final', 'PS1,PS2,PS3,PS4,PS5', 24, 50, 5, 100);
    ")
  }

  ngs <- db_query("SELECT COUNT(*) n FROM game_state WHERE id=1;")$n[1]
  if (is.na(ngs) || ngs == 0) {
    db_exec("INSERT INTO game_state(id, round, round_open, carryover, unlocked_questions, active_exam)
             VALUES(1, 1, 0, 0, 0, 'exam1');")
  }

  # Upsert roster from CRED (seed pw_hash if missing; always update admin flag & name)
  upsert_cred_to_db(CRED)
}

upsert_cred_to_db <- function(cred) {
  for (i in seq_len(nrow(cred))) {
    db_exec("
      INSERT INTO users(user_id, display_name, is_admin, pw_hash)
      VALUES(?, ?, ?, ?)
      ON CONFLICT(user_id) DO UPDATE SET
        display_name = excluded.display_name,
        is_admin     = excluded.is_admin,
        pw_hash      = COALESCE(users.pw_hash, excluded.pw_hash);
    ", list(
      as.character(cred$user[i]),
      as.character(cred$name[i]),
      coerce_is_admin(cred$is_admin[i]),
      as.character(cred$pw_hash[i])
    ))
  }
}

get_credentials <- function(
  sheet = "credentials",
  cache_path = NULL
) {

  sheet_id <- Sys.getenv("FLEX_PASS_SHEET_ID")

  # Ensure Drive auth is active (use your existing helper)
  if (!exists("google_auth", mode = "function")) {
    stop("get_credentials(): google_auth() not found. Define it first.")
  }
  ok <- tryCatch(google_auth(), error = function(e) FALSE)
  if (!isTRUE(ok)) stop("get_credentials(): google_auth() returned FALSE.")

  # Confirm folder exists / accessible
  tryCatch(
    googledrive::drive_get(googledrive::as_id(sheet_id)),
    error = function(e) stop("get_credentials(): cannot access folder: ", conditionMessage(e))
  )

  # Read credentials directly from Google Sheet by sheet_id
  cred <- tryCatch(
    googlesheets4::read_sheet(sheet_id, sheet = sheet),
    error = function(e) stop("get_credentials(): read_sheet failed: ", conditionMessage(e))
  )

  # Basic validation + normalization
  need <- c("user", "name", "pw_hash", "is_admin")
  missing <- setdiff(need, names(cred))
  if (length(missing)) {
    stop("get_credentials(): credentials sheet missing columns: ", paste(missing, collapse = ", "))
  }

  cred <- cred |>
    dplyr::mutate(
      user    = as.character(.data$user),
      name    = as.character(.data$name),
      pw_hash = as.character(.data$pw_hash),
      is_admin = as.logical(.data$is_admin)
    )

  # Drop empty users
  cred <- cred |> dplyr::filter(!is.na(.data$user) & nzchar(.data$user))

  cred
} 

CRED <- get_credentials()
logf("CRED: %s", paste(CRED$user, collapse = ", "))


stopifnot(all(c("user","name","pw_hash","is_admin") %in% names(CRED)))

# Attempt restore from Drive on startup (only if no local DB exists)
if (!file.exists(DB_PATH)) {
  logf("No local DB; restoring from Drive...")
  try(restore_db_from_drive(), silent = TRUE)
}

init_db()
gc()

logf('INIT DB COMPLETED')

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
  parts <- unlist(strsplit(as.character(x %||% ""), ",", fixed = TRUE))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  out <- suppressWarnings(as.numeric(parts))
  out <- out[is.finite(out) & out > 0]
  if (!length(out)) c(12,20,30,45,70) else out
}

# Returns TRUE when x looks like a formula (e.g. "11+c^2") rather than a
# comma-separated list of prices.  Heuristic: if any comma-separated token
# isn't a plain number, treat the whole thing as a formula.
is_cost_formula <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return(FALSE)
  parts <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  parts <- parts[nzchar(parts)]
  vals  <- suppressWarnings(as.numeric(parts))
  any(!is.finite(vals)) || grepl("[a-zA-Z^]", x)
}

# Safely evaluate a formula string with c = c_val (the question index).
# Only digits, basic operators, parentheses, and the letter 'c' are allowed.
eval_cost_formula <- function(expr_str, c_val) {
  expr_str <- trimws(as.character(expr_str %||% ""))
  if (grepl("[^0-9eE.+\\-*/^()c\\s]", expr_str, perl = TRUE)) {
    warning("Unsafe characters in cost formula; falling back to default cost 12")
    return(12)
  }
  c <- as.numeric(c_val)   # 'c' shadows base c() inside this scope only
  result <- tryCatch(
    eval(parse(text = expr_str)),
    error = function(e) {
      warning(sprintf("Cost formula evaluation failed ('%s'): %s", expr_str, e$message))
      12
    }
  )
  max(0, as.numeric(result[1]))
}

parse_names <- function(x) {
  parts <- unlist(strsplit(as.character(x %||% ""), ",", fixed = TRUE))
  trimws(parts[nzchar(trimws(parts))])
}

sanitize_colname <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

question_cost_for_round <- function(round, schedule_text) {
  r <- as.integer(round %||% 1L)
  if (is_cost_formula(schedule_text)) {
    eval_cost_formula(schedule_text, r)
  } else {
    sched <- parse_cost_schedule(schedule_text)
    if (r <= length(sched)) sched[r] else tail(sched, 1)
  }
}

# Roundless mode:
# - Question cost index is (unlocked_questions + 1)
# - Pledges are stored in a single fixed round bucket (round = 1)
is_roundless <- function(settings_row) {
  x <- suppressWarnings(as.integer(settings_row$roundless_mode[1]))
  isTRUE(is.finite(x) && x == 1L)
}

current_question_index <- function(state_row, settings_row) {
  if (is_roundless(settings_row)) {
    as.integer(state_row$unlocked_questions[1]) + 1L
  } else {
    as.integer(state_row$round[1])
  }
}

pledge_bucket_round <- function(state_row, settings_row) {
  if (is_roundless(settings_row)) 1L else as.integer(state_row$round[1])
}

spent_total <- function(uid) {
  if (is.null(uid) || !nzchar(uid)) return(0)
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

round_pledge <- function(uid, round) {
  if (is.null(uid) || !nzchar(uid)) return(0)
  x <- db_query("SELECT COALESCE(pledge,0) AS p FROM pledges WHERE user_id=? AND round=?;",
                list(uid, as.integer(round)))$p[1]
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) 0 else x
}

round_totals <- function(round) {
  db_query("SELECT COALESCE(SUM(pledge),0) AS pledged, COUNT(*) AS n FROM pledges WHERE round=?;",
           list(as.integer(round)))
}

compute_unlocks <- function(round, carryover, cost) {
  pledged <- as.numeric(round_totals(round)$pledged[1] %||% 0)
  eff <- pledged + as.numeric(carryover %||% 0)
  units <- floor(eff / cost)
  carry <- eff - units * cost
  list(pledged = pledged, effective = eff, units = as.integer(units), carry = as.numeric(carry))
}

# Count of flex extensions already declared for a specific pset
flex_count_for_pset <- function(uid, pset) {
  n <- db_query(
    "SELECT COALESCE(quantity, 0) AS n FROM resource_targets WHERE user_id=? AND resource_type='flex' AND target=?;",
    list(uid, pset))$n[1]
  if (!length(n) || is.na(n)) 0L else as.integer(n)
}

student_allocation_summary <- function(uid) {
  s <- get_settings()
  st <- get_state()

  init <- suppressWarnings(as.numeric(s$initial_fp[1]))
  if (!is.finite(init)) init <- 5

  pr <- pledge_bucket_round(st, s)
  pending_q <- if (isTRUE(as.integer(st$round_open[1]) == 1)) round_pledge(uid, pr) else 0

  agg <- db_query("
    SELECT purpose, COALESCE(SUM(amount),0) AS amt
    FROM ledger
    WHERE user_id=?
    GROUP BY purpose;
  ", list(uid))

  get_amt <- function(p) {
    a <- agg$amt[agg$purpose == p]
    if (!length(a)) 0 else as.numeric(a[1])
  }

  granted <- -get_amt("grant")
  spent_questions <- get_amt("question")
  spent_flex      <- get_amt("flex")
  spent_exam      <- get_amt("exam_point")

  spent_total_local <- spent_questions + spent_flex + spent_exam
  remaining_now <- init + granted - spent_total_local - pending_q
  remaining_now <- max(0, remaining_now)

  list(
    initial = init,
    granted = granted,
    pending_question = pending_q,
    spent_questions = spent_questions,
    spent_flex = spent_flex,
    spent_exam = spent_exam,
    remaining = remaining_now
  )
}

admin_student_summary <- function() {
  s <- get_settings()
  init <- suppressWarnings(as.numeric(s$initial_fp[1] %||% 0))
  if (!is.finite(init)) init <- 0

  students <- db_query("
    SELECT user_id, display_name
    FROM users
    ORDER BY display_name;
  ")
  if (!nrow(students)) return(tibble())

  led <- db_query("
    SELECT user_id, purpose, COALESCE(SUM(amount),0) AS amt, COUNT(*) AS n
    FROM ledger
    GROUP BY user_id, purpose;
  ")

  out <- students |>
    tibble::as_tibble() |>
    mutate(initial_fp = init)

  get_amt <- function(p) led |> filter(purpose == p) |> select(user_id, amt)
  get_n   <- function(p) led |> filter(purpose == p) |> select(user_id, n)

  grants_amt <- get_amt("grant") |> mutate(fp_granted = -as.numeric(amt)) |> select(user_id, fp_granted)
  q_amt      <- get_amt("question")    |> transmute(user_id, fp_spent_questions = as.numeric(amt))
  flex_amt   <- get_amt("flex")        |> transmute(user_id, fp_spent_flex = as.numeric(amt))
  exam_amt   <- get_amt("exam_point")  |> transmute(user_id, fp_spent_exam = as.numeric(amt))
  flex_n     <- get_n("flex") |> transmute(user_id, flex_passes = as.integer(n))

  exam_pts <- db_query("SELECT user_id, meta FROM ledger WHERE purpose='exam_point';")
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

  # Extensions used — from resource_targets
  rt_flex <- db_query("SELECT user_id, target FROM resource_targets WHERE resource_type='flex';")
  extensions_used <- if (nrow(rt_flex)) {
    rt_flex |> tibble::as_tibble() |>
      group_by(user_id) |>
      summarise(extensions_used = paste(sort(unique(target)), collapse=", "), .groups="drop")
  } else {
    tibble(user_id=character(), extensions_used=character())
  }

  # Bonus points per exam — from resource_targets
  rt_exam <- db_query("SELECT user_id, target, quantity FROM resource_targets WHERE resource_type='exam_point';")
  exam_names <- parse_names(s$exam_names[1])
  if (!length(exam_names)) exam_names <- c("Midterm 1", "Midterm 2", "Final")

  out <- out |>
    left_join(grants_amt, by = "user_id") |>
    left_join(q_amt,      by = "user_id") |>
    left_join(flex_amt,   by = "user_id") |>
    left_join(exam_amt,   by = "user_id") |>
    left_join(flex_n,     by = "user_id") |>
    left_join(exam_pts_sum, by = "user_id") |>
    left_join(extensions_used, by = "user_id") |>
    mutate(
      fp_granted         = coalesce(fp_granted, 0),
      fp_spent_questions = coalesce(fp_spent_questions, 0),
      fp_spent_flex      = coalesce(fp_spent_flex, 0),
      fp_spent_exam      = coalesce(fp_spent_exam, 0),
      flex_passes        = coalesce(flex_passes, 0L),
      exam_points        = coalesce(exam_points, 0L),
      fp_spent_total     = fp_spent_questions + fp_spent_flex + fp_spent_exam,
      fp_remaining       = initial_fp + fp_granted - fp_spent_total
    ) |>
    select(
      user_id, display_name,
      initial_fp, fp_granted,
      fp_spent_questions, fp_spent_flex, fp_spent_exam,
      flex_passes, exam_points, extensions_used,
      fp_spent_total, fp_remaining
    )

  for (en in exam_names) {
    col_nm <- paste0("bonus_pts_on_", sanitize_colname(en))
    if (nrow(rt_exam)) {
      vals <- rt_exam |> tibble::as_tibble() |>
        filter(target == en) |>
        group_by(user_id) |> summarise(!!col_nm := sum(quantity), .groups="drop")
      out <- left_join(out, vals, by="user_id")
    } else {
      out[[col_nm]] <- NA_integer_
    }
    out[[col_nm]] <- coalesce(out[[col_nm]], 0L)
  }
  out$extensions_used <- coalesce(out$extensions_used, "")

  # Extension declared per pset (0/1 per pset, mirrors bonus-pts-per-exam columns)
  pset_names <- parse_names(s$pset_names[1])
  if (!length(pset_names)) pset_names <- c("PS1", "PS2", "PS3", "PS4", "PS5")
  for (pn in pset_names) {
    col_nm <- paste0("extension_on_", sanitize_colname(pn))
    if (nrow(rt_flex)) {
      vals <- rt_flex |> tibble::as_tibble() |>
        filter(target == pn) |>
        transmute(user_id, !!col_nm := 1L)
      out <- left_join(out, vals, by="user_id")
    } else {
      out[[col_nm]] <- NA_integer_
    }
    out[[col_nm]] <- coalesce(out[[col_nm]], 0L)
  }
  out
}


# after you load CRED and after DB is initialized
u <- "kcoombs"

db_hash <- db_query("SELECT pw_hash FROM users WHERE user_id=?;", list(u))$pw_hash[1] %||% ""
cred_hash <- {
  r <- CRED[CRED$user == u, , drop = FALSE]   # adjust if you use user_id/display_name
  if (nrow(r) == 1) r$pw_hash[1] %||% "" else ""
}

logf("HASH COMPARE for", u,
     "| db_nchar=", nchar(db_hash),
     "| cred_nchar=", nchar(cred_hash),
     "| db_prefix=", substr(db_hash, 1, 12),
     "| cred_prefix=", substr(cred_hash, 1, 12),
     "| identical=", identical(db_hash, cred_hash))

# -------------------------
# UI
# -------------------------
login_ui <- function(msg = NULL) {
  fluidPage(
    titlePanel("Exam Questions + Flex Passes + Exam Points"),
    if (!is.null(msg)) div(style="color:#b00020; font-weight:bold;", msg),
    textInput("login_user", "Username"),
    passwordInput("login_pw", "Password"),
    actionButton("login_btn", "Sign in", class="btn-primary"),
    tags$small("Use the username and password provided by your instructor. Change password upon logging in.")
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

  rv <- reactiveValues(authed = FALSE, user = NULL, name = NULL, is_admin = FALSE,
                       impersonate = FALSE, impersonate_uid = NULL, impersonate_name = NULL)

  # -------------------------
  # Emergency: broken DB modal (shown instead of normal UI if init_db detected corruption)
  # -------------------------
  observe({
    req(DB_BROKEN)
    can_restore <- google_auth_ok() && nzchar(drive_folder_id())
    backup_choices <- if (can_restore) {
      tryCatch({
        files <- googledrive::drive_ls(googledrive::as_id(drive_folder_id()))
        files <- files[files$name != "appdata_latest_backup.zip", ]
        setNames(files$name, files$name)
      }, error = function(e) character(0))
    } else character(0)

    showModal(modalDialog(
      title = "Database Error — Restore Required",
      p(strong("The users table is corrupted"), " (PRIMARY KEY constraint is missing)."),
      p("This happens when the 'Amend SQL' button overwrites the users table directly.",
        "No data has been permanently deleted — a Drive backup will fully recover it."),
      if (can_restore && length(backup_choices)) {
        tagList(
          selectInput("modal_restore_file", "Select backup to restore",
            choices = backup_choices, selected = backup_choices[1]),
          actionButton("db_broken_restore_btn", "Restore from Drive", class = "btn-danger")
        )
      } else {
        p(em("Google Drive is not available. Manually replace the database file at:"),
          br(), code(DB_PATH))
      },
      footer = NULL,
      easyClose = FALSE
    ))
  })

  observeEvent(input$db_broken_restore_btn, {
    filename <- input$modal_restore_file %||% latest_zip_name()
    ok <- tryCatch(restore_db_from_drive(filename), error = function(e) {
      showNotification(paste("Restore failed:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (isTRUE(ok)) {
      removeModal()
      showNotification(
        "Database restored. Please reload the page to continue.",
        type = "message", duration = NULL
      )
    }
  })

  # -------------------------
  # Drive backup: on session end + daily timer (no per-action debounce)
  # -------------------------

  output$authed <- reactive(rv$authed)
  outputOptions(output, "authed", suspendWhenHidden = FALSE)
  output$auth_gate <- renderUI({ if (!rv$authed) login_ui() else NULL })

  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pw %||% ""

    row <- db_query(
      "SELECT user_id, display_name, is_admin, pw_hash FROM users WHERE user_id=?;",
      list(u)
    )

    if (nrow(row) != 1) {
      logf("LOGIN FAIL: user not found in DB:", u)
      showNotification("Login failed.", type="error")
      return()
    }

    ph <- row$pw_hash[1] %||% ""
    if (!nzchar(ph)) {
      logf("LOGIN FAIL: pw_hash missing/blank for:", u)
      showNotification("Login failed.", type="error")
      return()
    }

    ok <- FALSE
    ok <- tryCatch(bcrypt::checkpw(p, ph), error = function(e) {
      logf("LOGIN FAIL: bcrypt error for", u, ":", conditionMessage(e))
      FALSE
    })

    if (!isTRUE(ok)) {
      logf("LOGIN FAIL: bcrypt mismatch for:", u, "|", p, "|", ph)
      showNotification("Login failed.", type="error")
      return()
    }

    rv$authed   <- TRUE
    rv$user     <- row$user_id[1]
    rv$name     <- row$display_name[1]
    rv$is_admin <- isTRUE(as.integer(row$is_admin[1]) == 1)

    logf("LOGIN OK:", u, "| admin=", rv$is_admin)
  })


  authed   <- reactive(rv$authed)
  user_id  <- reactive({
    if (isTRUE(rv$impersonate) && !is.null(rv$impersonate_uid)) rv$impersonate_uid else rv$user
  })
  name     <- reactive({
    if (isTRUE(rv$impersonate) && !is.null(rv$impersonate_name)) rv$impersonate_name else rv$name
  })
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

  # Password changer panel (reused on Student + Admin)
  password_changer <- wellPanel(
    tags$details(
      tags$summary(tags$strong("Change password (click to show)")),
      div(style="margin-top:12px",
        passwordInput("pw_old",  "Current password"),
        passwordInput("pw_new",  "New password"),
        passwordInput("pw_new2", "Confirm new password"),
        actionButton("pw_change_btn", "Update password", class="btn-secondary")
      )
    )
  )

  observeEvent(input$pw_change_btn, {
    req(authed())
    if (isTRUE(rv$impersonate)) {
      showNotification("Stop impersonating before changing passwords.", type = "error"); return()
    }
    old <- input$pw_old %||% ""
    new <- input$pw_new %||% ""
    new2 <- input$pw_new2 %||% ""

    if (!nzchar(new) || nchar(new) < 8) {
      showNotification("New password must be at least 8 characters.", type="error"); return()
    }
    if (!identical(new, new2)) {
      showNotification("New passwords do not match.", type="error"); return()
    }

    row <- db_query("SELECT pw_hash FROM users WHERE user_id=?;", list(user_id()))
    if (nrow(row) != 1 || !bcrypt::checkpw(old, row$pw_hash[1])) {
      showNotification("Current password is incorrect.", type="error"); return()
    }

    new_hash <- bcrypt::hashpw(new)
    db_exec("UPDATE users SET pw_hash=? WHERE user_id=?;", list(new_hash, user_id()))
    set_state()
    showNotification("Password updated.", type="message")
  })

  # Daily backup: check hourly, fire if 24h elapsed
  .last_daily_backup <- reactiveVal(Sys.time())
  daily_tick <- reactiveTimer(3600000)  # 1 hour
  observeEvent(daily_tick(), {
    elapsed <- difftime(Sys.time(), .last_daily_backup(), units = "hours")
    if (elapsed >= 24) {
      if (db_changed_since_last_backup()) {
        logf("daily backup: 24h elapsed, backing up...")
        backup_db_async("daily backup")
      }
      .last_daily_backup(Sys.time())
    }
  })

  # ---------------- Student ----------------
  output$student_ui <- renderUI({
    req(authed())
    fluidPage(
      h4("Status"),
      uiOutput("whoami"),
      tags$hr(),
      uiOutput("student_status"),
      tags$hr(),
      uiOutput("purchase_box"),
      tags$hr(),
      uiOutput("use_resource_box"),
      tags$hr(),
      wellPanel(
        h5("Your allocations (live)"),
        uiOutput("student_alloc_table")
      ),
      tags$hr(),
      h4("Your ledger"),
      DTOutput("my_ledger"),
      password_changer,
      tags$hr()
    )
  })

  output$whoami <- renderUI({
    req(authed())
    base <- HTML(sprintf("<b>Logged in as:</b> %s (%s)", rv$user, rv$name))
    if (isTRUE(rv$impersonate)) {
      tagList(
        base,
        div(style = "background:#92400e; color:#fff; padding:6px 12px; border-radius:6px; margin-top:6px;",
          strong(sprintf("IMPERSONATING: %s (%s) — Student tab shows this student's view", rv$impersonate_name, rv$impersonate_uid)))
      )
    } else base
  })

  output$student_status <- renderUI({
    st <- state_poll()
    s  <- settings_poll()
    idx <- current_question_index(st, s)
    pr  <- pledge_bucket_round(st, s)
    cost <- question_cost_for_round(idx, s$question_cost_schedule[1])
    ws <- compute_unlocks(pr, st$carryover[1], cost)
    active_exam_id <- as.character(st$active_exam[1] %||% "exam1")
    questions <- get_exam_questions(active_exam_id)

    tagList(
      p(sprintf("Active exam: %s", active_exam_id)),
      p(sprintf("You start with %.2f flex passes (and can earn more). You may pledge them to help unlock access to potential exam questions for everyone, purchase bonus points on the final exam, or get an extension of %g hours on a problem set. Any exam questions purchased with flex passes will have a %g percent chance of appearing on the next exam and is worth %g (out of %g) points on the exam. You have %.2f remaining (excluding any open pledge).",
                as.numeric(s$initial_fp[1]),
                as.numeric(s$extension_hours[1] %||% 24),
                as.numeric(s$question_chance_pct[1] %||% 50),
                as.numeric(s$question_point_value[1] %||% 5),
                as.numeric(s$question_total_points[1] %||% 100),
                student_allocation_summary(user_id())$remaining)),
      p(sprintf("Pledging open: %s", ifelse(as.integer(st$round_open[1]) == 1, "YES", "NO"))),
      p(sprintf("Current question index: %d | Cost to unlock next question: %.2f (schedule: %s)",
                idx, cost, s$question_cost_schedule[1])),
      p(sprintf("Carryover into this round: %.2f", as.numeric(st$carryover[1]))),
      p(sprintf("Pledged currently: %.2f | Effective (pledged + carry): %.2f | Unlocks if closed now: %d",
                ws$pledged, ws$effective, ws$units)),
      p(sprintf("Unlocked so far: %d", as.integer(st$unlocked_questions[1]))),
      if (as.integer(st$unlocked_questions[1]) > 0)
        render_unlocked_questions(as.integer(st$unlocked_questions[1]), questions)
    )
  })

  output$student_alloc_table <- renderUI({
    req(authed())
    state_poll()  # refresh on heartbeat
    x <- student_allocation_summary(user_id())

    labels <- c("Starting", "Earned", "Spent: Questions", "Spent: Extensions",
                "Spent: Exam Pts", "Pending Pledge", "Remaining")
    vals <- c(
      x$initial %||% 0,
      x$granted %||% 0,
      x$spent_questions %||% 0,
      x$spent_flex %||% 0,
      x$spent_exam %||% 0,
      x$pending_question %||% 0,
      x$remaining %||% 0
    )

    cell_style <- "text-align:center; padding:4px 10px; white-space:nowrap;"
    header_cells <- lapply(labels, function(l)
      tags$th(style = paste0(cell_style, " font-size:0.82em; font-weight:600;"), l))
    value_cells  <- lapply(sprintf("%.2f", vals), function(v)
      tags$td(style = cell_style, v))

    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        class = "table table-bordered table-condensed",
        style = "width:auto; min-width:100%; font-size:0.9em; margin-bottom:0;",
        tags$thead(tags$tr(.list = header_cells)),
        tags$tbody(tags$tr(.list = value_cells))
      )
    )
  })

  # Purchase UI (single interface)
  output$purchase_box <- renderUI({
    req(authed())
    st <- state_poll()
    s  <- settings_poll()

    # Available for immediate spending, net of pending pledge
    alloc <- student_allocation_summary(user_id())
    available_now <- as.numeric(alloc$remaining %||% 0)
    pending_q     <- as.numeric(alloc$pending_question %||% 0)

    # IMPORTANT: allow students to edit their question pledge up to (pending + available_now)
    # For other purchase types, cap at available_now.
    choice <- input$buy_type %||% "question"
    max_for_slider <- if (identical(choice, "question")) (pending_q + available_now) else available_now

    step <- suppressWarnings(as.numeric(s$pledge_step[1]))
    if (!is.finite(step) || step <= 0) step <- 0.5

    # Clamp to sane range for slider (Shiny dislikes negative/NA)
    max_for_slider <- max(0, as.numeric(max_for_slider %||% 0))

    wellPanel(
      h4("Spend passes"),
      p(sprintf("Currently pledged to questions: %.2f | Available for immediate purchases: %.2f",
                pending_q, available_now)),
      selectInput(
        "buy_type", "What are you buying?",
        choices = c(
          "Contribute to unlocking exam questions" = "question",
          "Buy exam point(s)" = "exam_point",
          sprintf("Buy a %g-hour extension", as.numeric(s$extension_hours[1] %||% 24)) = "flex"
        ),
        selected = choice
      ),
      sliderInput(
        "buy_amt", "How many passes?",
        min = 0,
        max = max_for_slider,
        value = if (identical(choice, "question")) min(pending_q, max_for_slider) else 0,
        step = step
      ),
      # if choice is "question", state amount currently pledged
      if (identical(choice, "question")) tags$p(strong(sprintf("Currently pledged: %.2f", alloc$pending_question %||% 0)))
      ,
      checkboxInput("buy_confirm", "I confirm this purchase/allocation.", value = FALSE),
      actionButton("buy_submit", "Submit", class="btn-primary"),
      tags$small("Questions = allocation (editable while open). Exam points/extensions = immediate purchases.")
    )
  })

  # --- Use Resource box ---
  output$use_resource_box <- renderUI({
    req(authed())
    s <- settings_poll(); state_poll()

    s_exam_names <- parse_names(s$exam_names[1])
    s_pset_names <- parse_names(s$pset_names[1])
    if (!length(s_exam_names)) s_exam_names <- c("Midterm 1", "Midterm 2", "Final")
    if (!length(s_pset_names)) s_pset_names <- c("PS1", "PS2", "PS3", "PS4", "PS5")

    alloc <- student_allocation_summary(user_id())
    flex_purchased <- as.integer(alloc$spent_flex %||% 0)
    exam_purchased <- as.integer(alloc$spent_exam %||% 0)

    rt <- db_query(
      "SELECT resource_type, target, quantity FROM resource_targets WHERE user_id=?;",
      list(user_id()))
    flex_used <- sum(rt$quantity[rt$resource_type == "flex"])
    exam_used  <- sum(rt$quantity[rt$resource_type == "exam_point"])

    wellPanel(
      h4("Use your resources"),
      tags$hr(),
      h5(sprintf("%g-hour extensions", as.numeric(s$extension_hours[1] %||% 24))),
      p(sprintf("Purchased: %d | Declared: %d", flex_purchased, flex_used)),
      if (nrow(rt[rt$resource_type == "flex", , drop=FALSE]))
        tableOutput("my_extensions_table")
      else
        p(em("None declared yet."))
      ,
      if (flex_used < flex_purchased) tagList(
        selectInput("use_flex_pset", "Apply extension to:", choices = s_pset_names),
        actionButton("use_flex_submit", "Add extension", class="btn-primary")
      ) else if (flex_purchased > 0) p(em("All purchased extensions have been declared."))
      else p(em("No extensions purchased yet."))
      ,
      tags$hr(),
      h5("Exam bonus points"),
      p(sprintf("Purchased (passes): %d | Declared (passes): %d", exam_purchased, exam_used)),
      if (nrow(rt[rt$resource_type == "exam_point", , drop=FALSE]))
        tableOutput("my_exam_pts_table")
      else
        p(em("None declared yet."))
      ,
      if (exam_used < exam_purchased) tagList(
        selectInput("use_exam_name", "Apply bonus point(s) to:", choices = s_exam_names),
        numericInput("use_exam_qty", "Points to declare:", value=1, min=1,
                     max=exam_purchased - exam_used),
        actionButton("use_exam_submit", "Apply", class="btn-primary")
      ) else if (exam_purchased > 0) p(em("All purchased exam points have been declared."))
      else p(em("No exam points purchased yet."))
    )
  })

  output$my_extensions_table <- renderTable({
    req(authed()); state_poll()
    db_query("SELECT target AS 'Problem set', quantity AS 'Extensions', updated_at AS 'Last declared'
              FROM resource_targets WHERE user_id=? AND resource_type='flex'
              ORDER BY updated_at;", list(user_id()))
  }, rownames=FALSE)

  output$my_exam_pts_table <- renderTable({
    req(authed()); state_poll()
    db_query("SELECT target AS 'Exam', quantity AS 'Points', created_at AS 'Declared at'
              FROM resource_targets WHERE user_id=? AND resource_type='exam_point'
              ORDER BY created_at;", list(user_id()))
  }, rownames=FALSE)

  observeEvent(input$use_flex_submit, {
    req(authed())
    s <- get_settings()
    alloc <- student_allocation_summary(user_id())
    flex_purchased <- as.integer(alloc$spent_flex %||% 0)
    flex_used <- db_query(
      "SELECT COALESCE(SUM(quantity),0) AS n FROM resource_targets WHERE user_id=? AND resource_type='flex';",
      list(user_id()))$n[1]
    if (flex_used >= flex_purchased) {
      showNotification("No remaining extensions to declare.", type="error"); return()
    }
    pset <- as.character(input$use_flex_pset %||% "")
    if (!nzchar(pset)) { showNotification("Select a problem set.", type="error"); return() }

    # Enforce per-pset cap
    max_pp <- suppressWarnings(as.numeric(s$max_per_pset[1]))
    if (!is.finite(max_pp) || max_pp <= 0) max_pp <- Inf
    pset_count <- as.integer(flex_count_for_pset(user_id(), pset) %||% 0L)
    if (pset_count >= max_pp) {
      showNotification(sprintf("You have already used the maximum (%d) extensions for %s.", as.integer(max_pp), pset), type="error")
      return()
    }

    db_exec("
      INSERT INTO resource_targets(user_id, resource_type, target, quantity, updated_at)
      VALUES(?, 'flex', ?, 1, CURRENT_TIMESTAMP)
      ON CONFLICT(user_id, resource_type, target) DO UPDATE SET
        quantity   = resource_targets.quantity + 1,
        updated_at = CURRENT_TIMESTAMP;
    ", list(user_id(), pset))
    set_state()
    showNotification(sprintf("Extension declared for %s.", pset), type="message")
  })

  observeEvent(input$use_exam_submit, {
    req(authed())
    alloc <- student_allocation_summary(user_id())
    exam_purchased <- as.integer(alloc$spent_exam %||% 0)
    exam_used <- db_query(
      "SELECT COALESCE(SUM(quantity),0) AS n FROM resource_targets WHERE user_id=? AND resource_type='exam_point';",
      list(user_id()))$n[1]
    qty <- max(1L, as.integer(input$use_exam_qty %||% 1))
    if (exam_used + qty > exam_purchased) {
      showNotification("Not enough exam points available to declare.", type="error"); return()
    }
    exam <- as.character(input$use_exam_name %||% "")
    if (!nzchar(exam)) { showNotification("Select an exam.", type="error"); return() }
    db_exec("
      INSERT INTO resource_targets(user_id, resource_type, target, quantity, updated_at)
      VALUES(?, 'exam_point', ?, ?, CURRENT_TIMESTAMP)
      ON CONFLICT(user_id, resource_type, target) DO UPDATE SET
        quantity   = resource_targets.quantity + excluded.quantity,
        updated_at = CURRENT_TIMESTAMP;
    ", list(user_id(), exam, qty))
    set_state()
    showNotification(sprintf("Declared %d bonus point(s) on %s.", qty, exam), type="message")
  })

  observeEvent(input$buy_submit, {
    req(authed())
    if (!isTRUE(input$buy_confirm)) {
      showNotification("Please confirm before submitting.", type="error"); return()
    }

    s  <- settings_poll()
    st <- state_poll()

    amt <- suppressWarnings(as.numeric(input$buy_amt %||% 0))
    if (!is.finite(amt) || amt <= 0) {
      showNotification("Choose an amount > 0.", type="warning"); return()
    }

    typ <- input$buy_type %||% ""

    # Use allocation summary for caps (net of pending pledge)
    alloc <- student_allocation_summary(user_id())
    available_now <- as.numeric(alloc$remaining %||% 0)
    pending_q     <- as.numeric(alloc$pending_question %||% 0)

    if (typ == "question") {
      if (!isTRUE(as.integer(st$round_open[1]) == 1)) {
        showNotification("Question pledging is currently closed.", type="error"); return()
      }
      # cap question pledge at pending + available_now
      amt <- min(amt, pending_q + available_now)

      # In roundless mode, pledges live in a single fixed bucket (round=1)
      pr <- pledge_bucket_round(st, s)

      db_exec("
        INSERT INTO pledges(user_id, round, pledge, submitted_at)
        VALUES(?, ?, ?, CURRENT_TIMESTAMP)
        ON CONFLICT(user_id, round) DO UPDATE SET
          pledge = excluded.pledge,
          submitted_at = CURRENT_TIMESTAMP;
      ", list(user_id(), as.integer(pr), as.numeric(amt)))

      set_state()
      showNotification("Your question pledge/allocation was updated.", type="message")
      return()
    }

    if (typ == "flex") {
      # Interpret amt as count; require exactly 1 per purchase
      if (abs(amt - 1) > 1e-9) {
        showNotification("Extension must be purchased as exactly 1.", type="error"); return()
      }

      cost <- suppressWarnings(as.numeric(s$flex_cost[1]))
      if (!is.finite(cost) || cost <= 0) cost <- 1

      if (available_now < cost - 1e-9) {
        showNotification("Not enough passes remaining (net of your pledge).", type="error"); return()
      }

      db_exec("
        INSERT INTO ledger(user_id, round, purpose, amount, meta)
        VALUES(?, NULL, 'flex', ?, ?);
      ", list(user_id(), cost, 'extension'))

      set_state()
      showNotification(sprintf("%g-hour extension purchased.", suppressWarnings(as.numeric(s$extension_hours[1] %||% 24))), type="message")
      return()
    }

    if (typ == "exam_point") {
      # Exam points are integer quantities. Interpret amt as #points.
      n <- as.numeric(round(amt))
      if (n <= 0) {
        showNotification("Exam points must be a positive number.", type="error"); return()
      }

      unit_cost <- suppressWarnings(as.numeric(s$exam_point_cost[1]))
      if (!is.finite(unit_cost) || unit_cost <= 0) unit_cost <- 1

      cost <- n * unit_cost
      if (available_now < cost - 1e-9) {
        showNotification("Not enough passes remaining (net of your pledge).", type="error"); return()
      }

      db_exec("
        INSERT INTO ledger(user_id, round, purpose, amount, meta)
        VALUES(?, NULL, 'exam_point', ?, ?);
      ", list(user_id(), cost, sprintf("exam_points=%d", n)))

      set_state()
      showNotification(sprintf("Purchased %d exam point(s).", n), type="message")
      return()
    }

    showNotification("Unknown purchase type.", type="error")
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
    s  <- get_settings()
    pr <- pledge_bucket_round(st, s)
    pending_label <- if (is_roundless(s)) "pledge (current open question)" else "pledge (current round)"
    pending <- tibble(
      created_at = NA_character_,
      purpose = pending_label,
      round = pr,
      amount = round_pledge(user_id(), pr),
      meta = if (as.integer(st$round_open[1]) == 1) "pending (not charged yet)" else "round closed"
    )

    out <- bind_rows(pending, df)
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 10))
  })

  # ---------------- Projector ----------------
  output$projector_ui <- renderUI({
    req(authed())
    st <- state_poll()
    s  <- settings_poll()

    if (as.integer(st$round_open[1]) == 1 && !is_admin()) {
      return(wellPanel(
        h4("Projector hidden while round is open"),
        p(sprintf("Round %d is currently open.", st$round[1]))
      ))
    }

    idx <- current_question_index(st, s)
    pr  <- pledge_bucket_round(st, s)
    cost <- question_cost_for_round(idx, s$question_cost_schedule[1])
    ws <- compute_unlocks(pr, st$carryover[1], cost)
    active_exam_id <- as.character(st$active_exam[1] %||% "exam1")
    questions <- get_exam_questions(active_exam_id)

    fluidPage(
      h3("Class Progress"),
      p(sprintf("Active exam: %s", active_exam_id)),
      p(sprintf("Question index: %d | Unlocked: %d | Carryover: %.2f",
                idx, st$unlocked_questions[1], st$carryover[1])),
      p(sprintf("Pledged currently: %.2f | Effective: %.2f | Cost now: %.2f | Unlocks if closed now: %d",
                ws$pledged, ws$effective, cost, ws$units)),
      tags$hr(),
      if (as.integer(st$unlocked_questions[1]) > 0)
        render_unlocked_questions(as.integer(st$unlocked_questions[1]), questions)
    )
  })

  # ---------------- Admin ----------------
  output$admin_ui <- renderUI({
    req(authed())
    if (!is_admin()) return(fluidPage(h4("Admin"), p("You are not an admin.")))

    st <- state_poll()
    s  <- settings_poll()
    idx <- current_question_index(st, s)
    pr  <- pledge_bucket_round(st, s)
    cost <- question_cost_for_round(idx, s$question_cost_schedule[1])

    fluidPage(
      h4("Admin Controls"),
      tags$hr(),

      if (isTRUE(rv$impersonate)) div(
        style = "background:#92400e; color:#fff; padding:8px 16px; border-radius:6px; margin-bottom:12px;",
        strong(sprintf("IMPERSONATING: %s (%s)", rv$impersonate_name, rv$impersonate_uid)),
        " — Student tab actions are attributed to this student."
      ),

      wellPanel(
        h5("Impersonate student (act as any student)"),
        p("As admin, simulate actions as any student. The Student tab shows that student's view and all actions are attributed to them."),
        fluidRow(
          column(6, selectInput("impersonate_select", "Student to impersonate",
            choices = {
              us <- db_query("SELECT user_id, display_name FROM users WHERE COALESCE(is_admin,0)=0 ORDER BY display_name;")
              setNames(us$user_id, us$display_name)
            }
          )),
          column(3, br(), actionButton("impersonate_start", "Start impersonating", class = "btn-warning")),
          column(3, br(), actionButton("impersonate_stop",  "Stop impersonating",  class = "btn-secondary"))
        )
      ),

      wellPanel(
        h5("Settings"),
        fluidRow(
          column(4, numericInput("cfg_initial", "Initial flex passes per student", value = as.numeric(s$initial_fp[1]), min = 0)),
          column(4, numericInput("cfg_step", "Pledge step", value = as.numeric(s$pledge_step[1]), min = 0.1)),
          column(4, selectInput("cfg_shortfall", "Shortfall policy",
                                choices = c("bank_all","nocharge"),
                                selected = as.character(s$shortfall_policy[1])))
        ),
        fluidRow(
          column(4, numericInput("cfg_flex_cost", "Extension cost (passes each)", value = as.numeric(s$flex_cost[1]), min = 0)),
          column(4, numericInput("cfg_exam_cost", "Exam point cost (flex passes)", value = as.numeric(s$exam_point_cost[1]), min = 0)),
          column(4, textInput("cfg_sched",
                              "Question cost schedule — comma list (12,20,30) or formula in c (e.g. 11+c^2)",
                              value = as.character(s$question_cost_schedule[1]),
                              placeholder = "e.g. 12,20,30,45,70  or  11+c^2"))
        ),
        fluidRow(
          column(3, numericInput("cfg_extension_hours", "Extension duration (hours)", value = as.numeric(s$extension_hours[1] %||% 24), min = 1)),
          column(3, numericInput("cfg_question_chance", "Question appearance chance (%)", value = as.numeric(s$question_chance_pct[1] %||% 50), min = 0, max = 100)),
          column(3, numericInput("cfg_question_pts", "Question worth (points)", value = as.numeric(s$question_point_value[1] %||% 5), min = 0)),
          column(3, numericInput("cfg_question_total", "Exam total points", value = as.numeric(s$question_total_points[1] %||% 100), min = 1))
        ),
        fluidRow(
          column(4,
            checkboxInput("cfg_roundless", "Roundless mode (index costs to unlocked question #)",
                          value = is_roundless(s))
          ),
          column(8,
            tags$small("If enabled: pledges live in one bucket, you can open/close pledging as needed, and the cost schedule steps with each unlocked question.")
          )
        ),
        fluidRow(
          column(4, numericInput("cfg_max_per_pset", "Max extensions per pset", value = as.numeric(s$max_per_pset[1]), min = 0)),
          column(4, numericInput("cfg_max_fp_held", "Max flex passes held at once", value = as.numeric(s$max_fp_held[1] %||% 8), min = 1)),
          column(4, textInput("cfg_exam_names", "Exam names (comma-separated)",
                              value = as.character(s$exam_names[1] %||% "Midterm 1,Midterm 2,Final"))),
          column(4, textInput("cfg_pset_names", "Problem set names (comma-separated)",
                              value = as.character(s$pset_names[1] %||% "PS1,PS2,PS3,PS4,PS5")))
        ),
        actionButton("apply_settings", "Apply settings", class="btn-secondary")
      ),

      wellPanel(
        h5("Exam management"),
        {
          active_exam_id <- as.character(st$active_exam[1] %||% "exam1")
          db_exams <- list_exams_with_questions()
          all_exam_choices <- unique(c(db_exams, active_exam_id))
          all_exam_choices <- all_exam_choices[nzchar(all_exam_choices)]
          if (!length(all_exam_choices)) all_exam_choices <- "exam1"

          tagList(
            p(sprintf("Active exam: %s", active_exam_id)),
            div(style = "background:#fff3cd; border:1px solid #ffc107; padding:8px; margin-bottom:8px; border-radius:4px; font-size:0.9em;",
              tags$strong("Note:"), " Switching exam only changes the active question bank. Round, unlocked questions, carryover, and pledge status are not affected."
            ),
            fluidRow(
              column(4,
                selectInput("adm_active_exam", "Switch to exam",
                            choices = all_exam_choices, selected = active_exam_id)
              ),
              column(4, br(),
                actionButton("adm_switch_exam", "Switch exam", class = "btn-warning")
              )
            ),
            tags$hr(),
            h6("Upload questions for an exam"),
            p(tags$small("CSV must have a ", tags$code("question"), " column (HTML allowed). Optional ",
              tags$code("question_num"), " column; if absent, rows are numbered in order.")),
            fluidRow(
              column(3, textInput("upload_exam_id", "Exam ID", placeholder = "e.g. exam1")),
              column(5, fileInput("upload_questions_csv", "Questions CSV", accept = ".csv"))
            ),
            actionButton("do_upload_questions", "Upload questions", class = "btn-secondary"),
            tags$br(), tags$br(),
            tags$details(
              tags$summary(style = "cursor:pointer; font-weight:600;",
                           sprintf("Current questions for active exam (%s)", active_exam_id)),
              tags$br(),
              tableOutput("admin_exam_questions_table"),
              downloadButton("dl_exam_questions", "Download current exam questions (CSV)")
            )
          )
        }
      ),

      wellPanel(
        h5("Round controls"),
        p(sprintf("Active exam: %s | Pledging open: %s | Question index: %d | Cost now: %.2f",
                  as.character(st$active_exam[1] %||% "exam1"),
                  ifelse(as.integer(st$round_open[1]) == 1, "YES", "NO"), idx, cost)),
        fluidRow(
          column(3, actionButton("open_round", "Open round", class="btn-success")),
          column(3, actionButton("close_round", "Close & settle", class="btn-danger")),
          if (!is_roundless(s)) column(3, actionButton("next_round", "Next round", class="btn-primary"))
        )
      ),

      password_changer,

      wellPanel(
        h5("Admin: reset a user's password"),
        fluidRow(
          column(6,
            selectInput("reset_pw_user", "User", choices = {
              us <- db_query("SELECT user_id, display_name, is_admin FROM users ORDER BY is_admin DESC, display_name;")
              setNames(us$user_id, paste0(us$display_name, ifelse(us$is_admin == 1, " (admin)", "")))
            })
          ),
          column(3, passwordInput("reset_pw_new", "New password")),
          column(3, passwordInput("reset_pw_new2", "Confirm"))
        ),
        actionButton("reset_pw_btn", "Reset password", class = "btn-warning"),
        tags$small("Resets the selected user's password (bcrypt-hashed in the app database).")
      ),

      wellPanel(
        h5("Backup to Google Sheets"),
        p(if (nzchar(gs_backup_sheet_id())) sprintf("BACKUP_SHEET_ID is set. Sheet: %s", gs_backup_sheet_id()) else "BACKUP_SHEET_ID is not set. Set it to enable backups."),
        actionButton("backup_sheets_btn", "Backup now", class = "btn-secondary"),
        tags$small("Uses a service account via GOOGLE_APPLICATION_CREDENTIALS or GOOGLE_SERVICE_ACCOUNT_JSON.")
      ),

      # -------------- restore backup SQL database from Google Drive --------------
      wellPanel(
        h5("Restore backup SQL database from Google Drive. Pick a backup file from the list below."),
        selectInput("restore_db_from_drive_file", "Backup file", choices = {
          files <- googledrive::drive_ls(googledrive::as_id(drive_folder_id()))
          files <- files[files$name != "appdata_latest_backup.zip",]
          setNames(files$name, files$name)
        }, selected = "appdata_latest_backup.zip"),
        actionButton("restore_db_from_drive_btn", "Restore", class = "btn-secondary"),
        tags$small("Restores the backup SQL database from Google Drive.")
      ),

      # -------------- add way to amend SQL database with sheet from Google Drive. select tab from Google Drive from drop down and table in SQL database from drop down --------------
      wellPanel(
        h5("Amend SQL database with sheet from Google Drive"),
        fluidRow(
          column(6, selectInput("amend_sql_sheet", "Google Drive sheet", choices = googlesheets4::sheet_names(gs_backup_sheet_id())), selected = 'users'),
          column(6, selectInput("amend_sql_table", "SQL table", choices = DBI::dbListTables(get_con())), selected = 'users'),
          column(6, actionButton("amend_sql_btn", "Amend (overwrite!)", class = "btn-secondary", 
            onclick = "if(!confirm('WARNING: This will OVERWRITE the selected SQL table with the selected Google Sheet data. Are you sure you want to continue?')){event.stopPropagation();}")
          )
        ),
        tags$small("Amends the SQL database with the selected sheet from Google Drive. Never overwrite credentials column in SQL table users.")
      ),


      wellPanel(
        h5("Grant flex passes (students can gain more)"),
        fluidRow(
          column(6,
            selectInput("grant_user", "Student",
              choices = {
                us <- db_query("SELECT user_id, display_name FROM users WHERE COALESCE(is_admin,0)=0 ORDER BY display_name;")
                setNames(us$user_id, us$display_name)
              }
            )
          ),
          column(3, numericInput("grant_amt", "Flex passes to grant", value = 1, min = 0)),
          column(3, br(), actionButton("do_grant", "Grant", class="btn-success"))
        ),
        tags$small("Grants are recorded as negative ledger amounts (credits).")
      ),

      wellPanel(
        h5("Bulk flex pass grant / deduct"),
        p("Select students and apply a uniform amount to each. Positive = grant more passes; negative = deduct."),
        selectInput("bulk_grant_users", "Students",
          choices = {
            us <- db_query("SELECT user_id, display_name FROM users WHERE COALESCE(is_admin,0)=0 ORDER BY display_name;")
            setNames(us$user_id, us$display_name)
          },
          multiple = TRUE
        ),
        fluidRow(
          column(4, numericInput("bulk_grant_amt", "Amount per student (negative = deduct)",
                                 value = 1, min = -100, max = 100, step = 0.5)),
          column(4, br(), actionButton("do_bulk_grant", "Apply to selected", class = "btn-success"))
        ),
        tags$small("Each selected student receives this adjustment. Recorded in ledger with purpose 'grant'.")
      ),

      wellPanel(
        h5("Remove student"),
        fluidRow(
          column(8,
            selectInput("remove_user", "Student",
              choices = {
                us <- db_query("SELECT user_id, display_name FROM users WHERE COALESCE(is_admin,0)=0 ORDER BY display_name;")
                setNames(us$user_id, us$display_name)
              }
            )
          ),
          column(4, br(),
            actionButton("do_remove_user", "Remove", class = "btn-danger",
              onclick = "if(!confirm('Remove this student? This cannot be undone (unless you restore from backup).')) event.stopPropagation();")
          )
        ),
        tags$small("Removes the student from the roster. Their ledger history is kept for auditing but they will no longer be able to log in.")
      ),

      wellPanel(
        h5("Add student from credentials sheet"),
        p("Add the student to the ", tags$code("credentials"), " tab in the Google Sheet (with columns: user, name, pw_hash, is_admin), then click below."),
        actionButton("sync_cred_btn", "Sync from credentials sheet", class = "btn-secondary"),
        tags$small("Adds new users and updates names/admin flags. Never overwrites existing passwords.")
      ),

      tags$details(
        class = "well",
        style = "padding:15px; margin-bottom:10px;",
        tags$summary(style = "cursor:pointer; font-weight:bold; font-size:1.05em;",
                     "Admin export table: balances & purchases"),
        tags$br(),
        DTOutput("admin_export_table"),
        tags$br(),
        downloadButton("dl_admin_export", "Download balances & purchases (CSV)")
      ),

      wellPanel(
        h5("Exports"),
        downloadButton("dl_pledges", "Download pledges (CSV)"),
        downloadButton("dl_ledger", "Download ledger (CSV)")
      ),

      tags$details(
        class = "well",
        style = "padding:15px; margin-bottom:10px;",
        tags$summary(style = "cursor:pointer; font-weight:bold; font-size:1.05em;",
                     "Live pledges this round"),
        tags$br(),
        DTOutput("admin_pledges_table")
      ),

      tags$details(
        class = "well",
        style = "padding:15px; margin-bottom:10px;",
        tags$summary(style = "cursor:pointer; font-weight:bold; font-size:1.05em;",
                     "Quick totals (this round)"),
        tags$br(),
        tableOutput("admin_round_totals")
      )
    )
  })

  observeEvent(input$apply_settings, {
    req(is_admin())
    old <- get_settings()
    sched <- input$cfg_sched %||% ""
    sched_ok <- if (is_cost_formula(sched)) {
      val <- tryCatch(eval_cost_formula(sched, 1L), error = function(e) NA_real_)
      is.finite(val) && val > 0
    } else {
      length(parse_cost_schedule(sched)) > 0
    }
    if (!sched_ok) {
      showNotification("Bad schedule. Use a comma list (e.g. 12,20,30,45,70) or a formula in c (e.g. 11+c^2)", type="error")
      return()
    }
    set_settings(
      initial_fp = as.numeric(input$cfg_initial),
      pledge_step = as.numeric(input$cfg_step),
      flex_cost = as.numeric(input$cfg_flex_cost),
      exam_point_cost = as.numeric(input$cfg_exam_cost),
      question_cost_schedule = as.character(sched),
      shortfall_policy = as.character(input$cfg_shortfall),
      roundless_mode = as.integer(isTRUE(input$cfg_roundless)),
      exam_names = as.character(input$cfg_exam_names %||% ""),
      pset_names = as.character(input$cfg_pset_names %||% ""),
      max_per_pset = as.numeric(input$cfg_max_per_pset),
      max_fp_held  = as.numeric(input$cfg_max_fp_held),
      extension_hours = as.numeric(input$cfg_extension_hours %||% 24),
      question_chance_pct = as.numeric(input$cfg_question_chance %||% 50),
      question_point_value = as.numeric(input$cfg_question_pts %||% 5),
      question_total_points = as.numeric(input$cfg_question_total %||% 100)
    )

    # If enabling roundless, open round and normalize state + clear stray pledge rounds
    if (!is_roundless(old) && isTRUE(input$cfg_roundless)) {
      try(db_exec("DELETE FROM pledges WHERE round <> 1;"), silent = TRUE)
      try(set_state(round = 1, round_open = 1), silent = TRUE)
    }

    showNotification("Settings updated.", type="message")
  })

  observeEvent(input$backup_sheets_btn, {
    req(is_admin())
    ok <- tryCatch(backup_to_sheets(), error = function(e) {
      showNotification(paste("Backup failed:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (isTRUE(ok)) showNotification("Backup complete.", type = "message")
  })

  observeEvent(input$restore_db_from_drive_btn, {
    req(is_admin())
    ok <- tryCatch(restore_db_from_drive(input$restore_db_from_drive_file), error = function(e) {
      showNotification(paste("Restore failed:", conditionMessage(e)), type = "error")
      FALSE
    })
  })
  observeEvent(input$reset_pw_btn, {
    req(is_admin(), input$reset_pw_user)
    pw1 <- input$reset_pw_new %||% ""
    pw2 <- input$reset_pw_new2 %||% ""
    if (!nzchar(pw1) || nchar(pw1) < 8) {
      showNotification("New password must be at least 8 characters.", type = "error"); return()
    }
    if (!identical(pw1, pw2)) {
      showNotification("Passwords do not match.", type = "error"); return()
    }
    h <- bcrypt::hashpw(pw1)
    db_exec("UPDATE users SET pw_hash=? WHERE user_id=?;", list(h, as.character(input$reset_pw_user)))
    set_state()
    showNotification("Password reset.", type = "message")
  })

  observeEvent(input$do_grant, {
    req(is_admin(), input$grant_user)
    amt <- suppressWarnings(as.numeric(input$grant_amt %||% 0))
    if (!is.finite(amt) || amt <= 0) {
      showNotification("Grant must be > 0.", type="error")
      return()
    }
    uid <- as.character(input$grant_user)
    cap <- as.numeric(get_settings()$max_fp_held[1] %||% 8)
    current_held <- student_allocation_summary(uid)$remaining
    if (current_held >= cap) {
      showNotification(sprintf("Student already holds the maximum %.0f flex passes. Cannot grant more.", cap), type="warning")
      return()
    }
    amt <- min(amt, cap - current_held)
    db_exec("
      INSERT INTO ledger(user_id, round, purpose, amount, meta)
      VALUES(?, NULL, 'grant', ?, 'admin_grant');
    ", list(uid, -amt))
    set_state()
    showNotification(sprintf("Granted %.2f flex pass(es).", amt), type="message")
  })

  observeEvent(input$do_remove_user, {
    req(is_admin(), input$remove_user)
    uid <- as.character(input$remove_user)
    # Guard: never remove an admin
    row <- db_query("SELECT is_admin FROM users WHERE user_id=?;", list(uid))
    if (!nrow(row) || isTRUE(as.logical(row$is_admin[1]))) {
      showNotification("Cannot remove an admin account.", type = "error"); return()
    }
    db_exec("DELETE FROM users              WHERE user_id=?;", list(uid))
    db_exec("DELETE FROM pledges            WHERE user_id=?;", list(uid))
    db_exec("DELETE FROM resource_targets   WHERE user_id=?;", list(uid))
    set_state()
    showNotification(sprintf("Removed %s.", uid), type = "message")
  })

  observeEvent(input$sync_cred_btn, {
    req(is_admin())
    before <- db_query("SELECT COUNT(*) n FROM users;")$n[1]
    new_cred <- tryCatch(get_credentials(), error = function(e) {
      showNotification(paste("Could not read credentials sheet:", conditionMessage(e)), type = "error")
      NULL
    })
    if (is.null(new_cred)) return()
    CRED <<- new_cred
    upsert_cred_to_db(new_cred)
    after <- db_query("SELECT COUNT(*) n FROM users;")$n[1]
    added <- max(0L, as.integer(after) - as.integer(before))
    set_state()
    showNotification(
      sprintf("Synced %d users from credentials sheet (%d new).", nrow(new_cred), added),
      type = "message"
    )
  })

  observeEvent(input$open_round, {
    req(is_admin())
    set_state(round_open = 1)
    showNotification("Round opened.", type="message")
  })

  observeEvent(input$next_round, {
    req(is_admin())
    st <- get_state(); s <- get_settings()
    if (is_roundless(s)) {
      showNotification("Roundless mode is enabled: no Next round. Use open/close only.", type = "warning")
      return()
    }
    new_round <- as.integer(st$round[1]) + 1L
    set_state(round = new_round, round_open = 0)
    showNotification(sprintf("Moved to round %d.", new_round), type="message")
  })

  observeEvent(input$close_round, {
    req(is_admin())
    st <- get_state()
    s  <- get_settings()

    if (as.integer(st$round_open[1]) != 1) {
      showNotification("Round already closed.", type="warning")
      return()
    }

    idx <- current_question_index(st, s)
    pr  <- pledge_bucket_round(st, s)
    cost <- question_cost_for_round(idx, s$question_cost_schedule[1])
    ws <- compute_unlocks(pr, st$carryover[1], cost)
    funded <- ws$units > 0

    if (funded || identical(as.character(s$shortfall_policy[1]), "bank_all")) {
      db_exec("
        INSERT INTO ledger(user_id, round, purpose, amount, meta)
        SELECT user_id, round, 'question', COALESCE(pledge,0), 'settlement'
        FROM pledges
        WHERE round=?;
      ", list(as.integer(pr)))
      new_carry <- ws$carry
    } else {
      new_carry <- as.numeric(st$carryover[1])
    }

    new_unlocked <- as.integer(st$unlocked_questions[1]) + as.integer(ws$units)

    set_state(
      round_open = 0,
      carryover = as.numeric(new_carry),
      unlocked_questions = new_unlocked
    )

    # In roundless mode, clear the pledge bucket after settlement so it doesn't auto-recharge next close
    if (is_roundless(s)) {
      try(db_exec("DELETE FROM pledges WHERE round=?;", list(as.integer(pr))), silent = TRUE)
    }

    showNotification(
      if (funded) sprintf("Closed: unlocked %d question(s). Carryover %.2f.", ws$units, new_carry)
      else if (identical(as.character(s$shortfall_policy[1]), "bank_all")) sprintf("Closed (not funded): banked pledges. Carryover %.2f.", new_carry)
      else "Closed (not funded): no one charged; carryover unchanged.",
      type = if (funded) "message" else "warning"
    )
  })

  output$admin_pledges_table <- DT::renderDT({
    req(authed(), is_admin())
    st <- state_poll(); s <- settings_poll()
    pr <- pledge_bucket_round(st, s)
    df <- db_query("
      SELECT p.user_id, u.display_name, p.round, p.pledge, p.submitted_at
      FROM pledges p
      LEFT JOIN users u ON u.user_id = p.user_id
      WHERE p.round=?
      ORDER BY p.pledge DESC, p.submitted_at DESC;
    ", list(as.integer(pr)))
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 12))
  })

  output$admin_round_totals <- renderTable({
    req(authed(), is_admin())
    st <- state_poll(); s <- settings_poll()
    idx <- current_question_index(st, s)
    pr  <- pledge_bucket_round(st, s)
    cost <- question_cost_for_round(idx, s$question_cost_schedule[1])
    ws <- compute_unlocks(pr, st$carryover[1], cost)
    data.frame(
      Metric = c("Pledging open", "Question index", "Cost now", "Carryover", "Pledged", "Effective", "Unlocks if closed"),
      Value  = c(as.integer(st$round_open[1]) == 1, idx, sprintf("%.2f", cost),
                 sprintf("%.2f", st$carryover[1]), sprintf("%.2f", ws$pledged),
                 sprintf("%.2f", ws$effective), ws$units),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  observeEvent(input$amend_sql_btn, {
    req(is_admin())
    sheet <- input$amend_sql_sheet
    table <- input$amend_sql_table
    df    <- googlesheets4::read_sheet(gs_backup_sheet_id(), sheet = sheet)

    if (table == "users") {
      # Safe merge: add missing columns then update rows — never touch pw_hash or PRIMARY KEY
      df <- dplyr::select(df, -dplyr::any_of("pw_hash"))
      if (!"user_id" %in% names(df)) {
        showNotification("Sheet must have a 'user_id' column to merge into users.", type = "error")
        return()
      }
      existing_cols <- DBI::dbListFields(get_con(), "users")
      new_cols      <- setdiff(names(df), existing_cols)
      for (col in new_cols) {
        try(db_exec(sprintf("ALTER TABLE users ADD COLUMN \"%s\" TEXT;", col)), silent = TRUE)
        logf("Added column to users:", col)
      }
      update_cols <- setdiff(names(df), c("user_id", "pw_hash"))
      for (i in seq_len(nrow(df))) {
        uid <- as.character(df$user_id[i] %||% "")
        if (!nzchar(uid)) next
        for (col in update_cols) {
          db_exec(sprintf("UPDATE users SET \"%s\" = ? WHERE user_id = ?;", col),
            list(as.character(df[[col]][i] %||% ""), uid))
        }
      }
      set_state()
      showNotification(sprintf("users updated: %d rows, columns: %s",
        nrow(df), paste(update_cols, collapse = ", ")), type = "message")
    } else {
      DBI::dbWriteTable(get_con(), table, df, overwrite = TRUE)
      showNotification("SQL database amended.", type = "message")
    }
  })

  output$admin_export_table <- DT::renderDT({
    req(authed(), is_admin())
    state_poll()
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

  # ---------------- Exam management ----------------

  observeEvent(input$adm_switch_exam, {
    req(is_admin())
    exam_id <- trimws(as.character(input$adm_active_exam %||% ""))
    if (!nzchar(exam_id)) { showNotification("Select an exam first.", type = "error"); return() }
    db_exec(
      "UPDATE game_state SET active_exam=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;",
      list(exam_id)
    )
    showNotification(
      sprintf("Switched to exam '%s'. Round, carryover, and unlocked questions unchanged.", exam_id),
      type = "message"
    )
  })

  observeEvent(input$do_upload_questions, {
    req(is_admin())
    exam_id <- trimws(as.character(input$upload_exam_id %||% ""))
    if (!nzchar(exam_id)) { showNotification("Enter an exam ID.", type = "error"); return() }

    fi <- input$upload_questions_csv
    if (is.null(fi)) { showNotification("Select a CSV file.", type = "error"); return() }

    df <- tryCatch(
      readr::read_csv(fi$datapath, show_col_types = FALSE),
      error = function(e) {
        showNotification(paste("CSV read failed:", conditionMessage(e)), type = "error")
        NULL
      }
    )
    if (is.null(df)) return()

    # Accept 'question_html' or 'question' column
    qcol <- intersect(c("question_html", "question"), names(df))
    if (!length(qcol)) {
      showNotification("CSV must have a 'question' or 'question_html' column.", type = "error")
      return()
    }
    qcol <- qcol[1]

    nums <- if ("question_num" %in% names(df)) {
      suppressWarnings(as.integer(df$question_num))
    } else {
      seq_len(nrow(df))
    }
    texts <- as.character(df[[qcol]])
    valid <- !is.na(texts) & nzchar(texts) & !is.na(nums)

    if (!any(valid)) {
      showNotification("No valid questions found in CSV.", type = "error")
      return()
    }

    db_exec("DELETE FROM exam_questions WHERE exam_id=?;", list(exam_id))
    for (i in which(valid)) {
      db_exec(
        "INSERT INTO exam_questions(exam_id, question_num, question_html) VALUES(?,?,?);",
        list(exam_id, as.integer(nums[i]), texts[i])
      )
    }
    set_state()
    showNotification(
      sprintf("Uploaded %d question(s) for exam '%s'.", sum(valid), exam_id),
      type = "message"
    )
  })

  output$admin_exam_questions_table <- renderTable({
    req(authed(), is_admin())
    state_poll()
    st <- get_state()
    exam_id <- as.character(st$active_exam[1] %||% "exam1")
    df <- db_query(
      "SELECT question_num AS '#', question_html AS 'Question' FROM exam_questions WHERE exam_id=? ORDER BY question_num;",
      list(exam_id)
    )
    if (!nrow(df)) return(data.frame(Note = "No questions uploaded — using built-in question bank."))
    df
  }, rownames = FALSE, sanitize.text.function = identity)

  output$dl_exam_questions <- downloadHandler(
    filename = function() {
      st <- get_state()
      sprintf("questions_%s.csv", as.character(st$active_exam[1] %||% "exam"))
    },
    content = function(file) {
      st <- get_state()
      exam_id <- as.character(st$active_exam[1] %||% "exam1")
      df <- db_query(
        "SELECT question_num, question_html FROM exam_questions WHERE exam_id=? ORDER BY question_num;",
        list(exam_id)
      )
      if (!nrow(df)) {
        df <- tibble::tibble(
          question_num  = seq_along(QUESTIONS),
          question_html = vapply(QUESTIONS, function(q) as.character(q), character(1))
        )
      }
      readr::write_csv(df, file)
    }
  )

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
      sprintf("Now impersonating %s. Switch to Student tab to see their view.", row$display_name[1]),
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

  # ---------------- Bulk grant / deduct ----------------
  observeEvent(input$do_bulk_grant, {
    req(is_admin())
    users_sel <- input$bulk_grant_users
    if (!length(users_sel)) {
      showNotification("No students selected.", type = "warning"); return()
    }
    amt <- suppressWarnings(as.numeric(input$bulk_grant_amt %||% 0))
    if (!is.finite(amt) || amt == 0) {
      showNotification("Amount must be nonzero.", type = "error"); return()
    }
    meta <- if (amt > 0) "admin_bulk_grant" else "admin_bulk_deduct"
    cap <- as.numeric(get_settings()$max_fp_held[1] %||% 8)
    granted_count <- 0L
    capped_count  <- 0L
    for (uid in users_sel) {
      effective_amt <- amt
      if (amt > 0) {
        current_held <- student_allocation_summary(as.character(uid))$remaining
        if (current_held >= cap) next
        effective_amt <- min(amt, cap - current_held)
        if (effective_amt < amt) capped_count <- capped_count + 1L
      }
      db_exec("
        INSERT INTO ledger(user_id, round, purpose, amount, meta)
        VALUES(?, NULL, 'grant', ?, ?);
      ", list(as.character(uid), -effective_amt, meta))
      granted_count <- granted_count + 1L
    }
    set_state()
    msg <- sprintf("%s %.2f pass(es) %s %d student(s).",
      if (amt > 0) "Granted" else "Deducted",
      abs(amt),
      if (amt > 0) "to" else "from",
      granted_count
    )
    skipped <- length(users_sel) - granted_count
    if (skipped > 0 && amt > 0)
      msg <- paste0(msg, sprintf(" %d already at %.0f-pass cap, skipped.", skipped, cap))
    if (capped_count > 0)
      msg <- paste0(msg, sprintf(" %d grant(s) reduced to stay at cap.", capped_count))
    showNotification(msg, type = "message")
  })

  session$onSessionEnded(function() {
    # Synchronous backup on session end if DB changed
    if (db_changed_since_last_backup()) {
      logf("session ended: backing up to Drive...")
      tryCatch(backup_db_to_drive(), error = function(e) {
        logf("session-end backup FAILED:", conditionMessage(e))
      })
    }
    if (!is.null(conn) && DBI::dbIsValid(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
  })
}

shinyApp(ui, server)
