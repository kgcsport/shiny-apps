# app.R — Clock/English auction with central SQLite + Drive backups
# ---------------------------------------------------------------
# Key features:
# - Central coordinating SQLite database (auction.sqlite) for all auction state & clicks
# - Backup/restore to Google Drive folder (PUB_ECON_FOLDER) to survive Connect Cloud idling
# - Login required: credentials are READ from latest Drive snapshot of the *final question reveal* DB
#   in FLEX_PASS_FOLDER_ID. We NEVER overwrite that credential DB; we only download its latest backup.
# - Admin tab is visible ONLY to users with is_admin==1 in credentials.

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  shiny, DT, dplyr, tibble, DBI, RSQLite, googledrive, future, promises,
  bcrypt, digest, stringr
)

# ---- Debug/Crash visibility ----
options(shiny.error = function() {
  # prints a traceback to the console logs instead of failing silently
  traceback(2)
})
options(warn = 1)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

logf <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste(vapply(list(...), as.character, character(1)), collapse = " ")
  cat(ts, "-", msg, "\n", file = stderr()); flush(stderr())
}

APP_CONFIG <- list(
  folder_id      = Sys.getenv("PUB_ECON_FOLDER_ID", ""),      # Drive folder for THIS auction app backups
  flex_folder_id = Sys.getenv("FLEX_PASS_FOLDER_ID", ""),  # Drive folder where final_question_reveal backups live
  cred_db_path   = "finalqdata_latest_backup.zip",                       # informational; we actually read from the latest zip snapshot
  gsa_json       = Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", ""),
  gsa_path       = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
)

logf("CONFIG:",
     "PUB_ECON_FOLDER present:", nzchar(APP_CONFIG$folder_id),
     "| FLEX_PASS_FOLDER_ID present:", nzchar(APP_CONFIG$flex_folder_id),
     "| gsa_json present:", nzchar(APP_CONFIG$gsa_json),
     "| gsa_path:", APP_CONFIG$gsa_path)

# ---- Local data dir (Connect-friendly) ----
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
DB_PATH  <- file.path(DATA_DIR, "auction.sqlite")

# ---- Google Drive auth (service account) ----
google_auth <- function() {
  already_authed <- tryCatch(!is.null(googledrive::drive_token()), error = function(e) FALSE)
  if (already_authed) return(invisible(TRUE))

  cred <- APP_CONFIG$gsa_path %||% ""
  if (!nzchar(cred) || !file.exists(cred)) {
    js <- APP_CONFIG$gsa_json %||% ""
    if (nzchar(js)) {
      cred <- tempfile(fileext = ".json")
      writeLines(js, cred)
    }
  }
  if (!nzchar(cred) || !file.exists(cred)) {
    logf("google_auth(): no service-account credentials available.")
    return(invisible(FALSE))
  }

  ok <- tryCatch({
    googledrive::drive_auth(
      path   = cred,
      scopes = c(
        "https://www.googleapis.com/auth/drive",
        "https://www.googleapis.com/auth/drive.file"
      )
    )
    TRUE
  }, error = function(e) {
    logf("google_auth(): failed:", conditionMessage(e))
    FALSE
  })

  invisible(ok)
}

# ---- Drive helpers ----
drive_folder_id <- function() APP_CONFIG$folder_id
flex_folder_id  <- function() APP_CONFIG$flex_folder_id

latest_zip_name <- function() "auction_latest_backup.zip"

# Final-question-reveal backup file name (from your existing app.R)
# If you used a different name, change this ONE constant.
FLEX_LATEST_ZIP <- APP_CONFIG$cred_db_path

# Try to reveal modified time; fall back to created_time if not present.
drive_ls_with_times <- function(folder_id) {
  files <- googledrive::drive_ls(googledrive::as_id(folder_id))
  # drive_reveal adds columns when possible; not all backends return modified_time reliably
  files <- tryCatch(
    googledrive::drive_reveal(files, "modified_time"),
    error = function(e) files
  )
  files <- tryCatch(
    googledrive::drive_reveal(files, "created_time"),
    error = function(e) files
  )
  files
}

pick_latest_file_id <- function(folder_id, filename) {
  files <- drive_ls_with_times(folder_id)
  hit <- files[files$name == filename, , drop = FALSE]
  if (nrow(hit) < 1) return(list(id = NA_character_, when = NA_character_))

  # Prefer modified_time; otherwise created_time; otherwise just first.
  when <- NA_character_
  if ("modified_time" %in% names(hit) && any(!is.na(hit$modified_time))) {
    o <- order(hit$modified_time, decreasing = TRUE, na.last = TRUE)
    when <- as.character(hit$modified_time[o[1]])
    return(list(id = hit$id[o[1]], when = when))
  }
  if ("created_time" %in% names(hit) && any(!is.na(hit$created_time))) {
    o <- order(hit$created_time, decreasing = TRUE, na.last = TRUE)
    when <- as.character(hit$created_time[o[1]])
    return(list(id = hit$id[o[1]], when = when))
  }
  list(id = hit$id[1], when = NA_character_)
}

# ---- Auction DB backup/restore (THIS app) ----
hash_db <- function(path) {
  parts <- c(path, paste0(path, "-wal"), paste0(path, "-shm"))
  parts <- parts[file.exists(parts)]
  if (!length(parts)) return(NA_character_)
  raw <- unlist(lapply(parts, function(f) readBin(f, what = "raw", n = file.info(f)$size)))
  digest::digest(raw, algo = "xxhash64")
}

zip_db_files <- function(path) {
  files <- c(path, paste0(path, "-wal"), paste0(path, "-shm"))
  files <- files[file.exists(files)]
  if (!length(files)) return(NULL)
  zipfile <- file.path(tempdir(), sprintf("auction_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")))
  utils::zip(zipfile, files = files, flags = "-j")
  zipfile
}

backup_db_to_drive <- function() {
  if (!google_auth()) return(list(ok = FALSE, msg = "google_auth() failed / missing credentials"))
  folder <- drive_folder_id()
  if (!nzchar(folder)) return(list(ok = FALSE, msg = "PUB_ECON_FOLDER not set"))

  # Access check
  tryCatch(googledrive::drive_get(googledrive::as_id(folder)), error = function(e) {
    stop("drive_get(PUB_ECON_FOLDER) failed: ", conditionMessage(e))
  })

  # checkpoint WAL before snapshot
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  try(DBI::dbExecute(con, "PRAGMA wal_checkpoint(FULL);"), silent = TRUE)

  zipfile <- zip_db_files(DB_PATH)
  if (is.null(zipfile)) return(list(ok = FALSE, msg = "zip_db_files returned NULL (DB missing?)"))

  latest_zip <- file.path(tempdir(), latest_zip_name())
  if (!isTRUE(file.copy(zipfile, latest_zip, overwrite = TRUE))) {
    return(list(ok = FALSE, msg = "failed to stage latest zip"))
  }

  # Optional timestamped upload
  ts_err <- NULL
  tryCatch({
    googledrive::drive_upload(
      media = zipfile,
      path  = googledrive::as_id(folder),
      name  = basename(zipfile),
      type  = "application/zip",
      overwrite = FALSE
    )
  }, error = function(e) ts_err <<- conditionMessage(e))

  ok_latest <- tryCatch({
    googledrive::drive_upload(
      media = latest_zip,
      path  = googledrive::as_id(folder),
      name  = latest_zip_name(),
      type  = "application/zip",
      overwrite = TRUE
    )
    TRUE
  }, error = function(e) {
    logf("backup_db_to_drive(): overwrite failed:", conditionMessage(e))
    FALSE
  })

  if (ok_latest) {
    msg <- paste0("uploaded ", latest_zip_name(),
                  if (!is.null(ts_err)) paste0(" (timestamp upload failed: ", ts_err, ")") else "")
    list(ok = TRUE, msg = msg)
  } else {
    list(ok = FALSE, msg = "latest overwrite upload failed")
  }
}

restore_db_from_drive <- function(filename = latest_zip_name()) {
  if (!google_auth()) return(FALSE)
  folder <- drive_folder_id()
  if (!nzchar(folder)) {
    logf("restore_db_from_drive(): PUB_ECON_FOLDER not set")
    return(FALSE)
  }

  info <- pick_latest_file_id(folder, filename)
  if (!nzchar(info$id) || is.na(info$id)) {
    logf("restore_db_from_drive(): no snapshot found:", filename)
    return(FALSE)
  }

  zipfile <- file.path(tempdir(), filename)
  googledrive::drive_download(file = info$id, path = zipfile, overwrite = TRUE)

  # close any open connection first (handled below via conn)
  for (f in c(DB_PATH, paste0(DB_PATH, "-wal"), paste0(DB_PATH, "-shm"))) {
    if (file.exists(f)) try(unlink(f, force = TRUE), silent = TRUE)
  }

  utils::unzip(zipfile, exdir = dirname(DB_PATH))
  logf("restore_db_from_drive(): restored DB from Drive | when:", info$when %||% "NA")
  TRUE
}

# ---- Credentials: READ ONLY from flex-pass game Drive snapshot ----
# We download FLEX_LATEST_ZIP from FLEX_PASS_FOLDER_ID, unzip to temp, open the SQLite,
# and read users(user_id, display_name, pw_hash, is_admin).
read_credentials_from_flex_snapshot <- function() {
  if (!google_auth()) stop("read_credentials_from_flex_snapshot(): google_auth() failed")
  folder <- flex_folder_id()
  if (!nzchar(folder)) stop("read_credentials_from_flex_snapshot(): FLEX_PASS_FOLDER_ID not set")

  info <- pick_latest_file_id(folder, FLEX_LATEST_ZIP)
  if (!nzchar(info$id) || is.na(info$id)) {
    stop("No credentials snapshot found in FLEX_PASS_FOLDER_ID named ", FLEX_LATEST_ZIP)
  }

  zipfile <- file.path(tempdir(), FLEX_LATEST_ZIP)
  googledrive::drive_download(file = info$id, path = zipfile, overwrite = TRUE)

  exdir <- file.path(tempdir(), paste0("flexcred_", as.integer(Sys.time())))
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zipfile, exdir = exdir)

  # The snapshot created by your other app zips DB_PATH (finalqdata.sqlite) (and maybe -wal/-shm)
  # We search for the first *.sqlite file inside.
  dbs <- list.files(exdir, pattern = "\\.sqlite$", full.names = TRUE, recursive = TRUE)
  if (!length(dbs)) stop("Could not find any .sqlite inside ", FLEX_LATEST_ZIP)

  cred_db <- dbs[1]
  con <- DBI::dbConnect(RSQLite::SQLite(), cred_db)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  # Ensure schema matches expected
  # (Your existing app stores users table as: user_id, display_name, is_admin, pw_hash)
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(users);")
  need <- c("user_id", "display_name", "pw_hash", "is_admin")
  if (!all(need %in% cols$name)) {
    stop("Credentials DB users table missing columns: ",
         paste(setdiff(need, cols$name), collapse = ", "))
  }

  u <- DBI::dbGetQuery(con, "SELECT user_id, display_name, pw_hash, is_admin FROM users;") |>
    tibble::as_tibble() |>
    mutate(
      user_id = trimws(as.character(user_id)),
      display_name = as.character(display_name),
      pw_hash = as.character(pw_hash),
      is_admin = as.integer(is_admin)
    )

  logf("Users in credentials DB: ", paste(u$user_id, collapse = ", "))

  list(users = u, snapshot_when = info$when %||% NA_character_)
}

# ---- SQLite connection (auction db) ----
conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  conn
}
db_exec  <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

reg.finalizer(.GlobalEnv, function(e) {
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    try(DBI::dbDisconnect(conn), silent = TRUE)
  }
}, onexit = TRUE)

init_db <- function() {
  db_exec("
    CREATE TABLE IF NOT EXISTS auction_settings (
      id INTEGER PRIMARY KEY CHECK (id=1),
      item_name TEXT,
      tick_size REAL,
      tick_seconds INTEGER,
      start_price REAL,
      units_available INTEGER,
      max_price REAL,
      round INTEGER
    );
  ")
  db_exec("
    CREATE TABLE IF NOT EXISTS auction_state (
      id INTEGER PRIMARY KEY CHECK (id=1),
      running INTEGER,
      current_price REAL,
      units_remaining INTEGER,
      started_at TEXT,
      last_tick_at TEXT,
      ended_at TEXT
    );
  ")
  db_exec("
    CREATE TABLE IF NOT EXISTS accepts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      round INTEGER,
      user_id TEXT,
      display_name TEXT,
      price REAL,
      accepted_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")
  db_exec("CREATE INDEX IF NOT EXISTS ix_accepts_round ON accepts(round);")
  db_exec("CREATE INDEX IF NOT EXISTS ix_accepts_user  ON accepts(user_id);")

  nset <- db_query("SELECT COUNT(*) n FROM auction_settings WHERE id=1;")$n[1]
  if (nset == 0) {
    db_exec(
      "INSERT INTO auction_settings(id, item_name, tick_size, tick_seconds, start_price, units_available, max_price, round)
       VALUES (1, ?, ?, ?, ?, ?, ?, ?);",
      list("Fee-free driving pass (one unit)", 1, 3, 0, 5, 9999, 1)
    )
  }

  nst <- db_query("SELECT COUNT(*) n FROM auction_state WHERE id=1;")$n[1]
  if (nst == 0) {
    db_exec(
      "INSERT INTO auction_state(id, running, current_price, units_remaining, started_at, last_tick_at, ended_at)
       VALUES (1, 0, 0, 0, NULL, NULL, NULL);"
    )
  }
}

get_settings <- function() db_query("SELECT * FROM auction_settings WHERE id=1;")
get_state    <- function() db_query("SELECT * FROM auction_state WHERE id=1;")

# Avoid NA in conditionals when state/settings are empty (e.g. after bad restore)
running_now <- function() {
  st <- get_state()
  if (!nrow(st)) return(FALSE)
  isTRUE(as.integer(st$running[1]) == 1L)
}
safe_num <- function(x, default = 0) {
  v <- as.numeric(x)
  if (length(v) != 1 || is.na(v)) default else v
}
safe_int <- function(x, default = 0L) {
  v <- as.integer(x)
  if (length(v) != 1 || is.na(v)) default else v
}

start_auction <- function() {
  s <- get_settings()
  db_exec(
    "UPDATE auction_state
     SET running=1, current_price=?, units_remaining=?, started_at=CURRENT_TIMESTAMP, last_tick_at=CURRENT_TIMESTAMP, ended_at=NULL
     WHERE id=1;",
    list(as.numeric(s$start_price[1]), as.integer(s$units_available[1]))
  )
  invisible(TRUE)
}

stop_auction <- function() {
  db_exec("UPDATE auction_state SET running=0, ended_at=CURRENT_TIMESTAMP WHERE id=1;")
  invisible(TRUE)
}

advance_tick_if_needed <- function() {
  st <- get_state()
  if (as.integer(st$running[1]) != 1) return(invisible(FALSE))
  if (as.integer(st$units_remaining[1]) <= 0) {
    stop_auction()
    return(invisible(TRUE))
  }

  s <- get_settings()
  tick_s <- as.integer(s$tick_seconds[1])
  last <- as.POSIXct(st$last_tick_at[1], tz = "UTC")
  now  <- as.POSIXct(Sys.time(), tz = "UTC")

  if (is.na(last) || difftime(now, last, units = "secs") >= tick_s) {
    new_price <- as.numeric(st$current_price[1]) + as.numeric(s$tick_size[1])
    maxp <- as.numeric(s$max_price[1])
    if (is.finite(maxp) && new_price > maxp) {
      # hit cap -> stop
      db_exec("UPDATE auction_state SET running=0, ended_at=CURRENT_TIMESTAMP WHERE id=1;")
      return(invisible(TRUE))
    }
    db_exec(
      "UPDATE auction_state SET current_price=?, last_tick_at=CURRENT_TIMESTAMP WHERE id=1;",
      list(new_price)
    )
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

record_accept <- function(user_id, display_name) {
  st <- get_state()
  if (as.integer(st$running[1]) != 1) return(list(ok = FALSE, msg = "Auction is not running."))
  if (as.integer(st$units_remaining[1]) <= 0) return(list(ok = FALSE, msg = "No units remaining."))

  s  <- get_settings()
  rd <- as.integer(s$round[1])
  pr <- as.numeric(st$current_price[1])

  # Deduct unit atomically (best-effort)
  db_exec("INSERT INTO accepts(round, user_id, display_name, price) VALUES (?,?,?,?);",
          list(rd, user_id, display_name, pr))

  # recompute remaining based on count (robust to race conditions)
  nacc <- db_query("SELECT COUNT(*) n FROM accepts WHERE round=?;", list(rd))$n[1]
  remaining <- max(0L, as.integer(s$units_available[1]) - as.integer(nacc))

  db_exec("UPDATE auction_state SET units_remaining=? WHERE id=1;", list(remaining))
  if (remaining <= 0) stop_auction()

  list(ok = TRUE, msg = paste0("Accepted at price ", pr, "."))
}

# ---- Backup debounce ----
future::plan(future::multisession)
.last_hash <- NULL
backup_if_changed_async <- function(label = "backup") {
  new_hash <- hash_db(DB_PATH)
  if (!is.null(.last_hash) && identical(new_hash, .last_hash)) return(invisible(FALSE))
  .last_hash <<- new_hash

  promises::future_promise({
    backup_db_to_drive()
  }) %...>% (function(res) {
    if (is.list(res)) logf(label, "ok=", res$ok, "|", res$msg %||% "")
    invisible(NULL)
  }) %...!% (function(e) {
    logf(label, "FAILED:", conditionMessage(e))
  }) -> .ignored

  invisible(TRUE)
}

# ---- UI helpers ----
login_ui <- function(msg = NULL) {
  wellPanel(
    h3("Auction Login"),
    if (!is.null(msg)) div(style="color:#b00; font-weight:600;", msg),
    textInput("login_user", "Username"),
    passwordInput("login_pw", "Password"),
    actionButton("login_btn", "Sign in", class="btn-primary")
  )
}

# ---- Shiny app ----
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .bigprice { font-size: 56px; font-weight: 800; }
    .muted { color: #666; }
    .ok { color: #0a0; font-weight: 600; }
    .bad { color: #b00; font-weight: 600; }
  "))),
  uiOutput("auth_gate"),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
    onStop(function() {
    logf("==== onStop() fired (R process stopping) ====")
    })

    session$onSessionEnded(function() {
    logf("==== session ended (client disconnected) ====")
    })

  # ---- Start-up: restore auction DB snapshot if available ----
  tryCatch({
    if (nzchar(APP_CONFIG$folder_id) && google_auth()) {
      restore_db_from_drive()  # best-effort
    }
  }, error = function(e) logf("Startup restore (auction db) failed:", conditionMessage(e)))

  # Ensure schema exists even if restore failed
  tryCatch(init_db(), error = function(e) stop("init_db failed: ", conditionMessage(e)))

  # ---- Load credentials from flex snapshot ----
  cred_cache <- reactiveValues(users = NULL, when = NA_character_)
  refresh_creds <- function() {
    out <- read_credentials_from_flex_snapshot()
    cred_cache$users <- out$users
    cred_cache$when  <- out$snapshot_when
    logf("Loaded credentials:", nrow(out$users), "| snapshot when:", out$snapshot_when %||% "NA")
  }

  tryCatch(refresh_creds(), error = function(e) {
    # If creds fail to load, force a visible error rather than silent crash
    logf("Credential load failed:", conditionMessage(e))
    cred_cache$users <- tibble(user_id=character(), display_name=character(), pw_hash=character(), is_admin=integer())
    cred_cache$when  <- NA_character_
  })

  # ---- Auth state ----
  rv <- reactiveValues(authed = FALSE, user = NULL, name = NULL, is_admin = FALSE)

  output$auth_gate <- renderUI({
    if (!rv$authed) return(login_ui())
    NULL
  })

  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pw %||% ""
    if (!nzchar(u) || !nzchar(p)) return()

    users <- cred_cache$users
    row <- users |> filter(.data$user_id == u)
    if (nrow(row) != 1) {
      output$auth_gate <- renderUI(login_ui("Unknown username."))
      return()
    }
    ph <- row$pw_hash[1] %||% ""
    if (!nzchar(ph)) {
      output$auth_gate <- renderUI(login_ui("Password not set for this user."))
      return()
    }
    if (!bcrypt::checkpw(p, ph)) {
      output$auth_gate <- renderUI(login_ui("Incorrect password."))
      return()
    }

    rv$authed   <- TRUE
    rv$user     <- u
    rv$name     <- row$display_name[1] %||% u
    rv$is_admin <- isTRUE(as.integer(row$is_admin[1]) == 1)

    logf("LOGIN OK:", rv$user, "| admin=", rv$is_admin)
  })

  authed   <- reactive(rv$authed)
  user_id  <- reactive(rv$user)
  user_nm  <- reactive(rv$name)
  is_admin <- reactive(rv$is_admin)

  # ---- Main UI ----
  output$main_ui <- renderUI({
    logf("RENDER main_ui start")
    req(authed())
    logf("Authed: ", authed())

    tabsetPanel(
      tabPanel("Auction",
        fluidRow(
          column(
            7,
            h3("Clock Auction"),
            p(class="muted",
              "Price ticks up automatically. Click 'Accept' if you're willing to buy one unit at the current price."),
            p(class="muted", paste0("Credentials snapshot time: ", cred_cache$when %||% "NA")),
            uiOutput("auction_status"),
            uiOutput("auction_controls"),
            hr(),
            h4("Acceptances (this round)"),
            DTOutput("accept_table")
          ),
          column(
            5,
            h4("Your info"),
            verbatimTextOutput("me"),
            hr(),
            h4("What this teaches"),
            tags$ul(
              tags$li("Marginal decision: accept if value ≥ current price."),
              tags$li("Supply: as price rises, fewer participants accept."),
              tags$li("Equilibrium/clearing: units fill at a marginal price.")
            )
          )
        )
      ),
      tabPanel("Results",
        h3("Round summaries"),
        p(class="muted", "Use this to build a supply curve: price on y-axis, cumulative quantity on x-axis."),
        DTOutput("round_summary")
      ),
      tabPanel("Admin",
        uiOutput("admin_gate")
      )
    )
  })

  output$me <- renderText({
    req(authed())
    paste0("User: ", user_id(), "\n",
           "Name: ", user_nm(), "\n",
           "Admin: ", isTRUE(is_admin()))
  })

  # ---- Live ticking ----
  auto <- reactiveTimer(500) # check twice per second
  observe({
    auto()
    tryCatch({
      changed <- advance_tick_if_needed()
      if (isTRUE(changed)) {
        # best-effort backup
        backup_if_changed_async("tick backup")
      }
    }, error = function(e) logf("Tick loop error:", conditionMessage(e)))
  })

  output$auction_status <- renderUI({
    logf("RENDER auction_status start")
    st <- get_state()
    s  <- get_settings()
    running <- nrow(st) > 0 && isTRUE(as.integer(st$running[1]) == 1L)
    div(
      p(tags$b("Item: "), s$item_name[1] %||% ""),
      p(tags$b("Round: "), safe_int(s$round[1], 1L)),
      p(tags$b("Status: "), if (running) span(class="ok","RUNNING") else span(class="bad","STOPPED")),
      div(class="bigprice", paste0("$", format(round(safe_num(st$current_price[1]), 2), nsmall = 2))),
      p(class="muted", paste0("Units remaining: ", safe_int(st$units_remaining[1], 0L)))
    )
  })

  output$auction_controls <- renderUI({
    logf("RENDER auction_controls start")
    st <- get_state()
    running <- nrow(st) > 0 && isTRUE(as.integer(st$running[1]) == 1L)
    tagList(
      actionButton("accept_btn", "Accept (buy 1 unit at current price)", class = "btn-success btn-lg"),
      br(), br(),
      verbatimTextOutput("accept_msg")
    )
  })

  accept_msg <- reactiveVal("")

  observeEvent(input$accept_btn, {
    logf("observeEvent accept_btn start")
    req(authed())
    res <- tryCatch(record_accept(user_id(), user_nm()),
                    error = function(e) list(ok=FALSE, msg=conditionMessage(e)))
    accept_msg(if (isTRUE(res$ok)) paste0("OK: ", res$msg) else paste0("NO: ", res$msg))
    backup_if_changed_async("accept backup")
  })

  output$accept_msg <- renderText(accept_msg())

  output$accept_table <- renderDT({
    logf("RENDER accept_table start")
    s <- get_settings()
    rd <- safe_int(s$round[1], 1L)
    df <- db_query("SELECT display_name, user_id, price, accepted_at FROM accepts WHERE round=? ORDER BY datetime(accepted_at) ASC;",
                   list(rd))
    datatable(df, rownames = FALSE, options = list(pageLength = 25))
  })

  output$round_summary <- renderDT({
    logf("RENDER round_summary start")
    df <- db_query("
      SELECT round,
             COUNT(*) AS units_filled,
             MIN(price) AS min_price,
             MAX(price) AS max_price,
             AVG(price) AS avg_price
      FROM accepts
      GROUP BY round
      ORDER BY round;
    ")
    datatable(df, rownames = FALSE, options = list(pageLength = 50))
  })

  # ---- Admin gate and controls ----
  output$admin_gate <- renderUI({
    logf("RENDER admin_gate start")
    req(authed())
    if (!isTRUE(is_admin())) {
      return(tagList(h3("Admin"), p("You are not an admin."), br()))
    }

    st <- get_state()
    s  <- get_settings()
    running <- nrow(st) > 0 && isTRUE(as.integer(st$running[1]) == 1L)

    tagList(
      h3("Admin controls"),
      p(class="muted", "Admin visibility is based on is_admin in the credentials snapshot."),
      fluidRow(
        column(
          6,
          h4("Auction settings"),
          textInput("item_name", "Item name", value = s$item_name[1] %||% ""),
          numericInput("start_price", "Start price", value = safe_num(s$start_price[1], 0), min = 0),
          numericInput("tick_size", "Tick size ($ per tick)", value = safe_num(s$tick_size[1], 1), min = 0.01, step = 0.25),
          numericInput("tick_seconds", "Seconds per tick", value = safe_int(s$tick_seconds[1], 3), min = 1, step = 1),
          numericInput("units_available", "Units available", value = safe_int(s$units_available[1], 5), min = 1, step = 1),
          numericInput("max_price", "Max price (cap; set big for 'no cap')", value = safe_num(s$max_price[1], 9999), min = 0, step = 1),
          actionButton("save_settings", "Save settings", class="btn-primary")
        ),
        column(
          6,
          h4("Round control"),
          p(tags$b("Current round:"), safe_int(s$round[1], 1L)),
          if (!running) actionButton("start_btn", "Start auction", class="btn-success"),
          if (running)  actionButton("stop_btn",  "Stop auction",  class="btn-warning"),
          br(), br(),
          actionButton("next_round_btn", "Advance to next round (clears nothing; just increments round)", class="btn-info"),
          hr(),
          h4("Maintenance"),
          actionButton("backup_now", "Backup now", class="btn-default"),
          actionButton("restore_now", "Restore latest backup (auction db)", class="btn-default"),
          br(), br(),
          actionButton("refresh_creds", "Refresh credentials from FLEX snapshot", class="btn-default"),
          verbatimTextOutput("admin_msg")
        )
      )
    )
  })

  admin_msg <- reactiveVal("")

  observeEvent(input$save_settings, {
    logf("observeEvent save_settings start")
    req(authed(), is_admin())
    tryCatch({
      db_exec(
        "UPDATE auction_settings
         SET item_name=?, tick_size=?, tick_seconds=?, start_price=?, units_available=?, max_price=?
         WHERE id=1;",
        list(
          input$item_name %||% "",
          as.numeric(input$tick_size),
          as.integer(input$tick_seconds),
          as.numeric(input$start_price),
          as.integer(input$units_available),
          as.numeric(input$max_price)
        )
      )
      admin_msg("Saved settings.")
      backup_if_changed_async("settings backup")
    }, error = function(e) admin_msg(paste0("ERROR: ", conditionMessage(e))))
  })

  observeEvent(input$start_btn, {
    logf("observeEvent start_btn start")
    req(authed(), is_admin())
    tryCatch({
      start_auction()
      admin_msg("Started auction.")
      backup_if_changed_async("start backup")
    }, error = function(e) admin_msg(paste0("ERROR: ", conditionMessage(e))))
  })

  observeEvent(input$stop_btn, {
    logf("observeEvent stop_btn start")
    req(authed(), is_admin())
    tryCatch({
      stop_auction()
      admin_msg("Stopped auction.")
      backup_if_changed_async("stop backup")
    }, error = function(e) admin_msg(paste0("ERROR: ", conditionMessage(e))))
  })

  observeEvent(input$next_round_btn, {
    logf("observeEvent next_round_btn start")
    req(authed(), is_admin())
    tryCatch({
      st <- get_state()
      if (as.integer(st$running[1]) == 1) stop("Stop the auction before advancing rounds.")
      db_exec("UPDATE auction_settings SET round = round + 1 WHERE id=1;")
      # reset displayed price/remaining for cleanliness (not required)
      s <- get_settings()
      db_exec("UPDATE auction_state SET current_price=?, units_remaining=?, started_at=NULL, last_tick_at=NULL, ended_at=NULL WHERE id=1;",
              list(as.numeric(s$start_price[1]), as.integer(s$units_available[1])))
      admin_msg("Advanced to next round.")
      backup_if_changed_async("round advance backup")
    }, error = function(e) admin_msg(paste0("ERROR: ", conditionMessage(e))))
  })

  observeEvent(input$backup_now, {
    logf("observeEvent backup_now start")
    req(authed(), is_admin())
    res <- tryCatch(backup_db_to_drive(), error = function(e) list(ok=FALSE, msg=conditionMessage(e)))
    admin_msg(paste0("Backup: ", res$ok, " | ", res$msg %||% ""))
  })

  observeEvent(input$restore_now, {
    logf("observeEvent restore_now start")
    req(authed(), is_admin())
    ok <- tryCatch(restore_db_from_drive(), error = function(e) { admin_msg(paste0("ERROR: ", conditionMessage(e))); FALSE })
    if (isTRUE(ok)) admin_msg("Restored latest auction snapshot from Drive.")
  })

  observeEvent(input$refresh_creds, {
    logf("observeEvent refresh_creds start")
    req(authed(), is_admin())
    tryCatch({
      refresh_creds()
      admin_msg(paste0("Refreshed credentials. Snapshot time: ", cred_cache$when %||% "NA"))
    }, error = function(e) admin_msg(paste0("ERROR: ", conditionMessage(e))))
  })

  output$admin_msg <- renderText(admin_msg())

  # ---- Session end backup (best effort) ----
  session$onSessionEnded(function() {
    tryCatch({
      backup_if_changed_async("session end backup")
    }, error = function(e) NULL)
  })
}

# ---- Run app with guard so crashes surface ----
shinyApp(
  ui = ui,
  server = function(input, output, session) {
    tryCatch(
      server(input, output, session),
      error = function(e) {
        logf("FATAL server error:", conditionMessage(e))
        stop(e)
      }
    )
  }
)
