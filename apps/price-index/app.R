try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
# app.R — Personal Price Index Activity
# Students build a basket of goods and track prices across waves.
# Auth: bcrypt + SQLite — shares finalqdata.sqlite with final_question_reveal.
# Users (including passwords) come from that shared DB; no separate credentials file needed.
# DB backed up to Google Drive on session end (same FLEX_PASS_FOLDER_ID as final_question_reveal).

library(shiny); library(DT); library(bcrypt); library(dplyr); library(tidyr)
library(tibble); library(forcats); library(DBI); library(RSQLite); library(ggplot2)
library(scales); library(googledrive); library(promises); library(future)

future::plan(future::sequential)  # backup_async() is fire-and-forget; no workers needed

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b
logf   <- function(...) cat(format(Sys.time()), "-", paste(...), "\n", file = stderr())

# ── Database ──────────────────────────────────────────────────────────────────
app_data_dir <- local({
  dir <- NULL
  function() {
    if (!is.null(dir)) return(dir)
    root <- Sys.getenv("CONNECT_CONTENT_DIR", unset = getwd())
    d <- file.path(root, "data")
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(d)) stop("Data directory not writable: ", d)
    dir <<- normalizePath(d, winslash = "/", mustWork = TRUE)
    dir
  }
})

# Shared DB with final_question_reveal — same CONNECT_CONTENT_DIR resolution
DB_PATH <- file.path(app_data_dir(), "finalqdata.sqlite")
conn    <- NULL

get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn))
    conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  conn
}
db_exec  <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

init_db <- function() {
  # users table is owned by final_question_reveal — do not create or seed here.
  # Ensure optional columns exist in case this runs before final_question_reveal does.
  try(db_exec("ALTER TABLE users ADD COLUMN pw_hash  TEXT;"), silent = TRUE)
  try(db_exec("ALTER TABLE users ADD COLUMN section  TEXT;"), silent = TRUE)

  db_exec("CREATE TABLE IF NOT EXISTS app_state (
    id             INTEGER PRIMARY KEY CHECK(id = 1),
    current_wave   INTEGER DEFAULT 1,
    categories     TEXT,
    price_sources  TEXT,
    updated_at     TEXT    DEFAULT (CURRENT_TIMESTAMP)
  );")
  try(db_exec("ALTER TABLE app_state ADD COLUMN categories    TEXT;"), silent = TRUE)
  try(db_exec("ALTER TABLE app_state ADD COLUMN price_sources TEXT;"), silent = TRUE)

  # Each student's item definition (set once in wave 1, immutable after)
  db_exec("CREATE TABLE IF NOT EXISTS basket_items (
    item_id        INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id        TEXT    NOT NULL,
    item_name      TEXT    NOT NULL,
    store          TEXT    NOT NULL,
    category       TEXT    NOT NULL,
    times_per_month REAL   NOT NULL,
    created_at     TEXT    DEFAULT (CURRENT_TIMESTAMP),
    UNIQUE(user_id, item_name, store)
  );")

  # One price record per item per wave; timestamp auto-set
  db_exec("CREATE TABLE IF NOT EXISTS price_records (
    record_id   INTEGER PRIMARY KEY AUTOINCREMENT,
    item_id     INTEGER NOT NULL REFERENCES basket_items(item_id),
    user_id     TEXT    NOT NULL,
    price       REAL    NOT NULL,
    source      TEXT,
    wave        INTEGER NOT NULL,
    recorded_at TEXT    DEFAULT (CURRENT_TIMESTAMP),
    UNIQUE(item_id, wave)
  );")
  # Migration: add source column if upgrading from a DB without it
  cols <- db_query("PRAGMA table_info(price_records);")$name
  if (!"source" %in% cols)
    db_exec("ALTER TABLE price_records ADD COLUMN source TEXT;")

  db_exec("CREATE INDEX IF NOT EXISTS ix_pr_user  ON price_records(user_id);")
  db_exec("CREATE INDEX IF NOT EXISTS ix_pr_wave  ON price_records(wave);")
  db_exec("CREATE INDEX IF NOT EXISTS ix_bi_user  ON basket_items(user_id);")

  n <- db_query("SELECT COUNT(*) n FROM app_state WHERE id=1;")$n[1]
  if (!n) db_exec("INSERT INTO app_state(id, current_wave) VALUES(1, 1);")
}

init_db()

# ── List helpers — categories & sources stored in app_state ──────────────────
encode_list <- function(x) paste(x, collapse = "\n")
decode_list <- function(s) Filter(nzchar, trimws(strsplit(s %||% "", "\n")[[1]]))

get_categories <- function() {
  raw <- db_query("SELECT categories FROM app_state WHERE id=1;")$categories[1]
  lst <- decode_list(raw)
  if (length(lst)) lst else BLS_CATEGORIES
}

get_price_sources <- function() {
  raw <- db_query("SELECT price_sources FROM app_state WHERE id=1;")$price_sources[1]
  lst <- decode_list(raw)
  if (length(lst)) lst else PRICE_SOURCES
}

save_categories <- function(cats) {
  db_exec(
    "UPDATE app_state SET categories=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;",
    list(encode_list(cats)))
}

save_price_sources <- function(srcs) {
  db_exec(
    "UPDATE app_state SET price_sources=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;",
    list(encode_list(srcs)))
}

# ── Google Drive backup ───────────────────────────────────────────────────────
google_auth <- function() {
  already <- tryCatch(!is.null(googledrive::drive_token()), error = function(e) FALSE)
  if (isTRUE(already)) return(invisible(TRUE))
  cred <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (!nzchar(cred) || !file.exists(cred)) {
    js <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
    if (nzchar(js)) { cred <- tempfile(fileext = ".json"); writeLines(js, cred) }
  }
  if (!nzchar(cred) || !file.exists(cred)) return(invisible(FALSE))
  tryCatch({
    googledrive::drive_auth(path = cred,
      scopes = c("https://www.googleapis.com/auth/drive",
                 "https://www.googleapis.com/auth/drive.file"))
    invisible(TRUE)
  }, error = function(e) { logf("google_auth() failed:", e$message); invisible(FALSE) })
}

backup_db <- function() {
  # Uses the same folder as final_question_reveal so all DB backups land together.
  folder_id <- Sys.getenv("FLEX_PASS_FOLDER_ID", "")
  if (!nzchar(folder_id)) {
    logf("backup_db(): FLEX_PASS_FOLDER_ID not set — skipping backup.")
    return(invisible(FALSE))
  }
  if (!isTRUE(google_auth())) return(invisible(FALSE))
  try(DBI::dbExecute(get_con(), "PRAGMA wal_checkpoint(FULL);"), silent = TRUE)
  zf <- file.path(tempdir(), sprintf("finalqdata_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")))
  utils::zip(zf, files = DB_PATH[file.exists(DB_PATH)], flags = "-j")
  googledrive::drive_upload(media = zf, path = googledrive::as_id(folder_id),
                            name = basename(zf), type = "application/zip", overwrite = FALSE)
  logf("DB backup uploaded:", basename(zf))
  invisible(TRUE)
}

backup_async <- function() {
  promises::future_promise(backup_db()) %...!%
    (function(e) logf("Backup failed:", e$message)) -> .ignored
  invisible(TRUE)
}

# ── Constants ─────────────────────────────────────────────────────────────────
BLS_CATEGORIES <- c(
  "Food at Home (groceries)",
  "Food Away from Home (restaurants, cafes)",
  "Housing & Utilities",
  "Transportation",
  "Medical Care",
  "Entertainment & Recreation",
  "Education & Technology",
  "Personal Care",
  "Clothing & Apparel",
  "Other"
)

PRICE_SOURCES <- c(
  "In store (price tag)",
  "Store website",
  "Receipt",
  "App (Instacart, Amazon, etc.)"
)

# ── Anonymization (class view) ────────────────────────────────────────────────
ECONOMISTS <- c(
  "Adam Smith", "David Ricardo", "John Maynard Keynes", "Milton Friedman",
  "Paul Samuelson", "Friedrich Hayek", "Alfred Marshall", "Irving Fisher",
  "Joseph Schumpeter", "Vilfredo Pareto", "Arthur Pigou", "Gary Becker",
  "George Stigler", "Ronald Coase", "James Tobin", "Robert Solow",
  "Paul Krugman", "Janet Yellen", "Ben Bernanke", "Lawrence Summers",
  "Amartya Sen", "Elinor Ostrom", "Kenneth Arrow", "Paul Volcker",
  "Robert Lucas", "Thomas Sargent", "Eugene Fama", "Robert Shiller",
  "Angus Deaton", "Esther Duflo", "Abhijit Banerjee", "Michael Spence",
  "George Akerlof", "Joseph Stiglitz", "Oliver Williamson", "Finn Kydland",
  "Edward Prescott", "Christopher Sims", "Lars Peter Hansen", "Jean Tirole",
  "Oliver Hart", "Bengt Holmstrom", "William Nordhaus", "Paul Romer",
  "Richard Thaler", "Robert Engle", "Clive Granger", "James Heckman",
  "Daniel Kahneman", "Vernon Smith"
)

# Deterministic alias per user — stable even when other users join
economist_for_uid <- function(uid) {
  h   <- digest::digest(uid, algo = "crc32")
  idx <- (strtoi(substr(h, 1, 7), 16L) %% length(ECONOMISTS)) + 1L
  ECONOMISTS[[idx]]
}

# uid -> alias map; rare hash collisions get a Roman suffix (II, III, …)
make_anon_map <- function() {
  uids <- db_query(
    "SELECT DISTINCT user_id FROM basket_items ORDER BY user_id;")$user_id
  if (!length(uids)) return(setNames(character(0), character(0)))
  raw    <- vapply(uids, economist_for_uid, character(1))
  final  <- raw
  counts <- integer(length(ECONOMISTS)); names(counts) <- ECONOMISTS
  sfx    <- c("II", "III", "IV", "V", "VI", "VII")
  for (i in seq_along(raw)) {
    base <- raw[[i]]
    counts[[base]] <- counts[[base]] + 1L
    if (counts[[base]] > 1L)
      final[[i]] <- paste(base, sfx[[min(counts[[base]] - 1L, length(sfx))]])
  }
  setNames(final, uids)
}

apply_anon_map <- function(df, anon_map) {
  if (!nrow(df)) { df$anon_name <- character(0); return(df) }
  df$anon_name <- anon_map[df$user_id]
  df$anon_name[is.na(df$anon_name)] <- "Unknown Economist"
  df
}

# Per-student per-period CPI from a (possibly filtered) price data frame.
# df must have: user_id, anon_name, section, price, times_per_month, time_period
# (call add_time_period() first to populate time_period from wave or recorded_at)
compute_cpi_from_df <- function(df) {
  if (!nrow(df)) return(tibble::tibble())
  if (!"time_period" %in% names(df)) df$time_period <- df$wave
  per_period <- df |>
    dplyr::group_by(user_id, anon_name, section, time_period) |>
    dplyr::summarise(basket_cost = sum(price * times_per_month, na.rm = TRUE),
                     .groups = "drop")
  # Base = each student's earliest period (CPI = 100 there)
  base <- per_period |>
    dplyr::group_by(user_id) |>
    dplyr::filter(time_period == min(time_period)) |>
    dplyr::ungroup() |>
    dplyr::select(user_id, base_cost = basket_cost)
  per_period |>
    dplyr::left_join(base, by = "user_id") |>
    dplyr::filter(!is.na(base_cost), base_cost > 0) |>
    dplyr::mutate(cpi = round(basket_cost / base_cost * 100, 1))
}

# Apply source filter only (category is now shown via color/facet in the trend plot)
filter_price_df <- function(df, src_f) {
  if (!identical(src_f, "All") && nzchar(src_f %||% ""))
    df <- df[!is.na(df$source) & df$source == src_f, , drop = FALSE]
  df
}

# Per-user, per-category, per-period CPI (base = earliest period spend in that category)
compute_cpi_by_category <- function(df) {
  if (!nrow(df)) return(tibble::tibble())
  if (!"time_period" %in% names(df)) df$time_period <- df$wave
  per_period <- df |>
    dplyr::group_by(user_id, anon_name, section, category, time_period) |>
    dplyr::summarise(cat_cost = sum(price * times_per_month, na.rm = TRUE),
                     .groups = "drop")
  base <- per_period |>
    dplyr::group_by(user_id, category) |>
    dplyr::filter(time_period == min(time_period)) |>
    dplyr::ungroup() |>
    dplyr::select(user_id, category, base_cost = cat_cost)
  per_period |>
    dplyr::left_join(base, by = c("user_id", "category")) |>
    dplyr::filter(!is.na(base_cost), base_cost > 0) |>
    dplyr::mutate(cpi = round(cat_cost / base_cost * 100, 1))
}

# Average basket share (%) per category across students, for a given price data frame
compute_basket_shares <- function(df) {
  if (!nrow(df)) return(tibble::tibble())
  df |>
    dplyr::group_by(user_id) |>
    dplyr::mutate(total_spend = sum(price * times_per_month, na.rm = TRUE)) |>
    dplyr::group_by(user_id, category) |>
    dplyr::summarise(
      cat_spend   = sum(price * times_per_month, na.rm = TRUE),
      total_spend = dplyr::first(total_spend),
      .groups     = "drop"
    ) |>
    dplyr::mutate(share = ifelse(total_spend > 0, cat_spend / total_spend * 100, 0)) |>
    dplyr::group_by(category) |>
    dplyr::summarise(avg_share = round(mean(share, na.rm = TRUE), 1), .groups = "drop")
}

# ── Time-period helpers ───────────────────────────────────────────────────────
# Adds integer `time_period` to df derived from recorded_at (or wave when unit="wave").
# Weeks are anchored to Tuesday (user preference).
add_time_period <- function(df, unit = "wave") {
  if (unit == "wave" || !"recorded_at" %in% names(df)) {
    df$time_period <- df$wave
    return(df)
  }
  dates    <- as.Date(substr(df$recorded_at, 1, 10))
  min_date <- min(dates, na.rm = TRUE)
  df$time_period <- switch(unit,
    day = as.integer(difftime(dates, min_date, units = "days")) + 1L,
    week = {
      # Roll back to the most recent Tuesday on or before min_date
      dow       <- as.integer(format(min_date, "%w"))   # 0=Sun … 6=Sat; Tue=2
      first_tue <- min_date - (dow - 2L + 7L) %% 7L
      as.integer(difftime(dates, first_tue, units = "days")) %/% 7L + 1L
    },
    month = {
      min_ym <- as.integer(format(min_date, "%Y")) * 12L +
                as.integer(format(min_date, "%m")) - 1L
      ym     <- as.integer(format(dates, "%Y")) * 12L +
                as.integer(format(dates, "%m")) - 1L
      ym - min_ym + 1L
    },
    df$wave   # fallback
  )
  df
}

# Named character vector: period_integer -> display label for scale_x_continuous
make_period_labels <- function(df, unit) {
  periods <- sort(unique(df$time_period))
  if (unit == "wave") return(setNames(paste0("Wave ", periods), as.character(periods)))
  dates    <- as.Date(substr(df$recorded_at, 1, 10))
  min_date <- min(dates, na.rm = TRUE)
  switch(unit,
    day = {
      actual <- min_date + (periods - 1L)
      setNames(format(actual, "%b %d"), as.character(periods))
    },
    week = {
      dow       <- as.integer(format(min_date, "%w"))
      first_tue <- min_date - (dow - 2L + 7L) %% 7L
      wk_starts <- first_tue + (periods - 1L) * 7L
      setNames(paste0("Wk ", periods, "\n", format(wk_starts, "%b %d")),
               as.character(periods))
    },
    month = {
      per_date <- tapply(as.character(dates), df$time_period, min)
      setNames(format(as.Date(per_date[as.character(periods)]), "%b '%y"),
               as.character(periods))
    },
    setNames(paste0("Wave ", periods), as.character(periods))   # fallback
  )
}

time_unit_xlab <- function(unit)
  switch(unit, wave = "Wave", day = "Day", week = "Week", month = "Month", "Wave")

# ── Query helpers ─────────────────────────────────────────────────────────────
get_wave       <- function() db_query("SELECT current_wave FROM app_state WHERE id=1;")$current_wave[1]
get_user_items <- function(uid) db_query(
  "SELECT * FROM basket_items WHERE user_id=? ORDER BY category, item_name;", list(uid))

get_user_prices <- function(uid, wave = NULL) {
  sql <- "SELECT pr.record_id, pr.item_id, pr.wave, pr.price, pr.recorded_at,
                 bi.item_name, bi.store, bi.category, bi.times_per_month
          FROM price_records pr
          JOIN basket_items bi ON pr.item_id = bi.item_id
          WHERE pr.user_id = ?"
  if (!is.null(wave)) {
    db_query(paste(sql, "AND pr.wave = ? ORDER BY bi.category, bi.item_name;"),
             list(uid, as.integer(wave)))
  } else {
    db_query(paste(sql, "ORDER BY pr.wave, bi.category, bi.item_name;"), list(uid))
  }
}

compute_personal_cpi <- function(uid) {
  waves <- db_query("SELECT DISTINCT wave FROM price_records WHERE user_id=? ORDER BY wave;",
                    list(uid))$wave
  if (!length(waves)) return(NULL)
  costs <- vapply(waves, function(w) {
    pr <- get_user_prices(uid, w)
    if (!nrow(pr)) return(NA_real_)
    sum(pr$price * pr$times_per_month, na.rm = TRUE)
  }, numeric(1))
  base <- costs[1]
  if (is.na(base) || base == 0) return(NULL)
  data.frame(wave = waves, basket_cost = costs,
             personal_cpi = round(costs / base * 100, 1))
}

all_personal_cpis <- function() {
  uids <- db_query("SELECT DISTINCT user_id FROM basket_items;")$user_id
  rows <- lapply(uids, function(uid) {
    cdf <- compute_personal_cpi(uid)
    if (is.null(cdf)) return(NULL)
    nm  <- db_query("SELECT display_name, section FROM users WHERE user_id=?;",
                    list(uid))
    cdf$display_name <- nm$display_name[1]
    cdf$section      <- nm$section[1]
    cdf
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

# ── UI helpers ────────────────────────────────────────────────────────────────
login_ui <- function(msg = NULL) {
  fluidPage(
    titlePanel("Personal Price Index"),
    if (!is.null(msg)) div(style = "color:#b00020;font-weight:bold;margin-bottom:10px;", msg),
    div(style = "max-width:320px;",
      textInput("login_user", "Username"),
      passwordInput("login_pw", "Password"),
      actionButton("login_btn", "Sign in", class = "btn-primary"),
      tags$p(tags$small("Use the username and password from your instructor."))
    )
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(uiOutput("app_ui"))

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    authed   = FALSE,
    user_id  = NULL,
    name     = NULL,
    is_admin = FALSE,
    tick     = 0L   # increment to force reactive refresh after writes
  )

  bump <- function() rv$tick <- rv$tick + 1L

  categories_r <- reactive({ rv$tick; get_categories()    })
  sources_r    <- reactive({ rv$tick; get_price_sources() })

  # ── Auth ────────────────────────────────────────────────────────────────────
  output$app_ui <- renderUI({
    if (!rv$authed) return(login_ui())

    tabs <- list(
      tabPanel("My Basket",    uiOutput("student_panel")),
      tabPanel("Class View",   uiOutput("dashboard_panel"))
    )
    if (rv$is_admin) tabs <- c(tabs, list(tabPanel("Admin", uiOutput("admin_panel"))))

    do.call(navbarPage,
      c(list(title = "Personal Price Index",
             header = div(style = "float:right;margin-top:6px;",
               actionButton("logout_btn", "Sign out", class = "btn-sm btn-default"))),
        tabs))
  })

  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pw %||% ""
    if (!nzchar(u) || !nzchar(p)) {
      showNotification("Enter username and password.", type = "error"); return()
    }
    row <- db_query(
      "SELECT user_id, display_name, is_admin, pw_hash FROM users WHERE user_id=?;", list(u))
    if (!nrow(row)) { showNotification("User not found.", type = "error"); return() }
    ok <- tryCatch(bcrypt::checkpw(p, row$pw_hash[1]), error = function(e) FALSE)
    if (!ok) { showNotification("Incorrect password.", type = "error"); return() }
    rv$authed   <- TRUE
    rv$user_id  <- row$user_id[1]
    rv$name     <- row$display_name[1]
    rv$is_admin <- as.logical(row$is_admin[1])
    logf("LOGIN:", row$user_id[1])
  })

  observeEvent(input$logout_btn, {
    backup_async()
    rv$authed <- FALSE; rv$user_id <- NULL; rv$name <- NULL; rv$is_admin <- FALSE
  })

  session$onSessionEnded(function() backup_async())

  # ── Reactive data ────────────────────────────────────────────────────────────
  current_wave <- reactivePoll(8000, session,
    checkFunc = function() db_query("SELECT updated_at FROM app_state WHERE id=1;")$updated_at[1],
    valueFunc = get_wave
  )

  my_items <- reactive({
    rv$tick; req(rv$user_id)
    get_user_items(rv$user_id)
  })

  my_prices <- reactive({
    rv$tick; req(rv$user_id)
    get_user_prices(rv$user_id)
  })

  all_prices_poll <- reactivePoll(8000, session,
    checkFunc = function() {
      db_query("SELECT MAX(recorded_at) ts FROM price_records;")$ts[1] %||% ""
    },
    valueFunc = function() {
      db_query("
        SELECT u.user_id, u.display_name, u.section, bi.item_name, bi.store, bi.category,
               bi.times_per_month, pr.price, pr.source, pr.wave, pr.recorded_at
        FROM price_records pr
        JOIN basket_items bi ON pr.item_id = bi.item_id
        JOIN users u ON pr.user_id = u.user_id
        ORDER BY pr.wave, u.display_name, bi.category;")
    }
  )

  # ── Student panel ────────────────────────────────────────────────────────────
  output$student_panel <- renderUI({
    req(rv$authed)
    w     <- current_wave()
    items <- my_items()

    wave_badge <- if (w == 1)
      div(class = "alert alert-info",
          tags$strong("Wave 1 — Baseline:"),
          " Add the items you buy regularly and record today's prices.")
    else
      div(class = "alert alert-warning",
          tags$strong(paste0("Wave ", w, " — Price Update:")),
          " Return to the same store and record today's price for each item.")

    tagList(
      tags$h4(paste0("Welcome, ", rv$name)),
      wave_badge,
      tags$hr(),

      # ── Entry form ──
      if (w == 1) {
        prior_names  <- sort(unique(items$item_name))
        prior_stores <- sort(unique(items$store))
        wellPanel(
          tags$h5("Add an Item"),
          tags$p(style = "color:#555;font-size:0.9em;",
            "Be specific: brand, size, store location.",
            tags$em(" E.g. '32oz Gatorade Fruit Punch, CVS Main St.' not just 'Gatorade'.")),
          fluidRow(
            column(5,
              textInput("ni_name", "Item (brand & size)",
                        placeholder = "32oz Gatorade Fruit Punch"),
              tags$datalist(id = "ni_name_list",
                lapply(prior_names, function(n) tags$option(value = n))),
              tags$script('document.getElementById("ni_name").setAttribute("list","ni_name_list")')
            ),
            column(4,
              textInput("ni_store", "Store", placeholder = "CVS Main St."),
              tags$datalist(id = "ni_store_list",
                lapply(prior_stores, function(s) tags$option(value = s))),
              tags$script('document.getElementById("ni_store").setAttribute("list","ni_store_list")')
            ),
            column(3, selectInput("ni_cat", "Category", choices = categories_r()))
          ),
          fluidRow(
            column(3, numericInput("ni_freq",  "Times/month", value = 4, min = 0.5, step = 0.5)),
            column(3, numericInput("ni_price", "Price today ($)", value = NA, min = 0, step = 0.01)),
            column(4, selectizeInput("ni_source", "Price source",
                                     choices  = sources_r(),
                                     selected = NULL,
                                     options  = list(create = TRUE, createOnBlur = TRUE,
                                                     placeholder = "Where did you check?"))),
            column(2, tags$br(),
                   actionButton("add_item_btn", "Add Item",
                                class = "btn-success", style = "margin-top:4px;"))
          )
        )
      } else {
        # Wave 2+ — price update form for all items
        already <- db_query("SELECT item_id FROM price_records WHERE user_id=? AND wave=?;",
                            list(rv$user_id, as.integer(w)))$item_id
        pending <- items[!items$item_id %in% already, , drop = FALSE]

        if (!nrow(pending)) {
          div(class = "alert alert-success",
              icon("circle-check"),
              tags$strong(paste0(" All prices submitted for Wave ", w, "!")))
        } else {
          wellPanel(
            tags$h5(paste0("Update Prices — Wave ", w)),
            tags$p(style = "color:#555;font-size:0.9em;",
                   nrow(pending), " item(s) need a price. Go to the same store as Wave 1."),
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Item"), tags$th("Store"), tags$th("Category"),
                tags$th("New Price ($)"), tags$th("Source"), tags$th("")
              )),
              tags$tbody(
                lapply(seq_len(nrow(pending)), function(i) {
                  r <- pending[i, ]
                  tags$tr(
                    tags$td(tags$strong(r$item_name)),
                    tags$td(r$store),
                    tags$td(tags$span(class="badge bg-secondary", r$category)),
                    tags$td(numericInput(paste0("upd_", r$item_id), NULL,
                                        value = NA, min = 0, step = 0.01, width = "110px")),
                    tags$td(selectizeInput(paste0("upd_src_", r$item_id), NULL,
                                          choices  = sources_r(),
                                          selected = NULL,
                                          options  = list(create = TRUE, createOnBlur = TRUE,
                                                          placeholder = "Source"),
                                          width = "180px")),
                    tags$td(actionButton(paste0("upd_btn_", r$item_id), "Save",
                                        class = "btn-sm btn-primary"))
                  )
                })
              )
            )
          )
        }
      },

      tags$hr(),
      tags$h5("Your Basket"),
      DT::DTOutput("my_basket_tbl"),
      tags$hr(),
      uiOutput("personal_cpi_ui"),
      tags$hr(),
      downloadButton("dl_my_data", "Download my data (.csv)", class = "btn-sm btn-default")
    )
  })

  # ── Add item ────────────────────────────────────────────────────────────────
  observeEvent(input$add_item_btn, {
    uid <- req(rv$user_id)
    nm  <- trimws(input$ni_name   %||% "")
    st  <- trimws(input$ni_store  %||% "")
    cat <- input$ni_cat    %||% categories_r()[1]
    fr  <- as.numeric(input$ni_freq)
    pr  <- as.numeric(input$ni_price)
    src <- trimws(input$ni_source %||% "")

    if (!nzchar(nm))          { showNotification("Item name required.", type="error"); return() }
    if (!nzchar(st))          { showNotification("Store required.",     type="error"); return() }
    if (is.na(fr) || fr <= 0) { showNotification("Times/month must be > 0.", type="error"); return() }
    if (is.na(pr) || pr < 0)  { showNotification("Enter a valid price.", type="error"); return() }

    tryCatch({
      db_exec(
        "INSERT INTO basket_items(user_id, item_name, store, category, times_per_month)
         VALUES(?,?,?,?,?)
         ON CONFLICT(user_id, item_name, store) DO NOTHING;",
        list(uid, nm, st, cat, fr))

      iid <- db_query(
        "SELECT item_id FROM basket_items WHERE user_id=? AND item_name=? AND store=?;",
        list(uid, nm, st))$item_id[1]

      db_exec(
        "INSERT INTO price_records(item_id, user_id, price, source, wave)
         VALUES(?,?,?,?,1)
         ON CONFLICT(item_id, wave) DO UPDATE
           SET price=excluded.price, source=excluded.source, recorded_at=CURRENT_TIMESTAMP;",
        list(iid, uid, pr, if (nzchar(src)) src else NA_character_))

      updateTextInput(session, "ni_name",  value = "")
      updateTextInput(session, "ni_store", value = "")
      updateSelectizeInput(session, "ni_source", selected = character(0))
      updateNumericInput(session, "ni_price", value = NA)
      showNotification(paste0('"', nm, '" added.'), type = "message")
      bump()
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
  })

  # ── Wave 2+ price updates — dynamic observers ────────────────────────────────
  # Observers are cheap and idempotent; re-register on items change is fine here.
  observeEvent(my_items(), {
    items <- my_items()
    w     <- isolate(current_wave())
    uid   <- isolate(rv$user_id)
    lapply(items$item_id, function(iid) {
      btn_id <- paste0("upd_btn_", iid)
      observeEvent(input[[btn_id]], ignoreInit = TRUE, {
        pr <- as.numeric(input[[paste0("upd_", iid)]])
        if (is.na(pr) || pr < 0) {
          showNotification("Enter a valid price.", type = "error"); return()
        }
        wv <- isolate(current_wave())
        src <- trimws(input[[paste0("upd_src_", iid)]] %||% "")
        db_exec(
          "INSERT INTO price_records(item_id, user_id, price, source, wave)
           VALUES(?,?,?,?,?)
           ON CONFLICT(item_id, wave) DO UPDATE
             SET price=excluded.price, source=excluded.source, recorded_at=CURRENT_TIMESTAMP;",
          list(iid, uid, pr, if (nzchar(src)) src else NA_character_, as.integer(wv)))
        nm <- items$item_name[items$item_id == iid]
        showNotification(paste0('"', nm, '" saved for Wave ', wv, '.'), type = "message")
        bump()
      })
    })
  })

  # ── My basket table ─────────────────────────────────────────────────────────
  output$my_basket_tbl <- DT::renderDT({
    req(rv$user_id); rv$tick
    pr <- my_prices()
    if (!nrow(pr)) return(data.frame(Message = "No items yet — add them above."))

    wide <- pr |>
      dplyr::select(category, item_name, store, times_per_month, wave, price) |>
      dplyr::mutate(wave = paste0("Wave ", wave)) |>
      tidyr::pivot_wider(names_from = wave, values_from = price) |>
      dplyr::rename(Category = category, Item = item_name,
                    Store = store, `Times/mo` = times_per_month) |>
      dplyr::arrange(Category, Item)

    DT::datatable(wide, rownames = FALSE, options = list(dom = "t", pageLength = 25)) |>
      DT::formatCurrency(grep("^Wave", names(wide), value = TRUE), digits = 2)
  })

  # ── Personal CPI ────────────────────────────────────────────────────────────
  output$personal_cpi_ui <- renderUI({
    req(rv$user_id); rv$tick
    cpi_df <- compute_personal_cpi(rv$user_id)
    tags$div(
      tags$h5("Your Personal CPI"),
      if (is.null(cpi_df) || nrow(cpi_df) < 2) {
        tags$p(style="color:#888;", "Will appear after you submit Wave 2 prices.")
      } else {
        latest  <- cpi_df$personal_cpi[nrow(cpi_df)]
        delta   <- round(latest - 100, 1)
        col     <- if (delta > 0) "#b00020" else if (delta < 0) "#2d6a4f" else "#333"
        dir_txt <- if (delta > 0) "up" else if (delta < 0) "down" else "unchanged"
        tagList(
          tags$table(class = "table table-sm table-bordered",
            style = "max-width:360px;",
            tags$thead(tags$tr(tags$th("Wave"), tags$th("Basket Cost"), tags$th("CPI"))),
            tags$tbody(lapply(seq_len(nrow(cpi_df)), function(i) {
              r <- cpi_df[i, ]
              tags$tr(
                tags$td(paste("Wave", r$wave)),
                tags$td(sprintf("$%.2f", r$basket_cost)),
                tags$td(tags$strong(sprintf("%.1f", r$personal_cpi)))
              )
            }))
          ),
          tags$p(style = paste0("color:", col, ";font-weight:bold;"),
                 sprintf("Your basket is %s %.1f%% since Wave 1.", dir_txt, abs(delta)))
        )
      }
    )
  })

  # ── Class dashboard ──────────────────────────────────────────────────────────
  anon_map <- reactive({
    rv$tick
    make_anon_map()
  })

  output$dashboard_panel <- renderUI({
    req(rv$authed)
    amap       <- anon_map()
    my_alias   <- if (!is.null(rv$user_id) && rv$user_id %in% names(amap))
                    amap[[rv$user_id]] else NULL
    all_aliases <- sort(unique(unname(amap)))

    tagList(
      tags$h4("Class Price Index — Live Dashboard"),

      if (!is.null(my_alias))
        div(class = "alert alert-info",
          tags$strong(paste0("Your economist alias: ", my_alias)),
          " — your data appears under this name in the class plots below. ",
          tags$em("(This is a fake name for anonymization. Only you and your instructor know which alias is yours.)")
        ),

      wellPanel(
        tags$h5("Plot Controls"),
        # ── Global time-axis selector (affects all three plots) ──────────────
        fluidRow(
          column(12,
            tags$strong("Time Axis"), tags$hr(),
            radioButtons("time_unit", NULL,
              choices = c("Wave (manual)"          = "wave",
                          "Week (auto, Tue start)" = "week",
                          "Day"                    = "day",
                          "Month"                  = "month"),
              selected = "week",
              inline   = TRUE),
            tags$p(style = "font-size:0.82em;color:#666;margin-top:-6px;",
                   tags$strong("Wave"), " uses the instructor-set wave number. ",
                   tags$strong("Week/Day/Month"), " derive automatically from each ",
                   "submission\u2019s timestamp (weeks start on Tuesday).")
          )
        ),
        tags$hr(),
        fluidRow(
          # ── Category count chart controls ───────────────────────────────────
          column(12,
            tags$strong("Category Chart (bottom)"), tags$hr(),
            fluidRow(
              column(4,
                radioButtons("plot3_metric", "Show",
                  choices  = c("Item count"         = "count",
                               "Avg. basket share %" = "share"),
                  selected = "count",
                  inline   = TRUE)
              ),
              column(4,
                selectInput("plot3_period", "Period",
                  choices  = c("Period 1 (baseline)" = "1", "All periods" = "all"),
                  selected = "1")
              ),
              column(4,
                selectInput("plot3_facet", "Facet by",
                  choices  = c("None" = "none", "Section" = "section",
                               "Source" = "source", "Period" = "wave"),
                  selected = "none")
              )
            )
          )
        ),
        tags$hr(),
        fluidRow(
          # ── Left plot controls ──────────────────────────────────────────────
          column(6,
            tags$strong("Trend Plot (left)"), tags$hr(),
            fluidRow(
              column(6,
                selectInput("plot1_cat_mode", "Show categories",
                  choices  = c("Total basket"       = "all",
                               "Color by category"  = "color",
                               "Facet by category"  = "facet"),
                  selected = "all")
              ),
              column(6,
                selectInput("plot1_src", "Filter by source",
                  choices  = c("All sources" = "All",
                               setNames(sources_r(), sources_r())),
                  selected = "All")
              )
            ),
            radioButtons("plot1_group", "Group by",
              choices = c("Class average" = "average",
                          "Each student"  = "student",
                          "By section"    = "section"),
              inline  = TRUE),
            conditionalPanel(
              condition = "input.plot1_group == 'student'",
              selectizeInput("plot1_students",
                "Students to show (blank = all)",
                choices  = all_aliases,
                multiple = TRUE,
                options  = list(placeholder = "All students shown when blank"))
            )
          ),
          # ── Right plot controls ─────────────────────────────────────────────
          column(6,
            tags$strong("CPI at Latest Wave — by Student (right)"), tags$hr(),
            fluidRow(
              column(6,
                selectInput("plot2_color", "Fill by",
                  choices  = c("None"     = "none",
                               "Section"  = "section",
                               "Category" = "category"),
                  selected = "none")
              ),
              column(6,
                tags$br(),
                checkboxInput("plot2_avg", "Show mean", value = TRUE)
              )
            )
          )
        )
      ),

      fluidRow(
        column(6,
          tags$h6("CPI Trends Over Waves"),
          plotOutput("cpi_lines_plot", height = "340px")),
        column(6,
          tags$h6("CPI at Latest Wave"),
          plotOutput("cpi_latest_plot", height = "480px"))
      ),

      tags$hr(),
      tags$h6("Items Tracked by Category"),
      plotOutput("cat_count_plot", height = "280px"),

      tags$hr(),
      tags$details(
        tags$summary(
          style = "cursor:pointer; font-weight:600; font-size:1.05em; padding:4px 0;",
          "All Price Submissions — click to expand"
        ),
        tags$br(),
        downloadButton("dl_class_view", "Download class data (anonymized, .csv)",
                       class = "btn-sm btn-default"),
        tags$br(), tags$br(),
        DT::DTOutput("all_prices_tbl")
      )
    )
  })

  # Blank-plot helper reused by both renderers
  void_plot <- function(msg)
    ggplot() +
      annotate("text", x = .5, y = .5, label = msg, size = 5, color = "gray55") +
      theme_void()

  output$cpi_lines_plot <- renderPlot({
    df   <- all_prices_poll()
    amap <- anon_map()
    unit <- input$time_unit %||% "wave"
    if (!nrow(df)) return(void_plot("No data yet"))

    df      <- apply_anon_map(df, amap)
    df      <- filter_price_df(df, input$plot1_src %||% "All")
    df      <- add_time_period(df, unit)
    if (!nrow(df)) return(void_plot("No data for this filter"))

    cat_mode <- input$plot1_cat_mode %||% "all"
    grp      <- input$plot1_group    %||% "average"

    # ── Build base CPI data (total basket or per category) ─────────────────
    if (cat_mode == "all") {
      cpi_base <- compute_cpi_from_df(df)
      if (!nrow(cpi_base)) return(void_plot("CPI needs baseline data"))

      plot_df <- if (grp == "average") {
        cpi_base |>
          dplyr::group_by(time_period) |>
          dplyr::summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(group_label = "Class Average", cat_label = "Total basket")
      } else if (grp == "section") {
        cpi_base |>
          dplyr::mutate(grp_val = dplyr::if_else(
            !is.na(section) & nzchar(section), section, "Unknown")) |>
          dplyr::group_by(time_period, group_label = grp_val) |>
          dplyr::summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(cat_label = "Total basket")
      } else {
        sel <- input$plot1_students
        if (length(sel) > 0) cpi_base <- cpi_base[cpi_base$anon_name %in% sel, ]
        dplyr::rename(cpi_base, group_label = anon_name) |>
          dplyr::mutate(cat_label = "Total basket")
      }
      color_col <- "group_label"
      facet_col <- NULL

    } else {
      # Category-level CPI
      cpi_cat <- compute_cpi_by_category(df)
      if (!nrow(cpi_cat)) return(void_plot("CPI needs baseline data"))

      plot_df <- if (grp == "average") {
        cpi_cat |>
          dplyr::group_by(time_period, category) |>
          dplyr::summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(group_label = category, cat_label = category)
      } else if (grp == "section") {
        cpi_cat |>
          dplyr::mutate(grp_val = dplyr::if_else(
            !is.na(section) & nzchar(section), section, "Unknown")) |>
          dplyr::group_by(time_period, category, group_label = grp_val) |>
          dplyr::summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(cat_label = category)
      } else {
        sel <- input$plot1_students
        if (length(sel) > 0) cpi_cat <- cpi_cat[cpi_cat$anon_name %in% sel, ]
        dplyr::rename(cpi_cat, group_label = anon_name) |>
          dplyr::mutate(cat_label = category)
      }
      color_col <- "cat_label"
      facet_col <- if (cat_mode == "facet") "cat_label" else NULL
    }

    if (!nrow(plot_df)) return(void_plot("No data for this selection"))

    per_labels  <- make_period_labels(df, unit)
    plot_breaks <- sort(unique(plot_df$time_period))
    base_lbl    <- switch(unit, wave="Wave 1", day="Day 1", week="Week 1", month="Month 1", "Period 1")

    p <- ggplot(plot_df, aes(x = time_period, y = cpi,
                             group = interaction(group_label, cat_label))) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "gray60", linewidth = 0.7)

    n_color <- dplyr::n_distinct(plot_df[[color_col]])
    if (n_color > 1) {
      p <- p +
        geom_line(aes(color  = .data[[color_col]]), linewidth = 1.1) +
        geom_point(aes(color = .data[[color_col]]), size = 3) +
        labs(color = NULL) +
        theme(legend.position = if (n_color <= 14) "right" else "none")
    } else {
      p <- p +
        geom_line(color = "#951829", linewidth = 1.1) +
        geom_point(color = "#951829", size = 3)
    }

    if (!is.null(facet_col))
      p <- p + facet_wrap(~ .data[[facet_col]], scales = "free_y")

    p +
      scale_x_continuous(
        breaks = plot_breaks,
        labels = unname(per_labels[as.character(plot_breaks)])
      ) +
      labs(x = time_unit_xlab(unit), y = paste0("CPI (", base_lbl, " = 100)")) +
      theme_minimal(base_size = 12)
  })

  output$cpi_latest_plot <- renderPlot({
    df   <- all_prices_poll()
    amap <- anon_map()
    unit <- input$time_unit %||% "wave"
    if (!nrow(df)) return(void_plot("No data yet"))

    df     <- apply_anon_map(df, amap)
    df     <- add_time_period(df, unit)
    cpi_df <- compute_cpi_from_df(df)
    if (!nrow(cpi_df)) return(void_plot("CPI needs baseline data"))

    latest_period <- max(cpi_df$time_period)
    latest        <- dplyr::filter(cpi_df, time_period == latest_period)
    if (!nrow(latest)) return(void_plot("No data at latest period"))

    per_labels   <- make_period_labels(df, unit)
    latest_label <- gsub("\n", " ", unname(per_labels[as.character(latest_period)]))
    base_label   <- gsub("\n", " ", unname(per_labels["1"]))
    if (!length(latest_label) || is.na(latest_label))
      latest_label <- paste0(time_unit_xlab(unit), " ", latest_period)
    if (!length(base_label) || is.na(base_label))
      base_label <- paste0(time_unit_xlab(unit), " 1")

    color_by <- input$plot2_color %||% "none"

    # ── Category view: class-average CPI per category at latest period ────────
    if (color_by == "category") {
      cpi_cat <- compute_cpi_by_category(df)
      if (!nrow(cpi_cat)) return(void_plot("CPI needs baseline data"))
      latest_cat <- dplyr::filter(cpi_cat, time_period == max(cpi_cat$time_period))
      if (!nrow(latest_cat)) return(void_plot("No category data at latest period"))

      avg_cat <- latest_cat |>
        dplyr::group_by(category) |>
        dplyr::summarise(cpi      = round(mean(cpi, na.rm = TRUE), 1),
                         n_students = dplyr::n(),
                         .groups  = "drop") |>
        dplyr::mutate(category = forcats::fct_reorder(category, cpi))

      avg_cpi_all <- mean(latest$cpi, na.rm = TRUE)

      p <- ggplot(avg_cat, aes(x = cpi, y = category, fill = category)) +
        geom_col(alpha = 0.85, show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.1f  (n=%d)", cpi, n_students)),
                  hjust = -0.1, size = 3.3) +
        geom_vline(xintercept = 100, linetype = "dashed",
                   color = "gray55", linewidth = 0.8) +
        scale_fill_viridis_d(option = "D", direction = -1) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
        labs(x = paste0("Avg. category CPI (", base_label, " = 100)"), y = NULL,
             subtitle = paste0("Class average by category — ", latest_label,
                               " vs. ", base_label)) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.major.y = element_blank())

      if (isTRUE(input$plot2_avg))
        p <- p +
          geom_vline(xintercept = avg_cpi_all, color = "#1a1a2e",
                     linewidth = 1.1, linetype = "solid") +
          annotate("text", x = avg_cpi_all, y = Inf,
                   label = sprintf("Overall mean: %.1f", avg_cpi_all),
                   hjust = -0.1, vjust = 1.5, size = 3.3, color = "#1a1a2e")
      return(p)
    }

    # ── Per-student view ──────────────────────────────────────────────────────
    if (color_by == "section") {
      # Vectorised: avoid passing a column to %||%
      latest$fill_var <- ifelse(!is.na(latest$section) & nzchar(latest$section),
                                latest$section, "Unknown")
    } else {
      latest$fill_var <- "All students"
    }

    n_fills <- dplyr::n_distinct(latest$fill_var)
    avg_cpi <- mean(latest$cpi, na.rm = TRUE)

    latest <- dplyr::mutate(latest,
      anon_name = forcats::fct_reorder(anon_name, cpi))

    p <- ggplot(latest, aes(x = cpi, y = anon_name))

    p <- if (n_fills > 1) {
      p + geom_col(aes(fill = fill_var), alpha = 0.85) +
        scale_fill_brewer(palette = "Set2", name = "Section")
    } else {
      p + geom_col(fill = "#951829", alpha = 0.85)
    }

    p <- p +
      geom_vline(xintercept = 100, linetype = "dashed",
                 color = "gray55", linewidth = 0.8)

    if (isTRUE(input$plot2_avg))
      p <- p +
        geom_vline(xintercept = avg_cpi, color = "#1a1a2e",
                   linewidth = 1.1, linetype = "solid") +
        annotate("text", x = avg_cpi, y = Inf,
                 label = sprintf("Mean: %.1f", avg_cpi),
                 hjust = -0.15, vjust = 1.5, size = 3.5, color = "#1a1a2e")

    p +
      labs(x = paste0("Personal CPI (", base_label, " = 100)"), y = NULL,
           subtitle = paste0(latest_label, " vs. ", base_label)) +
      theme_minimal(base_size = 12) +
      theme(legend.position = if (n_fills > 1) "bottom" else "none",
            panel.grid.major.y = element_blank())
  })

  output$cat_count_plot <- renderPlot({
    df <- all_prices_poll()
    if (!nrow(df)) return(void_plot("No data yet"))

    unit       <- input$time_unit    %||% "wave"
    period_sel <- input$plot3_period %||% "1"
    facet_by   <- input$plot3_facet  %||% "none"
    metric     <- input$plot3_metric %||% "count"

    df         <- add_time_period(df, unit)
    df_plot    <- if (identical(period_sel, "1")) dplyr::filter(df, time_period == 1) else df
    if (!nrow(df_plot)) return(void_plot("No data for selected period"))

    per_labels <- make_period_labels(df, unit)

    # Normalise facet column
    if (facet_by == "section") {
      df_plot$facet_var <- ifelse(!is.na(df_plot$section) & nzchar(df_plot$section),
                                  df_plot$section, "Unknown")
    } else if (facet_by == "source") {
      df_plot$facet_var <- ifelse(!is.na(df_plot$source) & nzchar(df_plot$source),
                                  df_plot$source, "Unknown")
    } else if (facet_by == "wave") {
      df_plot$facet_var <- unname(per_labels[as.character(df_plot$time_period)])
      df_plot$facet_var[is.na(df_plot$facet_var)] <- paste0(time_unit_xlab(unit), " ?")
    }

    period_1_lbl <- gsub("\n", " ", unname(per_labels["1"]) %||% paste0(time_unit_xlab(unit), " 1"))
    period_lbl   <- if (identical(period_sel, "1")) period_1_lbl else paste0("All ", time_unit_xlab(unit), "s")

    if (metric == "share") {
      # Average % of basket spend per category, across students
      # Period 1: use as-is; all periods: use most recent price per item
      share_base <- if (identical(period_sel, "1")) {
        df_plot
      } else {
        df_plot |>
          dplyr::arrange(time_period) |>
          dplyr::group_by(user_id, item_name, store) |>
          dplyr::slice_tail(n = 1) |>
          dplyr::ungroup()
      }

      if (facet_by == "none") {
        plot_df <- compute_basket_shares(share_base) |>
          dplyr::mutate(category = forcats::fct_reorder(category, avg_share))

        p <- ggplot(plot_df, aes(x = avg_share, y = category)) +
          geom_col(fill = "#2166ac", alpha = 0.85) +
          geom_text(aes(label = paste0(round(avg_share, 1), "%")),
                    hjust = -0.15, size = 3.5) +
          scale_x_continuous(expand = expansion(mult = c(0, 0.2)),
                             labels = function(x) paste0(x, "%")) +
          labs(x = "Average share of monthly basket spend",
               y = NULL, subtitle = period_lbl) +
          theme_minimal(base_size = 12) +
          theme(panel.grid.major.y = element_blank())
      } else {
        plot_df <- share_base |>
          dplyr::group_by(facet_var) |>
          dplyr::group_modify(~ compute_basket_shares(.x)) |>
          dplyr::ungroup() |>
          dplyr::mutate(category = forcats::fct_reorder(category, avg_share, .fun = mean))

        p <- ggplot(plot_df, aes(x = avg_share, y = category, fill = facet_var)) +
          geom_col(alpha = 0.85, show.legend = FALSE) +
          geom_text(aes(label = paste0(round(avg_share, 1), "%")),
                    hjust = -0.15, size = 3.2) +
          scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                             labels = function(x) paste0(x, "%")) +
          scale_fill_brewer(palette = "Set2") +
          facet_wrap(~ facet_var, scales = "free_x") +
          labs(x = "Average share of monthly basket spend",
               y = NULL, subtitle = period_lbl) +
          theme_minimal(base_size = 12) +
          theme(panel.grid.major.y = element_blank())
      }
      return(p)
    }

    # ── Count metric (original behavior) ────────────────────────────────────
    count_df <- if (facet_by == "none") {
      df_plot |>
        dplyr::count(category) |>
        dplyr::mutate(category = forcats::fct_reorder(category, n))
    } else {
      df_plot |>
        dplyr::count(category, facet_var) |>
        dplyr::mutate(category = forcats::fct_reorder(category, n, .fun = sum))
    }

    p <- ggplot(count_df, aes(x = n, y = category)) +
      geom_col(fill = "#951829", alpha = 0.85) +
      geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Number of items", y = NULL, subtitle = period_lbl) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank())

    if (facet_by != "none")
      p <- p + facet_wrap(~ facet_var, scales = "free_x") +
        geom_col(aes(fill = facet_var), alpha = 0.85, show.legend = FALSE) +
        scale_fill_brewer(palette = "Set2")

    p
  })

  output$all_prices_tbl <- DT::renderDT({
    df   <- all_prices_poll()
    amap <- anon_map()
    if (!nrow(df)) return(data.frame(Message = "No submissions yet."))
    df |>
      apply_anon_map(amap) |>
      dplyr::mutate(recorded_at = substr(recorded_at, 1, 16)) |>
      dplyr::select(Economist = anon_name, Section = section, Wave = wave,
                    Category = category, Item = item_name, Store = store,
                    `Times/mo` = times_per_month, Price = price,
                    Source = source, Submitted = recorded_at) |>
      dplyr::arrange(Wave, Economist, Category)
  }, rownames = FALSE,
     options  = list(pageLength = 25,
                     order = list(list(2L, "asc"), list(0L, "asc"))))

  # ── Admin panel ──────────────────────────────────────────────────────────────
  output$admin_panel <- renderUI({
    req(rv$is_admin)
    cats <- categories_r()
    srcs <- sources_r()

    tagList(
      tags$h4("Admin Controls"),
      fluidRow(
        column(4, wellPanel(
          tags$h6("Current Wave"),
          numericInput("admin_wave", NULL, value = isolate(current_wave()),
                       min = 1, max = 10, step = 1, width = "100px"),
          actionButton("set_wave_btn", "Set Wave", class = "btn-warning"),
          tags$p(style="font-size:0.85em;color:#666;margin-top:6px;",
                 "Wave 1 = baseline entry. Wave 2, 3, … = price updates.")
        )),
        column(4, wellPanel(
          tags$h6("Export"),
          downloadButton("dl_all", "Download all data (.csv)"),
          tags$br(), tags$br(),
          actionButton("backup_btn", "Backup DB to Drive", class = "btn-sm btn-default")
        ))
      ),

      fluidRow(
        column(6, wellPanel(
          tags$h6("Item Categories"),
          tags$p(style = "font-size:0.85em;color:#555;",
                 "These appear in the student basket form and all category filters. ",
                 "Type a new entry and press Enter or Tab to add it; click \u00d7 to remove."),
          selectizeInput("admin_cats", NULL,
            choices  = cats,
            selected = cats,
            multiple = TRUE,
            options  = list(create        = TRUE,
                            createOnBlur  = TRUE,
                            plugins       = list("remove_button"),
                            placeholder   = "Add a category…")
          ),
          fluidRow(
            column(6, actionButton("save_cats",  "Save",             class = "btn-primary btn-sm")),
            column(6, actionButton("reset_cats", "Reset to defaults", class = "btn-default btn-sm"))
          )
        )),

        column(6, wellPanel(
          tags$h6("Price Sources"),
          tags$p(style = "font-size:0.85em;color:#555;",
                 "These appear in the source dropdown when students enter prices. ",
                 "Students can still type a custom source not on this list."),
          selectizeInput("admin_srcs", NULL,
            choices  = srcs,
            selected = srcs,
            multiple = TRUE,
            options  = list(create        = TRUE,
                            createOnBlur  = TRUE,
                            plugins       = list("remove_button"),
                            placeholder   = "Add a source…")
          ),
          fluidRow(
            column(6, actionButton("save_srcs",  "Save",             class = "btn-primary btn-sm")),
            column(6, actionButton("reset_srcs", "Reset to defaults", class = "btn-default btn-sm"))
          )
        ))
      ),

      tags$hr(),
      tags$h6("Assign Student Sections"),
      tags$p(style = "font-size:0.85em;color:#555;",
             "Set the section for each student. Section is used for color/facet grouping in class plots."),
      DT::DTOutput("admin_sections_tbl"),
      tags$br(),
      fluidRow(
        column(3, selectInput("sec_user",    "Student", choices = NULL)),
        column(3, textInput("sec_value",     "Section (e.g. MWF 10am)", placeholder = "Section label")),
        column(2, tags$br(), actionButton("sec_save_btn", "Save Section", class = "btn-sm btn-primary"))
      ),
      tags$hr(),
      tags$h6("Basket Items"),
      DT::DTOutput("admin_items_tbl"),
      tags$hr(),
      tags$h6("Price Records"),
      DT::DTOutput("admin_prices_tbl")
    )
  })

  observeEvent(input$set_wave_btn, {
    req(rv$is_admin)
    w <- as.integer(input$admin_wave)
    db_exec("UPDATE app_state SET current_wave=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;", list(w))
    showNotification(paste0("Wave set to ", w, "."), type = "message")
    logf("ADMIN wave set to", w)
  })

  observeEvent(input$save_cats, {
    req(rv$is_admin)
    cats <- input$admin_cats
    if (!length(cats)) { showNotification("Categories cannot be empty.", type = "error"); return() }
    save_categories(cats)
    bump()
    showNotification("Categories saved.", type = "message")
  })

  observeEvent(input$reset_cats, {
    req(rv$is_admin)
    save_categories(BLS_CATEGORIES)
    bump()
    showNotification("Categories reset to defaults.", type = "message")
  })

  observeEvent(input$save_srcs, {
    req(rv$is_admin)
    srcs <- input$admin_srcs
    if (!length(srcs)) { showNotification("Sources cannot be empty.", type = "error"); return() }
    save_price_sources(srcs)
    bump()
    showNotification("Price sources saved.", type = "message")
  })

  observeEvent(input$reset_srcs, {
    req(rv$is_admin)
    save_price_sources(PRICE_SOURCES)
    bump()
    showNotification("Price sources reset to defaults.", type = "message")
  })

  observeEvent(input$backup_btn, {
    req(rv$is_admin)
    showNotification("Backup started…", type = "message")
    backup_async()
  })

  # ── Section assignment ───────────────────────────────────────────────────────
  output$admin_sections_tbl <- DT::renderDT({
    req(rv$is_admin); rv$tick
    db_query("SELECT user_id AS Username, display_name AS Name,
                     COALESCE(section, '(not set)') AS Section
              FROM users ORDER BY display_name;")
  }, rownames = FALSE, selection = "none",
     options  = list(dom = "t", pageLength = 50))

  # Populate student picker from DB
  observe({
    req(rv$is_admin); rv$tick
    users <- db_query("SELECT user_id, display_name FROM users ORDER BY display_name;")
    if (!nrow(users)) return()
    updateSelectInput(session, "sec_user",
      choices = setNames(users$user_id, users$display_name))
  })

  observeEvent(input$sec_save_btn, {
    req(rv$is_admin)
    uid <- input$sec_user %||% ""
    sec <- trimws(input$sec_value %||% "")
    if (!nzchar(uid)) { showNotification("Select a student.", type = "error"); return() }
    db_exec("UPDATE users SET section=? WHERE user_id=?;",
            list(if (nzchar(sec)) sec else NA_character_, uid))
    bump()
    showNotification(paste0("Section saved for ", uid, "."), type = "message")
  })

  output$admin_items_tbl <- DT::renderDT({
    req(rv$is_admin); rv$tick
    db_query("SELECT u.display_name AS Student, u.section AS Section,
                     bi.category AS Category, bi.item_name AS Item,
                     bi.store AS Store, bi.times_per_month AS 'Times/mo',
                     bi.created_at AS Added
              FROM basket_items bi JOIN users u ON bi.user_id = u.user_id
              ORDER BY u.display_name, bi.category;")
  }, rownames = FALSE, options = list(pageLength = 30))

  output$admin_prices_tbl <- DT::renderDT({
    req(rv$is_admin)
    db_query("SELECT u.display_name AS Student, u.section AS Section,
                     pr.wave AS Wave, bi.item_name AS Item, bi.category AS Category,
                     pr.price AS Price, pr.source AS Source, pr.recorded_at AS Submitted
              FROM price_records pr
              JOIN basket_items bi ON pr.item_id = bi.item_id
              JOIN users u ON pr.user_id = u.user_id
              ORDER BY pr.wave, u.display_name;")
  }, rownames = FALSE, options = list(pageLength = 30))

  output$dl_all <- downloadHandler(
    filename = function() paste0("price_index_export_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(db_query("
        SELECT u.display_name AS student, u.section,
               bi.item_name, bi.store, bi.category, bi.times_per_month,
               pr.price, pr.wave, pr.recorded_at
        FROM price_records pr
        JOIN basket_items bi ON pr.item_id = bi.item_id
        JOIN users u         ON pr.user_id = u.user_id
        ORDER BY pr.wave, u.display_name;"), file, row.names = FALSE)
    }
  )

  # ── Class view download (economist aliases, no real names) ───────────────────
  output$dl_class_view <- downloadHandler(
    filename = function() paste0("class_price_index_", Sys.Date(), ".csv"),
    content  = function(file) {
      df   <- all_prices_poll()
      amap <- make_anon_map()
      out  <- apply_anon_map(df, amap) |>
        dplyr::mutate(recorded_at = substr(recorded_at, 1, 16)) |>
        dplyr::select(economist = anon_name, section, wave, category,
                      item_name, store, times_per_month, price, source, recorded_at) |>
        dplyr::arrange(wave, economist, category)
      write.csv(out, file, row.names = FALSE)
    }
  )

  # ── Student's own data download ───────────────────────────────────────────────
  output$dl_my_data <- downloadHandler(
    filename = function() paste0("my_price_index_", Sys.Date(), ".csv"),
    content  = function(file) {
      uid    <- rv$user_id
      prices <- get_user_prices(uid)
      cpi_df <- compute_personal_cpi(uid)
      out <- prices |>
        dplyr::select(wave, category, item_name, store,
                      times_per_month, price, source, recorded_at) |>
        dplyr::arrange(wave, category, item_name)
      if (!is.null(cpi_df) && nrow(cpi_df))
        out <- dplyr::left_join(out,
          dplyr::select(cpi_df, wave, basket_cost, personal_cpi),
          by = "wave")
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
