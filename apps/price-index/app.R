try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
# app.R — Personal Price Index Activity
# Students build a basket of goods and track prices across waves.
# Auth: bcrypt + SQLite — shares finalqdata.sqlite with final_question_reveal.
# Users (including passwords) come from that shared DB; no separate credentials file needed.
# DB backed up to Google Drive on session end (same FLEX_PASS_FOLDER_ID as final_question_reveal).

library(shiny); library(DT); library(bcrypt); library(dplyr); library(tidyr)
library(tibble); library(forcats); library(DBI); library(RSQLite); library(ggplot2)
library(googledrive); library(promises); library(future)

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

# Per-student per-wave CPI from a (possibly filtered) price data frame.
# df must have: user_id, anon_name, section, price, times_per_month, wave
compute_cpi_from_df <- function(df) {
  if (!nrow(df)) return(tibble::tibble())
  per_wave <- df |>
    dplyr::group_by(user_id, anon_name, section, wave) |>
    dplyr::summarise(basket_cost = sum(price * times_per_month, na.rm = TRUE),
                     .groups = "drop")
  base <- per_wave |>
    dplyr::filter(wave == 1) |>
    dplyr::select(user_id, base_cost = basket_cost)
  per_wave |>
    dplyr::left_join(base, by = "user_id") |>
    dplyr::filter(!is.na(base_cost), base_cost > 0) |>
    dplyr::mutate(cpi = round(basket_cost / base_cost * 100, 1))
}

# Apply category + source filters
filter_price_df <- function(df, cat_f, src_f) {
  if (!identical(cat_f, "All") && nzchar(cat_f %||% ""))
    df <- df[!is.na(df$category) & df$category == cat_f, , drop = FALSE]
  if (!identical(src_f, "All") && nzchar(src_f %||% ""))
    df <- df[!is.na(df$source)   & df$source   == src_f, , drop = FALSE]
  df
}

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
        fluidRow(
          # ── Category count chart controls ───────────────────────────────────
          column(12,
            tags$strong("Category Count Chart (bottom)"), tags$hr(),
            fluidRow(
              column(4,
                selectInput("plot3_wave", "Wave",
                  choices  = c("Wave 1 (baseline)" = "1", "All waves" = "all"),
                  selected = "1")
              ),
              column(4,
                selectInput("plot3_facet", "Facet by",
                  choices  = c("None" = "none", "Section" = "section",
                               "Source" = "source", "Wave" = "wave"),
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
                selectInput("plot1_cat", "Filter by category",
                  choices  = c("All categories" = "All",
                               setNames(categories_r(), categories_r())),
                  selected = "All")
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
            tags$strong("Latest Wave Plot (right)"), tags$hr(),
            fluidRow(
              column(6,
                selectInput("plot2_cat", "Filter by category",
                  choices  = c("All categories" = "All",
                               setNames(categories_r(), categories_r())),
                  selected = "All")
              ),
              column(6,
                selectInput("plot2_src", "Filter by source",
                  choices  = c("All sources" = "All",
                               setNames(sources_r(), sources_r())),
                  selected = "All")
              )
            ),
            fluidRow(
              column(6,
                selectInput("plot2_color", "Color by",
                  choices  = c("None"           = "none",
                               "Section"        = "section",
                               "Primary source" = "source"),
                  selected = "none")
              ),
              column(6,
                tags$br(),
                checkboxInput("plot2_avg", "Show class average", value = TRUE)
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
          plotOutput("cpi_latest_plot", height = "340px"))
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
    if (!nrow(df)) return(void_plot("No data yet"))

    df <- apply_anon_map(df, amap)
    df <- filter_price_df(df, input$plot1_cat %||% "All", input$plot1_src %||% "All")
    if (!nrow(df)) return(void_plot("No data for this filter"))

    cpi_df <- compute_cpi_from_df(df)
    if (!nrow(cpi_df)) return(void_plot("CPI needs Wave 1 data"))

    grp <- input$plot1_group %||% "average"

    plot_df <- if (grp == "average") {
      cpi_df |>
        dplyr::group_by(wave) |>
        dplyr::summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(group_label = "Class Average")
    } else if (grp == "section") {
      cpi_df |>
        dplyr::mutate(grp_val = ifelse(!is.na(section) & nzchar(section), section, "Unknown")) |>
        dplyr::group_by(wave, group_label = grp_val) |>
        dplyr::summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")
    } else {
      sel <- input$plot1_students
      if (length(sel) > 0)
        cpi_df <- cpi_df[cpi_df$anon_name %in% sel, , drop = FALSE]
      dplyr::rename(cpi_df, group_label = anon_name)
    }

    if (!nrow(plot_df)) return(void_plot("No data for this selection"))
    n_groups <- dplyr::n_distinct(plot_df$group_label)

    p_base <- ggplot(plot_df, aes(x = wave, y = cpi, group = group_label)) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "gray60", linewidth = 0.7)

    p <- if (n_groups > 1) {
      p_base +
        geom_line(aes(color  = group_label), linewidth = 1.1) +
        geom_point(aes(color = group_label), size = 3) +
        labs(color = NULL) +
        theme(legend.position = if (n_groups <= 12) "right" else "none")
    } else {
      p_base +
        geom_line(color = "#951829", linewidth = 1.1) +
        geom_point(color = "#951829", size = 3)
    }

    p +
      scale_x_continuous(breaks = seq_len(max(plot_df$wave))) +
      labs(x = "Wave", y = "CPI (Wave 1 = 100)") +
      theme_minimal(base_size = 12)
  })

  output$cpi_latest_plot <- renderPlot({
    df   <- all_prices_poll()
    amap <- anon_map()
    if (!nrow(df)) return(void_plot("No data yet"))

    df <- apply_anon_map(df, amap)

    cat_f   <- input$plot2_cat %||% "All"
    src_f   <- input$plot2_src %||% "All"
    df_filt <- filter_price_df(df, cat_f, src_f)
    if (!nrow(df_filt)) return(void_plot("No data for this filter"))

    cpi_df <- compute_cpi_from_df(df_filt)
    if (!nrow(cpi_df)) return(void_plot("CPI needs Wave 1 data"))

    latest_wave <- max(cpi_df$wave)
    latest      <- dplyr::filter(cpi_df, wave == latest_wave)

    color_by <- input$plot2_color %||% "none"
    if (color_by == "source") {
      src_map <- df_filt |>
        dplyr::filter(!is.na(source), nzchar(source)) |>
        dplyr::count(user_id, source) |>
        dplyr::arrange(user_id, dplyr::desc(n)) |>
        dplyr::group_by(user_id) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::select(user_id, color_var = source)
      latest <- dplyr::left_join(latest, src_map, by = "user_id")
      latest$color_var[is.na(latest$color_var)] <- "Unknown"
    } else if (color_by == "section") {
      latest$color_var <- dplyr::coalesce(
        ifelse(nzchar(latest$section %||% ""), latest$section, NA_character_), "Unknown")
    } else {
      latest$color_var <- "All students"
    }

    latest   <- dplyr::mutate(latest,
      anon_name = forcats::fct_reorder(anon_name, cpi))
    avg_cpi  <- mean(latest$cpi, na.rm = TRUE)
    n_colors <- dplyr::n_distinct(latest$color_var)
    x_lo     <- min(93,  min(latest$cpi, na.rm = TRUE) - 3)
    x_hi     <- max(107, max(latest$cpi, na.rm = TRUE) + 9)

    p <- ggplot(latest, aes(y = anon_name)) +
      geom_vline(xintercept = 100, linetype = "dashed",
                 color = "gray60", linewidth = 0.8) +
      geom_segment(aes(x = 100, xend = cpi, yend = anon_name),
                   color = "gray75", linewidth = 0.9)

    p <- if (n_colors > 1) {
      p + geom_point(aes(x = cpi, color = color_var), size = 4) +
        labs(color = switch(color_by,
               section = "Section", source = "Primary source", NULL))
    } else {
      p + geom_point(aes(x = cpi), size = 4, color = "#951829")
    }

    p <- p +
      geom_text(aes(x = cpi, label = sprintf("%.1f", cpi)),
                hjust = -0.3, size = 3)

    if (isTRUE(input$plot2_avg))
      p <- p +
        geom_vline(xintercept = avg_cpi, color = "#1a1a2e",
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = avg_cpi, y = Inf,
                 label = sprintf("Avg: %.1f", avg_cpi),
                 hjust = -0.1, vjust = 2, size = 3.2, color = "#1a1a2e")

    cat_lbl <- if (!identical(cat_f, "All")) paste0(" | ", cat_f) else ""
    src_lbl <- if (!identical(src_f, "All")) paste0(" | source: ", src_f) else ""

    p +
      scale_x_continuous(limits = c(x_lo, x_hi)) +
      labs(x = "Personal CPI (Wave 1 = 100)", y = NULL,
           subtitle = paste0("Wave ", latest_wave, " vs. Wave 1",
                             cat_lbl, src_lbl)) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank(),
            legend.position    = if (n_colors > 1) "bottom" else "none")
  })

  output$cat_count_plot <- renderPlot({
    df <- all_prices_poll()
    if (!nrow(df)) return(void_plot("No data yet"))

    wave_sel <- input$plot3_wave %||% "1"
    facet_by <- input$plot3_facet %||% "none"

    df_plot <- if (identical(wave_sel, "1")) {
      dplyr::filter(df, wave == 1)
    } else {
      df
    }
    if (!nrow(df_plot)) return(void_plot("No data for selected wave"))

    # Normalise facet column
    if (facet_by == "section") {
      df_plot$facet_var <- ifelse(!is.na(df_plot$section) & nzchar(df_plot$section),
                                  df_plot$section, "Unknown")
    } else if (facet_by == "source") {
      df_plot$facet_var <- ifelse(!is.na(df_plot$source) & nzchar(df_plot$source),
                                  df_plot$source, "Unknown")
    } else if (facet_by == "wave") {
      df_plot$facet_var <- paste0("Wave ", df_plot$wave)
    }

    count_df <- if (facet_by == "none") {
      df_plot |>
        dplyr::count(category) |>
        dplyr::mutate(category = forcats::fct_reorder(category, n))
    } else {
      df_plot |>
        dplyr::count(category, facet_var) |>
        dplyr::mutate(category = forcats::fct_reorder(category, n, .fun = sum))
    }

    wave_lbl <- if (identical(wave_sel, "1")) "Wave 1" else "All Waves"

    p <- ggplot(count_df, aes(x = n, y = category)) +
      geom_col(fill = "#951829", alpha = 0.85) +
      geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Number of items", y = NULL,
           subtitle = wave_lbl) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank())

    if (facet_by != "none") {
      p <- p + facet_wrap(~ facet_var, scales = "free_x") +
        geom_col(aes(fill = facet_var), alpha = 0.85, show.legend = FALSE) +
        scale_fill_brewer(palette = "Set2")
    }

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
