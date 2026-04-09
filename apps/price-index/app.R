# app.R — Personal Price Index Activity
# Students build a basket of goods and track prices across waves.
# Auth: bcrypt + SQLite, same pattern as final_question_reveal.
# Credentials: CRED_B64 / CRED_CSV / CRED_PATH env vars (same CSV as other apps).
# DB backed up to Google Drive on session end.

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  shiny, DT, bcrypt, dplyr, tidyr, tibble, readr, forcats,
  DBI, RSQLite, ggplot2, googlesheets4, googledrive, promises, future
)

future::plan(future::multisession)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b
logf   <- function(...) cat(format(Sys.time()), "-", paste(...), "\n", file = stderr())

# ── Credentials ───────────────────────────────────────────────────────────────
get_credentials <- function() {
  b64 <- Sys.getenv("CRED_B64", "")
  if (nzchar(b64)) {
    return(read_csv(rawToChar(base64enc::base64decode(b64)),
                    show_col_types = FALSE, trim_ws = TRUE))
  }
  csv <- Sys.getenv("CRED_CSV", "")
  if (nzchar(csv)) {
    con <- textConnection(csv); on.exit(close(con))
    return(read_csv(con, show_col_types = FALSE, trim_ws = TRUE))
  }
  path <- Sys.getenv("CRED_PATH", "credentials.csv")
  if (file.exists(path)) return(read_csv(path, show_col_types = FALSE, trim_ws = TRUE))
  stop("No credentials found: set CRED_B64, CRED_CSV, or CRED_PATH")
}

CRED <- get_credentials()

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

DB_PATH <- file.path(app_data_dir(), "price_index.sqlite")
conn    <- NULL

get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn))
    conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  conn
}
db_exec  <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

init_db <- function() {
  db_exec("CREATE TABLE IF NOT EXISTS users (
    user_id      TEXT PRIMARY KEY,
    display_name TEXT,
    section      TEXT,
    is_admin     INTEGER DEFAULT 0,
    pw_hash      TEXT
  );")

  db_exec("CREATE TABLE IF NOT EXISTS app_state (
    id           INTEGER PRIMARY KEY CHECK(id = 1),
    current_wave INTEGER DEFAULT 1,
    updated_at   TEXT    DEFAULT (CURRENT_TIMESTAMP)
  );")

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
    wave        INTEGER NOT NULL,
    recorded_at TEXT    DEFAULT (CURRENT_TIMESTAMP),
    UNIQUE(item_id, wave)
  );")

  db_exec("CREATE INDEX IF NOT EXISTS ix_pr_user  ON price_records(user_id);")
  db_exec("CREATE INDEX IF NOT EXISTS ix_pr_wave  ON price_records(wave);")
  db_exec("CREATE INDEX IF NOT EXISTS ix_bi_user  ON basket_items(user_id);")

  # Seed / refresh users from credentials CSV
  coerce_admin <- function(x) as.integer(tolower(as.character(x %||% "false")) %in%
                                           c("true","t","1","yes","y"))
  purrr::walk(seq_len(nrow(CRED)), function(i) {
    db_exec(
      "INSERT INTO users(user_id, display_name, section, is_admin, pw_hash)
       VALUES(?,?,?,?,?)
       ON CONFLICT(user_id) DO UPDATE
         SET display_name = excluded.display_name,
             section      = excluded.section,
             is_admin     = excluded.is_admin;",
      params = list(
        CRED$user[i],
        CRED$name[i],
        as.character(CRED$section[i] %||% NA),
        coerce_admin(CRED$is_admin[i]),
        CRED$pw_hash[i]
      )
    )
  })

  n <- db_query("SELECT COUNT(*) n FROM app_state WHERE id=1;")$n[1]
  if (!n) db_exec("INSERT INTO app_state(id, current_wave) VALUES(1, 1);")
}

init_db()

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
  folder_id <- Sys.getenv("PRICE_INDEX_FOLDER_ID", "")
  if (!nzchar(folder_id)) {
    logf("backup_db(): PRICE_INDEX_FOLDER_ID not set — skipping backup.")
    return(invisible(FALSE))
  }
  if (!isTRUE(google_auth())) return(invisible(FALSE))
  try(DBI::dbExecute(get_con(), "PRAGMA wal_checkpoint(FULL);"), silent = TRUE)
  zf <- file.path(tempdir(), sprintf("price_index_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")))
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
        SELECT u.display_name, u.section, bi.item_name, bi.store, bi.category,
               bi.times_per_month, pr.price, pr.wave, pr.recorded_at
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
        wellPanel(
          tags$h5("Add an Item"),
          tags$p(style = "color:#555;font-size:0.9em;",
            "Be specific: brand, size, store location.",
            tags$em(" E.g. '32oz Gatorade Fruit Punch, CVS Main St.' not just 'Gatorade'.")),
          fluidRow(
            column(5, textInput("ni_name",  "Item (brand & size)",
                                placeholder = "32oz Gatorade Fruit Punch")),
            column(4, textInput("ni_store", "Store",
                                placeholder = "CVS Main St.")),
            column(3, selectInput("ni_cat", "Category", choices = BLS_CATEGORIES))
          ),
          fluidRow(
            column(3, numericInput("ni_freq",  "Times/month", value = 4, min = 0.5, step = 0.5)),
            column(3, numericInput("ni_price", "Price today ($)", value = NA, min = 0, step = 0.01)),
            column(3, tags$br(),
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
                tags$th("New Price ($)"), tags$th("")
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
      uiOutput("personal_cpi_ui")
    )
  })

  # ── Add item ────────────────────────────────────────────────────────────────
  observeEvent(input$add_item_btn, {
    uid <- req(rv$user_id)
    nm  <- trimws(input$ni_name  %||% "")
    st  <- trimws(input$ni_store %||% "")
    cat <- input$ni_cat   %||% BLS_CATEGORIES[1]
    fr  <- as.numeric(input$ni_freq)
    pr  <- as.numeric(input$ni_price)

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
        "INSERT INTO price_records(item_id, user_id, price, wave)
         VALUES(?,?,?,1)
         ON CONFLICT(item_id, wave) DO UPDATE
           SET price=excluded.price, recorded_at=CURRENT_TIMESTAMP;",
        list(iid, uid, pr))

      updateTextInput(session, "ni_name",  value = "")
      updateTextInput(session, "ni_store", value = "")
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
        db_exec(
          "INSERT INTO price_records(item_id, user_id, price, wave)
           VALUES(?,?,?,?)
           ON CONFLICT(item_id, wave) DO UPDATE
             SET price=excluded.price, recorded_at=CURRENT_TIMESTAMP;",
          list(iid, uid, pr, as.integer(wv)))
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
  output$dashboard_panel <- renderUI({
    tagList(
      tags$h4("Class Price Index — Live Dashboard"),
      fluidRow(
        column(6,
          tags$h6("Items Tracked by Category (Wave 1)"),
          plotOutput("cat_plot", height = "300px")),
        column(6,
          tags$h6("Personal CPI by Student (latest wave)"),
          plotOutput("cpi_plot", height = "300px"))
      ),
      tags$hr(),
      tags$h6("All Price Submissions"),
      DT::DTOutput("all_prices_tbl")
    )
  })

  output$cat_plot <- renderPlot({
    df <- all_prices_poll()
    if (!nrow(df)) return(NULL)
    df |>
      dplyr::filter(wave == 1) |>
      dplyr::count(category) |>
      dplyr::mutate(category = forcats::fct_reorder(category, n)) |>
      ggplot(aes(x = n, y = category)) +
      geom_col(fill = "#951829", alpha = 0.85) +
      geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Number of items", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank())
  })

  output$cpi_plot <- renderPlot({
    cpi_all <- all_personal_cpis()
    if (is.null(cpi_all) || !nrow(cpi_all)) {
      return(ggplot() +
        annotate("text", x=.5, y=.5, label="Personal CPI available\nafter Wave 2",
                 size=5, color="gray55") + theme_void())
    }
    latest <- cpi_all |>
      dplyr::filter(wave == max(wave)) |>
      dplyr::mutate(display_name = forcats::fct_reorder(display_name, personal_cpi))

    ggplot(latest, aes(x = personal_cpi, y = display_name)) +
      geom_vline(xintercept = 100, linetype = "dashed", color = "gray60", linewidth = 0.8) +
      geom_point(size = 4, color = "#951829") +
      geom_text(aes(label = sprintf("%.1f", personal_cpi)), hjust = -0.35, size = 3.2) +
      scale_x_continuous(
        limits = c(min(93, min(latest$personal_cpi, na.rm=TRUE) - 2),
                   max(107, max(latest$personal_cpi, na.rm=TRUE) + 6))
      ) +
      labs(x = "Personal CPI (baseline = 100)", y = NULL,
           subtitle = paste0("Wave ", max(latest$wave), " vs. Wave 1")) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank())
  })

  output$all_prices_tbl <- DT::renderDT({
    df <- all_prices_poll()
    if (!nrow(df)) return(data.frame(Message = "No submissions yet."))
    df |>
      dplyr::mutate(recorded_at = substr(recorded_at, 1, 16)) |>
      dplyr::select(Student = display_name, Section = section, Wave = wave,
                    Category = category, Item = item_name, Store = store,
                    `Times/mo` = times_per_month, Price = price, Submitted = recorded_at) |>
      dplyr::arrange(Wave, Student, Category)
  }, rownames = FALSE,
     options = list(pageLength = 25, order = list(list(2, "asc"), list(0, "asc"))))

  # ── Admin panel ──────────────────────────────────────────────────────────────
  output$admin_panel <- renderUI({
    req(rv$is_admin)
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
                     pr.price AS Price, pr.recorded_at AS Submitted
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
}

shinyApp(ui, server)
