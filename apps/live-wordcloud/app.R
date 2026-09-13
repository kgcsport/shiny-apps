try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)

library(shiny)
library(DBI)
library(RSQLite)

this_file <- ""
for (i in rev(seq_len(sys.nframe()))) {
  candidate <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
  if (!is.null(candidate) && nzchar(candidate)) {
    this_file <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
    break
  }
}
this_dir <- if (nzchar(this_file)) dirname(this_file) else getwd()

shared_candidates <- c(
  file.path(this_dir, "..", "_shared", "sqlite.R"),
  file.path("apps", "_shared", "sqlite.R"),
  file.path("..", "_shared", "sqlite.R")
)
shared_sqlite <- Filter(file.exists, shared_candidates)
if (!length(shared_sqlite)) stop("Cannot find apps/_shared/sqlite.R")
source(shared_sqlite[[1]])
source(file.path(this_dir, "helpers.R"))

poll_id <- "excess-burden-tax-base"
db_path <- shared_db_path(demo = FALSE)
con <- connect_sqlite(db_path)

DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS live_poll_responses (
    id            INTEGER PRIMARY KEY AUTOINCREMENT,
    poll_id       TEXT NOT NULL,
    client_token  TEXT NOT NULL,
    response      TEXT NOT NULL,
    response_norm TEXT NOT NULL,
    created_at    TEXT DEFAULT CURRENT_TIMESTAMP,
    updated_at    TEXT DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(poll_id, client_token)
  );
")
DBI::dbExecute(con, "
  CREATE INDEX IF NOT EXISTS idx_live_poll_response
  ON live_poll_responses(poll_id, response_norm);
")
onStop(function() {
  if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
})

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { background:#fff; color:#202124; font-family:Arial,sans-serif; }
      .container-fluid { max-width:1080px; padding:12px 20px 8px; }
      h2 { margin:0 0 4px; color:#7a1731; font-size:30px; font-weight:700; }
      .prompt { margin:0 0 10px; font-size:18px; }
      .poll-entry { display:flex; gap:10px; align-items:flex-end; }
      .poll-entry .form-group { flex:1; margin-bottom:6px; }
      .poll-entry .btn { margin-bottom:6px; background:#7a1731; border-color:#7a1731; }
      .cloud-shell { height:310px; border-top:1px solid #ddd; margin-top:5px;
        display:flex; align-items:center; justify-content:center; overflow:hidden; }
      .word-cloud { width:100%; display:flex; flex-wrap:wrap; align-items:center;
        align-content:center; justify-content:center; gap:10px 18px; padding:14px; }
      .cloud-term { display:inline-block; line-height:1; font-weight:650;
        white-space:nowrap; }
      .cloud-empty { color:#777; font-size:20px; }
      .response-count { text-align:center; color:#666; font-size:14px; margin-top:2px; }
      .admin-box { margin-top:8px; padding:8px 12px; background:#f7f7f7;
        border:1px solid #ddd; border-radius:5px; }
      @media (max-width:650px) {
        .container-fluid { padding:10px 12px; }
        h2 { font-size:25px; }
        .prompt { font-size:16px; }
        .cloud-shell { height:285px; }
      }
    ")),
    tags$script(HTML("
      (function () {
        function sendPollToken() {
          var key = 'econ342-live-poll-token';
          var token = window.localStorage.getItem(key);
          if (!token) {
            token = (window.crypto && crypto.randomUUID)
              ? crypto.randomUUID()
              : Math.random().toString(36).slice(2) + Date.now().toString(36);
            window.localStorage.setItem(key, token);
          }
          Shiny.setInputValue('client_token', token, {priority: 'event'});
        }
        $(document).on('shiny:connected', sendPollToken);
      })();
    "))
  ),
  tags$h2("Make your recommendation"),
  tags$p(class = "prompt", "In 1–4 words, recommend one tax base."),
  div(
    class = "poll-entry",
    textInput("response", label = NULL, placeholder = "e.g., land value", width = "100%"),
    actionButton("submit", "Submit", class = "btn-primary")
  ),
  div(class = "cloud-shell", uiOutput("cloud")),
  textOutput("response_count", container = tags$div, class = "response-count"),
  uiOutput("admin_controls")
)

server <- function(input, output, session) {
  refresh_version <- reactiveVal(0L)
  cloud_timer <- reactiveTimer(3000, session)

  cloud_data <- reactive({
    cloud_timer()
    refresh_version()
    DBI::dbGetQuery(
      con,
      "SELECT response_norm, MIN(response) AS label, COUNT(*) AS n
       FROM live_poll_responses
       WHERE poll_id = ?
       GROUP BY response_norm
       ORDER BY n DESC, response_norm ASC;",
      params = list(poll_id)
    )
  })

  observeEvent(input$submit, {
    response <- trimws(input$response)
    error <- validate_poll_response(response)
    if (!is.null(error)) {
      showNotification(error, type = "error", duration = 4)
      return()
    }

    token <- trimws(input$client_token %||% session$token)
    normalized <- normalize_poll_response(response)
    DBI::dbExecute(
      con,
      "INSERT INTO live_poll_responses
         (poll_id, client_token, response, response_norm)
       VALUES (?, ?, ?, ?)
       ON CONFLICT(poll_id, client_token) DO UPDATE SET
         response = excluded.response,
         response_norm = excluded.response_norm,
         updated_at = CURRENT_TIMESTAMP;",
      params = list(poll_id, token, response, normalized)
    )
    updateTextInput(session, "response", value = "")
    refresh_version(refresh_version() + 1L)
    showNotification("Recommendation added.", type = "message", duration = 2)
  })

  output$cloud <- renderUI({
    dat <- cloud_data()
    if (!nrow(dat)) {
      return(div(class = "cloud-empty", "Responses will appear here."))
    }
    specs <- cloud_term_specs(dat)
    div(
      class = "word-cloud",
      lapply(seq_len(nrow(specs)), function(i) {
        tags$span(
          class = "cloud-term",
          title = paste0(specs$n[i], if (specs$n[i] == 1) " response" else " responses"),
          style = sprintf(
            "font-size:%.2frem;color:%s;opacity:%.2f;",
            specs$font_rem[i], specs$colour[i], specs$opacity[i]
          ),
          specs$label[i]
        )
      })
    )
  })

  output$response_count <- renderText({
    total <- sum(cloud_data()$n)
    if (total == 0) "No responses yet"
    else paste(total, if (total == 1) "response" else "responses")
  })

  admin_mode <- reactive({
    query <- parseQueryString(session$clientData$url_search %||% "")
    identical(query$admin %||% "", "1")
  })

  output$admin_controls <- renderUI({
    req(admin_mode())
    div(
      class = "admin-box",
      passwordInput("admin_password", "Instructor password"),
      actionButton("request_reset", "Clear all responses", class = "btn-danger")
    )
  })

  observeEvent(input$request_reset, {
    showModal(modalDialog(
      title = "Clear this poll?",
      "This permanently removes every response in the current word cloud.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Clear responses", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_reset, {
    configured <- Sys.getenv("SHINY_PASSWORD", "")
    if (!nzchar(configured) || !identical(input$admin_password, configured)) {
      removeModal()
      showNotification("Incorrect instructor password.", type = "error", duration = 4)
      return()
    }
    DBI::dbExecute(
      con,
      "DELETE FROM live_poll_responses WHERE poll_id = ?;",
      params = list(poll_id)
    )
    removeModal()
    refresh_version(refresh_version() + 1L)
    showNotification("Poll cleared.", type = "message", duration = 3)
  })
}

shinyApp(ui, server)
