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

default_poll_id <- "excess-burden-tax-base"
live_poll_base_url <- Sys.getenv(
  "LIVE_POLL_BASE_URL",
  "https://shiny.kylecoombs.com/live-wordcloud/"
)

con <- connect_sqlite(shared_db_path(demo = FALSE))
initialize_live_poll_schema(con)
seed_live_poll(
  con,
  default_poll_id,
  "Make your recommendation",
  "In 1–4 words, recommend one tax base.",
  max_words = 4L
)
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
      .poll-closed, .poll-missing { padding:10px; color:#666; font-size:18px; }
      .cloud-shell { height:310px; border-top:1px solid #ddd; margin-top:5px;
        display:flex; align-items:center; justify-content:center; overflow:hidden; }
      .word-cloud { width:100%; display:flex; flex-wrap:wrap; align-items:center;
        align-content:center; justify-content:center; gap:10px 18px; padding:14px; }
      .cloud-term { display:inline-block; line-height:1; font-weight:650;
        white-space:nowrap; }
      .cloud-empty { color:#777; font-size:20px; }
      .response-count { text-align:center; color:#666; font-size:14px; margin-top:2px; }
      html.display-mode { overflow:hidden; }
      html.display-mode .container-fluid { max-width:none; padding:0; }
      html.display-mode #poll_heading,
      html.display-mode #poll_instructions,
      html.display-mode #poll_entry,
      html.display-mode #admin_controls { display:none; }
      html.display-mode .cloud-shell { height:390px; margin:0; border:0; }
      html.display-mode .response-count { font-size:16px; margin-top:4px; }
      .admin-box { margin-top:16px; padding:14px; background:#f7f7f7;
        border:1px solid #ccc; border-radius:5px; }
      .admin-box h3 { margin-top:0; color:#7a1731; }
      .embed-code { font-size:12px; white-space:pre-wrap; word-break:break-all; }
      @media (max-width:650px) {
        .container-fluid { padding:10px 12px; }
        h2 { font-size:25px; }
        .prompt { font-size:16px; }
        .cloud-shell { height:285px; }
      }
    ")),
    tags$script(HTML("
      (function () {
        if (new URLSearchParams(window.location.search).get('display') === '1') {
          document.documentElement.classList.add('display-mode');
        }
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
        Shiny.addCustomMessageHandler('copyPollText', function (value) {
          navigator.clipboard.writeText(value);
        });
      })();
    "))
  ),
  uiOutput("poll_heading"),
  uiOutput("poll_instructions"),
  uiOutput("poll_entry"),
  div(class = "cloud-shell", uiOutput("cloud")),
  div(class = "response-count", textOutput("response_count")),
  uiOutput("admin_controls")
)

server <- function(input, output, session) {
  refresh_version <- reactiveVal(0L)
  cloud_timer <- reactiveTimer(3000, session)

  query_values <- reactive({
    parseQueryString(session$clientData$url_search %||% "")
  })

  selected_poll_id <- reactive({
    requested <- query_values()$poll %||% default_poll_id
    normalized <- normalize_poll_id(requested)
    if (nzchar(normalized)) normalized else default_poll_id
  })

  admin_mode <- reactive(identical(query_values()$admin %||% "", "1"))

  poll_record <- reactive({
    cloud_timer()
    refresh_version()
    get_live_poll(con, selected_poll_id())
  })

  cloud_data <- reactive({
    cloud_timer()
    refresh_version()
    live_poll_counts(con, selected_poll_id())
  })

  output$poll_heading <- renderUI({
    poll <- poll_record()
    if (!nrow(poll)) tags$h2("Poll not found") else tags$h2(poll$prompt[1])
  })

  output$poll_instructions <- renderUI({
    poll <- poll_record()
    if (!nrow(poll)) {
      return(tags$p(class = "poll-missing", "Check the poll URL or ask the instructor."))
    }
    tags$p(class = "prompt", poll$instructions[1])
  })

  output$poll_entry <- renderUI({
    poll <- poll_record()
    if (!nrow(poll)) return(NULL)
    if (!isTRUE(as.logical(poll$is_open[1]))) {
      return(div(class = "poll-closed", "Submissions are closed; results remain visible."))
    }
    div(
      class = "poll-entry",
      textInput("response", label = NULL, placeholder = "Type a short response", width = "100%"),
      actionButton("submit", "Submit", class = "btn-primary")
    )
  })

  observeEvent(input$submit, {
    poll <- poll_record()
    if (!nrow(poll) || !isTRUE(as.logical(poll$is_open[1]))) {
      showNotification("This poll is closed.", type = "error", duration = 4)
      return()
    }
    response <- trimws(input$response)
    error <- validate_poll_response(response, max_words = poll$max_words[1])
    if (!is.null(error)) {
      showNotification(error, type = "error", duration = 4)
      return()
    }
    token <- trimws(input$client_token %||% session$token)
    upsert_live_poll_response(con, selected_poll_id(), token, response)
    updateTextInput(session, "response", value = "")
    refresh_version(refresh_version() + 1L)
    showNotification("Response added.", type = "message", duration = 2)
  })

  output$cloud <- renderUI({
    if (!nrow(poll_record())) return(NULL)
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
    if (!nrow(poll_record())) return("")
    total <- sum(cloud_data()$n)
    if (total == 0) "No responses yet"
    else paste(total, if (total == 1) "response" else "responses")
  })

  admin_version <- reactiveVal(0L)
  admin_polls <- reactive({
    admin_version()
    list_live_polls(con)
  })

  output$admin_controls <- renderUI({
    req(admin_mode())
    polls <- admin_polls()
    choices <- stats::setNames(polls$poll_id, paste0(
      polls$prompt,
      ifelse(as.logical(polls$is_open), " (open)", " (closed)")
    ))
    div(
      class = "admin-box",
      tags$h3("Instructor poll manager"),
      passwordInput("admin_password", "Instructor password"),
      selectInput("admin_poll_select", "Saved question", choices = choices),
      actionButton("new_poll", "New question"),
      tags$hr(),
      textInput("admin_poll_id", "URL key (optional)", placeholder = "Generated from the question"),
      textInput("admin_prompt", "Question"),
      textInput("admin_instructions", "Response instruction"),
      numericInput("admin_max_words", "Maximum words", value = 4, min = 1, max = 8),
      checkboxInput("admin_is_open", "Accept responses", value = TRUE),
      actionButton("save_poll", "Save question", class = "btn-primary"),
      actionButton("request_reset", "Clear its responses", class = "btn-danger"),
      tags$hr(),
      tags$strong("Direct URL"),
      tags$div(class = "embed-code", textOutput("embed_url")),
      actionButton("copy_url", "Copy URL"),
      tags$br(), tags$br(),
      tags$strong("Quarto iframe"),
      tags$div(class = "embed-code", textOutput("embed_code")),
      actionButton("copy_embed", "Copy iframe code")
    )
  })

  observeEvent(input$admin_poll_select, {
    req(admin_mode(), nzchar(input$admin_poll_select %||% ""))
    poll <- get_live_poll(con, input$admin_poll_select)
    req(nrow(poll))
    updateTextInput(session, "admin_poll_id", value = poll$poll_id[1])
    updateTextInput(session, "admin_prompt", value = poll$prompt[1])
    updateTextInput(session, "admin_instructions", value = poll$instructions[1])
    updateNumericInput(session, "admin_max_words", value = poll$max_words[1])
    updateCheckboxInput(session, "admin_is_open", value = as.logical(poll$is_open[1]))
  }, ignoreInit = FALSE)

  observeEvent(input$new_poll, {
    updateSelectInput(session, "admin_poll_select", selected = character(0))
    updateTextInput(session, "admin_poll_id", value = "")
    updateTextInput(session, "admin_prompt", value = "")
    updateTextInput(session, "admin_instructions", value = "Respond in a few words.")
    updateNumericInput(session, "admin_max_words", value = 4)
    updateCheckboxInput(session, "admin_is_open", value = TRUE)
  })

  instructor_authorized <- function() {
    configured <- Sys.getenv("SHINY_PASSWORD", "")
    nzchar(configured) && identical(input$admin_password, configured)
  }

  observeEvent(input$save_poll, {
    if (!instructor_authorized()) {
      showNotification("Incorrect instructor password.", type = "error", duration = 4)
      return()
    }
    prompt <- trimws(input$admin_prompt %||% "")
    poll_id <- normalize_poll_id(input$admin_poll_id)
    if (!nzchar(prompt)) {
      showNotification("A question is required.", type = "error", duration = 4)
      return()
    }
    if (!nzchar(poll_id)) poll_id <- normalize_poll_id(prompt)
    saved <- save_live_poll(
      con,
      poll_id,
      prompt,
      input$admin_instructions,
      input$admin_max_words,
      input$admin_is_open
    )
    updateTextInput(session, "admin_poll_id", value = saved)
    admin_version(admin_version() + 1L)
    refresh_version(refresh_version() + 1L)
    showNotification("Question saved.", type = "message", duration = 3)
  })

  current_admin_poll_id <- reactive({
    normalize_poll_id(input$admin_poll_id %||% input$admin_poll_select %||% "")
  })

  embed_url <- reactive({
    id <- current_admin_poll_id()
    if (!nzchar(id)) return("Save a question to generate its URL.")
    paste0(live_poll_base_url, "?poll=", utils::URLencode(id, reserved = TRUE))
  })

  embed_code <- reactive({
    url <- embed_url()
    if (!startsWith(url, "http")) return(url)
    paste0(
      '<iframe src="', url,
      '&amp;display=1" title="Live class poll" style="width:100%;height:420px;',
      'border:1px solid #ddd;border-radius:6px;" loading="eager"></iframe>'
    )
  })

  output$embed_url <- renderText(embed_url())
  output$embed_code <- renderText(embed_code())

  observeEvent(input$copy_url, {
    session$sendCustomMessage("copyPollText", isolate(embed_url()))
    showNotification("Poll URL copied.", type = "message", duration = 2)
  })

  observeEvent(input$copy_embed, {
    session$sendCustomMessage("copyPollText", isolate(embed_code()))
    showNotification("Iframe code copied.", type = "message", duration = 2)
  })

  observeEvent(input$request_reset, {
    id <- current_admin_poll_id()
    if (!nzchar(id)) {
      showNotification("Select a saved question first.", type = "error", duration = 4)
      return()
    }
    showModal(modalDialog(
      title = "Clear this poll?",
      paste0("This permanently removes every response for “", id, "”."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Clear responses", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_reset, {
    if (!instructor_authorized()) {
      removeModal()
      showNotification("Incorrect instructor password.", type = "error", duration = 4)
      return()
    }
    clear_live_poll_responses(con, current_admin_poll_id())
    removeModal()
    refresh_version(refresh_version() + 1L)
    showNotification("Responses cleared.", type = "message", duration = 3)
  })
}

shinyApp(ui, server)
