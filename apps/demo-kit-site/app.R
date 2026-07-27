library(shiny)
library(httr)
library(DBI)
library(RSQLite)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Shared SQLite ─────────────────────────────────────────────────────────────
local({
  candidates <- Filter(file.exists, c("../_shared/sqlite.R", "_shared/sqlite.R",
                                       "/srv/shiny-server/_shared/sqlite.R"))
  if (!length(candidates)) stop("Cannot find shared SQLite helper. Tried paths relative to: ", getwd())
  source(candidates[[1]])
})

DB_PATH <- file.path(appdata_root(getwd()), "data", "demo_kit.sqlite")
.con <- NULL
db  <- function() {
  if (is.null(.con) || !DBI::dbIsValid(.con)) .con <<- connect_sqlite(DB_PATH)
  .con
}
dbx <- function(sql, p = list()) {
  if (length(p)) DBI::dbExecute(db(), sql, p) else DBI::dbExecute(db(), sql)
}
dbq <- function(sql, p = list()) {
  if (length(p)) DBI::dbGetQuery(db(), sql, p) else DBI::dbGetQuery(db(), sql)
}

dbx("CREATE TABLE IF NOT EXISTS gallery_submissions(
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  title          TEXT NOT NULL,
  url            TEXT NOT NULL,
  category       TEXT,
  description    TEXT,
  submitter_name TEXT,
  status         TEXT DEFAULT 'pending',
  submitted_at   TEXT DEFAULT CURRENT_TIMESTAMP
);")

# ── File catalog ──────────────────────────────────────────────────────────────
kit_md <- function(sub = NULL) sort(list.files(sub %||% ".", pattern = "\\.md$", FALSE))
make_grp <- function(sub = NULL, pfx = "") {
  f <- kit_md(sub); if (!length(f)) return(character(0))
  setNames(if (nzchar(pfx)) paste0(pfx, "/", f) else f, tools::file_path_sans_ext(f))
}
NAV <- Filter(length, list(
  "Kit files"     = make_grp(),
  "Examples"      = make_grp("examples",      "examples"),
  "Starter specs" = make_grp("starter_specs", "starter_specs")
))
ALL_PATHS <- unlist(NAV, use.names = FALSE)
FIRST     <- if (length(ALL_PATHS)) ALL_PATHS[[1]] else ""

read_kit <- function(rel)
  tryCatch(paste(readLines(rel, warn = FALSE), collapse = "\n"), error = function(e) "*(not found)*")
render_md <- function(rel) {
  html <- tryCatch(commonmark::markdown_html(read_kit(rel)),
                   error = function(e) paste0("<pre>", htmltools::htmlEscape(read_kit(rel)), "</pre>"))
  HTML(html)
}

CATS   <- c("Public goods game", "Auction", "Policy calculator", "Prediction market",
            "Participation / class jobs", "Quiz / review", "Other")
STACKS <- c("R Shiny + SQLite", "R Shiny + CSV", "Python Dash + SQLite", "Other")

ANTHROPIC_MODELS <- c(
  "Claude Sonnet (latest)" = "claude-sonnet-4-5-20251001",
  "Claude Haiku (fast)"    = "claude-haiku-4-5-20251001"
)
OPENAI_MODELS <- c(
  "GPT-4o"        = "gpt-4o",
  "GPT-4o mini"   = "gpt-4o-mini",
  "o1-mini"       = "o1-mini",
  "o3-mini"       = "o3-mini"
)
OPENROUTER_MODELS <- c(
  "Claude Sonnet via OpenRouter"  = "anthropic/claude-sonnet-4-5",
  "GPT-4o via OpenRouter"         = "openai/gpt-4o",
  "Gemini 1.5 Pro via OpenRouter" = "google/gemini-pro-1.5"
)

GEN_SYSTEM <- "You are an expert R Shiny developer building instructor-owned classroom teaching tools.

Generate a complete, working R Shiny application based on the specification provided.
The app must be self-contained, use fake data only, include a reset/test mode, and export data to CSV or SQLite.

Return your response using EXACTLY these section markers (no other text before or after each block):

=== app.R ===
[complete self-contained app.R code]

=== README.md ===
[installation and run instructions]

=== install.R ===
[install.packages() calls for every package the app needs]"

# ── API helpers ───────────────────────────────────────────────────────────────
call_anthropic <- function(api_key, prompt, model) {
  resp <- httr::POST(
    "https://api.anthropic.com/v1/messages",
    httr::add_headers("x-api-key" = api_key, "anthropic-version" = "2023-06-01"),
    body = list(model = model, max_tokens = 8000,
                system = GEN_SYSTEM,
                messages = list(list(role = "user", content = prompt))),
    encode = "json"
  )
  if (httr::http_error(resp)) stop(httr::content(resp, "text", encoding = "UTF-8"))
  httr::content(resp, "parsed")$content[[1]]$text
}

call_openai <- function(api_key, prompt, model) {
  resp <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    body = list(model = model,
                messages = list(
                  list(role = "system", content = GEN_SYSTEM),
                  list(role = "user",   content = prompt)
                )),
    encode = "json"
  )
  if (httr::http_error(resp)) stop(httr::content(resp, "text", encoding = "UTF-8"))
  httr::content(resp, "parsed")$choices[[1]]$message$content
}

call_openrouter <- function(api_key, prompt, model) {
  resp <- httr::POST(
    "https://openrouter.ai/api/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    body = list(model = model,
                messages = list(
                  list(role = "system", content = GEN_SYSTEM),
                  list(role = "user",   content = prompt)
                )),
    encode = "json"
  )
  if (httr::http_error(resp)) stop(httr::content(resp, "text", encoding = "UTF-8"))
  httr::content(resp, "parsed")$choices[[1]]$message$content
}

parse_sections <- function(txt) {
  markers <- c("=== app.R ===" , "=== README.md ===", "=== install.R ===")
  keys    <- c("app_r", "readme", "install_r")
  out     <- setNames(as.list(rep("", 3)), keys)
  for (i in seq_along(markers)) {
    s <- regexpr(markers[i], txt, fixed = TRUE)
    if (s == -1) next
    body_start <- s + nchar(markers[i])
    body_end   <- nchar(txt)
    for (j in seq_along(markers)[-i]) {
      ns <- regexpr(markers[j], txt, fixed = TRUE)
      if (ns > body_start && ns < body_end) body_end <- ns - 1
    }
    out[[keys[i]]] <- trimws(substr(txt, body_start, body_end))
  }
  out
}

# ── CSS ────────────────────────────────────────────────────────────────────────
CSS <- "
body { font-size: 15px; }
/* Doc viewer */
.kit-inner { display:flex; min-height:calc(100vh - 56px); }
.kit-snav  { width:210px; flex-shrink:0; background:#fff; border-right:1px solid #e0e0e0;
             padding:1rem .9rem; overflow-y:auto; position:sticky; top:56px;
             height:calc(100vh - 56px); box-sizing:border-box; }
.kit-snav-brand { font-size:.92rem; font-weight:700; margin-bottom:1rem; }
.kit-grp   { font-size:.65rem; font-weight:700; text-transform:uppercase; letter-spacing:.09em;
             color:#aaa; margin:.9rem 0 .2rem; padding-top:.5rem; border-top:1px solid #f0f0f0; }
.kit-grp:first-child { margin-top:0; border-top:none; }
.kit-link  { display:block; font-size:.84rem; padding:.22rem .45rem; border-radius:5px;
             cursor:pointer; color:#444; border:none; background:none; width:100%;
             text-align:left; margin-bottom:.05rem; }
.kit-link:hover  { background:#f0f0f0; }
.kit-link.active { background:#eaf0ff; color:#2d5be3; font-weight:600; }
.kit-body  { flex:1; padding:1.5rem 2rem 3rem; max-width:800px; min-width:0; }
.kit-doc   { background:#fff; border-radius:10px; border:1px solid #e5e5e5; padding:1.75rem 2rem; }
.kit-doc h1{font-size:1.55rem;font-weight:700;margin:0 0 .9rem;line-height:1.2;}
.kit-doc h2{font-size:1.1rem;font-weight:700;margin:1.6rem 0 .45rem;border-bottom:1px solid #eee;padding-bottom:.3rem;}
.kit-doc h3{font-size:.9rem;font-weight:700;margin:1.1rem 0 .3rem;}
.kit-doc p{margin:0 0 .8rem;line-height:1.65;color:#333;}
.kit-doc ul,.kit-doc ol{margin:0 0 .8rem 1.3rem;color:#333;line-height:1.65;}
.kit-doc li{margin-bottom:.2rem;}
.kit-doc pre{background:#f4f6f9;border:1px solid #e0e4ea;border-radius:6px;padding:.85rem 1rem;overflow-x:auto;font-size:.83em;}
.kit-doc code{background:#f0f2f5;border-radius:3px;padding:.1em .3em;font-size:.84em;}
.kit-doc pre code{background:none;padding:0;}
.kit-doc blockquote{border-left:3px solid #ddd;margin:.65rem 0;padding:.2rem .9rem;color:#666;}
.kit-doc table{border-collapse:collapse;width:100%;margin-bottom:.8rem;font-size:.9em;}
.kit-doc th,.kit-doc td{border:1px solid #ddd;padding:.4rem .65rem;text-align:left;}
.kit-doc th{background:#f5f5f5;font-weight:600;}
.kit-doc a{color:#2d5be3;}
/* Prompt builder + generator */
.pb-wrap { max-width:1100px; margin:0 auto; padding:1.5rem; }
.pb-cols { display:grid; grid-template-columns:1fr 1fr; gap:2rem; align-items:start; margin-top:1.25rem; }
@media(max-width:700px){.pb-cols{grid-template-columns:1fr;}}
.pb-label  { font-size:.78rem; font-weight:700; text-transform:uppercase;
             letter-spacing:.07em; color:#888; margin-bottom:.4rem; }
.pb-output { background:#f4f6f9; border:1px solid #dde2ea; border-radius:8px;
             padding:1rem 1.1rem; font-family:monospace; font-size:.79rem;
             white-space:pre-wrap; line-height:1.55; max-height:340px; overflow-y:auto; color:#1a1a1a; }
.pb-placeholder { color:#aaa; font-style:italic; }
.gen-box { background:#fff; border:1px solid #e0e5ee; border-radius:10px;
           padding:1.25rem 1.5rem; margin-top:1.5rem; }
.gen-box h5 { margin:0 0 .25rem; font-size:1rem; font-weight:700; }
.gen-box .note { font-size:.82rem; color:#888; margin-bottom:1rem; }
.gen-api-row { display:grid; grid-template-columns:1fr 1fr 1fr; gap:.75rem; align-items:end; margin-bottom:1rem; }
@media(max-width:600px){.gen-api-row{grid-template-columns:1fr;}}
.code-output { background:#1e2533; color:#d4e0f0; border-radius:8px;
               padding:1rem 1.1rem; font-family:monospace; font-size:.78rem;
               white-space:pre-wrap; line-height:1.55; max-height:420px; overflow-y:auto;
               margin-top:1rem; display:none; }
.code-output.visible { display:block; }
.gen-actions { display:flex; gap:.6rem; flex-wrap:wrap; margin-top:.85rem; }
/* Submit */
.sub-wrap { max-width:700px; margin:0 auto; padding:1.5rem; }
.sub-box  { background:#fff; border:1px solid #e5e5e5; border-radius:10px; padding:1.5rem; }
@media(max-width:680px){
  .kit-inner{flex-direction:column;}
  .kit-snav{width:100%;height:auto;position:static;border-right:none;border-bottom:1px solid #e0e0e0;}
  .kit-body{padding:1.25rem 1rem;}
}
"

# ── Sidebar nav (docs) ────────────────────────────────────────────────────────
sidebar_nav <- tagList(
  div(class = "kit-snav-brand", "AI Teaching Tool Kit"),
  lapply(names(NAV), function(grp) {
    keys <- NAV[[grp]]
    tagList(
      div(class = "kit-grp", grp),
      lapply(seq_along(keys), function(i) {
        k <- keys[[i]]
        tags$button(class = "kit-link",
          id      = paste0("nav_", gsub("[^a-zA-Z0-9]", "_", k)),
          onclick = sprintf("Shiny.setInputValue('sel_file','%s',{priority:'event'});", k),
          names(keys)[i])
      })
    )
  }),
  tags$hr(style = "margin:.9rem 0 .6rem; border-color:#eee;"),
  downloadButton("dl_all", "Download all (ZIP)",
                 class = "btn btn-sm btn-outline-secondary",
                 style = "width:100%; font-size:.78rem;")
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title    = "AI Teaching Tool Demo Kit",
  id       = "main_tabs",
  selected = "Prompt Builder",
  header   = tags$head(
    tags$style(HTML(CSS)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('hl_nav', function(k) {
        document.querySelectorAll('.kit-link').forEach(function(e){ e.classList.remove('active'); });
        var el = document.getElementById('nav_' + k.replace(/[^a-zA-Z0-9]/g,'_'));
        if (el) el.classList.add('active');
      });
      Shiny.addCustomMessageHandler('show_code', function(data) {
        var el = document.getElementById('gen_code_block');
        if (!el) return;
        el.textContent = data.text;
        el.classList.add('visible');
      });
    "))
  ),

  tabPanel("Prompt Builder",
    div(class = "pb-wrap",
      tags$h4(style = "margin-bottom:.2rem;", "Build your prompt"),
      tags$p(style = "color:#666; font-size:.9rem;",
             "Fill in what you know, then generate a prompt for any AI coding tool — or use an API key to generate app code directly."),
      div(class = "pb-cols",
        # ── Left: form fields ──
        div(
          textInput("pb_course",   "Course",   placeholder = "e.g. ECON 101", width = "100%"),
          numericInput("pb_size",  "Class size", value = NA, min = 1, width = "100%"),
          selectInput("pb_type",   "Activity type", c("(choose…)" = "", CATS), width = "100%"),
          textAreaInput("pb_goal", "Learning goal", rows = 2, width = "100%",
                        placeholder = "What should students understand by the end?"),
          textAreaInput("pb_student",    "Student actions",    rows = 2, width = "100%",
                        placeholder = "What do students click, choose, or enter?"),
          textAreaInput("pb_instructor", "Instructor actions", rows = 2, width = "100%",
                        placeholder = "Setup, round controls, reset, export…"),
          textAreaInput("pb_display",    "Public display",     rows = 2, width = "100%",
                        placeholder = "What goes on the shared classroom screen?"),
          textAreaInput("pb_scoring",    "Scoring / payoff rules", rows = 2, width = "100%",
                        placeholder = "Describe the formula or outcome logic"),
          selectInput("pb_stack",  "Preferred stack", STACKS, width = "100%"),
          textAreaInput("pb_constraints", "Known constraints", rows = 2, width = "100%",
                        placeholder = "Server limits, time, what students can see…"),
          div(style = "display:flex; gap:.5rem; align-items:center; margin-top:.25rem;",
            actionButton("gen_prompt", "Generate prompt", class = "btn btn-primary"),
            uiOutput("copy_btn_ui", inline = TRUE)
          )
        ),
        # ── Right: prompt output + generate section ──
        div(
          div(class = "pb-label", "Generated prompt"),
          uiOutput("pb_output_ui"),

          # ── AI code generation ──────────────────────────────────────────────
          div(class = "gen-box",
            tags$h5("✨ Generate app code"),
            tags$p(class = "note",
                   "Your API key is used only for this request and never stored."),
            div(class = "gen-api-row",
              div(
                tags$label(class = "control-label", "Provider"),
                selectInput("gen_provider", NULL,
                            c("Anthropic"  = "anthropic",
                              "OpenAI"     = "openai",
                              "OpenRouter" = "openrouter"),
                            width = "100%")
              ),
              div(
                tags$label(class = "control-label", "Model"),
                uiOutput("gen_model_ui")
              ),
              div(
                tags$label(class = "control-label", "API key"),
                passwordInput("gen_key", NULL, placeholder = "sk-ant-… or or-…", width = "100%")
              )
            ),
            actionButton("gen_code_btn", "Generate app code",
                         class = "btn btn-success",
                         style = "font-weight:600;"),
            tags$span(style = "margin-left:.5rem; font-size:.82rem; color:#888;",
                      uiOutput("gen_status_ui", inline = TRUE)),
            pre(id = "gen_code_block", class = "code-output"),
            uiOutput("gen_download_ui")
          )
        )
      )
    )
  ),

  tabPanel("Documentation",
    div(class = "kit-inner",
      div(class = "kit-snav", sidebar_nav),
      div(class = "kit-body",
        div(style = "display:flex; gap:.6rem; align-items:center; margin-bottom:1.25rem; flex-wrap:wrap;",
          downloadButton("dl_cur", "Download this file", class = "btn btn-sm btn-primary"),
          uiOutput("cur_label", inline = TRUE)
        ),
        uiOutput("doc_panel")
      )
    )
  ),

  tabPanel("Submit an App",
    div(class = "sub-wrap",
      tags$h4(style = "margin-bottom:.2rem;", "Submit your app to the gallery"),
      tags$p(style = "color:#666; font-size:.9rem; margin-bottom:1.25rem;",
             "Built something useful? Share the URL and a short description. Submissions are reviewed before appearing publicly."),
      div(class = "sub-box",
        fluidRow(
          column(6, textInput("sub_title", "App title", width = "100%")),
          column(6, textInput("sub_url",   "App URL (shinyapps.io or your server)", width = "100%"))
        ),
        fluidRow(
          column(6, selectInput("sub_cat",  "Category",         CATS, width = "100%")),
          column(6, textInput("sub_name", "Your name (optional)", width = "100%"))
        ),
        textAreaInput("sub_desc", "Short description", rows = 3, width = "100%",
                      placeholder = "What does it demonstrate? What do students do?"),
        actionButton("sub_submit", "Submit", class = "btn btn-primary"),
        uiOutput("sub_msg")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(sel = FIRST, prompt = NULL, generated = NULL)

  # ── Documentation ─────────────────────────────────────────────────────────────
  observeEvent(input$sel_file, {
    if (nzchar(input$sel_file %||% "")) rv$sel <- input$sel_file
  })
  observe({ session$sendCustomMessage("hl_nav", rv$sel) })

  output$doc_panel <- renderUI({
    f <- rv$sel
    if (!nzchar(f %||% ""))
      return(div(class = "kit-doc", tags$p(style = "color:#aaa;", "Select a file.")))
    div(class = "kit-doc", render_md(f))
  })
  output$cur_label <- renderUI({
    f <- rv$sel
    if (nzchar(f %||% "")) tags$code(style = "color:#aaa; font-size:.8rem;", f) else NULL
  })
  output$dl_cur <- downloadHandler(
    filename = function() basename(rv$sel),
    content  = function(f) writeLines(read_kit(rv$sel), f)
  )
  output$dl_all <- downloadHandler(
    filename = function() paste0("ai-teaching-tool-demo-kit-", Sys.Date(), ".zip"),
    content  = function(f) {
      tmp <- tempfile(); dir.create(tmp)
      for (rel in ALL_PATHS) writeLines(read_kit(rel), file.path(tmp, basename(rel)))
      zip(f, list.files(tmp, full.names = TRUE), flags = "-j")
    }
  )

  # ── Prompt builder ────────────────────────────────────────────────────────────
  observeEvent(input$gen_prompt, {
    course  <- trimws(input$pb_course      %||% "") %||% "[your course]"
    size    <- if (!is.na(input$pb_size %||% NA)) as.integer(input$pb_size) else "[class size]"
    type    <- if (nzchar(input$pb_type %||% "")) input$pb_type else "[activity type]"
    goal    <- trimws(input$pb_goal        %||% "") %||% "[learning goal]"
    student <- trimws(input$pb_student     %||% "") %||% "[what students do]"
    instr   <- trimws(input$pb_instructor  %||% "") %||% "[setup, round controls, reset, export]"
    display <- trimws(input$pb_display     %||% "") %||% "[what goes on the shared screen]"
    scoring <- trimws(input$pb_scoring     %||% "") %||% "[scoring or payoff rules]"
    stack   <- input$pb_stack %||% "R Shiny + SQLite"
    constr  <- trimws(input$pb_constraints %||% "") %||% "Must run with fake data. No external APIs."

    rv$prompt <- sprintf(
"I want you to build the smallest working prototype of a classroom teaching tool.

Course: %s
Class size: %s
Activity type: %s
Learning goal: %s
Student actions: %s
Instructor actions: %s
Public display: %s
Private instructor controls: [setup screen, round controls, data export, raw event log]
Scoring or payoff rules: %s
Data to save: timestamp, session_id, round_number, participant_id, action, outcome
Data not to save: real names, emails, grades, demographic info, accommodations, private notes
Export format: CSV for raw events and round summaries
Preferred tech stack: %s
Known constraints: %s

Requirements:
- Use fake data only during development.
- Do not ask for real student names, emails, grades, or private notes.
- Build a minimal working version first.
- Keep activity rules and scoring logic transparent and inspectable.
- Store raw event data before computing summaries.
- Create student-facing and instructor-facing views when needed.
- Include a reset / test mode.
- Include exportable raw data in CSV or SQLite.
- Write a README explaining how to install, run, test, reset, and export.",
      course, size, type, goal, student, instr, display, scoring, stack, constr)
  })

  output$pb_output_ui <- renderUI({
    if (is.null(rv$prompt))
      div(class = "pb-output", div(class = "pb-placeholder", "Fill the form and click Generate prompt."))
    else
      div(class = "pb-output", rv$prompt)
  })

  output$copy_btn_ui <- renderUI({
    req(rv$prompt)
    tags$button("Copy to clipboard", class = "btn btn-outline-secondary btn-sm",
      onclick = "var t = document.querySelector('.pb-output');
                 navigator.clipboard.writeText(t.innerText).then(function(){
                   var b = event.target; b.textContent='Copied!';
                   setTimeout(function(){ b.textContent='Copy to clipboard'; }, 2000);
                 });")
  })

  # ── Model selector ────────────────────────────────────────────────────────────
  output$gen_model_ui <- renderUI({
    choices <- switch(input$gen_provider %||% "anthropic",
                      openai     = OPENAI_MODELS,
                      openrouter = OPENROUTER_MODELS,
                      ANTHROPIC_MODELS)
    selectInput("gen_model", NULL, choices, width = "100%")
  })

  # ── Code generation ───────────────────────────────────────────────────────────
  gen_status <- reactiveVal(NULL)

  output$gen_status_ui <- renderUI({
    s <- gen_status()
    if (is.null(s)) return(NULL)
    if (s == "running") tags$em("Generating…")
    else if (startsWith(s, "error:")) span(style = "color:#c00;", sub("^error:", "", s))
    else NULL
  })

  observeEvent(input$gen_code_btn, {
    req(rv$prompt)
    key <- trimws(input$gen_key %||% "")
    if (!nzchar(key)) { showNotification("Enter an API key first.", type = "error"); return() }

    gen_status("running")
    rv$generated <- NULL

    result <- tryCatch({
      provider <- input$gen_provider %||% "anthropic"
      model    <- input$gen_model    %||% names(ANTHROPIC_MODELS)[1]
      raw <- switch(provider,
               openai     = call_openai(key, rv$prompt, model),
               openrouter = call_openrouter(key, rv$prompt, model),
               call_anthropic(key, rv$prompt, model))
      parse_sections(raw)
    }, error = function(e) {
      gen_status(paste0("error: ", conditionMessage(e)))
      NULL
    })

    if (!is.null(result)) {
      gen_status(NULL)
      rv$generated <- result
      display_code <- paste(
        "# ── app.R ─────────────────────────────────────────────────\n",
        result$app_r,
        "\n\n# ── install.R ──────────────────────────────────────────────\n",
        result$install_r
      )
      session$sendCustomMessage("show_code", list(text = display_code))
    }
  })

  output$gen_download_ui <- renderUI({
    req(rv$generated)
    div(class = "gen-actions",
      downloadButton("dl_generated_zip", "Download app (ZIP)",
                     class = "btn btn-primary btn-sm"),
      downloadButton("dl_generated_r", "Download app.R only",
                     class = "btn btn-outline-secondary btn-sm")
    )
  })

  output$dl_generated_zip <- downloadHandler(
    filename = function() paste0("shiny-app-", Sys.Date(), ".zip"),
    content  = function(f) {
      g   <- rv$generated
      tmp <- tempfile(); dir.create(tmp)
      writeLines(g$app_r,     file.path(tmp, "app.R"))
      writeLines(g$readme,    file.path(tmp, "README.md"))
      writeLines(g$install_r, file.path(tmp, "install.R"))
      zip(f, list.files(tmp, full.names = TRUE), flags = "-j")
    }
  )

  output$dl_generated_r <- downloadHandler(
    filename = function() "app.R",
    content  = function(f) writeLines(rv$generated$app_r, f)
  )

  # ── Gallery submission ────────────────────────────────────────────────────────
  observeEvent(input$sub_submit, {
    title <- trimws(input$sub_title %||% "")
    url   <- trimws(input$sub_url   %||% "")
    if (!nzchar(title) || !nzchar(url)) {
      showNotification("Title and URL are required.", type = "error"); return()
    }
    tryCatch({
      dbx("INSERT INTO gallery_submissions(title,url,category,description,submitter_name) VALUES(?,?,?,?,?);",
          list(title, url, input$sub_cat %||% "Other",
               trimws(input$sub_desc %||% ""), trimws(input$sub_name %||% "")))
      updateTextInput(session, "sub_title", value = "")
      updateTextInput(session, "sub_url",   value = "")
      updateTextAreaInput(session, "sub_desc", value = "")
      showNotification("Submitted — thank you! It will appear after review.", type = "message", duration = 6)
    }, error = function(e) showNotification(paste("Error:", conditionMessage(e)), type = "error"))
  })
}

shinyApp(ui, server)
