library(shiny)

# ── File catalog ──────────────────────────────────────────────────────────────
kit_md <- function(sub = NULL) sort(list.files(sub %||% ".", pattern = "\\.md$", FALSE))
make_grp <- function(sub = NULL, pfx = "") {
  f <- kit_md(sub); if (!length(f)) return(character(0))
  setNames(if (nzchar(pfx)) paste0(pfx, "/", f) else f, tools::file_path_sans_ext(f))
}
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

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

# ── CSS ────────────────────────────────────────────────────────────────────────
CSS <- "
body { font-size: 15px; }
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
.kit-doc h1 { font-size:1.55rem; font-weight:700; margin:0 0 .9rem; line-height:1.2; }
.kit-doc h2 { font-size:1.1rem; font-weight:700; margin:1.6rem 0 .45rem;
              border-bottom:1px solid #eee; padding-bottom:.3rem; }
.kit-doc h3 { font-size:.9rem; font-weight:700; margin:1.1rem 0 .3rem; }
.kit-doc p  { margin:0 0 .8rem; line-height:1.65; color:#333; }
.kit-doc ul, .kit-doc ol { margin:0 0 .8rem 1.3rem; color:#333; line-height:1.65; }
.kit-doc li { margin-bottom:.2rem; }
.kit-doc pre { background:#f4f6f9; border:1px solid #e0e4ea; border-radius:6px;
               padding:.85rem 1rem; overflow-x:auto; font-size:.83em; }
.kit-doc code { background:#f0f2f5; border-radius:3px; padding:.1em .3em; font-size:.84em; }
.kit-doc pre code { background:none; padding:0; }
.kit-doc blockquote { border-left:3px solid #ddd; margin:.65rem 0; padding:.2rem .9rem; color:#666; }
.kit-doc table { border-collapse:collapse; width:100%; margin-bottom:.8rem; font-size:.9em; }
.kit-doc th, .kit-doc td { border:1px solid #ddd; padding:.4rem .65rem; text-align:left; }
.kit-doc th { background:#f5f5f5; font-weight:600; }
.kit-doc a  { color:#2d5be3; }
.pb-wrap { max-width:1100px; margin:0 auto; padding:1.5rem; }
.pb-cols { display:grid; grid-template-columns:1fr 1fr; gap:2rem; align-items:start; margin-top:1.25rem; }
@media(max-width:700px) { .pb-cols { grid-template-columns:1fr; } }
.pb-label  { font-size:.78rem; font-weight:700; text-transform:uppercase;
             letter-spacing:.07em; color:#888; margin-bottom:.4rem; }
.pb-output { background:#f4f6f9; border:1px solid #dde2ea; border-radius:8px;
             padding:1rem 1.1rem; font-family:monospace; font-size:.79rem;
             white-space:pre-wrap; line-height:1.55; max-height:520px; overflow-y:auto; color:#1a1a1a; }
.pb-placeholder { color:#aaa; font-style:italic; }
@media(max-width:680px) {
  .kit-inner { flex-direction:column; }
  .kit-snav  { width:100%; height:auto; position:static;
               border-right:none; border-bottom:1px solid #e0e0e0; }
  .kit-body  { padding:1.25rem 1rem; }
}
"

# ── Sidebar nav ───────────────────────────────────────────────────────────────
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
  title  = "AI Teaching Tool Demo Kit",
  id     = "main_tabs",
  header = tags$head(
    tags$style(HTML(CSS)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('hl_nav', function(k) {
        document.querySelectorAll('.kit-link').forEach(function(e) { e.classList.remove('active'); });
        var el = document.getElementById('nav_' + k.replace(/[^a-zA-Z0-9]/g, '_'));
        if (el) el.classList.add('active');
      });
    "))
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

  tabPanel("Prompt Builder",
    div(class = "pb-wrap",
      tags$h4(style = "margin-bottom:.2rem;", "Build your prompt"),
      tags$p(style = "color:#666; font-size:.9rem;",
             "Fill in what you know, then copy the result into Claude Code, Cursor, or Codex."),
      div(class = "pb-cols",
        div(
          textInput("pb_course", "Course",
                    placeholder = "e.g. ECON 101 Principles of Microeconomics", width = "100%"),
          numericInput("pb_size", "Class size", value = NA, min = 1, width = "100%"),
          selectInput("pb_type", "Activity type", c("(choose…)" = "", CATS), width = "100%"),
          textAreaInput("pb_goal", "Learning goal", rows = 2, width = "100%",
                        placeholder = "What should students understand by the end?"),
          textAreaInput("pb_student", "Student actions", rows = 2, width = "100%",
                        placeholder = "What do students click, choose, or enter?"),
          textAreaInput("pb_instructor", "Instructor actions", rows = 2, width = "100%",
                        placeholder = "Setup, round controls, reset, export…"),
          textAreaInput("pb_display", "Public display", rows = 2, width = "100%",
                        placeholder = "What goes on the shared classroom screen?"),
          textAreaInput("pb_scoring", "Scoring / payoff rules", rows = 2, width = "100%",
                        placeholder = "Describe the formula or outcome logic"),
          selectInput("pb_stack", "Preferred stack", STACKS, width = "100%"),
          textAreaInput("pb_constraints", "Known constraints", rows = 2, width = "100%",
                        placeholder = "Server limits, time, what students can see…"),
          div(style = "display:flex; gap:.5rem; align-items:center; margin-top:.25rem;",
            actionButton("gen_prompt", "Generate prompt", class = "btn btn-primary"),
            uiOutput("copy_btn_ui", inline = TRUE)
          )
        ),
        div(
          div(class = "pb-label", "Generated prompt"),
          uiOutput("pb_output_ui")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(sel = FIRST, prompt = NULL)

  # ── Documentation ─────────────────────────────────────────────────────────────
  observeEvent(input$sel_file, {
    if (nzchar(input$sel_file %||% "")) rv$sel <- input$sel_file
  })
  observe({ session$sendCustomMessage("hl_nav", rv$sel) })

  output$doc_panel <- renderUI({
    f <- rv$sel
    if (!nzchar(f %||% ""))
      return(div(class = "kit-doc", tags$p(style = "color:#aaa;", "Select a file from the sidebar.")))
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

This is for a specific instructor-owned classroom activity, not a commercial edtech platform.
Build a local, inspectable tool I can test with fake data before considering classroom use.

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
Local or hosted use: Hosted on instructor-controlled Shiny Server
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
- Write a README explaining how to install, run, test, reset, and export.

Please inspect the project structure first, then propose the smallest viable
implementation plan. After approval, implement it and test with fake data.",
      course, size, type, goal, student, instr, display, scoring, stack, constr)
  })

  output$pb_output_ui <- renderUI({
    if (is.null(rv$prompt))
      div(class = "pb-output", div(class = "pb-placeholder", "Fill the form and click Generate."))
    else
      div(class = "pb-output", rv$prompt)
  })

  output$copy_btn_ui <- renderUI({
    req(rv$prompt)
    tags$button("Copy to clipboard", class = "btn btn-outline-secondary btn-sm",
      onclick = "var t = document.querySelector('.pb-output');
                 navigator.clipboard.writeText(t.innerText).then(function() {
                   var b = event.target; b.textContent = 'Copied!';
                   setTimeout(function(){ b.textContent = 'Copy to clipboard'; }, 2000);
                 });")
  })
}

shinyApp(ui, server)
