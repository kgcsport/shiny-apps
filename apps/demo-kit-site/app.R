library(shiny)
library(DBI)
library(RSQLite)
library(bcrypt)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Shared SQLite ─────────────────────────────────────────────────────────────
sqlite_candidates <- c("../_shared/sqlite.R", "_shared/sqlite.R",
                       "/srv/shiny-server/_shared/sqlite.R")
source(Filter(file.exists, sqlite_candidates)[[1]])

DB_PATH <- file.path(appdata_root(getwd()), "data", "demo_kit.sqlite")
.con <- NULL
db  <- function() {
  if (is.null(.con) || !DBI::dbIsValid(.con)) .con <<- connect_sqlite(DB_PATH)
  .con
}
dbx <- function(sql, p = list()) DBI::dbExecute(db(), sql, p)
dbq <- function(sql, p = list()) DBI::dbGetQuery(db(), sql, p)

dbx("CREATE TABLE IF NOT EXISTS gallery_submissions(
  id            INTEGER PRIMARY KEY AUTOINCREMENT,
  title         TEXT NOT NULL,
  url           TEXT NOT NULL,
  category      TEXT,
  description   TEXT,
  submitter_name TEXT,
  status        TEXT DEFAULT 'pending',
  submitted_at  TEXT DEFAULT CURRENT_TIMESTAMP
);")
dbx("CREATE TABLE IF NOT EXISTS kit_settings(key TEXT PRIMARY KEY, value TEXT);")
if (!nrow(dbq("SELECT 1 FROM kit_settings WHERE key='admin_pw_hash';")))
  dbx("INSERT INTO kit_settings VALUES('admin_pw_hash',?);",
      list(bcrypt::hashpw("classroom2025")))

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

# ── CSS ────────────────────────────────────────────────────────────────────────
CSS <- "
body { font-size: 15px; }
/* Doc viewer */
.kit-inner { display:flex; min-height:calc(100vh - 56px); }
.kit-snav  { width:210px; flex-shrink:0; background:#fff; border-right:1px solid #e0e0e0;
             padding:1rem .9rem; overflow-y:auto; position:sticky; top:56px; height:calc(100vh - 56px);
             box-sizing:border-box; }
.kit-snav-brand { font-size:.92rem; font-weight:700; margin-bottom:1rem; }
.kit-grp   { font-size:.65rem; font-weight:700; text-transform:uppercase; letter-spacing:.09em;
             color:#aaa; margin:.9rem 0 .2rem; padding-top:.5rem; border-top:1px solid #f0f0f0; }
.kit-grp:first-child { margin-top:0; border-top:none; }
.kit-link  { display:block; font-size:.84rem; padding:.22rem .45rem; border-radius:5px;
             cursor:pointer; color:#444; border:none; background:none; width:100%; text-align:left; margin-bottom:.05rem; }
.kit-link:hover { background:#f0f0f0; }
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
/* Prompt builder */
.pb-wrap  { max-width:1100px; margin:0 auto; padding:1.5rem; }
.pb-cols  { display:grid; grid-template-columns:1fr 1fr; gap:2rem; align-items:start; margin-top:1.25rem; }
@media(max-width:700px){.pb-cols{grid-template-columns:1fr;}}
.pb-output{ background:#f4f6f9; border:1px solid #dde2ea; border-radius:8px;
            padding:1rem 1.1rem; font-family:monospace; font-size:.79rem;
            white-space:pre-wrap; line-height:1.55; max-height:520px; overflow-y:auto; color:#1a1a1a; }
.pb-placeholder{color:#aaa;font-style:italic;}
.pb-label { font-size:.78rem;font-weight:700;text-transform:uppercase;letter-spacing:.07em;color:#888;margin-bottom:.4rem; }
/* Gallery */
.gal-wrap { max-width:1100px; margin:0 auto; padding:1.5rem; }
.gal-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(280px,1fr)); gap:1.1rem; }
.gal-card { background:#fff; border:1px solid #e5e5e5; border-radius:10px; overflow:hidden; display:flex; flex-direction:column; }
.gal-prev { height:180px; background:#f4f5f7; border-bottom:1px solid #eee;
            position:relative; display:flex; align-items:center; justify-content:center; }
.gal-prev iframe { position:absolute;top:0;left:0;width:100%;height:100%;border:none; }
.gal-icon { font-size:2rem; color:#ccc; }
.gal-body { padding:.9rem 1rem; flex:1; }
.gal-cat  { font-size:.68rem; font-weight:700; text-transform:uppercase; letter-spacing:.07em; color:#2d5be3; margin-bottom:.3rem; }
.gal-title{ font-size:1rem; font-weight:700; margin-bottom:.3rem; }
.gal-desc { font-size:.84rem; color:#555; line-height:1.5; }
.gal-foot { padding:.6rem 1rem; border-top:1px solid #f0f0f0; display:flex; gap:.5rem; align-items:center; }
.load-btn { font-size:.78rem; padding:.2rem .6rem; border-radius:5px;
            border:1.5px solid #2d5be3; color:#2d5be3; background:none; cursor:pointer; }
.load-btn:hover,.load-btn.on { background:#2d5be3; color:#fff; }
.submit-box { background:#fff; border:1px solid #e5e5e5; border-radius:10px; padding:1.5rem; margin-top:2rem; }
/* Admin */
.adm-wrap { max-width:700px; margin:0 auto; padding:1.5rem; }
.adm-box  { background:#fff; border:1px solid #e5e5e5; border-radius:10px; padding:1.5rem; margin-bottom:1rem; }
.adm-row  { display:flex; gap:.5rem; align-items:flex-start; padding:.65rem 0;
            border-bottom:1px solid #f5f5f5; font-size:.88rem; }
.adm-row:last-child { border-bottom:none; }
.adm-info { flex:1; }
.adm-title{ font-weight:600; }
.adm-meta { font-size:.78rem; color:#888; margin-top:.15rem; }
"

# ── Sidebar (docs) ────────────────────────────────────────────────────────────
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
  tags$hr(style = "margin:.9rem 0 .6rem;border-color:#eee;"),
  downloadButton("dl_all", "Download all (ZIP)",
                 class = "btn btn-sm btn-outline-secondary",
                 style = "width:100%;font-size:.78rem;")
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title  = "AI Teaching Tool Demo Kit",
  id     = "main_tabs",
  header = tags$head(
    tags$style(HTML(CSS)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('hl_nav', function(k) {
        document.querySelectorAll('.kit-link').forEach(function(e){e.classList.remove('active');});
        var el = document.getElementById('nav_' + k.replace(/[^a-zA-Z0-9]/g,'_'));
        if (el) el.classList.add('active');
      });
      function galToggle(i, url, title) {
        var btn = document.getElementById('glb'+i);
        var pre = document.getElementById('glp'+i);
        if (btn.classList.contains('on')) {
          pre.innerHTML = '<div class=\"gal-icon\">📋</div>';
          btn.textContent = 'Load preview'; btn.classList.remove('on');
        } else {
          pre.innerHTML = '<iframe src=\"'+url+'\" title=\"'+title+'\" loading=\"lazy\"></iframe>';
          btn.textContent = 'Hide'; btn.classList.add('on');
        }
      }
    "))
  ),

  tabPanel("Documentation",
    div(class = "kit-inner",
      div(class = "kit-snav", sidebar_nav),
      div(class = "kit-body",
        div(style = "display:flex;gap:.6rem;align-items:center;margin-bottom:1.25rem;flex-wrap:wrap;",
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
      tags$p(style = "color:#666;font-size:.9rem;",
             "Fill in what you know, then copy the result into Claude Code, Cursor, or Codex."),
      div(class = "pb-cols",
        div(
          textInput("pb_course", "Course", placeholder = "e.g. ECON 101 Principles of Microeconomics", width = "100%"),
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
          div(style = "display:flex;gap:.5rem;align-items:center;margin-top:.25rem;",
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
  ),

  tabPanel("Gallery",
    div(class = "gal-wrap",
      tags$h4(style = "margin-bottom:.2rem;", "What instructors have built"),
      tags$p(style = "color:#666;font-size:.9rem;margin-bottom:1.25rem;",
             "Click “Load preview” to embed a live app, or open it in a new tab."),
      uiOutput("gallery_grid"),
      div(class = "submit-box",
        tags$h5(style = "margin-bottom:1rem;", "Submit your app"),
        fluidRow(
          column(6, textInput("sub_title", "Title", width = "100%")),
          column(6, textInput("sub_url",   "App URL", placeholder = "https://…", width = "100%"))
        ),
        fluidRow(
          column(6, selectInput("sub_cat",  "Category",       CATS, width = "100%")),
          column(6, textInput("sub_name", "Your name (optional)", width = "100%"))
        ),
        textAreaInput("sub_desc", "Short description", rows = 2, width = "100%",
                      placeholder = "What does it demonstrate? What do students do?"),
        actionButton("sub_submit", "Submit to gallery", class = "btn btn-primary"),
        uiOutput("sub_msg")
      )
    )
  ),

  tabPanel("Admin",
    div(class = "adm-wrap",
      uiOutput("admin_panel")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(sel = FIRST, admin = FALSE, gal_ver = 0L, prompt = NULL)

  # ── Documentation ─────────────────────────────────────────────────────────────
  observeEvent(input$sel_file, { if (nzchar(input$sel_file %||% "")) rv$sel <- input$sel_file })
  observe({ session$sendCustomMessage("hl_nav", rv$sel) })

  output$doc_panel <- renderUI({
    f <- rv$sel
    if (!nzchar(f %||% ""))
      return(div(class = "kit-doc", tags$p(style = "color:#aaa;", "Select a file from the sidebar.")))
    div(class = "kit-doc", render_md(f))
  })
  output$cur_label <- renderUI({
    f <- rv$sel
    if (nzchar(f %||% "")) tags$code(style = "color:#aaa;font-size:.8rem;", f) else NULL
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

  # ── Gallery ───────────────────────────────────────────────────────────────────
  gallery_rows <- reactive({
    rv$gal_ver
    tryCatch(dbq("SELECT * FROM gallery_submissions WHERE status='approved' ORDER BY submitted_at DESC;"),
             error = function(e) data.frame())
  })

  output$gallery_grid <- renderUI({
    rows <- gallery_rows()
    if (!nrow(rows))
      return(div(style = "color:#aaa;font-size:.9rem;padding:1rem 0;",
                 "No approved apps yet — submit yours below!"))
    div(class = "gal-grid",
      lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        url_esc   <- gsub("'", "\\'", r$url   %||% "", fixed = TRUE)
        title_esc <- gsub("'", "\\'", r$title %||% "", fixed = TRUE)
        div(class = "gal-card",
          div(class = "gal-prev", id = paste0("glp", i), div(class = "gal-icon", "\U0001f4cb")),
          div(class = "gal-body",
            div(class = "gal-cat",   r$category %||% ""),
            div(class = "gal-title", r$title    %||% ""),
            div(class = "gal-desc",  r$description %||% "")
          ),
          div(class = "gal-foot",
            tags$button(class = "load-btn", id = paste0("glb", i),
              onclick = sprintf("galToggle(%d,'%s','%s');", i, url_esc, title_esc),
              "Load preview"),
            tags$a(href = r$url %||% "#", target = "_blank", rel = "noopener",
                   style = "margin-left:auto;font-size:.8rem;color:#888;", "Open ↗")
          )
        )
      })
    )
  })

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

  # ── Admin ─────────────────────────────────────────────────────────────────────
  observeEvent(input$adm_login, {
    pw <- input$adm_pw %||% ""
    h  <- tryCatch(dbq("SELECT value FROM kit_settings WHERE key='admin_pw_hash';")$value[1],
                   error = function(e) "")
    if (nzchar(h) && bcrypt::checkpw(pw, h)) rv$admin <- TRUE
    else showNotification("Incorrect password.", type = "error")
  })

  observeEvent(input$adm_action, {
    req(rv$admin)
    act <- input$adm_action$act
    sid <- as.integer(input$adm_action$id)
    if (act %in% c("approve", "reject")) {
      dbx("UPDATE gallery_submissions SET status=? WHERE id=?;",
          list(if (act == "approve") "approved" else "rejected", sid))
      rv$gal_ver <- rv$gal_ver + 1L
      showNotification(if (act == "approve") "Approved." else "Rejected.", type = "message")
    }
  })

  observeEvent(input$adm_set_pw, {
    req(rv$admin)
    pw <- trimws(input$adm_new_pw %||% "")
    if (nchar(pw) < 8) { showNotification("Password must be at least 8 characters.", type = "error"); return() }
    dbx("INSERT OR REPLACE INTO kit_settings VALUES('admin_pw_hash',?);", list(bcrypt::hashpw(pw)))
    updatePasswordInput(session, "adm_new_pw", value = "")
    showNotification("Password updated.", type = "message")
  })

  output$admin_panel <- renderUI({
    if (!rv$admin)
      return(div(class = "adm-box",
        tags$h5("Admin login"),
        passwordInput("adm_pw", "Password", width = "100%"),
        actionButton("adm_login", "Log in", class = "btn btn-primary btn-sm")
      ))

    pending <- tryCatch(
      dbq("SELECT * FROM gallery_submissions WHERE status='pending' ORDER BY submitted_at DESC;"),
      error = function(e) data.frame())

    tagList(
      div(class = "adm-box",
        tags$h5(style = "margin-bottom:1rem;",
                sprintf("Pending submissions (%d)", nrow(pending))),
        if (!nrow(pending))
          tags$p(style = "color:#aaa;", "Nothing pending.")
        else
          lapply(seq_len(nrow(pending)), function(i) {
            r <- pending[i, ]
            div(class = "adm-row",
              div(class = "adm-info",
                div(class = "adm-title", r$title %||% ""),
                div(class = "adm-meta",
                    sprintf("%s · %s · %s",
                            r$category %||% "", r$submitter_name %||% "(anon)", r$submitted_at %||% ""),
                    tags$br(),
                    tags$a(href = r$url %||% "#", target = "_blank", r$url %||% ""))
              ),
              tags$button("Approve", class = "btn btn-sm btn-success",
                onclick = sprintf("Shiny.setInputValue('adm_action',{act:'approve',id:%d},{priority:'event'});", r$id)),
              tags$button("Reject",  class = "btn btn-sm btn-danger",
                onclick = sprintf("Shiny.setInputValue('adm_action',{act:'reject',id:%d},{priority:'event'});", r$id))
            )
          })
      ),
      div(class = "adm-box",
        tags$h5("Change admin password"),
        passwordInput("adm_new_pw", "New password", width = "280px"),
        actionButton("adm_set_pw", "Set password", class = "btn btn-sm btn-outline-secondary")
      )
    )
  })
}

shinyApp(ui, server)
