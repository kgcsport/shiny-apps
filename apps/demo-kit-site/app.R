try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
library(shiny)
library(DBI)
library(RSQLite)
library(bcrypt)

# ── File path detection (same pattern as arcade) ───────────────────────────────
this_file <- ""
for (i in rev(seq_len(sys.nframe()))) {
  candidate_file <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
  if (!is.null(candidate_file) && nzchar(candidate_file)) {
    this_file <- normalizePath(candidate_file, winslash = "/", mustWork = TRUE)
    break
  }
}
this_dir <- if (nzchar(this_file)) dirname(this_file) else getwd()

# ── Shared SQLite helper ───────────────────────────────────────────────────────
shared_sqlite_candidates <- c(
  file.path(this_dir, "..", "_shared", "sqlite.R"),
  file.path("apps", "_shared", "sqlite.R"),
  file.path("_shared", "sqlite.R"),
  file.path("..", "_shared", "sqlite.R"),
  file.path("/srv/shiny-server", "_shared", "sqlite.R")
)
shared_sqlite <- Filter(file.exists, shared_sqlite_candidates)
if (!length(shared_sqlite)) {
  stop("Cannot find shared SQLite helper from ", getwd(),
       ". Tried: ", paste(shared_sqlite_candidates, collapse = ", "))
}
source(shared_sqlite[[1]])

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Kit directory ──────────────────────────────────────────────────────────────
kit_dir <- normalizePath(
  file.path(this_dir, "..", "..", "ai-teaching-tool-demo-kit"),
  mustWork = FALSE
)

read_kit_file <- function(filename) {
  path <- file.path(kit_dir, filename)
  if (!file.exists(path)) return(character(0))
  readLines(path, warn = FALSE)
}

render_md <- function(lines) {
  if (length(lines) == 0) return(tags$p(tags$em("File not found.")))
  text <- paste(lines, collapse = "\n")
  html <- tryCatch(commonmark::markdown_html(text), error = function(e) NULL)
  if (!is.null(html)) HTML(html) else tags$pre(style = "white-space:pre-wrap;", text)
}

# Read kit files once at startup
kit_files <- list(
  readme      = read_kit_file("README.md"),
  prompt_tmpl = read_kit_file("01_prompt_template.md"),
  privacy     = read_kit_file("03_privacy_workflow.md"),
  testing     = read_kit_file("04_testing_checklist.md"),
  adaptation  = read_kit_file("06_adaptation_guide.md"),
  review      = read_kit_file("05_review_form.md")
)

# ── Database ───────────────────────────────────────────────────────────────────
CONNECT_CONTENT_DIR <- appdata_root(getwd())
DB_PATH <- file.path(CONNECT_CONTENT_DIR, "data", "demo_kit.sqlite")

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <<- connect_sqlite(DB_PATH)
  }
  conn
}
db_query <- function(sql, params = NULL) {
  tryCatch(
    if (is.null(params)) DBI::dbGetQuery(get_con(), sql)
    else DBI::dbGetQuery(get_con(), sql, params = params),
    error = function(e) { message("db_query: ", e$message); data.frame() }
  )
}
db_exec <- function(sql, params = NULL) {
  tryCatch(
    if (is.null(params)) DBI::dbExecute(get_con(), sql)
    else DBI::dbExecute(get_con(), sql, params = params),
    error = function(e) { message("db_exec: ", e$message); -1L }
  )
}

# ── Table init ─────────────────────────────────────────────────────────────────
db_exec(
  "CREATE TABLE IF NOT EXISTS gallery_submissions (
     id             INTEGER PRIMARY KEY AUTOINCREMENT,
     title          TEXT NOT NULL,
     description    TEXT,
     category       TEXT,
     url            TEXT,
     submitter_name TEXT,
     status         TEXT DEFAULT 'pending',
     submitted_at   TEXT DEFAULT (datetime('now'))
   )"
)

# ── Admin password ─────────────────────────────────────────────────────────────
# Hardcoded password: "classroom2025"
ADMIN_PWD_HASH <- bcrypt::hashpw("classroom2025")

# ── Constants ──────────────────────────────────────────────────────────────────
ACTIVITY_TYPES <- c(
  "Public goods game",
  "Auction",
  "Policy calculator",
  "Prediction market",
  "Participation / class jobs",
  "Discussion randomizer",
  "Quiz / review tool",
  "Custom (describe below)"
)

TECH_STACKS <- c(
  "R Shiny + SQLite",
  "R Shiny + CSV",
  "Python Dash + SQLite",
  "Other (describe in constraints)"
)

# ── CSS ────────────────────────────────────────────────────────────────────────
SITE_CSS <- "
/* Reset / base */
body {
  background: #f5f6f8;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  color: #333;
  margin: 0;
  padding: 0;
}
.container-fluid {
  padding: 0 !important;
}

/* Header */
.site-header {
  background: #2c3e50;
  color: white;
  padding: 36px 24px;
  margin-bottom: 0;
}
.site-header h1 {
  color: white;
  margin: 0 0 8px 0;
  font-size: 1.9rem;
  font-weight: 700;
}
.site-header p {
  color: #a9bbc7;
  margin: 0;
  font-size: 1.05rem;
}

/* Main container */
.main-container {
  max-width: 960px;
  margin: 0 auto;
  padding: 28px 24px 48px 24px;
}

/* Typography */
h1, h2, h3, h4 { color: #2c3e50; }
h2 { font-size: 1.4rem; margin-top: 28px; margin-bottom: 12px; }
h3 { font-size: 1.15rem; }
a { color: #2980b9; }
a:hover { color: #1a5276; }

/* Buttons */
.action-button,
.btn-primary {
  background: #2980b9 !important;
  color: white !important;
  border: none !important;
  border-radius: 5px !important;
  padding: 8px 20px !important;
  font-size: 0.95rem !important;
  cursor: pointer !important;
  font-weight: 500 !important;
  transition: background 0.15s !important;
}
.action-button:hover,
.btn-primary:hover {
  background: #1f6391 !important;
}
.btn-secondary {
  background: white;
  color: #2980b9;
  border: 1px solid #2980b9;
  border-radius: 5px;
  padding: 7px 16px;
  font-size: 0.9rem;
  cursor: pointer;
  font-weight: 500;
  transition: background 0.15s;
}
.btn-secondary:hover { background: #e8f4fd; }

.btn-approve {
  background: #27ae60;
  color: white;
  border: none;
  border-radius: 5px;
  padding: 7px 16px;
  font-size: 0.9rem;
  cursor: pointer;
  font-weight: 500;
}
.btn-approve:hover { background: #1e8449; }

.btn-reject {
  background: #e74c3c;
  color: white;
  border: none;
  border-radius: 5px;
  padding: 7px 16px;
  font-size: 0.9rem;
  cursor: pointer;
  font-weight: 500;
}
.btn-reject:hover { background: #c0392b; }

.btn-link {
  color: #2980b9;
  text-decoration: none;
  font-weight: 600;
}
.btn-link:hover { text-decoration: underline; }

/* Cards */
.card {
  background: white;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  padding: 24px;
  margin-bottom: 20px;
}

/* Feature cards */
.feature-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
  gap: 18px;
  margin: 24px 0 32px 0;
}
.feature-card {
  background: white;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.07);
  padding: 22px;
  border-top: 4px solid #2980b9;
}
.feature-card h3 {
  font-size: 0.95rem;
  margin: 0 0 8px 0;
  color: #2c3e50;
}
.feature-card p {
  font-size: 0.88rem;
  color: #555;
  margin: 0;
  line-height: 1.55;
}

/* Gallery */
.gallery-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(270px, 1fr));
  gap: 18px;
  margin: 18px 0 28px 0;
}
.gallery-card {
  display: flex;
  flex-direction: column;
  gap: 6px;
}
.gallery-card h3 {
  margin: 0;
  font-size: 1rem;
}
.gallery-card-meta {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 4px;
}
.gallery-card-actions {
  margin-top: auto;
  padding-top: 10px;
  display: flex;
  align-items: center;
  gap: 10px;
  flex-wrap: wrap;
}
.gallery-card .submitter {
  font-size: 0.8rem;
  color: #888;
  margin: 0;
}

/* Badge */
.badge {
  display: inline-block;
  padding: 2px 10px;
  border-radius: 12px;
  background: #e8f4fd;
  color: #2980b9;
  font-size: 0.72rem;
  font-weight: 600;
  letter-spacing: 0.02em;
  white-space: nowrap;
}

/* Form sections */
.form-section {
  background: white;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.07);
  padding: 24px;
  margin-bottom: 20px;
}
.form-section > h3 {
  margin-top: 0;
  padding-bottom: 12px;
  border-bottom: 1px solid #eee;
  margin-bottom: 16px;
}

/* Prompt output */
#generated_prompt {
  font-size: 0.82rem;
  max-height: 480px;
  overflow-y: auto;
  white-space: pre-wrap;
  word-break: break-word;
  background: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 5px;
  padding: 14px;
  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
}
.copy-btn-row {
  text-align: right;
  margin-top: 8px;
}

/* Admin */
.admin-section {
  margin-top: 36px;
  padding-top: 28px;
  border-top: 2px solid #eee;
}
.admin-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 16px;
}
.admin-header h3 { margin: 0; }
.admin-pending-card {
  background: white;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.07);
  padding: 20px;
  margin-bottom: 14px;
  border-left: 4px solid #f39c12;
}
.admin-pending-card h4 { margin: 0 0 8px 0; }
.admin-pending-card p { margin: 4px 0; font-size: 0.9rem; }
.admin-actions {
  display: flex;
  gap: 8px;
  margin-top: 12px;
  flex-wrap: wrap;
}

/* Status messages */
.status-success {
  margin-top: 12px;
  padding: 10px 16px;
  border-radius: 5px;
  background: #eafaf1;
  color: #1e8449;
  border: 1px solid #a9dfbf;
  font-size: 0.9rem;
}
.status-error {
  margin-top: 12px;
  padding: 10px 16px;
  border-radius: 5px;
  background: #fdf2f2;
  color: #c0392b;
  border: 1px solid #f5b7b1;
  font-size: 0.9rem;
}

/* Docs */
.doc-content {
  line-height: 1.72;
  color: #333;
}
.doc-content h1 { font-size: 1.5rem; color: #2c3e50; }
.doc-content h2 { font-size: 1.2rem; color: #2c3e50; }
.doc-content h3 { font-size: 1.05rem; color: #2c3e50; }
.doc-content code {
  background: #f4f4f4;
  padding: 2px 6px;
  border-radius: 3px;
  font-size: 0.875em;
  font-family: 'SFMono-Regular', Consolas, monospace;
}
.doc-content pre {
  background: #f4f4f4;
  padding: 16px;
  border-radius: 6px;
  overflow-x: auto;
  font-size: 0.85em;
}
.doc-content ul, .doc-content ol { padding-left: 1.5em; }
.doc-content blockquote {
  border-left: 4px solid #2980b9;
  padding-left: 16px;
  color: #666;
  margin: 16px 0;
}

/* Empty state */
.empty-state {
  text-align: center;
  color: #999;
  padding: 36px 20px;
  font-style: italic;
  font-size: 0.95rem;
}

/* Tab overrides */
.nav-tabs > li > a {
  color: #555;
  font-weight: 500;
}
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  color: #2980b9;
  border-color: #ddd #ddd #fff;
}
.tab-content {
  padding-top: 4px;
}

/* Responsive */
@media (max-width: 600px) {
  .main-container { padding: 16px 12px 40px 12px; }
  .feature-grid { grid-template-columns: 1fr 1fr; }
  .gallery-grid { grid-template-columns: 1fr; }
  .site-header h1 { font-size: 1.5rem; }
  .site-header { padding: 24px 16px; }
}
@media (max-width: 400px) {
  .feature-grid { grid-template-columns: 1fr; }
}
"

# ── JavaScript ─────────────────────────────────────────────────────────────────
SITE_JS <- "
// Toggle iframe preview
function togglePreview(id) {
  var el = document.getElementById(id);
  if (!el) return;
  el.style.display = (el.style.display === 'none' || el.style.display === '') ? 'block' : 'none';
}

// Copy generated prompt to clipboard
function copyPrompt() {
  var el = document.getElementById('generated_prompt');
  if (!el) return;
  var text = (el.innerText || el.textContent || '').trim();
  if (!text) { alert('Generate a prompt first.'); return; }
  var btn = document.getElementById('copy_prompt_btn');
  function onCopied() {
    if (btn) {
      var orig = btn.textContent;
      btn.textContent = 'Copied!';
      setTimeout(function() { btn.textContent = orig; }, 2000);
    }
  }
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(text).then(onCopied).catch(function() {
      fallbackCopy(text, onCopied);
    });
  } else {
    fallbackCopy(text, onCopied);
  }
}
function fallbackCopy(text, cb) {
  var ta = document.createElement('textarea');
  ta.value = text;
  ta.style.position = 'fixed';
  ta.style.opacity = '0';
  document.body.appendChild(ta);
  ta.focus();
  ta.select();
  try { document.execCommand('copy'); if (cb) cb(); } catch(e) {}
  document.body.removeChild(ta);
}

// Admin approve / reject via event delegation -> Shiny message
$(document).on('click', '.approve-btn', function() {
  Shiny.setInputValue('admin_action', {action: 'approve', id: $(this).data('id')}, {priority: 'event'});
});
$(document).on('click', '.reject-btn', function() {
  Shiny.setInputValue('admin_action', {action: 'reject', id: $(this).data('id')}, {priority: 'event'});
});
"

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$title("AI Teaching Tool Demo Kit"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(SITE_CSS)),
    tags$script(HTML(SITE_JS))
  ),

  # Site header (full-width dark bar)
  div(class = "site-header",
    h1("AI Teaching Tool Demo Kit"),
    p("Build lightweight, instructor-owned classroom activities with AI.")
  ),

  # Main content container
  div(class = "main-container",
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",

      # ── Tab 1: Overview ──────────────────────────────────────────────────────
      tabPanel("Overview",
        br(),
        div(class = "card doc-content",
          uiOutput("overview_readme")
        ),
        h2("Key Features"),
        div(class = "feature-grid",
          div(class = "feature-card",
            h3("Build Your Own"),
            p("Use the interactive prompt builder to describe your classroom activity and get a ready-to-paste AI coding prompt.")
          ),
          div(class = "feature-card",
            h3("Test Before Class"),
            p("Follow the testing checklist to verify your tool works with fake data before ever using it in a live session.")
          ),
          div(class = "feature-card",
            h3("Own Your Data"),
            p("Keep student data local. The privacy workflow shows how to build without sharing real student PII with AI tools.")
          ),
          div(class = "feature-card",
            h3("Share & Remix"),
            p("Browse the gallery of instructor-built apps. Submit your own for others to discover and adapt.")
          )
        )
      ),

      # ── Tab 2: Build Your Own ────────────────────────────────────────────────
      tabPanel("Build Your Own",
        br(),
        div(class = "form-section",
          h3("Describe Your Activity"),
          p(style = "color:#666; margin-top:-8px; margin-bottom:16px;",
            "Fill in the fields and click Generate Prompt to get a ready-to-paste AI coding prompt."),
          fluidRow(
            column(6,
              textInput("pb_course", "Course",
                placeholder = "e.g. ECON 101 – Principles of Microeconomics")
            ),
            column(6,
              numericInput("pb_class_size", "Class size", value = 25, min = 1, max = 500)
            )
          ),
          selectInput("pb_activity_type", "Activity type", choices = ACTIVITY_TYPES, width = "100%"),
          textAreaInput("pb_learning_goal", "Learning goal", rows = 2, width = "100%",
            placeholder = "What should students understand or experience from this activity?"),
          textAreaInput("pb_student_actions", "Student actions", rows = 2, width = "100%",
            placeholder = "What will each student do? (choose, bid, vote, calculate, ...)"),
          textAreaInput("pb_instructor_actions", "Instructor actions", rows = 2, width = "100%",
            placeholder = "How will you start, advance, pause, or reset the activity?"),
          textAreaInput("pb_public_display", "Public display", rows = 2, width = "100%",
            placeholder = "What will students see? (anonymized results, round status, group totals, ...)"),
          textAreaInput("pb_private_controls", "Private instructor controls", rows = 2, width = "100%",
            placeholder = "What will only the instructor see? (setup screen, data export, reset mode, ...)"),
          textAreaInput("pb_scoring_rules", "Scoring or payoff rules", rows = 2, width = "100%",
            placeholder = "How are points, payoffs, or outcomes calculated?")
        ),
        div(class = "form-section",
          h3("Data & Privacy"),
          textAreaInput("pb_data_to_save", "Data to save", rows = 2, width = "100%",
            value = "timestamp, session_id, round_number, participant_id, action, outcome"),
          textAreaInput("pb_data_not_to_save", "Data NOT to save", rows = 2, width = "100%",
            value = "real names, emails, grades, demographic info, accommodations")
        ),
        div(class = "form-section",
          h3("Technical Preferences"),
          fluidRow(
            column(6,
              selectInput("pb_tech_stack", "Preferred tech stack",
                choices = TECH_STACKS, width = "100%")
            ),
            column(6,
              radioButtons("pb_local_or_hosted", "Local or hosted",
                choices = c("Local first",
                            "Hosted (Shiny Server / shinyapps.io)",
                            "Either"),
                selected = "Local first")
            )
          ),
          textAreaInput("pb_constraints", "Known constraints", rows = 2, width = "100%",
            placeholder = "Any known limitations? (must run on Windows laptop, no internet in classroom, ...)"),
          br(),
          actionButton("generate_prompt_btn", "Generate Prompt", class = "btn-primary",
            icon = icon("wand-magic-sparkles"))
        ),

        # Prompt output – hidden until button clicked
        conditionalPanel(
          condition = "input.generate_prompt_btn > 0",
          div(class = "form-section",
            h3("Your Prompt"),
            p(style = "color:#666; margin-top:-8px; margin-bottom:14px;",
              "Copy and paste this prompt into Claude Code, Codex, Cursor, or another agentic AI coding tool."),
            verbatimTextOutput("generated_prompt"),
            div(class = "copy-btn-row",
              tags$button(
                id = "copy_prompt_btn",
                onclick = "copyPrompt()",
                class = "btn-secondary",
                "Copy to clipboard"
              )
            )
          )
        )
      ),

      # ── Tab 3: Gallery ───────────────────────────────────────────────────────
      tabPanel("Gallery",
        br(),
        h2("Instructor App Gallery"),
        p(style = "color:#555;",
          "Browse apps built by instructors using the kit. Submit your own for review below."),
        uiOutput("gallery_cards"),

        # Submit form
        div(class = "card",
          h3("Submit Your App"),
          p(style = "color:#666; margin-top:-8px; margin-bottom:16px;",
            "Built something with the kit? Share it for other instructors to discover and remix."),
          fluidRow(
            column(6,
              textInput("gallery_title", tags$span("Title ", tags$span(style = "color:#e74c3c;", "*")),
                placeholder = "Your app name")
            ),
            column(6,
              selectInput("gallery_category", "Category", choices = ACTIVITY_TYPES, width = "100%")
            )
          ),
          textAreaInput("gallery_description", "Description", rows = 3, width = "100%",
            placeholder = "What does your app do? What classroom problem does it solve?"),
          fluidRow(
            column(6,
              textInput("gallery_url", "URL", placeholder = "https://...")
            ),
            column(6,
              textInput("gallery_submitter", "Your name (optional)", placeholder = "Anonymous")
            )
          ),
          actionButton("submit_gallery_btn", "Submit for Review", class = "btn-primary"),
          uiOutput("gallery_submit_status")
        ),

        # Admin panel (password-gated, inline)
        div(class = "admin-section",
          uiOutput("admin_panel")
        )
      ),

      # ── Tab 4: Docs ──────────────────────────────────────────────────────────
      tabPanel("Docs",
        br(),
        h2("Kit Documentation"),
        p(style = "color:#555;",
          "Browse the files that make up the AI Teaching Tool Demo Kit."),
        tabsetPanel(
          type = "tabs",
          tabPanel("Overview",
            br(),
            div(class = "doc-content card", uiOutput("doc_readme"))
          ),
          tabPanel("Prompt Template",
            br(),
            div(class = "doc-content card", uiOutput("doc_prompt"))
          ),
          tabPanel("Privacy Guide",
            br(),
            div(class = "doc-content card", uiOutput("doc_privacy"))
          ),
          tabPanel("Testing Checklist",
            br(),
            div(class = "doc-content card", uiOutput("doc_testing"))
          ),
          tabPanel("Adaptation Guide",
            br(),
            div(class = "doc-content card", uiOutput("doc_adaptation"))
          ),
          tabPanel("Review Form",
            br(),
            div(class = "doc-content card", uiOutput("doc_review"))
          )
        )
      )
    ) # tabsetPanel
  ) # main-container
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Admin auth ─────────────────────────────────────────────────────────────
  admin_logged_in <- reactiveVal(FALSE)

  observeEvent(input$admin_login_btn, {
    pwd <- input$admin_password %||% ""
    if (nzchar(trimws(pwd)) && bcrypt::checkpw(pwd, ADMIN_PWD_HASH)) {
      admin_logged_in(TRUE)
    } else {
      showNotification("Incorrect password.", type = "error", duration = 3)
    }
  })

  observeEvent(input$admin_logout_btn, {
    admin_logged_in(FALSE)
  })

  # ── Gallery refresh counter ────────────────────────────────────────────────
  gallery_refresh <- reactiveVal(0)

  # ── Submit new app ─────────────────────────────────────────────────────────
  gallery_submit_msg <- reactiveVal(NULL)

  observeEvent(input$submit_gallery_btn, {
    title <- trimws(input$gallery_title %||% "")
    if (!nzchar(title)) {
      gallery_submit_msg(list(type = "error", msg = "Title is required."))
      return()
    }
    db_exec(
      "INSERT INTO gallery_submissions (title, description, category, url, submitter_name)
       VALUES (?, ?, ?, ?, ?)",
      params = list(
        title,
        trimws(input$gallery_description %||% ""),
        input$gallery_category %||% "",
        trimws(input$gallery_url %||% ""),
        trimws(input$gallery_submitter %||% "")
      )
    )
    gallery_submit_msg(list(
      type = "success",
      msg  = "Submitted! Your app will appear in the gallery after review."
    ))
    updateTextInput(session, "gallery_title", value = "")
    updateTextAreaInput(session, "gallery_description", value = "")
    updateTextInput(session, "gallery_url", value = "")
    updateTextInput(session, "gallery_submitter", value = "")
    gallery_refresh(gallery_refresh() + 1)
  })

  output$gallery_submit_status <- renderUI({
    msg <- gallery_submit_msg()
    if (is.null(msg)) return(NULL)
    cls <- if (msg$type == "error") "status-error" else "status-success"
    div(class = cls, msg$msg)
  })

  # ── Admin approve / reject (via delegated JS click -> Shiny message) ───────
  observeEvent(input$admin_action, {
    req(admin_logged_in())
    action <- input$admin_action$action
    id     <- as.integer(input$admin_action$id)
    if (action == "approve") {
      db_exec("UPDATE gallery_submissions SET status = 'approved' WHERE id = ?",
              params = list(id))
      showNotification("Submission approved.", type = "message", duration = 2)
    } else if (action == "reject") {
      db_exec("UPDATE gallery_submissions SET status = 'rejected' WHERE id = ?",
              params = list(id))
      showNotification("Submission rejected.", type = "warning", duration = 2)
    }
    gallery_refresh(gallery_refresh() + 1)
  })

  # ── Gallery cards (approved) ───────────────────────────────────────────────
  output$gallery_cards <- renderUI({
    gallery_refresh()
    rows <- db_query(
      "SELECT * FROM gallery_submissions WHERE status = 'approved' ORDER BY submitted_at DESC"
    )
    if (nrow(rows) == 0) {
      return(div(class = "empty-state",
        "No approved apps yet. Be the first to submit one below!"))
    }
    cards <- lapply(seq_len(nrow(rows)), function(i) {
      r          <- rows[i, ]
      preview_id <- paste0("gallery_preview_", r$id)
      has_url    <- nzchar(r$url %||% "")
      by_line    <- nzchar(r$submitter_name %||% "")
      div(class = "gallery-card card",
        div(class = "gallery-card-meta",
          h3(r$title),
          span(class = "badge", r$category %||% "")
        ),
        if (nzchar(r$description %||% ""))
          p(style = "margin:0 0 4px 0; color:#444; font-size:0.9rem;", r$description),
        if (by_line)
          p(class = "submitter", paste0("by ", r$submitter_name)),
        div(class = "gallery-card-actions",
          if (has_url)
            tags$a(href = r$url, target = "_blank", rel = "noopener",
                   class = "btn-link", "Open →"),
          if (has_url)
            tags$button(
              class   = "btn-secondary",
              onclick = paste0("togglePreview('", preview_id, "')"),
              "Preview"
            )
        ),
        if (has_url)
          div(
            id    = preview_id,
            style = "display:none; margin-top:12px;",
            tags$iframe(
              src    = r$url,
              width  = "100%",
              height = "420px",
              style  = "border:1px solid #ddd; border-radius:5px;"
            )
          )
      )
    })
    div(class = "gallery-grid", cards)
  })

  # ── Admin panel ───────────────────────────────────────────────────────────
  output$admin_panel <- renderUI({
    if (!admin_logged_in()) {
      div(class = "card",
        h3("Admin"),
        p(style = "color:#888; font-size:0.88rem; margin-top:-8px; margin-bottom:14px;",
          "Instructors: log in to approve or reject pending gallery submissions."),
        passwordInput("admin_password", "Password", placeholder = "Admin password"),
        actionButton("admin_login_btn", "Login", class = "btn-primary")
      )
    } else {
      gallery_refresh()  # re-run when gallery changes
      pending <- db_query(
        "SELECT * FROM gallery_submissions WHERE status = 'pending' ORDER BY submitted_at ASC"
      )
      tagList(
        div(class = "admin-header",
          h3("Pending Submissions"),
          actionButton("admin_logout_btn", "Logout", class = "btn-secondary")
        ),
        if (nrow(pending) == 0) {
          div(class = "empty-state", "No pending submissions. Check back later.")
        } else {
          lapply(seq_len(nrow(pending)), function(i) {
            r       <- pending[i, ]
            has_url <- nzchar(r$url %||% "")
            by_line <- nzchar(r$submitter_name %||% "")
            div(class = "admin-pending-card",
              div(style = "display:flex; align-items:flex-start; justify-content:space-between; gap:8px;",
                h4(r$title),
                span(class = "badge", r$category %||% "")
              ),
              if (nzchar(r$description %||% "")) p(r$description),
              p(strong("URL: "),
                if (has_url)
                  tags$a(href = r$url, target = "_blank", rel = "noopener", r$url)
                else
                  tags$em("none")
              ),
              p(strong("Submitted by: "),
                if (by_line) r$submitter_name else tags$em("Anonymous")
              ),
              p(strong("Submitted at: "), r$submitted_at),
              div(class = "admin-actions",
                tags$button(
                  class     = "btn-approve approve-btn",
                  `data-id` = as.character(r$id),
                  "Approve"
                ),
                tags$button(
                  class     = "btn-reject reject-btn",
                  `data-id` = as.character(r$id),
                  "Reject"
                )
              )
            )
          })
        }
      )
    }
  })

  # ── Prompt builder ────────────────────────────────────────────────────────
  prompt_text <- eventReactive(input$generate_prompt_btn, {
    paste0(
      "I want you to build the smallest working prototype of a classroom teaching tool.\n\n",
      "This is for a specific instructor-owned classroom activity, not a commercial edtech platform. ",
      "Build a local, inspectable tool that I can test with fake data before considering classroom use.\n\n",
      "Course: ",                    trimws(input$pb_course           %||% ""), "\n",
      "Class size: ",                         input$pb_class_size     %||% "",  "\n",
      "Activity type: ",                      input$pb_activity_type  %||% "",  "\n",
      "Learning goal: ",             trimws(input$pb_learning_goal    %||% ""), "\n",
      "Student actions: ",           trimws(input$pb_student_actions  %||% ""), "\n",
      "Instructor actions: ",        trimws(input$pb_instructor_actions %||% ""), "\n",
      "Public display: ",            trimws(input$pb_public_display   %||% ""), "\n",
      "Private instructor controls: ", trimws(input$pb_private_controls %||% ""), "\n",
      "Scoring or payoff rules: ",   trimws(input$pb_scoring_rules    %||% ""), "\n",
      "Data to save: ",              trimws(input$pb_data_to_save     %||% ""), "\n",
      "Data not to save: ",          trimws(input$pb_data_not_to_save %||% ""), "\n",
      "Preferred tech stack: ",               input$pb_tech_stack     %||% "",  "\n",
      "Local or hosted use: ",                input$pb_local_or_hosted %||% "", "\n",
      "Known constraints: ",         trimws(input$pb_constraints      %||% ""), "\n\n",
      "Requirements:\n",
      "- Use fake data only.\n",
      "- Do not ask for real student names, emails, grades, accommodations, ",
        "demographic information, or private notes.\n",
      "- Build a minimal working version first.\n",
      "- Keep activity rules, parameters, and scoring logic transparent.\n",
      "- Store raw data before summaries.\n",
      "- Use local CSV, JSON, or SQLite storage unless I request something else.\n",
      "- Create student-facing and instructor-facing views when needed.\n",
      "- Include a reset or test mode.\n",
      "- Include exportable raw data.\n",
      "- Avoid unnecessary frameworks or external services.\n",
      "- Write a README that explains how to install, run, test, reset, ",
        "and export data from the prototype.\n\n",
      "Please start by inspecting the project structure, then propose the smallest viable ",
      "implementation plan. After that, implement it and test it with fake data."
    )
  })

  output$generated_prompt <- renderText({ prompt_text() })

  # ── Docs ──────────────────────────────────────────────────────────────────
  output$overview_readme  <- renderUI({ render_md(kit_files$readme) })
  output$doc_readme       <- renderUI({ render_md(kit_files$readme) })
  output$doc_prompt       <- renderUI({ render_md(kit_files$prompt_tmpl) })
  output$doc_privacy      <- renderUI({ render_md(kit_files$privacy) })
  output$doc_testing      <- renderUI({ render_md(kit_files$testing) })
  output$doc_adaptation   <- renderUI({ render_md(kit_files$adaptation) })
  output$doc_review       <- renderUI({ render_md(kit_files$review) })
}

shinyApp(ui, server)
