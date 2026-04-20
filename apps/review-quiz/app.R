try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
# app.R — Review Quiz
# Real-time multiple-choice quiz for in-class review.
# Instructor controls pacing (Admin tab); students answer on phones/laptops.
# Questions: upload a CSV via Admin tab, or fall back to built-in defaults.
# Auth: shared finalqdata.sqlite (same users as price-index app).

library(shiny); library(DBI); library(RSQLite); library(bcrypt); library(dplyr); library(ggplot2); library(DT)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b
logf   <- function(...) cat(format(Sys.time()), "-", paste(...), "\n", file = stderr())

# ── Built-in questions (fallback when no CSV has been uploaded) ───────────────
QUESTIONS_DEFAULT <- list(
  list(
    num     = 1L,
    topic   = "GDP Components",
    text    = "A U.S. firm licenses software to a German company. Which GDP component does this enter?",
    opts    = c(A = "C — Consumption",
                B = "I — Investment",
                C = "G — Government spending",
                D = "NX — Net Exports"),
    correct = "D",
    explain = "A foreign buyer purchases a domestically produced service → U.S. export → NX."
  ),
  list(
    num     = 2L,
    topic   = "Real vs. Nominal GDP",
    text    = "Nominal GDP: $400B → $440B. GDP deflator: 100 → 110. Real GDP growth was:",
    opts    = c(A = "10%", B = "5%", C = "0%", D = "−10%"),
    correct = "C",
    explain = "Real GDP = $440B / 1.10 = $400B — same as the prior year. All of the nominal gain was inflation."
  ),
  list(
    num     = 3L,
    topic   = "Solow Model",
    text    = "In the Solow model a higher savings rate permanently raises:",
    opts    = c(A = "The long-run growth rate of output",
                B = "The steady-state level of output per worker",
                C = "Both the growth rate and the level",
                D = "Neither — savings rate has no long-run effect"),
    correct = "B",
    explain = "Higher s raises steady-state k* and y* (a level effect), but the long-run growth rate is pinned by technology — same for all savings rates."
  ),
  list(
    num     = 4L,
    topic   = "Rule of 70 / CAGR",
    text    = "Real GDP per capita grew from $8,000 to $16,000 over 20 years. Using the Rule of 70, what annual growth rate does this imply?",
    opts    = c(A = "1.75%", B = "2.5%", C = "3.5%", D = "5%"),
    correct = "C",
    explain = "GDP doubled in 20 years → g ≈ 70/20 = 3.5%. CAGR formula confirms: (16/8)^(1/20) − 1 = 2^0.05 − 1 ≈ 3.5%."
  ),
  list(
    num     = 5L,
    topic   = "Business Cycle Phases",
    text    = "GDP has been rising for several quarters and growth this quarter is approximately zero — the economy is about to turn downward. This turning point is called a:",
    opts    = c(A = "Trough — the end of recession",
                B = "Recession — output is falling",
                C = "Expansion — output is rising",
                D = "Peak — the end of expansion where g ≈ 0"),
    correct = "D",
    explain = "Peak = end of expansion (g ≈ 0, about to fall). Trough = end of recession (g ≈ 0, about to rise). Expansion = rising output; contraction/recession = falling output."
  ),
  list(
    num     = 6L,
    topic   = "Keynesian Multiplier",
    text    = "MPC = 0.80. Congress raises spending by $150 billion. By how much does GDP change?",
    opts    = c(A = "$150 billion", B = "$450 billion",
                C = "$600 billion", D = "$750 billion"),
    correct = "D",
    explain = "Multiplier = 1/(1−0.8) = 5.  ΔGDP = 5 × $150B = $750B."
  ),
  list(
    num     = 7L,
    topic   = "Fiscal Policy",
    text    = "A state cuts spending during a recession to balance its budget. This is:",
    opts    = c(A = "Counter-cyclical — it stabilizes the economy",
                B = "Pro-cyclical — it amplifies the downturn",
                C = "Neutral — spending and taxes move together",
                D = "An automatic stabilizer"),
    correct = "B",
    explain = "Pro-cyclical: cutting spending during a contraction reduces AD further and deepens the recession."
  ),
  list(
    num     = 8L,
    topic   = "Quantity Theory of Money",
    text    = "The money supply M doubles. V and Y remain fixed. The price level P:",
    opts    = c(A = "Stays the same",
                B = "Rises by 50%",
                C = "Doubles",
                D = "Quadruples"),
    correct = "C",
    explain = "MV = PY → doubling M with V, Y fixed means P must double. Quantity theory: money growth → proportional inflation."
  )
)

# length(questions_r()) is now dynamic — see load_questions() below

# ── Database ──────────────────────────────────────────────────────────────────
app_data_dir <- local({
  dir <- NULL
  function() {
    if (!is.null(dir)) return(dir)
    root <- Sys.getenv("CONNECT_CONTENT_DIR", unset = getwd())
    d    <- file.path(root, "data")
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    dir  <<- normalizePath(d, winslash = "/", mustWork = TRUE)
    dir
  }
})

DB_PATH <- file.path(app_data_dir(), "finalqdata.sqlite")
conn    <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn))
    conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  conn
}
db_exec  <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

ensure_state <- function() {
  db_exec("INSERT OR REPLACE INTO quiz_state(id, current_q, revealed)
    SELECT 1,
      COALESCE((SELECT current_q FROM quiz_state WHERE id=1), 1),
      COALESCE((SELECT revealed  FROM quiz_state WHERE id=1), 0);")
}

init_db <- function() {
  db_exec("CREATE TABLE IF NOT EXISTS quiz_state (
    id         INTEGER PRIMARY KEY CHECK(id = 1),
    current_q  INTEGER DEFAULT 1,
    revealed   INTEGER DEFAULT 0,
    updated_at TEXT    DEFAULT (CURRENT_TIMESTAMP)
  );")
  ensure_state()

  db_exec("CREATE TABLE IF NOT EXISTS quiz_responses (
    response_id  INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id      TEXT    NOT NULL,
    q_num        INTEGER NOT NULL,
    answer       TEXT    NOT NULL,
    submitted_at TEXT    DEFAULT (CURRENT_TIMESTAMP),
    UNIQUE(user_id, q_num)
  );")

  db_exec("CREATE TABLE IF NOT EXISTS quiz_aliases (
    user_id TEXT PRIMARY KEY,
    alias   TEXT NOT NULL
  );")

  db_exec("CREATE TABLE IF NOT EXISTS quiz_questions (
    q_num   INTEGER PRIMARY KEY,
    topic   TEXT NOT NULL,
    text    TEXT NOT NULL,
    opt_a   TEXT NOT NULL,
    opt_b   TEXT NOT NULL,
    opt_c   TEXT NOT NULL,
    opt_d   TEXT NOT NULL,
    correct TEXT NOT NULL,
    explain TEXT
  );")
}
init_db()

# ── Question loading ──────────────────────────────────────────────────────────
# Returns questions from the DB if any have been uploaded, else built-in defaults.
load_questions <- function() {
  rows <- db_query("SELECT * FROM quiz_questions ORDER BY q_num;")
  if (!nrow(rows)) return(QUESTIONS_DEFAULT)
  lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    list(
      num     = as.integer(r$q_num),
      topic   = r$topic,
      text    = r$text,
      opts    = c(A = r$opt_a, B = r$opt_b, C = r$opt_c, D = r$opt_d),
      correct = r$correct,
      explain = r$explain %||% ""
    )
  })
}

# Parse and validate a CSV file path → data frame ready for DB insert, or stop() on error.
parse_question_csv <- function(path) {
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                 error = function(e) stop("Could not read CSV: ", conditionMessage(e)))
  required <- c("q_num", "topic", "text", "opt_a", "opt_b", "opt_c", "opt_d", "correct")
  missing  <- setdiff(required, names(df))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  df$correct <- toupper(trimws(df$correct))
  bad <- df$correct[!df$correct %in% c("A","B","C","D")]
  if (length(bad)) stop("'correct' must be A/B/C/D. Found: ", paste(unique(bad), collapse = ", "))
  if (!"explain" %in% names(df)) df$explain <- ""
  df[, c("q_num","topic","text","opt_a","opt_b","opt_c","opt_d","correct","explain")]
}

# ── Helpers ───────────────────────────────────────────────────────────────────
get_state <- function()
  db_query("SELECT current_q, revealed FROM quiz_state WHERE id=1;")

get_scores <- function(questions = load_questions()) {
  resps <- db_query("
    SELECT qr.user_id,
           COALESCE(qa.alias, u.display_name) AS display_name,
           qr.q_num, qr.answer
    FROM quiz_responses qr
    JOIN users u ON qr.user_id = u.user_id
    LEFT JOIN quiz_aliases qa ON qr.user_id = qa.user_id;")
  if (!nrow(resps)) return(data.frame(user_id=character(), display_name=character(), score=integer()))
  cm <- setNames(
    vapply(questions, `[[`, character(1), "correct"),
    vapply(questions, `[[`, integer(1),   "num"))
  resps$is_correct <- cm[as.character(resps$q_num)] == resps$answer
  resps |>
    dplyr::group_by(user_id, display_name) |>
    dplyr::summarise(score = sum(is_correct, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(score))
}

# ── CSS ───────────────────────────────────────────────────────────────────────
quiz_css <- HTML("
  .ans-btn  { width:100%; margin:5px 0; font-size:1.05em; padding:12px 16px;
               text-align:left; border-radius:6px; }
  .ans-selected { background:#1B4F72 !important; color:#fff !important;
                  border-color:#1B4F72 !important; opacity:1 !important; }
  .ans-btn:disabled { opacity:0.45; }
  .q-text   { font-size:1.15em; line-height:1.6; margin:14px 0; }
  .topic-pill { background:#951829; color:#fff; border-radius:12px;
                padding:2px 10px; font-size:0.82em; font-weight:600; }
  .status-box { border-radius:8px; padding:14px 18px; font-size:1.1em;
                font-weight:600; margin-top:10px; }
  .status-waiting  { background:#fff3cd; color:#856404; border:1px solid #ffc107; }
  .status-correct  { background:#d4edda; color:#155724; border:1px solid #28a745; }
  .status-wrong    { background:#f8d7da; color:#721c24; border:1px solid #dc3545; }
  .explain-box     { background:#f8f9fa; border-left:4px solid #6c757d;
                     padding:10px 14px; margin-top:10px; font-size:0.95em; color:#555; }
  .score-chip { background:#641A2B; color:#fff; border-radius:12px;
                padding:3px 12px; font-size:0.9em; font-weight:600; margin-right:10px; }
  .big-count  { font-size:3em; font-weight:700; color:#951829; line-height:1; }
")

# ── Login UI ──────────────────────────────────────────────────────────────────
login_ui <- function(msg = NULL) {
  fluidPage(
    tags$head(tags$style(quiz_css)),
    titlePanel("Midterm 2 Review Quiz"),
    if (!is.null(msg)) div(style = "color:#b00020; font-weight:bold; margin-bottom:10px;", msg),
    div(style = "max-width:320px;",
      textInput("login_user", "Username"),
      passwordInput("login_pw", "Password"),
      actionButton("login_btn", "Sign in", class = "btn-primary"),
      tags$p(tags$small("Same login as the Price Index app."))
    )
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(quiz_css)),
  uiOutput("app_ui")
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(authed = FALSE, user_id = NULL, name = NULL, alias = NULL, is_admin = FALSE)

  # ── Auth ────────────────────────────────────────────────────────────────────
  output$app_ui <- renderUI({
    if (!rv$authed) return(login_ui())

    tabs <- list(
      tabPanel("My Answer",   uiOutput("alias_ui"), uiOutput("student_panel")),
      tabPanel("Leaderboard", uiOutput("display_panel"))
    )
    if (rv$is_admin) tabs <- c(tabs, list(tabPanel("Admin", uiOutput("admin_panel"))))

    do.call(navbarPage, c(
      list(title = "Review Quiz",
           header = div(style = "float:right; margin-top:4px;",
             uiOutput("score_chip", inline = TRUE),
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
    al <- db_query("SELECT alias FROM quiz_aliases WHERE user_id=?;", list(row$user_id[1]))
    rv$alias <- if (nrow(al)) al$alias[1] else NULL
    logf("LOGIN:", row$user_id[1])
  })

  observeEvent(input$logout_btn, {
    rv$authed <- FALSE; rv$user_id <- NULL; rv$name <- NULL; rv$alias <- NULL; rv$is_admin <- FALSE
  })

  # ── Reactive polls ───────────────────────────────────────────────────────────
  state_r <- reactivePoll(2000, session,
    checkFunc  = function() db_query("SELECT updated_at FROM quiz_state WHERE id=1;")$updated_at[1],
    valueFunc  = get_state
  )

  responses_r <- reactivePoll(2000, session,
    checkFunc  = function() db_query("SELECT COUNT(*) n FROM quiz_responses;")$n[1],
    valueFunc  = function() db_query("
      SELECT qr.user_id, qr.q_num, qr.answer, qr.submitted_at, u.display_name
      FROM quiz_responses qr JOIN users u ON qr.user_id = u.user_id;")
  )

  questions_r <- reactivePoll(5000, session,
    checkFunc = function() db_query("SELECT COUNT(*) n FROM quiz_questions;")$n[1],
    valueFunc = load_questions
  )

  # ── Score chip (navbar) ──────────────────────────────────────────────────────
  output$score_chip <- renderUI({
    req(rv$user_id); responses_r()
    sc  <- get_scores(questions_r())
    my  <- sc$score[sc$user_id == rv$user_id]
    my  <- if (length(my)) my[1] else 0L
    qs  <- state_r()$current_q[1]
    span(class = "score-chip",
         paste0(rv$alias %||% rv$name, "  ", my, "/", qs - 1, " correct"))
  })

  # ── Alias input (separate so it isn't wiped by state_r() re-renders) ─────────
  output$alias_ui <- renderUI({
    req(rv$authed)
    div(style = "max-width:660px; padding:10px 0 4px;",
      div(style = "display:flex; align-items:center; gap:8px;",
        textInput("alias_input", NULL,
                  value       = rv$alias %||% rv$name,
                  placeholder = "Leaderboard name",
                  width       = "220px"),
        actionButton("save_alias_btn", "Save name", class = "btn-sm btn-default")
      )
    )
  })

  # ── Student panel ─────────────────────────────────────────────────────────────
  output$student_panel <- renderUI({
    req(rv$authed)
    responses_r()   # invalidate when any response is submitted
    qs   <- state_r()
    qnum <- max(1L, min(as.integer(qs$current_q[1] %||% 1L), length(questions_r())))
    rev  <- as.logical(qs$revealed[1])
    q    <- questions_r()[[qnum]]
    uid  <- rv$user_id

    existing <- db_query(
      "SELECT answer FROM quiz_responses WHERE user_id=? AND q_num=?;",
      list(uid, qnum))
    answered <- nrow(existing) > 0
    my_ans   <- if (answered) existing$answer[1] else NULL

    div(style = "max-width:660px; padding:16px 0;",
      # Question header
      tags$p(
        span(class = "topic-pill", q$topic),
        tags$span(style = "color:#888; margin-left:10px; font-size:0.88em;",
                  paste0("Q", qnum, " of ", length(questions_r())))
      ),
      tags$p(class = "q-text", q$text),

      if (!answered) {
        # Answer buttons
        tagList(
          lapply(names(q$opts), function(ltr) {
            actionButton(
              inputId = paste0("ans_btn_", ltr),
              label   = paste0(ltr, ".  ", q$opts[[ltr]]),
              class   = "btn btn-outline-secondary ans-btn",
              onclick = sprintf(
                "this.classList.add('ans-selected');
                 document.querySelectorAll('.ans-btn').forEach(function(b){b.disabled=true;});
                 Shiny.setInputValue('submit_ans','%s',{priority:'event'})", ltr)
            )
          }),
          tags$p(style = "color:#888; font-size:0.85em; margin-top:6px;",
                 "Tap your answer to lock in — no changes after submission.")
        )
      } else if (!rev) {
        div(class = "status-box status-waiting",
            "\U0001F512  Locked in: ", tags$strong(my_ans),
            "  \u2014  waiting for reveal\u2026")
      } else {
        # Revealed
        is_right <- (my_ans == q$correct)
        tagList(
          div(class = paste("status-box", if (is_right) "status-correct" else "status-wrong"),
            if (is_right)
              tagList("\u2705  Correct! — ", tags$strong(q$opts[[q$correct]]))
            else
              tagList("\u274C  Your answer: ", tags$strong(my_ans),
                      "  |  Correct: ", tags$strong(q$correct), " — ", q$opts[[q$correct]])
          ),
          div(class = "explain-box", q$explain)
        )
      }
    )
  })

  observeEvent(input$submit_ans, {
    req(rv$authed)
    qs   <- isolate(state_r())
    qnum <- max(1L, min(as.integer(qs$current_q[1] %||% 1L), length(questions_r())))
    if (as.logical(qs$revealed[1])) {
      showNotification("Answer already revealed — too late!", type = "warning"); return()
    }
    ltr <- input$submit_ans
    if (!ltr %in% c("A", "B", "C", "D")) return()
    tryCatch(
      db_exec(
        "INSERT INTO quiz_responses(user_id, q_num, answer)
         VALUES(?,?,?) ON CONFLICT(user_id, q_num) DO NOTHING;",
        list(rv$user_id, as.integer(qnum), ltr)),
      error = function(e) showNotification(paste("Error:", e$message), type = "error")
    )
  })

  # ── Leaderboard / display panel ───────────────────────────────────────────────
  output$display_panel <- renderUI({
    req(rv$authed)
    qs  <- state_r()
    rev <- as.logical(qs$revealed[1])

    tagList(
      fluidRow(
        column(5,
          tags$h5("Responses in"),
          div(class = "big-count", textOutput("resp_count", inline = TRUE)),
          tags$p(style = "color:#888;", "students answered this question")
        ),
        column(7,
          if (rev) tagList(
            tags$h5("Answer Distribution"),
            plotOutput("dist_plot", height = "220px")
          ) else
            div(style = "color:#aaa; padding:40px 0; text-align:center;",
                "\u23F3  Distribution shown after reveal")
        )
      ),
      tags$hr(),
      tags$h5("Leaderboard"),
      DT::DTOutput("leaderboard_tbl")
    )
  })

  output$resp_count <- renderText({
    qs   <- state_r()
    resp <- responses_r()
    sum(resp$q_num == qs$current_q[1])
  })

  output$dist_plot <- renderPlot({
    qs   <- state_r(); qnum <- qs$current_q[1]
    resp <- responses_r()
    q    <- questions_r()[[qnum]]
    resp_q <- resp[resp$q_num == qnum, , drop = FALSE]

    counts <- data.frame(
      ltr   = names(q$opts),
      label = paste0(names(q$opts), ": ", substr(unname(q$opts), 1, 40)),
      n     = vapply(names(q$opts), function(x) sum(resp_q$answer == x), integer(1)),
      stringsAsFactors = FALSE
    )
    counts$correct <- counts$ltr == q$correct
    counts$label   <- factor(counts$label, levels = counts$label)

    ggplot(counts, aes(x = label, y = n, fill = correct)) +
      geom_col(alpha = 0.9, width = 0.6) +
      geom_text(aes(label = n), vjust = -0.4, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("FALSE" = "#adb5bd", "TRUE" = "#2d6a4f"),
                        guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      scale_x_discrete(labels = function(x)
        vapply(x, function(s) paste(strwrap(s, 22), collapse="\n"), character(1))) +
      labs(x = NULL, y = "# responses",
           subtitle = paste0("Correct: ", q$correct, " \u2014 ", q$opts[[q$correct]])) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.x = element_blank(),
            plot.subtitle = element_text(color = "#2d6a4f", face = "bold"))
  })

  output$leaderboard_tbl <- DT::renderDT({
    responses_r()
    sc <- get_scores(questions_r())
    if (!nrow(sc))
      return(data.frame(Message = "No answers submitted yet."))
    sc |>
      dplyr::mutate(Rank = dplyr::row_number()) |>
      dplyr::select(Rank, Name = display_name, `Correct` = score)
  }, rownames = FALSE,
     options  = list(dom = "t", pageLength = 40,
                     order = list(list(0L, "asc"))))

  # ── Admin panel ───────────────────────────────────────────────────────────────
  output$admin_panel <- renderUI({
    req(rv$is_admin)
    qs   <- state_r()
    qnum <- max(1L, min(as.integer(qs$current_q[1] %||% 1L), length(questions_r())))
    rev  <- as.logical(qs$revealed[1])
    q    <- questions_r()[[qnum]]

    tagList(
      tags$h4("Quiz Controls"),
      wellPanel(
        fluidRow(
          column(4,
            tags$p(tags$strong("Question: "), qnum, " / ", length(questions_r())),
            tags$p(tags$strong("Status: "),
                   if (rev) span(style="color:#2d6a4f;", "\u2713 Revealed")
                   else span(style="color:#856404;", "\u23F3 Waiting")),
            tags$br(),
            actionButton("reveal_btn",
              label = if (rev) "Hide Answer" else "Reveal Answer \u2714",
              class = if (rev) "btn-warning btn-lg" else "btn-success btn-lg",
              width = "100%")
          ),
          column(4,
            tags$br(),
            actionButton("prev_btn", "\u2190 Previous",
                         class = "btn-default btn-lg", width = "100%",
                         disabled = if (qnum == 1L) "disabled" else NULL),
            tags$br(), tags$br(),
            actionButton("next_btn", "Next \u2192",
                         class = "btn-primary btn-lg", width = "100%",
                         disabled = if (qnum == length(questions_r())) "disabled" else NULL),
            tags$br(), tags$br(),
            selectInput("jump_q", NULL,
              choices  = setNames(
                seq_len(length(questions_r())),
                vapply(questions_r(), function(q) paste0("Q", q$num, " — ", q$topic), character(1))),
              selected = qnum, width = "100%")
          ),
          column(4,
            tags$br(),
            actionButton("reset_btn", "Reset All", class = "btn-danger", width = "100%"),
            tags$br(), tags$br(),
            actionButton("reinit_btn", "Fix DB State", class = "btn-warning btn-sm", width = "100%"),
            tags$br(), tags$br(),
            downloadButton("dl_responses", "Download CSV", class = "btn-sm btn-default")
          )
        )
      ),
      tags$details(
        tags$summary(
          style = "cursor:pointer; font-weight:600; font-size:1.05em;
                   padding:6px 0; list-style:none; user-select:none;",
          paste0("\u25B6  Q", qnum, " — ", q$topic, " (click to expand)")
        ),
        div(style = "padding:10px 0 4px 4px;",
          tags$p(class = "q-text", q$text),
          tags$ul(lapply(names(q$opts), function(ltr) {
            style <- if (ltr == q$correct)
              "color:#155724; font-weight:bold;" else "color:#555;"
            tags$li(style = style,
                    paste0(ltr, ". ", q$opts[[ltr]],
                           if (ltr == q$correct) "  \u2713" else ""))
          })),
          tags$p(class = "explain-box", q$explain)
        )
      ),

      tags$hr(),
      tags$h5("Upload Question Bank"),
      tags$p(style = "font-size:0.9em; color:#555;",
             "CSV columns (in any order): ",
             tags$code("q_num, topic, text, opt_a, opt_b, opt_c, opt_d, correct, explain"),
             ". The ", tags$code("explain"), " column is optional."),
      fileInput("upload_csv", NULL, accept = ".csv",
                placeholder = "Choose CSV file\u2026"),
      uiOutput("upload_preview_ui"),
      tags$p(style = "font-size:0.85em; color:#888;",
             "Loading a new bank resets to Q1 and clears all student responses.")
    )
  })

  set_state <- function(col, val)
    db_exec(sprintf(
      "UPDATE quiz_state SET %s=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;", col),
      list(val))

  observeEvent(input$reveal_btn, {
    req(rv$is_admin)
    rev <- as.logical(isolate(state_r())$revealed[1])
    set_state("revealed", if (rev) 0L else 1L)
  })

  observeEvent(input$next_btn, {
    req(rv$is_admin)
    qs <- isolate(state_r())
    if (qs$current_q[1] < length(questions_r()))
      db_exec("UPDATE quiz_state SET current_q=current_q+1, revealed=0,
               updated_at=CURRENT_TIMESTAMP WHERE id=1;")
  })

  observeEvent(input$jump_q, {
    req(rv$is_admin)
    target <- as.integer(input$jump_q)
    if (!is.na(target) && target != isolate(state_r())$current_q[1])
      db_exec("UPDATE quiz_state SET current_q=?, revealed=0,
               updated_at=CURRENT_TIMESTAMP WHERE id=1;", list(target))
  })

  observeEvent(input$prev_btn, {
    req(rv$is_admin)
    qs <- isolate(state_r())
    if (qs$current_q[1] > 1L)
      db_exec("UPDATE quiz_state SET current_q=current_q-1, revealed=0,
               updated_at=CURRENT_TIMESTAMP WHERE id=1;")
  })

  observeEvent(input$reset_btn, {
    req(rv$is_admin)
    showModal(modalDialog(
      title  = "Reset all quiz responses?",
      "This deletes every student answer and returns to Q1. Cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Yes, reset everything", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_reset, {
    req(rv$is_admin)
    db_exec("DELETE FROM quiz_responses;")
    db_exec("UPDATE quiz_state SET current_q=1, revealed=0,
             updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    removeModal()
    showNotification("Responses cleared, reset to Q1.", type = "message")
  })

  output$upload_preview_ui <- renderUI({
    req(rv$is_admin, input$upload_csv)
    result <- tryCatch(
      parse_question_csv(input$upload_csv$datapath),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      tagList(
        tags$p(style = "color:red; font-size:0.9em;",
               icon("circle-xmark"), " ", conditionMessage(result))
      )
    } else {
      tagList(
        tags$p(style = "color:green; font-size:0.9em;",
               icon("circle-check"),
               sprintf(" %d question(s) parsed OK.", nrow(result))),
        actionButton("load_questions_btn", "Load into quiz",
                     class = "btn-warning btn-sm"),
        actionButton("clear_questions_btn", "Restore defaults",
                     class = "btn-sm", style = "margin-left:6px;")
      )
    }
  })

  observeEvent(input$load_questions_btn, {
    req(rv$is_admin, input$upload_csv)
    questions <- tryCatch(
      parse_question_csv(input$upload_csv$datapath),
      error = function(e) NULL
    )
    req(!is.null(questions))
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, "DELETE FROM quiz_questions;")
    DBI::dbAppendTable(con, "quiz_questions", questions)
    db_exec("DELETE FROM quiz_responses;")
    db_exec("UPDATE quiz_state SET current_q=1, revealed=0,
             updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification(sprintf("%d questions loaded. Quiz reset to Q1.", nrow(questions)),
                     type = "message")
  })

  observeEvent(input$clear_questions_btn, {
    req(rv$is_admin)
    db_exec("DELETE FROM quiz_questions;")
    db_exec("DELETE FROM quiz_responses;")
    db_exec("UPDATE quiz_state SET current_q=1, revealed=0,
             updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Restored default questions. Quiz reset to Q1.", type = "message")
  })

  observeEvent(input$save_alias_btn, {
    req(rv$user_id)
    a <- trimws(input$alias_input %||% "")
    if (!nzchar(a)) { showNotification("Name cannot be blank.", type = "warning"); return() }
    db_exec("INSERT OR REPLACE INTO quiz_aliases(user_id, alias) VALUES(?, ?);",
            list(rv$user_id, a))
    rv$alias <- a
    showNotification(paste0("Leaderboard name set to: ", a), type = "message")
  })

  observeEvent(input$reinit_btn, {
    req(rv$is_admin)
    ensure_state()
    showNotification("DB state reinitialized — quiz is back at Q1.", type = "message")
  })

  output$dl_responses <- downloadHandler(
    filename = function() paste0("quiz_responses_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(db_query("
        SELECT u.display_name AS student, qr.q_num, qr.answer, qr.submitted_at
        FROM quiz_responses qr JOIN users u ON qr.user_id = u.user_id
        ORDER BY qr.q_num, qr.submitted_at;"),
        file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
