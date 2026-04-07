# app.R — class-job-picker
# Data layer migrated from Google Sheets → shared SQLite (finalqdata.sqlite)
# Google Sheets auth is still used for the summary write-back only.
#
# shiny::runApp(appDir = "C:/Users/kgcsp/OneDrive/Documents/Education/Teaching/shiny-apps/apps/class-job-picker", port = 3838, host = "127.0.0.1")

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(shiny, googlesheets4, dplyr, tidyr, tibble, jsonlite, DBI, RSQLite, bcrypt)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b

# ----------------------------
# SQLite CONNECTION (shared with final_question_reveal)
# ----------------------------
JOB_DB_PATH <- local({
  root <- Sys.getenv("CONNECT_CONTENT_DIR", unset = {
    # local dev: walk up to sibling app directory
    file.path(dirname(normalizePath(getwd())), "final_question_reveal")
  })
  file.path(root, "data", "finalqdata.sqlite")
})

job_conn <- NULL
get_job_con <- function() {
  if (is.null(job_conn) || !DBI::dbIsValid(job_conn))
    job_conn <<- DBI::dbConnect(RSQLite::SQLite(), JOB_DB_PATH)
  job_conn
}
jdb_exec  <- function(sql, p = NULL) DBI::dbExecute(get_job_con(), sql, params = p)
jdb_query <- function(sql, p = NULL) DBI::dbGetQuery(get_job_con(), sql, params = p)

# ----------------------------
# CONFIG (Google Sheets for summary write-back only)
# ----------------------------
SHEET_ID    <- Sys.getenv("CLASS_JOB_SHEET_ID")
CREDENTIALS <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
SERVICE_JSON <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")

get_gs_cred_path <- function() {
  p <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (nzchar(p) && file.exists(p)) return(p)
  js <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
  if (nzchar(js)) {
    tf <- tempfile(fileext = ".json")
    writeLines(js, tf)
    return(tf)
  }
  ""
}

gs4_deauth()
cred_path <- get_gs_cred_path()
if (nzchar(cred_path)) tryCatch(gs4_auth(path = cred_path), error = function(e) NULL)

# ----------------------------
# DEFAULT JOB LIST
# ----------------------------
# Jobs that appear in the "Class jobs" draw panel
DRAW_JOBS <- c(
  "last class summary",
  "materials summary 1", "materials summary 2", "materials summary 3",
  "note taker"
)
# Jobs tracked via their own tabs/manual entry — not in the main draw panel,
# and NOT counted toward the "max commits" cap
SPECIAL_JOBS <- c("cold call", "voluntary answer")
# Full list seeded into job_state on startup
DEFAULT_JOBS <- c(DRAW_JOBS, SPECIAL_JOBS)

# ----------------------------
# DB HELPER FUNCTIONS
# ----------------------------
sections_from_db <- function() {
  tryCatch(
    jdb_query("SELECT DISTINCT section FROM users
               WHERE section IS NOT NULL AND section != ''
               AND COALESCE(is_admin,0)=0
               ORDER BY section;")$section,
    error = function(e) character(0)
  )
}

init_job_defaults <- function(sections, jobs) {
  for (sec in sections) {
    for (job in jobs) {
      tryCatch(
        jdb_exec(
          "INSERT OR IGNORE INTO job_state(section, job, cycle_id, bag_json)
           VALUES(?,?,1,'[]')",
          list(as.character(sec), as.character(job))
        ),
        error = function(e) NULL
      )
    }
  }
}

# ----------------------------
# DATA LAYER
# ----------------------------
parse_bag <- function(bag_json) {
  if (is.null(bag_json) || is.na(bag_json) || bag_json == "" || bag_json == "[]")
    return(character(0))
  jsonlite::fromJSON(bag_json)
}

read_roster <- function(section_id) {
  jdb_query(
    "SELECT display_name FROM users
     WHERE section=? AND COALESCE(is_admin,0)=0
     ORDER BY display_name",
    list(as.character(section_id))
  )$display_name
}

read_state <- function(section_id, job_id = NULL) {
  if (is.null(job_id)) {
    jdb_query("SELECT * FROM job_state WHERE section=?",
              list(as.character(section_id)))
  } else {
    jdb_query("SELECT * FROM job_state WHERE section=? AND job=?",
              list(as.character(section_id), as.character(job_id)))
  }
}

write_state_job <- function(section_id, job_id, cycle_id_new, bag,
                            last_updated = Sys.time()) {
  bag_json <- jsonlite::toJSON(bag, auto_unbox = TRUE)
  jdb_exec(
    "INSERT INTO job_state(section, job, cycle_id, bag_json, last_updated)
     VALUES(?,?,?,?,?)
     ON CONFLICT(section, job) DO UPDATE SET
       cycle_id     = excluded.cycle_id,
       bag_json     = excluded.bag_json,
       last_updated = excluded.last_updated",
    list(as.character(section_id), as.character(job_id),
         as.integer(cycle_id_new), as.character(bag_json),
         as.character(last_updated))
  )
}

append_log <- function(rows_df) {
  for (i in seq_len(nrow(rows_df))) {
    cycle_val <- suppressWarnings(as.integer(rows_df$cycle_id[i]))
    if (is.na(cycle_val)) cycle_val <- 0L
    jdb_exec(
      "INSERT INTO job_log(logged_date, section, job, display_name, cycle_id)
       VALUES(?,?,?,?,?)",
      list(as.character(rows_df$date[i]),
           as.character(rows_df$section[i]),
           as.character(rows_df$job[i]),
           as.character(rows_df$name[i]),
           cycle_val)
    )
  }
}

# ----------------------------
# EXCLUSION HELPERS
# ----------------------------
compute_exclusions <- function(section_id, roster, max_commits = Inf) {
  lg <- jdb_query(
    "SELECT job, display_name FROM job_log
     WHERE section=? AND job NOT LIKE 'ADMIN__%'",
    list(as.character(section_id))
  )

  # Materials summary rotation uses all regular job entries
  matsumm_log    <- lg[grepl("materials summary", lg$job, ignore.case = TRUE), ]
  matsumm_counts <- table(factor(matsumm_log$display_name, levels = roster))
  min_matsumm    <- if (length(matsumm_counts) > 0) min(matsumm_counts) else 0
  matsumm_excluded <- names(matsumm_counts[matsumm_counts > min_matsumm])

  # Overcap counts all job entries (cold call and voluntary answer included)
  if (is.finite(max_commits) && max_commits > 0) {
    all_counts <- table(factor(lg$display_name, levels = roster))
    overcap_excluded <- names(all_counts[all_counts >= max_commits])
  } else {
    overcap_excluded <- character(0)
  }

  list(matsumm = matsumm_excluded, overcap = overcap_excluded)
}

# ----------------------------
# CORE LOGIC
# ----------------------------
ensure_bag_job <- function(section_id, job_id, absentees = character(0)) {
  job_id <- trimws(as.character(job_id))
  if (is.na(job_id) || job_id == "")
    stop("ensure_bag_job called with empty job. section_id=", section_id)

  roster <- read_roster(section_id)
  if (length(absentees) > 0) roster <- setdiff(roster, absentees)

  row <- read_state(section_id, job_id)
  if (nrow(row) == 0) {
    bag      <- character(0)
    cycle_id <- 0L
  } else {
    bag      <- parse_bag(row$bag_json[1])
    cycle_id <- as.integer(row$cycle_id[1]) %||% 0L
  }

  bag <- bag[bag %in% roster]

  if (length(bag) == 0) {
    cycle_id <- cycle_id + 1L
    bag <- sample(roster, size = length(roster), replace = FALSE)
    write_state_job(section_id, job_id, cycle_id, bag)
  }

  list(roster = roster, cycle_id = cycle_id, bag = bag)
}

draw_jobs_day <- function(section_id, jobs, absentees = character(0), exclusions = NULL) {
  roster <- read_roster(section_id)
  if (length(absentees) > 0) roster <- setdiff(roster, absentees)
  state  <- read_state(section_id)

  overcap_excluded <- if (!is.null(exclusions)) exclusions$overcap else character(0)
  matsumm_excluded <- if (!is.null(exclusions)) exclusions$matsumm else character(0)

  picked_today  <- character(0)
  assignments   <- setNames(character(length(jobs)), jobs)
  state_updates <- list()

  for (j in jobs) {
    row <- state[state$job == j, ]

    if (nrow(row) == 0) {
      bag      <- character(0)
      cycle_id <- 0L
    } else {
      bag      <- parse_bag(row$bag_json[1])
      cycle_id <- as.integer(row$cycle_id[1]) %||% 0L
    }
    bag <- bag[bag %in% roster]

    if (length(bag) == 0) {
      bag      <- sample(roster, length(roster))
      cycle_id <- cycle_id + 1L
    }

    avail <- setdiff(bag, picked_today)
    avail <- setdiff(avail, overcap_excluded)

    if (grepl("materials summary", j, ignore.case = TRUE)) {
      avail <- setdiff(avail, matsumm_excluded)
    }

    if (length(avail) == 0) avail <- setdiff(bag, picked_today)
    if (length(avail) == 0) stop("No eligible student for job: ", j, " in section: ", section_id)

    nm            <- sample(avail, 1)
    assignments[j] <- nm
    picked_today  <- c(picked_today, nm)

    state_updates[[j]] <- list(cycle_id = cycle_id, new_bag = setdiff(bag, nm))
  }

  list(assignments = assignments, state_updates = state_updates)
}

redraw_one <- function(section_id, current_assignments, job_to_redraw,
                       absentees = character(0)) {
  info   <- ensure_bag_job(section_id, job_to_redraw, absentees = absentees)
  roster <- read_roster(section_id)
  if (length(absentees) > 0) roster <- setdiff(roster, absentees)
  bag    <- info$bag

  taken_today <- unname(current_assignments)
  taken_today <- taken_today[!is.na(taken_today)]
  available   <- setdiff(bag, taken_today)

  if (length(available) == 0) {
    info2     <- ensure_bag_job(section_id, job_to_redraw, absentees = absentees)
    bag       <- info2$bag
    available <- setdiff(bag, taken_today)
  }

  if (length(available) == 0) stop("No one left to redraw from (bag exhausted).")

  new_name                          <- sample(available, 1)
  current_assignments[[job_to_redraw]] <- new_name

  list(assignments = current_assignments, roster = roster,
       cycle_id = info$cycle_id, bag = bag)
}

commit_jobs_day <- function(section_id, date, result) {
  section_id <- trimws(as.character(section_id))

  for (j in names(result$state_updates)) {
    upd <- result$state_updates[[j]]
    write_state_job(section_id, j, upd$cycle_id, upd$new_bag)
  }

  rows <- tibble(
    date     = as.character(date),
    section  = as.character(section_id),
    job      = names(result$assignments),
    name     = unname(result$assignments),
    cycle_id = vapply(names(result$assignments), function(j) {
      upd <- result$state_updates[[j]]
      if (is.null(upd)) 0L else as.integer(upd$cycle_id)
    }, integer(1))
  )

  append_log(rows)
}

clear_log <- function(delete_date = FALSE, section_id = NULL, date = Sys.Date()) {
  if (delete_date && inherits(date, "Date")) {
    date_str <- as.character(date)
    if (!is.null(section_id)) {
      jdb_exec("DELETE FROM job_log WHERE logged_date=? AND section=?",
               list(date_str, as.character(section_id)))
    } else {
      jdb_exec("DELETE FROM job_log WHERE logged_date=?", list(date_str))
    }
    return(TRUE)
  } else if (!delete_date) {
    jdb_exec("DELETE FROM job_log")
    return(TRUE)
  }
  return(FALSE)
}

append_admin_event <- function(section_id, date, event) {
  jdb_exec(
    "INSERT INTO job_log(logged_date, section, job, display_name, cycle_id)
     VALUES(?,?,?,?,?)",
    list(as.character(date), as.character(section_id),
         paste0("ADMIN__", event), "", 0L)
  )
}

deterministic_shuffle <- function(x, seed_string) {
  seed <- sum(utf8ToInt(seed_string)) %% .Machine$integer.max
  set.seed(seed)
  sample(x, length(x), replace = FALSE)
}

generate_summary_table <- function() {
  lg <- jdb_query(
    "SELECT logged_date, section, job, display_name FROM job_log
     WHERE job NOT LIKE 'ADMIN__%'"
  )
  roster_df <- jdb_query(
    "SELECT display_name, section FROM users WHERE COALESCE(is_admin,0)=0"
  ) %>% dplyr::rename(name = display_name)

  if (nrow(lg) == 0) {
    return(roster_df %>% dplyr::select(name, section))
  }

  lg <- lg %>%
    dplyr::rename(name = display_name) %>%
    dplyr::mutate(job = sub("\\s+[0-9]+$", "", job, ignore.case = TRUE))

  lg %>%
    dplyr::full_join(roster_df, by = c("name", "section")) %>%
    dplyr::group_by(name, section, job) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = job, values_from = count) %>%
    dplyr::arrange(name, section)
}

rebuild_state_from_log <- function(section_id, upto_date = Sys.Date()) {
  section_id <- trimws(as.character(section_id))
  upto_date  <- as.character(upto_date)

  roster <- read_roster(section_id)
  N      <- length(roster)
  if (N == 0) stop("Roster empty for section ", section_id)

  jobs_df <- jdb_query("SELECT job FROM job_state WHERE section=?",
                       list(section_id))
  jobs <- jobs_df$job

  lg <- jdb_query(
    "SELECT * FROM job_log WHERE section=? AND logged_date<=? ORDER BY created_at",
    list(section_id, upto_date)
  )

  last_reset_i <- suppressWarnings(
    max(which(lg$job == "ADMIN__RESET_BAG"), na.rm = TRUE)
  )
  lg_use <- if (is.finite(last_reset_i)) lg[(last_reset_i + 1):nrow(lg), , drop = FALSE] else lg

  for (j in jobs) {
    k            <- sum(lg_use$job == j)
    cycle_id     <- floor(k / N) + 1
    pos_in_cycle <- k %% N
    order        <- deterministic_shuffle(roster, paste(section_id, j, cycle_id, sep = "|"))
    bag          <- if (pos_in_cycle == 0) order else order[(pos_in_cycle + 1):N]
    write_state_job(section_id, j, cycle_id, bag)
  }

  TRUE
}

reset_bag <- function(section_id) {
  jdb_exec("UPDATE job_state SET bag_json='[]' WHERE section=?",
           list(as.character(section_id)))
}

# Import historical log from the Google Sheet into job_log.
# Uses the original `ts` column as `created_at` so re-running is idempotent
# (rows whose ts already exists in job_log are skipped).
import_log_from_sheets <- function() {
  if (!nzchar(SHEET_ID)) stop("CLASS_JOB_SHEET_ID not set")

  gs_log <- read_sheet(SHEET_ID, sheet = "log", col_types = "cccccc")
  # Expected columns: ts, date, section, job, name, cycle_id
  if (nrow(gs_log) == 0) return(0L)

  existing_ts <- jdb_query(
    "SELECT created_at FROM job_log WHERE created_at IS NOT NULL"
  )$created_at

  new_rows <- gs_log[!as.character(gs_log$ts) %in% existing_ts, ]
  if (nrow(new_rows) == 0) return(0L)

  n <- 0L
  for (i in seq_len(nrow(new_rows))) {
    cycle_val <- suppressWarnings(as.integer(new_rows$cycle_id[i]))
    if (is.na(cycle_val)) cycle_val <- 0L
    tryCatch({
      jdb_exec(
        "INSERT INTO job_log(logged_date, section, job, display_name, cycle_id, created_at)
         VALUES(?,?,?,?,?,?)",
        list(as.character(new_rows$date[i]),
             as.character(new_rows$section[i]),
             as.character(new_rows$job[i]),
             as.character(new_rows$name[i]),
             cycle_val,
             as.character(new_rows$ts[i]))
      )
      n <- n + 1L
    }, error = function(e) NULL)
  }
  n
}

# ----------------------------
# STUDENT POINTS HELPER
# ----------------------------
student_points_data <- function(display_name) {
  lg <- tryCatch(
    jdb_query(
      "SELECT logged_date, section, job FROM job_log
       WHERE display_name=? AND job NOT LIKE 'ADMIN__%'
       ORDER BY logged_date DESC",
      list(as.character(display_name))
    ),
    error = function(e) data.frame()
  )
  lg
}

student_points_ui <- function(display_name) {
  fluidPage(
    tags$head(tags$style(HTML(VASSAR_CSS))),
    titlePanel(paste0("My Class Jobs — ", display_name)),
    uiOutput("student_total"),
    fluidRow(
      column(5,
        h4("Summary by job type"),
        tableOutput("student_summary")
      ),
      column(7,
        h4("Recent entries (last 20)"),
        tableOutput("student_recent")
      )
    ),
    br(),
    actionButton("student_logout", "Log out", class = "btn-primary")
  )
}

# ----------------------------
# PASSWORD GATE
# ----------------------------
SHINY_PASSWORD <- Sys.getenv("SHINY_PASSWORD", "")

# ----------------------------
# UI
# ----------------------------
VASSAR_CSS <- "
  body { font-size: 16px; }
  .btn-primary       { background-color: #951829; border-color: #7a1221; }
  .btn-primary:hover { background-color: #7a1221; border-color: #5e0d19; }
  .btn-success       { background-color: #2d6a4f; border-color: #245c43; }
  .btn-success:hover { background-color: #245c43; border-color: #1b4d38; }
  .btn-danger        { background-color: #951829; border-color: #7a1221; }
  a { color: #951829; } a:hover { color: #7a1221; }
  .shiny-notification-panel { top:16px; bottom:auto; right:16px; min-width:340px; width:auto; }
  .shiny-notification { font-size:1.1rem; padding:1rem 1.25rem; border-radius:8px; border:none;
    box-shadow:0 4px 20px rgba(0,0,0,.25); color:#fff; margin-bottom:8px; }
  .shiny-notification-message { background:#2d6a4f; }
  .shiny-notification-warning { background:#92400e; }
  .shiny-notification-error   { background:#951829; }
  .shiny-notification-close   { color:rgba(255,255,255,.8); font-size:1.2rem; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(VASSAR_CSS))),
  tags$head(tags$style(HTML("
    .panel { background: #fff; border: 1px solid #ddd; border-radius: 12px; padding: 12px; margin-top: 10px; }
    .bigbtn button { font-size: 16px; padding: 10px; }
    .mono { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; white-space: pre-wrap; }
  "))),
  uiOutput("main_ui")
)

login_ui <- fluidPage(
  titlePanel("Class Jobs Console"),
  wellPanel(
    textInput("login_user", "Username (students only — leave blank for admin)"),
    passwordInput("login_pw", "Password"),
    actionButton("login_btn", "Sign in", class = "btn-primary"),
    tags$small(style = "color:#666;",
      "Students: enter your username and password. Instructors: leave username blank.")
  )
)

# Get initial section list (best-effort; updated in server after auth)
INITIAL_SECTIONS <- tryCatch(sections_from_db(), error = function(e) c("51", "52"))
if (length(INITIAL_SECTIONS) == 0) INITIAL_SECTIONS <- c("51", "52")

app_ui <- tagList(
  titlePanel("Class Jobs Console"),

  fluidRow(style = "display:flex; flex-wrap:wrap; gap:12px;",
    div(style = "min-width:180px; flex:1;",
        selectInput("section", "Section", choices = INITIAL_SECTIONS)
    ),
    div(style = "min-width:220px; flex:1;",
        dateInput("date", "Class date", value = Sys.Date())
    )
  ),

  tabsetPanel(
    tabPanel("Class jobs",
      fluidRow(
        column(4,
          div(class = "panel",
            fluidRow(style = "display: flex; flex-wrap: nowrap;",
              column(6,
                checkboxGroupInput("jobs_selected", "Select job(s)",
                                   choices = DRAW_JOBS, selected = DRAW_JOBS)
              ),
              column(6,
                div(
                  actionButton("draw",    "Draw jobs"),
                  actionButton("commit",  "Commit jobs"),
                  actionButton("refresh", "Refresh")
                )
              )
            )
          )
        ),
        column(8,
          div(class = "panel",
            fluidRow(
              column(6,
                h4("Draft (not yet committed)"),
                tableOutput("assignTable")
              ),
              column(6,
                uiOutput("redrawUI")
              )
            )
          )
        )
      )
    ),
    tabPanel("Cold Call",
      fluidRow(
        column(12,
          div(class = "panel",
            h4("Cold call (one at a time)"),
            fluidRow(
              column(6,
                checkboxInput("exclude_jobs_today", "Exclude today's job assignees",
                              value = TRUE)
              ),
              column(6,
                actionButton("draw_cold",   "Draw cold call"),
                actionButton("commit_cold", "Commit cold call"),
                div(style = "font-size: 22px; font-weight: 600; padding: 8px 0;",
                    textOutput("coldName"))
              )
            )
          )
        )
      )
    ),
    tabPanel("Manual Entry",
      fluidRow(
        column(12,
          div(class = "panel",
            h4("Manual log entry"),
            fluidRow(
              column(6,
                dateInput("man_date",    "Date (override)",    value = Sys.Date()),
                selectInput("man_section", "Section (override)", choices = INITIAL_SECTIONS)
              ),
              column(6,
                selectInput("man_job", "Type",
                            choices = c(DEFAULT_JOBS, "other")),
                uiOutput("man_name_ui")
              )
            ),
            actionButton("man_append", "Append to log")
          )
        )
      )
    ),
    tabPanel("Absentees",
      br(),
      h4("Absent students:"),
      p("Select absent students for the current section and date. They will be excluded from all job draws today."),
      uiOutput("absent_picker_ui")
    ),
    tabPanel("Student Preview",
      br(),
      uiOutput("impersonate_banner"),
      uiOutput("student_total"),
      fluidRow(
        column(5,
          h4("Summary by job type"),
          tableOutput("student_summary")
        ),
        column(7,
          h4("Recent entries (last 20)"),
          tableOutput("student_recent")
        )
      )
    ),
    tabPanel("Admin",
      fluidRow(
        column(8,
          div(class = "panel",
            h4("Admin"),
            actionButton("admin_clear_log",      "CLEAR log tab (danger)"),
            checkboxInput("admin_confirm",        "I understand this deletes data", value = FALSE),
            checkboxInput("admin_delete_date",    "Delete date's data",             value = FALSE),
            dateInput("admin_delete_date_input",  "Date to delete", value = Sys.Date()),
            actionButton("admin_reset_bag",       "RESET bag (danger)"),
            actionButton("admin_rebuild_state",   "REBUILD state from log (danger)"),
            actionButton("admin_generate_summary","GENERATE summary table"),
            actionButton("admin_import_log",      "Import log from Google Sheet"),
            hr(),
            wellPanel(
              h4("Preview student view"),
              p("Select any student to see their job history in the Student Preview tab."),
              fluidRow(
                column(6, uiOutput("impersonate_select_ui")),
                column(3, br(), actionButton("impersonate_start", "Preview",      class = "btn-warning")),
                column(3, br(), actionButton("impersonate_stop",  "Clear preview", class = "btn-secondary"))
              )
            ),
            hr(),
            wellPanel(
              h4("Manage jobs"),
              fluidRow(
                column(6,
                  textInput("new_job_name", "New job name", value = ""),
                  uiOutput("new_job_section_ui"),
                  actionButton("add_job_btn", "Add job", class = "btn-success")
                ),
                column(6,
                  h5("Remove an existing job"),
                  uiOutput("remove_job_ui")
                )
              )
            )
          )
        ),
        column(4,
          div(class = "panel",
            numericInput("max_commits", "Max commits before exclusion",
                         value = 10, min = 1, step = 1),
            hr(),
            uiOutput("exclusion_info")
          )
        )
      )
    ),
    tabPanel("Notes",
      br(),
      h4("Notes"),
      div(class = "mono", "
        - Draw creates a draft assignment.
        - Redraw changes one job (absent student is never logged).
        - Commit writes to the DB and removes those names from the bag.
        - Use the 'Absentees' tab to mark absent students each day.
        - Materials summary jobs share a rotation: a student who has done
          any materials summary is excluded from all materials summary
          draws until every student has caught up.
        - Students exceeding the 'Max commits' threshold are excluded
          from all draws (jobs and cold calls) until others catch up.
        - Both exclusions fall back gracefully if they would empty the pool.
        - 'GENERATE summary table' writes to the Google Sheet 'summary' tab.
        - State and log are stored in the shared SQLite DB (finalqdata.sqlite).
      ")
    )
  )
)

# ----------------------------
# SERVER
# ----------------------------
server <- function(input, output, session) {

  # --- Auth state ---
  authed           <- reactiveVal(!nzchar(SHINY_PASSWORD))  # TRUE = admin
  student_user     <- reactiveVal(NULL)   # display_name of logged-in student
  impersonate_user <- reactiveVal(NULL)   # display_name admin is previewing

  # Unified: who's the "active student" for the student view outputs
  view_student <- reactive({
    impersonate_user() %||% student_user()
  })

  output$main_ui <- renderUI({
    if (authed()) {
      app_ui
    } else if (!is.null(student_user())) {
      student_points_ui(student_user())
    } else {
      login_ui
    }
  })

  observeEvent(input$login_btn, {
    uname <- trimws(input$login_user %||% "")
    pw    <- input$login_pw %||% ""

    if (!nzchar(uname)) {
      # Admin path: single shared password
      if (nzchar(SHINY_PASSWORD) && identical(pw, SHINY_PASSWORD)) {
        authed(TRUE)
      } else if (!nzchar(SHINY_PASSWORD)) {
        authed(TRUE)
      } else {
        showNotification("Incorrect password.", type = "error")
      }
    } else {
      # DB path: check pw_hash in users table (works for both students and admins)
      row <- tryCatch(
        jdb_query(
          "SELECT display_name, pw_hash, COALESCE(is_admin,0) AS is_admin FROM users WHERE user_id=?",
          list(uname)
        ),
        error = function(e) data.frame()
      )
      if (nrow(row) == 0) {
        showNotification("Username not found.", type = "error")
      } else {
        ph <- row$pw_hash[1] %||% ""
        if (!nzchar(ph)) {
          showNotification("No password set for this account. Contact your instructor.", type = "error")
        } else if (bcrypt::checkpw(pw, ph)) {
          if (as.integer(row$is_admin[1]) == 1L) {
            authed(TRUE)   # admin via DB → full app
          } else {
            student_user(row$display_name[1])
          }
        } else {
          showNotification("Incorrect password.", type = "error")
        }
      }
    }
  })

  # --- Student view outputs & logout ---
  output$student_total <- renderUI({
    vs <- view_student(); req(!is.null(vs))
    n <- nrow(student_points_data(vs))
    p(style = "color:#666;", paste0("Total job entries: ", n))
  })

  output$student_summary <- renderTable({
    vs <- view_student(); req(!is.null(vs))
    lg <- student_points_data(vs)
    if (nrow(lg) == 0) return(NULL)
    lg$job_type <- sub("\\s+[0-9]+$", "", lg$job, ignore.case = TRUE)
    dplyr::count(lg, Job = job_type, name = "Times done") |>
      dplyr::arrange(dplyr::desc(`Times done`)) |>
      as.data.frame()
  }, striped = TRUE, hover = TRUE)

  output$student_recent <- renderTable({
    vs <- view_student(); req(!is.null(vs))
    lg <- student_points_data(vs)
    if (nrow(lg) == 0) return(NULL)
    dplyr::select(lg, Date = logged_date, Section = section, Job = job) |>
      head(20) |> as.data.frame()
  }, striped = TRUE, hover = TRUE)

  observeEvent(input$student_logout, {
    student_user(NULL)
    impersonate_user(NULL)
  })

  # --- Reactive values ---
  rv <- reactiveValues(
    draft        = NULL,
    draft_res    = NULL,
    cycle_id     = NA_integer_,
    bag_n        = NA_integer_,
    roster_n     = NA_integer_,
    cold_current = "",
    jobs_committed = FALSE,
    exclusions   = NULL,
    bag_summary  = ""
  )

  rv_absent <- reactiveVal(character(0))

  # --- Impersonation ---
  output$impersonate_select_ui <- renderUI({
    req(authed())
    students <- tryCatch(
      jdb_query("SELECT display_name FROM users WHERE COALESCE(is_admin,0)=0 ORDER BY display_name"),
      error = function(e) data.frame(display_name = character(0))
    )
    selectInput("impersonate_select", label = NULL,
                choices = c("— select student —" = "", students$display_name))
  })

  observeEvent(input$impersonate_start, {
    req(authed())
    nm <- input$impersonate_select %||% ""
    if (!nzchar(nm)) { showNotification("Select a student first.", type = "warning"); return() }
    impersonate_user(nm)
    showNotification(sprintf("Previewing: %s. Switch to Student Preview tab.", nm), type = "warning")
  })

  observeEvent(input$impersonate_stop, {
    req(authed())
    impersonate_user(NULL)
    showNotification("Preview cleared.", type = "message")
  })

  output$impersonate_banner <- renderUI({
    nm <- impersonate_user()
    if (is.null(nm)) {
      p(style = "color:#888;", "No student selected. Use the Admin tab to pick one.")
    } else {
      div(style = "background:#92400e; color:#fff; padding:8px 16px; border-radius:6px; margin-bottom:12px;",
          strong(sprintf("Previewing: %s", nm)))
    }
  })

  # --- On auth: refresh section pickers and seed default jobs ---
  observeEvent(authed(), {
    if (!authed()) return()
    secs <- tryCatch(sections_from_db(), error = function(e) INITIAL_SECTIONS)
    if (length(secs) == 0) secs <- INITIAL_SECTIONS

    updateSelectInput(session, "section",     choices = secs)
    updateSelectInput(session, "man_section", choices = secs)

    sec <- secs[1]
    jobs_df <- tryCatch(
      jdb_query("SELECT job FROM job_state WHERE section=? ORDER BY job",
                list(as.character(sec))),
      error = function(e) data.frame(job = DEFAULT_JOBS)
    )
    if (nrow(jobs_df) > 0) {
      draw_choices <- jobs_df$job[!tolower(jobs_df$job) %in% tolower(SPECIAL_JOBS)]
      updateCheckboxGroupInput(session, "jobs_selected",
                               choices = draw_choices, selected = draw_choices)
      updateSelectInput(session, "man_job", choices = jobs_df$job)
    }

    init_job_defaults(secs, DEFAULT_JOBS)
  }, ignoreInit = FALSE, once = TRUE)

  # Update job choices when section changes
  observeEvent(input$section, {
    req(authed())
    jobs_df <- tryCatch(
      jdb_query("SELECT job FROM job_state WHERE section=? ORDER BY job",
                list(as.character(input$section))),
      error = function(e) data.frame(job = DEFAULT_JOBS)
    )
    if (nrow(jobs_df) > 0) {
      draw_choices <- jobs_df$job[!tolower(jobs_df$job) %in% tolower(SPECIAL_JOBS)]
      updateCheckboxGroupInput(session, "jobs_selected",
                               choices = draw_choices, selected = draw_choices)
      updateSelectInput(session, "man_job", choices = jobs_df$job)
    }
  }, ignoreInit = TRUE)

  # --- Absent picker ---
  output$absent_picker_ui <- renderUI({
    req(authed(), input$section)
    roster <- read_roster(input$section)
    selectizeInput("absent_names", label = NULL, choices = roster,
                   selected = rv_absent(), multiple = TRUE,
                   options = list(placeholder = "Pick absentees…"))
  })

  observeEvent(list(input$section, input$date), {
    req(authed())
    rv_absent(character(0))
  })

  observeEvent(input$absent_names, {
    rv_absent(input$absent_names)
  })

  # --- Status update ---
  update_status <- function() {
    req(authed(), input$section)
    roster <- read_roster(input$section)
    if (length(rv_absent()) > 0) roster <- setdiff(roster, rv_absent())
    rv$roster_n <- length(roster)

    st_sec <- jdb_query("SELECT job, bag_json FROM job_state WHERE section=?",
                        list(as.character(input$section)))
    rv$bag_summary <- paste0(
      st_sec$job, ": ",
      sapply(st_sec$bag_json, function(x) length(parse_bag(x))),
      collapse = "\n"
    )
  }

  # --- Exclusion info panel ---
  output$exclusion_info <- renderUI({
    req(authed(), input$section)
    roster <- read_roster(input$section)
    max_c  <- if (!is.null(input$max_commits)) input$max_commits else Inf
    excl   <- compute_exclusions(input$section, roster, max_commits = max_c)
    rv$exclusions <- excl

    msgs <- character(0)
    if (length(excl$matsumm) > 0)
      msgs <- c(msgs, paste0("Mat. summ. excluded (", length(excl$matsumm), "): ",
                             paste(excl$matsumm, collapse = ", ")))
    if (length(excl$overcap) > 0)
      msgs <- c(msgs, paste0("Over ", max_c, " commits (", length(excl$overcap), "): ",
                             paste(excl$overcap, collapse = ", ")))
    if (length(msgs) == 0) return(NULL)

    tags$div(style = "font-size: 11px; color: #888; margin-top: 6px;",
             HTML(paste(msgs, collapse = "<br/>")))
  })

  observeEvent(TRUE, { update_status() }, once = TRUE)

  observeEvent(input$refresh, {
    update_status()
    rv$draft        <- NULL
    rv$draft_res    <- NULL
    rv_absent(character(0))
    rv$jobs_committed <- FALSE
  })

  observeEvent(list(input$section, input$date), {
    req(authed())
    rv$draft          <- NULL
    rv$draft_res      <- NULL
    rv$jobs_committed <- FALSE
    rv$cold_current   <- ""
    rv_absent(character(0))
  }, ignoreInit = TRUE)

  # --- Draw jobs ---
  observeEvent(input$draw, {
    jobs     <- input$jobs_selected
    absentees <- rv_absent()

    if (is.null(jobs) || length(jobs) == 0) {
      showNotification("Select at least one job.", type = "warning")
      return()
    }

    roster <- read_roster(input$section)
    max_c  <- if (!is.null(input$max_commits)) input$max_commits else Inf
    exclusions  <- compute_exclusions(input$section, roster, max_commits = max_c)
    rv$exclusions <- exclusions

    res      <- draw_jobs_day(input$section, jobs, absentees = absentees,
                              exclusions = exclusions)
    rv$draft_res  <- res
    rv$draft      <- res$assignments
    rv$jobs_committed <- FALSE
  })

  # --- Draw cold call ---
  observeEvent(input$draw_cold, {
    roster <- read_roster(input$section)
    if (length(rv_absent()) > 0) roster <- setdiff(roster, rv_absent())

    pool <- roster
    if (isTRUE(input$exclude_jobs_today) && !is.null(rv$draft))
      pool <- setdiff(roster, unname(rv$draft))

    if (!is.null(rv$exclusions)) {
      filtered <- setdiff(pool, rv$exclusions$overcap)
      if (length(filtered) > 0) pool <- filtered
    }

    if (length(pool) == 0) {
      showNotification("Cold-call pool is empty.", type = "error")
      rv$cold_current <- ""
      return()
    }

    rv$cold_current <- sample(pool, 1)
  })

  # --- Commit jobs ---
  observeEvent(input$commit, {
    req(rv$draft_res)

    if (rv$jobs_committed) {
      showNotification("Jobs already committed for today.", type = "warning")
      return()
    }

    commit_jobs_day(
      section_id = input$section,
      date       = input$date,
      result     = rv$draft_res
    )

    rv$draft          <- NULL
    rv$draft_res      <- NULL
    rv$jobs_committed <- TRUE
    update_status()
  })

  # --- Commit cold call ---
  observeEvent(input$commit_cold, {
    if (rv$cold_current == "") {
      showNotification("No cold call to commit.", type = "warning")
      return()
    }

    st       <- read_state(input$section, "cold call")
    cycle_id <- if (nrow(st) == 0) 0L else as.integer(st$cycle_id[1])

    rows <- tibble(
      date     = as.character(input$date),
      section  = as.character(input$section),
      job      = "cold call",
      name     = rv$cold_current,
      cycle_id = cycle_id
    )

    append_log(rows)
    rv$cold_current <- ""
    showNotification("Cold call committed.", type = "message")
  })

  # --- Manual entry ---
  output$man_name_ui <- renderUI({
    roster <- read_roster(input$man_section)
    selectizeInput("man_name", "Name", choices = roster, multiple = FALSE,
                   options = list(placeholder = "Type a name…"))
  })

  observeEvent(input$man_append, {
    nm <- input$man_name
    if (is.null(nm) || nm == "") {
      showNotification("Pick a name.", type = "warning")
      return()
    }

    st       <- read_state(input$man_section, input$man_job)
    cycle_id <- if (nrow(st) == 0) 0L else as.integer(st$cycle_id[1])

    rows <- tibble(
      date     = as.character(input$man_date),
      section  = as.character(input$man_section),
      job      = input$man_job,
      name     = nm,
      cycle_id = cycle_id
    )

    append_log(rows)
    showNotification("Appended.", type = "message")
  })

  # --- Outputs ---
  output$assignTable <- renderTable({
    if (is.null(rv$draft)) return(data.frame())
    tibble(job = names(rv$draft), name = unname(rv$draft))
  }, striped = TRUE, bordered = TRUE)

  output$coldName <- renderText({
    if (rv$cold_current == "") return("(no cold call drawn)")
    rv$cold_current
  })

  output$redrawUI <- renderUI({
    if (is.null(rv$draft)) return(NULL)
    jobs_df <- tryCatch(
      jdb_query("SELECT job FROM job_state WHERE section=? ORDER BY job",
                list(as.character(input$section))),
      error = function(e) data.frame(job = DRAW_JOBS)
    )
    draw_choices <- jobs_df$job[!tolower(jobs_df$job) %in% tolower(SPECIAL_JOBS)]
    tagList(
      h4("Redraw (if absent)"),
      fluidRow(
        selectInput("jobToRedraw", "Job to redraw", choices = draw_choices),
        actionButton("redraw", "Redraw selected job")
      )
    )
  })

  observeEvent(input$redraw, {
    req(rv$draft_res)

    job_to_redraw <- input$jobToRedraw
    absentees     <- rv_absent()

    info <- ensure_bag_job(input$section, job_to_redraw, absentees = absentees)
    bag  <- info$bag

    taken_today <- unname(rv$draft_res$assignments)
    taken_today <- taken_today[!is.na(taken_today)]
    available   <- setdiff(bag, taken_today)

    if (!is.null(rv$exclusions)) {
      available <- setdiff(available, rv$exclusions$overcap)
      if (grepl("materials summary", job_to_redraw, ignore.case = TRUE))
        available <- setdiff(available, rv$exclusions$matsumm)
      if (length(available) == 0) available <- setdiff(bag, taken_today)
    }

    if (length(available) == 0) {
      showNotification("No one left to redraw from (bag exhausted).", type = "error")
      return()
    }

    new_name <- sample(available, 1)

    rv$draft_res$assignments[[job_to_redraw]] <- new_name
    rv$draft_res$state_updates[[job_to_redraw]] <- list(
      cycle_id = info$cycle_id,
      new_bag  = setdiff(bag, new_name)
    )

    rv$draft <- rv$draft_res$assignments
    update_status()
  })

  # --- Admin: clear log ---
  observeEvent(input$admin_clear_log, {
    if (!isTRUE(input$admin_confirm)) {
      showNotification("Check the confirmation box first.", type = "warning")
      return()
    }
    log_clear <- clear_log(
      delete_date = isTRUE(input$admin_delete_date),
      section_id  = input$section,
      date        = input$admin_delete_date_input
    )
    rv$jobs_committed <- FALSE
    if (!log_clear) showNotification("Log not cleared, check date.", type = "message")
    else            showNotification("Log cleared.", type = "warning")
  })

  # --- Admin: reset bag ---
  observeEvent(input$admin_reset_bag, {
    if (!isTRUE(input$admin_confirm)) {
      showNotification("Check the confirmation box first.", type = "warning")
      return()
    }
    reset_bag(input$section)
    append_admin_event(input$section, input$date, "RESET_BAG")
    rv$jobs_committed <- FALSE
    rv$draft <- NULL
    showNotification("Bags reset and event logged.", type = "message")
  })

  # --- Admin: rebuild state from log ---
  observeEvent(input$admin_rebuild_state, {
    if (!isTRUE(input$admin_confirm)) {
      showNotification("Check the confirmation box first.", type = "warning")
      return()
    }
    ok <- rebuild_state_from_log(input$section, upto_date = input$date)
    if (ok) {
      rv$jobs_committed <- FALSE
      rv$draft <- NULL
      showNotification("State rebuilt from log.", type = "message")
    }
  })

  # --- Admin: generate summary ---
  observeEvent(input$admin_generate_summary, {
    summary_table <- generate_summary_table()
    if (nzchar(SHEET_ID)) {
      tryCatch(
        write_sheet(summary_table, ss = SHEET_ID, sheet = "summary"),
        error = function(e)
          showNotification(paste("Sheet write failed:", conditionMessage(e)), type = "error")
      )
      showNotification("Summary written to Google Sheet.", type = "message")
    } else {
      showNotification("CLASS_JOB_SHEET_ID not set; cannot write summary.", type = "warning")
    }
  })

  # --- Admin: import log from Google Sheet ---
  observeEvent(input$admin_import_log, {
    n <- tryCatch(
      import_log_from_sheets(),
      error = function(e) {
        showNotification(paste("Import failed:", conditionMessage(e)), type = "error")
        NULL
      }
    )
    if (!is.null(n))
      showNotification(paste0("Imported ", n, " new log entries from Google Sheet."),
                       type = "message")
  })

  # --- Admin: manage jobs ---
  output$new_job_section_ui <- renderUI({
    req(authed())
    secs <- tryCatch(sections_from_db(), error = function(e) INITIAL_SECTIONS)
    selectInput("new_job_section", "For section", choices = secs)
  })

  output$remove_job_ui <- renderUI({
    req(authed(), input$new_job_section)
    jobs_df <- tryCatch(
      jdb_query("SELECT job FROM job_state WHERE section=? ORDER BY job",
                list(as.character(input$new_job_section))),
      error = function(e) data.frame(job = character(0))
    )
    if (nrow(jobs_df) == 0)
      return(p("No jobs for this section."))
    tagList(
      selectInput("remove_job_name", "Job to remove", choices = jobs_df$job),
      actionButton("remove_job_btn", "Remove job", class = "btn-danger")
    )
  })

  observeEvent(input$add_job_btn, {
    nm  <- trimws(input$new_job_name)
    sec <- input$new_job_section
    if (!nzchar(nm)) {
      showNotification("Enter a job name.", type = "warning")
      return()
    }
    jdb_exec(
      "INSERT OR IGNORE INTO job_state(section, job, cycle_id, bag_json)
       VALUES(?,?,1,'[]')",
      list(as.character(sec), nm)
    )
    showNotification(paste0("Added job '", nm, "' for section ", sec, "."),
                     type = "message")
  })

  observeEvent(input$remove_job_btn, {
    req(input$remove_job_name, input$new_job_section)
    jdb_exec(
      "DELETE FROM job_state WHERE section=? AND job=?",
      list(as.character(input$new_job_section), as.character(input$remove_job_name))
    )
    showNotification(paste0("Removed job '", input$remove_job_name, "'."),
                     type = "warning")
  })

  # --- Last committed (available for future UI use) ---
  output$lastCommitted <- renderTable({
    req(authed(), input$section)
    lg <- jdb_query(
      "SELECT * FROM job_log WHERE section=? ORDER BY created_at DESC LIMIT 10",
      list(as.character(input$section))
    )
    if (nrow(lg) == 0) return(data.frame())
    lg
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
