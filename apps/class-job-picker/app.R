library(shiny)
library(googlesheets4)
library(dplyr)
library(jsonlite)
library(lubridate)

# shiny::runApp(appDir = "C:/Users/kgcsp/OneDrive/Documents/Education/Teaching/shiny-apps/class-job-picker", port = 3838, host = "127.0.0.1")

# ----------------------------
# CONFIG
# ----------------------------
# Put your Google Sheet ID here (the long string in the URL)
SHEET_ID <- Sys.getenv("CLASS_JOB_SHEET_ID")
## Testing
# SHEET_ID <- "1-_fWuwLC8hxHzrE4pimsDN6u75MWQeUgs9Zk_p093D0"
# Path to service account JSON (recommended)
SERVICE_JSON <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
# Credentials
CREDENTIALS <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")


# Default job list (edit as you like)
DEFAULT_JOBS <- read_sheet(SHEET_ID, sheet = "state", col_types = "c") %>% pull(job) %>% unique() %>% sort()

# ----------------------------
# AUTH
# ----------------------------

get_gs_cred_path <- function(CREDENTIALS, SERVICE_JSON) {
  # Prefer a file path; fall back to inline JSON.
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

gs4_deauth() # important: we are NOT using interactive user auth
gs4_auth(path = get_gs_cred_path(CREDENTIALS, SERVICE_JSON))

# ----------------------------
# SHEET HELPERS
# ----------------------------
read_roster <- function(section_id) {
  df <- read_sheet(SHEET_ID, sheet = "roster", col_types = "cc")
  df %>%
    filter(section == as.character(section_id)) %>%
    pull(name) %>%
    unique() %>%
    sort()
}

read_state <- function(section_id,job_id=NULL) {
  if (is.null(job_id)) {
    return(read_sheet(SHEET_ID, sheet = "state", col_types = "ccccc") %>%
    mutate(
      section = trimws(as.character(.data$section)),
      job     = trimws(as.character(.data$job))
    ) %>%
    filter(.data$section == trimws(as.character(section_id))))
  }
  else {
  read_sheet(SHEET_ID, sheet = "state", col_types = "ccccc") %>%
    mutate(
      section = trimws(as.character(.data$section)),
      job     = trimws(as.character(.data$job))
    ) %>%
    filter(.data$section == trimws(as.character(section_id)) & .data$job == trimws(as.character(job_id)))
  }
}

write_state_job <- function(section_id, job_id, cycle_id_new, bag, last_updated = Sys.time()) {
  st <- read_sheet(SHEET_ID, sheet = "state", col_types = "ccccc") %>%
    mutate(
      section = trimws(as.character(section)),
      job = trimws(as.character(job))
    )

  section_id <- trimws(as.character(section_id))
  job_id <- trimws(as.character(job_id))
  cycle_id_new <- as.integer(cycle_id_new)
  bag_json_new <- toJSON(bag, auto_unbox = TRUE)

  st2 <- st %>%
    mutate(
      cycle_id = ifelse(.data$section == section_id & .data$job == job_id, as.character(cycle_id_new), .data$cycle_id),
      bag_json = ifelse(.data$section == section_id & .data$job == job_id, bag_json_new, .data$bag_json),
      last_updated = ifelse(.data$section == section_id & .data$job == job_id, as.character(last_updated), .data$last_updated)
    )

  sheet_write(st2, ss = SHEET_ID, sheet = "state")
}

append_log <- function(rows_df) {
  # rows_df must have columns: ts,date,section,job,name,cycle_id
  # We append by reading and writing; for small classes this is fine.
  # (If this gets big, we can switch to a Sheets append API or use a separate database.)
  log_df <- read_sheet(SHEET_ID, sheet = "log", col_types = "cccccc")
  out <- bind_rows(log_df, rows_df)
  sheet_write(out, ss = SHEET_ID, sheet = "log")
}

parse_bag <- function(bag_json) {
  if (is.na(bag_json) || bag_json == "" || bag_json == "[]") return(character(0))
  fromJSON(bag_json)
}

# ----------------------------
# CORE LOGIC
# ----------------------------
# MODIFIED: Add argument to filter students out if absent
ensure_bag_job <- function(section_id, job_id, absentees = character(0)) {

  job_id <- trimws(as.character(job_id))
  if (is.na(job_id) || job_id == "") stop("ensure_bag_job called with empty job. section_id=", section_id, " job=", job_id)

  roster <- read_roster(section_id)
  if (length(absentees) > 0) {
    roster <- setdiff(roster, absentees)
  }
  st <- read_state(section_id, job_id)

  cycle_id <- suppressWarnings(as.integer(st$cycle_id[1]))
  if (is.na(cycle_id)) cycle_id <- 1

  bag <- parse_bag(st$bag_json[[1]])

  # Keep bag aligned to roster (handles roster edits)
  bag <- bag[bag %in% roster]

  if (length(bag) == 0) {
    cycle_id <- cycle_id + 1
    bag <- sample(roster, size = length(roster), replace = FALSE)
    write_state_job(section_id, job_id, cycle_id, bag)
  }

  list(roster = roster, cycle_id = cycle_id, bag = bag)
}

# MODIFIED: Add argument to filter students out if absent
draw_jobs_day <- function(section_id, jobs, absentees = character(0)) {

  roster <- read_roster(section_id)
  if (length(absentees) > 0) {
    roster <- setdiff(roster, absentees)
  }
  state  <- read_state(section_id)

  picked_today <- character(0)
  assignments  <- setNames(character(length(jobs)), jobs)
  state_updates <- list()

  for (j in jobs) {
    row <- state[state$job == j, ]

    bag <- parse_bag(row$bag_json)
    bag <- bag[bag %in% roster]

    if (length(bag) == 0) {
      bag <- sample(roster, length(roster))
      cycle_id <- as.integer(row$cycle_id) + 1
    } else {
      cycle_id <- as.integer(row$cycle_id)
    }

    avail <- setdiff(bag, picked_today)
    if (length(avail) == 0) stop("No eligible student for job: ", j, " in section: ", section_id)

    nm <- sample(avail, 1)
    assignments[j] <- nm
    picked_today <- c(picked_today, nm)

    state_updates[[j]] <- list(
      cycle_id = cycle_id,
      new_bag  = setdiff(bag, nm)
    )
  }

  list(assignments = assignments, state_updates = state_updates)
}

# NOTE: inconsistent naming 'ensure_bag' vs 'ensure_bag_job' below:
# MODIFIED: Add argument to filter students out if absent
redraw_one <- function(section_id, current_assignments, job_to_redraw, absentees = character(0)) {
  info <- ensure_bag_job(section_id, job_to_redraw, absentees = absentees)
  roster <- read_roster(section_id)
  if (length(absentees) > 0) {
    roster <- setdiff(roster, absentees)
  }
  bag <- info$bag

  taken_today <- unname(current_assignments)
  taken_today <- taken_today[!is.na(taken_today)]

  # Available = bag excluding those already assigned today
  available <- setdiff(bag, taken_today)

  if (length(available) == 0) {
    # try refilling cycle (without committing)
    info2 <- ensure_bag_job(section_id, job_to_redraw, absentees = absentees)
    bag <- info2$bag
    available <- setdiff(bag, taken_today)
  }

  if (length(available) == 0) stop("No one left to redraw from (bag exhausted).")

  new_name <- sample(available, 1)
  current_assignments[[job_to_redraw]] <- new_name

  list(assignments = current_assignments, roster = roster, cycle_id = info$cycle_id, bag = bag)
}

commit_jobs_day <- function(section_id, date, result) {

  # Read FULL state (all sections) so we don't wipe other sections
  st_all <- read_sheet(SHEET_ID, sheet = "state", col_types = "ccccc") %>%
    mutate(
      section = trimws(as.character(section)),
      job     = trimws(as.character(job))
    )

  section_id <- trimws(as.character(section_id))

  # Apply updates only to matching (section, job)
  for (j in names(result$state_updates)) {
    upd <- result$state_updates[[j]]
    j   <- trimws(as.character(j))

    idx <- st_all$section == section_id & st_all$job == j
    if (!any(idx)) next

    st_all$cycle_id[idx]      <- as.character(upd$cycle_id)
    st_all$bag_json[idx]      <- jsonlite::toJSON(upd$new_bag, auto_unbox = TRUE)
    st_all$last_updated[idx]  <- as.character(Sys.time())
  }

  # Write FULL state back
  sheet_write(st_all, ss = SHEET_ID, sheet = "state")

  # Log with cycle_id per job (important for rebuilds)
  rows <- tibble(
    ts      = as.character(Sys.time()),
    date    = as.character(date),
    section = as.character(section_id),
    job     = names(result$assignments),
    name    = unname(result$assignments),
    cycle_id = vapply(names(result$assignments), function(j) {
      idx <- st_all$section == section_id & st_all$job == trimws(j)
      if (!any(idx)) NA_character_ else as.character(st_all$cycle_id[idx][1])
    }, FUN.VALUE = character(1))
  )

  append_log(rows)
}

clear_log <- function(delete_date = FALSE, section_id = NULL, date = Sys.Date()) {
  if (delete_date && is.Date(date)) {
    lg <- read_sheet(SHEET_ID, sheet = "log", col_types = "cccccc")
    date <- as.character(date)
    if (!is.null(section_id)) section_id <- as.character(section_id)

    lg2 <- lg %>%
      filter(!(date == !!date & (is.null(section_id) | section == !!section_id)))

    sheet_write(lg2, ss = SHEET_ID, sheet = "log")
    return(TRUE)
  }
  else if (!delete_date) {
    empty <- tibble(
      ts = character(),
      date = character(),
      section = character(),
      job = character(),
      name = character(),
      cycle_id = character()
    )
    sheet_write(empty, ss = SHEET_ID, sheet = "log")
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

append_admin_event <- function(section_id, date, event) {
  rows <- tibble(
    ts = as.character(Sys.time()),
    date = as.character(date),
    section = as.character(section_id),
    job = paste0("ADMIN__", event),
    name = "",
    cycle_id = ""
  )
  append_log(rows)
}

deterministic_shuffle <- function(x, seed_string) {
  # Simple deterministic seed from string
  seed <- sum(utf8ToInt(seed_string)) %% .Machine$integer.max
  set.seed(seed)
  sample(x, length(x), replace = FALSE)
}

rebuild_state_from_log <- function(section_id, upto_date = Sys.Date()) {

  section_id <- trimws(as.character(section_id))
  upto_date  <- as.character(upto_date)

  roster <- read_roster(section_id)
  N <- length(roster)
  if (N == 0) stop("Roster empty for section ", section_id)

  st_all <- read_sheet(SHEET_ID, sheet="state", col_types="ccccc") %>%
    mutate(section = trimws(as.character(section)),
           job     = trimws(as.character(job)))

  lg <- read_sheet(SHEET_ID, sheet="log", col_types="cccccc") %>%
    mutate(section = trimws(as.character(section)),
           job     = trimws(as.character(job))) %>%
    filter(section == section_id, date <= upto_date) %>%
    arrange(ts)

  # Find last reset marker per section (optional but recommended)
  last_reset_i <- max(which(lg$job == "ADMIN__RESET_BAG"), na.rm = TRUE)
  if (is.finite(last_reset_i)) {
    lg_use <- lg[(last_reset_i+1):nrow(lg), , drop=FALSE]
  } else {
    lg_use <- lg
  }

  # Only jobs that exist in state for this section
  st_sec_idx <- st_all$section == section_id
  jobs <- st_all$job[st_sec_idx]

  for (j in jobs) {
    # Count how many times job assigned since last reset
    k <- sum(lg_use$job == j)

    cycle_id <- floor(k / N) + 1
    pos_in_cycle <- k %% N

    # Deterministic order for this job/cycle
    order <- deterministic_shuffle(roster, paste(section_id, j, cycle_id, sep="|"))

    # Remove already-taken in this cycle (we don't know exact order of real past draws,
    # but this gives a stable, consistent reconstruction)
    bag <- if (pos_in_cycle == 0) order else order[(pos_in_cycle+1):N]

    idx <- st_all$section == section_id & st_all$job == j
    st_all$cycle_id[idx]     <- as.character(cycle_id)
    st_all$bag_json[idx]     <- jsonlite::toJSON(bag, auto_unbox = TRUE)
    st_all$last_updated[idx] <- as.character(Sys.time())
  }

  sheet_write(st_all, ss=SHEET_ID, sheet="state")
  TRUE
}

reset_bag <- function(section_id) {
  st <- read_sheet(SHEET_ID, sheet="state", col_types="cccc") %>%
    mutate(section = trimws(as.character(section)))
  section_id <- trimws(as.character(section_id))

  st2 <- st %>%
    mutate(
      bag_json = ifelse(.data$section == section_id, "[]", .data$bag_json),
      last_updated = ifelse(.data$section == section_id, as.character(Sys.time()), .data$last_updated)
    )
  sheet_write(st2, ss=SHEET_ID, sheet="state")
}

# ----------------------------
# UI
# ----------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .panel { background: #fff; border: 1px solid #ddd; border-radius: 12px; padding: 12px; margin-top: 10px; }
    .bigbtn button { font-size: 16px; padding: 10px; }
    .mono { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; white-space: pre-wrap; }
  "))),
  titlePanel("Class Jobs Console"),

  # Make section and date global controls
  fluidRow(style = "display:flex; flex-wrap:wrap; gap:12px;",
    div(style="min-width:180px; flex:1;",
        selectInput("section", "Section", choices = c("51","52"))
    ),
    div(style="min-width:220px; flex:1;",
        dateInput("date", "Class date", value = Sys.Date())
    )
  ),

  tabsetPanel(
    tabPanel("Class jobs",
      fluidRow(
        column(
          4,
          div(class="panel",
              # section and date controls now global
              fluidRow(style = "display: flex; flex-wrap: nowrap;",
                column(6,
                  checkboxGroupInput("jobs_selected", "Select job(s)", choices = DEFAULT_JOBS, selected = DEFAULT_JOBS)
                ),
                column(6,
                  div(
                    actionButton("draw", "Draw jobs"),
                    actionButton("commit", "Commit jobs"),
                    actionButton("refresh", "Refresh")
                  )
                ),
              )
            )
          ),
        column(
          8,
          div(class="panel",
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
          div(class="panel",
            h4("Cold call (one at a time)"),
            fluidRow(
              column(6,
                checkboxInput("exclude_jobs_today", "Exclude today's job assignees", value = TRUE)
              ),
              column(6,
                actionButton("draw_cold", "Draw cold call"),
                actionButton("commit_cold", "Commit cold call"),
                div(style="font-size: 22px; font-weight: 600; padding: 8px 0;",
                    textOutput("coldName")
                )
              )
            )
          )
        )
      )
    ),
    tabPanel("Manual Entry",
      fluidRow(
        column(12,
          div(class="panel",
            h4("Manual log entry"),
            fluidRow(
              column(6,
                # Allow manual override for section/date:
                dateInput("man_date", "Date (override)", value = Sys.Date()),
                selectInput("man_section", "Section (override)", choices = c("51", "52"))
              ),
              column(6,
                selectInput("man_job", "Type", choices = c(DEFAULT_JOBS, "voluntary answer", "cold call", "other")),
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
    tabPanel("Admin",
      fluidRow(
        column(
          8,
          div(class="panel",
              h4("Admin"),
              actionButton("admin_clear_log", "CLEAR log tab (danger)"),
              checkboxInput("admin_confirm", "I understand this deletes data", value = FALSE),
              checkboxInput("admin_delete_date", "Delete date's data", value = FALSE),
              dateInput("admin_delete_date_input", "Date to delete", value = Sys.Date()),
              actionButton("admin_reset_bag", "RESET bag (danger)"),
              actionButton("admin_rebuild_state", "REBUILD state from log (danger)")
          )
        )
      )
    ),
    tabPanel("Notes",
      br(),
      h4("Notes"),
      div(class="mono", "
        - Draw creates a draft assignment.
        - Redraw changes one job (absent student is never logged).
        - Commit writes to the sheet and removes those names from the bag.
        - Use the 'Absentees' tab to mark absent students each day.
      ")
    )
  )
)

# ----------------------------
# SERVER
# ----------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    draft = NULL,   # named vector job -> name
    draft_res = NULL, # FULL draw result (assignments + state updates)
    cycle_id = NA_integer_,
    bag_n = NA_integer_,
    roster_n = NA_integer_,
    cold_current = "",
    jobs_committed = FALSE
  )

  # Track absentees by (section, date)
  rv_absent <- reactiveVal(character(0))

  # Absent picker UI (now on its own tab)
  output$absent_picker_ui <- renderUI({
    roster <- read_roster(input$section)
    tagList(
      selectizeInput("absent_names", label = NULL, choices = roster,
                     selected = rv_absent(), multiple = TRUE,
                     options = list(placeholder = "Pick absentees…"))
    )
  })

  observeEvent(list(input$section, input$date), {
    # Reset absentees selection when section or date changes
    rv_absent(character(0))
  })

  observeEvent(input$absent_names, {
    rv_absent(input$absent_names)
  })

  update_status <- function() {
    roster <- read_roster(input$section)
    # Exclude absentees
    if (length(rv_absent()) > 0) {
      roster <- setdiff(roster, rv_absent())
    }
    rv$roster_n <- length(roster)
    # optionally show per-job remaining counts (safe)
    st <- read_sheet(SHEET_ID, sheet="state", col_types="ccccc") %>%
      mutate(section=trimws(as.character(section)), job=trimws(as.character(job)))
    st_sec <- st %>% filter(section == as.character(input$section))
    rv$bag_summary <- paste0(st_sec$job, ": ", sapply(st_sec$bag_json, function(x) length(parse_bag(x))), collapse="\n")
  }

  observeEvent(TRUE, {
    update_status()
  }, once = TRUE)

  observeEvent(input$refresh, {
    update_status()
    rv$draft <- NULL
    rv$draft_res <- NULL
    rv_absent(character(0))
    rv$jobs_committed <- FALSE
  })

  observeEvent(list(input$section, input$date), {
    rv$draft <- NULL
    rv$draft_res <- NULL
    rv$jobs_committed <- FALSE
    rv$cold_current <- ""
    rv_absent(character(0))
  }, ignoreInit = TRUE)


  observeEvent(input$draw, {
    jobs <- input$jobs_selected
    absentees <- rv_absent()

    if (is.null(jobs) || length(jobs) == 0) {
      showNotification("Select at least one job.", type = "warning")
      return()
    }

    res <- draw_jobs_day(input$section, jobs, absentees = absentees)

    rv$draft_res <- res                 # keep the exact draw
    rv$draft     <- res$assignments     # what you display
    rv$jobs_committed <- FALSE          # allow commit after a new draw
  })


  observeEvent(input$draw_cold, {
    roster <- read_roster(input$section)
    if (length(rv_absent()) > 0) {
      roster <- setdiff(roster, rv_absent())
    }

    pool <- roster
    if (isTRUE(input$exclude_jobs_today) && !is.null(rv$draft)) {
      pool <- setdiff(roster, unname(rv$draft))
    }

    if (length(pool) == 0) {
      showNotification("Cold-call pool is empty.", type = "error")
      rv$cold_current <- ""
      return()
    }

    # Drawing clears the previous displayed name automatically
    rv$cold_current <- sample(pool, 1)
  })

  observeEvent(input$commit, {
    req(rv$draft_res)

    if (rv$jobs_committed) {
      showNotification("Jobs already committed for today.", type = "warning")
      return()
    }

    commit_jobs_day(
      section_id = input$section,
      date       = input$date,
      result     = rv$draft_res   # <-- commit EXACTLY what was drawn/redrawn
    )

    rv$draft <- NULL
    rv$draft_res <- NULL
    rv$jobs_committed <- TRUE
    update_status()
  })


  observeEvent(input$commit_cold, {
    if (rv$cold_current == "") {
      showNotification("No cold call to commit.", type = "warning")
      return()
    }

    st <- read_state(input$section, "cold call")
    cycle_id <- as.character(st$cycle_id[1])

    rows <- tibble(
      ts = as.character(Sys.time()),
      date = as.character(input$date),
      section = as.character(input$section),
      job = "cold call",
      name = rv$cold_current,
      cycle_id = cycle_id
    )

    append_log(rows)

    # Clear after commit
    rv$cold_current <- ""
    showNotification("Cold call committed.", type = "message")
  })

  output$man_name_ui <- renderUI({
    roster <- read_roster(input$man_section)
    selectizeInput("man_name", "Name", choices = roster, multiple = FALSE,
                  options = list(placeholder="Type a name…"))
  })

  observeEvent(input$man_append, {
    nm <- input$man_name
    if (is.null(nm) || nm == "") {
      showNotification("Pick a name.", type="warning")
      return()
    }

    # Use current cycle_id just for reference in log (doesn't change bag)
    st <- read_state(input$man_section, input$man_job)
    cycle_id <- as.character(st$cycle_id[1])

    rows <- tibble(
      ts = as.character(Sys.time()),
      date = as.character(input$man_date),
      section = as.character(input$man_section),
      job = input$man_job,
      name = nm,
      cycle_id = cycle_id
    )

    append_log(rows)
    showNotification("Appended.", type="message")
  })


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
    tagList(
      h4("Redraw (if absent)"),
      fluidRow(
          selectInput("jobToRedraw", "Job to redraw", choices = DEFAULT_JOBS),
          actionButton("redraw", "Redraw selected job")
      )
    )
  })

  observeEvent(input$redraw, {
    req(rv$draft_res)  # require full draft object

    job_to_redraw <- input$jobToRedraw
    absentees <- rv_absent()

    # Ensure bag exists / aligned (may refill) and get current bag snapshot
    info <- ensure_bag_job(input$section, job_to_redraw, absentees = absentees)
    roster <- info$roster
    bag <- info$bag

    taken_today <- unname(rv$draft_res$assignments)
    taken_today <- taken_today[!is.na(taken_today)]

    available <- setdiff(bag, taken_today)
    if (length(available) == 0) {
      showNotification("No one left to redraw from (bag exhausted).", type="error")
      return()
    }

    new_name <- sample(available, 1)

    # Update assignments in the stored draft
    rv$draft_res$assignments[[job_to_redraw]] <- new_name

    # IMPORTANT: also update the pending state update for that job
    # We remove the chosen name from the (current) bag and keep cycle_id from ensure_bag_job()
    rv$draft_res$state_updates[[job_to_redraw]] <- list(
      cycle_id = info$cycle_id,
      new_bag  = setdiff(bag, new_name)
    )

    # Refresh table view
    rv$draft <- rv$draft_res$assignments
    update_status()
  })

  observeEvent(input$admin_clear_log, {
    if (!isTRUE(input$admin_confirm)) {
      showNotification("Check the confirmation box first.", type = "warning")
      return()
    }
    log_clear <- clear_log(delete_date=isTRUE(input$admin_delete_date), section_id=input$section, date=input$admin_delete_date_input)
    # commited is not false, so can commit again
    rv$jobs_committed <- FALSE
    if (!log_clear) showNotification("Log not cleared, check date.", type = "message")
    else showNotification("Log cleared.", type = "warning")
  })

  observeEvent(input$admin_reset_bag, {
    if (!isTRUE(input$admin_confirm)) {
      showNotification("Check the confirmation box first.", type="warning")
      return()
    }
    reset_bag(input$section)
    append_admin_event(input$section, input$date, "RESET_BAG")
    rv$jobs_committed <- FALSE
    rv$draft <- NULL
    showNotification("Bags reset and event logged.", type="message")
  })

  # Add a new button in Admin UI: actionButton("admin_rebuild_state", "REBUILD state from log")
  observeEvent(input$admin_rebuild_state, {
    if (!isTRUE(input$admin_confirm)) {
      showNotification("Check the confirmation box first.", type="warning")
      return()
    }
    ok <- rebuild_state_from_log(input$section, upto_date = input$date)
    if (ok) {
      rv$jobs_committed <- FALSE
      rv$draft <- NULL
      showNotification("State rebuilt from log.", type="message")
    }
  })



  output$lastCommitted <- renderTable({
    lg <- read_sheet(SHEET_ID, sheet = "log", col_types = "cccccc")
    if (nrow(lg) == 0) return(data.frame())
    lg %>%
      filter(section == as.character(input$section)) %>%
      arrange(desc(ts)) %>%
      head(10)
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
