#!/usr/bin/env Rscript
# Critical regression fixture: unresolved score events must remain actionable
# in Live Score Audit after their class date has passed.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
app_source <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
tracker_start <- regexpr("output$live_tracker_tab <- renderUI({", app_source, fixed=TRUE)
if (tracker_start[[1]] < 1L) stop("Could not locate the Live Tracker render function.")
tracker_source <- substr(app_source, tracker_start[[1]], tracker_start[[1]] + 12000L)
tracker_required <- c(
  "revealed  <- td$revealed",
  "round     <- td$round",
  "mode      <- if (nrow(round))",
  "wage_mode <- identical(mode, \"wage_bidding\")",
  "rid       <- if (nrow(round))",
  "cur_course <- scoped_course()",
  "expanded_sections <- scoped_sections()"
)
tracker_positions <- vapply(tracker_required, function(x) regexpr(x, tracker_source, fixed=TRUE)[1], integer(1))
if (any(tracker_positions < 1L)) stop("Live Tracker required state is missing: ", paste(tracker_required[tracker_positions < 1L], collapse=" | "))
if (max(tracker_positions[1:5]) > regexpr("current_assignments", tracker_source, fixed=TRUE)[1]) stop("Live Tracker state must be initialized before it is used.")

audit_start <- regexpr("pending_scores <- \\{", app_source, perl = TRUE)
if (audit_start[[1]] < 1L) stop("Could not locate the Live Score Audit query.")
audit_source <- substr(app_source, audit_start[[1]], audit_start[[1]] + 5000L)
if (!grepl("WHERE lse.committed_at IS NULL", audit_source, fixed = TRUE)) {
  stop("Live Score Audit must select every unresolved score event.")
}
if (grepl("WHERE lse.round_id=? AND lse.committed_at IS NULL", audit_source, fixed = TRUE)) {
  stop("Live Score Audit must not hide unresolved events from prior class dates.")
}
if (grepl("lse.created_at;\\\"\\), list\\(\\)", audit_source, perl = TRUE)) {
  stop("Live Score Audit must not pass an empty parameter list to parameterless SQL.")
}

con <- dbConnect(SQLite(), ":memory:")
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "CREATE TABLE users (user_id TEXT PRIMARY KEY, display_name TEXT, course TEXT, section TEXT)")
dbExecute(con, "CREATE TABLE weekly_rounds (id INTEGER PRIMARY KEY, class_date TEXT)")
dbExecute(con, "CREATE TABLE job_posts (id INTEGER PRIMARY KEY, job_name TEXT)")
dbExecute(con, "CREATE TABLE job_assignments (id INTEGER PRIMARY KEY, round_id INTEGER, job_post_id INTEGER, scheduled_date TEXT)")
dbExecute(con, "CREATE TABLE live_score_events (id INTEGER PRIMARY KEY, round_id INTEGER, user_id TEXT, job_assignment_id INTEGER, job_post_id INTEGER, event_kind TEXT, outcome TEXT, tokens REAL, created_at TEXT, committed_at TEXT)")

dbExecute(con, "INSERT INTO users VALUES ('david','David','Econ 342','Econ 342-01'), ('zoe','Zoe','Econ 342','Econ 342-01'), ('other','Other','Econ 101','Econ 101-01')")
dbExecute(con, "INSERT INTO weekly_rounds VALUES (7,'2026-10-07'), (12,'2026-10-12'), (26,'2026-10-26')")
dbExecute(con, "INSERT INTO job_posts VALUES (1,'Critic/skeptic'), (2,'Note taker')")
dbExecute(con, "INSERT INTO job_assignments VALUES (1,7,1,'2026-10-07'), (2,12,2,'2026-10-12'), (3,26,1,'2026-10-26')")
dbExecute(con, "INSERT INTO live_score_events VALUES (1,7,'david',1,1,'assignment','complete',1.5,'2026-10-07 17:00:00',NULL), (2,12,'zoe',2,2,'assignment','missed',0,'2026-10-12 17:00:00',NULL), (3,26,'other',3,1,'assignment','complete',2,'2026-10-26 17:00:00',NULL), (4,7,'david',1,1,'assignment','complete',1.5,'2026-10-08 17:00:00','2026-10-08 18:00:00')")

audit_sql <- "SELECT lse.id, lse.round_id, lse.user_id, u.display_name, u.course, u.section, lse.job_assignment_id, lse.job_post_id, lse.event_kind, lse.outcome, lse.tokens, lse.created_at, COALESCE(NULLIF(ja.scheduled_date,''), NULLIF(wr_assignment.class_date,''), NULLIF(wr_event.class_date,''), substr(lse.created_at,1,10)) AS job_date, COALESCE(jp.job_name, ap.job_name, '') AS job_name FROM live_score_events lse JOIN users u ON u.user_id=lse.user_id LEFT JOIN weekly_rounds wr_event ON wr_event.id=lse.round_id LEFT JOIN job_posts jp ON jp.id=lse.job_post_id LEFT JOIN job_assignments ja ON ja.id=lse.job_assignment_id LEFT JOIN weekly_rounds wr_assignment ON wr_assignment.id=ja.round_id LEFT JOIN job_posts ap ON ap.id=ja.job_post_id WHERE lse.committed_at IS NULL ORDER BY job_date DESC, u.course, u.section, u.display_name, lse.created_at"
rows <- dbGetQuery(con, audit_sql)
scope_rows <- rows[rows$course == "Econ 342" & rows$section == "Econ 342-01", , drop = FALSE]
if (!identical(sort(scope_rows$id), c(1L, 2L))) {
  stop("Old-date unresolved assignment scores are missing from the selected-scope audit queue.")
}
if (any(scope_rows$job_date %in% c("2026-10-26", NA_character_))) {
  stop("Audit queue date resolution is incorrect for historical assignments.")
}
cat("critical Live Score Audit regression fixture: OK\n")
