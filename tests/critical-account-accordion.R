#!/usr/bin/env Rscript

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
source_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")
required <- c(
  'account_section("Needs your attention"',
  'account_section("Jobs"',
  'account_section("Tokens"',
  'account_section("Grades & feedback"',
  'account_section("Profile"',
  'actionButton("refresh_account_assignments"',
  "assignment_review_gradebook_export()",
  "account_pending_problem_sets(",
  'updateTabsetPanel(session, "arc_tabs", selected="Assignments")'
)
missing <- required[!vapply(required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(missing)) stop("Account accordion contract missing: ", paste(missing, collapse=" | "))

# Pure status cases: no upload -> problem-set task; PDF only -> self-grading;
# final Worker submission or a recorded grade -> no Account reminder.
expressions <- parse(file=app_path)
helper_names <- c("%||%", "account_clean_text", "account_assignment_key", "account_rows_df",
                  "account_worker_progress", "account_worker_schedules",
                  "account_pending_problem_sets")
helpers <- new.env(parent=baseenv())
for (expr in expressions) if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
  is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% helper_names) eval(expr, helpers)

sets <- data.frame(
  name=c("PS1","PS2","PS3","PS4"),
  original_deadline=rep("2026-11-10",4),
  solutions_posted_at=rep("2026-11-11",4),
  cloudflare_assignment_id=sprintf("econ342-2026-ps%02d",1:4),
  stringsAsFactors=FALSE)
export <- list(rows=data.frame(
  externalId=c("student@example.edu","student@example.edu"),
  studentEmail=c("student@example.edu","student@example.edu"),
  studentName=c("Student A","Student A"),
  assignmentId=c("econ342-2026-ps01","econ342-2026-ps02"),
  assignmentTitle=c("Problem Set 1","Problem Set 2"),
  gradebookKey=c("PS1","PS2"), status=c("submitted","draft"),
  scanVerifiedAt=c("2026-11-09T12:00:00Z","2026-11-09T12:00:00Z"),
  submittedAt=c("2026-11-11T12:00:00Z",""), updatedAt=c("",""),
  stringsAsFactors=FALSE))
schedules <- helpers$account_worker_schedules(data.frame(
  id=sprintf("econ342-2026-ps%02d",1:4), title=paste("Problem Set",1:4),
  gradebookKey=paste0("PS",1:4), dueAt=rep("2026-11-10T22:00:00Z",4),
  solutionsAt=rep("2026-11-11T14:00:00Z",4), gradingDueAt=rep("2026-11-13T22:00:00Z",4),
  stringsAsFactors=FALSE))
progress <- helpers$account_worker_progress(export, "student@example.edu", "Student A")
pending <- helpers$account_pending_problem_sets(
  sets, data.frame(assignment_name="PS4"), progress, schedules, TRUE)
if (!identical(as.character(pending$name), c("PS2","PS3")))
  stop("Completed Worker submissions and recorded grades must disappear from Account.")
if (!identical(as.character(pending$stage), c("self_grading","problem_set")))
  stop("PDF-only and never-uploaded assignments have the wrong Account stage.")
fallback <- helpers$account_pending_problem_sets(
  sets, data.frame(assignment_name="PS4"), data.frame(), data.frame(), FALSE)
if (!identical(as.character(fallback$name), c("PS1","PS2","PS3")))
  stop("Worker outage fallback must hide only assignments with a recorded grade.")

if (!requireNamespace("shiny",quietly=TRUE) || !requireNamespace("DBI",quietly=TRUE) ||
    !requireNamespace("RSQLite",quietly=TRUE)) {
  cat("critical Account accordion: source and helper cases OK\n"); quit(status=0)
}

# Exact Account path against a disposable shared SQLite database and mocked
# private Worker responses. No network or production data is touched.
scratch <- tempfile("account-accordion-")
dir.create(file.path(scratch,"data"), recursive=TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset=NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR=scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR=old_content)
  unlink(scratch,recursive=TRUE)
}, add=TRUE)
app <- new.env(parent=globalenv())
source(app_path, local=app)
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES('student@example.edu','Student A','',0,'ECON 342','01',1,0);")
app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES('student@example.edu','ECON 342','01');")
app$db_exec("DELETE FROM problem_sets;")
for (i in seq_len(nrow(sets))) app$db_exec(
  "INSERT INTO problem_sets(name,original_deadline,solutions_posted_at,active,cloudflare_assignment_id) VALUES(?,?,?,1,?);",
  unname(as.list(sets[i,c("name","original_deadline","solutions_posted_at","cloudflare_assignment_id")])))
app$db_exec("INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct) VALUES('student@example.edu','PS4',100,100,100);")
app$assignment_review_configured <- function() TRUE
app$assignment_review_assignments <- function() data.frame(
  id=sprintf("econ342-2026-ps%02d",1:4), title=paste("Problem Set",1:4),
  gradebookKey=paste0("PS",1:4), dueAt=rep("2026-11-10T22:00:00Z",4),
  solutionsAt=rep("2026-11-11T14:00:00Z",4), gradingDueAt=rep("2026-11-13T22:00:00Z",4),
  stringsAsFactors=FALSE)
app$assignment_review_gradebook_export <- function() export

shiny::testServer(app$server, {
  rv$authed <- TRUE; rv$user_id <- "student@example.edu"; rv$name <- "Student A"
  rv$course <- "ECON 342"; rv$section <- "01"; rv$is_admin <- FALSE
  rv$active_course <- "ECON 342"; rv$active_section <- "01"; rv$active_sections <- "01"
  session$setInputs(arc_tabs="Account")
  session$flushReact()
  html <- paste(as.character(output$account_tab), collapse="")
  for (label in c("Needs your attention","Jobs","Tokens","Grades &amp; feedback","Profile","PS2","PS3","Finish self-grading"))
    if (!grepl(label,html,fixed=TRUE)) stop("Rendered Account is missing: ",label)
  for (label in c(">PS1<",">PS4<")) if (grepl(label,html,fixed=TRUE))
    stop("Completed assignment leaked into rendered Account: ",label)
})

cat("critical Account accordion: PASS\n")
