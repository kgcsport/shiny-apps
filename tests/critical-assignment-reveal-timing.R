#!/usr/bin/env Rscript
# Critical contract: start/end visibility follows each assignment's saved timing.

app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
source_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")

# Explicit assignment timing wins. A manual "Any" is deliberately treated as
# inherited timing rather than silently exposing an end-of-class job at start.
timing_sql <- "COALESCE(NULLIF(NULLIF(ja.selection_time,''),'any'),"
if (length(gregexpr(timing_sql, source_text, fixed=TRUE)[[1]]) < 2L) {
  stop("Today must use explicit job-assignment timing for both personal and class reveal lists.")
}
required <- c(
  "INSERT OR IGNORE INTO job_assignments(round_id, user_id, job_post_id, assigned_wage, assignment_mode, selection_time)",
  "post_timing <- setNames",
  # Both Today lists decide visibility through the one shared rule.
  "assignment_is_revealed(sec, today_my_assign$selection_time[i], sr,",
  "assignment_is_revealed(revealed_jobs$section[i],",
  "reveal_timings_for_scope <- function(scope)"
)
missing <- required[!vapply(required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(missing)) stop("Assignment reveal timing contract missing: ", paste(missing, collapse=" | "))

cat("critical assignment reveal timing: source contract OK\n")

# Behavior: a student with a start-of-class job and an end-of-class job sees
# each only after its own group is shown to their section.
if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("RSQLite", quietly=TRUE)) quit(status=0)
scratch <- tempfile("reveal-timing-")
dir.create(file.path(scratch, "data"), recursive=TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset=NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR=scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR=old_content)
  unlink(scratch, recursive=TRUE)
}, add=TRUE)
app <- new.env(parent=globalenv())
source(app_path, local=app)
app$set_setting("class_days", ""); app$set_setting("bid_lock_enabled", "0")
for (tbl in c("scope_active_rounds", "scope_rounds", "section_scope_memberships", "weekly_rounds",
              "job_posts", "job_assignments", "assignment_timing_reveals"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
for (sec in c("01", "02")) {
  uid <- sprintf("s%s@test.edu", sec)
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
               VALUES(?,?,'',0,'ECON 101',?,1,0);", list(uid, uid, sec))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101',?);", list(uid, sec))
}
scope <- "econ 101::01"
class_day <- as.character(as.Date(format(Sys.time(), "%Y-%m-%d", tz=app$get_setting("class_tz", "America/New_York"))))
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('Reveal class',?,'random',0);", list(class_day))
rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Reveal role',2,'',0,1);")
cat_id <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
give <- function(name, post_timing, assignment_timing) {
  app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
               VALUES(?,?,?,1,2,0,1,?,1);", list(rid, name, cat_id, post_timing))
  post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO job_assignments(round_id,user_id,job_post_id,assigned_wage,assignment_mode,status,selection_time)
               VALUES(?,'s01@test.edu',?,2,'random','assigned',?);", list(rid, post, assignment_timing))
}
give("Opening recap", "start", "start")
give("Closing notes", "end", "end")
# A manual "Any" on the assignment inherits the job's end-of-class timing.
give("Inherited closer", "end", "any")

today_for <- function() {
  seen <- NULL
  shiny::testServer(app$server, {
    rv$authed <- TRUE; rv$user_id <- "s01@test.edu"; rv$name <- "s01"; rv$is_admin <- FALSE
    rv$course <- "ECON 101"; rv$section <- "01"; rv$active_course <- "ECON 101"
    rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
    session$setInputs(arc_tabs="Today"); session$flushReact()
    o <- output$today_tab
    html <- paste(as.character(if (is.list(o) && !is.null(o$html)) o$html else o), collapse="")
    # Only the reveal lists: the Job Pools section, last on the page, names
    # every job whether or not it has been shown.
    seen <<- gsub("<[^>]+>", " ", sub("^(.*)Job Pools.*$", "\\1", html))
  })
  seen
}
shows <- function(text, job) grepl(job, text, fixed=TRUE)
expect <- function(text, visible, hidden, when) {
  for (job in visible) if (!shows(text, job)) stop(when, ": the student should see ", job)
  for (job in hidden) if (shows(text, job)) stop(when, ": the student should not see ", job)
}
all_jobs <- c("Opening recap", "Closing notes", "Inherited closer")
expect(today_for(), character(0), all_jobs, "Nothing shown yet")
# Another section's reveal does not show this section's jobs.
app$toggle_section_reveal(rid, "02", "all")
expect(today_for(), character(0), all_jobs, "Other section shown")
app$toggle_section_reveal(rid, "01", "start")
expect(today_for(), "Opening recap", c("Closing notes", "Inherited closer"), "Start of class shown")
app$toggle_section_reveal(rid, "01", "end")
expect(today_for(), all_jobs, character(0), "Start and end shown")
app$toggle_section_reveal(rid, "01", "start")
expect(today_for(), c("Closing notes", "Inherited closer"), "Opening recap", "Start hidden again")

cat("critical assignment reveal timing: PASS\n")
