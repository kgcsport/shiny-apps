#!/usr/bin/env Rscript
# Critical contract: the Live Tracker's "N students assigned" line reports the
# same per-section reveal state as the Reveal/Hide button beside it. It used to
# read the retired app-wide flag and said "Hidden from students" after a reveal.

app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("RSQLite", quietly=TRUE)) quit(status=0)
scratch <- tempfile("tracker-reveal-status-")
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
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
             VALUES('prof@test.edu','Prof','',1,'ECON 101','01',1,0);")
for (sec in c("01", "02")) {
  uid <- sprintf("s%s@test.edu", sec)
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
               VALUES(?,?,'',0,'ECON 101',?,1,0);", list(uid, uid, sec))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101',?);", list(uid, sec))
}
class_day <- as.character(as.Date(format(Sys.time(), "%Y-%m-%d", tz=app$get_setting("class_tz", "America/New_York"))))
app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('Reveal class',?,'random',0);", list(class_day))
rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
for (sec in c("01", "02")) {
  scope <- paste0("econ 101::", sec)
  app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101',?,?);", list(sec, scope))
  app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
  app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101',?,?);", list(scope, sec, rid))
}
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Status role',2,'',0,1);")
cat_id <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
give <- function(name, timing, uid) {
  app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
               VALUES(?,?,?,2,2,0,1,?,1);", list(rid, name, cat_id, timing))
  post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO job_assignments(round_id,user_id,job_post_id,assigned_wage,assignment_mode,status,selection_time)
               VALUES(?,?,?,2,'random','assigned',?);", list(rid, uid, post, timing))
}
give("Opening recap", "start", "s01@test.edu")
give("Closing notes", "end", "s02@test.edu")

# What the instructor's Live Tracker says, and what its Reveal/Hide button
# reads, with `sections` selected in the banner. `click` presses the button,
# so the test follows the instructor's own path; the tracker is then read in a
# new session, because the page rereads the database on a real-time poll that
# does not tick inside testServer.
tracker <- function(sections, group="start", click=FALSE) {
  seen <- NULL
  shiny::testServer(app$server, {
    rv$authed <- TRUE; rv$user_id <- "prof@test.edu"; rv$name <- "Prof"; rv$is_admin <- TRUE; rv$admin_role <- "super_admin"
    rv$course <- "ECON 101"; rv$section <- "01"; rv$active_course <- "ECON 101"
    rv$active_section <- sections[1]; rv$active_sections <- sections; rv$impersonating <- FALSE
    session$setInputs(arc_tabs="Live Tracker", section_reveal_timing=group); session$flushReact()
    if (click) { session$setInputs(toggle_section_reveal_btn=1); session$flushReact() }
    o <- output$live_tracker_tab
    html <- paste(as.character(if (is.list(o) && !is.null(o$html)) o$html else o), collapse="")
    line <- regmatches(html, regexpr("[0-9]+ students assigned[^<]*", html))
    button <- regmatches(html, regexpr("<button[^>]*id=\"toggle_section_reveal_btn\"[^>]*>[^<]*", html))
    if (!length(line)) stop("The Live Tracker did not render the assigned-students line.")
    if (!length(button)) stop("The Live Tracker did not render the Reveal/Hide button.")
    seen <<- list(line=sub("^.*students assigned[^A-Za-z0-9]*", "", line),
                  button=trimws(sub("^.*>", "", button)))
  })
  if (click) tracker(sections, group) else seen
}
expect <- function(got, line, button, when) {
  if (!identical(got$line, line)) stop(when, ": the line should read '", line, "' but reads '", got$line, "'")
  if (!identical(got$button, button)) stop(when, ": the button should read '", button, "' but reads '", got$button, "'")
}
reveal_rows <- function() app$db_query(
  "SELECT section,timing,revealed FROM assignment_timing_reveals ORDER BY section,timing;")

expect(tracker("01"), "Hidden from students", "Reveal", "Nothing shown yet")
# The defect: the instructor reveals start-of-class jobs for section 01.
expect(tracker("01", click=TRUE), "Visible to students (start of class)", "Hide", "Start shown by the button")
shown <- reveal_rows()
if (!identical(shown$section, "01") || !identical(shown$timing, "start") || as.integer(shown$revealed) != 1L)
  stop("The Reveal button should have shown only section 01's start-of-class group.")
# The line reports every group; the button reports the group picked beside it.
expect(tracker("01", group="end"), "Visible to students (start of class)", "Reveal", "End group picked, start shown")
# Another section's state is its own.
expect(tracker("02"), "Hidden from students", "Reveal", "Other section untouched")
expect(tracker(c("01", "02")), "01: visible (start of class) · 02: hidden", "Reveal", "Two sections that differ")
app$toggle_section_reveal(rid, "01", "end")
expect(tracker("01"), "Visible to students", "Hide", "Start and end shown")
expect(tracker("01", group="all"), "Visible to students", "Hide", "All groups picked, all shown")
app$toggle_section_reveal(rid, "01", "start")
expect(tracker("01"), "Visible to students (end of class)", "Reveal", "Start hidden again")
app$toggle_section_reveal(rid, "01", "end")
expect(tracker("01"), "Hidden from students", "Reveal", "Everything hidden again")
# Display only: rendering the tracker writes nothing to the reveal table.
before <- reveal_rows(); invisible(tracker("01")); invisible(tracker(c("01", "02")))
if (!identical(before, reveal_rows())) stop("Rendering the Live Tracker changed the reveal table.")
# The retired app-wide flag no longer decides the text.
app$db_exec("UPDATE arcade_state SET assignments_revealed=1 WHERE id=1;")
expect(tracker("01"), "Hidden from students", "Reveal", "Old app-wide flag set")

cat("critical tracker reveal status: PASS\n")
