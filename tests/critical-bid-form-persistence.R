#!/usr/bin/env Rscript
# Regression coverage for the Job Market bid form in Class Job Market:
#   1. instructor actions that do not change the form (scores, assignments)
#      do not rebuild it;
#   2. when the form does rebuild, a bid the student typed but has not saved is
#      carried into the new form;
#   3. a new class date keeps the student's saved bids and their unsaved typing.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("bid-form-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)

app <- new.env(parent = globalenv())
source(app_path, local = app)
app$set_setting("class_days", "")
app$set_setting("bid_lock_enabled", "0")
for (tbl in c("scope_active_rounds", "scope_rounds", "section_scope_memberships", "weekly_rounds",
              "job_posts", "job_assignments", "job_wage_bids", "live_score_events", "token_ledger"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
for (person in list(c("admin@test.edu", 1L), c("s1@test.edu", 0L), c("s2@test.edu", 0L))) {
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
               VALUES(?,?,'',?,'ECON 101','01',1,0);", list(person[1], person[1], as.integer(person[2])))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');", list(person[1]))
}
scope <- "econ 101::01"
class_day <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = app$get_setting("class_tz", "America/New_York")))
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('First class',?,'wage_bidding',1);",
            list(as.character(class_day)))
rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Form role',2,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
post_sql <- "INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
             VALUES(?,?,?,1,2,0,1,'start',1);"
new_post <- function(name) {
  app$db_exec(post_sql, list(rid, name, role_cat))
  as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
}
recap <- new_post("Recap"); notes <- new_post("Note taker")
app$db_exec("INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,'s1@test.edu',3);", list(rid, recap))

set_student <- function(rv, uid) {
  rv$authed <- TRUE; rv$user_id <- uid; rv$name <- uid
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- FALSE
  rv$active_course <- "ECON 101"; rv$active_section <- "01"
  rv$active_sections <- "01"; rv$is_demo <- FALSE
}
# The instructor's session runs in a second R process against the same
# database file, as a separate browser would, because the student's session
# must stay open while the class date changes.
instructor_changes_date <- function(day) {
  script <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages({ library(DBI); library(RSQLite) })",
    sprintf("app <- new.env(parent = globalenv()); source(%s, local = app)", deparse(normalizePath(app_path))),
    "shiny::testServer(app$server, {",
    "  rv$authed <- TRUE; rv$user_id <- 'admin@test.edu'; rv$name <- 'Instructor'",
    "  rv$course <- 'ECON 101'; rv$section <- '01'; rv$is_admin <- TRUE",
    "  rv$admin_role <- 'super_admin'; rv$active_course <- 'ECON 101'",
    "  rv$active_section <- '01'; rv$active_sections <- '01'; rv$impersonating <- FALSE",
    sprintf("  session$setInputs(market_class_date = as.Date('%s'))", as.character(day)),
    "})"), script)
  status <- system2(file.path(R.home("bin"), "Rscript"), script, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) stop("The instructor session failed to change the class date.")
}
tick <- function(session, n = 4L) { for (i in seq_len(n)) app$shared_db_check(); session$flushReact() }
box_value <- function(html, post_id) {
  box <- regmatches(html, regexpr(sprintf("<input[^>]*id=\"wbp_%d\"[^>]*>", post_id), html))
  if (!length(box)) stop("Bid box missing for post ", post_id)
  sub("^.*value=\"([^\"]*)\".*$", "\\1", box)
}
post_id_for <- function(round_id, name) as.integer(app$db_query(
  "SELECT id FROM job_posts WHERE round_id=? AND job_name=?;", list(round_id, name))$id[1])

shiny::testServer(app$server, {
  set_student(rv, "s1@test.edu")
  session$setInputs(arc_tabs = "Job Market")
  tick(session)
  form <- output$jm_bid_form$html
  # The shell holds the form; the summary is a separate part of the page.
  stopifnot(grepl("jm_bid_form", output$job_market_tab$html, fixed = TRUE),
            grepl("jm_summary", output$job_market_tab$html, fixed = TRUE),
            !grepl("wbp_", output$job_market_tab$html, fixed = TRUE),
            box_value(form, recap) == "3", box_value(form, notes) == "2")
  renders <- bid_form_memory$.renders

  # The student types into both boxes and saves nothing.
  do.call(session$setInputs, stats::setNames(list(7.5, 9), paste0("wbp_", c(recap, notes))))

  # 1. Class-wide events that the form does not show: an assignment, a score,
  #    a classmate's bid. The summary updates; the form is not rebuilt.
  app$db_exec("INSERT INTO job_assignments(round_id,user_id,job_post_id,assigned_wage,assignment_mode,status)
               VALUES(?,'s1@test.edu',?,3,'wage_bidding','assigned');", list(rid, recap))
  tick(session)
  stopifnot(grepl("Recap", output$jm_summary$html, fixed = TRUE), bid_form_memory$.renders == renders)
  app$db_exec("INSERT INTO live_score_events(round_id,user_id,event_kind,outcome,tokens,logged_by,committed_at)
               VALUES(?,'s2@test.edu','participation','succeed',1,'admin@test.edu',datetime('now'));", list(rid))
  app$db_exec("INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,'s2@test.edu',1);", list(rid, recap))
  tick(session)
  stopifnot(bid_form_memory$.renders == renders, identical(output$jm_bid_form$html, form))

  # 2. The instructor adds a job. The form must rebuild to show it, and the
  #    two unsaved numbers come with it.
  scout <- new_post("Example scout")
  tick(session)
  form <- output$jm_bid_form$html
  stopifnot(bid_form_memory$.renders == renders + 1L, grepl("Example scout", form, fixed = TRUE),
            box_value(form, recap) == "7.5", box_value(form, notes) == "9", box_value(form, scout) == "2")
  # ...and they survive a second rebuild, still unsaved.
  critic <- new_post("Critic")
  tick(session)
  form <- output$jm_bid_form$html
  stopifnot(box_value(form, recap) == "7.5", box_value(form, notes) == "9",
            app$db_query("SELECT min_wage FROM job_wage_bids WHERE user_id='s1@test.edu';")$min_wage == 3)

  # 3. A new class date, made with the instructor's own control while the
  #    student's page is open. Each job gets a new copy with a new box id.
  instructor_changes_date(class_day + 7)
  next_rid <- as.integer(app$db_query("SELECT round_id FROM scope_active_rounds WHERE scope_key=?;", list(scope))$round_id[1])
  stopifnot(next_rid != rid)
  tick(session)
  form <- output$jm_bid_form$html
  next_recap <- post_id_for(next_rid, "Recap"); next_notes <- post_id_for(next_rid, "Note taker")
  carried <- app$db_query("SELECT min_wage FROM job_wage_bids WHERE round_id=? AND user_id='s1@test.edu';", list(next_rid))
  stopifnot(next_recap != recap,
            # the saved bid came forward to the new date...
            nrow(carried) == 1L, carried$min_wage[1] == 3,
            # ...and so did what the student had typed but not saved.
            box_value(form, next_recap) == "7.5", box_value(form, next_notes) == "9")

  # 4. Saving stores what is on screen, and a later rebuild shows the saved bids.
  do.call(session$setInputs, stats::setNames(list(7.5, 9, 1L), c(paste0("wbp_", c(next_recap, next_notes)), "submit_wage_bids")))
  tick(session)
  saved <- app$db_query("SELECT job_post_id, min_wage FROM job_wage_bids WHERE round_id=? AND user_id='s1@test.edu' ORDER BY job_post_id;", list(next_rid))
  stopifnot(setequal(saved$min_wage[saved$job_post_id %in% c(next_recap, next_notes)], c(7.5, 9)))
  form <- output$jm_bid_form$html
  stopifnot(box_value(form, next_recap) == "7.5", box_value(form, next_notes) == "9")
})

cat("critical bid form persistence: PASS\n")
