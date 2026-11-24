#!/usr/bin/env Rscript
# Regression coverage for sandbox separation in Class Job Market. A sandbox
# session keeps its records in a separate database. Running a draw or a cold
# call there must never read or write the real class's wages, bids, or
# assignments, even when the sandbox copy uses the same class-date and job ids.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("sandbox-separation-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old <- Sys.getenv(c("CONNECT_CONTENT_DIR", "DEMO_MODE"), unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch, DEMO_MODE = "1")   # every session in this process is a sandbox session
on.exit({
  for (name in names(old)) if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, stats::setNames(list(old[[name]]), name))
  unlink(scratch, recursive = TRUE)
}, add = TRUE)

app <- new.env(parent = globalenv())
source(app_path, local = app)
stopifnot(!identical(normalizePath(app$DB_PATH), normalizePath(app$shared_db_path(demo = TRUE), mustWork = FALSE)))
app$set_setting("class_days", "")
app$set_setting("bid_lock_enabled", "0")

# The same ids on both sides, as when the sandbox is copied from the real class.
ROUND <- 900L; RECAP <- 900L; ANSWER <- 901L; CATEGORY <- 900L
fixture <- function(exec, students, bid) {
  exec("INSERT OR REPLACE INTO weekly_rounds(id,label,class_date,assignment_mode,bidding_enabled,wage_pricing_rule,allow_multiple_jobs)
        VALUES(?,'Shared id date','2026-12-01','wage_bidding',1,'random_wage',1);", list(ROUND))
  exec("INSERT OR REPLACE INTO job_categories(id,name,default_wage,description,voluntary,in_draw) VALUES(?,'Separation role',4,'',0,1);", list(CATEGORY))
  exec("INSERT OR REPLACE INTO job_posts(id,round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
        VALUES(?,?,'Recap',?,2,4,0,1,'start',1);", list(RECAP, ROUND, CATEGORY))
  exec("INSERT OR REPLACE INTO job_posts(id,round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
        VALUES(?,?,'Cold call: answer a question',?,1,4,0,1,'during',1);", list(ANSWER, ROUND, CATEGORY))
  exec("INSERT OR IGNORE INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01','econ 101::01');")
  exec("INSERT OR IGNORE INTO scope_rounds(scope_key,round_id) VALUES('econ 101::01',?);", list(ROUND))
  exec("DELETE FROM scope_active_rounds WHERE scope_key='econ 101::01';")
  exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES('econ 101::01','ECON 101','01',?);", list(ROUND))
  for (uid in c("admin@test.edu", students)) {
    exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
          VALUES(?,?,'',?,'ECON 101','01',1,0);", list(uid, uid, as.integer(uid == "admin@test.edu")))
    exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');", list(uid))
  }
  for (uid in students) for (post in c(RECAP, ANSWER))
    exec("INSERT OR REPLACE INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,?);", list(ROUND, post, uid, bid))
}
real_students <- paste0("real", 1:3, "@test.edu")
sandbox_students <- paste0("sandbox", 1:3, "@test.edu")
fixture(app$db_exec, real_students, 0.5)          # the real class, in the main database
count_main <- function(table) app$db_query(sprintf("SELECT COUNT(*) n FROM %s WHERE round_id=?;", table), list(ROUND))$n[1]
tables <- c("class_wage_snapshots", "job_wage_bid_locks", "job_assignments", "live_score_events", "job_offers")
main_before <- vapply(tables, count_main, numeric(1))
stopifnot(all(main_before == 0))
main_bids_before <- app$db_query("SELECT user_id, job_post_id, min_wage FROM job_wage_bids ORDER BY user_id, job_post_id;")

set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}
shiny::testServer(app$server, {
  stopifnot(isTRUE(.sandbox))
  fixture(db_exec, sandbox_students, 0.5)        # the sandbox copy, in the sandbox database
  set_admin(rv)
  count <- function(table) db_query(sprintf("SELECT COUNT(*) n FROM %s WHERE round_id=?;", table), list(ROUND))$n[1]

  # A full draw, a cold call that is answered, and an offer to the class.
  session$setInputs(draw_timing_filter = "all", run_draw_btn = 1L)
  stopifnot(count("class_wage_snapshots") >= 1L, count("job_wage_bid_locks") == 6L, count("job_assignments") == 2L)
  draw_cold_call()
  stopifnot(is.list(rv$cold_call_draw), rv$cold_call_draw$user_id %in% sandbox_students)
  session$setInputs(record_cold_call_answer_btn = 1L)
  stopifnot(count("live_score_events") == 1L)
  # A cold call that is passed is recorded inside a transaction of its own.
  draw_cold_call()
  session$setInputs(cold_call_pass_btn = 1L)
  stopifnot(count("live_score_events") == 2L)
  session$setInputs(offer_post_id = RECAP, offer_taker_uid = "", offer_record_btn = 1L)
  stopifnot(count("job_offers") == 1L)

  # Everything the sandbox recorded names sandbox students only.
  winners <- db_query("SELECT user_id FROM job_assignments WHERE round_id=?;", list(ROUND))$user_id
  locked <- db_query("SELECT DISTINCT user_id FROM job_wage_bid_locks WHERE round_id=?;", list(ROUND))$user_id
  stopifnot(all(winners %in% sandbox_students), setequal(locked, sandbox_students))
})

# The real class is exactly as it was: no wage frozen, no bid locked, nobody
# assigned, nothing scored or offered, and its bids untouched.
main_after <- vapply(tables, count_main, numeric(1))
stopifnot(identical(main_after, main_before),
          identical(app$db_query("SELECT user_id, job_post_id, min_wage FROM job_wage_bids ORDER BY user_id, job_post_id;"), main_bids_before),
          !any(grepl("sandbox", app$db_query("SELECT user_id FROM users;")$user_id)))

cat("critical sandbox separation: PASS\n")
