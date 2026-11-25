#!/usr/bin/env Rscript
# Practice dates, the per-meeting run sheet, and the deviations log.
#   A practice date runs like any other class date, except that its draw puts
#   every student in the pool and ignores bids, and nothing is paid.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("practice-draw-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)
app <- new.env(parent = globalenv())
source(app_path, local = app)
scope <- "econ 101::01"
roster <- sprintf("s%02d@test.edu", 1:12)
for (uid in roster) app$db_exec(
  "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES(?,?,'',0,'ECON 101','01',1,0);",
  list(uid, paste("Person", uid)))
students <- data.frame(user_id = roster, stringsAsFactors = FALSE)
keep_class <- function(rows) rows[rows$user_id %in% roster, , drop = FALSE]
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Practice role',4,'',0,1);")
cat_id <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
jobs <- c("Note taker", "Prior lecture summary", "Reading summary")
new_date <- function(label, date, practice) {
  app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,wage_pricing_rule,bidding_enabled,practice_no_points,allow_multiple_jobs)
               VALUES(?,?,'wage_bidding','random_wage',1,?,0);", list(label, date, practice))
  rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
  for (job in jobs) app$db_exec(
    "INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active) VALUES(?,?,?,1,4,0,1,'start',1);",
    list(rid, job, cat_id))
  rid
}
round_of <- function(rid) app$db_query("SELECT * FROM weekly_rounds WHERE id=?;", list(rid))
posts_of <- function(rid) app$db_query("SELECT id, job_name FROM job_posts WHERE round_id=? ORDER BY id;", list(rid))
held <- function(rid) app$db_query("SELECT ja.id, ja.user_id, jp.job_name, ja.assigned_wage, ja.status FROM job_assignments ja
                                    JOIN job_posts jp ON jp.id=ja.job_post_id WHERE ja.round_id=? ORDER BY ja.id;", list(rid))

# ── A practice date's draw: everyone in the pool, bids ignored, nothing paid ─
practice <- new_date("Practice 1", "2027-01-19", 1L)
# Only two students bid, both very low. A bid-based draw would pick only them.
for (post in posts_of(practice)$id) for (uid in roster[1:2]) app$db_exec(
  "INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,0.5);", list(practice, post, uid))
winners <- character(0)
for (i in 1:40) {
  drew <- app$run_job_draw(round_of(practice), "all", keep_class)
  now <- held(practice)
  stopifnot(isTRUE(drew$changed), nrow(now) == length(jobs), all(now$assigned_wage == 0),
            !any(duplicated(now$user_id)))                     # one job per student, as the date is set
  winners <- c(winners, now$user_id)
}
# Over 120 picks from 12 students, non-bidders are drawn about as often as bidders.
stopifnot(length(unique(winners)) >= 11L, mean(winners %in% roster[1:2]) < 0.35)
# One job on its own, and a redraw, also come from the whole class.
app$db_exec("DELETE FROM job_assignments WHERE round_id=?;", list(practice))
one <- app$run_job_draw(round_of(practice), "all", keep_class, post_id = posts_of(practice)$id[1])
first <- held(practice)
moved <- app$redraw_job(first$id[1], scope_filter = keep_class, reason = "redrawn")
after <- held(practice)
stopifnot(isTRUE(one$changed), nrow(first) == 1L, isTRUE(moved$changed), nrow(after) == 2L,
          after$status[1] == "redrawn", after$status[2] == "assigned", after$user_id[2] != after$user_id[1])
# Scoring and committing a practice job pays nothing.
app$queue_assignment_score(after$id[2], "complete", "prof"); app$commit_pending_scores(identity)
stopifnot(nrow(app$db_query("SELECT 1 FROM token_ledger WHERE user_id LIKE '%@test.edu';")) == 0L,
          app$db_query("SELECT outcome FROM job_assignments WHERE id=?;", list(after$id[2]))$outcome == "complete")
# With cold calls limited to bidders, a practice date still calls on anyone.
app$set_setting("cold_call_bidders_only", "1")
stopifnot(nrow(app$cold_call_eligible(practice, students)) == length(roster),
          nrow(app$cold_call_eligible(practice, students, "board")) == length(roster))
app$set_setting("cold_call_bidders_only", "0")

# ── A paid date still draws from bids ────────────────────────────────────────
paid <- new_date("Paid 1", "2027-02-09", 0L)
for (post in posts_of(paid)$id) for (uid in roster[1:2]) app$db_exec(
  "INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,0.5);", list(paid, post, uid))
app$run_job_draw(round_of(paid), "all", keep_class)
stopifnot(all(held(paid)$user_id %in% roster[1:2]), all(held(paid)$assigned_wage > 0))

# ── Run sheet: the same steps on both kinds of date, read from the records ───
sheet <- function(rid) { s <- app$class_run_sheet(scope, rid, students); setNames(s$steps, vapply(s$steps, `[[`, "", "key")) }
keys <- c("jobs_drawn", "jobs_shown", "cold_calls", "jobs_scored", "committed")
stopifnot(identical(names(sheet(paid)), keys), identical(names(sheet(practice)), keys),
          isTRUE(app$class_run_sheet(scope, practice, students)$practice), !isTRUE(app$class_run_sheet(scope, paid, students)$practice))
stopifnot(sheet(paid)$jobs_drawn$done, !sheet(paid)$jobs_shown$done, !sheet(paid)$cold_calls$done,
          !sheet(paid)$jobs_scored$done, !sheet(paid)$committed$done)
stopifnot(!isTRUE(app$log_deviation(scope, paid, "nonsense")$ok), !isTRUE(app$log_deviation(scope, paid, "other", "")$ok),
          isTRUE(app$log_deviation(scope, paid, "volunteer_for_cold_call", "ran short of time", "prof")$ok))
stopifnot(!sheet(paid)$cold_calls$done, sheet(paid)$cold_calls$excused, !sheet(paid)$jobs_shown$excused,
          identical(unlist(app$class_run_sheet(scope, paid, students)$deviations), "Took a volunteer instead of a cold call"))
app$toggle_section_reveal(paid, "01", "all")
app$queue_cold_call(paid, roster[1], "answer", "prof")
stopifnot(sheet(paid)$jobs_shown$done, sheet(paid)$cold_calls$done, !sheet(paid)$jobs_scored$done)
for (id in held(paid)$id) app$queue_assignment_score(id, "complete", "prof")
stopifnot(sheet(paid)$jobs_scored$done, !sheet(paid)$committed$done)
app$commit_pending_scores(identity)
stopifnot(all(vapply(app$class_run_sheet(scope, paid, students)$steps, `[[`, TRUE, "done")))

cat("critical practice draw and run sheet: PASS\n")
