#!/usr/bin/env Rscript
# The practice lottery, the per-meeting run sheet, and the deviations log.
# A class with no practice schedule is untouched by all three.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("practice-lottery-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)
app <- new.env(parent = globalenv())
source(app_path, local = app)
jobs <- app$PRACTICE_DEFAULT_JOBS
scope <- "econ 101::01"
roster <- sprintf("s%02d@test.edu", 1:25)

# ── The lottery ──────────────────────────────────────────────────────────────
set.seed(99); before <- .Random.seed
made <- app$generate_practice_schedule(scope, roster, seed = 4242)
stopifnot(isTRUE(made$ok), identical(before, .Random.seed))          # leaves the session's randomness alone
arms <- app$db_query("SELECT user_id, arm, seed, schedule_hash FROM practice_arms WHERE scope_key=?;", list(scope))
turns <- app$db_query("SELECT user_id, job_name, meeting_no FROM practice_turns WHERE scope_key=?;", list(scope))
per_student <- table(factor(turns$user_id, levels = roster))
stopifnot(nrow(arms) == 25L, identical(as.integer(table(arms$arm)[c("none", "one", "two")]), c(5L, 10L, 10L)),
          nrow(turns) == 30L, all(table(turns$job_name) == 6L), setequal(turns$job_name, jobs),
          all(table(turns$meeting_no) == 5L), all(table(turns$job_name, turns$meeting_no) == 1L),
          all(per_student[arms$user_id[arms$arm == "none"]] == 0L), all(per_student[arms$user_id[arms$arm == "one"]] == 1L),
          all(per_student[arms$user_id[arms$arm == "two"]] == 2L),
          !any(duplicated(turns[c("user_id", "job_name")])), !any(duplicated(turns[c("user_id", "meeting_no")])),
          all(arms$seed == 4242L), length(unique(arms$schedule_hash)) == 1L, nchar(made$hash) == 64L)
# Generated once: a second attempt changes nothing.
again <- app$generate_practice_schedule(scope, roster, seed = 1)
stopifnot(!isTRUE(again$ok), nrow(app$db_query("SELECT 1 FROM practice_turns WHERE scope_key=?;", list(scope))) == 30L)
# The same seed and roster give the same schedule; another seed gives another.
same <- app$generate_practice_schedule("copy::a", rev(toupper(roster)), seed = 4242)
other <- app$generate_practice_schedule("copy::b", roster, seed = 4243)
stopifnot(identical(same$hash, made$hash), !identical(other$hash, made$hash))
# A roster that is not 25: the no-practice group stays at 5, the odd student gets one job.
odd <- app$generate_practice_schedule("copy::c", roster[1:22], seed = 7)
odd_arms <- table(app$db_query("SELECT arm FROM practice_arms WHERE scope_key='copy::c';")$arm)
stopifnot(isTRUE(odd$ok), odd_arms[["none"]] == 5L, odd_arms[["one"]] == 9L, odd_arms[["two"]] == 8L)
stopifnot(!isTRUE(app$generate_practice_schedule("bad::1", roster, seed = NA)$ok),
          !isTRUE(app$generate_practice_schedule("bad::2", roster, seed = 1, jobs = "Only one")$ok),
          !isTRUE(app$generate_practice_schedule("bad::3", roster[1:4], seed = 1)$ok))

# ── Practice meetings ────────────────────────────────────────────────────────
for (uid in roster) app$db_exec(
  "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES(?,?,'',0,'ECON 101','01',1,0);",
  list(uid, paste("Person", uid)))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Practice role',4,'',0,1);")
cat_id <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
new_date <- function(label, date, practice) {
  app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled,practice_no_points) VALUES(?,?,'wage_bidding',1,?);",
              list(label, date, practice))
  rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
  for (job in jobs) app$db_exec(
    "INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active) VALUES(?,?,?,1,4,0,1,'start',1);",
    list(rid, job, cat_id))
  rid
}
m1 <- new_date("Practice 1", "2027-01-19", 1L); m2 <- new_date("Practice 2", "2027-01-21", 1L)
paid <- new_date("Paid 1", "2027-02-09", 0L)
stopifnot(app$practice_meeting_no(scope, m1) == 1L, app$practice_meeting_no(scope, m2) == 2L,
          is.na(app$practice_meeting_no(scope, paid)), identical(app$scope_for_round(m1), scope))
# The seed is fixed by the class and its first practice date, not chosen.
auto <- app$practice_auto_seed(scope, "2027-01-19")
stopifnot(nrow(app$practice_dates_for(scope)) == 2L, app$practice_dates_for(scope)$class_date[1] == "2027-01-19",
          is.integer(auto), !is.na(auto), auto > 0L, identical(auto, app$practice_auto_seed(toupper(scope), "2027-01-19")),
          auto != app$practice_auto_seed("econ 101::02", "2027-01-19"), auto != app$practice_auto_seed(scope, "2027-01-21"))
students <- data.frame(user_id = roster, stringsAsFactors = FALSE)
due <- app$practice_turns_due(scope, m1)
stopifnot(nrow(due) == 5L, all(due$state == "due"), all(due$meeting_no == 1L), nrow(app$practice_turns_due(scope, paid)) == 0L)
sheet <- function(rid) { s <- app$class_run_sheet(scope, rid, students); setNames(lapply(s$steps, function(x) x), vapply(s$steps, `[[`, "", "key")) }
stopifnot(!sheet(m1)$practice_assigned$done, !sheet(m1)$practice_scored$done)

# One of today's five is absent: the other four are assigned, the fifth waits.
away <- due$user_id[1]
app$db_exec("INSERT INTO round_absences(round_id,user_id,marked_at) VALUES(?,?,CURRENT_TIMESTAMP);", list(m1, away))
given <- app$assign_practice_turns(scope, m1)
mine <- app$db_query("SELECT id, user_id, job_post_id, assigned_wage, assignment_mode FROM job_assignments WHERE round_id=?;", list(m1))
stopifnot(isTRUE(given$changed), nrow(mine) == 4L, all(mine$assignment_mode == "practice_lottery"), all(mine$assigned_wage == 0),
          !away %in% mine$user_id, grepl("absent", given$message, fixed = TRUE))
# Assigning again adds nothing.
stopifnot(!isTRUE(app$assign_practice_turns(scope, m1)$changed),
          nrow(app$db_query("SELECT 1 FROM job_assignments WHERE round_id=?;", list(m1))) == 4L)
# A practice date takes no draw, and a practice job is never handed on.
round_row <- app$db_query("SELECT * FROM weekly_rounds WHERE id=?;", list(m1))
drew <- app$run_job_draw(round_row, "all", identity)
passed <- app$redraw_job(mine$id[1], reason = "passed"); plain <- app$redraw_job(mine$id[1], reason = "redrawn")
stopifnot(!isTRUE(drew$changed), grepl("practice", drew$message, fixed = TRUE), !isTRUE(passed$changed), !isTRUE(plain$changed),
          nrow(app$db_query("SELECT 1 FROM job_assignments WHERE round_id=?;", list(m1))) == 4L)
# Scoring: two done, one missed, one marked absent (no replacement is drawn).
app$queue_assignment_score(mine$id[1], "complete", "prof"); app$queue_assignment_score(mine$id[2], "tried", "prof")
app$queue_assignment_score(mine$id[3], "missed", "prof")
gone <- app$redraw_job(mine$id[4], reason = "absent")
stopifnot(isTRUE(gone$changed), nrow(app$db_query("SELECT 1 FROM job_assignments WHERE round_id=?;", list(m1))) == 4L,
          app$db_query("SELECT status FROM job_assignments WHERE id=?;", list(mine$id[4]))$status == "absent_redrawn")
state <- app$practice_turns_due(scope, m1)
stopifnot(sum(state$state == "done") == 2L, sum(state$state == "missed") == 1L, sum(state$state == "due") == 2L)
app$commit_pending_scores(identity)
stopifnot(sum(app$practice_turns_due(scope, m1)$state == "done") == 2L,
          nrow(app$db_query("SELECT 1 FROM token_ledger WHERE user_id LIKE '%@test.edu';")) == 0L)   # practice pays nothing now
# The next meeting owes its own five plus the three not performed.
next_due <- app$practice_turns_due(scope, m2)
stopifnot(nrow(next_due) == 8L, all(next_due$state == "due"), sum(next_due$meeting_no == 1L) == 3L)
stopifnot(isTRUE(app$assign_practice_turns(scope, m2)$changed),
          nrow(app$db_query("SELECT 1 FROM job_assignments WHERE round_id=? AND assignment_mode='practice_lottery';", list(m2))) == 8L)

# ── Run sheet and deviations on a paid date ──────────────────────────────────
keys <- function(rid) vapply(app$class_run_sheet(scope, rid, students)$steps, `[[`, "", "key")
stopifnot(identical(keys(paid), c("jobs_drawn", "jobs_shown", "cold_calls", "jobs_scored", "committed")),
          !any(vapply(app$class_run_sheet(scope, paid, students)$steps, `[[`, TRUE, "done")))
stopifnot(!isTRUE(app$log_deviation(scope, paid, "nonsense")$ok), !isTRUE(app$log_deviation(scope, paid, "other", "")$ok),
          isTRUE(app$log_deviation(scope, paid, "volunteer_for_cold_call", "ran short of time", "prof")$ok))
s <- sheet(paid)
stopifnot(!s$cold_calls$done, s$cold_calls$excused, !s$jobs_drawn$excused,
          identical(unlist(app$class_run_sheet(scope, paid, students)$deviations), "Took a volunteer instead of a cold call"))
paid_row <- app$db_query("SELECT * FROM weekly_rounds WHERE id=?;", list(paid))
app$db_exec("UPDATE weekly_rounds SET assignment_mode='random' WHERE id=?;", list(paid)); paid_row$assignment_mode <- "random"
stopifnot(isTRUE(app$run_job_draw(paid_row, "all", identity)$changed), sheet(paid)$jobs_drawn$done, !sheet(paid)$jobs_shown$done)
app$toggle_section_reveal(paid, "01", "all")
app$queue_cold_call(paid, roster[1], "answer", "prof")
stopifnot(sheet(paid)$jobs_shown$done, sheet(paid)$cold_calls$done, !sheet(paid)$jobs_scored$done, !sheet(paid)$committed$done)
for (id in app$db_query("SELECT id FROM job_assignments WHERE round_id=?;", list(paid))$id) app$queue_assignment_score(id, "complete", "prof")
stopifnot(sheet(paid)$jobs_scored$done, !sheet(paid)$committed$done)
app$commit_pending_scores(identity)
stopifnot(all(vapply(app$class_run_sheet(scope, paid, students)$steps, `[[`, TRUE, "done")))
# A class with no schedule gets the ordinary steps and no practice step that could block it.
stopifnot(is.null(app$practice_schedule_info("nobody::00")), nrow(app$practice_turns_due("nobody::00", m1)) == 0L,
          !isTRUE(app$assign_practice_turns("nobody::00", m1)$changed))

cat("critical practice lottery and run sheet: PASS\n")
