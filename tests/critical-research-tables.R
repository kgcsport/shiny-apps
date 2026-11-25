#!/usr/bin/env Rscript
# Regression coverage for the two research tables in Class Job Market, each
# with a single key:
#   student x job x class date  (user_id, round_id, job_post_id)
#   offers and cold calls x student  (event_type, event_id, user_id)
# and for the record of each bid as it stood at the draw.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("research-tables-")
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
              "job_posts", "job_assignments", "job_wage_bids", "job_wage_bid_locks", "class_wage_snapshots",
              "round_absences", "job_offers", "job_offer_audience", "live_score_events", "token_ledger"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
students <- paste0("s", 1:4, "@test.edu")
for (uid in c("admin@test.edu", students, "other@test.edu")) {
  section <- if (uid == "other@test.edu") "02" else "01"
  app$db_exec(
    "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
     VALUES(?,?,'',?,'ECON 101',?,1,0);",
    list(uid, paste("Person", sub("@.*", "", uid)), as.integer(uid == "admin@test.edu"), section))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101',?);", list(uid, section))
}
scope <- "econ 101::01"
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Research role',4,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Research volunteer',4,'',1,0);")
vol_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
class_day <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = app$get_setting("class_tz", "America/New_York")))
new_date <- function(offset) {
  app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled,wage_pricing_rule,allow_multiple_jobs)
               VALUES(?,?,'wage_bidding',1,'random_wage',1);", list(paste("Date", offset), as.character(class_day + offset)))
  rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
  app$db_exec("DELETE FROM scope_active_rounds;")
  app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
  post <- function(name, cat, timing, voluntary = 0L) {
    app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
                 VALUES(?,?,?,2,4,?,?,?,1);", list(rid, name, cat, voluntary, 1L - voluntary, timing))
    as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  }
  list(rid = rid, recap = post("Recap", role_cat, "start"),
       answer = post("Cold call: answer a question", role_cat, "during"),
       vol = post("Volunteer: answer a question", vol_cat, "volunteer", 1L))
}
bid <- function(d, post, uid, wage) app$db_exec(
  "INSERT OR REPLACE INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,?);", list(d$rid, post, uid, wage))
set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}

quiet <- new_date(1)                     # a date where nothing happens
d <- new_date(2)
# s1 bids below any possible wage, s2 and s3 far above, s4 not at all.
bid(d, d$recap, students[1], 0.5); bid(d, d$recap, students[2], 50); bid(d, d$recap, students[3], 60)
bid(d, d$answer, students[2], 2); bid(d, d$answer, students[3], 70)
app$db_exec("INSERT INTO round_absences(round_id,user_id) VALUES(?,?);", list(d$rid, students[4]))

shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(draw_timing_filter = "all", run_draw_btn = 1L)
})
wage <- function(post) app$db_query("SELECT wage FROM class_wage_snapshots WHERE round_id=? AND job_post_id=?;", list(d$rid, post))$wage[1]
locks <- app$db_query("SELECT user_id, job_post_id, min_wage FROM job_wage_bid_locks WHERE round_id=? ORDER BY job_post_id, user_id;", list(d$rid))
stopifnot(nrow(locks) == 5L, locks$min_wage[locks$user_id == students[2] & locks$job_post_id == d$recap] == 50)
# After the draw, s2 lowers their Recap bid. The record of the draw does not move.
bid(d, d$recap, students[2], 1)
shiny::testServer(app$server, { set_admin(rv); session$setInputs(draw_timing_filter = "all", run_draw_btn = 2L) })
stopifnot(app$db_query("SELECT min_wage FROM job_wage_bid_locks WHERE round_id=? AND job_post_id=? AND user_id=?;",
                       list(d$rid, d$recap, students[2]))$min_wage == 50)

# In class: s1's job is scored, the open slot is offered and s3 takes it, s2 is
# cold-called and answers, s3 is cold-called and passes, s1 volunteers twice.
shiny::testServer(app$server, {
  set_admin(rv)
  aid <- db_query("SELECT id FROM job_assignments WHERE round_id=? AND user_id=?;", list(d$rid, students[1]))$id[1]
  db_exec("INSERT INTO live_score_events(round_id,user_id,job_assignment_id,event_kind,outcome,tokens,logged_by)
           VALUES(?,?,?,'assignment','complete',?,'admin@test.edu');", list(d$rid, students[1], aid, wage(d$recap)))
  session$setInputs(offer_post_id = d$recap, offer_taker_uid = students[3], offer_also_uids = students[2], offer_record_btn = 1L)
  rv$cold_call_draw <- list(user_id = students[2], display_name = "Person s2", section = "01")
  session$setInputs(record_cold_call_answer_btn = 1L)
  rv$cold_call_draw <- list(user_id = students[3], display_name = "Person s3", section = "01")
  session$setInputs(cold_call_pass_btn = 1L)
  session$setInputs(part_event_type = d$vol)
  session$setInputs(part_card_click = list(user_id = students[1], outcome = "succeed", nonce = 1L))
  session$setInputs(part_card_click = list(user_id = students[1], outcome = "miss", nonce = 2L))
  session$setInputs(commit_live_scores_btn = 1L)
})

read_tables <- function(mask) {
  result <- NULL
  shiny::testServer(app$server, {
    set_admin(rv)
    if (mask) session$setInputs(export_mask_ids = TRUE)
    read <- function(path) list(rows = utils::read.csv(path, stringsAsFactors = FALSE),
                                text = tolower(paste(readLines(path, warn = FALSE), collapse = "\n")))
    result <<- list(panel = read(output$dl_research_panel), events = read(output$dl_research_events))
  })
  result
}
tables <- read_tables(FALSE)
panel <- tables$panel$rows; events <- tables$events$rows
cell <- function(uid, post) panel[panel$student == paste("Person", sub("@.*", "", uid)) & panel$job_post_id == post, , drop = FALSE]

# Student x job x class date: complete, one row per key, nobody from another section.
stopifnot(nrow(panel) == 4L * 6L, !anyDuplicated(panel[c("student", "round_id", "job_post_id")]),
          !any(grepl("other", panel$student)), !"export_uid" %in% names(panel),
          all(panel$pricing_rule == "random_wage"))
# A date where nothing happened still has its rows, empty.
idle <- panel[panel$round_id == quiet$rid, ]
stopifnot(nrow(idle) == 12L, all(is.na(idle$bid_at_draw)), all(idle$assigned == 0), all(is.na(idle$frozen_wage)))
recap_wage <- wage(d$recap); answer_wage <- wage(d$answer)
s1 <- cell(students[1], d$recap); s2 <- cell(students[2], d$recap); s3 <- cell(students[3], d$recap); s4 <- cell(students[4], d$recap)
stopifnot(
  # the bid the draw used, and the later edit, side by side
  s2$bid_at_draw == 50, s2$bid_latest == 1, s1$bid_at_draw == 0.5, is.na(s4$bid_at_draw), is.na(s4$bid_latest),
  s1$frozen_wage == recap_wage, s1$frozen_wage_source == "random_draw", s1$posted_wage == 4,
  s1$in_lottery == 1, s2$in_lottery == 0, s4$in_lottery == 0,
  # the lottery winner, scored and paid
  s1$assigned == 1, s1$assigned_by == "wage_bidding", s1$assigned_wage == recap_wage,
  s1$outcome == "complete", s1$tokens_awarded == recap_wage,
  # the student who took the offer below their own bid
  s3$assigned == 1, s3$assigned_by == "volunteer_fill", s3$assigned_wage == recap_wage,
  s3$offers_present == 1, s3$offers_took == 1,
  s2$assigned == 0, s2$offers_volunteered == 1, s1$offers_passed == 1,
  # absence is per student and date, shown on every job row for that date
  s4$marked_absent == 1, s4$offers_present == 0, s1$marked_absent == 0,
  all(panel$marked_absent[panel$student == "Person s4" & panel$round_id == d$rid] == 1),
  all(panel$marked_absent[panel$round_id == quiet$rid] == 0))
vol <- cell(students[1], d$vol)
stopifnot(vol$voluntary_job == 1, vol$voluntary_n == 2, vol$voluntary_succeeded == 1, vol$voluntary_paid == 1,
          cell(students[2], d$vol)$voluntary_n == 0, cell(students[1], d$recap)$voluntary_n == 0)

# Offers and cold calls x student: one row per key.
stopifnot(!anyDuplicated(events[c("event_type", "event_id", "student")]),
          sum(events$event_type == "job_offer") == 3L, sum(events$event_type == "cold_call") == 2L)
offer <- events[events$event_type == "job_offer", ]
stopifnot(all(offer$offered_wage == recap_wage), all(offer$job == "Recap"), all(offer$job_inferred == 0),
          offer$response[offer$student == "Person s3"] == "took", offer$bid_at_event[offer$student == "Person s3"] == 60,
          offer$response[offer$student == "Person s2"] == "volunteered", offer$bid_at_event[offer$student == "Person s2"] == 1,
          !"Person s4" %in% offer$student)
answered <- events[events$event_type == "cold_call" & events$student == "Person s2", ]
passed <- events[events$event_type == "cold_call" & events$student == "Person s3", ]
stopifnot(answered$response == "answered", answered$job == "Cold call: answer a question",
          answered$offered_wage == answer_wage, answered$bid_at_event == 2, answered$job_inferred == 0,
          passed$response == "passed", passed$job_inferred == 1, passed$offered_wage == answer_wage,
          passed$bid_at_event == 70)

# Masked: the same tables with a hashed ID and no names or addresses.
masked <- read_tables(TRUE)
for (name in c("panel", "events")) {
  rows <- masked[[name]]$rows
  stopifnot(nrow(rows) == nrow(tables[[name]]$rows), identical(names(rows)[1], "masked_id"),
            !any(c("student", "export_uid", "user_id") %in% names(rows)),
            !grepl("person s", masked[[name]]$text, fixed = TRUE), !grepl("@test.edu", masked[[name]]$text, fixed = TRUE))
}
stopifnot(length(unique(masked$panel$rows$masked_id)) == 4L,
          !anyDuplicated(masked$panel$rows[c("masked_id", "round_id", "job_post_id")]),
          !anyDuplicated(masked$events$rows[c("event_type", "event_id", "masked_id")]))

# Practice columns: blank while the class has no practice date. Once a date is
# marked as practice, a student counts as having practised a job (matched by
# name, on every date) if they were drawn for it, or answered that kind of cold
# call, on a practice date.
stopifnot(all(c("practice_assigned", "practice_jobs_n") %in% names(panel)), all(is.na(panel$practice_assigned)),
          all(is.na(panel$practice_jobs_n)))
busy <- app$db_query("SELECT round_id, COUNT(*) n FROM job_assignments GROUP BY round_id ORDER BY n DESC LIMIT 1;")$round_id[1]
app$db_exec("UPDATE weekly_rounds SET practice_no_points=1 WHERE id=?;", list(busy))
drawn <- app$db_query("SELECT DISTINCT u.display_name AS student, jp.job_name AS job FROM job_assignments ja
                       JOIN job_posts jp ON jp.id=ja.job_post_id JOIN users u ON u.user_id=ja.user_id WHERE ja.round_id=?;", list(busy))
called <- app$db_query("SELECT DISTINCT u.display_name AS student FROM live_score_events lse JOIN users u ON u.user_id=lse.user_id
                        WHERE lse.round_id=? AND lse.event_kind='cold_call' AND lse.outcome='answer';", list(busy))
expected <- unique(c(paste(drawn$student, drawn$job), if (nrow(called)) paste(called$student, "Cold call: answer a question")))
with_practice <- read_tables(FALSE)$panel$rows
got <- unique(paste(with_practice$student, with_practice$job)[with_practice$practice_assigned == 1])
per_student <- tapply(with_practice$practice_jobs_n, with_practice$student, unique)
stopifnot(nrow(drawn) > 0L, !anyNA(with_practice$practice_assigned), setequal(got, expected),
          all(lengths(per_student) == 1L),
          all(vapply(names(per_student), function(st) per_student[[st]] == sum(startsWith(expected, paste0(st, " "))), logical(1))),
          # It is the job, not the date: the flag is the same on every date for that student and job.
          all(tapply(with_practice$practice_assigned, paste(with_practice$student, with_practice$job), function(v) length(unique(v))) == 1L))
app$db_exec("UPDATE weekly_rounds SET practice_no_points=0 WHERE id=?;", list(busy))

cat("critical research tables: PASS\n")
