#!/usr/bin/env Rscript
# Regression coverage for practice (no-points) class dates and end-of-term
# practice pay in Class Job Market. Exercises the real instructor controls and
# the Live Score Audit commit path against a scratch SQLite database.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("practice-dates-")
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
              "job_posts", "job_assignments", "live_score_events", "token_ledger"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))

people <- list(c("admin@test.edu", "Instructor", 1L), c("s1@test.edu", "Student 1", 0L),
               c("s2@test.edu", "Student 2", 0L), c("s3@test.edu", "Student 3", 0L))
for (person in people) {
  app$db_exec(
    "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
     VALUES(?,?,'',?,'ECON 101','01',1,0);", list(person[1], person[2], as.integer(person[3])))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');",
              list(person[1]))
}
scope <- "econ 101::01"
class_day <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = app$get_setting("class_tz", "America/New_York")))
new_round <- function(label, day) {
  app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES(?,?,'wage_bidding',1);",
              list(label, as.character(day)))
  rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
  rid
}
practice_round <- new_round("Practice class", class_day)
market_round <- new_round("Market class", class_day + 7)
app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);",
            list(scope, practice_round))
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))

app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Practice test role',5,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Practice test volunteer',5,'',1,0);")
vol_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
new_post <- function(rid, name, cat, voluntary = 0L) {
  app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
               VALUES(?,?,?,5,5,?,?,'start',1);", list(rid, name, cat, voluntary, 1L - voluntary))
  as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
}
p_recap <- new_post(practice_round, "Recap", role_cat)
p_notes <- new_post(practice_round, "Note taker", role_cat)
p_scout <- new_post(practice_round, "Example scout", role_cat)
p_vol   <- new_post(practice_round, "Volunteer answer", vol_cat, 1L)
m_recap <- new_post(market_round, "Recap", role_cat)
m_notes <- new_post(market_round, "Note taker", role_cat)

set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}
save_controls <- function(session, practice, click) session$setInputs(
  edit_round_mode = "wage_bidding", edit_round_bidding_enabled = "1", edit_round_tix = 10,
  edit_round_allow_multiple = TRUE, edit_round_practice = practice, update_round_btn = click)
assign_job <- function(rid, uid, post, wage) {
  app$db_exec("INSERT INTO job_assignments(round_id,user_id,job_post_id,assigned_wage,assignment_mode,status)
               VALUES(?,?,?,?,'wage_bidding','assigned');", list(rid, uid, post, wage))
  as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
}
score <- function(rid, uid, aid, outcome, tokens) app$db_exec(
  "INSERT INTO live_score_events(round_id,user_id,job_assignment_id,event_kind,outcome,tokens,logged_by)
   VALUES(?,?,?,'assignment',?,?,'admin@test.edu');", list(rid, uid, aid, outcome, tokens))
commit_all <- function(click) shiny::testServer(app$server, {
  set_admin(rv); session$setInputs(commit_live_scores_btn = click)
})
ledger <- function(where = "1=1") app$db_query(paste(
  "SELECT user_id,source_type,source_id,amount,earning FROM token_ledger WHERE", where, "ORDER BY id;"))

# 1. Default: a class date is not a practice date and pays normally.
stopifnot(app$db_query("SELECT COALESCE(practice_no_points,0) v FROM weekly_rounds WHERE id=?;", list(practice_round))$v[1] == 0L)

# 2. Switch the first date to practice through the real per-date control. An
#    assignment that already existed loses its wage.
early <- assign_job(practice_round, "s1@test.edu", p_recap, 5)
shiny::testServer(app$server, {
  set_admin(rv)
  save_controls(session, TRUE, 1L)
  stopifnot(app$round_is_practice(practice_round), !app$round_is_practice(market_round))
})
mode_kept <- app$db_query("SELECT assignment_mode FROM weekly_rounds WHERE id=?;", list(practice_round))$assignment_mode[1]
stopifnot(identical(mode_kept, "wage_bidding"),
          app$db_query("SELECT assigned_wage w FROM job_assignments WHERE id=?;", list(early))$w[1] == 0)

# 3. Every later write on a practice date is stored at zero, whichever path
#    made it: a new assignment, a re-priced assignment, and score events.
a_notes <- assign_job(practice_round, "s2@test.edu", p_notes, 5)
a_scout <- assign_job(practice_round, "s3@test.edu", p_scout, 5)
a_miss  <- assign_job(practice_round, "s3@test.edu", p_recap, 5)
app$db_exec("UPDATE job_assignments SET assigned_wage=9 WHERE id=?;", list(a_notes))
score(practice_round, "s1@test.edu", early, "complete", 5)
score(practice_round, "s2@test.edu", a_notes, "tried", 1)
score(practice_round, "s3@test.edu", a_scout, "complete", 5)
score(practice_round, "s3@test.edu", a_miss, "missed", 0)
stopifnot(all(app$db_query("SELECT assigned_wage w FROM job_assignments WHERE round_id=?;", list(practice_round))$w == 0),
          all(app$db_query("SELECT tokens t FROM live_score_events WHERE round_id=?;", list(practice_round))$t == 0))
# Voluntary and cold-call contributions through the live controls.
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(part_event_type = p_vol)
  session$setInputs(part_card_click = list(user_id = "s2@test.edu", outcome = "succeed", nonce = 1L))
  session$setInputs(part_card_click = list(user_id = "s3@test.edu", outcome = "try", nonce = 2L))
  rv$cold_call_draw <- list(user_id = "s1@test.edu", display_name = "Student 1", section = "01")
  session$setInputs(record_cold_call_answer_btn = 1L)
})
commit_all(1L)
practice_events <- app$db_query(
  "SELECT event_kind,outcome,tokens,committed_at FROM live_score_events WHERE round_id=?;", list(practice_round))
practice_jobs <- app$db_query("SELECT outcome,tokens_awarded FROM job_assignments WHERE round_id=? ORDER BY id;", list(practice_round))
stopifnot(nrow(practice_events) == 7L, all(!is.na(practice_events$committed_at)), all(practice_events$tokens == 0),
          identical(practice_jobs$outcome, c("complete", "tried", "complete", "missed")),
          all(practice_jobs$tokens_awarded == 0),
          nrow(ledger()) == 0L)
# The zero-token repair must not "fix" practice work into paid work.
shiny::testServer(app$server, { set_admin(rv); session$setInputs(repair_zeroed_jobs_btn = 1L) })
stopifnot(nrow(ledger()) == 0L)

# 4. An ordinary date still pays in full: Recap at 4 and 6, Note taker at 2.
shiny::testServer(app$server, { set_admin(rv); session$setInputs(active_round_select = market_round) })
m1 <- assign_job(market_round, "s1@test.edu", m_recap, 4)
m2 <- assign_job(market_round, "s2@test.edu", m_recap, 6)
m3 <- assign_job(market_round, "s3@test.edu", m_notes, 2)
score(market_round, "s1@test.edu", m1, "complete", 4)
score(market_round, "s2@test.edu", m2, "complete", 6)
score(market_round, "s3@test.edu", m3, "tried", 1)
commit_all(2L)
market_pay <- ledger("source_type='job'")
stopifnot(identical(market_pay$amount, c(4, 6, 1)), all(market_pay$earning == 1L))
# A date that has paid someone cannot be turned into a practice date.
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(active_round_select = market_round)
  save_controls(session, TRUE, 1L)
  stopifnot(!app$round_is_practice(market_round))
})
stopifnot(app$db_query("SELECT assigned_wage w FROM job_assignments WHERE id=?;", list(m1))$w[1] == 4)

# 5. End-of-term practice pay. Each job's own average: Recap = mean(4, 6) = 5,
#    the cold call and volunteer answer have no later wage so they use the
#    all-job average mean(4, 6, 2) = 4, a try pays 1 point by default, or half
#    when the class sets that (the volunteer answer uses 4, so 2), a miss pays nothing.
shiny::testServer(app$server, {
  set_admin(rv)
  own <- practice_pay_rows("job")
  pay <- function(rows, uid, label) rows$amount[rows$user_id == uid & rows$job_label == label]
  stopifnot(nrow(own) == 6L, !any(own$already_paid),
            pay(own, "s1@test.edu", "Recap") == 5,
            pay(own, "s2@test.edu", "Note taker") == 1,
            pay(own, "s3@test.edu", "Example scout") == 4,
            pay(own, "s2@test.edu", "Volunteer answer") == 4,
            pay(own, "s3@test.edu", "Volunteer answer") == 1,
            { app$set_setting("try_pay_points", "0")
              half <- practice_pay_rows("job")
              app$set_setting("try_pay_points", "1")
              pay(half, "s3@test.edu", "Volunteer answer") == 2 && pay(half, "s2@test.edu", "Note taker") == 1 },
            pay(own, "s1@test.edu", "Cold call: answer") == 4,
            !any(own$outcome == "missed"))
  pooled <- practice_pay_rows("all")
  stopifnot(pay(pooled, "s1@test.edu", "Recap") == 4, pay(pooled, "s2@test.edu", "Note taker") == 1)
  # Nothing is credited by a preview.
  session$setInputs(practice_pay_rule = "job", practice_pay_preview_btn = 1L)
  stopifnot(grepl("6 items to pay, 19 tokens in total", output$practice_pay_preview$html, fixed = TRUE),
            nrow(db_query("SELECT id FROM token_ledger WHERE source_type LIKE 'practice_pay%';")) == 0L)
  session$setInputs(practice_pay_commit_btn = 1L)
  stopifnot(all(practice_pay_rows("job")$already_paid))
  # A repeat click, and a switch of rule, cannot pay the same work again.
  session$setInputs(practice_pay_commit_btn = 2L)
  session$setInputs(practice_pay_rule = "all", practice_pay_preview_btn = 2L)
  session$setInputs(practice_pay_commit_btn = 3L)
})
practice_pay <- ledger("source_type LIKE 'practice_pay%'")
stopifnot(nrow(practice_pay) == 6L, sum(practice_pay$amount) == 19, all(practice_pay$earning == 1L),
          !any(duplicated(practice_pay[, c("source_type", "source_id")])),
          sum(practice_pay$amount[practice_pay$user_id == "s1@test.edu"]) == 9)

# 5b. Other-section rule, in a second course so it starts unpaid. Section 01
#     practised Recap; its pay comes only from section 02's wages (Recap 10,
#     Note taker 2), never from its own (Recap 3), so no bid in 01 can move it.
other <- list()
for (sec in c("01", "02")) {
  key <- paste0("econ 202::", sec)
  for (kind in c("practice", "market")) {
    app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled,practice_no_points) VALUES(?,?,'wage_bidding',1,?);",
                list(paste(key, kind), as.character(class_day + 30), as.integer(kind == "practice")))
    rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
    app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(key, rid))
    other[[paste(sec, kind)]] <- rid
  }
}
app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES('econ 202::01','ECON 202','01',?);",
            list(other[["01 practice"]]))
for (row in list(c("t1@test.edu", "Transfer 1", "01"), c("t2@test.edu", "Transfer 2", "01"),
                 c("t3@test.edu", "Transfer 3", "02"))) {
  app$db_exec(
    "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
     VALUES(?,?,'',0,'ECON 202',?,1,0);", list(row[1], row[2], row[3]))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 202',?);", list(row[1], row[3]))
}
seed <- function(rid, uid, job, wage, outcome) {
  aid <- assign_job(rid, uid, new_post(rid, job, role_cat), wage)
  app$db_exec("UPDATE job_assignments SET outcome=? WHERE id=?;", list(outcome, aid))
  aid
}
seed(other[["01 practice"]], "t1@test.edu", "Recap", 5, "complete")
seed(other[["01 practice"]], "t2@test.edu", "Example scout", 5, "complete")
seed(other[["01 market"]], "t1@test.edu", "Recap", 3, "complete")
stopifnot(all(app$db_query("SELECT assigned_wage w FROM job_assignments WHERE round_id=?;", list(other[["01 practice"]]))$w == 0))
set_admin_202 <- function(rv) { set_admin(rv); rv$course <- "ECON 202"; rv$active_course <- "ECON 202" }
shiny::testServer(app$server, {
  set_admin_202(rv)
  stopifnot(identical(current_scope_key(), "econ 202::01"))
  # No other-section wages yet: completed work cannot be priced or paid.
  none <- practice_pay_rows("other")
  stopifnot(nrow(none) == 2L, all(is.na(none$amount)))
  session$setInputs(practice_pay_rule = "other", practice_pay_preview_btn = 1L)
  stopifnot(grepl("cannot be priced", output$practice_pay_preview$html, fixed = TRUE))
  session$setInputs(practice_pay_commit_btn = 1L)
})
stopifnot(nrow(ledger("source_type LIKE 'practice_pay%'")) == 6L)
seed(other[["02 market"]], "t3@test.edu", "Recap", 10, "complete")
seed(other[["02 market"]], "t3@test.edu", "Note taker", 2, "complete")
shiny::testServer(app$server, {
  set_admin_202(rv)
  rows <- practice_pay_rows("other")
  stopifnot(rows$amount[rows$job_label == "Recap"] == 10,          # section 02's Recap wage
            rows$amount[rows$job_label == "Example scout"] == 6,   # section 02's all-job average
            practice_pay_rows("job")$amount[rows$job_label == "Recap"] == 3)
  session$setInputs(practice_pay_rule = "other", practice_pay_preview_btn = 1L)
  session$setInputs(practice_pay_commit_btn = 1L)
})
other_pay <- ledger("source_type LIKE 'practice_pay%'")
stopifnot(nrow(other_pay) == 8L, sum(utils::tail(other_pay$amount, 2)) == 16)

# 6. Switching the flag off does not retroactively pay or re-price the date.
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(active_round_select = practice_round)
  save_controls(session, FALSE, 1L)
  stopifnot(!app$round_is_practice(practice_round))
})
stopifnot(all(app$db_query("SELECT assigned_wage w FROM job_assignments WHERE round_id=?;", list(practice_round))$w == 0),
          nrow(ledger("source_type='job'")) == 3L)

cat("critical practice dates: PASS\n")
