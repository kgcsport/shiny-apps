#!/usr/bin/env Rscript
# Regression coverage for the random-wage pricing rule in Class Job Market:
# the wage is drawn independently of bids, only students who bid at or below it
# can be chosen, every winner is paid the drawn wage, slots may stay empty, and
# the existing pay-as-bid draw is unchanged.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("random-wage-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)

app <- new.env(parent = globalenv())
source(app_path, local = app)

# Pure rule: grid, draw, and winners.
stopifnot(identical(app$normalize_wage_pricing_rule("random_wage"), "random_wage"),
          identical(app$normalize_wage_pricing_rule("pay_as_bid"), "pay_as_bid"),
          identical(app$normalize_wage_pricing_rule("uniform"), "uniform_second_price"),
          identical(app$normalize_wage_pricing_rule(NULL), "pay_as_bid"),
          isTRUE(all.equal(app$random_wage_grid(4), seq(0.5, 4, by = 0.5))),
          isTRUE(all.equal(app$random_wage_grid(2.7), seq(0.5, 2.5, by = 0.5))),
          identical(app$random_wage_grid(0), 0.5), identical(app$random_wage_grid(NA), 0.5))
set.seed(20261123)
draws <- vapply(1:400, function(i) app$draw_random_wage(4), numeric(1))
stopifnot(all(draws %in% seq(0.5, 4, by = 0.5)), length(unique(draws)) == 8L)
bids <- data.frame(user_id = c("a", "b", "c", "d"), min_wage = c(1, 2, 3, 5000))
stopifnot(identical(app$random_wage_winners(bids, 0.5, 2), character(0)),
          identical(app$random_wage_winners(bids, 1, 2), "a"),
          setequal(app$random_wage_winners(bids, 2, 2), c("a", "b")),
          identical(app$random_wage_winners(bids[0, ], 2, 2), character(0)))
for (i in 1:200) {
  won <- app$random_wage_winners(bids, 3, 2)
  stopifnot(length(won) == 2L, all(won %in% c("a", "b", "c")), !anyDuplicated(won))
}
# Every eligible bidder has the same chance, whatever they bid below the wage.
picks <- table(unlist(lapply(1:3000, function(i) app$random_wage_winners(bids, 3, 1))))
stopifnot(all(abs(picks / 3000 - 1 / 3) < 0.04))

# Live draw through the instructor control.
app$set_setting("class_days", "")
app$set_setting("bid_lock_enabled", "0")
for (tbl in c("scope_active_rounds", "scope_rounds", "section_scope_memberships", "weekly_rounds",
              "job_posts", "job_assignments", "job_wage_bids", "class_wage_snapshots",
              "live_score_events", "token_ledger"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
students <- paste0("s", 1:6, "@test.edu")
for (uid in c("admin@test.edu", students)) {
  app$db_exec(
    "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
     VALUES(?,?,'',?,'ECON 101','01',1,0);", list(uid, uid, as.integer(uid == "admin@test.edu")))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');", list(uid))
}
scope <- "econ 101::01"
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Random wage role',4,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
class_day <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = app$get_setting("class_tz", "America/New_York")))
# Bids: three low, one opt-out, one coordinated-high pair member, one non-bidder.
bid_values <- c(1, 2, 3, 5000, 5000)
new_date <- function(offset, rule, slots = 2L) {
  app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled,wage_pricing_rule,allow_multiple_jobs)
               VALUES(?,?,'wage_bidding',1,?,1);", list(paste("Date", offset), as.character(class_day + offset), rule))
  rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
  app$db_exec("DELETE FROM scope_active_rounds;")
  app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
  app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
               VALUES(?,'Recap',?,?,4,0,1,'start',1);", list(rid, role_cat, slots))
  post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  for (i in seq_along(bid_values))
    app$db_exec("INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,?);",
                list(rid, post, students[i], bid_values[i]))
  list(rid = rid, post = post)
}
set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}
run_draw <- function(click = 1L) shiny::testServer(app$server, {
  set_admin(rv); session$setInputs(draw_timing_filter = "all", run_draw_btn = click)
})
assignments <- function(rid) app$db_query(
  "SELECT user_id, assigned_wage FROM job_assignments WHERE round_id=? ORDER BY user_id;", list(rid))
snapshot <- function(d) app$db_query(
  "SELECT wage, source FROM class_wage_snapshots WHERE round_id=? AND job_post_id=?;", list(d$rid, d$post))

seen_wages <- numeric(0); unfilled <- 0L
for (offset in 1:14) {
  d <- new_date(offset, "random_wage")
  run_draw()
  snap <- snapshot(d); got <- assignments(d$rid)
  stopifnot(nrow(snap) == 1L, identical(snap$source[1], "random_draw"),
            snap$wage[1] %in% seq(0.5, 4, by = 0.5))
  wage <- snap$wage[1]; seen_wages <- c(seen_wages, wage)
  eligible <- students[seq_along(bid_values)][bid_values <= wage]
  stopifnot(nrow(got) == min(2L, length(eligible)), all(got$user_id %in% eligible),
            all(got$assigned_wage == wage),
            !any(c(students[4], students[5], students[6]) %in% got$user_id),
            identical(app$db_query("SELECT bidding_enabled v FROM weekly_rounds WHERE id=?;", list(d$rid))$v[1], 0L))
  if (!nrow(got)) unfilled <- unfilled + 1L
  # Re-running the saved draw keeps the wage: it cannot be fished.
  run_draw(2L)
  again <- assignments(d$rid)
  stopifnot(snapshot(d)$wage[1] == wage, all(again$assigned_wage == wage), all(again$user_id %in% eligible))
}
# The drawn wage varies and does not track the bids on file.
stopifnot(length(unique(seen_wages)) >= 3L, max(seen_wages) <= 4)

# Everyone bidding high gains nothing: nobody is eligible and nothing is paid.
d <- new_date(20, "random_wage")
app$db_exec("UPDATE job_wage_bids SET min_wage=5000 WHERE round_id=?;", list(d$rid))
run_draw()
stopifnot(nrow(assignments(d$rid)) == 0L, snapshot(d)$wage[1] <= 4)

# Absent redraw on a random-wage date: the replacement must be eligible and is
# paid the same drawn wage; with nobody eligible the slot stays empty.
repeat {
  d <- new_date(21 + nrow(app$db_query("SELECT id FROM weekly_rounds;")), "random_wage", slots = 1L)
  run_draw()
  if (nrow(assignments(d$rid)) == 1L && snapshot(d)$wage[1] >= 2) break
}
wage <- snapshot(d)$wage[1]
eligible <- students[seq_along(bid_values)][bid_values <= wage]
first <- app$db_query("SELECT id,user_id FROM job_assignments WHERE round_id=?;", list(d$rid))
shiny::testServer(app$server, { set_admin(rv); session$setInputs(redraw_absent_btn = first$id[1]) })
after <- app$db_query("SELECT user_id,status,assigned_wage FROM job_assignments WHERE round_id=? ORDER BY id;", list(d$rid))
stopifnot(nrow(after) == 2L, identical(after$status[1], "absent_redrawn"), after$assigned_wage[1] == -wage,
          after$user_id[2] %in% setdiff(eligible, first$user_id[1]), after$assigned_wage[2] == wage)
# Make everyone else ineligible, then mark the replacement absent too.
app$db_exec("UPDATE job_wage_bids SET min_wage=5000 WHERE round_id=?;", list(d$rid))
second <- app$db_query("SELECT id FROM job_assignments WHERE round_id=? AND status='assigned';", list(d$rid))$id[1]
shiny::testServer(app$server, { set_admin(rv); session$setInputs(redraw_absent_btn = second) })
final <- app$db_query("SELECT status FROM job_assignments WHERE round_id=?;", list(d$rid))
stopifnot(nrow(final) == 2L, all(final$status == "absent_redrawn"),
          nrow(app$db_query("SELECT user_id FROM round_absences WHERE round_id=?;", list(d$rid))) == 2L)

# The existing pay-as-bid draw is unchanged: lowest bidders win and receive
# their own bid, and an empty slot is still filled from the remaining students.
d <- new_date(30, "pay_as_bid")
run_draw()
got <- assignments(d$rid)
stopifnot(identical(got$user_id, students[1:2]), identical(got$assigned_wage, c(1, 2)))
first <- app$db_query("SELECT id FROM job_assignments WHERE round_id=? AND user_id=?;", list(d$rid, students[1]))$id[1]
shiny::testServer(app$server, { set_admin(rv); session$setInputs(redraw_absent_btn = first) })
stopifnot(nrow(app$db_query("SELECT id FROM job_assignments WHERE round_id=? AND status='assigned';", list(d$rid))) == 2L)

# Pass on an assigned job: the job is redrawn and the original kept unpaid as
# a record, but unlike an absence the student stays in the pool for cold calls
# and other jobs, and is never redrawn into the job they passed on.
absences <- function(rid) app$db_query("SELECT user_id FROM round_absences WHERE round_id=?;", list(rid))$user_id
stopifnot(identical(absences(d$rid), students[1]))            # the absence above was recorded
d <- new_date(31, "pay_as_bid")
run_draw()
passer <- app$db_query("SELECT id,user_id,assigned_wage FROM job_assignments WHERE round_id=? AND user_id=?;", list(d$rid, students[2]))
shiny::testServer(app$server, { set_admin(rv); session$setInputs(redraw_pass_btn = passer$id[1]) })
rows <- app$db_query("SELECT user_id,status,assigned_wage FROM job_assignments WHERE round_id=? ORDER BY id;", list(d$rid))
stopifnot(nrow(rows) == 3L,
          identical(rows$status[rows$user_id == students[2]], "passed_redrawn"),
          rows$assigned_wage[rows$user_id == students[2]] == -passer$assigned_wage[1],
          sum(rows$status == "assigned") == 2L, !students[2] %in% rows$user_id[rows$status == "assigned"],
          length(absences(d$rid)) == 0L,
          identical(app$assignment_history_state("", "passed_redrawn")$label, "Passed / redrawn"),
          identical(app$assignment_history_state("", "absent_redrawn")$label, "Absent / redrawn"))
# A second click on the same pass does nothing more.
shiny::testServer(app$server, { set_admin(rv); session$setInputs(redraw_pass_btn = passer$id[1]) })
stopifnot(nrow(app$db_query("SELECT id FROM job_assignments WHERE round_id=?;", list(d$rid))) == 3L)
# The passer is still drawn for cold calls; an absent student is not.
shiny::testServer(app$server, {
  set_admin(rv)
  seen <- character(0)
  for (i in 1:80) { draw_cold_call(); seen <- c(seen, rv$cold_call_draw$user_id) }
  stopifnot(students[2] %in% seen)
})
# Random-wage date with nobody else eligible: a pass leaves the slot empty and
# records no absence.
repeat {
  d <- new_date(60 + nrow(app$db_query("SELECT id FROM weekly_rounds;")), "random_wage", slots = 1L)
  run_draw()
  if (nrow(assignments(d$rid)) == 1L) break
}
app$db_exec("UPDATE job_wage_bids SET min_wage=5000 WHERE round_id=?;", list(d$rid))
only <- app$db_query("SELECT id FROM job_assignments WHERE round_id=?;", list(d$rid))$id[1]
shiny::testServer(app$server, { set_admin(rv); session$setInputs(redraw_pass_btn = only) })
left <- app$db_query("SELECT status FROM job_assignments WHERE round_id=?;", list(d$rid))
stopifnot(nrow(left) == 1L, identical(left$status[1], "passed_redrawn"), length(absences(d$rid)) == 0L)

cat("critical random wage draw: PASS\n")
