#!/usr/bin/env Rscript
# Two opt-in rules for spring:
#   1. a floor on the wage a random-wage draw can produce;
#   2. paid cold calls drawn only from students who bid on a cold-call job
#      (at or below the drawn wage on random-wage dates).
# Both are off by default, so a class that sets neither is unchanged.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("coldcall-floor-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)
app <- new.env(parent = globalenv())
source(app_path, local = app)

# ── Wage floor ───────────────────────────────────────────────────────────────
stopifnot(isTRUE(all.equal(app$random_wage_grid(4), seq(0.5, 4, by = 0.5))),          # default: unchanged
          isTRUE(all.equal(app$random_wage_grid(4, floor_wage = 0), seq(0.5, 4, by = 0.5))),
          isTRUE(all.equal(app$random_wage_grid(4, floor_wage = 2), seq(2, 4, by = 0.5))),
          isTRUE(all.equal(app$random_wage_grid(4, floor_wage = 1.75), seq(2, 4, by = 0.5))),
          identical(app$random_wage_grid(4, floor_wage = 9), 4),                        # floor above the top: the top wage
          app$random_wage_floor_value() == 0)
set.seed(1)
free <- vapply(1:300, function(i) app$draw_random_wage(4), numeric(1))
stopifnot(min(free) == 0.5, max(free) == 4)
app$set_setting("random_wage_floor", "2")
floored <- vapply(1:300, function(i) app$draw_random_wage(4), numeric(1))
stopifnot(app$random_wage_floor_value() == 2, min(floored) == 2, max(floored) == 4, all(floored %% 0.5 == 0))
# A section can set its own floor without changing another's.
section_02 <- function(level) list(course = "econ 101", sections = "02", scope = "econ 101::02", level = level)
app$with_setting_context(section_02("section"), app$set_setting("random_wage_floor", "3"))
in_02 <- app$with_setting_context(section_02("auto"), app$random_wage_floor_value())
stopifnot(in_02 == 3, app$random_wage_floor_value() == 2)
app$set_setting("random_wage_floor", "0")

# ── Cold calls from bidders only ─────────────────────────────────────────────
for (tbl in c("weekly_rounds", "job_posts", "job_wage_bids", "job_wage_bid_locks", "class_wage_snapshots"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,wage_pricing_rule,bidding_enabled) VALUES('Random','2027-02-01','wage_bidding','random_wage',0);")
rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Cold Call',4,'',0,1);")
cc_cat <- as.integer(app$db_query("SELECT id FROM job_categories WHERE name='Cold Call' ORDER BY id DESC LIMIT 1;")$id[1])
post <- function(name, timing = "during") {
  app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active) VALUES(?,?,?,1,4,0,1,?,1);",
              list(rid, name, cc_cat, timing))
  as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
}
# The board job is created first, as in the fall class. Its name also contains
# the word "answer", which once made every answer resolve to the board job.
board <- post("Cold call: graph/answer on board"); answer <- post("Cold call: answer a question")
stopifnot(identical(as.character(app$cold_call_kind_of(c("Cold call: graph/answer on board", "Cold call: answer a question",
                                                          "Cold call: ask a question"))), c("board", "answer", "ask")),
          app$cold_call_post_for(rid, "answer")$id == answer, app$cold_call_post_for(rid, "board")$id == board)
drawn <- function(post_id, wage) app$db_exec(
  "INSERT OR REPLACE INTO class_wage_snapshots(round_id,snapshot_key,job_post_id,category_id,wage,source,snapshotted_at)
   VALUES(?,?,?,?,?,'random_draw',CURRENT_TIMESTAMP);", list(rid, paste0("post:", post_id), post_id, cc_cat, wage))
drawn(answer, 2); drawn(board, 3)
# Each kind is paid its own job's wage.
stopifnot(app$cold_call_wage_for(rid, "answer") == 2, app$cold_call_wage_for(rid, "board") == 3)
bid <- function(uid, post_id, wage) app$db_exec(
  "INSERT OR REPLACE INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,?);", list(rid, post_id, uid, wage))
bid("low", answer, 1.5); bid("exact", answer, 2); bid("high", answer, 3.5)
bid("boardonly", answer, 5); bid("boardonly", board, 3)
students <- data.frame(user_id = c("low", "exact", "high", "boardonly", "nobid"), stringsAsFactors = FALSE)
who <- function() sort(app$cold_call_eligible(rid, students)$user_id)

# Off by default: everyone present can be called, as before.
stopifnot(!app$cold_call_bidders_only(), identical(who(), sort(students$user_id)))
app$set_setting("cold_call_bidders_only", "1")
# On: a bid at or below the drawn wage of some cold-call job. No bid, or only
# bids above the wage, means not called.
stopifnot(app$cold_call_bidders_only(), identical(who(), c("boardonly", "exact", "low")))
# Answering and board work are separate jobs: given the kind, only that job's
# bids and drawn wage count.
who_for <- function(kind) sort(app$cold_call_eligible(rid, students, kind)$user_id)
stopifnot(identical(who_for("answer"), c("exact", "low")), identical(who_for("board"), "boardonly"))
# Bids recorded at the date's first draw are used; a later change is ignored.
app$db_exec("INSERT INTO job_wage_bid_locks(round_id,job_post_id,user_id,min_wage,submitted_at) SELECT round_id,job_post_id,user_id,min_wage,CURRENT_TIMESTAMP FROM job_wage_bids WHERE round_id=?;", list(rid))
bid("high", answer, 0.5)
stopifnot(identical(who(), c("boardonly", "exact", "low")))
# On a date without the random-wage rule, any bid on a cold-call job counts.
app$db_exec("UPDATE weekly_rounds SET wage_pricing_rule='pay_as_bid' WHERE id=?;", list(rid))
app$db_exec("DELETE FROM job_wage_bid_locks;")
stopifnot(identical(who(), c("boardonly", "exact", "high", "low")))
# A set wage for unprompted volunteering. Off by default: the volunteer is paid
# the class's volunteer wage. On: that wage whatever the day's draw was, a try
# never pays more than a success, and the one-paid-per-class limit still holds.
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Volunteer',4,'',1,0);")
vol_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active) VALUES(?,'Volunteer: answer',?,99,4,1,0,'volunteer',1);", list(rid, vol_cat))
vol_post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
round_row <- app$db_query("SELECT * FROM weekly_rounds WHERE id=?;", list(rid))
volunteer <- function(uid, outcome, capped = FALSE) app$queue_volunteer_event(round_row, vol_post, uid, outcome, "prof", pay_capped = capped)$tokens
before_flat <- volunteer("low", "succeed")
stopifnot(app$volunteer_flat_wage() == 0, before_flat > 1)
app$set_setting("volunteer_flat_wage", "1")
stopifnot(app$volunteer_flat_wage() == 1, volunteer("exact", "succeed") == 1, volunteer("high", "try") == 1, volunteer("nobid", "miss") == 0,
          volunteer("exact", "succeed", capped = TRUE) == 0)
app$set_setting("volunteer_flat_wage", "0.5")
stopifnot(volunteer("boardonly", "succeed") == 0.5, volunteer("boardonly", "try") == 0.5)
# A cold call is prompted and keeps its own job's wage.
stopifnot(app$cold_call_wage_for(rid, "answer") == 2, app$cold_call_wage_for(rid, "board") == 3)
app$set_setting("volunteer_flat_wage", "0")
stopifnot(volunteer("low", "succeed") == before_flat)

# Volunteering means asking a question. Stepping in to answer, or to work at
# the board, on a random-wage date fills a cold call nobody's bid cleared: it
# pays that cold-call job's drawn wage (answer 2, board 3), whatever the set
# volunteer wage is, and does not use up the one paid contribution.
app$db_exec("UPDATE weekly_rounds SET wage_pricing_rule='random_wage' WHERE id=?;", list(rid))
app$db_exec("DELETE FROM live_score_events;")
round_row <- app$db_query("SELECT * FROM weekly_rounds WHERE id=?;", list(rid))
vpost <- function(name) { app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active) VALUES(?,?,?,99,4,1,0,'volunteer',1);", list(rid, name, vol_cat))
  as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1]) }
ask <- vpost("Volunteer: ask a question"); fill_board <- vpost("Volunteer: graph/answer on board")
give <- function(post, uid, outcome = "succeed") app$queue_volunteer_event(round_row, post, uid, outcome, "prof", pay_capped = TRUE)$tokens
app$set_setting("volunteer_flat_wage", "1")
stopifnot(give(vol_post, "low") == 2, give(fill_board, "low") == 3,       # two fill-ins, both paid at the drawn wage
          give(ask, "low") == 1,                                           # the question: the set wage, still paid
          give(ask, "low") == 0,                                           # a second question the same class: unpaid
          give(vol_post, "low") == 2,                                      # another fill-in is still paid
          give(vol_post, "exact", "try") == 1, give(vol_post, "exact", "miss") == 0)
# On a practice date, or a date without the random-wage rule, nothing is a fill-in.
app$db_exec("UPDATE weekly_rounds SET wage_pricing_rule='pay_as_bid' WHERE id=?;", list(rid))
round_row <- app$db_query("SELECT * FROM weekly_rounds WHERE id=?;", list(rid))
stopifnot(give(vol_post, "high") == 1, give(vol_post, "high") == 0)        # the set wage and the limit apply again
app$set_setting("volunteer_flat_wage", "0")

# With no cold-call job posted, nobody is eligible rather than everybody.
app$db_exec("UPDATE job_posts SET active=0 WHERE round_id=?;", list(rid))
stopifnot(nrow(app$cold_call_eligible(rid, students)) == 0L)

cat("critical cold-call bidders and wage floor: PASS\n")
