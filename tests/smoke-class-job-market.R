# Smoke test for the class-job-market job catalog, bid lock, and clearing wage.
# Runs against a scratch SQLite DB by extracting the relevant functions and
# schema statements from app.R (the Shiny app itself is never started).
# Usage (from repo root): Rscript tests/smoke-class-job-market.R
# Requires: DBI, RSQLite
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

con <- dbConnect(SQLite(), tempfile(fileext = ".sqlite"))
db_query <- function(sql, params = NULL) {
  tryCatch(if (is.null(params)) dbGetQuery(con, sql) else dbGetQuery(con, sql, params = params),
           error = function(e) { message("db_query: ", e$message); data.frame() })
}
db_exec <- function(sql, params = NULL) {
  tryCatch(if (is.null(params)) dbExecute(con, sql) else dbExecute(con, sql, params = params),
           error = function(e) { message("db_exec: ", e$message); -1L })
}

app <- parse("apps/class-job-market/app.R")
# Pull the definitions we need out of app.R without running the whole app
wanted <- c("seed_class_job_defaults", "get_setting", "bid_lock_status",
            "volunteer_clearing_wage", "class_wage_snapshot", "freeze_class_wages",
            "close_bidding_after_draw", "overdue_pending_jobs", "assignment_round_for_timing",
            "reveal_timings_for_scope", "round_bid_datetime_value",
            "round_bid_window_values", "round_bid_window_status",
            "validate_ticket_allocation", "order_job_posts_for_clearing",
            "normalize_wage_pricing_rule", "uniform_procurement_wage", "compute_application_pairs",
            "ensure_column")
extracted <- 0
for (ex in app) {
  if (is.call(ex) && identical(as.character(ex[[1]]), "<-") &&
      is.name(ex[[2]]) && as.character(ex[[2]]) %in% wanted) {
    eval(ex, envir = globalenv()); extracted <- extracted + 1
  }
}
stopifnot(extracted == length(wanted))

# ── point-bid validation and roster scoping ─────────────────────────────────
ok <- validate_ticket_allocation(c(6, 4), 10)
stopifnot(ok$ok, ok$total == 10L, identical(ok$values, c(6L, 4L)))
stopifnot(!validate_ticket_allocation(c(10, 1), 10)$ok)
stopifnot(!validate_ticket_allocation(c(1.5, 2), 10)$ok)
window_values <- round_bid_window_values("2026-09-25", "09:30", "2026-09-25", "17:15")
stopifnot(window_values$open_at == "2026-09-25 09:30:00")
stopifnot(window_values$close_at == "2026-09-25 17:15:00")
stopifnot(inherits(try(round_bid_window_values("2026-09-25", "17:16",
                                              "2026-09-25", "17:15"), silent=TRUE),
                   "try-error"))
stopifnot(inherits(try(round_bid_datetime_value("2026-09-25", "25:00", "00:00"), silent=TRUE),
                   "try-error"))
window_row <- data.frame(bidding_enabled=2L,
                         bid_open_date="2026-09-25 09:30:00",
                         bid_close_date="2026-09-25 17:15:00")
stopifnot(round_bid_window_status(window_row, as.POSIXct("2026-09-25 12:00:00", tz="America/New_York"))$open)
stopifnot(round_bid_window_status(window_row, as.POSIXct("2026-09-25 08:00:00", tz="America/New_York"))$future)
stopifnot(round_bid_window_status(window_row, as.POSIXct("2026-09-25 18:00:00", tz="America/New_York"))$past)
window_row$bidding_enabled <- 1L
manual_status <- round_bid_window_status(window_row, as.POSIXct("2026-09-25 18:00:00", tz="America/New_York"))
stopifnot(manual_status$open, manual_status$manual_open, !manual_status$past)
window_row$bidding_enabled <- 0L
stopifnot(round_bid_window_status(window_row, as.POSIXct("2026-09-25 12:00:00", tz="America/New_York"))$paused)
legacy_window <- data.frame(bid_open_date="2026-09-20", bid_close_date="2026-09-30")
stopifnot(round_bid_window_status(legacy_window, as.POSIXct("2026-09-25 12:00:00", tz="America/New_York"))$open)
invalid_window <- data.frame(bidding_enabled=2L, bid_open_date="not-a-date", bid_close_date="")
stopifnot(round_bid_window_status(invalid_window)$invalid,
          !round_bid_window_status(invalid_window)$open)

set.seed(42)
point_posts <- data.frame(id=c(1L,2L), category_id=c(1L,2L), slots=c(1L,1L), wage=c(2,3))
point_students <- data.frame(user_id=c("s01-a","s01-b"))
point_bids <- data.frame(user_id=c("s01-a","s01-b","s02-outsider"),
                         category_id=c(1L,2L,1L), tickets=c(10L,10L,1000L))
point_pairs <- compute_application_pairs(point_posts, point_students, point_bids)
stopifnot(length(point_pairs) == 2L)
stopifnot(all(vapply(point_pairs, function(x) x[["uid"]], character(1)) %in% point_students$user_id))
stopifnot(all(vapply(point_pairs, function(x) {
  expected <- point_posts$wage[match(x$post_id, point_posts$id)]
  identical(as.numeric(x$wage), as.numeric(expected))
}, logical(1))))

point_multi_posts <- data.frame(id=c(2L,1L), category_id=c(2L,1L), slots=c(1L,1L),
                                wage=c(3,2), display_order=c(2L,1L))
point_multi_students <- data.frame(user_id="s01-a")
point_multi_bids <- data.frame(user_id=c("s01-a","s01-a"),
                               category_id=c(1L,2L), tickets=c(5L,5L))
point_multi_pairs <- compute_application_pairs(point_multi_posts, point_multi_students, point_multi_bids)
stopifnot(length(point_multi_pairs) == 2L)
stopifnot(identical(vapply(point_multi_pairs, function(x) x$post_id, integer(1)), c(1L,2L)))
stopifnot(all(vapply(point_multi_pairs, function(x) x$uid, character(1)) == "s01-a"))
point_single_pairs <- compute_application_pairs(point_multi_posts, point_multi_students, point_multi_bids, FALSE)
stopifnot(length(point_single_pairs) == 1L, point_single_pairs[[1]]$post_id == 1L)
second_price_bids <- data.frame(user_id=c("a","b","c"), min_wage=c(1,2,4))
stopifnot(uniform_procurement_wage(second_price_bids, c("a","b"), 2) == 4)
stopifnot(normalize_wage_pricing_rule("second_price") == "uniform_second_price")

# End-of-class jobs, especially lecture notes, belong to the class session
# that just ended. Timing must never advance their lecture/round index.
stopifnot(assignment_round_for_timing(3L, "end") == 3L)
stopifnot(assignment_round_for_timing(3L, "start") == 3L)
stopifnot(identical(reveal_timings_for_scope("start"), "start"))
stopifnot(identical(reveal_timings_for_scope("end"), "end"))
stopifnot(identical(reveal_timings_for_scope("all"), c("start", "end")))

# Pull the table-creation / migration SQL straight from app.R: run every
# top-level db_exec("...") call whose SQL is a literal string.
sql_run <- 0
run_exec_calls <- function(ex) {
  if (!is.call(ex)) return(invisible())
  fn <- as.character(ex[[1]])[1]
  if (fn %in% c("db_exec") && length(ex) >= 2 && is.character(ex[[2]])) {
    sql <- ex[[2]]
    if (grepl("job_|weekly_rounds|labor_settings|wage_bids|application_bids|users|arcade|volunteer_demand|class_wage_snapshots", sql)) {
      db_exec(sql); sql_run <<- sql_run + 1
    }
  } else if (fn == "ensure_column" && length(ex) >= 3 &&
             is.character(ex[[2]]) && is.character(ex[[3]])) {
    ensure_column(ex[[2]], ex[[3]])
  } else if (fn == "try" && length(ex) >= 2) {
    run_exec_calls(ex[[2]])
  }
}
for (ex in app) run_exec_calls(ex)
cat("ran", sql_run, "schema statements\n")
stopifnot(sql_run > 20)

# ── Simulate an OLD database state (pre-simplification catalog) ──────────────
old_cats <- c("Opening recap", "Reading analyst", "Policy/example scout",
              "Concept explainer", "Class record keeper", "Discussion lead",
              "Course-material fix or suggestion", "My custom category")
for (nm in old_cats)
  db_exec("INSERT INTO job_categories(name, default_wage) VALUES(?, 2);", list(nm))
db_exec("INSERT INTO job_templates(name, category_id, slots, suggested_wage, active)
         SELECT 'Opening recap', id, 1, 2, 1 FROM job_categories WHERE name='Opening recap';")
db_exec("INSERT INTO job_templates(name, category_id, slots, suggested_wage, active)
         SELECT 'Discussion lead', id, 1, 2, 1 FROM job_categories WHERE name='Discussion lead';")
db_exec("INSERT INTO weekly_rounds(label, assignment_mode, tiebreak_method, tokens_revealed, tickets_per_student)
         VALUES('Week 3','application_bidding','weighted_lottery',1,10);")
rid <- db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;")$id[1]
db_exec("INSERT INTO job_posts(round_id, job_name, category_id, slots)
         SELECT ?, 'Opening recap', id, 1 FROM job_categories WHERE name='Opening recap';", list(rid))
old_cat_id <- db_query("SELECT id FROM job_categories WHERE name='Opening recap';")$id[1]
db_exec("INSERT INTO wage_bids(round_id, category_id, user_id, min_wage) VALUES(?,?,'alice',3);",
        list(rid, old_cat_id))

# ── Run the new seed ─────────────────────────────────────────────────────────
seed_class_job_defaults()

cats <- db_query("SELECT name, voluntary, in_draw, default_wage FROM job_categories ORDER BY display_order, name;")
cat("categories after seed:\n"); print(cats)
stopifnot(setequal(
  cats$name,
  c("Class roles", "Volunteer", "Cold Call", "My custom category")))
stopifnot(cats$voluntary[cats$name == "Volunteer"] == 1)
stopifnot(cats$in_draw[cats$name == "Volunteer"] == 0)

tpl <- db_query("SELECT name, active, voluntary, in_draw, selection_time, slots, suggested_wage
                 FROM job_templates ORDER BY display_order;")
cat("\ntemplates after seed:\n"); print(tpl)
stopifnot(nrow(tpl[tpl$name == "Materials summary" & tpl$active == 1 & tpl$selection_time == "start", ]) == 1)
stopifnot(nrow(tpl[tpl$name == "Cold call: answer a question" & tpl$active == 1 & tpl$selection_time == "during", ]) == 1)
stopifnot(nrow(tpl[tpl$name == "Volunteer: ask a question" & tpl$voluntary == 1 & tpl$in_draw == 0 & tpl$slots == 99, ]) == 1)
# Old 'Opening recap' template deactivated, 'Discussion lead' kept + normalized
stopifnot(tpl$active[tpl$name == "Opening recap"] == 0)
stopifnot(tpl$active[tpl$name == "Discussion lead"] == 0)  # some-session job
stopifnot(tpl$selection_time[tpl$name == "Discussion lead"] == "end")

posts <- db_query("SELECT job_name, active, voluntary, in_draw, selection_time FROM job_posts WHERE round_id=?;", list(rid))
cat("\nposts in latest round:\n"); print(posts)
stopifnot(nrow(posts[posts$job_name == "Note taker" & posts$in_draw == 1, ]) == 1)
stopifnot(nrow(posts[posts$job_name == "Volunteer: answer a question" & posts$voluntary == 1, ]) == 1)
stopifnot(posts$active[posts$job_name == "Opening recap"] == 0)
# Cold-call templates are inactive so they should NOT have been copied
stopifnot(nrow(posts[grepl("^Cold call", posts$job_name) & posts$in_draw == 1, ]) == 2)

# Old category's bid migrated to Class roles, old category deleted
bid_cat <- db_query("SELECT jc.name FROM wage_bids wb JOIN job_categories jc ON jc.id=wb.category_id WHERE wb.user_id='alice';")
stopifnot(identical(bid_cat$name, "Class roles"))

# Individual jobs in the same category retain distinct wage submissions.
class_posts <- db_query("SELECT jp.id FROM job_posts jp JOIN job_categories jc ON jc.id=jp.category_id WHERE jp.round_id=? AND jc.name='Class roles' AND jp.active=1 ORDER BY jp.id LIMIT 2;", list(rid))
stopifnot(nrow(class_posts) == 2)
db_exec("INSERT INTO job_wage_bids(round_id, job_post_id, user_id, min_wage) VALUES(?,?,?,?);",
        list(rid, class_posts$id[1], "alice", 2))
db_exec("INSERT INTO job_wage_bids(round_id, job_post_id, user_id, min_wage) VALUES(?,?,?,?);",
        list(rid, class_posts$id[2], "alice", 7))
job_bids <- db_query("SELECT job_post_id, min_wage FROM job_wage_bids WHERE round_id=? AND user_id='alice' ORDER BY job_post_id;", list(rid))
stopifnot(nrow(job_bids) == 2, identical(job_bids$min_wage, c(2, 7)))

# ── Idempotence: instructor edits survive a restart re-seed ──────────────────
db_exec("UPDATE job_templates SET active=0 WHERE name='Critic/skeptic';")
db_exec("UPDATE job_templates SET active=1, selection_time='start' WHERE name='Discussion lead';")
n_tpl_before <- db_query("SELECT COUNT(*) n FROM job_templates;")$n[1]
seed_class_job_defaults()
stopifnot(db_query("SELECT COUNT(*) n FROM job_templates;")$n[1] == n_tpl_before)
tpl2 <- db_query("SELECT name, active, selection_time FROM job_templates;")
stopifnot(tpl2$active[tpl2$name == "Critic/skeptic"] == 0)          # edit preserved
stopifnot(tpl2$selection_time[tpl2$name == "Discussion lead"] == "start")
stopifnot(db_query("SELECT COUNT(*) n FROM job_categories;")$n[1] == 4)

# ── bid_lock_status ──────────────────────────────────────────────────────────
set_setting <- function(k, v) db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES(?,?);", list(k, v))
set_setting("bid_lock_enabled", "0")
stopifnot(isFALSE(bid_lock_status()$locked))
set_setting("bid_lock_enabled", "1")
set_setting("class_days", "Mon,Tue,Wed,Thu,Fri,Sat,Sun")
set_setting("class_start_time", "23:59"); set_setting("bid_lock_lead_min", "1439")
set_setting("bid_reopen_time", "23:59")
bl <- bid_lock_status()  # lock window covers ~whole day
cat("\nlock test (should be locked):", bl$locked, "|", bl$schedule_label, "\n")
stopifnot(isTRUE(bl$locked))
set_setting("class_days", "")
stopifnot(isFALSE(bid_lock_status()$locked))
# Defaults formatting: Mon/Wed 12pm class, 60-min lead, 5pm reopen
set_setting("class_days", "Mon,Wed"); set_setting("class_start_time", "12:00")
set_setting("bid_lock_lead_min", "60"); set_setting("bid_reopen_time", "17:00")
bl <- bid_lock_status()
cat("default schedule:", bl$schedule_label, "\n")
stopifnot(bl$lock_at == "11:00 AM", bl$class_at == "12:00 PM", bl$reopen_at == "5:00 PM")

# ── volunteer_clearing_wage ──────────────────────────────────────────────────
cat_ans <- db_query("SELECT id FROM job_categories WHERE name='Volunteer';")$id[1]
rid2 <- db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;")$id[1]
for (u in c("u1","u2","u3"))
  db_exec("INSERT INTO wage_bids(round_id, category_id, user_id, min_wage) VALUES(?,?,?,?);",
          list(rid2, cat_ans, u, match(u, c("u1","u2","u3")) * 2))  # bids 2, 4, 6
set_setting("volunteer_clearing_rule", "lowest")
stopifnot(volunteer_clearing_wage(rid2, cat_ans, 99L, query_fn = db_query) == 2)
set_setting("volunteer_clearing_rule", "demand")
stopifnot(volunteer_clearing_wage(rid2, cat_ans, 2L, query_fn = db_query) == 4)
stopifnot(volunteer_clearing_wage(rid2, cat_ans, 99L, query_fn = db_query) == 6)  # demand > bids -> highest
stopifnot(is.na(volunteer_clearing_wage(rid2, cat_ans + 999L, 1L, query_fn = db_query)))  # no bids -> NA
# posted rule: k comes from volunteer_demand for the round, fallback to slots
set_setting("volunteer_clearing_rule", "posted")
stopifnot(volunteer_clearing_wage(rid2, cat_ans, 1L, query_fn = db_query) == 2)  # nothing posted -> slots=1
db_exec("INSERT INTO volunteer_demand(round_id, category_id, demand) VALUES(?,?,2);", list(rid2, cat_ans))
stopifnot(volunteer_clearing_wage(rid2, cat_ans, 1L, query_fn = db_query) == 4)  # posted k=2 -> 2nd lowest
db_exec("UPDATE volunteer_demand SET demand=50 WHERE round_id=? AND category_id=?;", list(rid2, cat_ans))
stopifnot(volunteer_clearing_wage(rid2, cat_ans, 1L, query_fn = db_query) == 6)  # capped at n bids
set_setting("volunteer_clearing_rule", "lowest")

# The first class draw freezes every post wage. Later bid edits cannot alter
# cold-call, volunteer, or other displayed wages during class.
db_exec("UPDATE weekly_rounds SET assignment_mode='wage_bidding' WHERE id=?;", list(rid))
freeze_class_wages(rid, query_fn=db_query, exec_fn=db_exec)
frozen_post <- class_posts$id[1]
frozen_wage <- class_wage_snapshot(rid, frozen_post, query_fn=db_query)
stopifnot(frozen_wage == 2)
db_exec("UPDATE job_wage_bids SET min_wage=9 WHERE round_id=? AND job_post_id=? AND user_id='alice';",
        list(rid, frozen_post))
freeze_class_wages(rid, query_fn=db_query, exec_fn=db_exec)
stopifnot(class_wage_snapshot(rid, frozen_post, query_fn=db_query) == frozen_wage)

# Drawing closes bids until an instructor manually reopens them. With a
# recurring schedule enabled, it records a one-off lock and returns to schedule.
set_setting("bid_lock_enabled", "0")
db_exec("UPDATE weekly_rounds SET bidding_enabled=1 WHERE id=?;", list(rid))
stopifnot(close_bidding_after_draw(rid) == "closed until manually reopened")
stopifnot(db_query("SELECT bidding_enabled FROM weekly_rounds WHERE id=?;", list(rid))$bidding_enabled[1] == 0)
set_setting("bid_lock_enabled", "1")
close_bidding_after_draw(rid, as.POSIXct("2026-09-30 12:00:00", tz="America/New_York"))
stopifnot(db_query("SELECT bidding_enabled FROM weekly_rounds WHERE id=?;", list(rid))$bidding_enabled[1] == 2)
stopifnot(nzchar(get_setting("bid_draw_locked_until", "")))

# Every unfinished non-volunteer assignment from a past indexed class date is
# returned, rather than only assignments from the immediately previous row.
db_exec("INSERT OR IGNORE INTO users(user_id,display_name,course,section,active,is_admin,is_demo)
         VALUES('overdue-student','Overdue Student','TEST','S01',1,0,0);")
class_cat <- db_query("SELECT id FROM job_categories WHERE name='Class roles' LIMIT 1;")$id[1]
overdue_dates <- c("2000-01-01", "2001-01-01")
for (d in overdue_dates) {
  db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode) VALUES(?,?,'random');",
          list(paste("Past", d), d))
  old_rid <- db_query("SELECT last_insert_rowid() id;")$id[1]
  db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,active,in_draw,selection_time)
           VALUES(?, ?, ?, 1, 3, 1, 1, 'start');",
          list(old_rid, paste("Past job", d), class_cat))
  old_pid <- db_query("SELECT last_insert_rowid() id;")$id[1]
  db_exec("INSERT INTO job_assignments(round_id,user_id,job_post_id,assigned_wage,assignment_mode,
                                        status,outcome,scheduled_date,display_on_today)
           VALUES(?, 'overdue-student', ?, 3, 'random', 'assigned', '', ?, 1);",
          list(old_rid, old_pid, d))
}
overdue_rows <- overdue_pending_jobs(query_fn=db_query)
overdue_rows <- overdue_rows[overdue_rows$user_id == "overdue-student", , drop=FALSE]
stopifnot(nrow(overdue_rows) == 2L)
stopifnot(identical(as.character(overdue_rows$job_date), rev(overdue_dates)))

cat("\nALL SMOKE TESTS PASSED\n")
