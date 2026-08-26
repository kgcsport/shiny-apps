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
wanted <- c("seed_class_job_defaults", "get_setting", "bid_lock_status", "volunteer_clearing_wage")
extracted <- 0
for (ex in app) {
  if (is.call(ex) && identical(as.character(ex[[1]]), "<-") &&
      is.name(ex[[2]]) && as.character(ex[[2]]) %in% wanted) {
    eval(ex, envir = globalenv()); extracted <- extracted + 1
  }
}
stopifnot(extracted == length(wanted))

# Pull the table-creation / migration SQL straight from app.R: run every
# top-level db_exec("...") call whose SQL is a literal string.
sql_run <- 0
run_exec_calls <- function(ex) {
  if (!is.call(ex)) return(invisible())
  fn <- as.character(ex[[1]])[1]
  if (fn %in% c("db_exec") && length(ex) >= 2 && is.character(ex[[2]])) {
    sql <- ex[[2]]
    if (grepl("job_|weekly_rounds|labor_settings|wage_bids|application_bids|users|arcade|volunteer_demand", sql)) {
      db_exec(sql); sql_run <<- sql_run + 1
    }
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
  c("Class roles", "Answer a question", "Ask a question", "Board work", "My custom category")))
stopifnot(cats$voluntary[cats$name == "Ask a question"] == 1)
stopifnot(cats$in_draw[cats$name == "Ask a question"] == 0)

tpl <- db_query("SELECT name, active, voluntary, in_draw, selection_time, slots, suggested_wage
                 FROM job_templates ORDER BY display_order;")
cat("\ntemplates after seed:\n"); print(tpl)
stopifnot(nrow(tpl[tpl$name == "Materials summary" & tpl$active == 1 & tpl$selection_time == "start", ]) == 1)
stopifnot(nrow(tpl[tpl$name == "Cold call: answer a question" & tpl$active == 0 & tpl$selection_time == "during", ]) == 1)
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
stopifnot(nrow(posts[grepl("^Cold call", posts$job_name), ]) == 0)

# Old category's bid migrated to Class roles, old category deleted
bid_cat <- db_query("SELECT jc.name FROM wage_bids wb JOIN job_categories jc ON jc.id=wb.category_id WHERE wb.user_id='alice';")
stopifnot(identical(bid_cat$name, "Class roles"))

# ── Idempotence: instructor edits survive a restart re-seed ──────────────────
db_exec("UPDATE job_templates SET active=0 WHERE name='Critic/skeptic';")
db_exec("UPDATE job_templates SET active=1, selection_time='start' WHERE name='Discussion lead';")
n_tpl_before <- db_query("SELECT COUNT(*) n FROM job_templates;")$n[1]
seed_class_job_defaults()
stopifnot(db_query("SELECT COUNT(*) n FROM job_templates;")$n[1] == n_tpl_before)
tpl2 <- db_query("SELECT name, active, selection_time FROM job_templates;")
stopifnot(tpl2$active[tpl2$name == "Critic/skeptic"] == 0)          # edit preserved
stopifnot(tpl2$selection_time[tpl2$name == "Discussion lead"] == "start")
stopifnot(db_query("SELECT COUNT(*) n FROM job_categories;")$n[1] == 5)

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
cat_ans <- db_query("SELECT id FROM job_categories WHERE name='Answer a question';")$id[1]
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

cat("\nALL SMOKE TESTS PASSED\n")
