#!/usr/bin/env Rscript
# Regression coverage for shared change detection in Class Job Market. The
# contract: a student's page still picks up changes made by this process or by
# another one, a tab switch refreshes at once, and a session with nothing
# changing runs no queries beyond the two change counters.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("shared-polling-")
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
              "job_posts", "job_assignments", "job_wage_bids", "token_ledger"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
for (uid in c("s1@test.edu", "s2@test.edu")) {
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
               VALUES(?,?,'',0,'ECON 101','01',1,0);", list(uid, uid))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');", list(uid))
}
scope <- "econ 101::01"
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('Poll class','2026-11-30','wage_bidding',1);")
rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Poll role',2,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
post_sql <- "INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
             VALUES(?,?,?,1,2,0,1,'start',1);"
app$db_exec(post_sql, list(rid, "Recap", role_cat))

# Count every query that reaches the database, apart from the two change
# counters, by tracing the DBI call itself. This catches queries from session
# code and app-level helpers alike.
.query_log <- character(0)
suppressMessages(trace("dbGetQuery", where = asNamespace("DBI"), print = FALSE, tracer = quote({
  if (is.character(statement) && !grepl("PRAGMA data_version|total_changes\\(\\)", statement))
    assign(".query_log", c(get(".query_log", envir = globalenv()), statement), envir = globalenv())
})))
on.exit(suppressMessages(untrace("dbGetQuery", where = asNamespace("DBI"))), add = TRUE)
reset_queries <- function() assign(".query_log", character(0), envir = globalenv())
query_log <- function() get(".query_log", envir = globalenv())
set_student <- function(rv, uid) {
  rv$authed <- TRUE; rv$user_id <- uid; rv$name <- uid
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- FALSE
  rv$active_course <- "ECON 101"; rv$active_section <- "01"
  rv$active_sections <- "01"; rv$is_demo <- FALSE
}
jobs_seen <- function(poll) sort(as.character(poll$wage_posts$job_name))
other_process <- function(sql, params) {
  con <- DBI::dbConnect(RSQLite::SQLite(), app$DB_PATH)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sql, params = params)
}

# One step of the app-wide loop, then let the session react. In production the
# loop runs once a second; here each call stands for one second.
tick <- function(session, n = 1L) { for (i in seq_len(n)) app$shared_db_check(); session$flushReact() }

shiny::testServer(app$server, {
  set_student(rv, "s1@test.edu")
  session$setInputs(arc_tabs = "Job Market")
  tick(session, 4L)
  stopifnot(isTRUE(shared_polling_on), identical(jobs_seen(jobs_poll()), "Recap"),
            token_poll()$tokens_on_hand == 0, app$.shared_poll$sessions == 1L)

  # 1. Idle: with nothing changing, the loop reports no change and this
  #    session runs no query, until the once-a-minute heartbeat asks every
  #    session to re-check so clock-driven changes are not missed.
  reset_queries()
  quiet <- vapply(1:50, function(i) app$shared_db_check(), logical(1))
  session$elapse(50000); session$flushReact()
  stopifnot(!any(quiet), length(query_log()) == 0L)
  beat <- vapply(1:12, function(i) app$shared_db_check(), logical(1))
  session$flushReact()
  stopifnot(sum(beat) == 1L, length(query_log()) > 0L)

  # 2. A change made by another student's session in this process reaches the
  #    page on the next step.
  app$db_exec(post_sql, list(rid, "Note taker", role_cat))
  tick(session)
  stopifnot(identical(jobs_seen(jobs_poll()), c("Note taker", "Recap")))

  # 3. A second change straight after is held for the gap, then delivered: the
  #    last change in a burst is never lost.
  app$db_exec(post_sql, list(rid, "Critic", role_cat))
  tick(session)
  stopifnot(identical(jobs_seen(jobs_poll()), c("Note taker", "Recap")))
  tick(session, 2L)
  stopifnot(identical(jobs_seen(jobs_poll()), c("Critic", "Note taker", "Recap")))

  # 4. A change committed by another process (the login relay, a second app)
  #    is seen too.
  tick(session, 3L)
  other_process(post_sql, list(rid, "Scout", role_cat))
  tick(session)
  stopifnot("Scout" %in% jobs_seen(jobs_poll()))
  tick(session, 3L)
  other_process("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES(?,?,'job',3,1,'x');",
                list("s1@test.edu", "s1@test.edu"))
  tick(session)
  stopifnot(token_poll()$tokens_on_hand == 3, token_poll()$tokens_earned == 3)

  # 5. Unrelated churn costs this student only cheap checks and never a rebuild:
  #    another student's ledger entry leaves this student's wallet value alone.
  tick(session, 3L)
  before <- token_poll()
  app$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES('s2@test.edu','s2','job',9,1,'x');")
  tick(session)
  stopifnot(identical(token_poll(), before))

  # 6. This session's own write shows on its page at once, without the loop.
  tick(session, 3L)
  db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES('s1@test.edu','s1','job',2,1,'own');")
  session$flushReact()
  stopifnot(token_poll()$tokens_on_hand == 5)

  # 7. Switching tabs refreshes at once, with no database change and no wait,
  #    and the polls for the tab the student left go quiet again.
  tick(session, 4L)
  reset_queries()
  session$setInputs(arc_tabs = "Spend"); session$flushReact()
  stopifnot(any(grepl("flex_question", query_log())))
  tick(session, 4L)
  reset_queries()
  quiet <- vapply(1:40, function(i) app$shared_db_check(), logical(1))
  session$elapse(40000); session$flushReact()
  stopifnot(!any(quiet), length(query_log()) == 0L)
})
# The loop stops working for nobody: the session count returns to zero.
stopifnot(app$.shared_poll$sessions == 0L)

cat("critical shared polling: PASS\n")
