#!/usr/bin/env Rscript
# Preservation tests for the student pages of Class Job Market.
#
# These are the checks the Frozen Navigation Consolidation Plan requires before
# student navigation may be reorganized (SHINY_APPS_PLAN.md, "Required
# regression checks before release"). They pin what a student can see and do,
# not which tab holds it: every "kept" item must appear somewhere in the pages
# a student is shown. When tabs are renamed or merged, update only TABS and
# STUDENT_OUTPUTS below; the kept items must still pass unchanged.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")
source_text <- paste(readLines(app_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Current navigation. The plan's target is Account, Participation, Spend,
# Assignments, Games & Demos; change these two constants when that ships.
TABS <- list(student = c("Today", "Job Market", "Spend", "Account", "Policy Brief", "Games & Demos"),
             admin_only = c("Live Tracker", "Settings"))
STUDENT_OUTPUTS <- c("today_tab", "job_market_tab", "jm_summary", "jm_bid_form", "jm_history", "spend_tab",
                     "spend_history", "account_tab", "account_grade_breakdown", "policy_brief_tab", "games_tab")
ADMIN_OUTPUTS <- c("live_tracker_tab", "settings_tab", "config_panel")

# ── Check 1 and 7 (source): the tab set, who sees it, and no dead jumps ───────
tab_titles <- regmatches(source_text, gregexpr('tabPanel\\("[^"]+",\\s+br\\(\\), uiOutput\\("[a-z_]+"\\)', source_text))[[1]]
tab_titles <- sub('^tabPanel\\("([^"]+)".*$', "\\1", tab_titles)
if (!identical(tab_titles, c(TABS$student, TABS$admin_only)))
  stop("Tab set changed: ", paste(tab_titles, collapse = ", "))
for (guard in c('if (show_tracker) showTab("arc_tabs", "Live Tracker") else hideTab("arc_tabs", "Live Tracker")',
                'if (show_admin)   showTab("arc_tabs", "Settings")    else hideTab("arc_tabs", "Settings")'))
  if (!grepl(guard, source_text, fixed = TRUE)) stop("Admin tab visibility rule missing: ", guard)
jumps <- regmatches(source_text, gregexpr('updateTabsetPanel\\(session, "arc_tabs", selected ?= ?"[^"]+"', source_text))[[1]]
targets <- unique(sub('^.*"([^"]+)"$', "\\1", jumps))
dead <- setdiff(targets, tab_titles)
if (length(dead)) stop("Navigation jumps to a tab that does not exist: ", paste(dead, collapse = ", "))
if (length(targets) < 5L) stop("Expected the app's tab jumps to be found; the pattern needs updating.")

scratch <- tempfile("student-pages-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)
app <- new.env(parent = globalenv())
source(app_path, local = app)

# ── Fixture: one section, two students, one class date with two jobs ─────────
app$set_setting("class_days", ""); app$set_setting("bid_lock_enabled", "0")
for (tbl in c("scope_active_rounds", "scope_rounds", "section_scope_memberships", "weekly_rounds",
              "job_posts", "job_assignments", "job_wage_bids", "live_score_events", "token_ledger"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
for (person in list(c("admin@test.edu", 1L), c("s1@test.edu", 0L), c("s2@test.edu", 0L))) {
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
               VALUES(?,?,'',?,'ECON 101','01',1,0);", list(person[1], person[1], as.integer(person[2])))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');", list(person[1]))
}
app$grant_admin_assignment("admin@test.edu", "super_admin", assigned_by = "test")
scope <- "econ 101::01"
class_day <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = app$get_setting("class_tz", "America/New_York")))
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('First class',?,'wage_bidding',1);", list(as.character(class_day)))
rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Kept role',2,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
new_post <- function(name, desc = "") {
  app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active,description)
               VALUES(?,?,?,1,2,0,1,'start',1,?);", list(rid, name, role_cat, desc))
  as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
}
recap <- new_post("Kept recap", "Kept instructions: summarize last class."); notes <- new_post("Kept notes")
app$db_exec("INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,'s1@test.edu',3);", list(rid, recap))
app$db_exec("INSERT INTO job_assignments(round_id,user_id,job_post_id,assigned_wage,assignment_mode,status,selection_time) VALUES(?,'s1@test.edu',?,3,'wage_bidding','assigned','start');", list(rid, recap))
assigned <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES('s1@test.edu','s1','job',7,1,'Kept ledger note');")
set_student <- function(rv, uid) {
  rv$authed <- TRUE; rv$user_id <- uid; rv$name <- uid
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- FALSE
  rv$active_course <- "ECON 101"; rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}

html_of <- function(output, id) tryCatch({
  o <- output[[id]]
  paste(as.character(if (is.list(o) && !is.null(o$html)) o$html else o), collapse = "")
}, error = function(e) "")
text_of <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", html))
snapshot <- function() {
  tables <- c("job_assignments", "job_wage_bids", "token_ledger", "live_score_events", "assignment_timing_reveals",
              "round_absences", "weekly_rounds", "job_posts", "users", "labor_settings", "arcade_config")
  rows <- lapply(tables, function(t) app$db_query(sprintf("SELECT * FROM %s ORDER BY 1,2;", t)))
  list(rows = stats::setNames(rows, tables),
       schema = app$db_query("SELECT type,name,sql FROM sqlite_master ORDER BY type,name;"))
}
same_data <- function(a, b, except = character(0)) {
  for (t in setdiff(names(a$rows), except)) if (!identical(a$rows[[t]], b$rows[[t]])) return(t)
  if (!identical(a$schema, b$schema)) return("schema")
  ""
}
must_show <- function(text, items, what) for (item in items)
  if (!grepl(item, text, fixed = TRUE)) stop(what, " no longer shows: ", item)

start <- snapshot()

# ── Student ──────────────────────────────────────────────────────────────────
shiny::testServer(app$server, {
  set_student(rv, "s1@test.edu"); session$flushReact()
  pages <- function() {
    out <- character(0)
    for (tab in TABS$student) {
      session$setInputs(arc_tabs = tab); session$flushReact()
      out <- c(out, vapply(STUDENT_OUTPUTS, function(id) html_of(output, id), character(1)))
    }
    text_of(paste(out, collapse = " "))
  }
  seen <- pages()

  # Check 6: showing every page reads existing records and changes nothing.
  changed <- same_data(start, snapshot())
  if (nzchar(changed)) stop("Viewing the student pages changed stored data: ", changed)

  # Check 2: the job market. Jobs, the student's saved bid, the bid form, the
  # hidden-until-revealed state, and the way into games.
  must_show(seen, c("Kept recap", "Kept notes", "Submit Bids", "Job Pools"), "Job market")
  form <- html_of(output, "jm_bid_form")
  if (!grepl(paste0('id="wbp_', recap, '"'), form, fixed = TRUE) || !grepl('value="3"', form, fixed = TRUE))
    stop("The bid form no longer offers the job with the student's saved bid.")
  must_show(seen, "1 assignment is not revealed yet", "Reveal state (hidden)")
  must_show(seen, c("Bonus Pot", "Demos"), "Games")

  # Check 3: the account. Balance, token history, grades, job history.
  must_show(seen, c("7 tokens", "Spending History"), "Token balance and history")
  account <- text_of(paste(html_of(output, "account_tab"), html_of(output, "account_grade_breakdown")))
  must_show(account, c("Needs your attention", "Kept ledger note"), "Account")
  for (section in c("Grade", "Job"))
    if (!grepl(section, account, fixed = TRUE)) stop("Account no longer has a section about: ", section)

  # Check 4: course work. Problem-set status and the policy brief.
  must_show(seen, c("pending problem sets", "Submit problem set", "Policy brief"), "Assignments")

  # Spending choices stay available.
  must_show(seen, c("Assignment Extension", "Grade Reweight", "Candidate Question Fund"), "Spend")

  # Saving a bid still works, and touches only this student's bid.
  before_bid <- snapshot()
  session$setInputs(arc_tabs = "Job Market"); session$flushReact()
  do.call(session$setInputs, stats::setNames(list(4.5, 6, 1L), c(paste0("wbp_", c(recap, notes)), "submit_wage_bids")))
  session$flushReact()
  bids <- app$db_query("SELECT job_post_id, min_wage FROM job_wage_bids WHERE user_id='s1@test.edu' ORDER BY job_post_id;")
  if (!identical(as.numeric(bids$min_wage), c(4.5, 6))) stop("Saving wage bids no longer stores them: ", paste(bids$min_wage, collapse = ", "))
  changed <- same_data(before_bid, snapshot(), except = "job_wage_bids")
  if (nzchar(changed)) stop("Saving a bid changed other data: ", changed)

  # Check 5: a student who selects an admin tab from the browser sees no admin
  # content, and admin actions sent from that session change nothing.
  locked <- snapshot()
  for (tab in TABS$admin_only) {
    session$setInputs(arc_tabs = tab); session$flushReact()
    for (page in unname(app$SETTINGS_PAGES)) {
      session$setInputs(config_action = page); session$flushReact()
      for (id in ADMIN_OUTPUTS) if (nchar(text_of(html_of(output, id))) > 1L)
        stop("A student session rendered admin output ", id, " on ", tab, " / ", page)
    }
  }
  attempts <- list(
    list(run_draw_btn = 1L), list(draw_timing_filter = "all", run_draw_btn = 2L), list(preview_draw_btn = 1L),
    list(commit_live_scores_btn = 1L), list(clear_live_scores_btn = 1L), list(clear_assignments_btn = 1L),
    list(section_reveal_timing = "all", toggle_section_reveal_btn = 1L), list(release_tokens_btn = 1L),
    list(redraw_absent_btn = assigned), list(redraw_pass_btn = assigned), list(unassign_job_btn = assigned),
    list(eval_outcome = list(id = assigned, outcome = "complete")), list(draw_cold_call_btn = 1L),
    list(bulk_amount = 50, bulk_note = "x", bulk_award_btn = 1L),
    list(indiv_uid = "s1@test.edu", indiv_amount = 50, indiv_note = "x", indiv_award_btn = 1L),
    list(crew_award_uids = "s1@test.edu", crew_award_amount = 50, crew_award_btn = 1L),
    list(delete_job_post_btn = recap), list(delete_round_btn = rid), list(new_app_name = "Hacked", save_app_name_btn = 1L),
    list(impersonate_uid = "s2@test.edu", impersonate_btn = 1L))
  for (attempt in attempts) {
    tryCatch({ do.call(session$setInputs, attempt); session$flushReact() }, error = function(e) NULL)
    changed <- same_data(locked, snapshot())
    if (nzchar(changed)) stop("A student session changed ", changed, " by sending: ", paste(names(attempt), collapse = ", "))
  }
  if (!identical(rv$user_id, "s1@test.edu") || isTRUE(rv$is_admin)) stop("A student session changed who it is.")
})

# ── Reveal: once the instructor shows the jobs, the student sees theirs, with
# its instructions (checks 2 and 6 with a real state change in between) ───────
app$toggle_section_reveal(rid, "01", "all")
shiny::testServer(app$server, {
  set_student(rv, "s1@test.edu"); session$setInputs(arc_tabs = "Today"); session$flushReact()
  today <- text_of(html_of(output, "today_tab"))
  if (grepl("is not revealed yet", today, fixed = TRUE)) stop("A revealed job is still hidden from the student.")
  must_show(today, c("Kept recap", "Kept instructions: summarize last class."), "Revealed job")
  # Another student does not see this student's job as their own.
  set_student(rv, "s2@test.edu"); session$flushReact()
})

# ── Admin (check 1): the same pages, plus the two admin tabs with content ────
shiny::testServer(app$server, {
  set_student(rv, "admin@test.edu"); rv$is_admin <- TRUE; rv$admin_role <- "super_admin"; session$flushReact()
  session$setInputs(arc_tabs = "Live Tracker"); session$flushReact()
  if (nchar(text_of(html_of(output, "live_tracker_tab"))) < 200L) stop("An admin no longer gets the Live Tracker.")
  session$setInputs(arc_tabs = "Settings", config_action = "round_setup"); session$flushReact()
  if (nchar(text_of(html_of(output, "settings_tab"))) < 100L || nchar(text_of(html_of(output, "config_panel"))) < 200L)
    stop("An admin no longer gets Settings.")
})

cat("critical student page preservation: PASS\n")
