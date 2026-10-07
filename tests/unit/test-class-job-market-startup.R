# Unit tests for class-job-market startup and live-DB compatibility.
# Run: Rscript tests/run-unit-tests.R

suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
})

repo_root <- normalizePath(file.path(getwd(), "..", ".."), mustWork = TRUE)
app_file <- file.path(repo_root, "apps", "class-job-market", "app.R")

with_app_env <- function(code) {
  old_connect <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
  old_demo <- Sys.getenv("DEMO_MODE", unset = NA_character_)
  old_admin_emails <- Sys.getenv("ADMIN_EMAILS", unset = NA_character_)
  td <- tempfile("class-job-market-test-")
  dir.create(file.path(td, "data"), recursive = TRUE)
  Sys.setenv(CONNECT_CONTENT_DIR = td)
  Sys.unsetenv("DEMO_MODE")
  Sys.unsetenv("ADMIN_EMAILS")
  on.exit({
    if (is.na(old_connect)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_connect)
    if (is.na(old_demo)) Sys.unsetenv("DEMO_MODE") else Sys.setenv(DEMO_MODE = old_demo)
    if (is.na(old_admin_emails)) Sys.unsetenv("ADMIN_EMAILS") else Sys.setenv(ADMIN_EMAILS = old_admin_emails)
    unlink(td, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  force(code)
}

source_app <- function() {
  e <- new.env(parent = globalenv())
  source(app_file, local = e)
  if (exists("con", envir = e, inherits = FALSE)) {
    on.exit(suppressWarnings(try(if (DBI::dbIsValid(e$con)) DBI::dbDisconnect(e$con), silent = TRUE)), add = TRUE)
  }
  e
}

db_path <- function() {
  file.path(Sys.getenv("CONNECT_CONTENT_DIR"), "data", "class-job-market.sqlite")
}

cols <- function(con, table) {
  DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s);", table))$name
}

test_that("class-job-market starts against a fresh DB with required tables and columns", {
  with_app_env({
    app <- NULL
    expect_error(app <- suppressWarnings(source_app()), NA)
    shiny::testServer(app$server, {
      session$setInputs(auth_cookie = "")
      expect_true(any(grepl("Classroom Economy", as.character(output$root_ui), fixed = TRUE)))
    })
    expect_equal(app$norm_username(" KCOOMBS@VASSAR.EDU "), "kcoombs@vassar.edu")
    expect_equal(app$unique_ci(c("ECON 101", "econ 101", "ECON 102")), c("ECON 101", "ECON 102"))
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
    on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

    expect_true(file.exists(db_path()))
    expect_true(all(c("user_id", "display_name", "pw_hash", "course", "section", "active", "is_demo") %in% cols(con, "users")))
    expect_true("assignments_revealed" %in% cols(con, "arcade_state"))
    expect_true(all(c("bidding_enabled", "class_date", "allow_multiple_jobs", "wage_pricing_rule") %in%
                    cols(con, "weekly_rounds")))
    expect_true(all(c("round_id", "user_id", "marked_at") %in% cols(con, "round_absences")))
    expect_true(all(c("round_id", "job_post_id", "user_id", "min_wage", "submitted_at") %in%
                      cols(con, "job_wage_bids")))
    expect_true(all(c("cloudflare_assignment_id", "extension_target") %in% cols(con, "problem_sets")))
    expect_true(all(c("sync_status", "sync_error", "synced_at") %in% cols(con, "extension_purchases")))
    expect_true(all(c("round_id", "snapshot_key", "job_post_id", "category_id", "wage", "source") %in%
                      cols(con, "class_wage_snapshots")))
    expect_true(all(c("tokens_awarded", "tokens_credited", "status", "job_post_id",
                      "scheduled_date", "display_on_today") %in% cols(con, "job_assignments")))
    expect_true(all(c("job_post_id", "event_kind", "tokens", "committed_at") %in% cols(con, "live_score_events")))
    expect_true(all(c("unlock_cost","unlocked_at","course","topic","draw_group",
                      "draw_count","pool_size") %in% cols(con,"flex_questions")))
    expect_true(all(c("question_id","user_id","amount","ledger_id","contributed_at","scope_key") %in%
                      cols(con,"flex_question_contributions")))
    expect_true(all(c("question_id","scope_key","unlock_cost","unlocked_at") %in%
                      cols(con,"flex_question_scope_state")))
    expect_true(all(c("course_key","section_key","scope_key","course","section") %in%
                      cols(con,"section_scope_memberships")))
    expect_true(all(c("scope_key","course","sections","round_id") %in%
                      cols(con,"scope_active_rounds")))
    expect_true(all(c("scope_key","round_id") %in% cols(con,"scope_rounds")))
    expect_true("scope_key" %in% cols(con,"public_good_contributions"))
    expect_true(all(c("course","policy_team","component","status","gradebook_item",
                      "base_score","adjustment","final_score","overall_feedback",
                      "next_steps","released_at") %in%
                    cols(con,"policy_rubric_assessments")))
    expect_true(all(c("assessment_id","criterion_key","criterion_label","max_points",
                      "performance_level","score","feedback") %in%
                    cols(con,"policy_rubric_scores")))

    expect_equal(app$section_scope_key("ECON 342",c("B","A","A")),"econ 342::a|b")
    expect_equal(app$serialize_scope_sections(c("B","A","A")),"A||B")
    expect_equal(app$parse_scope_sections("B||A||A"),c("A","B"))

    expect_equal(app$question_cost_for_n(1, "N * (1 + (q / 2)^2)", 20), 20L)
    expect_equal(app$question_cost_for_n(2, "N * (1 + (q / 2)^2)", 20), 25L)
    expect_equal(app$question_cost_for_n(3, "N * (1 + (q / 2)^2)", 20), 40L)
    expect_equal(app$question_cost_for_n(4, "N * (1 + (q / 2)^2)", 20), 65L)
    expect_equal(app$question_cost_for_n(1, "20,25,40,65", 20), 20L)
    expect_equal(app$question_cost_for_n(4, "20,25,40,65", 20), 65L)

    round <- DBI::dbGetQuery(con, "SELECT label, class_date, tokens_revealed FROM weekly_rounds ORDER BY id DESC LIMIT 1;")
    posts <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM job_posts;")
    cats <- DBI::dbGetQuery(con, "SELECT name FROM job_categories ORDER BY display_order, name;")
    cold_posts <- DBI::dbGetQuery(con, "
      SELECT COUNT(*) n
      FROM job_posts
      WHERE COALESCE(active,1)=1
        AND COALESCE(in_draw,1)=1
        AND selection_time='during'
        AND job_name LIKE 'Cold call:%';")
    expect_equal(round$label[1], "Current Class")
    expect_equal(round$tokens_revealed[1], 0)
    expect_equal(round$class_date[1], as.character(Sys.Date()))
    round_rules <- DBI::dbGetQuery(con, "SELECT allow_multiple_jobs,wage_pricing_rule,bidding_enabled FROM weekly_rounds ORDER BY id DESC LIMIT 1;")
    expect_equal(round_rules$allow_multiple_jobs[1], 1L)
    expect_equal(round_rules$wage_pricing_rule[1], "pay_as_bid")
    expect_equal(round_rules$bidding_enabled[1], 0L)

    snapshot_post <- DBI::dbGetQuery(con,
      "SELECT id, category_id FROM job_posts WHERE round_id=(SELECT MAX(id) FROM weekly_rounds) AND active=1 ORDER BY id LIMIT 1;")
    snapshot_rid <- DBI::dbGetQuery(con, "SELECT MAX(id) AS id FROM weekly_rounds;")$id[1]
    DBI::dbExecute(con, "UPDATE weekly_rounds SET assignment_mode='wage_bidding' WHERE id=?;",
                   params=list(snapshot_rid))
    DBI::dbExecute(con,
      "INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,?);",
      params=list(snapshot_rid, snapshot_post$id[1], "snapshot-student", 4))
    app$set_setting("volunteer_clearing_rule", "lowest")
    expect_gt(app$freeze_class_wages(snapshot_rid), 0L)
    expect_equal(app$class_wage_snapshot(snapshot_rid, snapshot_post$id[1]), 4)
    DBI::dbExecute(con,
      "UPDATE job_wage_bids SET min_wage=1 WHERE round_id=? AND job_post_id=? AND user_id=?;",
      params=list(snapshot_rid, snapshot_post$id[1], "snapshot-student"))
    expect_gt(app$freeze_class_wages(snapshot_rid), 0L)
    expect_equal(app$class_wage_snapshot(snapshot_rid, snapshot_post$id[1]), 4)
    DBI::dbExecute(con, "UPDATE weekly_rounds SET assignment_mode='random' WHERE id=?;",
                   params=list(snapshot_rid))

    expect_equal(cats$name, c("Class roles", "Volunteer", "Cold Call"))
    expect_gt(posts$n[1], 0)
    expect_gt(cold_posts$n[1], 0)

    ordered_templates <- DBI::dbGetQuery(con, "
      SELECT name, display_order FROM job_templates
      WHERE lower(name) IN (
        \x27last class recap\x27,\x27materials summary\x27,\x27note taker\x27,
        \x27critic/skeptic\x27,\x27policy/example scout\x27)
      ORDER BY display_order;")
    expect_equal(ordered_templates$name,
                 c("Last class recap", "Materials summary", "Note taker",
                   "Critic/skeptic", "Policy/example scout"))
    expect_equal(ordered_templates$display_order, 1:5)

    post_ids <- DBI::dbGetQuery(con,
      "SELECT id, display_order FROM job_posts WHERE round_id=(SELECT MAX(id) FROM weekly_rounds) ORDER BY display_order, id LIMIT 2;")
    DBI::dbExecute(con,
      "INSERT OR IGNORE INTO users(user_id,display_name,is_admin,active,is_demo) VALUES(\x27multi-job-student\x27,\x27Multiple Jobs\x27,0,1,0);")
    for (post_id in post_ids$id) {
      DBI::dbExecute(con,
        "INSERT INTO job_assignments(round_id,user_id,job_post_id,status) VALUES((SELECT MAX(id) FROM weekly_rounds),?,?,\x27assigned\x27);",
        params = list("multi-job-student", post_id))
    }
    expect_equal(DBI::dbGetQuery(con,
      "SELECT COUNT(*) n FROM job_assignments WHERE user_id=\x27multi-job-student\x27;")$n[1], 2L)
    expect_error(DBI::dbExecute(con,
      "INSERT INTO job_assignments(round_id,user_id,job_post_id) VALUES((SELECT MAX(id) FROM weekly_rounds),?,?);",
      params = list("multi-job-student", post_ids$id[1])), "UNIQUE")

    point_posts <- data.frame(id = c(2L, 1L), category_id = c(2L, 1L),
                              slots = c(1L, 1L), wage = c(2, 2),
                              display_order = c(2L, 1L))
    point_students <- data.frame(user_id = "multi-job-student")
    point_bids <- data.frame(user_id = c("multi-job-student", "multi-job-student"),
                             category_id = c(1L, 2L), tickets = c(5L, 5L))
    point_pairs <- app$compute_application_pairs(point_posts, point_students, point_bids)
    expect_equal(vapply(point_pairs, function(x) x$post_id, integer(1)), c(1L, 2L))
    expect_equal(vapply(point_pairs, function(x) x$uid, character(1)),
                 rep("multi-job-student", 2))

    settings <- DBI::dbGetQuery(con, "
      SELECT key, value FROM labor_settings
      WHERE key IN ('active_course', 'active_section', 'hide_archived_students',
                    'today_announcement')
      ORDER BY key;")
    expect_equal(settings$key, c("active_course", "active_section", "hide_archived_students",
                                 "today_announcement"))
    expect_equal(settings$value, c("", "", "0", ""))
    expect_gte(app$set_setting("today_announcement", "Bring worksheet 3."), 0L)
    expect_equal(app$get_setting("today_announcement", ""), "Bring worksheet 3.")
    expect_gte(app$set_setting("today_announcement", ""), 0L)
    expect_equal(app$get_setting("today_announcement", "missing"), "")

    extension_settings <- DBI::dbGetQuery(con, "
      SELECT key, value FROM labor_settings
      WHERE key IN ('extension_base_hours', 'extension_base_tokens',
                    'extension_cost_exponent', 'extension_max_hours',
                    'extension_step_hours', 'extension_shortcuts')
      ORDER BY key;")
    expect_equal(extension_settings$key,
                 sort(c("extension_base_hours", "extension_base_tokens",
                        "extension_cost_exponent", "extension_max_hours",
                        "extension_step_hours", "extension_shortcuts")))

    values <- c(extension_base_hours="24", extension_base_tokens="3",
                extension_cost_exponent="1.35", extension_max_hours="168",
                extension_step_hours="1", extension_shortcuts="24,48,72")
    pricing <- app$extension_pricing_settings(function(key, default)
      if (!is.null(values[[key]])) values[[key]] else default)
    costs <- vapply(c(24, 48, 72), app$extension_cost_for_hours, numeric(1), settings=pricing)
    expect_equal(costs, c(3, 8, 14))
    expect_gt(pricing$exponent, 1)
    expect_gte(costs[3] - costs[2], costs[2] - costs[1])

    policy_exec <- function(sql, params)
      DBI::dbExecute(con, sql, params=params)
    app$upsert_policy_group_assignment(
      "alice", "Team A", "2026-10-05", "Trade", "tariffs", 2L, "seed-1",
      exec_fn=policy_exec)
    app$upsert_policy_group_assignment(
      "alice", "Team B", "2026-10-12", "Labor", NA_character_, 1L, NA_character_,
      exec_fn=policy_exec)
    policy <- DBI::dbGetQuery(con, "
      SELECT user_id, policy_team, presentation_date, course_unit,
             topic_interests, assigned_rank, allocation_seed
      FROM policy_group_assignments WHERE user_id='alice';")
    expect_equal(nrow(policy), 1L)
    expect_equal(policy$policy_team, "Team B")
    expect_equal(policy$presentation_date, "2026-10-12")
    expect_equal(policy$course_unit, "Labor")
    expect_true(is.na(policy$topic_interests))
    expect_equal(policy$assigned_rank, 1L)
    expect_true(is.na(policy$allocation_seed))

    expect_equal(app$assignment_history_state()$label, "Outstanding")
    expect_equal(app$assignment_history_state("complete")$label, "Completed")
    expect_equal(app$assignment_history_state("tried")$label, "Tried")
    expect_equal(app$assignment_history_state("missed")$label, "Not completed")
    expect_equal(
      app$assignment_history_state(pending_outcome = "complete")$label,
      "Pending: Completed"
    )
    expect_equal(app$assignment_history_state(assignment_status = "absent_redrawn")$code, "absent")
    expect_false(app$exclude_existing_assignments_for_timing("all"))
    expect_false(app$exclude_existing_assignments_for_timing("during"))
    expect_false(app$exclude_existing_assignments_for_timing("during class"))
    expect_false(app$exclude_existing_assignments_for_timing("start"))
    expect_false(app$exclude_existing_assignments_for_timing("end"))
    expect_true(app$exclude_existing_assignments_for_timing("start", FALSE))
    expect_true(app$exclude_existing_assignments_for_timing("end", FALSE))
    expect_false(app$exclude_existing_assignments_for_timing("during", FALSE))
    expect_equal(app$normalize_wage_pricing_rule("second_price"), "uniform_second_price")
    expect_equal(app$normalize_assignment_mode("RANDOM"), "random")
    expect_equal(app$normalize_assignment_mode("bogus"), "random")
    expect_equal(app$round_bidding_enabled_for_mode("random", 1L), 0L)
    expect_equal(app$round_bidding_enabled_for_mode("wage_bidding", 1L), 1L)
    random_sig <- app$round_state_signature(data.frame(id=1L, assignment_mode="random", bidding_enabled=0L))
    wage_sig <- app$round_state_signature(data.frame(id=1L, assignment_mode="wage_bidding", bidding_enabled=0L))
    expect_false(identical(random_sig, wage_sig))
    wage_bids <- data.frame(user_id=c("a","b","c"), min_wage=c(1,2,4))
    expect_equal(app$uniform_procurement_wage(wage_bids, c("a","b"), 2), 4)
    expect_equal(app$uniform_procurement_wage(wage_bids, "a", 2), 2)
    single_pairs <- app$compute_application_pairs(point_posts, point_students, point_bids, FALSE)
    expect_equal(length(single_pairs), 1L)
    expect_equal(single_pairs[[1]]$post_id, 1L)

    expect_match(app$ARCADE_CSS, "min-height: 44px", fixed=TRUE)
    expect_match(app$ARCADE_CSS, "max-width: 1100px", fixed=TRUE)
    expect_match(app$ARCADE_CSS, "overflow-wrap: normal", fixed=TRUE)
    expect_match(app$ARCADE_CSS, ".arc-body .btn-file", fixed=TRUE)
    expect_match(app$ARCADE_CSS, "padding-left: 1.8rem", fixed=TRUE)
    app_source <- paste(readLines(app_file, warn = FALSE), collapse = "\n")
    expect_match(app_source, "session$allowReconnect(TRUE)", fixed = TRUE)
    expect_match(app_source, "choose_reveal_section_btn", fixed = TRUE)
    expect_match(app_source, "COALESCE(is_demo,0)=0", fixed = TRUE)
    expect_match(app_source, "No assigned jobs.", fixed = TRUE)
    expect_match(app_source, "account-jobs-panel", fixed = TRUE)
    expect_match(app_source, "!timing_key %in% c(\"during\", \"during class\")", fixed = TRUE)
    expect_match(app_source, "gradebook_upload_template_", fixed = TRUE)
    expect_match(app_source, "CSV rubric — one row per student and assignment", fixed = TRUE)
    expect_match(app_source, "user_id = rep(as.character(student$user_id)", fixed = TRUE)
    expect_match(app_source, "assignment = manual_items$assignment", fixed = TRUE)
    expect_match(app_source, "if (all(is.na(c(scr, pct)))) next", fixed = TRUE)
    expect_match(app_source, "idx_student_grades_student_assignment", fixed = TRUE)
    expect_match(app_source, "upsert_student_grade", fixed = TRUE)
    expect_match(app_source, "save_manual_grade_btn", fixed = TRUE)
    expect_match(app_source, "Saving replaces any existing grade", fixed = TRUE)
    expect_match(app_source, "save_today_announcement_btn", fixed = TRUE)
    expect_match(app_source, "selectizeInput(\"active_section_sel\"", fixed = TRUE)
    expect_match(app_source, "Current selected section scope", fixed = TRUE)
    expect_match(app_source, "flex_question_scope_state", fixed = TRUE)
    expect_match(app_source, "draw_count and pool_size", fixed = TRUE)
    expect_match(app_source, "public_good_contributions(scope_key,public_good_id)", fixed = TRUE)
    expect_match(app_source, "announcement_poll <- reactivePoll", fixed = TRUE)
    expect_match(app_source, "edit_round_bidding_enabled", fixed = TRUE)
    expect_match(app_source, "edit_round_allow_multiple", fixed = TRUE)
    expect_match(app_source, "edit_round_wage_pricing", fixed = TRUE)
    expect_match(app_source, "Uniform second price", fixed = TRUE)
    expect_match(app_source, "Open now (override schedules)", fixed = TRUE)
    expect_match(app_source, "window$scheduled && bl$locked", fixed = TRUE)
    expect_match(app_source, "FROM job_wage_bids WHERE user_id=?", fixed = TRUE)
    expect_match(app_source, "list(uid))$ts[1]", fixed = TRUE)
    expect_false(grepl("MAX(submitted_at),'') ts FROM job_wage_bids;", app_source, fixed = TRUE))
    expect_match(app_source, "edit_round_open_time", fixed = TRUE)
    expect_match(app_source, "round_bid_window_values", fixed = TRUE)
    expect_match(app_source, "Last Class Jobs Still Pending", fixed = TRUE)
    expect_match(app_source, "tags$th(\"Class date\")", fixed = TRUE)
    expect_match(app_source, "< date('now','localtime')", fixed = TRUE)
    expect_match(app_source, "COALESCE(ja.assigned_wage, jp.wage_override, jc.default_wage, 0)", fixed = TRUE)
    expect_false(grepl("CASE WHEN ja.assignment_mode='wage_bidding' AND ja.assigned_wage IS NOT NULL", app_source, fixed = TRUE))
    expect_match(app_source, "scheduled_date <- as.character(suppressWarnings(as.Date(round$class_date", fixed = TRUE)
    expect_match(app_source, "display_today <- 1L", fixed = TRUE)
    expect_false(grepl("manual_assign_show_today", app_source, fixed = TRUE))
    expect_match(app_source, "observeEvent(input$market_class_date", fixed = TRUE)
    expect_match(app_source, "activate_class_date", fixed = TRUE)
    expect_match(app_source, "Dates replace rounds", fixed = TRUE)
    expect_match(app_source, "Class Job Controls", fixed = TRUE)
    expect_match(app_source, "freeze_class_wages(rid)", fixed = TRUE)
    expect_match(app_source, "close_bidding_after_draw(rid)", fixed = TRUE)
    expect_match(app_source, "active_round_id", fixed = TRUE)
    expect_match(app_source, "COALESCE(jp.voluntary,COALESCE(jc.voluntary,0),0)=0", fixed = TRUE)
    expect_match(app$ARCADE_CSS, ".today-announcement", fixed = TRUE)
    expect_match(app$ARCADE_CSS, ".arc-font-ctrl { display:flex; flex:1; }", fixed=TRUE)
    expect_match(as.character(app$COOKIE_JS), "classJobFontScale", fixed=TRUE)
  })
})

test_that("ADMIN_EMAILS bootstraps Google admins on fresh DB startup", {
  with_app_env({
    Sys.setenv(ADMIN_EMAILS = "'kcoombs@vassar.edu', other-admin@vassar.edu")
    expect_error(suppressWarnings(source_app()), NA)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
    on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

    admins <- DBI::dbGetQuery(con, "
      SELECT user_id, is_admin, active, COALESCE(is_demo,0) AS is_demo
      FROM users
      WHERE user_id IN ('kcoombs@vassar.edu', 'other-admin@vassar.edu')
      ORDER BY user_id;")

    expect_equal(admins$user_id, c("kcoombs@vassar.edu", "other-admin@vassar.edu"))
    expect_true(all(admins$is_admin == 1L))
    expect_true(all(admins$active == 1L))
    expect_true(all(admins$is_demo == 0L))
  })
})

test_that("demo bootstrap upgrades an old users schema and repairs credentials", {
  with_app_env({
    app <- suppressWarnings(source_app())
    demo_path <- file.path(Sys.getenv("CONNECT_CONTENT_DIR"), "data", "class-job-market-demo.sqlite")
    demo_con <- DBI::dbConnect(RSQLite::SQLite(), demo_path)
    on.exit(suppressWarnings(try(DBI::dbDisconnect(demo_con), silent=TRUE)), add=TRUE)
    DBI::dbExecute(demo_con, "
      CREATE TABLE users(
        user_id TEXT PRIMARY KEY,
        display_name TEXT,
        pw_hash TEXT,
        is_admin INTEGER DEFAULT 0,
        section TEXT
      );")
    DBI::dbExecute(demo_con, "
      INSERT INTO users(user_id,display_name,pw_hash,is_admin,section)
      VALUES('alice','Old Alice','bad-hash',0,'OLD');")
    DBI::dbExecute(demo_con, "
      CREATE TABLE job_categories(
        id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, wage REAL, active INTEGER DEFAULT 1
      );")
    DBI::dbExecute(demo_con, "
      CREATE TABLE weekly_rounds(
        id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT, section TEXT,
        status TEXT DEFAULT 'open', created_at TEXT DEFAULT CURRENT_TIMESTAMP
      );")
    DBI::dbExecute(demo_con, "INSERT INTO weekly_rounds(label) VALUES('Legacy Random');")
    DBI::dbExecute(demo_con, "
      CREATE TABLE job_posts(
        id INTEGER PRIMARY KEY AUTOINCREMENT, round_id INTEGER,
        category_id INTEGER, slots INTEGER DEFAULT 1, wage REAL DEFAULT 0
      );")
    DBI::dbExecute(demo_con, "
      CREATE TABLE job_templates(
        id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT
      );")

    expect_error(app$demo_db_bootstrap(demo_con, db_path()), NA)
    expect_true(all(c("course", "active", "is_demo") %in% cols(demo_con, "users")))
    repaired <- DBI::dbGetQuery(demo_con, "
      SELECT user_id, display_name, pw_hash, is_admin, course, section, active, is_demo
      FROM users WHERE user_id IN ('alice','instructor') ORDER BY user_id;")
    expect_equal(repaired$user_id, c("alice", "instructor"))
    expect_true(bcrypt::checkpw("test123", repaired$pw_hash[repaired$user_id == "alice"]))
    expect_true(bcrypt::checkpw("admin123", repaired$pw_hash[repaired$user_id == "instructor"]))
    expect_true(all(repaired$course == "DEMO 101"))
    expect_true(all(repaired$active == 1L))

    practice <- DBI::dbGetQuery(demo_con, "
      SELECT wr.id, wr.label, wr.assignment_mode, wr.bidding_enabled,
             COUNT(jp.id) AS jobs
      FROM weekly_rounds wr
      LEFT JOIN job_posts jp ON jp.round_id=wr.id
      WHERE wr.label='Demo Practice'
      GROUP BY wr.id, wr.label, wr.assignment_mode, wr.bidding_enabled;")
    expect_equal(nrow(practice), 1L)
    expect_equal(practice$assignment_mode[1], "application_bidding")
    expect_equal(practice$bidding_enabled[1], 1L)
    legacy_random <- DBI::dbGetQuery(demo_con, "SELECT assignment_mode,bidding_enabled FROM weekly_rounds WHERE label='Legacy Random';")
    expect_equal(legacy_random$assignment_mode[1], "random")
    expect_equal(legacy_random$bidding_enabled[1], 0L)
    expect_equal(practice$jobs[1], 4L)
    fake_categories <- DBI::dbGetQuery(demo_con, "
      SELECT name FROM job_categories
      WHERE name IN ('Recap & Synthesis','Notes & Records',
                     'Examples & Evidence','Critique & Questions');")
    expect_equal(nrow(fake_categories), 4L)
    expect_true(all(c("default_wage", "description", "voluntary", "in_draw") %in%
                    cols(demo_con, "job_categories")))
    expect_true(all(c("job_name", "wage_override", "in_draw", "selection_time", "description") %in%
                    cols(demo_con, "job_posts")))

    # Exercise the same columns used by Settings -> Add job type / Add job post.
    DBI::dbExecute(demo_con,
      "INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw)
       VALUES('Test Job Type',2,'Added after migration',0,1);")
    added_category <- DBI::dbGetQuery(demo_con,
      "SELECT id FROM job_categories WHERE name='Test Job Type';")$id[1]
    DBI::dbExecute(demo_con,
      "INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,
                             in_draw,selection_time,description)
       VALUES(?,?,?,1,2,1,'start','Added after migration');",
      list(practice$id[1], "Test Sandbox Job", added_category))
    expect_equal(DBI::dbGetQuery(demo_con,
      "SELECT COUNT(*) n FROM job_posts WHERE job_name='Test Sandbox Job';")$n[1], 1L)

    category_id <- DBI::dbGetQuery(demo_con,
      "SELECT id FROM job_categories WHERE name='Recap & Synthesis';")$id[1]
    DBI::dbExecute(demo_con,
      "INSERT INTO application_bids(round_id,category_id,user_id,tickets)
       VALUES(?,?,?,?);", list(practice$id[1], category_id, "alice", 10L))
    expect_error(app$demo_db_bootstrap(demo_con, db_path()), NA)
    expect_equal(DBI::dbGetQuery(demo_con,
      "SELECT COUNT(*) n FROM weekly_rounds WHERE label='Demo Practice';")$n[1], 1L)
    expect_equal(DBI::dbGetQuery(demo_con,
      "SELECT COUNT(*) n FROM application_bids WHERE user_id='alice';")$n[1], 1L)
  })
})

test_that("custom grade item weights drive category and overall grades", {
  with_app_env({
    app <- suppressWarnings(source_app())
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
    on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

    DBI::dbExecute(con, "DELETE FROM gradebook_item_names;")
    DBI::dbExecute(con, "DELETE FROM gradebook_categories;")
    DBI::dbExecute(con, "DELETE FROM student_grades;")
    DBI::dbExecute(con, "INSERT OR IGNORE INTO users(user_id, display_name, section, active) VALUES('alice', 'Alice', 'S01', 1);")
    DBI::dbExecute(con, "
      INSERT INTO gradebook_categories(id, name, weight, item_count, item_prefix, max_points, source, display_order)
      VALUES(100, 'Problem Sets', 30, 3, 'PS', 100, 'manual', 1);")
    DBI::dbExecute(con, "
      INSERT INTO gradebook_item_names(category_id, item_index, item_name, item_weight)
      VALUES
        (100, 1, 'PS1', 5),
        (100, 2, 'PS2', 10),
        (100, 3, 'PS3', 15);")
    DBI::dbExecute(con, "
      INSERT INTO student_grades(user_id, assignment_name, score, max_score, grade_pct)
      VALUES
        ('alice', 'PS1', 100, 100, 100),
        ('alice', 'PS2',  50, 100,  50),
        ('alice', 'PS3', 100, 100, 100);")

    result <- app$compute_student_grade("alice")
    expected <- (100 * 5 + 50 * 10 + 100 * 15) / 30

    expect_equal(result$cats$cat_avg[1], expected, tolerance = 1e-8)
    expect_equal(result$cats$graded_weight[1], 30)
    expect_equal(result$overall, expected, tolerance = 1e-8)
    expect_equal(result$items$item_weight, c(5, 10, 15))
  })
})


test_that("policy rubric uses narrow anchors with a separate missing state", {
  with_app_env({
    app <- suppressWarnings(source_app())
    on.exit(suppressWarnings(try(
      if (!is.null(app$conn) && DBI::dbIsValid(app$conn)) DBI::dbDisconnect(app$conn),
      silent = TRUE)), add = TRUE)

    catalog <- app$policy_rubric_catalog()
    expect_equal(names(catalog), c("presentation","progress","brief"))
    expect_true(all(vapply(catalog, function(x)
      sum(as.numeric(x$criteria$max_points)) == 100, logical(1))))

    expect_equal(unname(app$policy_rubric_anchor_points(15)),
                 c(15,12.8,10.5,7.5,0))
    expect_equal(unname(app$policy_rubric_anchor_points(20)),
                 c(20,17,14,10,0))
    expect_equal(names(app$policy_rubric_anchor_points(15)),
                 c("excellent","proficient","developing","incomplete","missing"))

    presentation_scores <- setNames(
      as.numeric(catalog$presentation$criteria$max_points),
      catalog$presentation$criteria$key)
    scored <- app$policy_rubric_score("presentation", presentation_scores, -2)
    expect_true(scored$ok)
    expect_equal(scored$base_score, 100)
    expect_equal(scored$final_score, 98)

    presentation_scores[1] <- 16
    expect_false(app$policy_rubric_score(
      "presentation", presentation_scores, 0)$ok)
    expect_false(app$policy_rubric_score(
      "presentation", presentation_scores * 0, -11)$ok)

    source_text <- paste(readLines(app_file, warn=FALSE), collapse="\n")
    expect_match(source_text, '"Policy Rubrics"        = "policy_rubrics"', fixed=TRUE)
    expect_match(source_text, 'Save private draft', fixed=TRUE)
    expect_match(source_text, 'Release to team', fixed=TRUE)
    expect_match(source_text, 'account_policy_feedback', fixed=TRUE)
  })
})


test_that("policy rubric drafts privately, releases to a team, and writes grades", {
  with_app_env({
    app <- suppressWarnings(source_app())
    on.exit(suppressWarnings(try(
      if (!is.null(app$conn) && DBI::dbIsValid(app$conn)) DBI::dbDisconnect(app$conn),
      silent = TRUE)), add = TRUE)

    for (uid in c("rubric-alice","rubric-bob")) {
      app$db_exec(
        "INSERT INTO users(user_id,display_name,is_admin,course,section,active,is_demo)
         VALUES(?,?,0,'ECON 342','A',1,0);",
        list(uid, tools::toTitleCase(sub("rubric-","",uid))))
      app$upsert_policy_group_assignment(
        uid, "Rubric Team", "2026-10-07", "Business and capital taxation")
    }
    app$db_exec(
      "INSERT INTO gradebook_categories(name,weight,item_count,item_prefix,max_points,source,display_order)
       VALUES('Policy project',100,1,'Policy Presentation',100,'manual',1);")
    category_id <- app$db_query(
      "SELECT id FROM gradebook_categories WHERE name='Policy project';")$id[1]
    app$db_exec(
      "INSERT INTO gradebook_item_names(category_id,item_index,item_name)
       VALUES(?,1,'Policy Presentation');", list(category_id))

    shiny::testServer(app$server, {
      rv$authed <- TRUE
      rv$is_admin <- TRUE
      rv$impersonating <- FALSE
      rv$user_id <- "rubric-admin"
      rv$active_course <- "ECON 342"
      rv$active_sections <- "A"

      session$setInputs(
        policy_rubric_team="Rubric Team",
        policy_rubric_component="presentation",
        policy_rubric_gradebook_item="Policy Presentation",
        policy_level_presentation_question="excellent",
        policy_score_presentation_question=15,
        policy_feedback_presentation_question="Focused question.",
        policy_level_presentation_context="proficient",
        policy_score_presentation_context=12.8,
        policy_feedback_presentation_context="Add one baseline.",
        policy_level_presentation_economics="excellent",
        policy_score_presentation_economics=20,
        policy_feedback_presentation_economics="Clear mechanism.",
        policy_level_presentation_evidence="developing",
        policy_score_presentation_evidence=14,
        policy_feedback_presentation_evidence="Explain identification.",
        policy_level_presentation_alternatives="proficient",
        policy_score_presentation_alternatives=12.8,
        policy_feedback_presentation_alternatives="Good comparison.",
        policy_level_presentation_communication="excellent",
        policy_score_presentation_communication=15,
        policy_feedback_presentation_communication="Clear delivery.",
        policy_rubric_adjustment=0,
        policy_rubric_overall="Strong early version.",
        policy_rubric_next="Strengthen the evidence section."
      )
      session$setInputs(save_policy_rubric_draft_btn=1)
      session$flushReact()

      expect_equal(db_query(
        "SELECT COUNT(*) n FROM policy_rubric_assessments WHERE status='draft';")$n[1], 1L)
      expect_equal(db_query(
        "SELECT COUNT(*) n FROM student_grades WHERE assignment_name='Policy Presentation';")$n[1], 0L)

      session$setInputs(release_policy_rubric_btn=1)
      session$flushReact()

      released <- db_query(
        "SELECT final_score FROM policy_rubric_assessments WHERE status='released';")
      expect_equal(nrow(released), 1L)
      expect_equal(released$final_score[1], 89.6)
      expect_equal(db_query(
        "SELECT COUNT(*) n FROM policy_rubric_assessments WHERE status='draft';")$n[1], 0L)
      expect_equal(db_query(
        "SELECT COUNT(*) n FROM policy_rubric_scores
         WHERE assessment_id=(SELECT id FROM policy_rubric_assessments WHERE status='released');")$n[1], 6L)
      grades <- db_query(
        "SELECT user_id,grade_pct FROM student_grades
         WHERE assignment_name='Policy Presentation' ORDER BY user_id;")
      expect_equal(grades$user_id, c("rubric-alice","rubric-bob"))
      expect_equal(grades$grade_pct, c(89.6,89.6))
    })
  })
})

test_that("active lecture can move backward without deleting rounds", {
  with_app_env({
    app <- suppressWarnings(source_app())
    on.exit(suppressWarnings(try(
      if (!is.null(app$conn) && DBI::dbIsValid(app$conn)) DBI::dbDisconnect(app$conn),
      silent = TRUE)), add = TRUE)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
    on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

    old_rid <- DBI::dbGetQuery(con, "SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;")$id[1]
    post_id <- DBI::dbGetQuery(con, "SELECT id FROM job_posts WHERE round_id=? ORDER BY id LIMIT 1;",
                               params = list(old_rid))$id[1]
    DBI::dbExecute(con,
      "INSERT INTO users(user_id,display_name,is_admin,active,is_demo) VALUES(?,?,0,1,0);",
      params = list("today-test", "Today Test"))
    DBI::dbExecute(con,
      "INSERT INTO job_assignments(round_id,user_id,job_post_id,status,outcome,display_on_today) VALUES(?,?,?,\"assigned\",\"\",1);",
      params = list(old_rid, "today-test", post_id))
    DBI::dbExecute(con, "INSERT INTO weekly_rounds(label,assignment_mode) VALUES(?,?);",
                   params = list("Future bidding cycle", "wage_bidding"))
    new_rid <- DBI::dbGetQuery(con, "SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;")$id[1]

    expect_gt(new_rid, old_rid)
    expect_equal(app$active_round_id(), new_rid)
    expect_equal(app$previous_round_id(new_rid), old_rid)
    expect_true(app$set_active_round_id(old_rid))
    expect_equal(app$active_round_id(), old_rid)
    expect_equal(app$active_round_row()$id[1], old_rid)
    expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM weekly_rounds;")$n[1], 2L)
  })
})


test_that("class-job-market migrates an older live DB schema on startup", {
  with_app_env({
    local({
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
      on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

      DBI::dbExecute(con, "CREATE TABLE users(user_id TEXT PRIMARY KEY, display_name TEXT, is_admin INTEGER DEFAULT 0);")
      DBI::dbExecute(con, "CREATE TABLE arcade_state(id INTEGER PRIMARY KEY CHECK(id=1), active_game TEXT, updated_at TEXT);")
      DBI::dbExecute(con, "INSERT INTO arcade_state(id, active_game) VALUES(1, NULL);")
      DBI::dbExecute(con, "CREATE TABLE live_score_events(id INTEGER PRIMARY KEY AUTOINCREMENT);")
      DBI::dbExecute(con, "CREATE TABLE weekly_rounds(id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT, section TEXT, status TEXT DEFAULT 'open', created_at TEXT DEFAULT CURRENT_TIMESTAMP);")
      DBI::dbExecute(con, "CREATE TABLE job_categories(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, slots INTEGER DEFAULT 1, wage REAL DEFAULT 0, section TEXT, active INTEGER DEFAULT 1);")
      DBI::dbExecute(con, "INSERT INTO job_categories(id, name, wage) VALUES(1, 'Note Taker', 7);")
      DBI::dbExecute(con, "CREATE TABLE job_posts(id INTEGER PRIMARY KEY AUTOINCREMENT, round_id INTEGER, category_id INTEGER, slots INTEGER DEFAULT 1, wage REAL DEFAULT 0);")
      DBI::dbExecute(con, "INSERT INTO job_posts(id, round_id, category_id, wage) VALUES(1, 1, 1, 6);")
      DBI::dbExecute(con, "CREATE TABLE wage_bids(id INTEGER PRIMARY KEY AUTOINCREMENT, round_id INTEGER, user_id TEXT, category_id INTEGER, wage REAL, created_at TEXT DEFAULT CURRENT_TIMESTAMP);")
      DBI::dbExecute(con, "INSERT INTO wage_bids(round_id, user_id, category_id, wage, created_at) VALUES(1, 'alice', 1, 5, '2026-08-01 09:00:00');")
      DBI::dbExecute(con, "CREATE TABLE application_bids(id INTEGER PRIMARY KEY AUTOINCREMENT, round_id INTEGER, user_id TEXT, category_id INTEGER, rank INTEGER, created_at TEXT DEFAULT CURRENT_TIMESTAMP);")
      DBI::dbExecute(con, "CREATE TABLE job_assignments(id INTEGER PRIMARY KEY AUTOINCREMENT, round_id INTEGER, user_id TEXT, category_id INTEGER, wage REAL, tokens REAL DEFAULT 0, outcome TEXT, awarded_ledger_id INTEGER, created_at TEXT DEFAULT CURRENT_TIMESTAMP, UNIQUE(round_id,user_id));")
      DBI::dbExecute(con, "INSERT INTO job_assignments(round_id, user_id, category_id, wage, tokens) VALUES(1, 'alice', 1, 6, 4);")
    })

    expect_error(suppressWarnings(source_app()), NA)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
    on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

    expect_true(all(c("pw_hash", "course", "section", "active", "is_demo") %in% cols(con, "users")))
    expect_true("assignments_revealed" %in% cols(con, "arcade_state"))
    expect_true("bidding_enabled" %in% cols(con, "weekly_rounds"))
    expect_true(all(c("round_id", "user_id", "job_assignment_id", "job_post_id", "event_kind", "outcome", "tokens", "logged_by", "committed_at", "created_at") %in% cols(con, "live_score_events")))
    expect_true(all(c("default_wage", "description", "voluntary", "in_draw") %in% cols(con, "job_categories")))
    expect_true(all(c("job_name", "wage_override", "active", "display_order", "selection_time", "description") %in% cols(con, "job_posts")))
    expect_true("description" %in% cols(con, "job_templates"))
    expect_true(all(c("assigned_wage", "tokens_awarded", "tokens_credited", "status",
                      "scheduled_date", "display_on_today") %in% cols(con, "job_assignments")))
    expect_true(all(c("min_wage", "submitted_at") %in% cols(con, "wage_bids")))
    expect_true(all(c("round_id", "job_post_id", "user_id", "min_wage", "submitted_at") %in%
                      cols(con, "job_wage_bids")))
    expect_true("tickets" %in% cols(con, "application_bids"))

    cat_row <- DBI::dbGetQuery(con, "SELECT default_wage FROM job_categories WHERE id=1;")
    post_row <- DBI::dbGetQuery(con, "SELECT job_name, wage_override FROM job_posts WHERE id=1;")
    bid_row <- DBI::dbGetQuery(con, "SELECT min_wage, submitted_at FROM wage_bids WHERE user_id='alice';")
    assign_row <- DBI::dbGetQuery(con, "SELECT assigned_wage, tokens_awarded, status FROM job_assignments WHERE user_id='alice';")

    expect_equal(cat_row$default_wage[1], 7)
    expect_equal(post_row$job_name[1], "Note Taker")
    expect_equal(post_row$wage_override[1], 6)
    expect_equal(bid_row$min_wage[1], 5)
    expect_equal(bid_row$submitted_at[1], "2026-08-01 09:00:00")
    expect_equal(assign_row$assigned_wage[1], 6)
    expect_equal(assign_row$tokens_awarded[1], 4)
    expect_equal(assign_row$status[1], "assigned")

    migrated_post <- DBI::dbGetQuery(con,
      "SELECT id, round_id FROM job_posts WHERE job_name IS NOT NULL ORDER BY id DESC LIMIT 1;")
    DBI::dbExecute(con,
      "INSERT INTO job_assignments(round_id,user_id,job_post_id,status) VALUES(?,?,?,\x27assigned\x27);",
      params = list(migrated_post$round_id[1], "alice", migrated_post$id[1]))
    expect_gte(DBI::dbGetQuery(con,
      "SELECT COUNT(*) n FROM job_assignments WHERE user_id=\x27alice\x27;")$n[1], 2L)
  })
})

test_that("fresh semester reset script requires explicit confirmation", {
  script <- file.path(repo_root, "tests", "setup", "fresh_semester_db.R")
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), script, stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("Refusing to reset without --yes", out, fixed = TRUE)))
})
