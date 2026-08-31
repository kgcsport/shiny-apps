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
    expect_error(suppressWarnings(source_app()), NA)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
    on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

    expect_true(file.exists(db_path()))
    expect_true(all(c("user_id", "display_name", "pw_hash", "section", "active", "is_demo") %in% cols(con, "users")))
    expect_true("assignments_revealed" %in% cols(con, "arcade_state"))
    expect_true(all(c("tokens_awarded", "tokens_credited", "status", "job_post_id") %in% cols(con, "job_assignments")))
    expect_true(all(c("job_post_id", "event_kind", "tokens", "committed_at") %in% cols(con, "live_score_events")))

    round <- DBI::dbGetQuery(con, "SELECT label, tokens_revealed FROM weekly_rounds ORDER BY id DESC LIMIT 1;")
    posts <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM job_posts;")
    cold_posts <- DBI::dbGetQuery(con, "
      SELECT COUNT(*) n
      FROM job_posts
      WHERE COALESCE(active,1)=1
        AND COALESCE(in_draw,1)=1
        AND selection_time='during'
        AND job_name LIKE 'Cold call:%';")
    expect_equal(round$label[1], "Current Class")
    expect_equal(round$tokens_revealed[1], 0)
    expect_gt(posts$n[1], 0)
    expect_gt(cold_posts$n[1], 0)
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
      DBI::dbExecute(con, "CREATE TABLE job_assignments(id INTEGER PRIMARY KEY AUTOINCREMENT, round_id INTEGER, user_id TEXT, category_id INTEGER, wage REAL, tokens REAL DEFAULT 0, outcome TEXT, awarded_ledger_id INTEGER, created_at TEXT DEFAULT CURRENT_TIMESTAMP);")
      DBI::dbExecute(con, "INSERT INTO job_assignments(round_id, user_id, category_id, wage, tokens) VALUES(1, 'alice', 1, 6, 4);")
    })

    expect_error(suppressWarnings(source_app()), NA)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
    on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

    expect_true(all(c("pw_hash", "section", "active", "is_demo") %in% cols(con, "users")))
    expect_true("assignments_revealed" %in% cols(con, "arcade_state"))
    expect_true(all(c("round_id", "user_id", "job_assignment_id", "job_post_id", "event_kind", "outcome", "tokens", "logged_by", "committed_at", "created_at") %in% cols(con, "live_score_events")))
    expect_true(all(c("default_wage", "description", "voluntary", "in_draw") %in% cols(con, "job_categories")))
    expect_true(all(c("job_name", "wage_override", "active", "display_order", "selection_time") %in% cols(con, "job_posts")))
    expect_true(all(c("assigned_wage", "tokens_awarded", "tokens_credited", "status") %in% cols(con, "job_assignments")))
    expect_true(all(c("min_wage", "submitted_at") %in% cols(con, "wage_bids")))
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
  })
})

test_that("fresh semester reset script requires explicit confirmation", {
  script <- file.path(repo_root, "tests", "setup", "fresh_semester_db.R")
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), script, stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("Refusing to reset without --yes", out, fixed = TRUE)))
})
