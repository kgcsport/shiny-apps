#!/usr/bin/env Rscript
# Regression coverage for the saved wage-bidding draw (Draw Jobs) in Class Job
# Market. Historically the saved draw ignored the class date's wage pricing rule
# and one-job-per-student setting (only Preview applied them). A per-course /
# section switch, off by default, makes the saved draw apply them:
#   - switch off: the saved draw is exactly as before (pay as bid, multiple jobs);
#   - switch on: a uniform-second-price date pays every winner the lowest losing
#     bid, a one-job-per-student date gives no student two jobs, and a
#     pay-as-bid date is unchanged.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("saved-draw-")
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
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Saved draw role',4,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
class_day <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = app$get_setting("class_tz", "America/New_York")))

# Each date: two jobs with two slots each; three students bid 1, 2, 3 on both
# jobs and three students do not bid.
bid_values <- c(1, 2, 3)
offset <- 0L
new_date <- function(rule, allow_multiple) {
  offset <<- offset + 1L
  app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled,wage_pricing_rule,allow_multiple_jobs)
               VALUES(?,?,'wage_bidding',1,?,?);",
              list(paste("Date", offset), as.character(class_day + offset), rule, as.integer(allow_multiple)))
  rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
  app$db_exec("DELETE FROM scope_active_rounds;")
  app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
  for (job in c("Recap", "Notes")) {
    app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
                 VALUES(?,?,?,2,4,0,1,'start',1);", list(rid, job, role_cat))
    post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
    for (i in seq_along(bid_values))
      app$db_exec("INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,?);",
                  list(rid, post, students[i], bid_values[i]))
  }
  rid
}
set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}
run_draw <- function() shiny::testServer(app$server, {
  set_admin(rv); session$setInputs(draw_timing_filter = "all", run_draw_btn = 1L)
})
set_switch <- function(on) shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(vol_clearing_rule_sel = "lowest", draw_uses_date_settings_input = on,
                    save_vol_clearing_btn = 1L)
  stopifnot(identical(saved_draw_uses_date_settings_for_scope(), on))
})
assignments <- function(rid) app$db_query(
  "SELECT user_id, job_post_id, assigned_wage FROM job_assignments
   WHERE round_id=? ORDER BY job_post_id, user_id;", list(rid))
# The historical saved draw: both jobs go to the two lowest bidders, each paid
# their own bid.
is_legacy_draw <- function(got)
  nrow(got) == 4L && length(unique(got$job_post_id)) == 2L &&
    identical(got$user_id, rep(students[1:2], 2)) && identical(got$assigned_wage, c(1, 2, 1, 2))

# 1. Default (switch off): the saved draw is exactly as it has always been,
#    whatever the class-date settings say.
shiny::testServer(app$server, { set_admin(rv); stopifnot(!saved_draw_uses_date_settings_for_scope()) })
for (rule in c("pay_as_bid", "uniform_second_price")) for (multi in c(TRUE, FALSE)) {
  rid <- new_date(rule, multi)
  run_draw()
  stopifnot(is_legacy_draw(assignments(rid)))
}

# 2. The instructor turns the switch on for this course / section only.
set_switch(TRUE)
stopifnot(identical(app$get_setting("draw_uses_date_settings::econ 101::01"), "1"),
          is.null(app$get_setting("draw_uses_date_settings")))

# 2a. Uniform second price, multiple jobs allowed: the two lowest bidders win
#     both jobs and every winner is paid the lowest losing bid (3).
rid <- new_date("uniform_second_price", TRUE)
run_draw()
got <- assignments(rid)
stopifnot(nrow(got) == 4L, identical(got$user_id, rep(students[1:2], 2)), all(got$assigned_wage == 3))

# 2b. Uniform second price, one job per student (the reported case): nobody
#     holds two jobs, the two lowest bidders share the first job cleared and
#     are both paid the lowest losing bid, and each job pays one wage.
for (i in 1:10) {
  rid <- new_date("uniform_second_price", FALSE)
  run_draw()
  got <- assignments(rid)
  low <- got[got$user_id %in% students[1:2], , drop = FALSE]
  stopifnot(nrow(got) == 4L, !anyDuplicated(got$user_id),
            nrow(low) == 2L, length(unique(low$job_post_id)) == 1L, all(low$assigned_wage == 3),
            all(tapply(got$assigned_wage, got$job_post_id, function(w) length(unique(w))) == 1L),
            all(got$assigned_wage >= 3))
}

# 2c. Pay as bid, one job per student: nobody holds two jobs and bidders are
#     still paid their own bid.
for (i in 1:10) {
  rid <- new_date("pay_as_bid", FALSE)
  run_draw()
  got <- assignments(rid)
  stopifnot(nrow(got) == 4L, !anyDuplicated(got$user_id),
            got$assigned_wage[got$user_id == students[1]] == 1,
            got$assigned_wage[got$user_id == students[2]] == 2,
            got$assigned_wage[got$user_id == students[3]] == 3)
}

# 2d. Pay as bid, multiple jobs allowed: unchanged by the switch.
rid <- new_date("pay_as_bid", TRUE)
run_draw()
stopifnot(is_legacy_draw(assignments(rid)))

# 3. Turning the switch off restores the historical saved draw.
set_switch(FALSE)
rid <- new_date("uniform_second_price", FALSE)
run_draw()
stopifnot(is_legacy_draw(assignments(rid)))

cat("critical saved draw date settings: PASS\n")
