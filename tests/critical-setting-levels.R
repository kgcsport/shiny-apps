#!/usr/bin/env Rscript
# Regression coverage for settings at three levels in Class Job Market. A
# setting is read for the section first, then the course, then app-wide, and
# is saved at the level the instructor has chosen. Until a more specific value
# is saved, everything behaves as it did when settings were app-wide.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("setting-levels-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)

app <- new.env(parent = globalenv())
source(app_path, local = app)
for (tbl in c("scope_active_rounds", "scope_rounds", "section_scope_memberships", "weekly_rounds", "job_posts"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
app$db_exec("DELETE FROM labor_settings WHERE key LIKE '%@course:%' OR key LIKE '%@section:%' OR key LIKE '%::%';")
app$set_setting("class_days", ""); app$set_setting("bid_lock_enabled", "1")
app$set_setting("bid_reopen_time", "17:00"); app$set_setting("volunteer_clearing_rule", "lowest")
people <- list(c("admin@test.edu", "ECON 101", "01", 1L), c("a1@test.edu", "ECON 101", "01", 0L),
               c("a2@test.edu", "ECON 101", "02", 0L), c("b1@test.edu", "ECON 202", "01", 0L))
for (person in people) {
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
               VALUES(?,?,'',?,?,?,1,0);", list(person[[1]], person[[1]], as.integer(person[[4]]), person[[2]], person[[3]]))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,?,?);", list(person[[1]], person[[2]], person[[3]]))
}

# 1. The rule itself, with no session: an explicit context stands in for one.
ctx <- function(course, sections, level = "auto") list(course = course, sections = sections,
                                                      scope = paste0(course, "::", paste(sections, collapse = "|")), level = level)
read <- function(key, course, sections, with_scope = FALSE)
  app$with_setting_context(ctx(course, sections), app$resolve_setting(key, "built-in", with_scope))
stopifnot(identical(read("probe", "econ 101", "01"), list(value = "built-in", source = "default")))
app$set_setting("probe", "everyone")                       # no context: app-wide
stopifnot(identical(read("probe", "econ 101", "01"), list(value = "everyone", source = "app")),
          identical(app$get_setting("probe"), "everyone"))
app$with_setting_context(ctx("econ 101", "01", "course"), app$set_setting("probe", "course 101"))
app$with_setting_context(ctx("econ 101", "01", "section"), app$set_setting("probe", "section 01"))
stopifnot(identical(read("probe", "econ 101", "01"), list(value = "section 01", source = "section")),
          identical(read("probe", "econ 101", "02"), list(value = "course 101", source = "course")),
          identical(read("probe", "econ 202", "01"), list(value = "everyone", source = "app")),
          # with no context, as at startup or in a script, only the app-wide value exists
          identical(app$get_setting("probe"), "everyone"),
          # a pooled pair of sections has no single section, so the course value applies
          identical(read("probe", "econ 101", c("01", "02"))$source, "course"))
# Saving for several sections at once writes each of them.
app$with_setting_context(ctx("econ 202", c("01", "02"), "section"), app$set_setting("probe", "202 sections"))
stopifnot(identical(read("probe", "econ 202", "01")$value, "202 sections"), identical(read("probe", "econ 202", "02")$value, "202 sections"),
          identical(read("probe", "econ 202", "03")$value, "everyone"))
# The older per-scope form sits between section and course.
app$raw_set_setting("probe::econ 101::02", "scope 02")
stopifnot(identical(read("probe", "econ 101", "02", with_scope = TRUE), list(value = "scope 02", source = "scope")),
          identical(read("probe", "econ 101", "02")$value, "course 101"),
          identical(read("probe", "econ 101", "01", with_scope = TRUE)$source, "section"))
# App bookkeeping never moves to a course or section, whatever level is chosen.
app$with_setting_context(ctx("econ 101", "01", "section"), {
  app$set_setting("active_round_id", "7")
  app$set_setting("already::qualified", "x")
})
stopifnot(identical(app$raw_get_setting("active_round_id"), "7"), identical(app$raw_get_setting("already::qualified"), "x"),
          nrow(app$db_query("SELECT key FROM labor_settings WHERE key LIKE 'active_round_id@%' OR key LIKE 'already::qualified@%';")) == 0L)
app$db_exec("DELETE FROM labor_settings WHERE key LIKE 'probe%' OR key LIKE 'already%';")

# 2. In the app. Sessions for an instructor and for students in three places.
set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}
as_student <- function(uid, course, section, expr) {
  expr <- substitute(expr); out <- NULL
  shiny::testServer(app$server, {
    rv$authed <- TRUE; rv$user_id <- uid; rv$name <- uid
    rv$course <- course; rv$section <- section; rv$is_admin <- FALSE
    rv$active_course <- course; rv$active_section <- section
    rv$active_sections <- section; rv$is_demo <- FALSE
    out <<- eval(expr)
  })
  out
}
sees <- function() c(a1 = as_student("a1@test.edu", "ECON 101", "01", app$get_setting("volunteer_clearing_rule")),
                     a2 = as_student("a2@test.edu", "ECON 101", "02", app$get_setting("volunteer_clearing_rule")),
                     b1 = as_student("b1@test.edu", "ECON 202", "01", app$get_setting("volunteer_clearing_rule")))
reopens <- function() c(a1 = as_student("a1@test.edu", "ECON 101", "01", app$bid_lock_status()$reopen_at),
                        a2 = as_student("a2@test.edu", "ECON 101", "02", app$bid_lock_status()$reopen_at),
                        b1 = as_student("b1@test.edu", "ECON 202", "01", app$bid_lock_status()$reopen_at))
stopifnot(all(sees() == "lowest"), all(reopens() == "5:00 PM"))

# Unchanged default: with the selector left alone, a save is app-wide.
shiny::testServer(app$server, {
  set_admin(rv)
  stopifnot(grepl("Save settings for", output$setting_level_panel$html, fixed = TRUE))
  session$setInputs(vol_clearing_rule_sel = "demand", save_vol_clearing_btn = 1L)
})
stopifnot(all(sees() == "demand"), identical(app$raw_get_setting("volunteer_clearing_rule"), "demand"),
          nrow(app$db_query("SELECT key FROM labor_settings WHERE key LIKE '%@course:%' OR key LIKE '%@section:%';")) == 0L)

# Saved for one section: only that section changes.
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(setting_level_sel = "section")
  session$setInputs(vol_clearing_rule_sel = "posted", save_vol_clearing_btn = 1L)
  stopifnot(grepl("1 setting saved for this course", output$setting_level_panel$html, fixed = TRUE))
})
stopifnot(identical(sees(), c(a1 = "posted", a2 = "demand", b1 = "demand")),
          identical(app$raw_get_setting("volunteer_clearing_rule"), "demand"),
          identical(app$raw_get_setting("volunteer_clearing_rule@section:econ 101/01"), "posted"))

# Saved for the course: both of its sections follow, except where a section
# has its own value. A helper that reads settings internally follows too.
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(setting_level_sel = "course")
  session$setInputs(vol_clearing_rule_sel = "lowest", save_vol_clearing_btn = 1L)
  app$set_setting("bid_reopen_time", "20:00")
})
stopifnot(identical(sees(), c(a1 = "posted", a2 = "lowest", b1 = "demand")),
          identical(reopens(), c(a1 = "8:00 PM", a2 = "8:00 PM", b1 = "5:00 PM")),
          identical(app$raw_get_setting("bid_reopen_time"), "17:00"))

# Saving a whole form for one section stores only the fields that differ from
# what the section would follow anyway, and setting a field back to the course
# value removes the stored value so the section follows the course again.
stored_for_section <- function() sort(app$db_query(
  "SELECT key FROM labor_settings WHERE key LIKE '%@section:econ 101/01';")$key)
stopifnot(identical(stored_for_section(), "volunteer_clearing_rule@section:econ 101/01"))
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(setting_level_sel = "section")
  # unchanged rule and switches, saved again through the same form
  session$setInputs(vol_clearing_rule_sel = "posted", volunteer_paid_cap_input = FALSE, bid_no_anchor_input = FALSE,
                    save_vol_clearing_btn = 1L)
})
stopifnot(identical(stored_for_section(), "volunteer_clearing_rule@section:econ 101/01"))
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(setting_level_sel = "section")
  session$setInputs(vol_clearing_rule_sel = "lowest", save_vol_clearing_btn = 1L)   # the course's value
})
stopifnot(length(stored_for_section()) == 0L, identical(sees(), c(a1 = "lowest", a2 = "lowest", b1 = "demand")))
app$raw_set_setting("volunteer_clearing_rule@section:econ 101/01", "posted")       # put it back for the next step

# Removing the section's own value lets the course value through. A value that
# belongs to another course cannot be removed from here.
app$raw_set_setting("volunteer_clearing_rule@course:econ 202", "demand")
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(clear_setting_override = "volunteer_clearing_rule@course:econ 202")
  session$setInputs(clear_setting_override = "volunteer_clearing_rule@section:econ 101/01")
})
stopifnot(identical(sees(), c(a1 = "lowest", a2 = "lowest", b1 = "demand")),
          identical(app$raw_get_setting("volunteer_clearing_rule@course:econ 202"), "demand"))

# Settings that were always per course/section still save there by default,
# and can now also be set for a whole course.
capped <- function(uid, course, section) as_student(uid, course, section, volunteer_pay_capped_for_scope())
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(vol_clearing_rule_sel = "lowest", volunteer_paid_cap_input = TRUE, save_vol_clearing_btn = 1L)
})
stopifnot(identical(app$raw_get_setting("volunteer_paid_cap::econ 101::01"), "1"),
          capped("a1@test.edu", "ECON 101", "01"), !capped("a2@test.edu", "ECON 101", "02"), !capped("b1@test.edu", "ECON 202", "01"))
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(setting_level_sel = "course")
  session$setInputs(vol_clearing_rule_sel = "lowest", bid_no_anchor_input = TRUE, save_vol_clearing_btn = 1L)
})
hidden <- function(uid, course, section) as_student(uid, course, section, bid_anchor_hidden_for_scope())
stopifnot(hidden("a1@test.edu", "ECON 101", "01"), hidden("a2@test.edu", "ECON 101", "02"), !hidden("b1@test.edu", "ECON 202", "01"))

# 3. A draw locks bidding for its own course/section, not for every class.
app$db_exec("DELETE FROM labor_settings WHERE key LIKE 'bid_draw_locked_until%';")
app$db_exec("INSERT INTO weekly_rounds(id,label,class_date,assignment_mode,bidding_enabled) VALUES(50,'A','2026-12-01','wage_bidding',1);")
app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES('econ 101::01',50);")
app$close_bidding_after_draw(50L)
locked <- function(uid, course, section) as_student(uid, course, section, app$bid_lock_status()$locked)
stopifnot(locked("a1@test.edu", "ECON 101", "01"), !locked("a2@test.edu", "ECON 101", "02"), !locked("b1@test.edu", "ECON 202", "01"),
          is.null(app$raw_get_setting("bid_draw_locked_until")))

cat("critical setting levels: PASS\n")
