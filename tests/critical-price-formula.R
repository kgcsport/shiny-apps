#!/usr/bin/env Rscript
# One formula for perk prices: price = rate x percentage points of course grade.
# Nothing changes for a class until the instructor presses Apply.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("price-formula-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)
app <- new.env(parent = globalenv())
source(app_path, local = app)
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9))

# ── The arithmetic, with the registered inputs ───────────────────────────────
f <- app$price_formula(app$PRICE_FORMULA_DEFAULTS, class_size = 25)
stopifnot(near(f$rate, 10),                              # 40 points buy 4 grade points
          near(f$extension_per_hour, 0.5),               # 15% x 2/600 = 0.05 points of grade per hour
          near(f$extension$base_tokens, 12), f$extension$exponent == 1,
          near(f$reweight_per_point, 2),
          nrow(f$questions) == 24L, near(f$questions$chance[1], 0.5), near(f$questions$chance[24], 6 / 35),
          # First reveal: 25 students x 25% x 0.20 / 10 x 0.5 expected appearances x 10 points = 62.5 -> 63.
          f$questions$section_price[1] == 63L, f$questions$section_price[24] == 7L,
          all(diff(f$questions$section_price) <= 0), all(diff(f$questions$chance) < 0),
          all(diff(f$questions$known_on_exam) > 0), f$questions$known_on_exam[24] < 4.2)
# The formulas handed to the app charge exactly the table's prices.
by_formula <- vapply(1:24, function(k) app$question_cost_for_n(k, f$question_formula, 25L), integer(1))
stopifnot(identical(as.integer(by_formula), as.integer(f$questions$section_price)),
          app$question_cost_for_n(1, f$question_formula, 50L) == 125L,          # scales with class size
          near(app$rw_cost_for_n(5, f$reweight_formula), 10), near(app$rw_cost_for_n(1, f$reweight_formula), 2))
# Changing an input moves every price in step.
double_rate <- app$PRICE_FORMULA_DEFAULTS; double_rate$grade_points_a_term_buys <- 2
g <- app$price_formula(double_rate, 25)
stopifnot(near(g$rate, 20), near(g$extension_per_hour, 1), near(g$reweight_per_point, 4), g$questions$section_price[1] == 125L)

# ── Until applied, the class's prices are what they were ─────────────────────
before <- list(ext = app$extension_pricing_settings(), flex = app$get_setting("flex_cost_schedule"),
               rw = app$get_setting("reweight_cost_schedule"))
stopifnot(before$ext$exponent > 1, app$extension_cost_for_hours(24) == 3)
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES('admin@test.edu','Instructor','',1,'ECON 101','01',1,0);")
for (uid in c("s@test.edu", "t@test.edu")) {
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES(?,?,'',0,'ECON 101','01',1,0);", list(uid, uid))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');", list(uid))
}
app$db_exec("INSERT OR IGNORE INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01','econ 101::01');")
set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"; rv$course <- "ECON 101"; rv$section <- "01"
  rv$is_admin <- TRUE; rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"; rv$active_section <- "01"
  rv$active_sections <- "01"; rv$impersonating <- FALSE
}
shiny::testServer(app$server, {
  set_admin(rv)
  html <- paste(as.character(output$price_formula_panel$html), collapse = "")
  preview <- paste(as.character(output$price_formula_preview$html), collapse = "")
  stopifnot(grepl("apply_price_formula_btn", html, fixed = TRUE), grepl("pf_final_weight", html, fixed = TRUE),
            grepl("Rate: 10 points", preview, fixed = TRUE), !grepl("pf_final_weight", preview, fixed = TRUE))
  # Typing a number redraws the preview, not the boxes.
  session$setInputs(pf_grade_points_a_term_buys = 2)
  stopifnot(grepl("Rate: 20 points", paste(as.character(output$price_formula_preview$html), collapse = ""), fixed = TRUE))
})
stopifnot(identical(app$extension_pricing_settings(), before$ext), identical(app$get_setting("flex_cost_schedule"), before$flex))

# ── Apply: the three existing price settings now follow the formula ──────────
shiny::testServer(app$server, {
  set_admin(rv)
  inputs <- stats::setNames(as.list(unlist(app$PRICE_FORMULA_DEFAULTS)), paste0("pf_", names(app$PRICE_FORMULA_DEFAULTS)))
  do.call(session$setInputs, inputs)
  session$setInputs(apply_price_formula_btn = 1L)
})
after <- app$with_setting_context(list(course = "econ 101", sections = "01", scope = "econ 101::01", level = "auto"),
  list(ext = app$extension_pricing_settings(), flex = app$resolve_setting("flex_cost_schedule", "", with_scope = TRUE)$value,
       rw = app$get_setting("reweight_cost_schedule"), params = app$price_formula_params(),
       day = app$extension_cost_for_hours(24), six = app$extension_cost_for_hours(6), one = app$extension_cost_for_hours(1)))
stopifnot(after$ext$exponent == 1, near(after$ext$base_tokens, 12), after$day == 12, after$six == 3, after$one == 1,
          identical(after$flex, f$question_formula), identical(after$rw, f$reweight_formula),
          near(after$params$final_weight, 25))
# ── Reweighting priced by each student's own gain ────────────────────────────
for (tbl in c("grade_reweight_requests", "student_grades", "gradebook_item_names", "gradebook_categories"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
app$db_exec("INSERT INTO gradebook_categories(id,name,weight,item_count,item_prefix,max_points,source,display_order) VALUES(101,'Problem Sets',50,2,'PS',100,'manual',1);")
app$db_exec("INSERT INTO gradebook_categories(id,name,weight,item_count,item_prefix,max_points,source,display_order) VALUES(102,'Exams',50,2,'Exam',100,'manual',2);")
app$db_exec("INSERT INTO gradebook_item_names(category_id,item_index,item_name,item_weight) VALUES(101,1,'PS1',25),(101,2,'PS2',25),(102,1,'Exam1',25),(102,2,'Exam2',25);")
grades <- function(uid, ps, exam) app$db_exec(
  "INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct) VALUES(?,'PS1',?,100,?),(?,'PS2',?,100,?),(?,'Exam1',?,100,?),(?,'Exam2',?,100,?);",
  list(uid, ps, ps, uid, ps, ps, uid, exam, exam, uid, exam, exam))
grades("s@test.edu", 100, 70)     # uneven: 30-point gap
grades("t@test.edu", 80, 80)      # even
in_class <- function(expr) app$with_setting_context(list(course = "econ 101", sections = "01", scope = "econ 101::01", level = "auto"), expr)
cost <- function(uid, from, to) in_class(app$rw_cost_for_request(uid, 5, from = from, to = to))
# Off (the default): everyone pays the standard price, 2 points per point of weight.
stopifnot(!in_class(app$reweight_priced_by_gain()), cost("s@test.edu", "Exams", "Problem Sets") == 10, cost("t@test.edu", "Exams", "Problem Sets") == 10)
shiny::testServer(app$server, {
  set_admin(rv)
  do.call(session$setInputs, stats::setNames(as.list(unlist(app$PRICE_FORMULA_DEFAULTS)), paste0("pf_", names(app$PRICE_FORMULA_DEFAULTS))))
  session$setInputs(pf_reweight_by_gain = TRUE, pf_question_pool = "section", apply_price_formula_btn = 2L)
})
# On: moving 5 points of weight across a 30-point gap is worth 1.5 grade points,
# so it costs 15. An even student, or a move that does not help, pays the
# standard 10. The standard price is always the floor.
stopifnot(in_class(app$reweight_priced_by_gain()), near(in_class(app$rw_gain_for_request("s@test.edu", "Exams", "Problem Sets", 5)), 1.5),
          cost("s@test.edu", "Exams", "Problem Sets") == 15, cost("t@test.edu", "Exams", "Problem Sets") == 10,
          cost("s@test.edu", "Problem Sets", "Exams") == 10, in_class(app$rw_cost_for_request("s@test.edu", 5)) == 10)
# The student is shown, and charged, their own price.
app$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES('s@test.edu','s','job',40,1,'fixture');")
shiny::testServer(app$server, {
  rv$authed <- TRUE; rv$user_id <- "s@test.edu"; rv$name <- "s"; rv$is_admin <- FALSE; rv$course <- "ECON 101"; rv$section <- "01"
  rv$active_course <- "ECON 101"; rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
  session$setInputs(rw_points = 5L, rw_from = "Exams", rw_to = "Problem Sets", rw_level = "category")
  stopifnot(grepl("This request: 15 tokens", paste(as.character(output$rw_cost_preview$html), collapse = ""), fixed = TRUE))
})

# ── Exam questions pooled by section or across the course ────────────────────
for (uid in c("u@test.edu", "v@test.edu", "w@test.edu")) {
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES(?,?,'',0,'ECON 101','02',1,0);", list(uid, uid))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','02');", list(uid))
}
pool_seen <- new.env()
look <- function(tag) shiny::testServer(app$server, {
  set_admin(rv)
  assign(tag, list(course = flex_pool_is_course(), key = flex_fund_key(), size = flex_roster_size()), envir = pool_seen)
})
look("section")
stopifnot(!pool_seen$section$course, pool_seen$section$key == "econ 101::01", pool_seen$section$size == 2L)
shiny::testServer(app$server, {
  set_admin(rv)
  do.call(session$setInputs, stats::setNames(as.list(unlist(app$PRICE_FORMULA_DEFAULTS)), paste0("pf_", names(app$PRICE_FORMULA_DEFAULTS))))
  session$setInputs(pf_reweight_by_gain = TRUE, pf_question_pool = "course", apply_price_formula_btn = 3L)
})
look("course")
# Course-wide: one pool for both sections, priced for all five students.
stopifnot(pool_seen$course$course, pool_seen$course$key == "econ 101::course-pool", pool_seen$course$size == 5L,
          app$question_cost_for_n(1, f$question_formula, 5L) == 13L, app$question_cost_for_n(1, f$question_formula, 2L) == 5L)

# A student's session cannot apply it.
app$set_setting("reweight_cost_schedule", "1:2")
shiny::testServer(app$server, {
  rv$authed <- TRUE; rv$user_id <- "s@test.edu"; rv$is_admin <- FALSE; rv$course <- "ECON 101"; rv$section <- "01"
  rv$active_course <- "ECON 101"; rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
  session$setInputs(apply_price_formula_btn = 1L)
})
stopifnot(identical(app$get_setting("reweight_cost_schedule"), "1:2"))

cat("critical price formula: PASS\n")
