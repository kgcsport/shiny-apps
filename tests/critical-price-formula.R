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
# A student's session cannot apply it.
app$set_setting("reweight_cost_schedule", "1:2")
shiny::testServer(app$server, {
  rv$authed <- TRUE; rv$user_id <- "s@test.edu"; rv$is_admin <- FALSE; rv$course <- "ECON 101"; rv$section <- "01"
  rv$active_course <- "ECON 101"; rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
  session$setInputs(apply_price_formula_btn = 1L)
})
stopifnot(identical(app$get_setting("reweight_cost_schedule"), "1:2"))

cat("critical price formula: PASS\n")
