#!/usr/bin/env Rscript
# Candidate questions are funded exam by exam. A database from before that
# keeps every contribution, reveal and locked price; from then on each exam
# has its own next question, progress and price sequence.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("exam-funds-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)
app <- new.env(parent = globalenv())
source(app_path, local = app)
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-8))

# ── A database as it was: one queue of questions, exams as typed text ────────
for (tbl in c("flex_question_contributions", "flex_question_scope_state", "flex_questions", "token_ledger", "user_environments",
              "gradebook_item_names", "gradebook_categories", "section_scope_memberships"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
app$db_exec("DELETE FROM users;")
app$set_setting("flex_cost_schedule", "2,4,6,8,10")
person <- function(uid, course, section, admin = 0L) {
  app$db_exec("INSERT INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES(?,?,?,?,?,1,0);", list(uid, uid, admin, course, section))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,?,?);", list(uid, course, section))
  if (!admin) app$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES(?,?,'fixture',100,1,'');", list(uid, uid))
}
person("prof@test.edu", "ECON 101", "01", admin = 1L)
for (uid in c("a1@test.edu", "a2@test.edu", "a3@test.edu")) person(uid, "ECON 101", "01")
for (uid in c("b1@test.edu", "b2@test.edu")) person(uid, "ECON 101", "02")
person("d1@test.edu", "DATA 210", "01")
gradebook <- function(course, items) {
  app$db_exec("INSERT INTO gradebook_categories(name,weight,item_count,item_prefix,max_points,source,display_order,course) VALUES('Exams',100,?,'Exam',100,'manual',1,?);",
              list(length(items), course))
  id <- app$db_query("SELECT MAX(id) id FROM gradebook_categories;")$id
  for (i in seq_along(items)) app$db_exec("INSERT INTO gradebook_item_names(category_id,item_index,item_name) VALUES(?,?,?);", list(id, i, items[i]))
  id
}
econ_exams <- gradebook("econ 101", c("Midterm", "Final")); gradebook("data 210", "Final")
question <- function(text, exam, course = "ECON 101") {
  app$db_exec("INSERT INTO flex_questions(question_text,order_index,exam_tag,course) VALUES(?,(SELECT COALESCE(MAX(order_index),0)+1 FROM flex_questions),?,?);",
              list(text, exam, course))
  as.integer(app$db_query("SELECT MAX(id) id FROM flex_questions;")$id)
}
f1 <- question("Final one", "Final"); f2 <- question("Final two", "Final"); f3 <- question("Final three", "final ")
quiz <- question("Quiz one", "Old quiz")                        # an exam name that is in no gradebook
m1 <- question("Midterm one", "Midterm"); m2 <- question("Midterm two", "Midterm")
other <- question("Data final", "Final", course = "DATA 210")
s01 <- "econ 101::01"; s02 <- "econ 101::02"
app$db_exec("INSERT INTO flex_question_scope_state(question_id,scope_key,unlock_cost,unlocked_at) VALUES(?,?,2,'2026-10-01 10:00:00');", list(f1, s01))
app$db_exec("INSERT INTO flex_question_scope_state(question_id,scope_key,unlock_cost) VALUES(?,?,9);", list(f2, s01))   # a price locked under an older schedule
app$db_exec("INSERT INTO flex_question_scope_state(question_id,scope_key,unlock_cost) VALUES(?,?,2);", list(f1, s02))
app$db_exec("INSERT INTO flex_question_contributions(question_id,user_id,amount,scope_key) VALUES(?,'a1@test.edu',2,?),(?,'a2@test.edu',4,?),(?,'b1@test.edu',1,?);",
            list(f1, s01, f2, s01, f1, s02))
snapshot <- function() list(
  state = app$db_query("SELECT * FROM flex_question_scope_state ORDER BY question_id, scope_key;"),
  paid = app$db_query("SELECT * FROM flex_question_contributions ORDER BY id;"),
  ledger = app$db_query("SELECT COUNT(*) n, SUM(amount) total FROM token_ledger;"))
before <- snapshot()

# ── Migration ────────────────────────────────────────────────────────────────
app$link_flex_question_exams()
keys <- app$db_query("SELECT id, COALESCE(exam_item_key,'') k FROM flex_questions ORDER BY id;")
stopifnot(identical(keys$k[match(c(f1, f2, f3, quiz, m1, m2, other), keys$id)], c("final", "final", "final", "", "midterm", "midterm", "final")),
          identical(snapshot(), before))                    # no contribution, reveal, price or token moved
app$link_flex_question_exams(); stopifnot(identical(snapshot(), before))

as_student <- function(rv, session, uid, course, section) {
  rv$authed <- TRUE; rv$user_id <- uid; rv$name <- uid; rv$is_admin <- FALSE; rv$course <- course; rv$section <- section
  rv$active_course <- course; rv$active_section <- section; rv$active_sections <- section; rv$impersonating <- FALSE
  session$setInputs(arc_tabs = "Spend"); session$flushReact()
}
as_admin <- function(rv, session, course = "ECON 101", section = "01") {
  as_student(rv, session, "prof@test.edu", course, section); rv$is_admin <- TRUE; rv$admin_role <- "super_admin"; session$flushReact()
}
fund <- function(state, exam) Filter(function(f) identical(f$exam_key, exam), state$funds)[[1]]
seen <- new.env()

# ── Section 01: three funds, each counting its own reveals ───────────────────
shiny::testServer(app$server, {
  as_student(rv, session, "a3@test.edu", "ECON 101", "01")
  st <- flex_question_state()
  stopifnot(identical(vapply(st$funds, function(f) f$exam_key, ""), c("final", "tag:old quiz", "midterm")),
            identical(vapply(st$funds, function(f) f$exam_label, ""), c("Final", "Old quiz", "Midterm")),
            !any(st$questions$question_text == "Data final"))
  final <- fund(st, "final"); mid <- fund(st, "midterm"); old <- fund(st, "tag:old quiz")
  stopifnot(final$unlocked_n == 1L, final$next_question$id == f2, final$target == 9, final$funded == 4,   # the locked price and progress
            mid$unlocked_n == 0L, mid$next_question$id == m1, mid$target == 2,                              # the midterm starts at its first price
            old$next_question$id == quiz, old$target == 2, final$class_size == 3L)
  # The original box pays into the first exam with a question left: the final.
  session$setInputs(flex_contribution = 50, submit_flex_contribution = 1L); session$flushReact()
  st <- flex_question_state(); final <- fund(st, "final")
  stopifnot(final$unlocked_n == 2L, final$next_question$id == f3, final$target == 6,                       # third reveal of this exam
            fund(st, "midterm")$target == 2, fund(st, "midterm")$funded == 0)
  # Another exam's box pays into that exam only.
  session$setInputs(`flex_contribution__midterm` = 1.5, `submit_flex_contribution__midterm` = 1L); session$flushReact()
  st <- flex_question_state()
  stopifnot(fund(st, "midterm")$funded == 1.5, fund(st, "final")$funded == 0, fund(st, "final")$unlocked_n == 2L)
  # A question whose exam is in no gradebook is still funded.
  session$setInputs(`flex_contribution__tagoldquiz` = 2, `submit_flex_contribution__tagoldquiz` = 1L); session$flushReact()
  stopifnot(fund(flex_question_state(), "tag:old quiz")$unlocked_n == 1L)
})
paid <- function(uid) -sum(app$db_query("SELECT amount FROM token_ledger WHERE user_id=? AND source_type='flex_question_contribution';", list(uid))$amount)
stopifnot(near(paid("a3@test.edu"), 5 + 1.5 + 2),         # charged the 5 the final still needed, not the 50 typed
          near(app$db_query("SELECT SUM(amount) s FROM flex_question_contributions WHERE question_id=? AND scope_key=?;", list(f2, s01))$s, 9))
# Paying past the price is not possible: the next student is charged only what is left.
shiny::testServer(app$server, {
  as_student(rv, session, "a1@test.edu", "ECON 101", "01")
  session$setInputs(`flex_contribution__midterm` = 99, `submit_flex_contribution__midterm` = 1L); session$flushReact()
  stopifnot(fund(flex_question_state(), "midterm")$unlocked_n == 1L)
})
stopifnot(near(paid("a1@test.edu"), 0.5),
          near(app$db_query("SELECT SUM(amount) s FROM flex_question_contributions WHERE question_id=? AND scope_key=?;", list(m1, s01))$s, 2))

# A contribution can be as small as a cent, and is saved as typed.
shiny::testServer(app$server, {
  as_student(rv, session, "a2@test.edu", "ECON 101", "01")
  session$setInputs(flex_contribution = 0.37, submit_flex_contribution = 1L); session$flushReact()
  stopifnot(near(fund(flex_question_state(), "final")$funded, 0.37))
})
stopifnot(near(paid("a2@test.edu"), 0.37))

# ── Section 02 saw none of that; another course sees none of it at all ───────
shiny::testServer(app$server, {
  as_student(rv, session, "b2@test.edu", "ECON 101", "02")
  st <- flex_question_state(); final <- fund(st, "final")
  stopifnot(final$unlocked_n == 0L, final$next_question$id == f1, final$target == 2, final$funded == 1, final$class_size == 2L,
            fund(st, "midterm")$unlocked_n == 0L, nrow(st$unlocked) == 0L)
})
shiny::testServer(app$server, {
  as_student(rv, session, "d1@test.edu", "DATA 210", "01")
  st <- flex_question_state()
  stopifnot(length(st$funds) == 1L, st$funds[[1]]$exam_label == "Final", identical(st$questions$question_text, "Data final"))
})

# ── Instructor: an exam's own schedule, a section-only question, uploads ─────
bad <- file.path(scratch, "bad.csv"); good <- file.path(scratch, "good.csv")
write.csv(data.frame(question_text = c("Up one", "Up two"), exam = c("Final", "Midterm 9")), bad, row.names = FALSE)
write.csv(data.frame(question_text = c("Up one", "Up two"), exam = c("final", "Midterm"), section = c("", "01")), good, row.names = FALSE)
shiny::testServer(app$server, {
  as_admin(rv, session, "ECON 101", "01")
  n0 <- app$db_query("SELECT COUNT(*) n FROM flex_questions;")$n
  # A file that names an exam the gradebook does not have saves nothing.
  session$setInputs(upload_flex_questions = list(name = "bad.csv", datapath = bad), upload_flex_questions_btn = 1L); session$flushReact()
  stopifnot(app$db_query("SELECT COUNT(*) n FROM flex_questions;")$n == n0)
  session$setInputs(upload_flex_questions = list(name = "good.csv", datapath = good), upload_flex_questions_btn = 2L); session$flushReact()
  up <- app$db_query("SELECT question_text, exam_tag, exam_item_key, COALESCE(section,'') section FROM flex_questions WHERE question_text LIKE 'Up %' ORDER BY id;")
  stopifnot(identical(up$exam_item_key, c("final", "midterm")), identical(up$exam_tag, c("Final", "Midterm")), identical(up$section, c("", "01")))
  # The midterm gets its own prices; the final keeps the class's.
  session$setInputs(`flex_cost_exam__final` = "", `flex_cost_exam__midterm` = "30,40,50", `flex_cost_exam__tagoldquiz` = "",
                    save_flex_exam_costs_btn = 1L); session$flushReact()
  st <- flex_question_state()
  stopifnot(fund(st, "midterm")$target == 40, fund(st, "final")$target == 6)       # second midterm reveal; third final reveal
  # A question for section 02 only, added by hand.
})
shiny::testServer(app$server, {
  as_admin(rv, session, "ECON 101", "02")
  session$setInputs(new_fq_text = "Only for 02", new_fq_exam = "final", new_fq_section = "02", new_fq_topic = "", new_fq_draw_group = "",
                    new_fq_draw_count = NA, new_fq_pool_size = NA, add_flex_question_btn = 1L); session$flushReact()
  seen$in_02 <- flex_question_state()$questions$question_text
})
shiny::testServer(app$server, {
  as_student(rv, session, "a2@test.edu", "ECON 101", "01")
  seen$in_01 <- flex_question_state()$questions$question_text
})
stopifnot("Only for 02" %in% seen$in_02, !"Only for 02" %in% seen$in_01, "Up two" %in% seen$in_01, !"Up two" %in% seen$in_02)

# ── Renaming the gradebook item keeps its questions ──────────────────────────
shiny::testServer(app$server, {
  as_admin(rv, session, "ECON 101", "01")
  session$setInputs(rename_gb_item_btn = list(cat_id = econ_exams, idx = 2, name = "Final Exam", weight = "")); session$flushReact()
  final <- fund(flex_question_state(), "final")
  stopifnot(final$exam_label == "Final Exam", final$unlocked_n == 2L, final$next_question$id == f3)
})

# ── One pool for the whole course ────────────────────────────────────────────
app$raw_set_setting(app$setting_level_key("flex_pool_level", "course", "econ 101"), "course")
shiny::testServer(app$server, {
  as_student(rv, session, "b1@test.edu", "ECON 101", "02")
  st <- flex_question_state(); final <- fund(st, "final")
  # A fresh pool: nothing revealed yet, priced for all five students' pool.
  stopifnot(final$scope_key == "econ 101::course-pool", final$unlocked_n == 0L, final$next_question$id == f1, final$class_size == 5L)
  only_02 <- st$questions[st$questions$question_text == "Only for 02", ]
  stopifnot(only_02$fund_key == "econ 101::02", only_02$class_size == 2L)           # still its own section's
  session$setInputs(flex_contribution = 2, submit_flex_contribution = 3L); session$flushReact()
})
shiny::testServer(app$server, {
  as_student(rv, session, "a1@test.edu", "ECON 101", "01")
  final <- fund(flex_question_state(), "final")
  stopifnot(final$unlocked_n == 1L, final$unlocked$id == f1)                         # section 01 sees what section 02 paid for
  # The export names each contribution's exam and the question's section.
  rows <- read.csv(output$dl_flex_purchases, stringsAsFactors = FALSE)
})
# The section pools' records are still there, untouched by the switch.
stopifnot(near(app$db_query("SELECT SUM(amount) s FROM flex_question_contributions WHERE scope_key=?;", list(s01))$s, 2 + 9 + 2 + 2 + 0.37),
          all(before$paid$id %in% app$db_query("SELECT id FROM flex_question_contributions;")$id))
shiny::testServer(app$server, {
  as_admin(rv, session, "ECON 101", "02")
  rows <- read.csv(output$dl_flex_purchases, stringsAsFactors = FALSE)
  stopifnot(all(c("exam", "exam_item_key", "question_section") %in% names(rows)), nrow(rows) >= 1L, all(rows$exam_item_key == "final"))
})

cat("critical exam question funds: PASS\n")
