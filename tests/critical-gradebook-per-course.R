#!/usr/bin/env Rscript
# Each course has its own gradebook. A database that still has one shared
# gradebook is migrated at startup without changing anybody's grade, and from
# then on a change in one course's gradebook does not reach another course.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("gradebook-course-")
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

# ── A database as it was before: one gradebook, two courses, three sections ──
for (tbl in c("grade_reweight_requests", "student_grades", "gradebook_item_names", "gradebook_categories", "gradebook_exam_drops"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
# Only this fixture's courses: drop the students a fresh database is seeded with.
app$db_exec("DELETE FROM user_environments;"); app$db_exec("DELETE FROM users WHERE COALESCE(is_admin,0)=0;")
people <- data.frame(
  uid = c("a1@test.edu", "a2@test.edu", "b1@test.edu", "d1@test.edu"),
  course = c("ECON 101", "ECON 101", "ECON 101", "DATA 210"), section = c("01", "01", "02", "01"),
  ps1 = c(100, 80, 60, 90), final = c(70, 90, 50, 40), stringsAsFactors = FALSE)
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES('prof@test.edu','Prof',1,'ECON 101','01',1,0);")
for (i in seq_len(nrow(people))) {
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES(?,?,0,?,?,1,0);",
              list(people$uid[i], people$uid[i], people$course[i], people$section[i]))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,?,?);",
              list(people$uid[i], people$course[i], people$section[i]))
  app$db_exec("INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct) VALUES(?,'PS1',?,100,?),(?,'Final',?,100,?);",
              list(people$uid[i], people$ps1[i], people$ps1[i], people$uid[i], people$final[i], people$final[i]))
}
app$db_exec("INSERT INTO gradebook_categories(id,name,weight,item_count,item_prefix,max_points,source,display_order) VALUES(101,'Problem Sets',40,1,'PS',100,'manual',1);")
app$db_exec("INSERT INTO gradebook_categories(id,name,weight,item_count,item_prefix,max_points,source,display_order) VALUES(102,'Exams',60,1,'Exam',100,'manual',2);")
app$db_exec("INSERT INTO gradebook_item_names(category_id,item_index,item_name,item_weight) VALUES(101,1,'PS1',40),(102,1,'Final',60);")
app$db_exec("INSERT INTO gradebook_exam_drops(user_id,assignment_name,transfer_to,note) VALUES('d1@test.edu','Final','PS1','fixture');")
app$db_exec("INSERT INTO grade_reweight_requests(user_id,from_category,to_category,points,cost,level,status) VALUES('a1@test.edu','Exams','Problem Sets',5,10,'category','approved');")
overall <- function() vapply(people$uid, function(uid) app$compute_student_grade(uid)$overall, numeric(1))
before <- overall()
stopifnot(all(is.finite(before)), near(app$compute_student_grade("a2@test.edu")$overall, 0.4 * 80 + 0.6 * 90))

# ── Migration: every course gets its own copy and no grade moves ─────────────
# Both steps run at startup: grades get their student's home course, and each
# course gets its own gradebook.
stopifnot(all(is.na(app$db_query("SELECT course FROM student_grades;")$course)))
app$assign_grade_courses()
stopifnot(identical(app$db_query("SELECT DISTINCT course FROM student_grades ORDER BY course;")$course, c("data 210", "econ 101")))
app$migrate_gradebooks_per_course()
cats <- app$db_query("SELECT id, name, weight, COALESCE(course,'') course FROM gradebook_categories ORDER BY id;")
stopifnot(identical(sort(unique(cats$course)), c("", "data 210", "econ 101")),
          sum(cats$course == "") == 2L, sum(cats$course == "econ 101") == 2L, sum(cats$course == "data 210") == 2L,
          nrow(app$db_query("SELECT 1 FROM gradebook_item_names;")) == 6L,
          near(overall(), before))
# Running it again copies nothing.
app$migrate_gradebooks_per_course()
stopifnot(nrow(app$db_query("SELECT 1 FROM gradebook_categories;")) == 6L, near(overall(), before))
own <- function(course) app$gradebook_categories_for(course)
stopifnot(all(own("ECON 101")$course == "econ 101"), all(own("data 210")$course == "data 210"),
          !any(own("ECON 101")$id %in% own("DATA 210")$id))

# ── A change in one course stays in that course ───────────────────────────────
econ_exam <- own("ECON 101")$id[own("ECON 101")$name == "Exams"]
econ_ps <- own("ECON 101")$id[own("ECON 101")$name == "Problem Sets"]
app$db_exec("UPDATE gradebook_categories SET weight=80 WHERE id=?;", list(econ_exam))
app$db_exec("UPDATE gradebook_categories SET weight=20 WHERE id=?;", list(econ_ps))
app$db_exec("UPDATE gradebook_item_names SET item_weight=80 WHERE category_id=?;", list(econ_exam))
app$db_exec("UPDATE gradebook_item_names SET item_weight=20 WHERE category_id=?;", list(econ_ps))
after <- overall()
stopifnot(near(after[["d1@test.edu"]], before[["d1@test.edu"]]),                 # the other course is untouched
          near(after[["a2@test.edu"]], 0.2 * 80 + 0.8 * 90),                     # this course follows its own weights
          !near(after[["b1@test.edu"]], before[["b1@test.edu"]]))                # both of its sections

as_admin <- function(rv, course, section = "01") {
  rv$authed <- TRUE; rv$user_id <- "prof@test.edu"; rv$name <- "Prof"; rv$is_admin <- TRUE; rv$admin_role <- "super_admin"
  rv$course <- course; rv$section <- section; rv$active_course <- course; rv$active_section <- section
  rv$active_sections <- section; rv$impersonating <- FALSE
}
seen <- new.env()
shiny::testServer(app$server, {
  as_admin(rv, "ECON 101"); session$flushReact()
  # Renaming an item moves this course's grades and records, and no one else's.
  session$setInputs(rename_gb_item_btn = list(cat_id = econ_exam, idx = 1, name = "Final Exam", weight = "80"))
  session$flushReact()
  # A category added here belongs to this course.
  session$setInputs(new_gb_name = "Quizzes", new_gb_weight = 10, new_gb_count = 1, new_gb_prefix = "Quiz", new_gb_max = 100,
                    new_gb_source = "manual", add_gb_cat_btn = 1L)
  session$flushReact()
  seen$econ_items <- unlist(app$get_all_gradebook_items(), use.names = FALSE)
  seen$econ_grade <- app$compute_student_grade("a2@test.edu")$overall
})
grades_named <- function(name) sort(app$db_query("SELECT user_id FROM student_grades WHERE assignment_name=?;", list(name))$user_id)
stopifnot(identical(grades_named("Final Exam"), c("a1@test.edu", "a2@test.edu", "b1@test.edu")),
          identical(grades_named("Final"), "d1@test.edu"),
          app$db_query("SELECT assignment_name n FROM gradebook_exam_drops WHERE user_id='d1@test.edu';")$n[1] == "Final",
          all(c("Final Exam", "PS1") %in% seen$econ_items), !"Final" %in% seen$econ_items,
          app$db_query("SELECT COALESCE(course,'') c FROM gradebook_categories WHERE name='Quizzes';")$c == "econ 101",
          near(overall()[["d1@test.edu"]], before[["d1@test.edu"]]))
shiny::testServer(app$server, {
  as_admin(rv, "DATA 210"); session$flushReact()
  seen$data_items <- unlist(app$get_all_gradebook_items(), use.names = FALSE)
  seen$data_grade <- app$compute_student_grade("d1@test.edu")$overall
})
stopifnot(identical(sort(seen$data_items), c("Final", "PS1")), near(seen$data_grade, before[["d1@test.edu"]]))

# ── A course added later follows the shared gradebook until it is edited ─────
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES('n1@test.edu','n1',0,'ECON 999','01',1,0);")
app$db_exec("INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct) VALUES('n1@test.edu','PS1',50,100,50),('n1@test.edu','Final',100,100,100);")
stopifnot(all(is.na(own("ECON 999")$course)), near(app$compute_student_grade("n1@test.edu")$overall, 0.4 * 50 + 0.6 * 100))
shiny::testServer(app$server, {
  as_admin(rv, "ECON 999"); session$flushReact()
  session$setInputs(new_gb_name = "Labs", new_gb_weight = 0, new_gb_count = 1, new_gb_prefix = "Lab", new_gb_max = 100,
                    new_gb_source = "manual", add_gb_cat_btn = 1L)
  session$flushReact()
})
stopifnot(identical(sort(own("ECON 999")$name), c("Exams", "Labs", "Problem Sets")), all(own("ECON 999")$course == "econ 999"),
          sum(is.na(app$db_query("SELECT course FROM gradebook_categories;")$course)) == 2L,   # the shared gradebook is as it was
          near(app$compute_student_grade("n1@test.edu")$overall, 0.4 * 50 + 0.6 * 100))

# ── A student in two courses has a separate grade in each ────────────────────
# a2's home course is ECON 101. Add them to DATA 210, where "Final" is also an item.
app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES('a2@test.edu','DATA 210','01');")
shiny::testServer(app$server, {
  as_admin(rv, "DATA 210"); session$flushReact()
  app$upsert_student_grade("a2@test.edu", "Final", 33, 100, 33)
  app$upsert_student_grade("a2@test.edu", "PS1", 44, 100, 44)
  app$upsert_student_grade("a2@test.edu", "PS1", 55, 100, 55)          # saving again replaces, within the course
  seen$a2_in_data <- app$compute_student_grade("a2@test.edu")$overall
})
a2 <- app$db_query("SELECT course, assignment_name, grade_pct FROM student_grades WHERE user_id='a2@test.edu' ORDER BY course, assignment_name;")
stopifnot(identical(a2$course, c("data 210", "data 210", "econ 101", "econ 101")),
          identical(a2$assignment_name, c("Final", "PS1", "Final Exam", "PS1")), identical(a2$grade_pct, c(33, 55, 90, 80)),
          near(seen$a2_in_data, 0.4 * 55 + 0.6 * 33),                           # DATA 210's weights and DATA 210's grades
          near(overall()[["a2@test.edu"]], after[["a2@test.edu"]]))             # the home-course grade has not moved
# A grade written with no course counts for the student's home course.
app$db_exec("INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct) VALUES('b1@test.edu','Extra',1,1,100);")
stopifnot(is.na(app$db_query("SELECT course FROM student_grades WHERE assignment_name='Extra';")$course))
# Clearing or deleting grades in one course leaves the others.
count <- function(course) app$db_query("SELECT COUNT(*) n FROM student_grades WHERE COALESCE(course,'')=?;", list(course))$n[1]
econ_n <- count("econ 101")
shiny::testServer(app$server, {
  as_admin(rv, "DATA 210"); session$flushReact()
  session$setInputs(delete_grade_assignments = "PS1", delete_grade_assignments_btn = 1L); session$flushReact()
  seen$data_after_delete <- count("data 210")
  session$setInputs(clear_grades_btn = 1L); session$flushReact()
})
stopifnot(seen$data_after_delete == 2L,       # d1's Final and a2's Final remain; both PS1 grades are gone
          count("data 210") == 0L, count("econ 101") == econ_n, econ_n >= 6L,
          near(overall()[["a2@test.edu"]], after[["a2@test.edu"]]))

cat("critical gradebook per course: PASS\n")
