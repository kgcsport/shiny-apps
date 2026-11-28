#!/usr/bin/env Rscript
# Tokens, grade reweights, exam drops and manual-grade marks belong to one
# student in one course. Records from before that take the student's home
# course without changing any balance or grade; afterwards a student in two
# courses has a separate balance and separate purchases in each.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("records-course-")
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

# ── A database as it was: nothing carries a course ───────────────────────────
for (tbl in c("token_ledger", "grade_reweight_requests", "student_grades", "gradebook_item_names", "gradebook_categories",
              "user_environments", "enrollments", "course_offerings"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
app$db_exec("DELETE FROM users;")
# The two tables whose key changes, as they were made before.
app$db_exec("DROP TABLE assignment_grade_sync_override;"); app$db_exec("DROP TABLE gradebook_exam_drops;")
app$db_exec("CREATE TABLE assignment_grade_sync_override(user_id TEXT NOT NULL, assignment_name TEXT NOT NULL, reason TEXT,
             created_at TEXT DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY(user_id COLLATE NOCASE, assignment_name COLLATE NOCASE));")
app$db_exec("CREATE TABLE gradebook_exam_drops(user_id TEXT NOT NULL, assignment_name TEXT NOT NULL, transfer_to TEXT NOT NULL, note TEXT,
             created_at TEXT DEFAULT CURRENT_TIMESTAMP, updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
             PRIMARY KEY(user_id COLLATE NOCASE, assignment_name COLLATE NOCASE));")
person <- function(uid, course, admin = 0L) {
  app$db_exec("INSERT INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES(?,?,?,?,'01',1,0);", list(uid, uid, admin, course))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,?,'01');", list(uid, course))
}
person("prof@test.edu", "ECON 101", admin = 1L); person("ann@test.edu", "ECON 101"); person("bob@test.edu", "ECON 101"); person("dee@test.edu", "DATA 210")
app$db_exec("INSERT INTO user_environments(user_id,course,section) VALUES('ann@test.edu','DATA 210','01');")   # Ann is in both courses
app$db_exec("INSERT INTO gradebook_categories(id,name,weight,item_count,item_prefix,max_points,source,display_order) VALUES(1,'Problem Sets',40,1,'PS',100,'manual',1),(2,'Exams',60,1,'Exam',100,'manual',2);")
app$db_exec("INSERT INTO gradebook_item_names(category_id,item_index,item_name,item_weight) VALUES(1,1,'PS1',40),(2,1,'Final',60);")
for (uid in c("ann@test.edu", "bob@test.edu", "dee@test.edu"))
  app$db_exec("INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct) VALUES(?,'PS1',100,100,100),(?,'Final',50,100,50);", list(uid, uid))
ledger <- function(uid, amount, earning) app$db_exec(
  "INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES(?,?,'fixture',?,?,'');", list(uid, uid, amount, earning))
ledger("ann@test.edu", 50, 1L); ledger("ann@test.edu", -5, 0L); ledger("bob@test.edu", 30, 1L); ledger("dee@test.edu", 20, 1L)
app$db_exec("INSERT INTO grade_reweight_requests(user_id,from_category,to_category,points,cost,level,status) VALUES('ann@test.edu','Exams','Problem Sets',5,5,'category','approved');")
app$db_exec("INSERT INTO gradebook_exam_drops(user_id,assignment_name,transfer_to,note) VALUES('dee@test.edu','Final','PS1','excused');")
app$db_exec("INSERT INTO assignment_grade_sync_override(user_id,assignment_name,reason) VALUES('ann@test.edu','PS1','Manual grade');")
balance <- function(uid) app$db_query("SELECT COALESCE(SUM(amount),0) t FROM token_ledger WHERE user_id=?;", list(uid))$t
overall <- function(uid) app$compute_student_grade(uid)$overall
before <- list(bal = vapply(c("ann@test.edu", "bob@test.edu", "dee@test.edu"), balance, numeric(1)),
               grade = vapply(c("ann@test.edu", "bob@test.edu", "dee@test.edu"), overall, numeric(1)),
               rows = app$db_query("SELECT COUNT(*) n FROM token_ledger;")$n)
stopifnot(near(before$grade[["ann@test.edu"]], 0.45 * 100 + 0.55 * 50),      # Ann's approved reweight
          near(before$grade[["bob@test.edu"]], 0.4 * 100 + 0.6 * 50))

# ── What startup does to it ──────────────────────────────────────────────────
migrate <- function() {
  app$assign_record_courses(); app$assign_grade_courses()
  app$rekey_by_course("assignment_grade_sync_override", c("user_id TEXT NOT NULL", "assignment_name TEXT NOT NULL", "reason TEXT", "created_at TEXT DEFAULT CURRENT_TIMESTAMP"))
  app$rekey_by_course("gradebook_exam_drops", c("user_id TEXT NOT NULL", "assignment_name TEXT NOT NULL", "transfer_to TEXT NOT NULL", "note TEXT",
                                                 "created_at TEXT DEFAULT CURRENT_TIMESTAMP", "updated_at TEXT DEFAULT CURRENT_TIMESTAMP"))
  app$migrate_gradebooks_per_course()
}
migrate()
in_course <- function(uid, course) app$token_balance_in_course(uid, course)
stopifnot(identical(app$db_query("SELECT DISTINCT course FROM token_ledger ORDER BY 1;")$course, c("data 210", "econ 101")),
          app$db_query("SELECT COUNT(*) n FROM token_ledger;")$n == before$rows,
          near(vapply(names(before$bal), balance, numeric(1)), before$bal),                 # no balance moved
          near(in_course("ann@test.edu", "ECON 101"), 45), near(in_course("ann@test.edu", "data 210"), 0),
          near(in_course("dee@test.edu", "data 210"), 20),
          app$db_query("SELECT course FROM grade_reweight_requests;")$course == "econ 101",
          identical(app$db_query("SELECT course, user_id, assignment_name, transfer_to, note FROM gradebook_exam_drops;"),
                    data.frame(course = "data 210", user_id = "dee@test.edu", assignment_name = "Final", transfer_to = "PS1", note = "excused")),
          identical(app$db_query("SELECT course, user_id, assignment_name, reason FROM assignment_grade_sync_override;"),
                    data.frame(course = "econ 101", user_id = "ann@test.edu", assignment_name = "PS1", reason = "Manual grade")),
          near(overall("ann@test.edu"), before$grade[["ann@test.edu"]]),                    # no grade moved
          near(overall("bob@test.edu"), before$grade[["bob@test.edu"]]),
          # Dee's drop still applies in her course: the Final's weight is on PS1.
          # (Before the rebuild this fixture's old-style table could not be read.)
          near(overall("dee@test.edu"), 100))
migrate()                                                                                    # a second start changes nothing
stopifnot(app$db_query("SELECT COUNT(*) n FROM gradebook_exam_drops;")$n == 1L, near(vapply(names(before$bal), balance, numeric(1)), before$bal))

# ── Ann in her second course ─────────────────────────────────────────────────
in_session <- function(course, uid = "prof@test.edu", admin = TRUE, expr) shiny::testServer(app$server, {
  rv$authed <- TRUE; rv$user_id <- uid; rv$name <- uid; rv$is_admin <- admin; if (admin) rv$admin_role <- "super_admin"
  rv$course <- course; rv$section <- "01"; rv$active_course <- course; rv$active_section <- "01"; rv$active_sections <- "01"
  rv$impersonating <- FALSE; session$flushReact()
  expr(session, rv, output)
})
seen <- new.env()
in_session("DATA 210", "ann@test.edu", FALSE, function(session, rv, output) {
  # Nothing earned or bought in ECON 101 shows here.
  stopifnot(near(app$token_balance_in_course("ann@test.edu"), 0), near(app$gross_participation_tokens("ann@test.edu"), 0),
            app$rw_points_requested("ann@test.edu") == 0L, nrow(app$rw_pending_request("ann@test.edu")) == 0L,
            near(app$compute_student_grade("ann@test.edu")$cats$weight, c(40, 60)))        # no reweight in this course
  app$token_credit("ann@test.edu", "ann", 12, 1L, "job", NA, "earned in DATA 210")
  stopifnot(near(app$token_balance_in_course("ann@test.edu"), 12), near(app$gross_participation_tokens("ann@test.edu"), 12))
})
in_session("ECON 101", "ann@test.edu", FALSE, function(session, rv, output) {
  stopifnot(near(app$token_balance_in_course("ann@test.edu"), 45), near(app$gross_participation_tokens("ann@test.edu"), 50),
            app$rw_points_requested("ann@test.edu") == 5L,
            near(app$compute_student_grade("ann@test.edu")$cats$weight, c(45, 55)))
})
stopifnot(near(in_course("ann@test.edu", "data 210"), 12), near(in_course("ann@test.edu", "econ 101"), 45))
# An entry written with no course (another app, an older writer) counts for the home course.
ledger("ann@test.edu", 3, 1L)
stopifnot(near(in_course("ann@test.edu", "econ 101"), 48), near(in_course("ann@test.edu", "data 210"), 12))

# One request may wait for review in each course, and only one.
pending <- function(course) app$db_exec(
  "INSERT INTO grade_reweight_requests(user_id,from_category,to_category,points,cost,level,status,course) VALUES('ann@test.edu','Exams','Problem Sets',1,1,'category','pending',?);", list(course))
refused <- function(course) inherits(tryCatch(DBI::dbExecute(app$get_con(),
  "INSERT INTO grade_reweight_requests(user_id,from_category,to_category,points,cost,level,status,course) VALUES('ann@test.edu','Exams','Problem Sets',1,1,'category','pending',?);",
  params = list(course)), error = function(e) e), "error")
pending("econ 101"); pending("data 210")
stopifnot(refused("econ 101"), refused("data 210"),
          app$db_query("SELECT COUNT(*) n FROM grade_reweight_requests WHERE status='pending';")$n == 2L)

# ── Drops and manual-grade marks with the same student and item in two courses ─
app$db_exec("INSERT INTO gradebook_exam_drops(course,user_id,assignment_name,transfer_to,note) VALUES('econ 101','ann@test.edu','Final','PS1','x'),('data 210','ann@test.edu','Final','PS1','y');")
app$mark_grade_override("ann@test.edu", "PS1", "Manual grade", course = "DATA 210")
stopifnot(app$db_query("SELECT COUNT(*) n FROM gradebook_exam_drops WHERE user_id='ann@test.edu';")$n == 2L,
          identical(sort(app$db_query("SELECT course FROM assignment_grade_sync_override WHERE user_id='ann@test.edu';")$course), c("data 210", "econ 101")))
# The instructor of one course sees and removes only that course's.
in_session("DATA 210", expr = function(session, rv, output) {
  session$setInputs(delete_exam_drop_btn = list(user_id = "dee@test.edu", assignment = "Final")); session$flushReact()
})
stopifnot(app$db_query("SELECT COUNT(*) n FROM gradebook_exam_drops WHERE user_id='dee@test.edu';")$n == 0L,
          app$db_query("SELECT COUNT(*) n FROM gradebook_exam_drops WHERE user_id='ann@test.edu';")$n == 2L)
in_session("ECON 101", expr = function(session, rv, output) {
  session$setInputs(exam_drop_student = "bob@test.edu", exam_drop_assignment = "Final", exam_drop_transfer_to = "PS1",
                    exam_drop_note = "missed", save_exam_drop_btn = 1L); session$flushReact()
  rows <- read.csv(output$dl_tokens, stringsAsFactors = FALSE)
  stopifnot(!any(grepl("DATA 210", rows$note)), all(rows$user_id %in% c("ann@test.edu", "bob@test.edu")))   # this course's ledger only
})
stopifnot(app$db_query("SELECT course FROM gradebook_exam_drops WHERE user_id='bob@test.edu';")$course == "econ 101",
          near(overall("bob@test.edu"), 100))             # the Final's weight moved to PS1

cat("critical records per course: PASS\n")
