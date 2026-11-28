#!/usr/bin/env Rscript
# Grades from the assignments Worker are filed by offering, sign-in email and
# item key. Rows for another course are left alone, and a row whose course ID
# is not an offering here is matched the way it always was.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("worker-sync-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)
app <- new.env(parent = globalenv())
source(app_path, local = app)

for (tbl in c("enrollments", "course_offerings", "user_environments", "student_grades", "gradebook_item_names",
              "gradebook_categories", "assignment_grade_sync_override"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
app$db_exec("DELETE FROM users;")
person <- function(uid, name, course, section = "01", active = 1L) {
  app$db_exec("INSERT INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES(?,?,0,?,?,?,0);", list(uid, name, course, section, active))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,?,?);", list(uid, course, section))
}
person("ann@test.edu", "Ann Lee", "ECON 342"); person("bo@test.edu", "Bo Diaz", "ECON 342")
person("sam1@test.edu", "Sam Roy", "ECON 342"); person("sam2@test.edu", "Sam Roy", "ECON 342")   # two students, one name
person("dee@test.edu", "Dee Park", "ECON 102"); person("ann2@test.edu", "Ann Lee", "ECON 102")   # same name as Ann, other course
app$sync_enrollments()
stopifnot(identical(sort(app$db_query("SELECT offering_id FROM course_offerings;")$offering_id), c("econ102-2026-fall", "econ342-2026-fall")))
# Each course has its own gradebook; both have a PS3 and a Final.
for (course in c("econ 342", "econ 102")) {
  app$db_exec("INSERT INTO gradebook_categories(name,weight,item_count,item_prefix,max_points,source,display_order,course) VALUES('Problem Sets',50,3,'PS',100,'manual',1,?);", list(course))
  id <- app$db_query("SELECT MAX(id) id FROM gradebook_categories;")$id
  app$db_exec("INSERT INTO gradebook_item_names(category_id,item_index,item_name) VALUES(?,1,'PS1'),(?,2,'PS2'),(?,3,'PS3');", list(id, id, id))
  app$db_exec("INSERT INTO gradebook_categories(name,weight,item_count,item_prefix,max_points,source,display_order,course) VALUES('Final',50,1,'',100,'manual',2,?);", list(course))
}
row <- function(course, key, ext, name, pct = 90, email = "", title = "Problem Set 3: Taxes") data.frame(
  assignmentId = "a1", assignmentTitle = title, gradebookKey = key, courseId = course, externalId = ext, studentName = name,
  studentEmail = email, status = "submitted", submittedAt = "2026-11-01T00:00:00Z", scanVerifiedAt = "", score = pct, maxPoints = 100,
  gradePct = pct, stringsAsFactors = FALSE)
grade <- function(uid, item, course) {
  g <- app$db_query("SELECT grade_pct g FROM student_grades WHERE user_id=? AND assignment_name=? AND COALESCE(course,'')=?;", list(uid, item, course))$g
  if (length(g)) g else NA_real_
}

# ── Rows that name an offering ───────────────────────────────────────────────
rows <- rbind(
  row("econ342-2026-fall", "PS3", "ann@test.edu", "Ann Lee", 91),          # email and key: filed
  row("econ342-2026-fall", "PS3", "s-0042", "Bo Diaz", 82),                # old Worker ID, one Bo in the course: by name
  row("econ342-2026-fall", "PS3", "s-0043", "Sam Roy", 73),                # two Sams: not guessed
  row("econ342-2026-fall", "PS3", "s-0044", "Dee Park", 64),               # Dee is not in this course
  row("econ342-2026-fall", "quiz9", "ann@test.edu", "Ann Lee", 55),        # no such item
  row("econ342-2026-fall", "PS2", "x", "", 77, email = "Sam2@Test.edu"),   # matched on the Worker's email field
  row("econ102-2026-fall", "PS3", "dee@test.edu", "Dee Park", 46),         # another course's row
  row("econ102-2026-fall", "PS3", "ann2@test.edu", "Ann Lee", 37))
n <- app$import_worker_grades(rows, "ECON 342")
stopifnot(n[["synced"]] == 3L, n[["by_name"]] == 1L, n[["unmatched_student"]] == 2L, n[["unmatched_key"]] == 1L,
          n[["other_course"]] == 2L, n[["unlinked"]] == 0L,
          grade("ann@test.edu", "PS3", "econ 342") == 91, grade("bo@test.edu", "PS3", "econ 342") == 82,
          grade("sam2@test.edu", "PS2", "econ 342") == 77,
          is.na(grade("sam1@test.edu", "PS3", "econ 342")), is.na(grade("sam2@test.edu", "PS3", "econ 342")),
          nrow(app$db_query("SELECT 1 FROM student_grades WHERE user_id IN ('dee@test.edu','ann2@test.edu');")) == 0L)
# The other course takes its own rows and nothing of this one's.
m <- app$import_worker_grades(rows, "econ 102")
stopifnot(m[["synced"]] == 2L, m[["other_course"]] == 6L,
          grade("dee@test.edu", "PS3", "econ 102") == 46, grade("ann2@test.edu", "PS3", "econ 102") == 37,
          grade("ann@test.edu", "PS3", "econ 342") == 91)
# Running the same rows again changes nothing.
before <- app$db_query("SELECT user_id, assignment_name, course, grade_pct FROM student_grades ORDER BY 1,2,3;")
app$import_worker_grades(rows, "ECON 342")
stopifnot(identical(app$db_query("SELECT user_id, assignment_name, course, grade_pct FROM student_grades ORDER BY 1,2,3;"), before))

# ── An item keeps its key when it is renamed ─────────────────────────────────
ps_cat <- app$db_query("SELECT id FROM gradebook_categories WHERE course='econ 342' AND name='Problem Sets';")$id
shiny::testServer(app$server, {
  rv$authed <- TRUE; rv$user_id <- "prof@test.edu"; rv$name <- "Prof"; rv$is_admin <- TRUE; rv$admin_role <- "super_admin"
  rv$course <- "ECON 342"; rv$section <- "01"; rv$active_course <- "ECON 342"; rv$active_section <- "01"; rv$active_sections <- "01"
  rv$impersonating <- FALSE; session$flushReact()
  session$setInputs(rename_gb_item_btn = list(cat_id = ps_cat, idx = 3, name = "Problem Set 3 (taxes)", weight = ""))
  session$flushReact()
})
catalog <- app$manual_grade_catalog("econ 342")
stopifnot(catalog$item_key[catalog$assignment == "Problem Set 3 (taxes)"] == "ps3",
          grade("ann@test.edu", "Problem Set 3 (taxes)", "econ 342") == 91)
app$import_worker_grades(row("econ342-2026-fall", "PS3", "ann@test.edu", "Ann Lee", 95), "ECON 342")
stopifnot(grade("ann@test.edu", "Problem Set 3 (taxes)", "econ 342") == 95,
          app$manual_grade_catalog("econ 102")$item_key[3] == "ps3")       # the other course's item is untouched

# ── Rows from a Worker course that is no offering here: matched as before ────
old <- rbind(row("econ342-fall-old-id", "", "bo@test.edu", "Bo Diaz", 70, title = "Problem set 1: Intro"),   # key from the title
             row("econ342-fall-old-id", "PS1", "nobody", "Ann Lee", 60))                                      # name, across all students: two Anns
k <- app$import_worker_grades(old, "ECON 342")
stopifnot(k[["synced"]] == 1L, k[["unlinked"]] == 1L, k[["unmatched_student"]] == 1L, grade("bo@test.edu", "PS1", "econ 342") == 70)

# ── Policy, protection and exclusion still apply ─────────────────────────────
app$mark_grade_override("ann@test.edu", "PS2", "Manual grade")
late <- row("econ342-2026-fall", "PS1", "ann@test.edu", "Ann Lee", 88); late$status <- "draft"
p <- app$import_worker_grades(rbind(row("econ342-2026-fall", "PS2", "ann@test.edu", "Ann Lee", 11), late,
                                    row("econ342-2026-fall", "PS1", "sam1@test.edu", "Sam Roy", 66)), "ECON 342",
                              excluded_items = "ps1")
stopifnot(p[["synced"]] == 0L, p[["skipped"]] == 3L,   # one protected, two excluded
          is.na(grade("ann@test.edu", "PS2", "econ 342")), is.na(grade("sam1@test.edu", "PS1", "econ 342")))

# ── Syncing one chosen assignment writes nothing for the others ──────────────
one <- app$import_worker_grades(rbind(row("econ342-2026-fall", "PS1", "bo@test.edu", "Bo Diaz", 12),
                                      row("econ342-2026-fall", "PS2", "bo@test.edu", "Bo Diaz", 34)), "ECON 342", only_key = "ps2")
stopifnot(one[["synced"]] == 1L, one[["filtered"]] == 1L, grade("bo@test.edu", "PS2", "econ 342") == 34,
          grade("bo@test.edu", "PS1", "econ 342") == 70)                    # PS1 keeps the grade it had

cat("critical worker grade sync: PASS\n")
