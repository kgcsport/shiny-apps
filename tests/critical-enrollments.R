#!/usr/bin/env Rscript
# Offerings and enrollments are filled from the roster and kept in line with
# it. An enrollment's ID never changes, and nothing else in the app is touched.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
scratch <- tempfile("enrollments-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)
app <- new.env(parent = globalenv())
source(app_path, local = app)

# ── A roster as it stands in fall ────────────────────────────────────────────
for (tbl in c("enrollments", "course_offerings", "user_environments")) app$db_exec(sprintf("DELETE FROM %s;", tbl))
app$db_exec("DELETE FROM users;")
person <- function(uid, course, section, admin = 0L, active = 1L, demo = 0L) {
  app$db_exec("INSERT INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES(?,?,?,?,?,?,?);",
              list(uid, uid, admin, course, section, active, demo))
  if (!demo) app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,?,?);", list(uid, trimws(course), section))
}
person("prof@test.edu", "ECON 342", "01", admin = 1L)
person("ann@test.edu", "ECON 342", "01")
person("Bo@Test.edu", "econ 342 ", "02")                 # same course, typed differently
person("cy@test.edu", "ECON 342", "01", active = 0L)     # archived
person("dee@test.edu", "DATA 210", "01")
person("demo@test.edu", "DEMO 101", "01", demo = 1L)
# Ann is also in DATA 210, and has a stale second section in her home course.
app$db_exec("INSERT INTO user_environments(user_id,course,section) VALUES('ann@test.edu','DATA 210','01'),('ann@test.edu','ECON 342','09');")
counts <- function() c(users = app$db_query("SELECT COUNT(*) n FROM users;")$n, env = app$db_query("SELECT COUNT(*) n FROM user_environments;")$n)
before <- counts()

app$sync_enrollments()
offerings <- app$db_query("SELECT offering_id, course, term FROM course_offerings ORDER BY offering_id;")
rows <- function() app$db_query(
  "SELECT e.enrollment_id, e.offering_id, LOWER(e.user_id) user_id, e.section, e.status
   FROM enrollments e ORDER BY e.offering_id, LOWER(e.user_id);")
first <- rows()
stopifnot(identical(offerings$offering_id, c("data210-2026-fall", "econ342-2026-fall")),
          identical(offerings$course, c("data 210", "econ 342")), all(offerings$term == "2026-fall"),
          # One enrollment per student per course; no instructor, no demo account.
          identical(paste(first$offering_id, first$user_id),
                    c("data210-2026-fall ann@test.edu", "data210-2026-fall dee@test.edu",
                      "econ342-2026-fall ann@test.edu", "econ342-2026-fall bo@test.edu", "econ342-2026-fall cy@test.edu")),
          # Ann's home section, not the stale one; Bo's section; Cy is inactive.
          identical(first$section, c("01", "01", "01", "02", "01")),
          identical(first$status, c("active", "active", "active", "active", "inactive")),
          identical(counts(), before))                     # the roster itself is untouched

# Running it again changes nothing and keeps every ID.
app$sync_enrollments()
stopifnot(identical(rows(), first))

# A section change and a return from the archive update the same enrollment.
app$db_exec("UPDATE users SET section='02' WHERE user_id='ann@test.edu';")
app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES('ann@test.edu','ECON 342','02');")
app$db_exec("UPDATE users SET active=1 WHERE user_id='cy@test.edu';")
app$sync_enrollments()
moved <- rows()
stopifnot(identical(moved$enrollment_id, first$enrollment_id),
          moved$section[moved$offering_id == "econ342-2026-fall" & moved$user_id == "ann@test.edu"] == "02",
          moved$status[moved$user_id == "cy@test.edu"] == "active")

# ── A new term ────────────────────────────────────────────────────────────────
app$raw_set_setting("current_term", " 2027 Spring ")
stopifnot(app$current_term() == "2027-spring")
person("eve@test.edu", "ECON 102", "01")                   # a course new this term
person("fay@test.edu", "DATA 210", "01")                   # a new student in a course taught before
app$sync_enrollments()
spring <- rows()
stopifnot(identical(sort(app$db_query("SELECT offering_id FROM course_offerings;")$offering_id),
                    c("data210-2026-fall", "data210-2027-spring", "econ102-2027-spring", "econ342-2026-fall")),
          spring$offering_id[spring$user_id == "eve@test.edu"] == "econ102-2027-spring",
          spring$offering_id[spring$user_id == "fay@test.edu"] == "data210-2027-spring",
          # Fall's students stay in fall's offerings with the IDs they had.
          { kept <- spring[spring$enrollment_id %in% first$enrollment_id, ]; kept <- kept[order(kept$enrollment_id), ]
            was <- moved[order(moved$enrollment_id), ]; rownames(kept) <- rownames(was) <- NULL
            identical(kept, was) },
          nrow(spring) == nrow(first) + 2L)

# The term is set from Settings; anything but year-season is refused.
shiny::testServer(app$server, {
  rv$authed <- TRUE; rv$user_id <- "prof@test.edu"; rv$name <- "Prof"; rv$is_admin <- TRUE; rv$admin_role <- "super_admin"
  rv$course <- "ECON 342"; rv$section <- "01"; rv$active_course <- "ECON 342"; rv$active_section <- "01"; rv$active_sections <- "01"
  rv$impersonating <- FALSE; session$flushReact()
  session$setInputs(current_term_input = "spring 27", save_current_term_btn = 1L); session$flushReact()
  stopifnot(app$current_term() == "2027-spring")
  session$setInputs(current_term_input = "2027-Summer", save_current_term_btn = 2L); session$flushReact()
  stopifnot(app$current_term() == "2027-summer")
})
app$raw_set_setting("current_term", "2027-spring")

# Two course names that reduce to the same letters get different offering IDs.
stopifnot(app$offering_for("ECON-102") != app$offering_for("ECON 102"),
          app$offering_for("ECON-102") == "econ102-2027-spring-2", app$offering_for("econ 102") == "econ102-2027-spring")

cat("critical enrollments: PASS\n")
