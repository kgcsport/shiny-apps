suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

# Startup migration and scoped-role grants must remain compatible with the
# existing users/is_admin table while keeping super-admins durable.
test_root <- tempfile("admin-roles-")
dir.create(file.path(test_root, "data"), recursive = TRUE)
db_path <- file.path(test_root, "data", "class-job-market.sqlite")
con <- dbConnect(SQLite(), db_path)
dbExecute(con, "CREATE TABLE users(user_id TEXT PRIMARY KEY, display_name TEXT, pw_hash TEXT, is_admin INTEGER DEFAULT 0, course TEXT, section TEXT, active INTEGER DEFAULT 1, is_demo INTEGER DEFAULT 0)")
dbExecute(con, "INSERT INTO users(user_id,display_name,is_admin,course,section) VALUES (?, ?, 1, ?, ?), (?, ?, 0, ?, ?), (?, ?, 0, ?, ?)", params = list("legacy@vassar.edu", "Legacy Admin", "ECON 160", "A", "ta@vassar.edu", "TA", "ECON 160", "A", "student@vassar.edu", "Student", "ECON 160", "A"))
dbDisconnect(con)

Sys.setenv(CONNECT_CONTENT_DIR = test_root, ADMIN_EMAILS = "")
source("apps/class-job-market/app.R")

env_seed <- db_query("SELECT course,section FROM user_environments WHERE user_id=?;", list("student@vassar.edu"))
stopifnot(nrow(env_seed) == 1L, env_seed$course[1] == "ECON 160", env_seed$section[1] == "A")
db_exec("INSERT INTO user_environments(user_id,course,section) VALUES(?,?,?);", list("student@vassar.edu", "ECON 342", "B"))
envs <- db_query("SELECT course,section FROM user_environments WHERE user_id=? ORDER BY course,section;", list("student@vassar.edu"))
stopifnot(nrow(envs) == 2L, any(envs$course == "ECON 160"), any(envs$course == "ECON 342"))
app_text <- paste(readLines("apps/class-job-market/app.R", warn=FALSE), collapse="\n")
for (needle in c("user_environments", "session_environment_rows <- function()", "pin_session_environment()", "input$environment_sel")) stopifnot(grepl(needle, app_text, fixed=TRUE))

legacy <- admin_assignments_for("legacy@vassar.edu")
stopifnot(nrow(legacy) == 1L, legacy$role[1] == "super_admin")

grant_admin_assignment("ta@vassar.edu", "section_admin", "ECON 160", "A", "test")
ta <- admin_assignments_for("ta@vassar.edu")
stopifnot(nrow(ta) == 1L, ta$role[1] == "section_admin", ta$course[1] == "ECON 160", ta$section[1] == "A")
user_row <- db_query("SELECT is_admin,admin_role FROM users WHERE user_id=?", list("ta@vassar.edu"))
stopifnot(user_row$is_admin[1] == 1L, user_row$admin_role[1] == "section_admin")

# A user can have multiple scoped roles; highest role remains the compatibility label.
grant_admin_assignment("ta@vassar.edu", "course_admin", "ECON 160", "", "test")
stopifnot(effective_admin_role("ta@vassar.edu") == "course_admin")
stopifnot(nrow(admin_assignments_for("ta@vassar.edu")) == 2L)

# The sole super-admin cannot be removed accidentally.
sole_super_id <- db_query("SELECT id FROM admin_assignments WHERE user_id=?", list("legacy@vassar.edu"))$id[1]
stopifnot(inherits(try(revoke_admin_assignment(sole_super_id), silent = TRUE), "try-error"))

cat("admin roles: OK\n")
