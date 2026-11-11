#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RSQLite)
})

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
scratch <- tempfile("grade-reweighting-")
dir.create(file.path(scratch, "data"), recursive=TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset=NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR=scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR=old_content)
  unlink(scratch, recursive=TRUE, force=TRUE)
}, add=TRUE)

app <- new.env(parent=globalenv())
source(app_path, local=app)
on.exit(suppressWarnings(try(
  if (!is.null(app$conn) && DBI::dbIsValid(app$conn)) DBI::dbDisconnect(app$conn),
  silent=TRUE)), add=TRUE)

app$db_exec("DELETE FROM grade_reweight_requests;")
app$db_exec("DELETE FROM student_grades;")
app$db_exec("DELETE FROM gradebook_item_names;")
app$db_exec("DELETE FROM gradebook_categories;")
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES('admin@test.edu','Instructor',1,'ECON 342','01',1,0);")
for (uid in c("alice@test.edu", "bob@test.edu"))
  app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,is_admin,course,section,active,is_demo) VALUES(?,?,0,'ECON 342','01',1,0);",
              list(uid, sub("@.*", "", uid)))

app$db_exec("INSERT INTO gradebook_categories(id,name,weight,item_count,item_prefix,max_points,source,display_order) VALUES(101,'Problem Sets',50,2,'PS',100,'manual',1);")
app$db_exec("INSERT INTO gradebook_categories(id,name,weight,item_count,item_prefix,max_points,source,display_order) VALUES(102,'Policy Project',50,2,'Policy',100,'manual',2);")
app$db_exec("INSERT INTO gradebook_item_names(category_id,item_index,item_name,item_weight) VALUES(101,1,'PS1',25),(101,2,'PS2',25),(102,1,'Policy Presentation',25),(102,2,'Written Policy Brief',25);")
for (uid in c("alice@test.edu", "bob@test.edu")) {
  app$db_exec("INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct) VALUES(?, 'PS1',100,100,100),(?,'PS2',100,100,100),(?,'Policy Presentation',60,100,60),(?,'Written Policy Brief',80,100,80);",
              list(uid, uid, uid, uid))
}

app$db_exec("INSERT INTO grade_reweight_requests(user_id,from_category,to_category,points,cost,level,status) VALUES('alice@test.edu','Problem Sets','Policy Project',5,20,'category','pending');")
alice_request <- app$db_query("SELECT id FROM grade_reweight_requests WHERE user_id='alice@test.edu';")$id[1]
stopifnot(abs(app$compute_student_grade("alice@test.edu")$overall - 85) < 1e-8)

shiny::testServer(app$server, {
  rv$authed <- TRUE
  rv$user_id <- "admin@test.edu"
  rv$name <- "Instructor"
  rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"
  rv$course <- "ECON 342"
  rv$section <- "01"
  rv$active_course <- "ECON 342"
  rv$active_section <- "01"
  rv$active_sections <- "01"
  rv$impersonating <- FALSE

  session$setInputs(approve_grade_reweight_request=alice_request)
  session$flushReact()
  approved <- app$db_query("SELECT status FROM grade_reweight_requests WHERE id=?;", list(alice_request))
  stopifnot(identical(approved$status[1], "approved"))
  alice_grade <- app$compute_student_grade("alice@test.edu")
  stopifnot(abs(alice_grade$overall - 83.5) < 1e-8)
  stopifnot(all.equal(alice_grade$cats$weight, c(45,55), tolerance=1e-8) == TRUE)

  session$setInputs(rename_gb_item_btn=list(cat_id=102,idx=2,name="Final Policy Brief",weight="30"))
  session$flushReact()
  saved_item <- app$db_query("SELECT item_name,item_weight FROM gradebook_item_names WHERE category_id=102 AND item_index=2;")
  stopifnot(identical(saved_item$item_name[1], "Final Policy Brief"))
  stopifnot(abs(saved_item$item_weight[1] - 30) < 1e-8)
  renamed_grades <- app$db_query("SELECT COUNT(*) n FROM student_grades WHERE assignment_name='Final Policy Brief';")
  old_grades <- app$db_query("SELECT COUNT(*) n FROM student_grades WHERE assignment_name='Written Policy Brief';")
  stopifnot(renamed_grades$n[1] == 2L, old_grades$n[1] == 0L)

  session$setInputs(config_action="policy_rubrics")
  session$flushReact()
  deadlines_html <- paste(as.character(output$config_panel), collapse="")
  stopifnot(grepl("2026-11-23", deadlines_html, fixed=TRUE))
  stopifnot(grepl("2026-12-15", deadlines_html, fixed=TRUE))
  session$setInputs(policy_progress_due_input="2026-11-24",
                    policy_final_brief_due_input="2026-12-16",
                    save_policy_due_dates_btn=1)
  session$flushReact()
})

settings <- app$db_query("SELECT key,value FROM labor_settings WHERE key LIKE 'policy_%_due::%';")
stopifnot(any(settings$key == "policy_progress_due::econ 342::01" & settings$value == "2026-11-24"))
stopifnot(any(settings$key == "policy_final_brief_due::econ 342::01" & settings$value == "2026-12-16"))

app$db_exec("INSERT INTO grade_reweight_requests(user_id,from_category,to_category,points,cost,level,status) VALUES('bob@test.edu','Policy Presentation','Final Policy Brief',5,20,'assignment','approved');")
bob_grade <- app$compute_student_grade("bob@test.edu")
bob_policy <- bob_grade$items[bob_grade$items$category_name == "Policy Project",]
stopifnot(all.equal(bob_policy$item_weight, c(20,35), tolerance=1e-8) == TRUE)
stopifnot(bob_grade$overall > 85)

source_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")
stopifnot(grepl('type="button"', source_text, fixed=TRUE))
stopifnot(grepl("{priority:'event'});return false;", source_text, fixed=TRUE))
cat("critical grade reweighting and policy dates: PASS\n")
