#!/usr/bin/env Rscript
# Regression coverage for masked exports in Class Job Market: with masking on,
# no export file contains a student's name or user ID, the hashed ID is stable
# across files and sessions, and unmasked files keep their existing columns.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("masked-exports-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch)
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)

app <- new.env(parent = globalenv())
source(app_path, local = app)
app$set_setting("class_days", "")
for (tbl in c("scope_active_rounds", "scope_rounds", "section_scope_memberships", "weekly_rounds",
              "job_posts", "job_assignments", "job_wage_bids", "token_ledger", "application_bids",
              "participation_events"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))

people <- list(c("admin@test.edu", "Instructor", 1L), c("quinn.rivers@test.edu", "Quinn Rivers", 0L),
               c("Dana.Okafor@test.edu", "Dana Okafor", 0L))
for (person in people) {
  app$db_exec(
    "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
     VALUES(?,?,'',?,'ECON 101','01',1,0);", list(person[1], person[2], as.integer(person[3])))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');", list(person[1]))
}
scope <- "econ 101::01"
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('Export class','2026-11-23','wage_bidding',1);")
rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Export role',2,'',0,1);")
cat_id <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
             VALUES(?,'Recap',?,1,2,0,1,'start',1);", list(rid, cat_id))
post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
for (uid in c("quinn.rivers@test.edu", "Dana.Okafor@test.edu")) {
  app$db_exec("INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,3);", list(rid, post, uid))
  app$db_exec("INSERT INTO job_assignments(round_id,user_id,job_post_id,assigned_wage,assignment_mode,status) VALUES(?,?,?,3,'wage_bidding','assigned');", list(rid, uid, post))
  app$db_exec("INSERT INTO application_bids(round_id,category_id,user_id,tickets) VALUES(?,?,?,4);", list(rid, cat_id, uid))
  app$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note)
               SELECT user_id,display_name,'individual_adj',2,1,'Bonus for ' || display_name FROM users WHERE user_id=?;", list(uid))
  app$db_exec("INSERT INTO participation_events(round_id,user_id,event_type,tokens,note,logged_by) VALUES(?,?,'answer',1,'note','admin@test.edu');", list(rid, uid))
}

set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}
exports <- c("dl_assignments", "dl_wage_bids", "dl_app_bids", "dl_tokens", "dl_participation_events",
             "dl_extensions", "dl_reweight_requests", "dl_pubgood_contribs", "dl_flex_purchases", "dl_students")
identifying <- c("quinn", "rivers", "dana", "okafor")
read_export <- function(path) {
  text <- tolower(paste(readLines(path, warn = FALSE), collapse = "\n"))
  rows <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
  list(text = text, rows = rows)
}
collect <- function(mask) {
  found <- list()
  shiny::testServer(app$server, {
    set_admin(rv)
    if (mask) session$setInputs(export_mask_ids = TRUE)
    for (name in exports) found[[name]] <<- read_export(output[[name]])
    found[["key"]] <<- read_export(output$dl_masked_id_key)
  })
  found
}

# Unmasked files are as they were: names present, no internal ID column, and
# no hashed ID.
plain <- collect(FALSE)
stopifnot(grepl("quinn rivers", plain$dl_wage_bids$text, fixed = TRUE),
          identical(names(plain$dl_wage_bids$rows), c("round", "student", "job", "category", "min_wage", "submitted_at")),
          identical(names(plain$dl_assignments$rows), c("round", "student", "job", "wage", "assignment_mode", "created_at")),
          "note" %in% names(plain$dl_tokens$rows), "user_id" %in% names(plain$dl_tokens$rows))
for (name in exports) stopifnot(!any(c("export_uid", "masked_id") %in% names(plain[[name]]$rows)))

# Masked files carry the hashed ID and nothing that identifies a student.
masked <- collect(TRUE)
for (name in exports) {
  rows <- masked[[name]]$rows
  stopifnot(!any(vapply(identifying, grepl, logical(1), x = masked[[name]]$text, fixed = TRUE)),
            !any(c("export_uid", "user_id", "display_name", "student", "note") %in% names(rows)))
  if (nrow(rows)) stopifnot(identical(names(rows)[1], "masked_id"), all(grepl("^S[0-9a-f]{12}$", rows$masked_id)))
}
populated <- c("dl_assignments", "dl_wage_bids", "dl_app_bids", "dl_tokens", "dl_participation_events", "dl_students")
stopifnot(all(vapply(populated, function(name) nrow(masked[[name]]$rows) == 2L, logical(1))))

# One ID per student, the same in every file, matching the private key, not
# case-sensitive, and unchanged in a later session.
key <- masked$key$rows
stopifnot(nrow(key) == 2L, !anyDuplicated(key$masked_id), all(c("user_id", "display_name") %in% names(key)))
for (name in populated) stopifnot(setequal(masked[[name]]$rows$masked_id, key$masked_id))
again <- collect(TRUE)
stopifnot(identical(sort(again$dl_wage_bids$rows$masked_id), sort(masked$dl_wage_bids$rows$masked_id)))
shiny::testServer(app$server, {
  set_admin(rv)
  stopifnot(identical(masked_student_id("DANA.OKAFOR@test.edu"), masked_student_id(" dana.okafor@test.edu ")),
            masked_student_id("dana.okafor@test.edu") %in% key$masked_id,
            is.na(masked_student_id(NA)), is.na(masked_student_id("")))
})
# The ID depends on a secret stored in this database, not on the email alone.
unsalted <- paste0("S", substr(digest::digest("dana.okafor@test.edu", algo = "sha256", serialize = FALSE), 1L, 12L))
stopifnot(nchar(app$get_setting("export_mask_salt", "")) >= 32L, !unsalted %in% key$masked_id)

cat("critical masked exports: PASS\n")
