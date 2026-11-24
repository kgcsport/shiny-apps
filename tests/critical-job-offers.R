#!/usr/bin/env Rscript
# Regression coverage for offering an unfilled job to the class on a
# random-wage date in Class Job Market: the offer, everyone present, their bid
# at that moment, and their response are recorded; a taker is assigned at the
# drawn wage; and the export carries it, masked or not.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("job-offers-")
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
app$set_setting("bid_lock_enabled", "0")
for (tbl in c("scope_active_rounds", "scope_rounds", "section_scope_memberships", "weekly_rounds",
              "job_posts", "job_assignments", "job_wage_bids", "class_wage_snapshots",
              "round_absences", "job_offers", "job_offer_audience", "token_ledger"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))
students <- paste0("s", 1:5, "@test.edu")
people <- c("admin@test.edu", students, "other@test.edu")
for (uid in people) {
  app$db_exec(
    "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
     VALUES(?,?,'',?,'ECON 101',?,1,0);",
    list(uid, paste("Person", sub("@.*", "", uid)), as.integer(uid == "admin@test.edu"),
         if (uid == "other@test.edu") "02" else "01"))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101',?);",
              list(uid, if (uid == "other@test.edu") "02" else "01"))
}
scope <- "econ 101::01"
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Offer role',4,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
class_day <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = app$get_setting("class_tz", "America/New_York")))
new_date <- function(offset, rule) {
  app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled,wage_pricing_rule,allow_multiple_jobs)
               VALUES(?,?,'wage_bidding',1,?,1);", list(paste("Date", offset), as.character(class_day + offset), rule))
  rid <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
  app$db_exec("DELETE FROM scope_active_rounds;")
  app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
  app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
               VALUES(?,'Recap',?,2,4,0,1,'start',1);", list(rid, role_cat))
  post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  # Everyone bids far above anything the job can pay, except s5, who does not bid.
  for (uid in students[1:4])
    app$db_exec("INSERT INTO job_wage_bids(round_id,job_post_id,user_id,min_wage) VALUES(?,?,?,?);",
                list(rid, post, uid, 100 + match(uid, students)))
  list(rid = rid, post = post)
}
set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}
record_offer <- function(post, taker = "", also = character(0), click = 1L) shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(offer_post_id = post, offer_taker_uid = taker, offer_also_uids = also, offer_record_btn = click)
})
run_draw <- function() shiny::testServer(app$server, {
  set_admin(rv); session$setInputs(draw_timing_filter = "all", run_draw_btn = 1L)
})
offers <- function() app$db_query("SELECT * FROM job_offers ORDER BY id;")
audience <- function(offer_id) app$db_query(
  "SELECT user_id, bid_at_offer, response FROM job_offer_audience WHERE offer_id=? ORDER BY user_id;", list(offer_id))

# 1. Offers belong to random-wage dates, after the draw has fixed a wage.
plain <- new_date(1, "pay_as_bid")
record_offer(plain$post, students[1])
d <- new_date(2, "random_wage")
record_offer(d$post, students[1])
stopifnot(nrow(offers()) == 0L, nrow(app$db_query("SELECT id FROM job_assignments;")) == 0L)

run_draw()
wage <- app$db_query("SELECT wage FROM class_wage_snapshots WHERE round_id=? AND job_post_id=?;", list(d$rid, d$post))$wage[1]
stopifnot(wage <= 4, nrow(app$db_query("SELECT id FROM job_assignments WHERE round_id=?;", list(d$rid))) == 0L)
shiny::testServer(app$server, {
  set_admin(rv)
  panel <- output$job_offer_panel$html
  stopifnot(grepl("Offer A Job To The Class", panel, fixed = TRUE), grepl("0 of 2 filled", panel, fixed = TRUE))
})

# 2. One student steps in and another also volunteers. s3 is absent.
app$db_exec("INSERT INTO round_absences(round_id,user_id) VALUES(?,?);", list(d$rid, students[3]))
record_offer(d$post, students[1], also = c(students[2], students[1]))
first <- offers()
seen <- audience(first$id[1])
stopifnot(nrow(first) == 1L, first$wage[1] == wage, first$taken_by[1] == students[1], first$scope_key[1] == scope,
          # everyone present in this section, and nobody else
          setequal(seen$user_id, students[c(1, 2, 4, 5)]),
          seen$response[seen$user_id == students[1]] == "took",
          seen$response[seen$user_id == students[2]] == "volunteered",
          all(seen$response[seen$user_id %in% students[4:5]] == "passed"),
          # each student's bid at that moment; s5 never bid
          seen$bid_at_offer[seen$user_id == students[1]] == 101,
          seen$bid_at_offer[seen$user_id == students[4]] == 104,
          is.na(seen$bid_at_offer[seen$user_id == students[5]]))
taken <- app$db_query("SELECT id, user_id, assigned_wage, assignment_mode, status FROM job_assignments WHERE round_id=?;", list(d$rid))
stopifnot(nrow(taken) == 1L, taken$user_id[1] == students[1], taken$assigned_wage[1] == wage,
          identical(taken$assignment_mode[1], "volunteer_fill"), first$assignment_id[1] == taken$id[1])
# A later bid change does not rewrite what was recorded.
app$db_exec("UPDATE job_wage_bids SET min_wage=1 WHERE round_id=? AND user_id=?;", list(d$rid, students[4]))
stopifnot(audience(first$id[1])$bid_at_offer[3] == 104)

# 3. The same student cannot take the same job twice, and a student from
#    another section or one marked absent cannot be recorded as the taker.
record_offer(d$post, students[1], click = 2L)
record_offer(d$post, "other@test.edu", click = 3L)
record_offer(d$post, students[3], click = 4L)
stopifnot(nrow(offers()) == 1L)

# 4. An offer nobody takes is still recorded for everyone present.
record_offer(d$post, "", click = 5L)
second <- offers()[2, ]
passed <- audience(second$id)
stopifnot(is.na(second$taken_by), is.na(second$assignment_id), nrow(passed) == 4L, all(passed$response == "passed"),
          passed$bid_at_offer[passed$user_id == students[4]] == 1,
          nrow(app$db_query("SELECT id FROM job_assignments WHERE round_id=?;", list(d$rid))) == 1L)

# 5. A student whose bid was at or below the wage and takes the offer is an
#    ordinary assignment, not a volunteer fill.
app$db_exec("UPDATE job_wage_bids SET min_wage=0.5 WHERE round_id=? AND user_id=?;", list(d$rid, students[2]))
record_offer(d$post, students[2], click = 6L)
stopifnot(identical(app$db_query("SELECT assignment_mode FROM job_assignments WHERE round_id=? AND user_id=?;",
                                 list(d$rid, students[2]))$assignment_mode, "manual"))

# 6. The export has one row per offer per student present, and masks cleanly.
read_export <- function(mask) {
  result <- NULL
  shiny::testServer(app$server, {
    set_admin(rv)
    if (mask) session$setInputs(export_mask_ids = TRUE)
    path <- output$dl_job_offers   # the file exists only while the session does
    result <<- list(rows = utils::read.csv(path, stringsAsFactors = FALSE),
                    text = tolower(paste(readLines(path, warn = FALSE), collapse = "\n")))
  })
  result
}
open_file <- read_export(FALSE)
stopifnot(nrow(open_file$rows) == 12L, !"export_uid" %in% names(open_file$rows),
          all(c("offer_id", "job", "offered_wage", "student", "bid_at_offer", "response") %in% names(open_file$rows)),
          sum(open_file$rows$response == "took") == 2L, grepl("person s1", open_file$text, fixed = TRUE))
masked <- read_export(TRUE)
stopifnot(nrow(masked$rows) == 12L, identical(names(masked$rows)[1], "masked_id"),
          !any(c("student", "export_uid", "user_id") %in% names(masked$rows)),
          !grepl("person s", masked$text, fixed = TRUE), !grepl("@test.edu", masked$text, fixed = TRUE),
          length(unique(masked$rows$masked_id)) == 4L)

cat("critical job offers: PASS\n")
