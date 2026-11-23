#!/usr/bin/env Rscript
# Regression coverage for two measurement rules in Class Job Market:
#   1. With the per-course switch on, the wage-bid form never pre-fills or
#      displays an instructor wage before the student has bid.
#   2. With the per-course switch on, a student is paid for at most one
#      voluntary contribution per class date; later ones are recorded unpaid.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny is required for this test")

scratch <- tempfile("bid-anchor-")
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
for (tbl in c("scope_active_rounds", "section_scope_memberships", "weekly_rounds",
              "job_posts", "job_wage_bids", "live_score_events", "token_ledger"))
  app$db_exec(sprintf("DELETE FROM %s;", tbl))

people <- list(c("admin@test.edu", "Instructor", 1L), c("s1@test.edu", "Student 1", 0L),
               c("s2@test.edu", "Student 2", 0L), c("s3@test.edu", "Student 3", 0L))
for (person in people) {
  app$db_exec(
    "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
     VALUES(?,?,'',?,'ECON 101','01',1,0);", list(person[1], person[2], as.integer(person[3])))
  app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,'ECON 101','01');",
              list(person[1]))
}
class_day <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = app$get_setting("class_tz", "America/New_York")))
app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('Market class',?,'wage_bidding',1);",
            list(as.character(class_day)))
round_id <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES('econ 101::01','ECON 101','01',?);", list(round_id))
app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01','econ 101::01');")

app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Anchor test volunteer',7,'',1,0);")
vol_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Anchor test role',7,'',0,1);")
role_cat <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
             VALUES(?,'Volunteer answer',?,99,7,1,0,'volunteer',1);", list(round_id, vol_cat))
vol_post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
             VALUES(?,'Recap',?,1,7,0,1,'start',1);", list(round_id, role_cat))
role_post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
app$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
             VALUES(?,'Note taker',?,1,7,0,1,'end',1);", list(round_id, role_cat))
other_post <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])

set_admin <- function(rv) {
  rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- TRUE
  rv$admin_role <- "super_admin"; rv$active_course <- "ECON 101"
  rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
}
set_student <- function(rv, uid, name) {
  rv$authed <- TRUE; rv$user_id <- uid; rv$name <- name
  rv$course <- "ECON 101"; rv$section <- "01"; rv$is_admin <- FALSE
  rv$active_course <- "ECON 101"; rv$active_section <- "01"
  rv$active_sections <- "01"; rv$is_demo <- FALSE
}
bid_box <- function(html, post_id) {
  hit <- regmatches(html, regexpr(sprintf("<input[^>]*id=\"wbp_%d\"[^>]*>", post_id), html))
  if (!length(hit)) stop("Bid box missing for post ", post_id)
  hit
}
# Shiny renders an empty numeric box as value="NA", which browsers show blank.
box_is_empty <- function(box) !grepl("value=", box, fixed = TRUE) || grepl("value=\"NA\"", box, fixed = TRUE)

# 0. Default (switch off): existing courses keep the pre-filled posted wage
#    and the fallback volunteer wage.
shiny::testServer(app$server, {
  set_student(rv, "s1@test.edu", "Student 1")
  stopifnot(!bid_anchor_hidden_for_scope(),
            grepl("value=\"7\"", bid_box(output$jm_bid_form$html, role_post), fixed = TRUE),
            grepl("7 tokens", output$job_market_tab$html, fixed = TRUE))
})
shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(vol_clearing_rule_sel = "lowest", bid_no_anchor_input = TRUE, save_vol_clearing_btn = 1L)
  stopifnot(bid_anchor_hidden_for_scope(), !volunteer_pay_capped_for_scope())
})
stopifnot(identical(app$get_setting("bid_no_anchor::econ 101::01"), "1"), is.null(app$get_setting("bid_no_anchor")))

# 1. Switch on: empty boxes, no instructor wage on the volunteer wage list,
#    and saving an untouched form records no bid.
shiny::testServer(app$server, {
  set_student(rv, "s1@test.edu", "Student 1")
  form <- output$jm_bid_form$html
  for (post_id in c(role_post, other_post))
    stopifnot(box_is_empty(bid_box(form, post_id)))
  # Volunteer jobs inherit the cold-call wage and are not bid on. Before any
  # bid exists the instructor's fallback wage (7) must not be displayed.
  stopifnot(!grepl(sprintf("wbp_%d\"", vol_post), form, fixed = TRUE),
            !grepl("value=\"7\"", form, fixed = TRUE), !grepl("7 tokens", form, fixed = TRUE))
  market <- output$job_market_tab$html
  stopifnot(grepl("Volunteer Wages For This Class", market, fixed = TRUE),
            grepl("Set by bids", market, fixed = TRUE), !grepl("7 tokens", market, fixed = TRUE))
  args <- stats::setNames(list(NA_real_, NA_real_, 1L),
                          c(paste0("wbp_", role_post), paste0("wbp_", other_post), "submit_wage_bids"))
  do.call(session$setInputs, args)
})
stopifnot(nrow(app$db_query("SELECT id FROM job_wage_bids;")) == 0L)
shiny::testServer(app$server, {
  set_student(rv, "s1@test.edu", "Student 1")
  args <- stats::setNames(list(3, NA_real_, 1L),
                          c(paste0("wbp_", role_post), paste0("wbp_", other_post), "submit_wage_bids"))
  do.call(session$setInputs, args)
})
bids <- app$db_query("SELECT job_post_id,min_wage FROM job_wage_bids;")
stopifnot(nrow(bids) == 1L, bids$job_post_id[1] == role_post, bids$min_wage[1] == 3)
# The student's own saved bid is shown back; the unbid job stays empty.
shiny::testServer(app$server, {
  set_student(rv, "s1@test.edu", "Student 1")
  form <- output$jm_bid_form$html
  stopifnot(grepl("value=\"3\"", bid_box(form, role_post), fixed = TRUE),
            box_is_empty(bid_box(form, other_post)))
})

volunteer <- function(uid, outcome, click) shiny::testServer(app$server, {
  set_admin(rv)
  session$setInputs(part_event_type = vol_post)
  session$setInputs(part_card_click = list(user_id = uid, outcome = outcome, nonce = click))
})
events <- function(uid) app$db_query(
  "SELECT outcome,tokens FROM live_score_events WHERE user_id=? AND event_kind='participation' ORDER BY id;", list(uid))

# 2. Default (switch off): every voluntary contribution is paid, as before.
volunteer("s1@test.edu", "succeed", 1L); volunteer("s1@test.edu", "succeed", 2L)
stopifnot(identical(events("s1@test.edu")$tokens, c(7, 7)))

# 3. Switch on for this course/section through the real settings control.
shiny::testServer(app$server, {
  set_admin(rv)
  stopifnot(!volunteer_pay_capped_for_scope())
  session$setInputs(vol_clearing_rule_sel = "lowest", volunteer_paid_cap_input = TRUE,
                    save_vol_clearing_btn = 1L)
  stopifnot(volunteer_pay_capped_for_scope())
})
stopifnot(identical(app$get_setting("volunteer_paid_cap::econ 101::01"), "1"),
          is.null(app$get_setting("volunteer_paid_cap")),
          identical(app$get_setting("volunteer_clearing_rule"), "lowest"))
volunteer("s2@test.edu", "succeed", 1L); volunteer("s2@test.edu", "succeed", 2L)
volunteer("s2@test.edu", "try", 3L)
s2 <- events("s2@test.edu")
stopifnot(identical(s2$outcome, c("succeed", "succeed", "try")), identical(s2$tokens, c(7, 0, 0)))
# A miss earns nothing and does not use up the paid contribution.
volunteer("s3@test.edu", "miss", 1L); volunteer("s3@test.edu", "try", 2L)
volunteer("s3@test.edu", "succeed", 3L)
s3 <- events("s3@test.edu")
stopifnot(identical(s3$outcome, c("miss", "try", "succeed")), identical(s3$tokens, c(0, 1, 0)))
# The rule is per class date: a new date pays again.
app$db_exec("INSERT INTO live_score_events(round_id,user_id,job_post_id,event_kind,outcome,tokens,logged_by)
             VALUES(?,?,?,'participation','succeed',3,'fixture');", list(round_id + 1000L, "s3@test.edu", vol_post))
shiny::testServer(app$server, {
  set_admin(rv)
  stopifnot(volunteer_already_paid(round_id, "S2@TEST.EDU"),
            !volunteer_already_paid(round_id + 2000L, "s2@test.edu"),
            volunteer_capped_tokens(3, round_id, "s2@test.edu") == 0,
            volunteer_capped_tokens(3, round_id + 2000L, "s2@test.edu") == 3)
  # Switching off restores unlimited paid volunteering.
  session$setInputs(vol_clearing_rule_sel = "lowest", volunteer_paid_cap_input = FALSE,
                    save_vol_clearing_btn = 1L)
  stopifnot(!volunteer_pay_capped_for_scope())
})
volunteer("s2@test.edu", "succeed", 4L)
stopifnot(utils::tail(events("s2@test.edu")$tokens, 1) == 7)

cat("critical bid anchor and volunteer cap: PASS\n")
