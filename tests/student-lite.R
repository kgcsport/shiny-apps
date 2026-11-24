#!/usr/bin/env Rscript
# Tests for the Student Lite demo service (student-lite/server.R). The handler
# is called directly with request objects against a scratch SQLite database.
suppressPackageStartupMessages({ library(DBI); library(RSQLite); library(jsonlite) })

scratch <- tempfile("student-lite-")
dir.create(file.path(scratch, "data"), recursive = TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = scratch, LITE_NO_SERVE = "1", LITE_DIR = normalizePath("student-lite"))
on.exit({
  if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
  unlink(scratch, recursive = TRUE)
}, add = TRUE)

source("student-lite/server.R")
hub <- lite_load_hub(normalizePath("apps/class-job-market/app.R"))
hub$set_setting("class_days", "")
hub$set_setting("bid_lock_enabled", "0")
for (tbl in c("scope_active_rounds", "scope_rounds", "section_scope_memberships", "weekly_rounds",
              "job_posts", "job_wage_bids", "token_ledger"))
  hub$db_exec(sprintf("DELETE FROM %s;", tbl))
add_user <- function(uid, name, password, admin = 0L, active = 1L, section = "01") hub$db_exec(
  "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
   VALUES(?,?,?,?,'ECON 101',?,?,0);", list(uid, name, bcrypt::hashpw(password), admin, section, active))
add_user("ana@test.edu", "Ana Test", "ana-pass")
add_user("ben@test.edu", "Ben Test", "ben-pass", section = "02")
add_user("gone@test.edu", "Gone Test", "gone-pass", active = 0L)
add_user("prof@test.edu", "Prof Test", "prof-pass", admin = 1L)
scope <- "econ 101::01"
hub$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 101','01',?);", list(scope))
hub$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('Lite class','2026-11-30','wage_bidding',1);")
rid <- as.integer(hub$db_query("SELECT last_insert_rowid() id;")$id[1])
hub$db_exec("INSERT INTO scope_rounds(scope_key,round_id) VALUES(?,?);", list(scope, rid))
hub$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES(?,'ECON 101','01',?);", list(scope, rid))
hub$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Lite role',2,'',0,1);")
role_cat <- as.integer(hub$db_query("SELECT last_insert_rowid() id;")$id[1])
new_post <- function(name, timing = "start") {
  hub$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
               VALUES(?,?,?,1,2,0,1,?,1);", list(rid, name, role_cat, timing))
  as.integer(hub$db_query("SELECT last_insert_rowid() id;")$id[1])
}
recap <- new_post("Recap"); notes <- new_post("Note taker", "end")
volunteer <- new_post("Volunteer: answer a question", "volunteer")
hub$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES('ana@test.edu','Ana Test','job',5,1,'x');")
hub$db_exec("INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES('ana@test.edu','Ana Test','extension',-2,0,'x');")

app <- lite_new_app(hub)
request <- function(method, path, body = NULL, cookie = "", headers = list()) {
  raw <- if (is.null(body)) raw(0) else charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
  req <- c(list(REQUEST_METHOD = method, PATH_INFO = path, HTTP_COOKIE = cookie,
                HTTP_X_REQUESTED_WITH = if (method == "POST") "student-lite" else "",
                rook.input = list(read = function() raw)), headers)
  res <- app$call(req)
  res$json <- if (nzchar(res$body %||% "") && grepl("json", res$headers[["Content-Type"]] %||% ""))
    jsonlite::fromJSON(as.character(res$body), simplifyVector = FALSE) else NULL
  res
}
login <- function(user, password) {
  res <- request("POST", "/api/login", list(username = user, password = password))
  list(res = res, cookie = sub(";.*$", "", res$headers[["Set-Cookie"]] %||% ""))
}

# The page and health check need no session; everything else does.
page <- request("GET", "/")
stopifnot(page$status == 200L, grepl("text/html", page$headers[["Content-Type"]]), grepl("Class Jobs", page$body, fixed = TRUE),
          request("GET", "/healthz")$status == 200L,
          request("GET", "/api/state")$status == 401L,
          request("POST", "/api/bids", list(bids = list()))$status == 401L,
          request("GET", "/nope")$status == 404L)

# Login uses the hub's own password check and account rules.
stopifnot(login("ana@test.edu", "wrong")$res$status == 401L,
          login("nobody@test.edu", "x")$res$status == 401L,
          login("gone@test.edu", "gone-pass")$res$status == 403L)
ana <- login("ANA@test.edu", "ana-pass")
stopifnot(ana$res$status == 200L, grepl("^arcade_token=[A-Za-z0-9]{48}$", ana$cookie),
          grepl("SameSite=Lax", ana$res$headers[["Set-Cookie"]], fixed = TRUE),
          grepl("Path=/;", ana$res$headers[["Set-Cookie"]], fixed = TRUE),
          nrow(hub$db_query("SELECT token FROM arcade_sessions WHERE user_id='ana@test.edu';")) == 1L)
# An unknown or malformed token is not a session, and a state-changing call
# without the page's own header is refused.
stopifnot(request("GET", "/api/state", cookie = paste0("arcade_token=", strrep("a", 48)))$status == 401L,
          request("GET", "/api/state", cookie = "arcade_token=abc")$status == 401L,
          request("GET", "/api/state", cookie = "arcade_token=' OR 1=1 --")$status == 401L)

# Shared sign-in: a token issued the way the Google login relay issues one is
# accepted here without a second login, among other cookies, for students only.
google_token <- hub$make_token()
hub$store_token(google_token, "ben@test.edu")
shared <- request("GET", "/api/state", cookie = paste0("other=1; arcade_token=", google_token, "; theme=dark"))
prof_token <- hub$make_token(); hub$store_token(prof_token, "prof@test.edu")
stopifnot(shared$status == 200L, shared$json$student$name == "Ben Test",
          request("GET", "/api/state", cookie = paste0("arcade_token=", prof_token))$status == 401L,
          identical(request("GET", "/api/config")$json$google, isTRUE(hub$GOOGLE_AUTH_ENABLED)))
no_header <- app$call(list(REQUEST_METHOD = "POST", PATH_INFO = "/api/bids", HTTP_COOKIE = ana$cookie,
                           rook.input = list(read = function() charToRaw('{"bids":{}}'))))
stopifnot(no_header$status == 400L)

# State: wallet, the selected class date, and only the jobs a student bids on.
state <- request("GET", "/api/state", cookie = ana$cookie)
jobs <- vapply(state$json$jobs, function(j) j$job_name, character(1))
stopifnot(state$status == 200L, state$json$student$name == "Ana Test",
          state$json$wallet$earned == 5, state$json$wallet$on_hand == 3,
          state$json$round$label == "Lite class", isTRUE(state$json$bidding$open),
          setequal(jobs, c("Recap", "Note taker")),
          all(vapply(state$json$jobs, function(j) is.null(j$my_bid) && j$posted_wage == 2, logical(1))))
# Unchanged state costs nothing to re-send.
again <- request("GET", "/api/state", cookie = ana$cookie, headers = list(HTTP_IF_NONE_MATCH = state$headers[["ETag"]]))
stopifnot(again$status == 304L, !nzchar(again$body))

# Bids: saved through the same table and rules; blanks, volunteer jobs, other
# dates' jobs, and nonsense are ignored.
bids <- stats::setNames(list(3.5, NULL, 9, -1, "abc"), c(recap, notes, volunteer, 999999, "x"))
saved <- request("POST", "/api/bids", list(bids = bids), cookie = ana$cookie)
rows <- hub$db_query("SELECT user_id, job_post_id, min_wage FROM job_wage_bids;")
stopifnot(saved$status == 200L, saved$json$saved == 1L, nrow(rows) == 1L,
          rows$user_id == "ana@test.edu", rows$job_post_id == recap, rows$min_wage == 3.5)
mine <- Filter(function(j) j$job_name == "Recap", saved$json$state$jobs)[[1]]
stopifnot(mine$my_bid == 3.5)
request("POST", "/api/bids", list(bids = stats::setNames(list(1), recap)), cookie = ana$cookie)
stopifnot(hub$db_query("SELECT min_wage FROM job_wage_bids;")$min_wage == 1,
          nrow(hub$db_query("SELECT id FROM job_wage_bids;")) == 1L)

# A student in another section has no access to this section's class date.
ben <- login("ben@test.edu", "ben-pass")
ben_state <- request("GET", "/api/state", cookie = ben$cookie)
ben_save <- request("POST", "/api/bids", list(bids = stats::setNames(list(1), recap)), cookie = ben$cookie)
stopifnot(ben_state$status == 200L, nrow(hub$db_query("SELECT id FROM job_wage_bids WHERE user_id='ben@test.edu';")) == 0L,
          !isTRUE(ben_save$json$saved > 0))

# Closed bidding is enforced by the hub's rule, on the server.
hub$db_exec("UPDATE weekly_rounds SET bidding_enabled=0 WHERE id=?;", list(rid))
closed <- request("POST", "/api/bids", list(bids = stats::setNames(list(8), recap)), cookie = ana$cookie)
stopifnot(closed$status == 409L, identical(closed$json$ok, FALSE),
          hub$db_query("SELECT min_wage FROM job_wage_bids;")$min_wage == 1,
          !isTRUE(request("GET", "/api/state", cookie = ana$cookie)$json$bidding$open))
hub$db_exec("UPDATE weekly_rounds SET bidding_enabled=1, assignment_mode='random' WHERE id=?;", list(rid))
stopifnot(request("POST", "/api/bids", list(bids = stats::setNames(list(8), recap)), cookie = ana$cookie)$status == 409L)
hub$db_exec("UPDATE weekly_rounds SET assignment_mode='wage_bidding' WHERE id=?;", list(rid))

# With the no-anchor switch on, the posted wage is never sent to the browser.
hub$set_setting(paste0("bid_no_anchor::", scope), "1")
quiet <- request("GET", "/api/state", cookie = ana$cookie)
stopifnot(isTRUE(quiet$json$no_anchor), all(vapply(quiet$json$jobs, function(j) is.null(j$posted_wage), logical(1))),
          !grepl("posted_wage\":2", as.character(quiet$body), fixed = TRUE))

# Signing out ends the shared session; expired sessions and archived students
# lose access at once.
out <- request("POST", "/api/logout", list(), cookie = paste0("arcade_token=", google_token))
stopifnot(out$status == 200L, grepl("Max-Age=0", out$headers[["Set-Cookie"]], fixed = TRUE),
          nrow(hub$db_query("SELECT token FROM arcade_sessions WHERE token=?;", list(google_token))) == 0L,
          request("GET", "/api/state", cookie = paste0("arcade_token=", google_token))$status == 401L)
hub$db_exec("UPDATE arcade_sessions SET expires_at=datetime('now','-1 minute') WHERE user_id='ana@test.edu';")
stopifnot(request("GET", "/api/state", cookie = ana$cookie)$status == 401L)
hub$db_exec("UPDATE arcade_sessions SET expires_at=datetime('now','+1 day') WHERE user_id='ana@test.edu';")
stopifnot(request("GET", "/api/state", cookie = ana$cookie)$status == 200L)
hub$db_exec("UPDATE users SET active=0 WHERE user_id='ana@test.edu';")
stopifnot(request("GET", "/api/state", cookie = ana$cookie)$status == 401L)

# ── Instructor tracker ───────────────────────────────────────────────────────
# Fresh fixtures: two sections, a cold-call job, a volunteer job, one assigned job.
hub$db_exec("UPDATE users SET active=1 WHERE user_id='ana@test.edu';")
add_user("cy@test.edu", "Cy Test", "cy-pass")
add_user("ta@test.edu", "Ta Test", "ta-pass", admin = 1L)
hub$grant_admin_assignment("prof@test.edu", "super_admin", assigned_by = "test")
hub$grant_admin_assignment("ta@test.edu", "section_admin", course = "ECON 101", section = "02", assigned_by = "test")
hub$db_exec("INSERT INTO job_categories(name,default_wage,description,voluntary,in_draw) VALUES('Lite volunteer',2,'',1,0);")
vol_cat <- as.integer(hub$db_query("SELECT last_insert_rowid() id;")$id[1])
hub$db_exec("INSERT INTO job_posts(round_id,job_name,category_id,slots,wage_override,voluntary,in_draw,selection_time,active)
             VALUES(?,'Volunteer: answer',?,99,2,1,0,'volunteer',1);", list(rid, vol_cat))
vol_post <- as.integer(hub$db_query("SELECT last_insert_rowid() id;")$id[1])
cold_post <- new_post("Cold call: answer a question", "during")
hub$db_exec("INSERT INTO job_assignments(round_id,user_id,job_post_id,assigned_wage,assignment_mode,status)
             VALUES(?,'ana@test.edu',?,3,'wage_bidding','assigned');", list(rid, recap))
ana_job <- as.integer(hub$db_query("SELECT last_insert_rowid() id;")$id[1])
hub$db_exec("DELETE FROM live_score_events;")
hub$set_setting(paste0("bid_no_anchor::", scope), "0")

prof <- login("prof@test.edu", "prof-pass"); ta <- login("ta@test.edu", "ta-pass"); cy <- login("cy@test.edu", "cy-pass")
stopifnot(prof$res$status == 200L, ta$res$status == 200L)
act <- function(who, body, section = "01") request("POST", "/api/tracker/act",
  c(list(course = "ECON 101", section = section), body), cookie = who$cookie)
tracker_state_for <- function(who, section = "01") {
  req <- list(REQUEST_METHOD = "GET", PATH_INFO = "/api/tracker/state", HTTP_COOKIE = who$cookie,
              QUERY_STRING = paste0("course=ECON+101&section=", section), rook.input = list(read = function() raw(0)))
  res <- app$call(req)
  res$json <- if (nzchar(res$body %||% "")) jsonlite::fromJSON(as.character(res$body), simplifyVector = FALSE) else NULL
  res
}
events <- function() hub$db_query("SELECT user_id,event_kind,outcome,tokens,committed_at,job_assignment_id FROM live_score_events ORDER BY id;")

# Who may use it: the page loads for anyone, the data only for instructors, and
# only for sections they run.
stopifnot(request("GET", "/tracker")$status == 200L, grepl("Class Tracker", request("GET", "/tracker")$body, fixed = TRUE),
          request("GET", "/api/tracker/classes")$status == 401L,
          request("GET", "/api/tracker/classes", cookie = cy$cookie)$status == 401L,
          tracker_state_for(cy)$status == 401L, act(cy, list(action = "draw"))$status == 401L,
          request("GET", "/api/state", cookie = prof$cookie)$status == 401L)
classes <- function(who) vapply(request("GET", "/api/tracker/classes", cookie = who$cookie)$json$classes,
                                function(c) paste(c$course, c$section), character(1))
# A super admin sees every section, including the built-in demo course; a
# section instructor sees only their own.
stopifnot(all(c("ECON 101 01", "ECON 101 02") %in% classes(prof)), identical(classes(ta), "ECON 101 02"),
          tracker_state_for(ta, "01")$status == 403L, act(ta, list(action = "draw"), "01")$status == 403L,
          tracker_state_for(ta, "02")$status == 200L)

# State for the section: its own students, the volunteer job, the assigned job.
state <- tracker_state_for(prof)$json
names_in <- vapply(state$students, function(s) s$name, character(1))
stopifnot(setequal(names_in, c("Ana Test", "Cy Test")), state$round$label == "Lite class", state$pending == 0L,
          length(state$volunteer_jobs) == 1L, state$volunteer_jobs[[1]]$job_post_id == vol_post,
          length(state$assignments) == 1L, state$assignments[[1]]$name == "Ana Test", state$assignments[[1]]$wage == 3)

# Drawing a cold call freezes the date's wages and closes bidding, as the main
# tracker does, and only picks a present student from this section.
drawn <- act(prof, list(action = "draw"))
stopifnot(drawn$status == 200L, drawn$json$drawn$user_id %in% c("ana@test.edu", "cy@test.edu"),
          nrow(hub$db_query("SELECT wage FROM class_wage_snapshots WHERE round_id=?;", list(rid))) > 0L,
          hub$db_query("SELECT bidding_enabled v FROM weekly_rounds WHERE id=?;", list(rid))$v[1] != 1L)
stopifnot(act(prof, list(action = "absent", user_id = "cy@test.edu"))$status == 200L)
for (i in 1:10) stopifnot(act(prof, list(action = "draw"))$json$drawn$user_id == "ana@test.edu")
stopifnot(act(prof, list(action = "draw", exclude = list("ana@test.edu")))$status == 409L)

# Recording: the amounts come from the hub's own rules and wait in the audit.
wage <- hub$cold_call_wage_for(rid, "answer")
stopifnot(act(prof, list(action = "cold_call", user_id = "ana@test.edu", kind = "answer"))$status == 200L,
          act(prof, list(action = "cold_call", user_id = "ana@test.edu", kind = "pass"))$status == 200L,
          act(prof, list(action = "cold_call", user_id = "ben@test.edu", kind = "answer"))$status == 409L,
          act(prof, list(action = "cold_call", user_id = "ana@test.edu", kind = "dance"))$status == 409L)
log <- events()
stopifnot(nrow(log) == 2L, log$outcome[1] == "answer", log$tokens[1] == wage, is.na(log$committed_at[1]),
          log$outcome[2] == "pass", log$tokens[2] == 0, !is.na(log$committed_at[2]))
# A pass by an insured student is sent to the main tracker, so the claim is paid there.
hub$db_exec("INSERT INTO insurance_products(scope_key,round_id,benefit,premium,status,price_step) VALUES(?,?,2,1,'open',0.5);", list(scope, rid))
product <- as.integer(hub$db_query("SELECT last_insert_rowid() id;")$id[1])
hub$db_exec("INSERT INTO insurance_purchases(product_id,user_id,premium) VALUES(?,'ana@test.edu',1);", list(product))
insured <- act(prof, list(action = "cold_call", user_id = "ana@test.edu", kind = "pass"))
stopifnot(insured$status == 409L, grepl("Pass Insurance", insured$json$message, fixed = TRUE), nrow(events()) == 2L)

# Voluntary contributions, with and without the one-paid-per-class limit.
stopifnot(act(prof, list(action = "volunteer", user_id = "ana@test.edu", job_post_id = vol_post, outcome = "succeed"))$status == 200L,
          act(prof, list(action = "volunteer", user_id = "ana@test.edu", job_post_id = vol_post, outcome = "succeed"))$status == 200L,
          act(prof, list(action = "volunteer", user_id = "ana@test.edu", job_post_id = recap, outcome = "succeed"))$status == 409L)
vol <- events()[events()$event_kind == "participation", ]
stopifnot(nrow(vol) == 2L, all(vol$tokens > 0), vol$tokens[1] == vol$tokens[2])
hub$set_setting(paste0("volunteer_paid_cap::", scope), "1")
limited <- act(prof, list(action = "volunteer", user_id = "ana@test.edu", job_post_id = vol_post, outcome = "succeed"))
stopifnot(limited$status == 200L, grepl("unpaid", limited$json$message, fixed = TRUE), isTRUE(limited$json$state$paid_volunteer_limit),
          utils::tail(events()$tokens, 1) == 0)

# Scoring an assigned job: once, at the wage on the assignment, this section only.
scored <- act(prof, list(action = "assignment", assignment_id = ana_job, outcome = "complete"))
again <- act(prof, list(action = "assignment", assignment_id = ana_job, outcome = "missed"))
stopifnot(scored$status == 200L, scored$json$state$assignments[[1]]$queued == "complete", again$status == 409L,
          act(ta, list(action = "assignment", assignment_id = ana_job, outcome = "complete"), "02")$status == 409L)
job_event <- events()[!is.na(events()$job_assignment_id), ]
stopifnot(nrow(job_event) == 1L, job_event$tokens == 3, is.na(job_event$committed_at),
          scored$json$state$pending == nrow(events()[is.na(events()$committed_at), ]),
          nrow(hub$db_query("SELECT id FROM token_ledger WHERE source_type IN ('job','participation') AND note <> 'x';")) == 0L)

cat("student lite: PASS\n")
