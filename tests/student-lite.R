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
          login("gone@test.edu", "gone-pass")$res$status == 403L,
          login("prof@test.edu", "prof-pass")$res$status == 403L)
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

cat("student lite: PASS\n")
