#!/usr/bin/env Rscript
# Student Lite: a small HTTP service that gives students a fast page for the
# two things they do most in Class Job Market, checking their points and
# entering wage bids, without opening a Shiny session.
#
# It is a demo, not a replacement. It runs beside the Shiny app against the
# same SQLite database and loads the Shiny app's own code, so the rules that
# decide whether bidding is open, which jobs can be bid on, and how a password
# is checked are the app's functions, not a second copy of them.
#
# Run:  Rscript student-lite/server.R        (LITE_PORT, default 8090)
# The database is found the same way the Shiny app finds it
# (CONNECT_CONTENT_DIR or /srv/shiny-server/appdata).

suppressPackageStartupMessages({ library(httpuv); library(jsonlite) })

lite_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  configured <- Sys.getenv("LITE_DIR", "")
  if (nzchar(configured)) configured
  else if (length(file_arg)) dirname(normalizePath(file_arg[1]))
  else if (dir.exists("student-lite")) normalizePath("student-lite")
  else getwd()
})

lite_hub_path <- function() {
  candidates <- c(Sys.getenv("LITE_HUB_APP", ""),
                  file.path(lite_dir, "..", "apps", "class-job-market", "app.R"),
                  "/srv/shiny-server/class-job-market/app.R")
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(hit)) stop("Cannot find the Class Job Market app.R; set LITE_HUB_APP.")
  normalizePath(hit[1])
}

# Load the hub's code once. `hub` then holds its database helpers and rules.
lite_load_hub <- function(path = lite_hub_path()) {
  hub <- new.env(parent = globalenv())
  source(path, local = hub, chdir = TRUE)
  hub
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

lite_json <- function(body, status = 200L, headers = list()) list(
  status = as.integer(status),
  headers = c(list("Content-Type" = "application/json; charset=utf-8",
                   "Cache-Control" = "no-store"), headers),
  body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", na = "null", digits = NA))

lite_new_app <- function(hub, secure_cookie = FALSE) {
  q <- hub$db_query
  # Sessions are the hub's own. After Google sign-in the hub's login relay
  # stores a token in arcade_sessions and sets it as the arcade_token cookie for
  # the whole site; this service accepts that same cookie, so a student who is
  # signed in to the Shiny app is signed in here, and the reverse. A password
  # login issues the same kind of token.
  session_seconds <- as.integer(hub$SESSION_DAYS %||% 14L) * 86400L
  token_cookie <- function(token) paste0(
    "arcade_token=", utils::URLencode(token, reserved = TRUE), "; Path=/; SameSite=Lax; Max-Age=", session_seconds,
    if (secure_cookie) "; Secure" else "")
  clear_cookie <- "arcade_token=; Path=/; SameSite=Lax; Max-Age=0"
  session_token <- function(req) {
    cookies <- strsplit(req$HTTP_COOKIE %||% "", ";\\s*")[[1]]
    value <- sub("^arcade_token=", "", cookies[startsWith(cookies, "arcade_token=")])
    if (!length(value)) return("")
    token <- utils::URLdecode(value[1])
    if (grepl("^[A-Za-z0-9]{20,128}$", token)) token else ""
  }
  session_user <- function(req) {
    row <- hub$lookup_token(session_token(req))
    if (!nrow(row)) return(NULL)
    if (as.integer(row$active[1] %||% 1L) == 0L || as.integer(row$is_admin[1] %||% 0L) == 1L ||
        as.integer(row$is_demo[1] %||% 0L) == 1L) return(NULL)
    row[1, c("user_id", "display_name", "course", "section"), drop = FALSE]
  }
  read_body <- function(req) {
    raw <- tryCatch(req$rook.input$read(), error = function(e) raw(0))
    if (length(raw) > 20000L) return(NULL)
    tryCatch(jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE), error = function(e) NULL)
  }

  # The student's course/section scope and the class date that scope has
  # selected, resolved the same way the hub resolves them for a student.
  scope_key <- function(user) {
    mapped <- q("SELECT scope_key FROM section_scope_memberships WHERE course_key=? AND section_key=? LIMIT 1;",
                list(hub$norm_key(user$course), hub$norm_key(user$section)))
    if (nrow(mapped) && nzchar(mapped$scope_key[1] %||% "")) mapped$scope_key[1]
    else hub$section_scope_key(user$course, user$section)
  }
  scoped_setting <- function(key, scope, default) hub$get_setting(paste0(key, "::", scope), hub$get_setting(key, default))
  round_row <- function(scope) {
    mapped <- q("SELECT sar.round_id FROM scope_active_rounds sar JOIN weekly_rounds wr ON wr.id=sar.round_id
                 WHERE sar.scope_key=? LIMIT 1;", list(scope))
    rid <- if (nrow(mapped)) as.integer(mapped$round_id[1]) else hub$active_round_id()
    if (is.na(rid)) return(data.frame())
    # A section with no selected date falls back to the app-wide date, but
    # never to a date that belongs to a different course/section.
    owners <- q("SELECT scope_key FROM scope_rounds WHERE round_id=?;", list(rid))$scope_key
    if (!nrow(mapped) && length(owners) && !scope %in% owners) return(data.frame())
    q("SELECT * FROM weekly_rounds WHERE id=?;", list(rid))
  }
  # Jobs a student bids on: every active post for the date except volunteer
  # jobs, which inherit the cold-call wage. Same query and filter as the hub.
  wage_posts <- function(rid) {
    posts <- q("SELECT jp.id AS job_post_id, jp.job_name, jp.category_id,
                       COALESCE(jp.wage_override, jc.default_wage, 0) AS default_wage,
                       COALESCE(jp.description, jc.description, '') AS description,
                       COALESCE(NULLIF(jp.selection_time,''), NULLIF(jc.selection_time,''), 'any') AS selection_time,
                       jc.name AS category_name
                FROM job_posts jp LEFT JOIN job_categories jc ON jc.id=jp.category_id
                WHERE jp.round_id=? AND COALESCE(jp.active,1)=1
                ORDER BY jp.display_order, jp.job_name;", list(rid))
    if (!nrow(posts)) return(posts)
    volunteer <- tolower(trimws(as.character(posts$category_name %||% ""))) == "volunteer" |
      tolower(trimws(as.character(posts$selection_time %||% ""))) == "volunteer" |
      grepl("^volunteer", tolower(as.character(posts$job_name %||% "")))
    posts[!volunteer, , drop = FALSE]
  }
  bidding_status <- function(round) {
    if (!nrow(round)) return(list(open = FALSE, message = "No class date is selected yet."))
    if (!identical(as.character(round$assignment_mode[1] %||% "random"), "wage_bidding"))
      return(list(open = FALSE, message = "This class date is not accepting wage bids."))
    window <- hub$round_bid_window_status(round[1, ], tz = hub$get_setting("class_tz", "America/New_York"))
    lock <- hub$bid_lock_status()
    if (isTRUE(window$scheduled) && isTRUE(lock$locked)) return(list(open = FALSE, message = lock$locked_msg))
    if (!isTRUE(window$open)) return(list(open = FALSE, message = "The wage-bidding window is closed."))
    list(open = TRUE, message = "Bidding is open. Your last saved bids are used when bidding locks.")
  }
  state <- function(user) {
    scope <- scope_key(user)
    round <- round_row(scope)
    rid <- if (nrow(round)) as.integer(round$id[1]) else NA_integer_
    posts <- if (!is.na(rid) && identical(as.character(round$assignment_mode[1] %||% ""), "wage_bidding"))
      wage_posts(rid) else data.frame()
    mine <- if (!is.na(rid)) q(
      "SELECT jp.id AS job_post_id, COALESCE(jwb.min_wage, wb.min_wage) AS min_wage
       FROM job_posts jp
       LEFT JOIN job_wage_bids jwb ON jwb.round_id=jp.round_id AND jwb.job_post_id=jp.id AND jwb.user_id=?
       LEFT JOIN wage_bids wb ON wb.round_id=jp.round_id AND wb.category_id=jp.category_id AND wb.user_id=?
       WHERE jp.round_id=?;", list(user$user_id, user$user_id, rid)) else data.frame()
    no_anchor <- identical(as.character(scoped_setting("bid_no_anchor", scope, "0")), "1")
    wallet <- q("SELECT COALESCE(SUM(CASE WHEN earning=1 AND amount>0 THEN amount ELSE 0 END),0) earned,
                        COALESCE(SUM(amount),0) on_hand FROM token_ledger WHERE user_id=?;", list(user$user_id))
    jobs <- lapply(seq_len(nrow(posts)), function(i) {
      post <- posts[i, ]
      bid <- mine$min_wage[mine$job_post_id == post$job_post_id]
      bid <- if (length(bid) && !is.na(bid[1])) as.numeric(bid[1]) else NULL
      list(job_post_id = as.integer(post$job_post_id), job_name = post$job_name,
           category = post$category_name %||% "", timing = post$selection_time %||% "any",
           description = post$description %||% "", my_bid = bid,
           # With the no-anchor switch on, the posted wage is never sent.
           posted_wage = if (no_anchor) NULL else as.numeric(post$default_wage))
    })
    list(student = list(name = user$display_name, course = user$course, section = user$section),
         wallet = list(earned = as.numeric(wallet$earned[1]), on_hand = as.numeric(wallet$on_hand[1])),
         round = if (nrow(round)) list(label = round$label[1] %||% "", class_date = round$class_date[1] %||% "") else NULL,
         bidding = bidding_status(round), no_anchor = no_anchor, jobs = jobs)
  }
  save_bids <- function(user, bids) {
    scope <- scope_key(user)
    round <- round_row(scope)
    status <- bidding_status(round)
    if (!isTRUE(status$open)) return(list(ok = FALSE, message = status$message))
    rid <- as.integer(round$id[1])
    allowed <- as.integer(wage_posts(rid)$job_post_id)
    saved <- 0L
    for (key in names(bids)) {
      post_id <- suppressWarnings(as.integer(key))
      value <- suppressWarnings(as.numeric(bids[[key]] %||% NA_real_))
      if (is.na(post_id) || !post_id %in% allowed || length(value) != 1L || !is.finite(value) || value < 0) next
      hub$db_exec(
        "INSERT INTO job_wage_bids(round_id, job_post_id, user_id, min_wage) VALUES(?,?,?,?)
         ON CONFLICT(round_id, job_post_id, user_id)
         DO UPDATE SET min_wage=excluded.min_wage, submitted_at=CURRENT_TIMESTAMP;",
        list(rid, post_id, user$user_id, value))
      saved <- saved + 1L
    }
    list(ok = TRUE, message = sprintf("Saved %d wage bid%s.", saved, if (saved == 1L) "" else "s"), saved = saved)
  }

  index_path <- file.path(lite_dir, "public", "index.html")
  handle <- function(req) {
    path <- req$PATH_INFO %||% "/"
    method <- toupper(req$REQUEST_METHOD %||% "GET")
    if (method == "GET" && path %in% c("/", "/index.html")) return(list(
      status = 200L,
      headers = list("Content-Type" = "text/html; charset=utf-8", "Cache-Control" = "no-cache",
                     "X-Content-Type-Options" = "nosniff", "Referrer-Policy" = "same-origin",
                     "Content-Security-Policy" = "default-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline'; frame-ancestors 'none'"),
      body = paste(readLines(index_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
    if (method == "GET" && path == "/healthz") return(lite_json(list(ok = TRUE)))
    if (!startsWith(path, "/api/")) return(lite_json(list(error = "Not found"), 404L))
    # State-changing calls must come from this page's own script.
    if (method == "POST" && !identical(req$HTTP_X_REQUESTED_WITH %||% "", "student-lite"))
      return(lite_json(list(error = "Bad request"), 400L))

    if (method == "POST" && path == "/api/login") {
      body <- read_body(req)
      username <- trimws(as.character(body$username %||% ""))
      password <- as.character(body$password %||% "")
      row <- if (nzchar(username)) q(
        "SELECT user_id, pw_hash, COALESCE(active,1) active, COALESCE(is_admin,0) is_admin, COALESCE(is_demo,0) is_demo
         FROM users WHERE LOWER(user_id)=LOWER(?);", list(username)) else data.frame()
      ok <- nrow(row) == 1L && nzchar(password) &&
        isTRUE(tryCatch(bcrypt::checkpw(password, row$pw_hash[1]), error = function(e) FALSE))
      if (!ok) { Sys.sleep(0.4); return(lite_json(list(error = "Incorrect username or password."), 401L)) }
      if (as.integer(row$active[1]) == 0L)
        return(lite_json(list(error = "This account has been archived. Contact your instructor."), 403L))
      if (as.integer(row$is_admin[1]) == 1L || as.integer(row$is_demo[1]) == 1L)
        return(lite_json(list(error = "Instructor and demo accounts use the main app."), 403L))
      token <- hub$make_token()
      hub$store_token(token, row$user_id[1])
      return(lite_json(list(ok = TRUE), headers = list("Set-Cookie" = token_cookie(token))))
    }
    if (method == "POST" && path == "/api/logout") {
      # Signing out ends the shared session, here and in the Shiny app.
      hub$delete_token(session_token(req))
      return(lite_json(list(ok = TRUE), headers = list("Set-Cookie" = clear_cookie)))
    }
    if (method == "GET" && path == "/api/config")
      return(lite_json(list(google = isTRUE(hub$GOOGLE_AUTH_ENABLED))))

    user <- session_user(req)
    if (is.null(user)) return(lite_json(list(error = "Sign in first."), 401L))
    if (method == "GET" && path == "/api/state") {
      body <- state(user)
      etag <- paste0('"', digest::digest(body, algo = "md5"), '"')
      if (identical(req$HTTP_IF_NONE_MATCH %||% "", etag))
        return(list(status = 304L, headers = list("ETag" = etag, "Cache-Control" = "no-store"), body = ""))
      return(lite_json(body, headers = list("ETag" = etag)))
    }
    if (method == "POST" && path == "/api/bids") {
      body <- read_body(req)
      if (is.null(body) || !is.list(body$bids)) return(lite_json(list(error = "Bad request"), 400L))
      result <- save_bids(user, body$bids)
      return(lite_json(c(result, list(state = state(user))), if (isTRUE(result$ok)) 200L else 409L))
    }
    lite_json(list(error = "Not found"), 404L)
  }
  list(call = function(req) tryCatch(handle(req), error = function(e) {
    message("student-lite error: ", conditionMessage(e))
    lite_json(list(error = "Something went wrong."), 500L)
  }))
}

if (identical(environment(), globalenv()) && !interactive() && !nzchar(Sys.getenv("LITE_NO_SERVE", ""))) {
  port <- suppressWarnings(as.integer(Sys.getenv("LITE_PORT", "8090")))
  host <- Sys.getenv("LITE_HOST", "0.0.0.0")
  app <- lite_new_app(lite_load_hub(), secure_cookie = identical(Sys.getenv("LITE_SECURE_COOKIE", "0"), "1"))
  message(sprintf("Student Lite listening on %s:%d", host, port))
  httpuv::runServer(host, port, app)
}
