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
  # Settings are read for the student's own section, then course, then
  # app-wide, exactly as the hub reads them for that student.
  setting_context <- function(user) list(course = hub$norm_key(user$course), sections = hub$norm_key(user$section),
                                         scope = scope_key(user), level = "auto")
  scoped_setting <- function(key, scope, default) hub$resolve_setting(key, default, with_scope = TRUE)$value
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
    # The student's own jobs for this class date. A job appears only once the
    # instructor has shown its group (start or end of class) to this section,
    # by the hub's own rule; until then only a count is sent.
    assigned <- if (!is.na(rid)) q(
      "SELECT jp.job_name, COALESCE(jp.description, '') AS description,
              ABS(COALESCE(ja.assigned_wage, jp.wage_override, jc.default_wage, 0)) AS wage,
              COALESCE(ja.outcome,'') AS outcome,
              COALESCE(NULLIF(NULLIF(ja.selection_time,''),'any'),
                       NULLIF(jp.selection_time,''), NULLIF(jc.selection_time,''), 'start') AS timing
       FROM job_assignments ja JOIN job_posts jp ON jp.id=ja.job_post_id
       LEFT JOIN job_categories jc ON jc.id=jp.category_id
       WHERE ja.round_id=? AND LOWER(ja.user_id)=LOWER(?) AND COALESCE(ja.status,'assigned')='assigned'
         AND COALESCE(ja.display_on_today,1)=1
       ORDER BY COALESCE(jp.display_order,99), jp.id;", list(rid, user$user_id)) else data.frame()
    reveals <- if (nrow(assigned)) q(
      "SELECT round_id, section, COALESCE(timing,'start') AS timing, COALESCE(revealed,0) AS revealed
       FROM assignment_timing_reveals WHERE round_id=?;", list(rid)) else data.frame()
    visible <- vapply(seq_len(nrow(assigned)), function(i)
      isTRUE(hub$assignment_is_revealed(user$section, assigned$timing[i], reveals, round_id = rid)), logical(1))
    practice <- nrow(round) > 0 && isTRUE(as.integer(round$practice_no_points[1] %||% 0L) == 1L)
    my_jobs <- lapply(which(visible), function(i) list(
      job_name = assigned$job_name[i], description = assigned$description[i],
      when = if (identical(hub$assignment_reveal_timing(assigned$timing[i]), "end")) "End of class" else "Start of class",
      # Practice dates pay nothing, so no wage is shown.
      wage = if (practice) NULL else as.numeric(assigned$wage[i]),
      outcome = if (nzchar(assigned$outcome[i])) assigned$outcome[i] else NULL))
    list(student = list(name = user$display_name, course = user$course, section = user$section),
         my_jobs = my_jobs, jobs_not_shown = sum(!visible),
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

  # ── Instructor tracker ──────────────────────────────────────────────────────
  # A fast page for the in-class loop: cold calls, voluntary contributions, and
  # the job draw, showing jobs to students, scoring, and committing scores. Every
  # one of these runs the hub's own rule, so the two trackers cannot disagree.
  # Previews, manual assignments, offers, token release, and repairs stay in the
  # main app.
  session_instructor <- function(req) {
    row <- hub$lookup_token(session_token(req))
    if (!nrow(row) || as.integer(row$active[1] %||% 1L) == 0L || as.integer(row$is_admin[1] %||% 0L) != 1L ||
        as.integer(row$is_demo[1] %||% 0L) == 1L) return(NULL)
    roles <- hub$admin_assignments_for(row$user_id[1])
    if (!nrow(roles)) return(NULL)
    list(user_id = row$user_id[1], name = row$display_name[1], roles = roles,
         course = row$course[1] %||% "", section = row$section[1] %||% "")
  }
  may_run <- function(instructor, course, section) {
    roles <- instructor$roles
    course_key <- hub$norm_key(course); section_key <- hub$norm_key(section)
    any(roles$role == "super_admin") ||
      any(roles$role == "course_admin" & hub$norm_key(roles$course) == course_key) ||
      any(roles$role == "section_admin" & hub$norm_key(roles$course) == course_key & hub$norm_key(roles$section) == section_key)
  }
  class_list <- function(instructor) {
    rows <- q("SELECT DISTINCT course, section FROM users
               WHERE COALESCE(active,1)=1 AND COALESCE(is_admin,0)=0 AND COALESCE(is_demo,0)=0
                 AND TRIM(COALESCE(course,''))<>'' AND TRIM(COALESCE(section,''))<>''
               ORDER BY course, section;")
    if (!nrow(rows)) return(list())
    keep <- vapply(seq_len(nrow(rows)), function(i) may_run(instructor, rows$course[i], rows$section[i]), logical(1))
    rows <- rows[keep, , drop = FALSE]
    lapply(seq_len(nrow(rows)), function(i) list(course = rows$course[i], section = rows$section[i]))
  }
  class_students <- function(course, section) {
    rows <- q("SELECT user_id, display_name, course, section FROM users
               WHERE COALESCE(active,1)=1 AND COALESCE(is_admin,0)=0 AND COALESCE(is_demo,0)=0
               ORDER BY display_name;")
    rows[hub$norm_key(rows$course) == hub$norm_key(course) & hub$norm_key(rows$section) == hub$norm_key(section), , drop = FALSE]
  }
  class_context <- function(course, section) {
    user <- list(course = course, section = section)
    list(course = hub$norm_key(course), sections = hub$norm_key(section), scope = scope_key(user), level = "auto")
  }
  tracker_state <- function(course, section) {
    context <- class_context(course, section)
    round <- round_row(context$scope)
    students <- class_students(course, section)
    if (!nrow(round)) return(list(course = course, section = section, round = NULL, students = list(),
                                  volunteer_jobs = list(), assignments = list(), open_jobs = list(), pending = 0L))
    rid <- as.integer(round$id[1])
    absent <- hub$norm_key(q("SELECT user_id FROM round_absences WHERE round_id=?;", list(rid))$user_id)
    events <- q("SELECT user_id, event_kind, outcome, COALESCE(tokens,0) tokens FROM live_score_events WHERE round_id=?;", list(rid))
    count <- function(uid, kind) sum(hub$norm_key(events$user_id) == hub$norm_key(uid) & events$event_kind == kind)
    volunteer_posts <- q(
      "SELECT jp.id, jp.job_name FROM job_posts jp LEFT JOIN job_categories jc ON jc.id=jp.category_id
       WHERE jp.round_id=? AND COALESCE(jp.active,1)=1 AND (COALESCE(jc.voluntary,0)=1 OR COALESCE(jp.voluntary,0)=1)
       ORDER BY jp.display_order, jp.job_name;", list(rid))
    assignments <- q(
      "SELECT ja.id, ja.user_id, u.display_name, jp.job_name, ABS(ja.assigned_wage) AS wage,
              COALESCE(ja.status,'assigned') AS status, COALESCE(ja.outcome,'') AS outcome,
              COALESCE(NULLIF(ja.selection_time,''), NULLIF(jp.selection_time,''), 'any') AS timing,
              (SELECT lse.outcome FROM live_score_events lse
                WHERE lse.job_assignment_id=ja.id AND lse.committed_at IS NULL ORDER BY lse.id DESC LIMIT 1) AS queued
       FROM job_assignments ja JOIN users u ON u.user_id=ja.user_id JOIN job_posts jp ON jp.id=ja.job_post_id
       WHERE ja.round_id=? AND COALESCE(ja.status,'assigned')='assigned'
       ORDER BY jp.display_order, jp.job_name, u.display_name;", list(rid))
    assignments <- assignments[hub$norm_key(assignments$user_id) %in% hub$norm_key(students$user_id), , drop = FALSE]
    waiting <- q("SELECT lse.id, lse.user_id, u.display_name, lse.event_kind, lse.outcome
                  FROM live_score_events lse JOIN users u ON u.user_id=lse.user_id
                  WHERE lse.committed_at IS NULL ORDER BY lse.id;")
    waiting <- waiting[hub$norm_key(waiting$user_id) %in% hub$norm_key(students$user_id), , drop = FALSE]
    pending <- nrow(waiting)
    # Jobs in the draw that still have an empty slot, so one can be drawn alone.
    open_jobs <- q(
      "SELECT jp.id, jp.job_name, COALESCE(jp.slots,1) - (SELECT COUNT(*) FROM job_assignments ja
                WHERE ja.round_id=jp.round_id AND ja.job_post_id=jp.id AND COALESCE(ja.status,'assigned')='assigned') AS open
       FROM job_posts jp WHERE jp.round_id=? AND COALESCE(jp.active,1)=1 AND COALESCE(jp.in_draw,1)=1
       ORDER BY COALESCE(jp.display_order,99), jp.id;", list(rid))
    open_jobs <- open_jobs[open_jobs$open > 0, , drop = FALSE]
    shown <- q("SELECT COALESCE(timing,'start') timing FROM assignment_timing_reveals
                WHERE round_id=? AND COALESCE(revealed,0)=1 AND LOWER(TRIM(section))=?;",
               list(rid, hub$norm_key(section)))$timing
    list(
      course = course, section = section,
      round = list(label = round$label[1] %||% "", class_date = round$class_date[1] %||% "",
                   practice = isTRUE(as.integer(round$practice_no_points[1] %||% 0L) == 1L)),
      wages = list(answer = hub$cold_call_wage_for(rid, "answer"), board = hub$cold_call_wage_for(rid, "board")),
      cold_call_by_bid = hub$cold_call_bidders_only(),
      run_sheet = hub$class_run_sheet(context$scope, rid, students),
      deviation_kinds = lapply(names(hub$DEVIATION_KINDS), function(k) list(kind = k, label = hub$DEVIATION_KINDS[[k]])),
      paid_volunteer_limit = identical(as.character(hub$resolve_setting("volunteer_paid_cap", "0", with_scope = TRUE)$value), "1"),
      students = lapply(seq_len(nrow(students)), function(i) list(
        user_id = students$user_id[i], name = students$display_name[i],
        absent = hub$norm_key(students$user_id[i]) %in% absent,
        cold_calls = count(students$user_id[i], "cold_call"), volunteered = count(students$user_id[i], "participation"))),
      volunteer_jobs = lapply(seq_len(nrow(volunteer_posts)), function(i) list(
        job_post_id = as.integer(volunteer_posts$id[i]), job_name = volunteer_posts$job_name[i])),
      assignments = lapply(seq_len(nrow(assignments)), function(i) list(
        assignment_id = as.integer(assignments$id[i]), name = assignments$display_name[i], job_name = assignments$job_name[i],
        wage = as.numeric(assignments$wage[i]), timing = assignments$timing[i],
        outcome = if (nzchar(assignments$outcome[i])) assignments$outcome[i] else NULL,
        queued = if (is.na(assignments$queued[i])) NULL else assignments$queued[i])),
      open_jobs = lapply(seq_len(nrow(open_jobs)), function(i) list(
        job_post_id = as.integer(open_jobs$id[i]), job_name = open_jobs$job_name[i], open = as.integer(open_jobs$open[i]))),
      shown = list(start = "start" %in% shown, end = "end" %in% shown),
      last_pending = if (pending) sprintf("%s (%s)", waiting$display_name[pending], waiting$outcome[pending]) else NULL,
      pending = as.integer(pending))
  }
  tracker_action <- function(instructor, course, section, action, body) {
    context <- class_context(course, section)
    round <- round_row(context$scope)
    if (!nrow(round)) return(list(ok = FALSE, message = "No class date is selected for this section."))
    rid <- as.integer(round$id[1])
    students <- class_students(course, section)
    in_class <- function(uid) nzchar(uid) && hub$norm_key(uid) %in% hub$norm_key(students$user_id)
    name_of <- function(uid) students$display_name[hub$norm_key(students$user_id) == hub$norm_key(uid)][1]
    absent <- hub$norm_key(q("SELECT user_id FROM round_absences WHERE round_id=?;", list(rid))$user_id)
    uid <- as.character(body$user_id %||% "")

    if (identical(action, "draw")) {
      # The same first steps as the main tracker's draw: freeze the date's
      # wages and close bidding, then pick among the students who are present.
      hub$freeze_class_wages(rid)
      hub$close_bidding_after_draw(rid)
      skip <- hub$norm_key(as.character(unlist(body$exclude %||% character(0))))
      pool <- students[!hub$norm_key(students$user_id) %in% c(absent, skip), , drop = FALSE]
      by_bid <- hub$cold_call_bidders_only()
      kind <- if (by_bid) as.character(body$kind %||% "answer") else NULL
      if (by_bid && !kind %in% c("answer", "board")) return(list(ok = FALSE, message = "Unknown cold-call kind."))
      pool <- hub$cold_call_eligible(rid, pool, kind)
      if (!nrow(pool)) return(list(ok = FALSE, message = if (by_bid)
        sprintf("No present student bid at or below the wage for %s. Take a volunteer instead.",
                if (identical(kind, "board")) "board work" else "an answer")
        else "No students are available to call."))
      pick <- pool[sample.int(nrow(pool), 1L), ]
      return(list(ok = TRUE, drawn = list(user_id = pick$user_id, name = pick$display_name, kind = kind)))
    }
    keep_class <- function(rows) {
      if (!nrow(rows)) return(rows)
      rows[hub$norm_key(rows$course) == hub$norm_key(course) & hub$norm_key(rows$section) == hub$norm_key(section), , drop = FALSE]
    }
    if (identical(action, "deviation")) {
      result <- hub$log_deviation(context$scope, rid, as.character(body$kind %||% ""), body$note %||% "", instructor$user_id)
      return(list(ok = isTRUE(result$ok), message = result$message))
    }
    if (identical(action, "draw_jobs")) {
      timing <- as.character(body$timing %||% "all")
      if (!timing %in% c("all", "start", "during", "end")) return(list(ok = FALSE, message = "Unknown draw."))
      by_date <- identical(as.character(hub$resolve_setting("draw_uses_date_settings", "0", with_scope = TRUE)$value), "1")
      result <- hub$run_job_draw(round, timing, keep_class, by_date)
      return(list(ok = isTRUE(result$ok) && isTRUE(result$changed), message = result$message))
    }
    if (identical(action, "reveal")) {
      which <- as.character(body$timing %||% "all")
      if (!which %in% c("all", "start", "end")) return(list(ok = FALSE, message = "Unknown choice."))
      now <- hub$toggle_section_reveal(rid, section, which)
      return(list(ok = TRUE, message = if (now == 1L) "Students can now see their jobs." else "Jobs are hidden from students."))
    }
    if (identical(action, "commit")) {
      applied <- hub$commit_pending_scores(keep_class)
      return(list(ok = TRUE, message = if (is.na(applied)) "Nothing was waiting."
                  else sprintf("Committed %d score%s.", applied, if (applied == 1L) "" else "s")))
    }
    if (identical(action, "undo")) {
      waiting <- keep_class(q("SELECT lse.id, u.display_name, u.course, u.section, lse.outcome
                               FROM live_score_events lse JOIN users u ON u.user_id=lse.user_id
                               WHERE lse.committed_at IS NULL ORDER BY lse.id;"))
      if (!nrow(waiting)) return(list(ok = FALSE, message = "Nothing waiting to undo."))
      last <- waiting[nrow(waiting), ]
      hub$db_exec("DELETE FROM live_score_events WHERE id=? AND committed_at IS NULL;", list(as.integer(last$id)))
      return(list(ok = TRUE, message = sprintf("Removed %s (%s).", last$display_name, last$outcome)))
    }
    if (identical(action, "draw_job")) {
      by_date <- identical(as.character(hub$resolve_setting("draw_uses_date_settings", "0", with_scope = TRUE)$value), "1")
      result <- hub$run_job_draw(round, "all", keep_class, by_date,
                                 post_id = suppressWarnings(as.integer(body$job_post_id %||% NA)))
      return(list(ok = isTRUE(result$ok) && isTRUE(result$changed), message = result$message))
    }
    if (action %in% c("job_pass", "job_absent", "job_redraw")) {
      assign_id <- suppressWarnings(as.integer(body$assignment_id %||% NA))
      owner <- q("SELECT user_id, round_id FROM job_assignments WHERE id=?;", list(assign_id))
      if (!nrow(owner) || owner$round_id[1] != rid || !in_class(owner$user_id[1]))
        return(list(ok = FALSE, message = "That assignment is not on today's list for this section."))
      result <- hub$redraw_job(assign_id, scope_filter = keep_class,
                               reason = switch(action, job_pass = "passed", job_redraw = "redrawn", "absent"))
      return(list(ok = isTRUE(result$ok) && isTRUE(result$changed), message = result$message))
    }
    if (!in_class(uid) && !identical(action, "assignment"))
      return(list(ok = FALSE, message = "That student is not in this section."))

    if (identical(action, "cold_call")) {
      kind <- as.character(body$kind %||% "")
      if (kind %in% c("answer", "board")) {
        # With bidders-only cold calls, only a student whose bid cleared that
        # kind's wage can be paid for it, whatever the page sent.
        if (hub$cold_call_bidders_only() &&
            !nrow(hub$cold_call_eligible(rid, students[hub$norm_key(students$user_id) == hub$norm_key(uid), , drop = FALSE], kind)))
          return(list(ok = FALSE, message = sprintf("%s did not bid at or below the wage for %s.", name_of(uid),
                                                    if (kind == "board") "board work" else "an answer")))
        tokens <- hub$queue_cold_call(rid, uid, kind, instructor$user_id)
        return(list(ok = TRUE, message = sprintf("%s: %s recorded.", name_of(uid), if (kind == "answer") "answer" else "board work")))
      }
      if (identical(kind, "pass")) {
        insured <- q("SELECT COUNT(*) n FROM insurance_purchases ip JOIN insurance_products p ON p.id=ip.product_id
                      WHERE p.round_id=? AND p.status IN ('open','closed') AND LOWER(ip.user_id)=LOWER(?)
                        AND NOT EXISTS (SELECT 1 FROM insurance_claims ic WHERE ic.product_id=ip.product_id
                                        AND LOWER(ic.user_id)=LOWER(ip.user_id));", list(rid, uid))$n[1]
        if (as.integer(insured %||% 0L) > 0L)
          return(list(ok = FALSE, message = sprintf("%s holds Pass Insurance for today. Record this pass in the main tracker so the claim is paid.", name_of(uid))))
        hub$db_exec("INSERT INTO live_score_events(round_id,user_id,event_kind,outcome,tokens,logged_by,committed_at,tokens_credited)
                     VALUES(?,?,'cold_call','pass',0,?,datetime('now'),1);", list(rid, uid, instructor$user_id))
        return(list(ok = TRUE, message = sprintf("%s passed.", name_of(uid))))
      }
      return(list(ok = FALSE, message = "Unknown cold-call result."))
    }
    if (identical(action, "volunteer")) {
      outcome <- as.character(body$outcome %||% "")
      if (!outcome %in% c("succeed", "try", "miss")) return(list(ok = FALSE, message = "Unknown result."))
      capped <- identical(as.character(hub$resolve_setting("volunteer_paid_cap", "0", with_scope = TRUE)$value), "1")
      result <- hub$queue_volunteer_event(round, suppressWarnings(as.integer(body$job_post_id %||% NA)), uid, outcome,
                                          instructor$user_id, pay_capped = capped)
      if (!isTRUE(result$ok)) return(list(ok = FALSE, message = result$message))
      return(list(ok = TRUE, message = if (result$tokens < result$uncapped)
        sprintf("%s recorded, unpaid: already paid for a contribution today.", name_of(uid))
        else sprintf("%s recorded.", name_of(uid))))
    }
    if (identical(action, "absent")) {
      hub$db_exec("INSERT OR REPLACE INTO round_absences(round_id,user_id,marked_at) VALUES(?,?,CURRENT_TIMESTAMP);", list(rid, uid))
      return(list(ok = TRUE, message = sprintf("%s marked absent for today.", name_of(uid))))
    }
    if (identical(action, "assignment")) {
      assign_id <- suppressWarnings(as.integer(body$assignment_id %||% NA))
      outcome <- as.character(body$outcome %||% "")
      owner <- q("SELECT user_id, round_id FROM job_assignments WHERE id=?;", list(assign_id))
      if (!nrow(owner) || owner$round_id[1] != rid || !in_class(owner$user_id[1]) || !outcome %in% c("complete", "tried", "missed"))
        return(list(ok = FALSE, message = "That assignment is not on today's list for this section."))
      result <- hub$queue_assignment_score(assign_id, outcome, instructor$user_id)
      return(list(ok = isTRUE(result$ok), message = result$message))
    }
    list(ok = FALSE, message = "Unknown action.")
  }

  index_path <- file.path(lite_dir, "public", "index.html")
  tracker_path <- file.path(lite_dir, "public", "tracker.html")
  page <- function(path) list(
    status = 200L,
    headers = list("Content-Type" = "text/html; charset=utf-8", "Cache-Control" = "no-cache",
                   "X-Content-Type-Options" = "nosniff", "Referrer-Policy" = "same-origin",
                   "Content-Security-Policy" = "default-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline'; frame-ancestors 'none'"),
    body = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  query_value <- function(req, name) {
    pairs <- strsplit(sub("^\\?", "", req$QUERY_STRING %||% ""), "&", fixed = TRUE)[[1]]
    hit <- pairs[startsWith(pairs, paste0(name, "="))]
    if (!length(hit)) "" else utils::URLdecode(gsub("+", " ", sub("^[^=]*=", "", hit[1]), fixed = TRUE))
  }
  handle <- function(req) {
    path <- req$PATH_INFO %||% "/"
    method <- toupper(req$REQUEST_METHOD %||% "GET")
    if (method == "GET" && path %in% c("/", "/index.html")) return(page(index_path))
    if (method == "GET" && path %in% c("/tracker", "/tracker/")) return(page(tracker_path))
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
      if (as.integer(row$is_demo[1]) == 1L)
        return(lite_json(list(error = "Demo accounts use the main app."), 403L))
      # An instructor may sign in, for the tracker page. The student page and
      # its endpoints still accept students only.
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

    if (startsWith(path, "/api/tracker/")) {
      instructor <- session_instructor(req)
      if (is.null(instructor)) return(lite_json(list(error = "Sign in as an instructor first."), 401L))
      if (method == "GET" && path == "/api/tracker/classes")
        # "home" is the instructor's own course and section, offered first.
        return(lite_json(list(instructor = instructor$name, classes = class_list(instructor),
                              home = list(course = instructor$course, section = instructor$section))))
      body <- if (method == "POST") read_body(req) else NULL
      course <- as.character((if (method == "POST") body$course else query_value(req, "course")) %||% "")
      section <- as.character((if (method == "POST") body$section else query_value(req, "section")) %||% "")
      if (!nzchar(course) || !nzchar(section) || !may_run(instructor, course, section))
        return(lite_json(list(error = "You do not run that section."), 403L))
      context <- class_context(course, section)
      if (method == "GET" && path == "/api/tracker/state") {
        state_body <- hub$with_setting_context(context, tracker_state(course, section))
        etag <- paste0('"', digest::digest(state_body, algo = "md5"), '"')
        if (identical(req$HTTP_IF_NONE_MATCH %||% "", etag))
          return(list(status = 304L, headers = list("ETag" = etag, "Cache-Control" = "no-store"), body = ""))
        return(lite_json(state_body, headers = list("ETag" = etag)))
      }
      if (method == "POST" && path == "/api/tracker/act") {
        if (is.null(body)) return(lite_json(list(error = "Bad request"), 400L))
        result <- hub$with_setting_context(context, {
          done <- tracker_action(instructor, course, section, as.character(body$action %||% ""), body)
          c(done, list(state = tracker_state(course, section)))
        })
        return(lite_json(result, if (isTRUE(result$ok)) 200L else 409L))
      }
      return(lite_json(list(error = "Not found"), 404L))
    }

    user <- session_user(req)
    if (is.null(user)) return(lite_json(list(error = "Sign in first."), 401L))
    if (method == "GET" && path == "/api/state") {
      body <- hub$with_setting_context(setting_context(user), state(user))
      etag <- paste0('"', digest::digest(body, algo = "md5"), '"')
      if (identical(req$HTTP_IF_NONE_MATCH %||% "", etag))
        return(list(status = 304L, headers = list("ETag" = etag, "Cache-Control" = "no-store"), body = ""))
      return(lite_json(body, headers = list("ETag" = etag)))
    }
    if (method == "POST" && path == "/api/bids") {
      body <- read_body(req)
      if (is.null(body) || !is.list(body$bids)) return(lite_json(list(error = "Bad request"), 400L))
      result <- hub$with_setting_context(setting_context(user), {
        saved <- save_bids(user, body$bids)
        c(saved, list(state = state(user)))
      })
      return(lite_json(result, if (isTRUE(result$ok)) 200L else 409L))
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
