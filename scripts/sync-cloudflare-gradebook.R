#!/usr/bin/env Rscript
# Flat Cloudflare -> Shiny gradebook import. The Worker returns normalized rows;
# this script performs only explicit-key matching and an idempotent upsert.
suppressPackageStartupMessages({library(DBI); library(RSQLite); library(jsonlite); library(httr2)})
`%||%` <- function(a, b) {
  if (is.null(a) || !length(a)) return(b)
  # Only scalar NA is missing; preserve data frames and multi-value vectors.
  if (is.atomic(a) && length(a) == 1L && is.na(a)) return(b)
  a
}
script_args <- commandArgs(trailingOnly=FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
script_dir <- if (nzchar(script_file)) dirname(normalizePath(script_file, mustWork=FALSE)) else getwd()
shared_sqlite <- file.path(script_dir, "..", "apps", "_shared", "sqlite.R")
if (!file.exists(shared_sqlite)) shared_sqlite <- "/srv/shiny-server/_shared/sqlite.R"
source(shared_sqlite, local=TRUE)
con <- connect_sqlite(shared_db_path(demo=FALSE)); on.exit(try(dbDisconnect(con), silent=TRUE), add=TRUE)
db_query <- function(sql, params=NULL) if (is.null(params)) dbGetQuery(con,sql) else dbGetQuery(con,sql,params=params)
db_exec <- function(sql, params=NULL) if (is.null(params)) dbExecute(con,sql) else dbExecute(con,sql,params=params)
origin <- sub("/+$$", "", Sys.getenv("ASSIGNMENT_API_ORIGIN", "https://econ342-self-grading.kyle-g-coombs.workers.dev"))
token <- trimws(Sys.getenv("ASSIGNMENT_ADMIN_TOKEN", "")); if (!nzchar(token)) stop("ASSIGNMENT_ADMIN_TOKEN is missing")
response <- request(paste0(origin,"/api/admin/review/gradebook-export")) |> req_headers(Authorization=paste("Bearer",token)) |> req_timeout(30) |> req_error(is_error=function(x) FALSE) |> req_perform()
body <- tryCatch(resp_body_json(response,simplifyVector=TRUE), error=function(e) list())
if (resp_status(response) >= 300) stop(sprintf("Worker export failed: HTTP %s — %s", resp_status(response), body$error %||% body$message %||% "no response body"))
rows <- body$rows %||% data.frame(); if (!is.data.frame(rows)) rows <- if(length(rows)) do.call(rbind,lapply(rows,as.data.frame,stringsAsFactors=FALSE)) else data.frame(); if(!nrow(rows)){summary<-body$assignmentSummary %||% data.frame(); labels<-if(is.data.frame(summary)&&nrow(summary))paste(sprintf("%s=%s/%s",summary$id,summary$gradebookKey %||% "<missing>",summary$submissionRows %||% 0),collapse=", ") else "none";cat(sprintf("Worker returned 0 grade rows across %d assignment(s). Keys/submissions: %s\n",as.integer(body$assignments %||% 0L),labels));quit(save="no",status=0)}
cats <- db_query("SELECT * FROM gradebook_categories ORDER BY display_order,id;"); inames <- db_query("SELECT * FROM gradebook_item_names ORDER BY category_id,item_index;")
compact <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(as.character(x %||% ""))))
key <- function(x) tolower(trimws(as.character(x %||% "")))[1]
worker_key <- function(row) { raw <- compact(row$gradebookKey[1] %||% ""); title <- as.character(row$assignmentTitle[1] %||% ""); source <- if(nzchar(raw)) raw else title; hit <- regmatches(source, regexpr("(problem\\s*set|ps)\\s*[0-9]+", source, ignore.case=TRUE, perl=TRUE)); number <- regmatches(compact(hit), regexpr("[0-9]+$", compact(hit))); if(length(number)&&nzchar(number)) paste0("ps",number) else compact(source) }
# One gradebook per course: a course's own categories, or the shared ones if it has none.
cat_course <- if ("course" %in% names(cats)) ifelse(is.na(cats$course), "", tolower(trimws(cats$course))) else rep("", nrow(cats))
catalog_for <- function(course) {
  mine <- cats[nzchar(course) & cat_course == course, , drop=FALSE]
  if (!nrow(mine)) mine <- cats[!nzchar(cat_course), , drop=FALSE]
  if (!nrow(mine)) return(data.frame(assignment=character(0), item_key=character(0), stringsAsFactors=FALSE))
  do.call(rbind, lapply(seq_len(nrow(mine)), function(i) {
    c <- mine[i, ]; n <- max(1L, as.integer(c$item_count %||% 1L))
    pref <- if (!is.null(c$item_prefix) && !is.na(c$item_prefix) && nzchar(c$item_prefix)) c$item_prefix else c$name
    ov <- if (nrow(inames)) inames[inames$category_id == c$id, , drop=FALSE] else data.frame()
    do.call(rbind, lapply(seq_len(n), function(j) {
      z <- if (nrow(ov)) ov[ov$item_index == j, , drop=FALSE] else data.frame()
      name <- if (nrow(z) && nzchar(z$item_name[1] %||% "")) z$item_name[1] else if (n == 1) c$name else paste0(pref, " ", j)
      stored <- if (nrow(z) && "item_key" %in% names(z)) as.character(z$item_key[1]) else NA_character_
      data.frame(assignment=name, item_key=if (!is.na(stored) && nzchar(stored)) stored else compact(name), stringsAsFactors=FALSE)
    }))
  }))
}
policy <- db_query("SELECT value FROM labor_settings WHERE key='assignment_grade_policy';")$value[1] %||% "final_score"; if(!policy %in% c("final_score","split_half"))policy<-"final_score"; protected_raw <- db_query("SELECT value FROM labor_settings WHERE key='assignment_grade_protected';")$value[1] %||% "PS1"; protected_items <- compact(strsplit(protected_raw,"[,;\\n]+")[[1]]); excluded_raw <- db_query("SELECT value FROM labor_settings WHERE key='assignment_grade_excluded';")$value[1] %||% ""; excluded_items <- compact(strsplit(excluded_raw,"[,;\\n]+")[[1]])
everyone <- db_query("SELECT user_id,display_name,LOWER(TRIM(COALESCE(course,''))) AS course FROM users WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0;")
offerings <- tryCatch(db_query("SELECT offering_id, course FROM course_offerings;"), error=function(e) data.frame(offering_id=character(0), course=character(0)))
enrolled <- function(offering_id) tryCatch(db_query("SELECT u.user_id,u.display_name FROM enrollments e JOIN users u ON LOWER(u.user_id)=LOWER(e.user_id) WHERE e.offering_id=? AND COALESCE(e.status,'active')='active';", list(offering_id)), error=function(e) data.frame())
db_exec("CREATE TABLE IF NOT EXISTS assignment_grade_sync_override(user_id TEXT NOT NULL,assignment_name TEXT NOT NULL,reason TEXT,created_at TEXT DEFAULT CURRENT_TIMESTAMP,PRIMARY KEY(user_id COLLATE NOCASE,assignment_name COLLATE NOCASE));")
db_exec("CREATE TABLE IF NOT EXISTS assignment_grade_sync_log(assignment_id TEXT PRIMARY KEY,assignment_title TEXT,gradebook_item TEXT,policy TEXT,last_synced_at TEXT,status TEXT DEFAULT 'pending',error TEXT,rows_synced INTEGER DEFAULT 0);")
upsert <- function(uid,item,score,mx,pct,title,course) db_exec("INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct,week_tag,course) VALUES(?,?,?,?,?,?,?) ON CONFLICT DO UPDATE SET score=excluded.score,max_score=excluded.max_score,grade_pct=excluded.grade_pct,week_tag=excluded.week_tag,uploaded_at=CURRENT_TIMESTAMP;",list(uid,item,score,mx,pct,title,course))
total<-0L; skipped<-0L; unmatched_key<-0L; unmatched_student<-0L; by_name<-0L; unlinked<-0L
# A row whose courseId is one of this app's offerings is filed for that
# offering's course: the student by sign-in email among its enrollments, the
# item by key in its gradebook. Any other row is matched as it always was
# (any active student, filed under their home course) and counted.
if (nrow(rows)) for (i in seq_len(nrow(rows))) {
  r <- rows[i, , drop=FALSE]
  offering <- key(if ("courseId" %in% names(r)) r$courseId[1] else "")
  linked <- nzchar(offering) && offering %in% tolower(offerings$offering_id)
  roster <- if (linked) enrolled(offering) else everyone
  ids <- unique(c(key(r$externalId[1] %||% ""), key(if ("studentEmail" %in% names(r)) r$studentEmail[1] else ""))); ids <- ids[nzchar(ids)]
  hit <- if (nrow(roster)) which(tolower(trimws(roster$user_id)) %in% ids) else integer(0)
  named <- FALSE
  if (!length(hit) && nrow(roster)) {
    hit <- which(tolower(trimws(roster$display_name)) == key(r$studentName[1] %||% ""))
    if (length(hit) != 1L) hit <- integer(0) else named <- TRUE   # a name counts only when it picks out one student
  }
  if (!length(hit)) { unmatched_student <- unmatched_student + 1L; next }
  uid <- roster$user_id[hit[1]]
  course <- if (linked) offerings$course[tolower(offerings$offering_id) == offering][1] else everyone$course[tolower(everyone$user_id) == tolower(uid)][1]
  catalog <- catalog_for(course %||% "")
  item_hit <- which(catalog$item_key == worker_key(r))
  if (!length(item_hit)) { unmatched_key <- unmatched_key + 1L; next }
  item <- catalog$assignment[item_hit[1]]
  if (compact(item) %in% excluded_items) { skipped <- skipped + 1L; next }
  protected <- db_query("SELECT 1 FROM assignment_grade_sync_override WHERE LOWER(user_id)=LOWER(?) AND LOWER(assignment_name)=LOWER(?) LIMIT 1;", list(uid, item))
  if (nrow(protected)) { skipped <- skipped + 1L; next }
  submitted <- identical(as.character(r$status[1] %||% ""), "submitted") && nzchar(as.character(r$submittedAt[1] %||% ""))
  scan <- nzchar(as.character(r$scanVerifiedAt[1] %||% "")); pct <- suppressWarnings(as.numeric(r$gradePct[1] %||% NA))
  if (policy == "final_score" && (!submitted || !is.finite(pct))) next
  if (policy == "split_half" && !scan && (!submitted || !is.finite(pct))) next
  grade <- if (policy == "split_half") (if (submitted) 100 else if (scan) 50 else 0) else pct
  existing <- db_query("SELECT 1 FROM student_grades WHERE LOWER(user_id)=LOWER(?) AND LOWER(assignment_name)=LOWER(?) AND COALESCE(course,'') IN (?, '') LIMIT 1;", list(uid, item, course %||% ""))
  if (compact(item) %in% protected_items && nrow(existing)) { skipped <- skipped + 1L; next }
  upsert(uid, item, suppressWarnings(as.numeric(r$score[1] %||% NA)), suppressWarnings(as.numeric(r$maxPoints[1] %||% 100)), grade, as.character(r$assignmentTitle[1] %||% item), course %||% "")
  total <- total + 1L; if (named) by_name <- by_name + 1L; if (!linked) unlinked <- unlinked + 1L
}
cat(sprintf("Imported %d grade row(s) from Worker export (%d unmatched: %d assignment-key, %d student-identity; %d protected overrides; %d matched by name; %d from a Worker course ID that is not an offering here).\n",total,unmatched_key+unmatched_student,unmatched_key,unmatched_student,skipped,by_name,unlinked))
