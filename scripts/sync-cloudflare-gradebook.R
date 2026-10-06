#!/usr/bin/env Rscript
# Background Cloudflare -> Shiny gradebook sync.
# Run inside the Shiny container so it shares the mounted DB and env secrets.
suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(jsonlite); library(httr2)
})
`%||%` <- function(a,b) if (!is.null(a) && length(a)>0 && !is.na(a[1])) a else b
script_args <- commandArgs(trailingOnly=FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
script_dir <- if (nzchar(script_file)) dirname(normalizePath(script_file, mustWork=FALSE)) else getwd()
shared_sqlite <- file.path(script_dir, "..", "apps", "_shared", "sqlite.R")
if (!file.exists(shared_sqlite)) shared_sqlite <- "/srv/shiny-server/_shared/sqlite.R"
source(shared_sqlite, local=TRUE)
path <- shared_db_path(demo=FALSE)
con <- connect_sqlite(path)
on.exit <- function(...) try(DBI::dbDisconnect(con), silent=TRUE)
db_query <- function(sql, params=NULL) if (is.null(params)) dbGetQuery(con,sql) else dbGetQuery(con,sql,params=params)
db_exec <- function(sql, params=NULL) if (is.null(params)) dbExecute(con,sql) else dbExecute(con,sql,params=params)
origin <- sub("/+$", "", Sys.getenv("ASSIGNMENT_API_ORIGIN", "https://econ342-self-grading.kyle-g-coombs.workers.dev"))
token <- Sys.getenv("ASSIGNMENT_ADMIN_TOKEN", "")
if (!nzchar(token)) stop("ASSIGNMENT_ADMIN_TOKEN is missing")
get_json <- function(path) {
  r <- request(paste0(origin,path)) |> req_headers(Authorization=paste("Bearer",token)) |> req_timeout(30) |> req_error(is_error=function(x) FALSE) |> req_perform()
  body <- tryCatch(resp_body_json(r,simplifyVector=TRUE), error=function(e) list())
  if (resp_status(r) >= 300) stop(body$error %||% paste("Worker returned",resp_status(r)))
  body
}
rows_df <- function(x) {
  if (is.data.frame(x)) return(x)
  if (!length(x)) return(data.frame())
  do.call(rbind,lapply(x,as.data.frame,stringsAsFactors=FALSE))
}
compact <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(as.character(x %||% ""))))
cats <- db_query("SELECT * FROM gradebook_categories ORDER BY display_order,id;")
inames <- db_query("SELECT * FROM gradebook_item_names ORDER BY category_id,item_index;")
catalog <- do.call(rbind,lapply(seq_len(nrow(cats)), function(i) {
  c <- cats[i,]; n <- max(1L,as.integer(c$item_count %||% 1L)); pref <- if (!is.null(c$item_prefix)&&!is.na(c$item_prefix)&&nzchar(c$item_prefix)) c$item_prefix else c$name
  ov <- if(nrow(inames)) inames[inames$category_id==c$id,,drop=FALSE] else data.frame()
  data.frame(assignment=vapply(seq_len(n),function(j){z<-if(nrow(ov))ov[ov$item_index==j,,drop=FALSE] else data.frame();if(nrow(z)&&nzchar(z$item_name[1] %||% ""))z$item_name[1] else if(n==1)c$name else paste0(pref," ",j)},character(1)),stringsAsFactors=FALSE)
}))
match_item <- function(a) {
  if (!nrow(catalog)) return(NA_character_)
  ac <- compact(a$id %||% ""); tc <- compact(a$title %||% ""); nc <- compact(catalog$assignment)
  hit <- which(nc %in% c(ac,tc) & nzchar(nc)); if(length(hit)==1) return(catalog$assignment[hit])
  num <- regmatches(ac,regexpr("[0-9]+$",ac)); hit <- if(length(num)&&nzchar(num)) which(grepl(paste0("(problemset|ps)",num),nc)) else integer()
  if(length(hit)==1) catalog$assignment[hit] else NA_character_
}
db_exec("CREATE TABLE IF NOT EXISTS assignment_grade_sync_log(assignment_id TEXT PRIMARY KEY,assignment_title TEXT,gradebook_item TEXT,policy TEXT,last_synced_at TEXT,status TEXT DEFAULT 'pending',error TEXT,rows_synced INTEGER DEFAULT 0);")
policy <- db_query("SELECT value FROM labor_settings WHERE key='assignment_grade_policy';")$value[1] %||% "final_score"
if (!policy %in% c("final_score","split_half")) policy <- "final_score"
roster <- db_query("SELECT user_id,display_name FROM users WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0;")
key <- function(x) tolower(trimws(as.character(x %||% "")))
upsert <- function(uid,item,pct,title) db_exec("INSERT INTO student_grades(user_id,assignment_name,score,max_score,grade_pct,week_tag) VALUES(?,?,?,100,?,?) ON CONFLICT DO UPDATE SET score=excluded.score,max_score=100,grade_pct=excluded.grade_pct,week_tag=excluded.week_tag,uploaded_at=CURRENT_TIMESTAMP;",list(uid,item,pct,pct,title))
assignments <- rows_df(get_json("/api/admin/review/assignments")); total <- 0L
if (nrow(assignments)) for(i in seq_len(nrow(assignments))) {
  a<-assignments[i,,drop=FALSE]; aid<-as.character(a$id[1] %||% ""); title<-as.character(a$title[1] %||% aid); item<-match_item(a)
  if(is.na(item)||!nzchar(item)){db_exec("INSERT OR REPLACE INTO assignment_grade_sync_log VALUES(?,?,?,?,CURRENT_TIMESTAMP,'unmatched',?,0);",list(aid,title,NA,policy,"No matching existing gradebook item"));next}
  d<-get_json(paste0("/api/admin/review/assignments/",URLencode(aid,reserved=TRUE))); subs<-rows_df(d$submissions %||% data.frame()); n<-0L
  if(nrow(subs)) for(j in seq_len(nrow(subs))){s<-subs[j,,drop=FALSE]; hit<-which(key(roster$user_id)==key(s$externalId[1] %||% ""));if(!length(hit))hit<-which(key(roster$display_name)==key(s$name[1] %||% ""));if(!length(hit))next
    scan<-nzchar(as.character(s$scanVerifiedAt[1] %||% "")); submitted<-identical(as.character(s$status[1] %||% ""),"submitted")&&nzchar(as.character(s$submittedAt[1] %||% "")); score<-suppressWarnings(as.numeric(s$totalScore[1] %||% NA)); mx<-suppressWarnings(as.numeric(s$maxPoints[1] %||% NA)); pct<-if(is.finite(score)&&is.finite(mx)&&mx>0)100*score/mx else NA_real_
    if(policy=="final_score"&&(!submitted||!is.finite(pct)))next;if(policy=="split_half"&&!scan&&(!submitted||!is.finite(pct)))next; grade<-if(policy=="split_half")(if(scan)50 else 0)+(if(submitted&&is.finite(pct))0.5*pct else 0) else pct;upsert(roster$user_id[hit[1]],item,grade,title);n<-n+1L }
  db_exec("INSERT OR REPLACE INTO assignment_grade_sync_log VALUES(?,?,?,?,CURRENT_TIMESTAMP,'synced',?,?);",list(aid,title,item,policy,"",n));total<-total+n
}
cat(sprintf("Synced %d grade row(s) across %d assignment(s) using %s policy.\n",total,nrow(assignments),policy))
try(dbDisconnect(con),silent=TRUE)
