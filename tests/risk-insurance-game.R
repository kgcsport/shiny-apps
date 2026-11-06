#!/usr/bin/env Rscript
root <- if (file.exists("apps/club-insurance-game/app.R")) "." else "/srv/shiny-server"
app_path <- file.path(root,"apps","club-insurance-game","app.R")
math_path <- file.path(root,"apps","club-insurance-game","risk_math.R")
hub_path <- file.path(root,"apps","class-job-market","app.R")
if (!file.exists(app_path)) app_path <- file.path(root,"club-insurance-game","app.R")
if (!file.exists(math_path)) math_path <- file.path(root,"club-insurance-game","risk_math.R")
if (!file.exists(hub_path)) hub_path <- file.path(root,"class-job-market","app.R")
source(math_path)

stopifnot(insurance_loss(81,"percent",.2)==16.2)
stopifnot(insurance_loss(6,"fixed",4)==4)
stopifnot(insurance_premium(4,.5)==2)
stopifnot(insurance_purchase(2,2), !insurance_purchase(1.99,2))
insured_bad <- insurance_outcome(20,4,2,TRUE,TRUE)
uninsured_bad <- insurance_outcome(20,4,2,FALSE,TRUE)
insured_good <- insurance_outcome(20,4,2,TRUE,FALSE)
stopifnot(insured_bad$net_change == -2, uninsured_bad$net_change == -4,
          insured_good$net_change == -2)
for (alpha in c(-.5,0,.1,.5)) {
  recovered <- implied_cara(cara_wtp(alpha,.5,4),.5,4)
  if (!isTRUE(all.equal(recovered,alpha,tolerance=1e-6))) stop("CARA inversion failed.")
}
for (gamma in c(-2,0,1,2,5)) {
  recovered <- implied_crra(crra_wtp(gamma,.5,20,4),.5,20,4)
  if (!isTRUE(all.equal(recovered,gamma,tolerance=1e-5))) stop("CRRA inversion failed.")
}
if (!is.na(implied_crra(2,.5,4,4))) stop("CRRA must be undefined at zero bad-state wealth.")

app <- paste(readLines(app_path,warn=FALSE),collapse="\n")
hub <- paste(readLines(hub_path,warn=FALSE),collapse="\n")
required_app <- c("risk_insurance_rounds","risk_insurance_choices","risk_insurance_outcomes",
  "PRIMARY KEY(course,section,round,user_id)","apply_tokens","insurance_outcome(",
  "endowment_band(","implied_cara","implied_crra","Naïve rank","Results sealed",
  "req(rv$admin,!rv$impersonating)","earning,note) VALUES(?,?,?,?,?,0,?)")
missing <- required_app[!vapply(required_app,grepl,logical(1),x=app,fixed=TRUE)]
if(length(missing))stop("Risk & Insurance app contract missing: ",paste(missing,collapse=" | "))
required_hub <- c('label = "Risk & Insurance", embedded = TRUE, url = "/club-insurance-game/"',
  "if (targetPath === '/club-insurance-game') return '/club-insurance-game/';",
  'if (target %in% c("/club-insurance-game", "/club-insurance-game/")) return("/club-insurance-game/")',
  '`club-insurance-game` = uiOutput("embedded_risk_insurance")',
  'title="Risk & Insurance"')
missing_hub <- required_hub[!vapply(required_hub,grepl,logical(1),x=hub,fixed=TRUE)]
if(length(missing_hub))stop("Risk & Insurance hub contract missing: ",paste(missing_hub,collapse=" | "))

if (!requireNamespace("DBI",quietly=TRUE) || !requireNamespace("RSQLite",quietly=TRUE)) {
  cat("risk insurance game: source and math OK (SQLite unavailable)\n");quit(status=0)
}
con <- DBI::dbConnect(RSQLite::SQLite(),":memory:");on.exit(DBI::dbDisconnect(con),add=TRUE)
DBI::dbExecute(con,"CREATE TABLE outcomes(course TEXT,section TEXT,round INTEGER,user_id TEXT,net REAL,UNIQUE(course,section,round,user_id))")
DBI::dbExecute(con,"CREATE TABLE ledger(user_id TEXT,source_type TEXT,source_id INTEGER,amount REAL,earning INTEGER)")
settle_once <- function(uid,outcome_id,outcome) {
  inserted <- DBI::dbExecute(con,"INSERT OR IGNORE INTO outcomes VALUES('ECON 342','01',1,?,?)",list(uid,outcome$net_change))
  if(inserted==1L) for(row in list(c("premium",-outcome$premium_debit),c("loss",-outcome$loss_debit),c("indemnity",outcome$indemnity_credit))) if(abs(as.numeric(row[2]))>=.005) DBI::dbExecute(con,"INSERT INTO ledger VALUES(?,?,?,?,0)",list(uid,row[1],outcome_id,as.numeric(row[2])))
}
settle_once("student-a",1,insured_bad);settle_once("student-a",1,insured_bad)
if(DBI::dbGetQuery(con,"SELECT COUNT(*) n FROM outcomes")$n[1]!=1L)stop("Outcome settlement is not idempotent.")
if(sum(DBI::dbGetQuery(con,"SELECT amount FROM ledger")$amount)!=insured_bad$net_change)stop("Ledger cashflows do not match insured bad-state payoff.")

# Exercise the real Shiny observers and real persistence boundary.
scratch <- tempfile("risk-insurance-");dir.create(file.path(scratch,"data"),recursive=TRUE)
old_connect <- Sys.getenv("CONNECT_CONTENT_DIR",unset=NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR=scratch)
on.exit({if(is.na(old_connect))Sys.unsetenv("CONNECT_CONTENT_DIR")else Sys.setenv(CONNECT_CONTENT_DIR=old_connect);unlink(scratch,recursive=TRUE)},add=TRUE)
db_path <- file.path(scratch,"data","class-job-market.sqlite")
live <- DBI::dbConnect(RSQLite::SQLite(),db_path)
DBI::dbExecute(live,"CREATE TABLE users(user_id TEXT PRIMARY KEY,display_name TEXT,is_admin INTEGER,active INTEGER,course TEXT,section TEXT,is_demo INTEGER,pw_hash TEXT)")
DBI::dbExecute(live,"CREATE TABLE token_ledger(id INTEGER PRIMARY KEY AUTOINCREMENT,user_id TEXT,display_name TEXT,source_type TEXT,source_id INTEGER,amount REAL,earning INTEGER,note TEXT,created_at TEXT DEFAULT CURRENT_TIMESTAMP)")
DBI::dbExecute(live,"CREATE TABLE scope_arcade_state(scope_key TEXT PRIMARY KEY,active_game TEXT,updated_at TEXT DEFAULT CURRENT_TIMESTAMP)")
DBI::dbExecute(live,"INSERT INTO users VALUES('admin','Instructor',1,1,'ECON 342','01',0,''),('student-a','Student A',0,1,'ECON 342','01',0,'')")
DBI::dbExecute(live,"INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning) VALUES('student-a','Student A','seed',20,1)")
DBI::dbExecute(live,"INSERT INTO scope_arcade_state(scope_key,active_game) VALUES('econ 342::01','club-insurance-game')")
DBI::dbDisconnect(live)

app_env <- new.env(parent=globalenv());source(app_path,local=app_env)
shiny::testServer(app_env$server,{
  rv$user<-"admin";rv$name<-"Instructor";rv$admin<-TRUE;rv$course<-"ECON 342";rv$section<-"01";rv$player_user<-"admin";rv$player_name<-"Instructor"
  session$setInputs(round_no=1,loss_type="percent",loss_percent=20,probability=50,premium_rate=50,settle_seed=1,apply_tokens=TRUE,open_round=1)
  session$flushReact()
})
live <- DBI::dbConnect(RSQLite::SQLite(),db_path)
round_row <- DBI::dbGetQuery(live,"SELECT * FROM risk_insurance_rounds")
if(nrow(round_row)!=1L||round_row$status[1]!="open"||round_row$loss_value[1]!=.2)stop("Instructor open workflow failed.")
DBI::dbDisconnect(live)

shiny::testServer(app_env$server,{
  rv$user<-"student-a";rv$name<-"Student A";rv$admin<-FALSE;rv$course<-"ECON 342";rv$section<-"01";rv$player_user<-"student-a";rv$player_name<-"Student A"
  session$setInputs(max_premium=2.5,save_choice=1);session$flushReact()
})
live <- DBI::dbConnect(RSQLite::SQLite(),db_path)
choice <- DBI::dbGetQuery(live,"SELECT * FROM risk_insurance_choices")
if(nrow(choice)!=1L||choice$starting_balance[1]!=20||choice$loss_amount[1]!=4||choice$posted_premium[1]!=2||choice$purchased[1]!=1L)stop("Student purchase workflow failed.")
DBI::dbDisconnect(live)

shiny::testServer(app_env$server,{
  rv$user<-"admin";rv$name<-"Instructor";rv$admin<-TRUE;rv$course<-"ECON 342";rv$section<-"01";rv$player_user<-"admin";rv$player_name<-"Instructor"
  session$setInputs(close_round=1);session$flushReact();session$setInputs(settle_round=1);session$flushReact();session$setInputs(reveal_round=1);session$flushReact()
})
live <- DBI::dbConnect(RSQLite::SQLite(),db_path)
final_round <- DBI::dbGetQuery(live,"SELECT status FROM risk_insurance_rounds")$status[1]
real_outcome <- DBI::dbGetQuery(live,"SELECT * FROM risk_insurance_outcomes")
cashflows <- DBI::dbGetQuery(live,"SELECT amount,earning FROM token_ledger WHERE source_type LIKE 'risk_insurance_%'")
if(final_round!="revealed"||nrow(real_outcome)!=1L||real_outcome$net_change[1]!=-2)stop("Close/settle/reveal workflow failed.")
if(sum(cashflows$amount)!=-2||any(cashflows$earning!=0L))stop("Real token settlement is incorrect or counted as earnings.")
DBI::dbDisconnect(live)
cat("risk insurance game: PASS\n")
