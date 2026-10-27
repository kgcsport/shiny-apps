suppressPackageStartupMessages({library(DBI);library(RSQLite)})
`%||%` <- function(a,b) if(is.null(a)||!length(a)||is.na(a[1])) b else a
source("apps/class-job-market/collective_action.R")
con<-dbConnect(SQLite(),":memory:");on.exit(dbDisconnect(con),add=TRUE);ca_init(con)
dbExecute(con,"CREATE TABLE users (user_id TEXT PRIMARY KEY, course TEXT, section TEXT, is_admin INTEGER DEFAULT 0, active INTEGER DEFAULT 1, is_demo INTEGER DEFAULT 0)")
dbExecute(con,"INSERT INTO users(user_id,course,section,is_admin) VALUES ('a','ECON160','A',0),('b','ECON160','A',0),('c','ECON160','A',0),('d','ECON160','A',0),('instructor','ECON160','A',1),('other-course','ECON101','A',0)")
dbExecute(con,"INSERT INTO collective_action_sessions(scope_key,course,sections,title) VALUES ('econ160::a', 'ECON160', 'A', 'Prototype')"); sid<-dbGetQuery(con,"SELECT last_insert_rowid() id")$id[1]
dbExecute(con,"INSERT INTO collective_action_rounds(session_id,scope_key,sequence,mechanism,endowment,multiplier,status) VALUES (?, 'econ160::a',1,'linear',3,1.5,'draft')",params=list(sid));rid<-dbGetQuery(con,"SELECT last_insert_rowid() id")$id[1]
stopifnot(identical(ca_round_roster(con,rid),c("a","b","c","d")))
ca_open_round(con,rid);ca_submit_choice(con,rid,"a",2);ca_submit_choice(con,rid,"b",1);ca_submit_choice(con,rid,"b",3);ca_lock_round(con,rid)
out<-dbGetQuery(con,"SELECT * FROM collective_action_outcomes");pay<-dbGetQuery(con,"SELECT * FROM collective_action_payouts");stopifnot(out$eligible_n==4,out$total_contributed==5,nrow(pay)==4,all(abs(pay$pooled_return-1.875)<1e-9),inherits(try(ca_submit_choice(con,rid,"a",1),silent=TRUE),"try-error"))
cat("collective action prototype: OK\n")
