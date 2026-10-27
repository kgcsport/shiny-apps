suppressPackageStartupMessages({library(DBI);library(RSQLite)})
`%||%` <- function(a,b) if(is.null(a)||!length(a)||is.na(a[1])) b else a
source("apps/class-job-market/collective_action.R")
con<-dbConnect(SQLite(),":memory:");on.exit(dbDisconnect(con),add=TRUE);ca_init(con)
dbExecute(con,"INSERT INTO collective_action_sessions(scope_key,course,sections,title) VALUES ('econ160::a', 'ECON160', 'A', 'Prototype')"); sid<-dbGetQuery(con,"SELECT last_insert_rowid() id")$id[1]
dbExecute(con,"INSERT INTO collective_action_rounds(session_id,scope_key,sequence,mechanism,endowment,multiplier,status) VALUES (?, 'econ160::a',1,'linear',3,1.5,'draft')",params=list(sid));rid<-dbGetQuery(con,"SELECT last_insert_rowid() id")$id[1]
ca_open_round(con,rid,c("a","b","c","d"));ca_submit_choice(con,rid,"a",2);ca_submit_choice(con,rid,"b",1);ca_submit_choice(con,rid,"b",3);ca_lock_round(con,rid)
out<-dbGetQuery(con,"SELECT * FROM collective_action_outcomes");pay<-dbGetQuery(con,"SELECT * FROM collective_action_payouts");stopifnot(out$eligible_n==4,out$total_contributed==5,nrow(pay)==4,all(abs(pay$pooled_return-1.875)<1e-9),inherits(try(ca_submit_choice(con,rid,"a",1),silent=TRUE),"try-error"))
cat("collective action prototype: OK\n")
