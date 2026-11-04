#!/usr/bin/env Rscript
# Workflow regression: two independently logged-in students can submit to the
# same active Price War round without either submission changing the round state.
suppressPackageStartupMessages({ library(DBI); library(RSQLite); library(shiny) })
root <- if (file.exists("apps/price-war/app.R")) getwd() else "/srv/shiny-server"
app <- file.path(root, "apps", "price-war", "app.R")
if (!file.exists(app)) app <- file.path(root, "price-war", "app.R")
scratch <- tempfile("price-war-student-")
dir.create(file.path(scratch, "data"), recursive=TRUE)
old_root <- Sys.getenv("CONNECT_CONTENT_DIR", "")
Sys.setenv(CONNECT_CONTENT_DIR=scratch)
on.exit({ Sys.setenv(CONNECT_CONTENT_DIR=old_root); unlink(scratch, recursive=TRUE) }, add=TRUE)
source(app, local=FALSE)
test_db <- shared_db_path(FALSE)
setup <- dbConnect(SQLite(), test_db)
on.exit(dbDisconnect(setup), add=TRUE)
dbExecute(setup, "CREATE TABLE users(user_id TEXT PRIMARY KEY,display_name TEXT,is_admin INTEGER,active INTEGER,is_demo INTEGER DEFAULT 0,course TEXT,section TEXT)")
dbExecute(setup, "CREATE TABLE user_environments(user_id TEXT,course TEXT,section TEXT)")
dbExecute(setup, "CREATE TABLE arcade_sessions(token TEXT PRIMARY KEY,user_id TEXT,expires_at TEXT)")
dbExecute(setup, "CREATE TABLE admin_assignments(user_id TEXT,role TEXT,course TEXT,section TEXT)")
dbExecute(setup, "CREATE TABLE scope_arcade_state(scope_key TEXT PRIMARY KEY,active_game TEXT)")
dbExecute(setup, "INSERT INTO users(user_id,display_name,is_admin,active,course,section) VALUES('student-a','Student A',0,1,'Econ','01'),('student-b','Student B',0,1,'Econ','01'),('instructor','Instructor',1,1,'Econ','01')")
dbExecute(setup, "INSERT INTO user_environments VALUES('student-a','Econ','01'),('student-b','Econ','01')")
dbExecute(setup, "INSERT INTO arcade_sessions VALUES('token-a','student-a','2035-01-01 00:00:00'),('token-b','student-b','2035-01-01 00:00:00'),('token-admin','instructor','2035-01-01 00:00:00')")
dbExecute(setup, "INSERT INTO admin_assignments VALUES('instructor','super_admin','','')")
dbExecute(setup, "INSERT INTO scope_arcade_state VALUES('econ::01','price_war')")
dbExecute(setup, "INSERT INTO price_war_rounds(course,section,round,status) VALUES('Econ','01',1,'open')")

play <- function(token, action) {
  testServer(server, {
    session$setInputs(auth_cookie=token)
    session$flushReact()
    if (identical(action, "High")) session$setInputs(play_high=1) else session$setInputs(play_low=1)
    session$flushReact()
    play_ui <- paste(as.character(output$play_ui), collapse="\n")
    if (!grepl(paste("You have played", action), play_ui, fixed=TRUE)) stop("A player must see their currently saved Price War choice.")
  })
}
play("token-a", "High")
play("token-b", "Low")
submissions <- dbGetQuery(setup, "SELECT user_id,action FROM price_war_submissions ORDER BY user_id")
if (!identical(submissions$action, c("High", "Low"))) stop("Both students must retain their own Price War submissions.")
round_state <- dbGetQuery(setup, "SELECT status FROM price_war_rounds WHERE course='Econ' AND section='01' AND round=1")
if (!identical(round_state$status[1], "open")) stop("Student submissions must not mutate Price War round state.")
testServer(server, {
  session$setInputs(auth_cookie="token-a")
  session$flushReact()
  session$setInputs(withdraw=1)
  session$flushReact()
  play_ui <- paste(as.character(output$play_ui), collapse="\n")
  if (!grepl("You have not yet played.", play_ui, fixed=TRUE)) stop("A player must see that their Price War choice was withdrawn.")
})
if (dbGetQuery(setup, "SELECT COUNT(*) AS n FROM price_war_submissions WHERE user_id='student-a'")$n[1] != 0L) stop("Withdraw must delete only the player's current Price War entry.")
play("token-a", "High")
testServer(server, {
  session$setInputs(auth_cookie="token-a")
  session$flushReact()
  sealed_results <- paste(as.character(output$results_ui), collapse="\n")
  if (!grepl("sealed until the instructor reveals", sealed_results, fixed=TRUE) ||
      grepl("HL", sealed_results, fixed=TRUE)) {
    stop("Students must not see Price War pair counts before Reveal.")
  }
})
testServer(server, {
  session$setInputs(auth_cookie="token-admin")
  session$flushReact()
  diagnostic <- paste(as.character(output$settings_diagnostics), collapse="\n")
  if (!grepl("Live submission diagnostic", diagnostic, fixed=TRUE) || !grepl("Student A", diagnostic, fixed=TRUE) || !grepl("Student B", diagnostic, fixed=TRUE) || !grepl("HL", diagnostic, fixed=TRUE)) {
    stop("Instructor diagnostics must show the saved student entries and their pending pair outcome before reveal.")
  }
  session$setInputs(round=1, status="open", scale=.75, hh_a=3, hh_b=3, hl_a=0, hl_b=5, lh_a=5, lh_b=0, ll_a=1, ll_b=1, save_round=1)
  session$flushReact()
})
saved_scale <- dbGetQuery(setup, "SELECT pd_scale FROM price_war_rounds WHERE course='Econ' AND section='01' AND round=1")$pd_scale[1]
if (!isTRUE(all.equal(as.numeric(saved_scale), .75))) stop("Instructor Price War settings must save the edited values.")
cat("Price War student-play workflow: OK\n")
