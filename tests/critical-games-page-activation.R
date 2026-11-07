#!/usr/bin/env Rscript

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
source_text <- paste(readLines(app_path,warn=FALSE),collapse="\n")
required <- c(
  'selectInput("games_active_game_sel"',
  'actionButton("games_set_active_btn"',
  'observeEvent(input$games_set_active_btn',
  'apply_active_game_selection(input$games_active_game_sel %||% "")',
  '} else NULL,',
  'set_active_game_for_scope(game)',
  'req(rv$is_admin, is_super_admin(), !rv$impersonating)'
)
missing <- required[!vapply(required,grepl,logical(1),x=source_text,fixed=TRUE)]
if(length(missing))stop("Games-page activation contract missing: ",paste(missing,collapse=" | "))

if(!requireNamespace("shiny",quietly=TRUE)||!requireNamespace("DBI",quietly=TRUE)||
   !requireNamespace("RSQLite",quietly=TRUE)){
  cat("critical Games-page activation: source contract OK\n");quit(status=0)
}

scratch <- tempfile("games-page-activation-")
dir.create(file.path(scratch,"data"),recursive=TRUE)
old_content <- Sys.getenv("CONNECT_CONTENT_DIR",unset=NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR=scratch)
on.exit({
  if(is.na(old_content))Sys.unsetenv("CONNECT_CONTENT_DIR")else Sys.setenv(CONNECT_CONTENT_DIR=old_content)
  unlink(scratch,recursive=TRUE)
},add=TRUE)
app <- new.env(parent=globalenv());source(app_path,local=app)
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES('admin@test.edu','Instructor','',1,'ECON 342','01',1,0);")
app$db_exec("INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo) VALUES('student@test.edu','Student','',0,'ECON 342','01',1,0);")

shiny::testServer(app$server,{
  rv$authed<-TRUE;rv$user_id<-"admin@test.edu";rv$name<-"Instructor"
  rv$course<-"ECON 342";rv$section<-"01";rv$is_admin<-TRUE
  rv$admin_role<-"super_admin";rv$active_course<-"ECON 342"
  rv$active_section<-"01";rv$active_sections<-"01";rv$impersonating<-FALSE
  session$setInputs(arc_tabs="Games & Demos");session$flushReact()

  empty_html <- paste(as.character(output$games_tab),collapse="")
  if(!grepl("Instructor · Active game",empty_html,fixed=TRUE))
    stop("Authorized instructor control is missing from Games & Demos.")
  if(grepl("Active Game Slot",empty_html,fixed=TRUE)||grepl("Active Now",empty_html,fixed=TRUE))
    stop("The inactive game window must disappear completely.")

  session$setInputs(games_active_game_sel="club-insurance-game",games_set_active_btn=1)
  session$flushReact();session$elapse(3100);session$flushReact()
  active_row <- app$db_query("SELECT active_game FROM scope_arcade_state WHERE scope_key='econ 342::01';")
  if(nrow(active_row)!=1L||active_row$active_game[1]!="club-insurance-game")
    stop("Games-page switch did not persist in the selected scope.")
  active_html <- paste(as.character(output$games_tab),collapse="")
  if(!grepl("Active Now",active_html,fixed=TRUE)||!grepl("active_slot_inner",active_html,fixed=TRUE))
    stop("Active game window did not appear after switching.")

  session$setInputs(games_active_game_sel="",games_set_active_btn=2)
  session$flushReact();session$elapse(3100);session$flushReact()
  cleared <- app$db_query("SELECT active_game FROM scope_arcade_state WHERE scope_key='econ 342::01';")
  if(nrow(cleared)!=1L||!is.na(cleared$active_game[1]))
    stop("Games-page turn-off did not clear the selected scope.")
  cleared_html <- paste(as.character(output$games_tab),collapse="")
  if(grepl("Active Now",cleared_html,fixed=TRUE)||grepl("active_slot_inner",cleared_html,fixed=TRUE))
    stop("Active game window remained after turn-off.")
})

cat("critical Games-page activation: PASS\n")
