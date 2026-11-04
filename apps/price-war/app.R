# Price War — independently scoped, simultaneous-entry coordination game.
library(shiny); library(DBI); library(RSQLite); library(bcrypt)

this_dir <- getwd()
shared <- Filter(file.exists, c(file.path(this_dir, "..", "_shared", "sqlite.R"), file.path("apps", "_shared", "sqlite.R"), "/srv/shiny-server/_shared/sqlite.R"))[[1]]
source(shared)
GOOGLE_AUTH_ENABLED <- nzchar(Sys.getenv("GOOGLE_CLIENT_ID", "")) && nzchar(Sys.getenv("GOOGLE_CLIENT_SECRET", "")) && nzchar(Sys.getenv("SHINY_APP_URL", ""))
DB_PATH <- shared_db_path()
con <- NULL
db <- function() { if (is.null(con) || !dbIsValid(con)) con <<- connect_sqlite(DB_PATH); con }
q <- function(sql, p=list()) if (length(p)) dbGetQuery(db(), sql, params=p) else dbGetQuery(db(), sql)
x <- function(sql, p=list()) if (length(p)) dbExecute(db(), sql, params=p) else dbExecute(db(), sql)
`%||%` <- function(a,b) if (is.null(a) || !length(a) || is.na(a[1])) b else a
key <- function(course, section) paste0(tolower(trimws(course)), "::", tolower(trimws(section)))

pair_code_counts <- function(actions) {
  actions <- as.character(actions)
  actions <- actions[actions %in% c("High", "Low")]
  actions <- actions[seq_len(length(actions) - length(actions) %% 2L)]
  codes <- if (length(actions)) paste0(substr(actions[seq(1L, length(actions), by=2L)], 1L, 1L), substr(actions[seq(2L, length(actions), by=2L)], 1L, 1L)) else character()
  totals <- table(factor(codes, levels=c("HH", "HL", "LH", "LL")))
  data.frame(pair=names(totals), count=as.integer(totals), stringsAsFactors=FALSE)
}

# Price War never reads or writes Bonus Pot's legacy coordination tables.
x("CREATE TABLE IF NOT EXISTS price_war_rounds(
  course TEXT NOT NULL, section TEXT NOT NULL, round INTEGER NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending', pd_scale REAL NOT NULL DEFAULT .1,
  hh_a REAL NOT NULL DEFAULT 3, hh_b REAL NOT NULL DEFAULT 3,
  hl_a REAL NOT NULL DEFAULT 0, hl_b REAL NOT NULL DEFAULT 5,
  lh_a REAL NOT NULL DEFAULT 5, lh_b REAL NOT NULL DEFAULT 0,
  ll_a REAL NOT NULL DEFAULT 1, ll_b REAL NOT NULL DEFAULT 1,
  updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY(course,section,round));")
x("CREATE TABLE IF NOT EXISTS price_war_submissions(
  course TEXT NOT NULL, section TEXT NOT NULL, round INTEGER NOT NULL,
  user_id TEXT NOT NULL, display_name TEXT, action TEXT NOT NULL,
  submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY(course,section,round,user_id));")
x("CREATE TABLE IF NOT EXISTS price_war_payouts(
  course TEXT NOT NULL, section TEXT NOT NULL, round INTEGER NOT NULL,
  user_id TEXT NOT NULL, payout REAL NOT NULL, created_at TEXT DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY(course,section,round,user_id));")

shared_auth_js <- tags$script(HTML("(function() {
  Shiny.addCustomMessageHandler('set_arcade_cookie', function(msg) {
    if (msg.token) document.cookie = 'arcade_token=' + encodeURIComponent(msg.token) + '; expires=' + new Date(msg.expires).toUTCString() + '; path=/; SameSite=Lax';
  });
  function sendAuthCookie() {
    var match = document.cookie.match(/(?:^|; )arcade_token=([^;]*)/);
    Shiny.setInputValue('auth_cookie', match ? decodeURIComponent(match[1]) : '', {priority: 'event'});
  }
  $(document).on('shiny:sessioninitialized', sendAuthCookie);
  setTimeout(sendAuthCookie, 300);
})();"))

ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f7f7f5}.pw{max-width:850px;margin:0 auto 2rem}.pw-header{background:#951829;color:#fff;padding:.7rem max(1rem,calc((100vw - 850px)/2));display:flex;align-items:center;gap:.75rem;margin:0 0 1rem}.pw-header-title{font-size:1.3rem;font-weight:700;flex:1}.pw-header-name{font-size:.88rem;opacity:.85}.pw-env-picker{display:flex;gap:.45rem;align-items:center}.pw-env-picker .form-group{margin:0}.pw-env-picker label{font-size:.72rem;margin:0 0 .1rem}.pw-env-picker .selectize-input{min-width:120px;min-height:34px;padding:6px 8px;font-size:.83rem;color:#222}.pw-env-single{font-size:.88rem;padding:.35rem .55rem;border:1px solid rgba(255,255,255,.45);border-radius:6px;background:rgba(255,255,255,.12)}.card{background:#fff;border:1px solid #ddd;border-radius:9px;padding:1.2rem;margin:.8rem 0}.live{color:#176b3a;font-weight:700}.muted{color:#666}.login-page{max-width:420px;margin:4rem auto;padding:0 1rem}.login-card{background:#fff;border-radius:12px;padding:2rem 2.25rem;box-shadow:0 3px 14px rgba(0,0,0,.1)}.login-logo{font-size:1.6rem;font-weight:700;color:#951829;margin-bottom:.35rem;text-align:center}.login-tagline{font-size:.83rem;color:#888;text-align:center;margin-bottom:1.25rem}.btn-google{display:block;text-align:center;width:100%;padding:.65rem 1rem;background:#fff;border:1.5px solid #dadce0;border-radius:6px;font-size:.95rem;font-weight:500;color:#3c4043;margin-bottom:.75rem}.admin-login-toggle summary{cursor:pointer;font-size:.82rem;color:#888;text-align:center;padding:.3rem 0}@media(max-width:640px){.pw-header{flex-wrap:wrap;padding:.6rem .75rem}.pw-header-title{flex-basis:100%}.pw-env-picker{width:100%}.pw-env-picker>div{flex:1}.pw-env-picker .selectize-input{min-width:0}}")), shared_auth_js),
  uiOutput("page")
)

server <- function(input, output, session) {
  rv <- reactiveValues(user=NULL, name=NULL, admin=FALSE, course="", section="", message=NULL, player_user=NULL, player_name=NULL, impersonating=FALSE, my_submission_version=0L)
  logged_in <- reactive(!is.null(rv$user))
  requested_embed <- reactive({
    params <- tryCatch(parseQueryString(sub("^\\?", "", session$clientData$url_search %||% "")), error=function(e) list())
    list(embedded=identical(as.character(params$embedded %||% ""), "1"), course=as.character(params$course %||% ""), section=as.character(params$section %||% ""), impersonate=as.character(params$impersonate %||% ""))
  })
  environments <- reactive({
    req(logged_in())
    if (!rv$admin) {
      z <- tryCatch(q("SELECT course,section FROM user_environments WHERE lower(user_id)=lower(?) ORDER BY course,section;", list(rv$user)), error=function(e)data.frame())
      if (!nrow(z)) z <- q("SELECT course,section FROM users WHERE lower(user_id)=lower(?) LIMIT 1;", list(rv$user))
      return(z)
    }
    roles <- tryCatch(q("SELECT role,course,section FROM admin_assignments WHERE lower(user_id)=lower(?);", list(rv$user)), error=function(e)data.frame())
    if (nrow(roles) && any(roles$role == "super_admin")) return(q("SELECT DISTINCT course,section FROM users WHERE COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0 AND trim(coalesce(course,''))<>'' ORDER BY course,section;"))
    out <- data.frame(course=character(),section=character())
    for(i in seq_len(nrow(roles))) {
      r <- roles[i,]
      if (r$role == "course_admin") out <- rbind(out, q("SELECT DISTINCT course,section FROM users WHERE lower(course)=lower(?) AND COALESCE(active,1)=1;",list(r$course)))
      else if (r$role == "section_admin") out <- rbind(out,data.frame(course=r$course,section=r$section))
    }
    out[!duplicated(paste(tolower(out$course),tolower(out$section))),,drop=FALSE]
  })
  choose_env <- function(course, section) {
    e <- environments(); hit <- which(tolower(e$course)==tolower(course) & tolower(e$section)==tolower(section))
    if(!length(hit)) return(FALSE)
    rv$course <- e$course[hit[1]]; rv$section <- e$section[hit[1]]; TRUE
  }
  current_game_state <- function() {
    active_row <- tryCatch(q("SELECT active_game FROM scope_arcade_state WHERE scope_key=?;",list(key(rv$course,rv$section))),error=function(e)data.frame())
    r <- if (nzchar(rv$course) && nzchar(rv$section)) q("SELECT * FROM price_war_rounds WHERE course=? AND section=? ORDER BY round DESC LIMIT 1;",list(rv$course,rv$section)) else data.frame()
    list(active=nrow(active_row) && identical(active_row$active_game[1],"price_war"), round=r)
  }
  # Like Bonus Pot's olig_poll: student input depends only on game state. A
  # classmate's submission does not touch price_war_rounds and cannot reset it.
  game_poll <- reactivePoll(1500, session,
    checkFunc=function() {
      state <- current_game_state(); active_stamp <- if (state$active) "price_war" else ""
      round_stamp <- if (nrow(state$round)) paste(state$round$round[1],state$round$status[1],state$round$updated_at[1],sep="|") else ""
      paste(rv$course,rv$section,active_stamp,round_stamp,sep="|")
    },
    valueFunc=current_game_state
  )
  # Like Bonus Pot's subs_poll: results may refresh on every submission, but
  # this data source is deliberately not used by the student form or settings.
  submission_poll <- reactivePoll(1500, session,
    checkFunc=function() {
      state <- current_game_state(); r <- state$round
      if (!nrow(r)) return("")
      sig <- q("SELECT COUNT(*) n, COALESCE(GROUP_CONCAT(user_id || ':' || action || ':' || submitted_at, '|'),'') sig FROM price_war_submissions WHERE course=? AND section=? AND round=?;",list(rv$course,rv$section,r$round[1]))
      paste(r$round[1],sig$n[1],sig$sig[1],sep="|")
    },
    valueFunc=function() {
      state <- current_game_state(); r <- state$round
      actions <- if (nrow(r)) q("SELECT action FROM price_war_submissions WHERE course=? AND section=? AND round=? ORDER BY submitted_at,user_id;",list(rv$course,rv$section,r$round[1]))$action else character()
      pair_code_counts(actions)
    }
  )
  # Keep the player's saved choice separate from the class-wide submission
  # poll.  Other students entering the round must not redraw these controls.
  my_submission <- reactive({
    rv$my_submission_version
    state <- game_poll(); r <- state$round
    if (!nrow(r) || !nzchar(rv$player_user)) return("")
    saved <- q("SELECT action FROM price_war_submissions WHERE course=? AND section=? AND round=? AND user_id=? LIMIT 1;",list(rv$course,rv$section,r$round[1],rv$player_user))
    if (nrow(saved)) as.character(saved$action[1]) else ""
  })
  finish <- function(row) { rv$user<-row$user_id[1];rv$name<-row$display_name[1];rv$admin<-isTRUE(as.integer(row$is_admin[1])==1);rv$player_user<-rv$user;rv$player_name<-rv$name;rv$impersonating<-FALSE;e<-environments();if(nrow(e)){rv$course<-e$course[1];rv$section<-e$section[1]};rv$message<-NULL }
  observe({
    req(logged_in())
    request <- requested_embed()
    if (isTRUE(request$embedded) && nzchar(request$course) && nzchar(request$section) &&
        (!identical(tolower(rv$course), tolower(request$course)) || !identical(tolower(rv$section), tolower(request$section)))) {
      choose_env(request$course, request$section)
    }
    if (isTRUE(request$embedded) && rv$admin && nzchar(request$impersonate)) {
      target <- q("SELECT user_id,display_name FROM users WHERE lower(user_id)=lower(?) AND COALESCE(active,1)=1 AND COALESCE(is_admin,0)=0 AND lower(course)=lower(?) AND lower(section)=lower(?) LIMIT 1;",list(request$impersonate,rv$course,rv$section))
      if (nrow(target)) { rv$player_user <- target$user_id[1]; rv$player_name <- target$display_name[1]; rv$impersonating <- TRUE }
    }
  })
  observeEvent(input$auth_cookie,{ tok<-input$auth_cookie%||%"";if(nzchar(tok)&&!logged_in()){z<-tryCatch(q("SELECT u.user_id,u.display_name,u.is_admin FROM arcade_sessions s JOIN users u ON u.user_id=s.user_id WHERE s.token=? AND datetime(s.expires_at)>datetime('now') LIMIT 1;",list(tok)),error=function(e)data.frame());if(nrow(z))finish(z)}},ignoreInit=FALSE)
  issue_shared_cookie <- function(user_id) { token<-paste(sample(c(letters,LETTERS,0:9),48L,replace=TRUE),collapse=""); x("INSERT INTO arcade_sessions(token,user_id,expires_at) VALUES(?,?,datetime('now','+14 days'));",list(token,user_id)); session$sendCustomMessage("set_arcade_cookie",list(token=token,expires=format(Sys.time()+14*86400,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))) }
  observeEvent(input$login_btn,{ z<-tryCatch(q("SELECT user_id,display_name,pw_hash,is_admin,active FROM users WHERE lower(user_id)=lower(?) LIMIT 1;",list(input$login_user%||%"")),error=function(e)data.frame());if(!nrow(z)||!isTRUE(as.integer(z$active[1])==1)||!bcrypt::checkpw(input$login_pw%||%"",z$pw_hash[1])){rv$message<-"Incorrect username or password.";return()};finish(z);issue_shared_cookie(z$user_id[1]) })

observeEvent(input$save_round, {
req(rv$admin, !rv$impersonating,nzchar(rv$course),nzchar(rv$section))
    vals <- as.numeric(c(input$scale,input$hh_a,input$hh_b,input$hl_a,input$hl_b,input$lh_a,input$lh_b,input$ll_a,input$ll_b))
    if(any(!is.finite(vals))) return()
    x("INSERT INTO price_war_rounds(course,section,round,status,pd_scale,hh_a,hh_b,hl_a,hl_b,lh_a,lh_b,ll_a,ll_b,updated_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,CURRENT_TIMESTAMP) ON CONFLICT(course,section,round) DO UPDATE SET status=excluded.status,pd_scale=excluded.pd_scale,hh_a=excluded.hh_a,hh_b=excluded.hh_b,hl_a=excluded.hl_a,hl_b=excluded.hl_b,lh_a=excluded.lh_a,lh_b=excluded.lh_b,ll_a=excluded.ll_a,ll_b=excluded.ll_b,updated_at=CURRENT_TIMESTAMP;",as.list(c(rv$course,rv$section,as.integer(input$round),input$status,vals)))
    showNotification("Price War round saved.")
  })
  save_player_choice <- function(action) {
    req(logged_in())
    state <- current_game_state(); r <- state$round
    if(!state$active || !nrow(r) || r$status[1]!="open") return()
    x("INSERT INTO price_war_submissions(course,section,round,user_id,display_name,action,submitted_at) VALUES(?,?,?,?,?,?,CURRENT_TIMESTAMP) ON CONFLICT(course,section,round,user_id) DO UPDATE SET action=excluded.action,submitted_at=CURRENT_TIMESTAMP;",list(rv$course,rv$section,r$round[1],rv$player_user,rv$player_name,action))
    rv$my_submission_version <- rv$my_submission_version + 1L
    showNotification(paste0("You have played ", action, ". You can change or withdraw until the round closes."))
  }
  observeEvent(input$play_high, save_player_choice("High"))
  observeEvent(input$play_low, save_player_choice("Low"))
  observeEvent(input$withdraw, {
    req(logged_in())
    state <- current_game_state(); r <- state$round
    if(!state$active || !nrow(r) || r$status[1]!="open") return()
    removed <- x("DELETE FROM price_war_submissions WHERE course=? AND section=? AND round=? AND user_id=?;",list(rv$course,rv$section,r$round[1],rv$player_user))
    rv$my_submission_version <- rv$my_submission_version + 1L
    showNotification(if (removed) "Your choice has been withdrawn." else "You have not yet played.")
  })
  observeEvent(input$payout, {
    req(rv$admin, !rv$impersonating)
    r <- current_game_state()$round
    if(!nrow(r)||r$status[1]!="revealed") return()
    s <- q("SELECT * FROM price_war_submissions WHERE course=? AND section=? AND round=? ORDER BY submitted_at,user_id;",list(rv$course,rv$section,r$round[1]))
    if(nrow(s)<2)return();s<-s[seq_len(nrow(s)-nrow(s)%%2),]
    for(i in seq(1,nrow(s),by=2)){a<-s$action[i];b<-s$action[i+1];pay<-if(a=="High"&&b=="High")c(r$hh_a[1],r$hh_b[1])else if(a=="High")c(r$hl_a[1],r$hl_b[1])else if(b=="High")c(r$lh_a[1],r$lh_b[1])else c(r$ll_a[1],r$ll_b[1]);for(j in 0:1){amt<-round(pay[j+1]*r$pd_scale[1]*2)/2;if(amt>0){n<-x("INSERT OR IGNORE INTO price_war_payouts(course,section,round,user_id,payout) VALUES(?,?,?,?,?);",list(rv$course,rv$section,r$round[1],s$user_id[i+j],amt));if(n==1)x("INSERT INTO token_ledger(user_id,display_name,source_type,source_id,amount,earning,note) VALUES(?,?,?,?,?,1,?);",list(s$user_id[i+j],s$display_name[i+j],"price_war_payout",r$round[1],amt,paste("Price War",rv$course,rv$section,"round",r$round[1])))}}}
    showNotification("Price War payouts applied once.")
  })

  output$play_ui <- renderUI({
    state <- game_poll(); r <- state$round
    if (!state$active) return(div(class="card muted","No Price War round is active for this class / section."))
    if (!nrow(r)) return(div(class="card muted","Your instructor has not set up a Price War round yet."))
    saved <- my_submission()
    status_message <- if (nzchar(saved)) paste0("You have played ", saved, ".") else "You have not yet played."
    div(class="card",h3(paste("Round",r$round[1])),p(class=if(r$status[1]=="open")"live" else "muted",toupper(r$status[1])),p(class=if(nzchar(saved))"live" else "muted",status_message),if(r$status[1]=="open")tagList(p("Choose, change, or withdraw your price. Your choice is paired with another student after the round closes."),div(class="btn-group",role="group",actionButton("play_high","High",class="btn-primary"),actionButton("play_low","Low",class="btn-warning"),actionButton("withdraw","Withdraw",class="btn-danger"))) else p("Choices are no longer being accepted."))
  })
  output$results_ui <- renderUI({
    state <- game_poll(); r <- state$round
    if (!nrow(r)) return(div(class="card muted","Results will appear after an instructor creates a round."))
    if (!identical(as.character(r$status[1]), "revealed"))
      return(div(class="card muted", h3(paste("Round", r$round[1], "results")),
                 p("Choices and pair counts are sealed until the instructor reveals the round.")))
    counts <- submission_poll()
    div(class="card",h3(paste("Round",r$round[1],"pair counts")),p("The first choice in each label is Player A; the second is Player B."),fluidRow(lapply(seq_len(nrow(counts)), function(i) column(3,div(class="card",style="text-align:center;margin:.25rem 0;",h4(counts$pair[i]),div(style="font-size:2em;font-weight:700;",counts$count[i]))))))
  })
  output$settings_ui <- renderUI({
    req(rv$admin, !rv$impersonating)
    r <- game_poll()$round
    div(class="card",h3("Round settings"),p("These controls are visible only to instructors."),numericInput("round","Round",value=if(nrow(r))r$round[1]else 1,min=1),selectInput("status","Status",c("pending","open","closed","revealed"),selected=if(nrow(r))r$status[1]else"pending"),numericInput("scale","Tokens per payoff point",value=if(nrow(r))r$pd_scale[1]else .1),fluidRow(column(6,numericInput("hh_a","High / High: A",value=if(nrow(r))r$hh_a[1]else 3),numericInput("hl_a","High / Low: A",value=if(nrow(r))r$hl_a[1]else 0),numericInput("lh_a","Low / High: A",value=if(nrow(r))r$lh_a[1]else 5),numericInput("ll_a","Low / Low: A",value=if(nrow(r))r$ll_a[1]else 1)),column(6,numericInput("hh_b","High / High: B",value=if(nrow(r))r$hh_b[1]else 3),numericInput("hl_b","High / Low: B",value=if(nrow(r))r$hl_b[1]else 5),numericInput("lh_b","Low / High: B",value=if(nrow(r))r$lh_b[1]else 0),numericInput("ll_b","Low / Low: B",value=if(nrow(r))r$ll_b[1]else 1))),actionButton("save_round","Save round",class="btn-primary"),actionButton("payout","Apply revealed payouts",class="btn-success"),tags$hr(),uiOutput("settings_diagnostics"))
  })
  output$settings_diagnostics <- renderUI({
    req(rv$admin)
    r <- game_poll()$round
    if (!nrow(r)) return(div(class="alert alert-info", "Create a round to inspect submissions."))
    counts <- submission_poll()
    entries <- q("SELECT display_name,action,submitted_at FROM price_war_submissions WHERE course=? AND section=? AND round=? ORDER BY submitted_at,user_id;",list(rv$course,rv$section,r$round[1]))
    paired <- sum(counts$count)
    tagList(
      h4("Live submission diagnostic"),
      p(sprintf("%d submitted choice(s): %d complete pair(s), %d waiting for a partner.", nrow(entries), paired, nrow(entries) - paired)),
      fluidRow(lapply(seq_len(nrow(counts)), function(i) column(3,div(class="card",style="text-align:center;margin:.25rem 0;",strong(counts$pair[i]),div(style="font-size:1.5em;font-weight:700;",counts$count[i]))))),
      if (nrow(entries)) tags$table(class="table table-condensed",tags$thead(tags$tr(tags$th("Student"),tags$th("Choice"),tags$th("Submitted"))),tags$tbody(lapply(seq_len(nrow(entries)),function(i) tags$tr(tags$td(entries$display_name[i] %||% "Student"),tags$td(entries$action[i]),tags$td(entries$submitted_at[i]))))) else p(class="muted", "No entries yet.")
    )
  })
  output$page <- renderUI({
    if(!logged_in()) return(div(class="login-page",div(class="login-card",div(class="login-logo","🎓 Price War"),div(class="login-tagline",if(GOOGLE_AUTH_ENABLED)"Sign in with your school Google account." else "Log in with your Class Job Market credentials."),if(!is.null(rv$message))p(style="color:#951829",rv$message),if(GOOGLE_AUTH_ENABLED)tags$a(href="/auth/login?return_to=%2Fprice-war%2F",class="btn-google","Sign in with Google"),if(GOOGLE_AUTH_ENABLED)tags$details(class="admin-login-toggle",tags$summary("Sign in with password"),textInput("login_user",NULL,placeholder="Username"),passwordInput("login_pw",NULL,placeholder="Password"),actionButton("login_btn","Sign In →",class="btn btn-primary btn-block"))else tagList(textInput("login_user",NULL,placeholder="Username"),passwordInput("login_pw",NULL,placeholder="Password"),actionButton("login_btn","Sign In →",class="btn btn-primary btn-block")))))
    e <- environments(); courses <- unique(as.character(e$course)); course <- if(rv$course%in%courses)rv$course else courses[1]; sections <- unique(as.character(e$section[tolower(e$course)==tolower(course)])); section <- if(rv$section%in%sections)rv$section else sections[1]
    tabs <- list(tabPanel("Play",uiOutput("play_ui")),tabPanel("Results",uiOutput("results_ui")))
    if(rv$admin && !rv$impersonating) tabs <- c(tabs,list(tabPanel("Settings",uiOutput("settings_ui"))))
    header <- if (requested_embed()$embedded) NULL else div(class="pw-header",div(class="pw-header-title","🎓 Classroom Economy · Price War"),span(class="pw-header-name",rv$name),if(nrow(e)>1L)div(class="pw-env-picker",div(tags$label("Class"),selectInput("environment_course_sel",NULL,choices=courses,selected=course,width="100%")),div(tags$label("Section"),selectInput("environment_section_sel",NULL,choices=sections,selected=section,width="100%"))) else span(class="pw-env-single",paste(course,"·",section)))
    tagList(header,div(class="pw",div(class="card",h2("Price War"),p("A High/Low-price Prisoner's-Dilemma game."),if(rv$impersonating)div(class="alert alert-info",paste("Viewing as",rv$player_name,"(student view)."))),do.call(tabsetPanel,c(list(id="price_war_tab"),tabs))))
  })
  observeEvent(input$environment_course_sel,{e<-environments();course<-input$environment_course_sel%||%"";rows<-e[tolower(e$course)==tolower(course),,drop=FALSE];if(nrow(rows))choose_env(course,rows$section[1])},ignoreInit=TRUE)
  observeEvent(input$environment_section_sel,{choose_env(input$environment_course_sel%||%rv$course,input$environment_section_sel%||%"")},ignoreInit=TRUE)
  session$onSessionEnded(function(){if(!is.null(con)&&dbIsValid(con))dbDisconnect(con)})
}
shinyApp(ui,server)
