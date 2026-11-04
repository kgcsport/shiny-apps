# Price War — independently scoped coordination game.
library(shiny); library(DBI); library(RSQLite); library(bcrypt)

this_dir <- getwd()
shared <- Filter(file.exists, c(file.path(this_dir, "..", "_shared", "sqlite.R"), file.path("apps", "_shared", "sqlite.R"), "/srv/shiny-server/_shared/sqlite.R"))[[1]]
source(shared)
GOOGLE_AUTH_ENABLED <- nzchar(Sys.getenv("GOOGLE_CLIENT_ID", "")) && nzchar(Sys.getenv("GOOGLE_CLIENT_SECRET", "")) && nzchar(Sys.getenv("SHINY_APP_URL", ""))
DB_PATH <- shared_db_path()
con <- NULL
db <- function() { if (is.null(con) || !dbIsValid(con)) con <<- connect_sqlite(DB_PATH); con }
q <- function(sql, p=list()) {
  if (length(p)) dbGetQuery(db(), sql, params=p) else dbGetQuery(db(), sql)
}
x <- function(sql, p=list()) {
  if (length(p)) dbExecute(db(), sql, params=p) else dbExecute(db(), sql)
}
`%||%` <- function(a,b) if (is.null(a) || !length(a) || is.na(a[1])) b else a
key <- function(course, section) paste0(tolower(trimws(course)), "::", tolower(trimws(section)))

# Clean-slate schema: its records do not read or write the legacy coordination
# game tables used by Bonus Pot.
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

shared_auth_js <- tags$script(HTML("
(function() {
  Shiny.addCustomMessageHandler('set_arcade_cookie', function(msg) {
    if (msg.token) {
      document.cookie = 'arcade_token=' + encodeURIComponent(msg.token) +
        '; expires=' + new Date(msg.expires).toUTCString() + '; path=/; SameSite=Lax';
    }
  });
  function sendAuthCookie() {
    var match = document.cookie.match(/(?:^|; )arcade_token=([^;]*)/);
    Shiny.setInputValue('auth_cookie', match ? decodeURIComponent(match[1]) : '', {priority: 'event'});
  }
  $(document).on('shiny:sessioninitialized', sendAuthCookie);
  setTimeout(sendAuthCookie, 300);
})();
"))

ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f7f7f5}.pw{max-width:850px;margin:2rem auto}.card{background:#fff;border:1px solid #ddd;border-radius:9px;padding:1.2rem;margin:.8rem 0}.live{color:#176b3a;font-weight:700}.muted{color:#666}.login-page{max-width:420px;margin:4rem auto;padding:0 1rem}.login-card{background:#fff;border-radius:12px;padding:2rem 2.25rem;box-shadow:0 3px 14px rgba(0,0,0,.1)}.login-logo{font-size:1.6rem;font-weight:700;color:#951829;margin-bottom:.35rem;text-align:center}.login-tagline{font-size:.83rem;color:#888;text-align:center;margin-bottom:1.25rem}.btn-google{display:block;text-align:center;width:100%;padding:.65rem 1rem;background:#fff;border:1.5px solid #dadce0;border-radius:6px;font-size:.95rem;font-weight:500;color:#3c4043;margin-bottom:.75rem}.admin-login-toggle summary{cursor:pointer;font-size:.82rem;color:#888;text-align:center;padding:.3rem 0}")), shared_auth_js),
  uiOutput("page")
)

server <- function(input, output, session) {
  rv <- reactiveValues(user=NULL, name=NULL, admin=FALSE, course="", section="", message=NULL)
  poll <- reactiveTimer(1500, session)
  logged_in <- reactive(!is.null(rv$user))
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
      r <- roles[i,]; if (r$role == "course_admin") out <- rbind(out, q("SELECT DISTINCT course,section FROM users WHERE lower(course)=lower(?) AND COALESCE(active,1)=1;",list(r$course)))
      else if (r$role == "section_admin") out <- rbind(out,data.frame(course=r$course,section=r$section))
    }; out[!duplicated(paste(tolower(out$course),tolower(out$section))),,drop=FALSE]
  })
  choose_env <- function(course, section) { e <- environments(); hit <- which(tolower(e$course)==tolower(course) & tolower(e$section)==tolower(section)); if(!length(hit)) return(FALSE); rv$course<-e$course[hit[1]];rv$section<-e$section[hit[1]];TRUE }
  active <- reactive({ poll(); z <- tryCatch(q("SELECT active_game FROM scope_arcade_state WHERE scope_key=?;",list(key(rv$course,rv$section))),error=function(e)data.frame()); nrow(z)&&identical(z$active_game[1],"price_war") })
  round_row <- reactive({ poll(); if(!nzchar(rv$course)||!nzchar(rv$section)) return(data.frame()); q("SELECT * FROM price_war_rounds WHERE course=? AND section=? ORDER BY round DESC LIMIT 1;",list(rv$course,rv$section)) })
  finish <- function(row) { rv$user<-row$user_id[1];rv$name<-row$display_name[1];rv$admin<-isTRUE(as.integer(row$is_admin[1])==1); e<-environments();if(nrow(e)){rv$course<-e$course[1];rv$section<-e$section[1]} }
  observeEvent(input$auth_cookie,{ tok<-input$auth_cookie%||%"";if(nzchar(tok)&&!logged_in()){z<-tryCatch(q("SELECT u.user_id,u.display_name,u.is_admin FROM arcade_sessions s JOIN users u ON u.user_id=s.user_id WHERE s.token=? AND datetime(s.expires_at)>datetime('now') LIMIT 1;",list(tok)),error=function(e)data.frame());if(nrow(z))finish(z)}},ignoreInit=FALSE)
  issue_shared_cookie <- function(user_id) { token<-paste(sample(c(letters,LETTERS,0:9),48L,replace=TRUE),collapse=""); x("INSERT INTO arcade_sessions(token,user_id,expires_at) VALUES(?,?,datetime('now','+14 days'));",list(token,user_id)); session$sendCustomMessage("set_arcade_cookie",list(token=token,expires=format(Sys.time()+14*86400,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))) }
  observeEvent(input$login_btn,{ z<-tryCatch(q("SELECT user_id,display_name,pw_hash,is_admin,active FROM users WHERE lower(user_id)=lower(?) LIMIT 1;",list(input$login_user%||%"")),error=function(e)data.frame());if(!nrow(z)||!isTRUE(as.integer(z$active[1])==1)||!bcrypt::checkpw(input$login_pw%||%"",z$pw_hash[1])){rv$message<-"Incorrect username or password.";return()};finish(z);issue_shared_cookie(z$user_id[1]) })
  observeEvent(input$save_round,{ req(rv$admin,nzchar(rv$course),nzchar(rv$section)); vals<-as.numeric(c(input$scale,input$hh_a,input$hh_b,input$hl_a,input$hl_b,input$lh_a,input$lh_b,input$ll_a,input$ll_b));if(any(!is.finite(vals)))return(); x("INSERT INTO price_war_rounds(course,section,round,status,pd_scale,hh_a,hh_b,hl_a,hl_b,lh_a,lh_b,ll_a,ll_b,updated_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,CURRENT_TIMESTAMP) ON CONFLICT(course,section,round) DO UPDATE SET status=excluded.status,pd_scale=excluded.pd_scale,hh_a=excluded.hh_a,hh_b=excluded.hh_b,hl_a=excluded.hl_a,hl_b=excluded.hl_b,lh_a=excluded.lh_a,lh_b=excluded.lh_b,ll_a=excluded.ll_a,ll_b=excluded.ll_b,updated_at=CURRENT_TIMESTAMP;",as.list(c(rv$course,rv$section,as.integer(input$round),input$status,vals))); showNotification("Price War round saved.") })
  observeEvent(input$submit,{ req(logged_in(),active());r<-round_row();if(!nrow(r)||r$status[1]!="open")return();a<-input$action%||%"";if(!a%in%c("High","Low"))return();x("INSERT INTO price_war_submissions(course,section,round,user_id,display_name,action,submitted_at) VALUES(?,?,?,?,?,?,CURRENT_TIMESTAMP) ON CONFLICT(course,section,round,user_id) DO UPDATE SET action=excluded.action,submitted_at=CURRENT_TIMESTAMP;",list(rv$course,rv$section,r$round[1],rv$user,rv$name,a));showNotification("Choice saved.") })
  observeEvent(input$payout,{ req(rv$admin);r<-round_row();if(!nrow(r)||r$status[1]!="revealed")return();s<-q("SELECT * FROM price_war_submissions WHERE course=? AND section=? AND round=? ORDER BY submitted_at,user_id;",list(rv$course,rv$section,r$round[1]));if(nrow(s)<2)return();s<-s[seq_len(nrow(s)-nrow(s)%%2),];for(i in seq(1,nrow(s),by=2)){a<-s$action[i];b<-s$action[i+1];pay<-if(a=="High"&&b=="High")c(r$hh_a[1],r$hh_b[1])else if(a=="High")c(r$hl_a[1],r$hl_b[1])else if(b=="High")c(r$lh_a[1],r$lh_b[1])else c(r$ll_a[1],r$ll_b[1]);for(j in 0:1){amt<-round(pay[j+1]*r$pd_scale[1]*2)/2; if(amt>0){n<-x("INSERT OR IGNORE INTO price_war_payouts(course,section,round,user_id,payout) VALUES(?,?,?,?,?);",list(rv$course,rv$section,r$round[1],s$user_id[i+j],amt));if(n==1)x("INSERT INTO token_ledger(user_id,display_name,source_type,source_id,amount,earning,note) VALUES(?,?,?,?,?,1,?);",list(s$user_id[i+j],s$display_name[i+j],"price_war_payout",r$round[1],amt,paste("Price War",rv$course,rv$section,"round",r$round[1])))} }};showNotification("Price War payouts applied once.") })
  output$page <- renderUI({
    if(!logged_in()) return(div(class="login-page",div(class="login-card",div(class="login-logo","🎓 Price War"),div(class="login-tagline",if(GOOGLE_AUTH_ENABLED)"Sign in with your school Google account." else "Log in with your Class Job Market credentials."),if(!is.null(rv$message))p(style="color:#951829",rv$message),if(GOOGLE_AUTH_ENABLED)tags$a(href="/auth/login?return_to=%2Fprice-war%2F",class="btn-google","Sign in with Google"),if(GOOGLE_AUTH_ENABLED)tags$details(class="admin-login-toggle",tags$summary("Sign in with password"),textInput("login_user",NULL,placeholder="Username"),passwordInput("login_pw",NULL,placeholder="Password"),actionButton("login_btn","Sign In →",class="btn btn-primary btn-block"))else tagList(textInput("login_user",NULL,placeholder="Username"),passwordInput("login_pw",NULL,placeholder="Password"),actionButton("login_btn","Sign In →",class="btn btn-primary btn-block")))))
    e<-environments(); choices<-setNames(paste(e$course,e$section,sep="||"),paste(e$course,"—",e$section)); r<-round_row(); tagList(div(class="pw",div(class="card",h2("Price War"),p("A High/Low-price Prisoner's-Dilemma game."),selectInput("env","Class / section",choices,selected=paste(rv$course,rv$section,sep="||"))),if(!active())div(class="card muted","No Price War round is active for this class / section." ) else if(!nrow(r))div(class="card muted","Your instructor has not set up a Price War round yet.") else div(class="card",h3(paste("Round",r$round[1])),p(class=if(r$status[1]=="open")"live" else "muted",toupper(r$status[1])),if(r$status[1]=="open")tagList(radioButtons("action","Your price",c("High price"="High","Low price"="Low"),inline=TRUE),actionButton("submit","Save choice",class="btn-primary"))),if(rv$admin)div(class="card",h3("Instructor setup"),numericInput("round","Round",value=if(nrow(r))r$round[1]else 1,min=1),selectInput("status","Status",c("pending","open","closed","revealed"),selected=if(nrow(r))r$status[1]else"pending"),numericInput("scale","Tokens per payoff point",value=if(nrow(r))r$pd_scale[1]else .1),fluidRow(column(6,numericInput("hh_a","High / High: A",value=if(nrow(r))r$hh_a[1]else 3),numericInput("hl_a","High / Low: A",value=if(nrow(r))r$hl_a[1]else 0),numericInput("lh_a","Low / High: A",value=if(nrow(r))r$lh_a[1]else 5),numericInput("ll_a","Low / Low: A",value=if(nrow(r))r$ll_a[1]else 1)),column(6,numericInput("hh_b","High / High: B",value=if(nrow(r))r$hh_b[1]else 3),numericInput("hl_b","High / Low: B",value=if(nrow(r))r$hl_b[1]else 5),numericInput("lh_b","Low / High: B",value=if(nrow(r))r$lh_b[1]else 0),numericInput("ll_b","Low / Low: B",value=if(nrow(r))r$ll_b[1]else 1))),actionButton("save_round","Save round",class="btn-primary"),actionButton("payout","Apply revealed payouts",class="btn-success"))))
  })
  observeEvent(input$env,{ p<-strsplit(input$env,"||",fixed=TRUE)[[1]];if(length(p)==2)choose_env(p[1],p[2]) })
  session$onSessionEnded(function(){if(!is.null(con)&&dbIsValid(con))dbDisconnect(con)})
}
shinyApp(ui,server)
