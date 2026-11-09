try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent=TRUE)
suppressPackageStartupMessages({ library(shiny); library(DBI); library(RSQLite); library(bcrypt) })

`%||%` <- function(a,b) if (is.null(a) || !length(a) || (length(a)==1L && is.na(a))) b else a
norm <- function(x) tolower(trimws(as.character(x %||% "")))

this_file <- ""
for (i in rev(seq_len(sys.nframe()))) {
  candidate <- tryCatch(sys.frame(i)$ofile, error=function(e) NULL)
  if (!is.null(candidate) && nzchar(candidate)) { this_file <- normalizePath(candidate, winslash="/", mustWork=TRUE); break }
}
this_dir <- if (nzchar(this_file)) dirname(this_file) else getwd()
shared_candidates <- c(file.path(this_dir,"..","_shared","sqlite.R"),file.path("apps","_shared","sqlite.R"),file.path("_shared","sqlite.R"),file.path("..","_shared","sqlite.R"),file.path("/srv/shiny-server","_shared","sqlite.R"))
shared_path <- Filter(file.exists, shared_candidates)
if (!length(shared_path)) stop("Cannot find shared SQLite helper.")
source(shared_path[[1]])
source(file.path(this_dir,"risk_math.R"))

DB_PATH <- shared_db_path()
con <- NULL
db <- function() { if (is.null(con) || !dbIsValid(con)) con <<- connect_sqlite(DB_PATH); con }
q <- function(sql,p=list()) if(length(p)) dbGetQuery(db(),sql,params=p) else dbGetQuery(db(),sql)
x <- function(sql,p=list()) if(length(p)) dbExecute(db(),sql,params=p) else dbExecute(db(),sql)
key <- function(course,section) paste0(norm(course),"::",norm(section))
fmt <- function(z) sub("\\.?0+$","",sprintf("%.2f",round_tokens(z)))
risk_comparison_table <- function(rows, facets = "round") {
  if (!nrow(rows)) return(data.frame())
  available <- c("round","loss_rule","loss_size","probability","premium_rate",
                 "apply_tokens","band")
  facets <- intersect(as.character(facets %||% "round"), available)
  if (!length(facets)) facets <- "round"
  labels <- c(round="Round",loss_rule="Loss rule",loss_size="Loss size",
              probability="Bad-state chance",premium_rate="Premium rate",
              apply_tokens="Changes balances",band="Starting endowment")
  group_key <- do.call(interaction,c(rows[facets],list(drop=TRUE,lex.order=TRUE)))
  groups <- split(seq_len(nrow(rows)),group_key)
  out <- lapply(groups,function(idx){
    z <- rows[idx,,drop=FALSE]
    keys <- z[1,facets,drop=FALSE]
    names(keys) <- unname(labels[facets])
    data.frame(keys, N=nrow(z), Purchased=sprintf("%.0f%%",100*mean(z$purchased)),
      `Mean max premium`=fmt(mean(z$max_premium)),
      `Median risk-premium share`=if(all(is.na(z$risk_score)))"—"else sprintf("%.1f%%",100*median(z$risk_score,na.rm=TRUE)),
      `Median CARA`=if(all(is.na(z$cara)))"—"else sprintf("%.3f",median(z$cara,na.rm=TRUE)),
      `Median CRRA`=if(all(is.na(z$crra)))"—"else sprintf("%.2f",median(z$crra,na.rm=TRUE)),
      check.names=FALSE,stringsAsFactors=FALSE)
  })
  do.call(rbind,out)
}
risk_distribution_data <- function(rows, metric = "max_premium", facets = "round") {
  metrics <- c(max_premium="Willingness to pay (tokens)",cara="Implied CARA",crra="Implied CRRA")
  metric <- as.character(metric %||% "max_premium")[1]
  if(!metric%in%names(metrics)||!nrow(rows)||!metric%in%names(rows))return(data.frame())
  available<-c("round","loss_rule","loss_size","probability","premium_rate","apply_tokens","band")
  facets<-intersect(as.character(facets%||%character()),available)
  values<-suppressWarnings(as.numeric(rows[[metric]]));keep<-is.finite(values)
  if(!any(keep))return(data.frame())
  rows<-rows[keep,,drop=FALSE];values<-values[keep]
  facet<-if(!length(facets))rep("All revealed choices",nrow(rows))else apply(rows[facets],1,function(z)paste(z,collapse=" · "))
  data.frame(value=values,facet=facet,metric=unname(metrics[metric]),stringsAsFactors=FALSE)
}

x("CREATE TABLE IF NOT EXISTS risk_insurance_rounds(
 course TEXT NOT NULL, section TEXT NOT NULL, round INTEGER NOT NULL,
 status TEXT NOT NULL DEFAULT 'draft', loss_type TEXT NOT NULL DEFAULT 'percent',
 loss_value REAL NOT NULL DEFAULT 0.2, probability REAL NOT NULL DEFAULT 0.5,
 premium_rate REAL NOT NULL DEFAULT 0.5, apply_tokens INTEGER NOT NULL DEFAULT 1,
 settle_seed INTEGER NOT NULL DEFAULT 1, created_by TEXT,
 created_at TEXT DEFAULT CURRENT_TIMESTAMP, updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
 PRIMARY KEY(course,section,round));")
x("CREATE TABLE IF NOT EXISTS risk_insurance_choices(
 course TEXT NOT NULL, section TEXT NOT NULL, round INTEGER NOT NULL,
 user_id TEXT NOT NULL, display_name TEXT, starting_balance REAL NOT NULL,
 loss_amount REAL NOT NULL, posted_premium REAL NOT NULL, max_premium REAL NOT NULL,
 purchased INTEGER NOT NULL, submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
 PRIMARY KEY(course,section,round,user_id));")
x("CREATE TABLE IF NOT EXISTS risk_insurance_outcomes(
 id INTEGER PRIMARY KEY AUTOINCREMENT, course TEXT NOT NULL, section TEXT NOT NULL,
 round INTEGER NOT NULL, user_id TEXT NOT NULL, display_name TEXT,
 shocked INTEGER NOT NULL, premium_debit REAL NOT NULL DEFAULT 0,
 loss_debit REAL NOT NULL DEFAULT 0, indemnity_credit REAL NOT NULL DEFAULT 0,
 net_change REAL NOT NULL DEFAULT 0, end_balance REAL,
 premium_ledger_id INTEGER, loss_ledger_id INTEGER, indemnity_ledger_id INTEGER,
 settled_at TEXT DEFAULT CURRENT_TIMESTAMP,
 UNIQUE(course,section,round,user_id));")

GOOGLE_AUTH_ENABLED <- nzchar(Sys.getenv("GOOGLE_CLIENT_ID","")) && nzchar(Sys.getenv("GOOGLE_CLIENT_SECRET","")) && nzchar(Sys.getenv("SHINY_APP_URL",""))
shared_auth_js <- tags$script(HTML("(function(){
 Shiny.addCustomMessageHandler('set_arcade_cookie',function(m){if(m.token)document.cookie='arcade_token='+encodeURIComponent(m.token)+'; expires='+new Date(m.expires).toUTCString()+'; path=/; SameSite=Lax';});
 function send(){var m=document.cookie.match(/(?:^|; )arcade_token=([^;]*)/);Shiny.setInputValue('auth_cookie',m?decodeURIComponent(m[1]):'',{priority:'event'});}
 $(document).on('shiny:sessioninitialized',send);setTimeout(send,300);
})()"))

css <- "
body{background:#f7f7f5;color:#222}.ri{max-width:980px;margin:0 auto 2rem}.ri-header{background:#951829;color:#fff;padding:.7rem max(1rem,calc((100vw - 980px)/2));display:flex;align-items:center;gap:.75rem;margin-bottom:1rem}.ri-title{font-size:1.3rem;font-weight:700;flex:1}.ri-env{display:flex;gap:.4rem;align-items:center}.ri-env .form-group{margin:0}.ri-env label{font-size:.7rem;margin:0}.ri-env .selectize-input{min-width:120px;min-height:34px;padding:6px;color:#222}.card{background:#fff;border:1px solid #ddd;border-radius:9px;padding:1.1rem;margin:.75rem 0}.metric-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(140px,1fr));gap:.6rem}.metric{background:#f4f0ec;border-radius:7px;padding:.7rem}.metric b{display:block;font-size:1.35rem;color:#951829}.live{color:#176b3a;font-weight:700}.muted{color:#666}.choice-box{border-left:5px solid #951829;background:#fff8f3;padding:.8rem 1rem;margin:.8rem 0}.btn-primary{background:#951829;border-color:#7a1221}.login-page{max-width:420px;margin:4rem auto;padding:0 1rem}.login-card{background:#fff;border-radius:12px;padding:2rem;box-shadow:0 3px 14px rgba(0,0,0,.1)}.login-logo{font-size:1.5rem;font-weight:700;color:#951829;text-align:center}.login-tagline{text-align:center;color:#777;margin:.4rem 0 1.2rem}.btn-google{display:block;text-align:center;padding:.65rem;border:1px solid #ccc;border-radius:6px;margin-bottom:.7rem}.admin-login-toggle summary{cursor:pointer;text-align:center;color:#777}.table-wrap{overflow-x:auto}.ri table{width:100%;font-size:.9rem}.ri th{background:#f1ece8}.ri th,.ri td{padding:.42rem;border-bottom:1px solid #ddd;text-align:right}.ri th:first-child,.ri td:first-child{text-align:left}@media(max-width:650px){.ri-header{flex-wrap:wrap;padding:.6rem .75rem}.ri-title{flex-basis:100%}.ri-env{width:100%}.ri-env>div{flex:1}.ri-env .selectize-input{min-width:0}}
"
ui <- fluidPage(tags$head(tags$style(HTML(css)),shared_auth_js),uiOutput("page"))

server <- function(input,output,session) {
  rv <- reactiveValues(user=NULL,name=NULL,admin=FALSE,course="",section="",message=NULL,
                       player_user=NULL,player_name=NULL,impersonating=FALSE,choice_ver=0L)
  logged_in <- reactive(!is.null(rv$user))

  request <- reactive({
    p <- tryCatch(parseQueryString(sub("^\\?","",session$clientData$url_search %||% "")),error=function(e)list())
    list(embedded=identical(as.character(p$embedded %||% ""),"1"),
         results=identical(as.character(p$view %||% ""),"results"),
         course=as.character(p$course %||% ""),section=as.character(p$section %||% ""),
         impersonate=as.character(p$impersonate %||% ""))
  })
  environments <- reactive({
    req(logged_in())
    if (!rv$admin) {
      z <- tryCatch(q("SELECT course,section FROM user_environments WHERE lower(user_id)=lower(?) ORDER BY course,section;",list(rv$user)),error=function(e)data.frame())
      if(!nrow(z)) z <- q("SELECT course,section FROM users WHERE lower(user_id)=lower(?) LIMIT 1;",list(rv$user))
      return(z)
    }
    roles <- tryCatch(q("SELECT role,course,section FROM admin_assignments WHERE lower(user_id)=lower(?);",list(rv$user)),error=function(e)data.frame())
    if(nrow(roles)&&any(roles$role=="super_admin")) return(q("SELECT DISTINCT course,section FROM users WHERE COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0 AND trim(coalesce(course,''))<>'' ORDER BY course,section;"))
    out <- data.frame(course=character(),section=character())
    for(i in seq_len(nrow(roles))) {
      r <- roles[i,]
      if(r$role=="course_admin") out <- rbind(out,q("SELECT DISTINCT course,section FROM users WHERE lower(course)=lower(?) AND COALESCE(active,1)=1;",list(r$course)))
      else if(r$role=="section_admin") out <- rbind(out,data.frame(course=r$course,section=r$section))
    }
    out[!duplicated(paste(norm(out$course),norm(out$section))),,drop=FALSE]
  })
  choose_env <- function(course,section) {
    e <- environments(); hit <- which(norm(e$course)==norm(course)&norm(e$section)==norm(section))
    if(!length(hit)) return(FALSE)
    rv$course<-e$course[hit[1]];rv$section<-e$section[hit[1]];TRUE
  }
  finish_login <- function(row) {
    rv$user<-row$user_id[1];rv$name<-row$display_name[1];rv$admin<-isTRUE(as.integer(row$is_admin[1])==1L)
    rv$player_user<-rv$user;rv$player_name<-rv$name;rv$impersonating<-FALSE
    e<-environments();if(nrow(e)){rv$course<-e$course[1];rv$section<-e$section[1]};rv$message<-NULL
  }
  issue_cookie <- function(uid) {
    tok<-paste(sample(c(letters,LETTERS,0:9),48,replace=TRUE),collapse="")
    x("INSERT INTO arcade_sessions(token,user_id,expires_at) VALUES(?,?,datetime('now','+14 days'));",list(tok,uid))
    session$sendCustomMessage("set_arcade_cookie",list(token=tok,expires=format(Sys.time()+14*86400,"%Y-%m-%dT%H:%M:%SZ",tz="UTC")))
  }
  observeEvent(input$auth_cookie,{tok<-input$auth_cookie%||%"";if(nzchar(tok)&&!logged_in()){z<-tryCatch(q("SELECT u.user_id,u.display_name,u.is_admin FROM arcade_sessions s JOIN users u ON u.user_id=s.user_id WHERE s.token=? AND datetime(s.expires_at)>datetime('now') LIMIT 1;",list(tok)),error=function(e)data.frame());if(nrow(z))finish_login(z)}},ignoreInit=FALSE)
  observeEvent(input$login_btn,{z<-tryCatch(q("SELECT user_id,display_name,pw_hash,is_admin,active FROM users WHERE lower(user_id)=lower(?) LIMIT 1;",list(input$login_user%||%"")),error=function(e)data.frame());if(!nrow(z)||!isTRUE(as.integer(z$active[1])==1L)||!bcrypt::checkpw(input$login_pw%||%"",z$pw_hash[1])){rv$message<-"Incorrect username or password.";return()};finish_login(z);issue_cookie(z$user_id[1])})
  observe({
    req(logged_in()); r<-request()
    if(nzchar(r$course)&&nzchar(r$section)) choose_env(r$course,r$section)
    if(r$embedded&&rv$admin&&nzchar(r$impersonate)) {
      z<-q("SELECT user_id,display_name FROM users WHERE lower(user_id)=lower(?) AND lower(course)=lower(?) AND lower(section)=lower(?) AND COALESCE(active,1)=1 AND COALESCE(is_admin,0)=0 LIMIT 1;",list(r$impersonate,rv$course,rv$section))
      if(nrow(z)){rv$player_user<-z$user_id[1];rv$player_name<-z$display_name[1];rv$impersonating<-TRUE}
    }
  })

  output$risk_impersonation_controls<-renderUI({
    req(rv$admin)
    students<-tryCatch(q("SELECT user_id,display_name FROM users WHERE lower(course)=lower(?) AND lower(section)=lower(?) AND COALESCE(active,1)=1 AND COALESCE(is_admin,0)=0 ORDER BY display_name,user_id;",list(rv$course,rv$section)),error=function(e)data.frame())
    if(rv$impersonating)return(div(class="choice-box",
      span("Viewing as ",strong(rv$player_name))," ",
      actionButton("risk_stop_impersonating","Stop viewing as student",class="btn btn-sm btn-outline-secondary")))
    if(!nrow(students))return(p(class="muted","No active students in this class / section."))
    div(class="choice-box",
      fluidRow(column(8,selectInput("risk_impersonate_user","View the game as a student",
        choices=setNames(students$user_id,paste0(students$display_name," (",students$user_id,")")),width="100%")),
        column(4,actionButton("risk_impersonate_btn","View as student",class="btn btn-sm btn-primary",style="margin-top:1.55rem;"))))
  })
  observeEvent(input$risk_impersonate_btn,{
    req(rv$admin)
    uid<-as.character(input$risk_impersonate_user%||%"")
    z<-tryCatch(q("SELECT user_id,display_name FROM users WHERE lower(user_id)=lower(?) AND lower(course)=lower(?) AND lower(section)=lower(?) AND COALESCE(active,1)=1 AND COALESCE(is_admin,0)=0 LIMIT 1;",list(uid,rv$course,rv$section)),error=function(e)data.frame())
    if(!nrow(z)){showNotification("Choose an active student in this class / section.",type="error");return()}
    rv$player_user<-z$user_id[1];rv$player_name<-z$display_name[1];rv$impersonating<-TRUE
    updateTabsetPanel(session,"risk_tabs",selected="Play")
  },ignoreNULL=TRUE)
  observeEvent(input$risk_stop_impersonating,{
    req(rv$admin)
    rv$player_user<-rv$user;rv$player_name<-rv$name;rv$impersonating<-FALSE
  },ignoreNULL=TRUE)

  latest_round <- function() if(nzchar(rv$course)&&nzchar(rv$section)) q("SELECT * FROM risk_insurance_rounds WHERE lower(course)=lower(?) AND lower(section)=lower(?) ORDER BY round DESC LIMIT 1;",list(rv$course,rv$section)) else data.frame()
  is_active <- function() {z<-tryCatch(q("SELECT active_game FROM scope_arcade_state WHERE scope_key=?;",list(key(rv$course,rv$section))),error=function(e)data.frame());nrow(z)&&identical(z$active_game[1],"club-insurance-game")}
  state <- reactivePoll(1500,session,
    checkFunc=function(){r<-latest_round();paste(rv$course,rv$section,is_active(),if(nrow(r))paste(r$round[1],r$status[1],r$updated_at[1])else"",sep="|")},
    valueFunc=function()list(active=is_active(),round=latest_round()))
  submission_data <- reactivePoll(1500,session,
    checkFunc=function(){r<-latest_round();if(!nrow(r))return("");z<-q("SELECT COUNT(*) n,COALESCE(MAX(submitted_at),'') ts,COALESCE(SUM(purchased),0) buyers FROM risk_insurance_choices WHERE lower(course)=lower(?) AND lower(section)=lower(?) AND round=?;",list(rv$course,rv$section,r$round[1]));paste(z$n,z$ts,z$buyers,sep="|")},
    valueFunc=function(){r<-latest_round();if(!nrow(r))return(data.frame());q("SELECT * FROM risk_insurance_choices WHERE lower(course)=lower(?) AND lower(section)=lower(?) AND round=? ORDER BY user_id;",list(rv$course,rv$section,r$round[1]))})
  my_choice <- reactive({rv$choice_ver;r<-state()$round;if(!nrow(r)||!nzchar(rv$player_user))return(data.frame());q("SELECT * FROM risk_insurance_choices WHERE lower(course)=lower(?) AND lower(section)=lower(?) AND round=? AND lower(user_id)=lower(?);",list(rv$course,rv$section,r$round[1],rv$player_user))})
  balance <- function(uid) {z<-q("SELECT COALESCE(SUM(amount),0) b FROM token_ledger WHERE lower(user_id)=lower(?);",list(uid));round_tokens(z$b[1]%||%0)}
  analytics <- function(r,choices) {
    if(!nrow(choices))return(choices)
    choices$expected_loss<-r$probability[1]*choices$loss_amount
    choices$risk_premium<-choices$max_premium-choices$expected_loss
    choices$risk_score<-mapply(naive_risk_score,max_premium=choices$max_premium,
                               loss=choices$loss_amount,MoreArgs=list(probability=r$probability[1]))
    choices$cara<-mapply(implied_cara,max_premium=choices$max_premium,
                         loss=choices$loss_amount,MoreArgs=list(probability=r$probability[1]))
    choices$crra<-mapply(implied_crra,max_premium=choices$max_premium,
                         wealth=choices$starting_balance,loss=choices$loss_amount,
                         MoreArgs=list(probability=r$probability[1]))
    choices$band<-as.character(endowment_band(choices$starting_balance))
    choices$risk_rank<-rank(-choices$risk_score,ties.method="min",na.last="keep")
    choices
  }
  revealed_rounds <- reactive({
    if(!nzchar(rv$course)||!nzchar(rv$section))return(data.frame())
    q("SELECT * FROM risk_insurance_rounds WHERE lower(course)=lower(?) AND lower(section)=lower(?) AND status='revealed' ORDER BY round;",list(rv$course,rv$section))
  })
  selected_result_round <- reactive({
    rounds<-revealed_rounds();if(!nrow(rounds))return(data.frame())
    selected<-suppressWarnings(as.integer(input$results_round_sel%||%tail(rounds$round,1)))
    hit<-rounds[rounds$round==selected,,drop=FALSE]
    if(nrow(hit))hit else rounds[nrow(rounds),,drop=FALSE]
  })
  selected_result_choices <- reactive({
    r<-selected_result_round();if(!nrow(r))return(data.frame())
    q("SELECT * FROM risk_insurance_choices WHERE lower(course)=lower(?) AND lower(section)=lower(?) AND round=? ORDER BY user_id;",list(rv$course,rv$section,r$round[1]))
  })
  all_revealed_analytics <- reactive({
    rounds<-revealed_rounds();if(!nrow(rounds))return(data.frame())
    pieces<-lapply(seq_len(nrow(rounds)),function(i){
      r<-rounds[i,,drop=FALSE]
      choices<-q("SELECT * FROM risk_insurance_choices WHERE lower(course)=lower(?) AND lower(section)=lower(?) AND round=?;",list(rv$course,rv$section,r$round[1]))
      if(!nrow(choices))return(NULL)
      a<-analytics(r,choices)
      a$round<-paste("Round",r$round[1])
      a$loss_rule<-if(r$loss_type[1]=="percent")"Percent of balance"else"Fixed tokens"
      a$loss_size<-if(r$loss_type[1]=="percent")paste0(fmt(100*r$loss_value[1]),"%")else paste0(fmt(r$loss_value[1])," tokens")
      a$probability<-paste0(fmt(100*r$probability[1]),"%")
      a$premium_rate<-paste0(fmt(100*r$premium_rate[1]),"% of loss")
      a$apply_tokens<-if(as.integer(r$apply_tokens[1])==1L)"Yes"else"No"
      a
    })
    pieces<-Filter(Negate(is.null),pieces);if(!length(pieces))data.frame()else do.call(rbind,pieces)
  })

  observeEvent(input$save_choice,{
    req(logged_in());if(rv$admin&&!rv$impersonating){showNotification("Administrators are not student players.",type="warning");return()}
    s<-state();r<-s$round;if(!s$active||!nrow(r)||r$status[1]!="open"){showNotification("This round is not open.",type="error");return()}
    b<-balance(rv$player_user);loss<-insurance_loss(b,r$loss_type[1],r$loss_value[1]);premium<-insurance_premium(loss,r$premium_rate[1]);wtp<-round_tokens(input$max_premium%||%NA_real_)
    if(!is.finite(wtp)||wtp<0||wtp>loss+0.005){showNotification(paste0("Enter a maximum premium from 0 to ",fmt(loss),"."),type="error");return()}
    buy<-as.integer(insurance_purchase(wtp,premium))
    x("INSERT INTO risk_insurance_choices(course,section,round,user_id,display_name,starting_balance,loss_amount,posted_premium,max_premium,purchased,submitted_at) VALUES(?,?,?,?,?,?,?,?,?,?,CURRENT_TIMESTAMP) ON CONFLICT(course,section,round,user_id) DO UPDATE SET display_name=excluded.display_name,starting_balance=excluded.starting_balance,loss_amount=excluded.loss_amount,posted_premium=excluded.posted_premium,max_premium=excluded.max_premium,purchased=excluded.purchased,submitted_at=CURRENT_TIMESTAMP;",list(rv$course,rv$section,r$round[1],rv$player_user,rv$player_name,b,loss,premium,wtp,buy))
    rv$choice_ver<-rv$choice_ver+1L;showNotification(if(buy)paste0("Saved. You buy full insurance for ",fmt(premium)," tokens.")else"Saved. You remain uninsured at the posted price.")
  })

  save_round <- function(status="draft") {
    req(rv$admin,!rv$impersonating,nzchar(rv$course),nzchar(rv$section))
    round<-as.integer(input$round_no%||%NA);type<-input$loss_type%||%"percent";raw<-as.numeric(if(type=="percent")input$loss_percent%||%NA else input$loss_tokens%||%NA);prob<-as.numeric(input$probability%||%NA)/100;rate<-as.numeric(input$premium_rate%||%NA)/100;seed<-as.integer(input$settle_seed%||%NA)
    if(is.na(round)||round<1||!type%in%c("percent","fixed")||!is.finite(raw)||raw<=0||!is.finite(prob)||prob<=0||prob>=1||!is.finite(rate)||rate<0||is.na(seed)){showNotification("Enter a valid round, loss, probability, premium rate, and seed.",type="error");return(FALSE)}
    value<-if(type=="percent")raw/100 else raw
    old<-q("SELECT status FROM risk_insurance_rounds WHERE lower(course)=lower(?) AND lower(section)=lower(?) AND round=?;",list(rv$course,rv$section,round))
    if(nrow(old)&&old$status[1]%in%c("closed","settled","revealed")){showNotification("Closed or settled round parameters are frozen. Start a new round.",type="error");return(FALSE)}
    x("INSERT INTO risk_insurance_rounds(course,section,round,status,loss_type,loss_value,probability,premium_rate,apply_tokens,settle_seed,created_by,updated_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,CURRENT_TIMESTAMP) ON CONFLICT(course,section,round) DO UPDATE SET status=excluded.status,loss_type=excluded.loss_type,loss_value=excluded.loss_value,probability=excluded.probability,premium_rate=excluded.premium_rate,apply_tokens=excluded.apply_tokens,settle_seed=excluded.settle_seed,updated_at=CURRENT_TIMESTAMP;",list(rv$course,rv$section,round,status,type,value,prob,rate,as.integer(isTRUE(input$apply_tokens)),seed,rv$user))
    showNotification(paste("Round",round,"saved as",status));TRUE
  }
  observeEvent(input$save_round,save_round("draft"))
  observeEvent(input$open_round,save_round("open"))
  observeEvent(input$close_round,{req(rv$admin,!rv$impersonating);r<-latest_round();if(nrow(r)&&r$status[1]=="open"){x("UPDATE risk_insurance_rounds SET status='closed',updated_at=CURRENT_TIMESTAMP WHERE course=? AND section=? AND round=?;",list(r$course[1],r$section[1],r$round[1]));showNotification("Round closed.")}else showNotification("Open the round before closing it.",type="warning")})

  observeEvent(input$settle_round,{
    req(rv$admin,!rv$impersonating);r<-latest_round();if(!nrow(r)||r$status[1]!="closed"){showNotification("Close the round before settlement.",type="error");return()}
    choices<-q("SELECT * FROM risk_insurance_choices WHERE course=? AND section=? AND round=? ORDER BY user_id;",list(r$course[1],r$section[1],r$round[1]));if(!nrow(choices)){showNotification("No student choices to settle.",type="error");return()}
    set.seed(as.integer(r$settle_seed[1]));shocked<-runif(nrow(choices))<as.numeric(r$probability[1])
    dbWithTransaction(db(),{
      for(i in seq_len(nrow(choices))){ch<-choices[i,];existing<-q("SELECT id FROM risk_insurance_outcomes WHERE course=? AND section=? AND round=? AND user_id=?;",list(r$course[1],r$section[1],r$round[1],ch$user_id));if(nrow(existing))next
        outcome<-insurance_outcome(ch$starting_balance,ch$loss_amount,ch$posted_premium,ch$purchased==1,shocked[i]);prem<-outcome$premium_debit;loss<-outcome$loss_debit;indemnity<-outcome$indemnity_credit;net<-outcome$net_change;end<-outcome$end_balance
        x("INSERT INTO risk_insurance_outcomes(course,section,round,user_id,display_name,shocked,premium_debit,loss_debit,indemnity_credit,net_change,end_balance) VALUES(?,?,?,?,?,?,?,?,?,?,?);",list(r$course[1],r$section[1],r$round[1],ch$user_id,ch$display_name,as.integer(shocked[i]),prem,loss,indemnity,net,end))
        oid<-q("SELECT last_insert_rowid() id;")$id[1]
        if(as.integer(r$apply_tokens[1])==1L){
          add_ledger<-function(kind,amount,note){if(abs(amount)<.005)return(NA_integer_);x("INSERT INTO token_ledger(user_id,display_name,source_type,source_id,amount,earning,note) VALUES(?,?,?,?,?,0,?);",list(ch$user_id,ch$display_name,kind,oid,amount,note));as.integer(q("SELECT last_insert_rowid() id;")$id[1])}
          pl<-add_ledger("risk_insurance_premium",-prem,paste("Risk & Insurance premium",r$course[1],r$section[1],"round",r$round[1]));ll<-add_ledger("risk_insurance_loss",-loss,paste("Risk & Insurance loss",r$course[1],r$section[1],"round",r$round[1]));il<-add_ledger("risk_insurance_indemnity",indemnity,paste("Risk & Insurance indemnity",r$course[1],r$section[1],"round",r$round[1]))
          x("UPDATE risk_insurance_outcomes SET premium_ledger_id=?,loss_ledger_id=?,indemnity_ledger_id=? WHERE id=?;",list(pl,ll,il,oid))
        }
      }
      x("UPDATE risk_insurance_rounds SET status='settled',updated_at=CURRENT_TIMESTAMP WHERE course=? AND section=? AND round=?;",list(r$course[1],r$section[1],r$round[1]))
    });showNotification("Round settled once. Reveal when ready.")
  })
  observeEvent(input$reveal_round,{req(rv$admin,!rv$impersonating);r<-latest_round();if(nrow(r)&&r$status[1]=="settled"){x("UPDATE risk_insurance_rounds SET status='revealed',updated_at=CURRENT_TIMESTAMP WHERE course=? AND section=? AND round=?;",list(r$course[1],r$section[1],r$round[1]));showNotification("Results revealed.")}else showNotification("Settle before revealing.",type="warning")})

  output$play_ui <- renderUI({
    s<-state();r<-s$round
    if(!s$active)return(div(class="card muted","Risk & Insurance is not active for this class / section."))
    if(!nrow(r))return(div(class="card muted","Your instructor has not created a round."))
    if(rv$admin&&!rv$impersonating)return(div(class="card",h3("Instructor view"),p("Use Settings to configure the round and Results to project aggregates.")))
    b<-balance(rv$player_user);loss<-insurance_loss(b,r$loss_type[1],r$loss_value[1]);premium<-insurance_premium(loss,r$premium_rate[1]);saved<-my_choice()
    round_desc<-if(r$loss_type[1]=="percent")paste0(fmt(100*r$loss_value[1]),"% of your starting balance")else paste0(fmt(r$loss_value[1])," tokens")
    div(class="card",h3(paste("Round",r$round[1])),p(class=if(r$status[1]=="open")"live"else"muted",toupper(r$status[1])),
      div(class="metric-grid",div(class="metric","Starting balance",tags$b(fmt(b)," tokens")),div(class="metric","Loss if unlucky",tags$b(paste0(fmt(loss)," tokens"))),div(class="metric","Chance of loss",tags$b(paste0(fmt(100*r$probability[1]),"%"))),div(class="metric","Full-insurance price",tags$b(paste0(fmt(premium)," tokens")))),
      p("This round's loss is ",strong(round_desc),". Full insurance pays the entire loss. The premium is paid in either state."),
      if(r$status[1]=="open")tagList(div(class="choice-box",numericInput("max_premium","Highest premium you would pay for full insurance",value=if(nrow(saved))saved$max_premium[1]else premium,min=0,max=loss,step=.01),p(class="muted",paste0("At the posted price of ",fmt(premium),", you buy if your maximum is at least the price.")),actionButton("save_choice","Save choice",class="btn-primary")),if(nrow(saved))p(class="live",if(saved$purchased[1]==1)paste0("Saved: insured at ",fmt(saved$posted_premium[1])," tokens.")else"Saved: uninsured at this price.")) else if(r$status[1]=="revealed")uiOutput("my_outcome") else p("Choices are closed. Results have not yet been revealed."))
  })
  output$my_outcome<-renderUI({r<-state()$round;z<-q("SELECT o.*,c.purchased FROM risk_insurance_outcomes o JOIN risk_insurance_choices c USING(course,section,round,user_id) WHERE o.course=? AND o.section=? AND o.round=? AND o.user_id=?;",list(rv$course,rv$section,r$round[1],rv$player_user));if(!nrow(z))return(p("No outcome was recorded for you."));div(class="choice-box",h4(if(z$shocked[1]==1)"Bad state: loss occurred"else"Good state: no loss"),p(if(z$purchased[1]==1)"You bought full insurance."else"You remained uninsured."),p(strong("Net token change: "),fmt(z$net_change[1])," · ",strong("Round-end balance: "),fmt(z$end_balance[1])))})

  aggregate_table <- function(a){
    bands<-levels(endowment_band(1));out<-lapply(bands,function(b){z<-a[a$band==b,,drop=FALSE];if(!nrow(z))return(NULL);data.frame(`Starting endowment`=b,N=nrow(z),Purchased=sum(z$purchased),`Purchase rate`=sprintf("%.0f%%",100*mean(z$purchased)),`Mean max premium`=fmt(mean(z$max_premium)),`Median CARA`=if(all(is.na(z$cara)))"—"else sprintf("%.3f",median(z$cara,na.rm=TRUE)),`Median CRRA`=if(all(is.na(z$crra)))"—"else sprintf("%.2f",median(z$crra,na.rm=TRUE)),check.names=FALSE)});do.call(rbind,out)
  }
  output$results_ui<-renderUI({
    current<-state()$round;rounds<-revealed_rounds()
    if(!nrow(rounds)){
      choices<-submission_data()
      return(div(class="card",h3("Results sealed"),p(sprintf("%d choice%s submitted.",nrow(choices),if(nrow(choices)==1)""else"s")),p("Purchase and risk-preference results appear after settlement and reveal.")))
    }
    r<-selected_result_round();choices<-selected_result_choices();a<-analytics(r,choices)
    o<-q("SELECT * FROM risk_insurance_outcomes WHERE course=? AND section=? AND round=?;",list(rv$course,rv$section,r$round[1]));profit<-sum(o$premium_debit)-sum(o$indemnity_credit)
    round_labels<-vapply(seq_len(nrow(rounds)),function(i){rr<-rounds[i,];rule<-if(rr$loss_type=="percent")paste0(fmt(100*rr$loss_value),"%")else paste0(fmt(rr$loss_value)," tokens");paste0("Round ",rr$round," · ",rule," · p=",fmt(100*rr$probability),"% · premium=",fmt(100*rr$premium_rate),"%")},character(1))
    div(class="card",
      if(nrow(current)&&current$status[1]!="revealed")div(class="choice-box",strong(paste("Round",current$round[1],"is still sealed."))," Prior revealed rounds remain available below."),
      selectInput("results_round_sel","Show revealed round",choices=setNames(rounds$round,round_labels),selected=r$round[1],width="100%"),
      h3(paste("Round",r$round[1],"results")),div(class="metric-grid",div(class="metric","Choices",tags$b(nrow(a))),div(class="metric","Purchased",tags$b(sprintf("%.0f%%",100*mean(a$purchased)))),div(class="metric","Losses realized",tags$b(sum(o$shocked))),div(class="metric","Insurer result",tags$b(paste0(fmt(profit)," tokens")))),h4("Purchases by starting endowment"),div(class="table-wrap",tableOutput("band_table")),h4("Preference measures"),p("Naïve rank compares the risk premium as a share of the possible loss and deliberately ignores assets. CARA uses WTP and loss size. CRRA also uses the starting balance; it is undefined when the bad state leaves zero token wealth."),div(class="metric-grid",div(class="metric","Median risk-premium share",tags$b(if(all(is.na(a$risk_score)))"—"else sprintf("%.1f%%",100*median(a$risk_score,na.rm=TRUE)))),div(class="metric","Median CARA α",tags$b(if(all(is.na(a$cara)))"—"else sprintf("%.3f",median(a$cara,na.rm=TRUE)))),div(class="metric","Median CRRA γ",tags$b(if(all(is.na(a$crra)))"—"else sprintf("%.2f",median(a$crra,na.rm=TRUE))))),
      h4("Compare revealed results"),
      selectizeInput("comparison_facets","Group comparison by",multiple=TRUE,
        choices=c("Round"="round","Loss rule"="loss_rule","Loss size"="loss_size","Bad-state probability"="probability","Premium rate"="premium_rate","Changes real balances"="apply_tokens","Starting endowment band"="band"),selected="round",width="100%"),
      div(class="table-wrap",tableOutput("history_table")),
      h4("Distribution explorer"),p(class="muted","These anonymous market distributions are identical for students and instructors. Individual outcomes remain on Play."),
      fluidRow(column(4,selectInput("distribution_metric","Measure",choices=c("Willingness to pay"="max_premium","Implied CARA"="cara","Implied CRRA"="crra"),selected="max_premium",width="100%")),
        column(8,selectizeInput("distribution_facets","Split histograms by",multiple=TRUE,
          choices=c("Round"="round","Loss rule"="loss_rule","Loss size"="loss_size","Bad-state probability"="probability","Premium rate"="premium_rate","Changes real balances"="apply_tokens","Starting endowment band"="band"),selected="round",width="100%"))),
      plotOutput("distribution_plot",height="520px"))
  })
  output$band_table<-renderTable({r<-selected_result_round();req(nrow(r));aggregate_table(analytics(r,selected_result_choices()))},sanitize.text.function=function(x)x)
  output$history_table<-renderTable({
    rows<-all_revealed_analytics();req(nrow(rows));risk_comparison_table(rows,input$comparison_facets%||%"round")
  },sanitize.text.function=function(x)x)
  output$distribution_plot<-renderPlot({
    d<-risk_distribution_data(all_revealed_analytics(),input$distribution_metric%||%"max_premium",input$distribution_facets%||%"round")
    validate(need(nrow(d),"No finite values are available for this measure."))
    panels<-split(d$value,factor(d$facet,levels=unique(d$facet)));n<-length(panels)
    validate(need(n<=16,"Choose fewer grouping dimensions to keep the histograms readable."))
    nc<-ceiling(sqrt(n));nr<-ceiling(n/nc);old<-par(mfrow=c(nr,nc),mar=c(4,3.5,3,1),oma=c(0,0,1,0));on.exit(par(old),add=TRUE)
    for(lbl in names(panels)){
      z<-panels[[lbl]];hist(z,breaks=max(1,min(10,ceiling(sqrt(length(z))))),col="#951829",border="white",main=lbl,xlab=d$metric[1],ylab="Students")
      if(length(z)>1)rug(z,col="#333333")
    }
  },res=110)

  output$settings_ui<-renderUI({req(rv$admin)
    student_view_controls<-uiOutput("risk_impersonation_controls")
    if(rv$impersonating)return(tagList(student_view_controls,div(class="card",h3("Round settings are locked"),p(class="muted","Stop viewing as a student to configure or advance the round."))))
    r<-state()$round;choices<-submission_data();current<-if(nrow(r))r else data.frame(round=1,status="draft",loss_type="percent",loss_value=.2,probability=.5,premium_rate=.5,apply_tokens=1,settle_seed=1)
    a<-if(nrow(choices))analytics(current,choices)else choices
    tagList(student_view_controls,div(class="card",h3("Round settings"),p("Run a proportional-loss round and a fixed-token-loss round as separate round numbers."),fluidRow(column(4,numericInput("round_no","Round",current$round[1],min=1,step=1)),column(4,selectInput("loss_type","Loss rule",c("Percent of starting balance"="percent","Fixed token loss"="fixed"),selected=current$loss_type[1])),column(4,conditionalPanel("input.loss_type === 'percent'",numericInput("loss_percent","Loss percent",if(current$loss_type[1]=="percent")100*current$loss_value[1]else 20,min=.01,max=100,step=.01)),conditionalPanel("input.loss_type === 'fixed'",numericInput("loss_tokens","Fixed token loss",if(current$loss_type[1]=="fixed")current$loss_value[1]else 4,min=.01,step=.01)))),fluidRow(column(4,numericInput("probability","Bad-state probability (%)",100*current$probability[1],min=1,max=99,step=1)),column(4,numericInput("premium_rate","Premium as % of covered loss",100*current$premium_rate[1],min=0,max=100,step=1)),column(4,numericInput("settle_seed","Draw seed",current$settle_seed[1],min=1,step=1))),checkboxInput("apply_tokens","Apply premiums and losses to real token balances",value=as.integer(current$apply_tokens[1])==1L),p(class="muted",paste0("Actuarially fair premium rate at this probability: ",fmt(100*current$probability[1]),"%. Amounts round to 0.01 token.")),div(style="display:flex;gap:.4rem;flex-wrap:wrap",actionButton("save_round","Save draft"),actionButton("open_round","Open",class="btn-success"),actionButton("close_round","Close",class="btn-warning"),actionButton("settle_round","Settle once",class="btn-danger"),actionButton("reveal_round","Reveal",class="btn-primary")),hr(),h4("Live instructor diagnostic"),p(sprintf("%d submitted · %d would purchase",nrow(choices),if(nrow(choices))sum(choices$purchased)else 0)),if(nrow(a))div(class="table-wrap",tableOutput("individual_table"))else p(class="muted","No choices yet.")))
  })
  output$individual_table<-renderTable({r<-state()$round;choices<-submission_data();req(rv$admin,nrow(r),nrow(choices));a<-analytics(r,choices);data.frame(Student=a$display_name,Balance=fmt(a$starting_balance),Band=a$band,Loss=fmt(a$loss_amount),Fair=fmt(a$expected_loss),WTP=fmt(a$max_premium),Buy=ifelse(a$purchased==1,"Yes","No"),`Naïve rank`=a$risk_rank,CARA=ifelse(is.na(a$cara),"—",sprintf("%.3f",a$cara)),CRRA=ifelse(is.na(a$crra),"—",sprintf("%.2f",a$crra)),check.names=FALSE)},sanitize.text.function=function(x)x)

  output$page<-renderUI({
    if(!logged_in())return(div(class="login-page",div(class="login-card",div(class="login-logo","🎲 Risk & Insurance"),div(class="login-tagline",if(GOOGLE_AUTH_ENABLED)"Sign in with your school Google account."else"Use your Class Job Market credentials."),if(!is.null(rv$message))p(style="color:#951829",rv$message),if(GOOGLE_AUTH_ENABLED)tags$a(href="/auth/login?return_to=%2Fclub-insurance-game%2F",class="btn-google","Sign in with Google"),tags$details(class="admin-login-toggle",open=!GOOGLE_AUTH_ENABLED,tags$summary("Sign in with password"),textInput("login_user",NULL,placeholder="Username"),passwordInput("login_pw",NULL,placeholder="Password"),actionButton("login_btn","Sign In →",class="btn btn-primary btn-block")))))
    e<-environments();courses<-unique(as.character(e$course));course<-if(rv$course%in%courses)rv$course else courses[1];sections<-unique(as.character(e$section[norm(e$course)==norm(course)]));section<-if(rv$section%in%sections)rv$section else sections[1]
    tabs<-list(tabPanel("Play",uiOutput("play_ui")),tabPanel("Results",uiOutput("results_ui")));if(rv$admin)tabs<-c(tabs,list(tabPanel("Settings",uiOutput("settings_ui"))))
    header<-if(request()$embedded||request()$results)NULL else div(class="ri-header",div(class="ri-title","🎓 Classroom Economy · Risk & Insurance"),span(rv$name),if(nrow(e)>1)div(class="ri-env",div(tags$label("Class"),selectInput("env_course",NULL,courses,course)),div(tags$label("Section"),selectInput("env_section",NULL,sections,section)))else span(paste(course,"·",section)))
    selected<-if(request()$results)"Results"else"Play";tagList(header,div(class="ri",div(class="card",h2("Risk & Insurance"),p("Choose what full insurance is worth before the app draws a token loss.")),do.call(tabsetPanel,c(list(id="risk_tabs",selected=selected),tabs))))
  })
  observeEvent(input$env_course,{e<-environments();rows<-e[norm(e$course)==norm(input$env_course),,drop=FALSE];if(nrow(rows))choose_env(input$env_course,rows$section[1])},ignoreInit=TRUE)
  observeEvent(input$env_section,choose_env(input$env_course%||%rv$course,input$env_section%||%""),ignoreInit=TRUE)
  session$onSessionEnded(function(){if(!is.null(con)&&dbIsValid(con))dbDisconnect(con)})
}

shinyApp(ui,server)
