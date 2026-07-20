try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
library(shiny)
library(DBI)
library(RSQLite)
library(bcrypt)

this_file <- ""
for (i in rev(seq_len(sys.nframe()))) {
  candidate_file <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
  if (!is.null(candidate_file) && nzchar(candidate_file)) {
    this_file <- normalizePath(candidate_file, winslash = "/", mustWork = TRUE)
    break
  }
}
this_dir <- if (nzchar(this_file)) dirname(this_file) else getwd()
shared_sqlite_candidates <- c(
  file.path(this_dir, "..", "_shared", "sqlite.R"),
  file.path("apps", "_shared", "sqlite.R"),
  file.path("_shared", "sqlite.R"),
  file.path("..", "_shared", "sqlite.R"),
  file.path("/srv/shiny-server", "_shared", "sqlite.R")
)
shared_sqlite <- Filter(file.exists, shared_sqlite_candidates)
if (!length(shared_sqlite)) {
  stop("Cannot find shared SQLite helper from ", getwd(), ". Tried: ", paste(shared_sqlite_candidates, collapse = ", "))
}
shared_sqlite <- shared_sqlite[[1]]
source(shared_sqlite)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Database ──────────────────────────────────────────────────────────────────
CONNECT_CONTENT_DIR <- appdata_root(getwd())
DB_PATH <- file.path(CONNECT_CONTENT_DIR, "data", "finalqdata.sqlite")

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    conn <<- connect_sqlite(DB_PATH)
  }
  conn
}
db_query <- function(sql, params = NULL) {
  tryCatch(
    if (is.null(params)) DBI::dbGetQuery(get_con(), sql)
    else DBI::dbGetQuery(get_con(), sql, params = params),
    error = function(e) { message("db_query: ", e$message); data.frame() }
  )
}
db_exec <- function(sql, params = NULL) {
  tryCatch(
    if (is.null(params)) DBI::dbExecute(get_con(), sql)
    else DBI::dbExecute(get_con(), sql, params = params),
    error = function(e) { message("db_exec: ", e$message); -1L }
  )
}

# ── Table init ────────────────────────────────────────────────────────────────
db_exec("
  CREATE TABLE IF NOT EXISTS arcade_state (
    id          INTEGER PRIMARY KEY CHECK (id = 1),
    active_game TEXT,
    updated_at  TEXT DEFAULT CURRENT_TIMESTAMP
  );
")
if (!db_query("SELECT COUNT(*) n FROM arcade_state WHERE id=1;")$n[1])
  db_exec("INSERT INTO arcade_state(id, active_game) VALUES(1, NULL);")

db_exec("
  CREATE TABLE IF NOT EXISTS arcade_sessions (
    token      TEXT PRIMARY KEY,
    user_id    TEXT NOT NULL,
    expires_at TEXT NOT NULL,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
  );
")
db_exec("DELETE FROM arcade_sessions WHERE expires_at < CURRENT_TIMESTAMP;")

db_exec("
  CREATE TABLE IF NOT EXISTS arcade_config (
    key   TEXT PRIMARY KEY,
    value TEXT
  );
")
db_exec("INSERT OR IGNORE INTO arcade_config(key,value) VALUES('app_name','Classroom Economy');")

# Ensure these columns exist on the users table (other apps own it, but we add ours)
try(db_exec("ALTER TABLE users ADD COLUMN active  INTEGER DEFAULT 1;"), silent = TRUE)
try(db_exec("ALTER TABLE users ADD COLUMN is_demo INTEGER DEFAULT 0;"), silent = TRUE)

# Demo account
DEMO_HASH <- bcrypt::hashpw("freetour")
db_exec(
  "INSERT OR IGNORE INTO users(user_id, display_name, pw_hash, is_admin, section, active, is_demo)
   VALUES(?,?,?,0,'Demo',1,1);",
  list("demo", "Demo User", DEMO_HASH))

# Seed fake data for demo account (only if ledger is empty)
if (!db_query("SELECT COUNT(*) n FROM ledger WHERE user_id='demo';")$n[1]) {
  for (row in list(
    list("demo",  5.0, "Class Job",  "Record Keeper — Wk 1", "2024-09-05 10:00:00"),
    list("demo",  3.5, "Bonus Pot",  "Round 1 payout",        "2024-09-12 10:00:00"),
    list("demo", -1.0, "Pledge",     "Exam 1 question pledge", "2024-09-15 10:00:00"),
    list("demo",  4.0, "PD payout",  "Prisoner's Dilemma R1", "2024-09-19 10:00:00"),
    list("demo",  1.0, "Class Job",  "Analyst — Wk 2",        "2024-09-26 10:00:00")
  )) {
    db_exec("INSERT INTO ledger(user_id,amount,purpose,meta,created_at) VALUES(?,?,?,?,?);", row)
  }
}

# Ensure olig tables exist
db_exec("
  CREATE TABLE IF NOT EXISTS olig_settings (
    id INTEGER PRIMARY KEY,
    current_round INTEGER DEFAULT 1,
    round_status TEXT DEFAULT 'pending',
    current_game TEXT DEFAULT 'bonus',
    bonus_multiplier REAL DEFAULT 1.5,
    pd_payoff_points REAL DEFAULT 10,
    pd_scale REAL DEFAULT 0.1,
    contrib_cap REAL DEFAULT 0,
    use_section_size INTEGER DEFAULT 1,
    section TEXT DEFAULT '',
    updated_at TEXT DEFAULT CURRENT_TIMESTAMP
  );
")
db_exec("CREATE TABLE IF NOT EXISTS olig_submissions (
  round     INTEGER NOT NULL,
  user_id   TEXT    NOT NULL,
  section   TEXT    DEFAULT 'default',
  choice    TEXT,
  contribute REAL,
  PRIMARY KEY (round, user_id)
);")
db_exec("CREATE TABLE IF NOT EXISTS olig_payouts (
  id      INTEGER PRIMARY KEY AUTOINCREMENT,
  round   INTEGER,
  user_id TEXT,
  game    TEXT,
  payout  REAL,
  meta    TEXT,
  section TEXT DEFAULT 'default'
);")
db_exec("CREATE TABLE IF NOT EXISTS pledges (
  user_id      TEXT,
  exam_id      TEXT DEFAULT 'exam1',
  round        INTEGER,
  pledge       REAL,
  submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, exam_id, round)
);")

SESSION_DAYS <- 14L

make_token <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 48L, replace = TRUE), collapse = "")
}
store_token <- function(token, user_id) {
  db_exec(
    "INSERT INTO arcade_sessions(token, user_id, expires_at)
     VALUES(?, ?, datetime('now', ?));",
    list(token, user_id, paste0("+", SESSION_DAYS, " days"))
  )
}
delete_token <- function(token) {
  if (nzchar(token %||% ""))
    db_exec("DELETE FROM arcade_sessions WHERE token=?;", list(token))
}
lookup_token <- function(token) {
  if (!nzchar(token %||% "")) return(data.frame())
  db_query(
    "SELECT u.user_id, u.display_name, u.is_admin, u.section, u.active,
            COALESCE(u.is_demo,0) AS is_demo
     FROM arcade_sessions s
     JOIN users u ON u.user_id = s.user_id
     WHERE s.token = ? AND s.expires_at > CURRENT_TIMESTAMP;",
    list(token)
  )
}

get_config <- function(key, default = NULL) {
  r <- db_query("SELECT value FROM arcade_config WHERE key=?;", list(key))
  if (!nrow(r) || is.na(r$value[1])) return(default)
  r$value[1]
}
APP_NAME <- get_config("app_name", "Classroom Economy")

# ── Game catalog ──────────────────────────────────────────────────────────────
# type "either"  — live session slot OR elective use between classes
# type "session" — live during class only
# Semester tools (price-index, flex-pass-app) have moved to the TOOLS list.
GAMES <- list(
  list(id = "bonus_pot",        type = "either",
       label = "Bonus Pot",     embedded = TRUE,
       desc = "Contribute flex passes to a shared pot. The group earns back more when participation is high — but individual incentives push the other way."),
  list(id = "prisoners_dilemma", type = "either",
       label = "Prisoner's Dilemma", embedded = TRUE,
       desc = "Cooperate or defect? See how individual incentives produce outcomes that are collectively worse."),
  list(id = "price_war",        type = "either",
       label = "Price War",     embedded = TRUE,
       desc = "Set prices in a duopoly. Can you sustain collusion, or does competition drive prices to the floor?"),
  list(id = "supply-auction-game", type = "either",
       label = "Supply Auction", embedded = FALSE, url = "/supply-auction-game/",
       desc = "Bid in a live ascending-price auction. Win units at the market-clearing price."),
  list(id = "review-quiz",      type = "either",
       label = "Review Quiz",   embedded = FALSE, url = "/review-quiz/",
       desc = "Answer quiz questions and see the live class histogram. Used periodically through the semester."),
  list(id = "excise-tax-game",  type = "session",
       label = "Excise Tax Market", embedded = FALSE, url = "/excise-tax-game/",
       desc = "Trade in a call market before and after an excise tax. See where the burden lands."),
  list(id = "sloman-trading-game", type = "session",
       label = "Sloman Trading Game", embedded = FALSE, url = "/sloman-trading-game/",
       desc = "Produce shapes and see how market prices respond to your team's supply decisions."),
  list(id = "airplanes-game",   type = "session",
       label = "Airplanes",     embedded = FALSE, url = "/airplanes-game/",
       desc = "Enter production data in a live classroom experiment on division of labour."),
  list(id = "club-insurance-game", type = "session",
       label = "Clubs & Insurance", embedded = FALSE, url = "/club-insurance-game/",
       desc = "Choose your insurance level in a group risk pool and explore adverse selection.")
)

game_info <- function(id) Find(function(g) g$id == id, GAMES)

# ── Tools catalog ─────────────────────────────────────────────────────────────
# Always-available simulations and semester-long graded tools.
TOOLS <- list(
  list(id = "indiff-to-demand",  label = "Indifference to Demand",  url = "/indiff-to-demand/",
       desc = "Trace how budget constraints and indifference curves generate a demand curve. Adjust prices and income interactively.",
       graded = FALSE),
  list(id = "theory-of-firm",    label = "Theory of the Firm",      url = "/theory-of-firm/",
       desc = "Explore cost curves, profit maximization, and shutdown decisions for a price-taking firm.",
       graded = FALSE),
  list(id = "tax-incidence",     label = "Tax Incidence",           url = "/tax-incidence/",
       desc = "See how the burden of an excise tax divides between buyers and sellers depending on supply and demand elasticity.",
       graded = FALSE),
  list(id = "price-index",       label = "Price Index",             url = "/price-index/",
       desc = "Build a basket of goods and track prices across waves to measure your personal inflation rate.",
       graded = FALSE),
  list(id = "flex-pass-app",     label = "Flex Pass Accounting",    url = "/flex_pass_actions/",
       desc = "See the full exam-question unlock panel, purchase exam points, and view your complete ledger history.",
       graded = TRUE)
)

# ── CSS ───────────────────────────────────────────────────────────────────────
ARCADE_CSS <- "
body { font-family: system-ui, -apple-system, sans-serif; background: #f4f5f7; margin: 0; }

/* ── Header ─────────────────────────────────────────────────────────────── */
.arc-header {
  background: #951829; color: #fff;
  padding: .7rem 1.5rem;
  display: flex; align-items: center; gap: .75rem;
  margin-bottom: 0;
}
.arc-title  { font-size: 1.3rem; font-weight: 700; flex: 1; letter-spacing: .02em; }
.arc-name   { font-size: .88rem; opacity: .85; }
.arc-bal    { font-size: .88rem; background: rgba(255,255,255,.18);
              padding: .2rem .65rem; border-radius: 999px; font-weight: 600; }
.arc-signout { background: rgba(255,255,255,.15); color: #fff;
               border: 1px solid rgba(255,255,255,.4); font-size: .82rem;
               padding: .25rem .6rem; border-radius: 6px; cursor: pointer; }
.arc-signout:hover { background: rgba(255,255,255,.28); }

/* ── Page body ──────────────────────────────────────────────────────────── */
.arc-body { max-width: 900px; margin: 0 auto; padding: 1.25rem 1rem 3rem; }

/* ── Nav tabs ───────────────────────────────────────────────────────────── */
.nav-tabs { border-bottom: 2px solid #e0e0e0; margin-bottom: 1.25rem; }
.nav-tabs .nav-link        { color: #555; border: none; padding: .55rem .9rem; }
.nav-tabs .nav-link.active { color: #951829; border-bottom: 2px solid #951829;
                              font-weight: 600; margin-bottom: -2px; }
.nav-tabs .nav-link:hover  { color: #951829; }

/* ── Section labels ─────────────────────────────────────────────────────── */
.sec-label {
  font-size: .72rem; font-weight: 700; color: #951829;
  text-transform: uppercase; letter-spacing: .08em;
  border-bottom: 1px solid #e8e8e8; padding-bottom: .3rem;
  margin: 1.1rem 0 .7rem;
}

/* ── Demo banner ────────────────────────────────────────────────────────── */
.demo-banner {
  background: #fff3cd; border: 1px solid #ffc107;
  border-radius: 0; padding: .45rem 1.5rem;
  font-size: .83rem; color: #856404;
}

/* ── Login page ─────────────────────────────────────────────────────────── */
.login-page { max-width: 420px; margin: 4rem auto; padding: 0 1rem; }
.login-card { background: #fff; border-radius: 12px; padding: 2rem 2.25rem;
              box-shadow: 0 3px 14px rgba(0,0,0,.1); }
.login-logo { font-size: 1.6rem; font-weight: 700; color: #951829;
              margin-bottom: .35rem; text-align: center; }
.login-tagline { font-size: .83rem; color: #888; text-align: center;
                 margin-bottom: 1.25rem; }
.btn-block  { width: 100%; }
.btn-demo   { width: 100%; background: transparent; border: 1.5px dashed #ccc;
              color: #666; font-size: .88rem; padding: .45rem; border-radius: 6px;
              cursor: pointer; margin-top: .5rem; }
.btn-demo:hover { border-color: #951829; color: #951829; }
.preview-grid { display: grid; grid-template-columns: 1fr 1fr;
                gap: .5rem; margin: 1rem 0 .75rem; }
.preview-card { background: #f8f8f8; border: 1px solid #e8e8e8; border-radius: 6px;
                padding: .55rem .65rem; }
.preview-card-icon  { font-size: 1.1rem; }
.preview-card-label { font-weight: 600; font-size: .85rem; }
.preview-card-desc  { color: #888; font-size: .74rem; }
.login-howto { margin-top: 1rem; }
.login-howto summary { cursor: pointer; font-weight: 600; color: #951829;
                       font-size: .85rem; text-align: center; }
.login-howto ul { margin: .5rem 0 0; padding-left: 1.1rem; font-size: .84rem; color: #555; }
.login-howto li { margin-bottom: .3rem; }

/* ── How-to callout (per-tab) ───────────────────────────────────────────── */
.tab-howto { background: #f0f4ff; border: 1px solid #c7d7f8; border-radius: 7px;
             padding: .55rem .9rem; margin-bottom: .9rem; font-size: .83rem; color: #3a4e7c; }

/* ── Active game slot ───────────────────────────────────────────────────── */
.slot-card {
  background: #fff; border-radius: 12px; padding: 1.2rem 1.4rem;
  box-shadow: 0 2px 8px rgba(0,0,0,.07); margin-bottom: 1.1rem;
  border-left: 4px solid #951829;
}
.slot-header { font-size: .72rem; font-weight: 700; color: #951829;
               text-transform: uppercase; letter-spacing: .1em; margin-bottom: .7rem; }
.no-game { color: #aaa; text-align: center; padding: 1.5rem 0; font-style: italic; }

/* ── Launch card ────────────────────────────────────────────────────────── */
.launch-card { border: 2px solid #951829; border-radius: 10px;
               padding: 1.1rem 1.3rem; display: flex; align-items: center; gap: 1.25rem; }
.launch-info  { flex: 1; }
.launch-title { font-size: 1.15rem; font-weight: 700; margin-bottom: .3rem; }
.launch-desc  { color: #555; font-size: .88rem; }
.btn-launch   { background: #951829; color: #fff; padding: .5rem 1.2rem;
                border: none; border-radius: 8px; font-size: .93rem; font-weight: 600;
                text-decoration: none; white-space: nowrap; }
.btn-launch:hover { background: #7a1320; color: #fff; text-decoration: none; }

/* ── Badge / pill ───────────────────────────────────────────────────────── */
.badge-live { background: #951829; color: #fff; font-size: .7rem;
              padding: .15rem .45rem; border-radius: 999px;
              vertical-align: middle; margin-left: .4rem; }
.badge-mode { background: #e8f0fe; color: #1a56db; font-size: .72rem;
              padding: .15rem .45rem; border-radius: 4px; font-weight: 600; }
.badge-type { background: #f0f0f0; color: #666; font-size: .7rem;
              padding: .12rem .38rem; border-radius: 3px; }
.badge-graded { background: #fff3cd; color: #856404; font-size: .7rem;
                padding: .12rem .38rem; border-radius: 3px; }

/* ── Today tab ──────────────────────────────────────────────────────────── */
.today-card { background: #fff; border-radius: 10px; border: 1px solid #e8e8e8;
              padding: .9rem 1.1rem; margin-bottom: .65rem; }
.today-active-slot { background: #fff8f8; border: 1px solid #f0c0c8; border-radius: 10px;
                     padding: .9rem 1.1rem; margin-bottom: .8rem; }
.job-tile { border-left: 3px solid #951829; padding: .4rem .75rem;
            background: #fff8f8; border-radius: 0 6px 6px 0; }
.job-tile-name { font-weight: 700; font-size: 1rem; }
.job-tile-meta { color: #888; font-size: .83rem; margin-top: .1rem; }
.wage-tbl { width: 100%; border-collapse: collapse; }
.wage-tbl td { padding: .22rem 0; font-size: .88rem; border-bottom: 1px solid #f4f4f4; }
.wage-tbl td:last-child { text-align: right; font-weight: 600;
                           font-family: ui-monospace, monospace; color: #1a6e3c; }
.wage-tbl tr:last-child td { border-bottom: none; }
.pool-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(130px, 1fr)); gap: .5rem; }
.pool-card { background: #fff; border: 1px solid #e8e8e8; border-radius: 7px;
             padding: .5rem .65rem; font-size: .82rem; }
.pool-card-name { font-weight: 600; margin-bottom: .12rem; }
.pool-card-fill { font-size: .74rem; color: #888; }
.pool-card-full { border-color: #1a6e3c; background: #f0fdf4; }
.pool-card-full .pool-card-fill { color: #1a6e3c; }

/* ── Games catalog ──────────────────────────────────────────────────────── */
.game-list-item { background: #fff; border: 1px solid #e8e8e8; border-radius: 8px;
                  padding: .7rem .9rem; margin-bottom: .4rem; }
.game-list-item.is-expanded { border-color: #951829; }
.game-list-header { display: flex; align-items: center; gap: .6rem; cursor: pointer; }
.game-list-label { font-weight: 600; flex: 1; }
.game-list-detail { margin-top: .65rem; padding-top: .65rem;
                    border-top: 1px solid #f0f0f0; font-size: .88rem; color: #555; }
.game-list-actions { display: flex; gap: .5rem; margin-top: .6rem; flex-wrap: wrap; }

/* ── Tools tab ──────────────────────────────────────────────────────────── */
.tools-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
              gap: .65rem; margin-bottom: 1rem; }
.tool-card { background: #fff; border: 1px solid #e8e8e8; border-radius: 10px;
             padding: 1rem 1.1rem; display: flex; flex-direction: column; gap: .3rem; }
.tool-card-label { font-weight: 600; font-size: .95rem; }
.tool-card-desc  { color: #666; font-size: .82rem; flex: 1; }
.tool-card-foot  { display: flex; align-items: center; justify-content: space-between;
                   margin-top: .5rem; }

/* ── Account tab (merged Wallet + Profile) ──────────────────────────────── */
.bal-tiles { display: grid; grid-template-columns: repeat(3, 1fr); gap: .65rem;
             margin-bottom: 1.25rem; }
@media (max-width: 480px) { .bal-tiles { grid-template-columns: 1fr 1fr; } }
.bal-tile { background: #fff; border-radius: 10px; border: 1px solid #e8e8e8;
            padding: .85rem 1rem; text-align: center; }
.bal-tile-label { font-size: .72rem; color: #888; margin-bottom: .25rem; }
.bal-tile-val   { font-size: 1.8rem; font-weight: 700; line-height: 1.05; }
.bal-tile-sub   { font-size: .68rem; color: #aaa; margin-top: .15rem; }
.bal-tile-fp    .bal-tile-val { color: #951829; }
.bal-tile-toke  .bal-tile-val { color: #1a56db; }
.bal-tile-toke2 .bal-tile-val { color: #555; }
.bal-big   { font-size: 2.2rem; font-weight: 700; color: #951829; line-height: 1.1; }
.bal-label { color: #888; font-size: .8rem; margin-bottom: .1rem; }
.pending-pledge { background: #fff8e1; border: 1px solid #ffe082; border-radius: 8px;
                  padding: .5rem .85rem; font-size: .85rem; color: #795548;
                  margin-top: .5rem; }
.cr { color: #1a6e3c; }
.dr { color: #b00020; }
.profile-panel { background: #fff; border-radius: 10px; padding: 1.1rem;
                 border: 1px solid #e8e8e8; height: 100%; }

/* ── Job Market tab ─────────────────────────────────────────────────────── */
.jm-card { background: #fff; border-radius: 10px; border: 1px solid #e8e8e8;
           padding: 1rem 1.1rem; margin-bottom: .8rem; }
.jm-assignment { border-left: 3px solid #951829; background: #fff8f8;
                 border-radius: 0 8px 8px 0; padding: .6rem .9rem; }
.jm-bid-row { display: flex; align-items: center; gap: .7rem;
              padding: .4rem 0; border-bottom: 1px solid #f4f4f4; }
.jm-bid-row:last-child { border-bottom: none; }
.jm-bid-label { flex: 1; font-size: .9rem; }
.jm-bid-input { width: 100px; flex-shrink: 0; }
.jm-history { font-size: .84rem; color: #555; }

/* ── Admin ──────────────────────────────────────────────────────────────── */
"

# ── UI ────────────────────────────────────────────────────────────────────────
COOKIE_JS <- HTML("
(function() {
  function getCookie(name) {
    var m = document.cookie.match('(?:^|; )' + name.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&') + '=([^;]*)');
    return m ? decodeURIComponent(m[1]) : '';
  }
  $(document).on('shiny:sessioninitialized', function() {
    Shiny.setInputValue('auth_cookie', getCookie('arcade_token'), {priority: 'event'});
  });
  Shiny.addCustomMessageHandler('set_arcade_cookie', function(msg) {
    if (msg.token) {
      document.cookie = 'arcade_token=' + encodeURIComponent(msg.token) +
        '; expires=' + new Date(msg.expires).toUTCString() +
        '; path=/; SameSite=Lax';
    } else {
      document.cookie = 'arcade_token=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/; SameSite=Lax';
    }
  });
})();
")

ui <- fluidPage(
  tags$head(
    tags$title(APP_NAME),
    tags$style(HTML(ARCADE_CSS)),
    tags$script(COOKIE_JS)
  ),
  uiOutput("root_ui")
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    authed         = FALSE,
    user_id        = NULL,
    name           = NULL,
    section        = NULL,
    is_admin       = FALSE,
    is_demo        = FALSE,
    token          = NULL,
    game_detail_id = NULL,    # which game card is expanded in the catalog
    bp_contrib_val = NULL,    # preserve across olig_poll re-renders
    pd_choice_val  = NULL
  )

  # ── Root UI ──────────────────────────────────────────────────────────────────
  output$root_ui <- renderUI({
    if (!rv$authed) {
      # ── Login page ──
      div(class = "login-page",
        div(class = "login-card",
          div(class = "login-logo", paste0("\U0001f393 ", APP_NAME)),
          div(class = "login-tagline", "Log in with credentials from your instructor."),
          textInput("login_user", NULL, placeholder = "Username"),
          passwordInput("login_pw", NULL, placeholder = "Password"),
          actionButton("login_btn", "Sign In →", class = "btn btn-primary btn-block"),
          tags$button(
            type = "button", class = "btn-demo",
            onclick = "Shiny.setInputValue('demo_btn', +new Date(), {priority:'event'});",
            "\U0001f50d Explore without an account — Demo Mode"
          ),
          tags$hr(style = "margin: .9rem 0 .5rem;"),
          tags$p(style = "font-size:.75rem;color:#aaa;text-align:center;margin-bottom:.4rem;",
                 "What's here"),
          div(class = "preview-grid",
            div(class = "preview-card",
              div(class = "preview-card-icon", "\U0001f4cb"),
              div(class = "preview-card-label", "Today"),
              div(class = "preview-card-desc", "Jobs, wages & active game")),
            div(class = "preview-card",
              div(class = "preview-card-icon", "\U0001f3ea"),
              div(class = "preview-card-label", "Job Market"),
              div(class = "preview-card-desc", "Bid for or apply to jobs")),
            div(class = "preview-card",
              div(class = "preview-card-icon", "\U0001f3ae"),
              div(class = "preview-card-label", "Games"),
              div(class = "preview-card-desc", "Live & elective games")),
            div(class = "preview-card",
              div(class = "preview-card-icon", "\U0001f52c"),
              div(class = "preview-card-label", "Tools"),
              div(class = "preview-card-desc", "Economics simulations"))
          ),
          tags$details(class = "login-howto",
            tags$summary("How to get started"),
            tags$ul(
              tags$li(tags$strong("Sign in"), " using the username and password your instructor gave you."),
              tags$li(tags$strong("Today"), " shows your current job assignment, prevailing wages, and any active class game."),
              tags$li(tags$strong("Job Market"), " is where you submit wage bids or ticket allocations each round."),
              tags$li(tags$strong("Games"), " shows the active game and the full catalog — play electively any time."),
              tags$li(tags$strong("Account"), " tracks your Flex Pass balance, Participation Tokens, and transaction history.")
            )
          )
        )
      )
    } else {
      # ── Authenticated app ──
      tagList(
        div(class = "arc-header",
          div(class = "arc-title", paste0("\U0001f393 ", APP_NAME)),
          uiOutput("header_widgets", inline = TRUE),
          actionButton("logout_btn", "Sign out", class = "arc-signout")
        ),
        if (rv$is_demo)
          div(class = "demo-banner",
              "\U0001f50d Demo mode — you're exploring with a fake account. Nothing you do here is saved."),
        div(class = "arc-body",
          tabsetPanel(id = "arc_tabs", type = "tabs", selected = "Today",
            tabPanel("Today",      br(), uiOutput("today_tab")),
            tabPanel("Job Market", br(), uiOutput("job_market_tab")),
            tabPanel("Games",      br(), uiOutput("games_tab")),
            tabPanel("Tools",      br(), uiOutput("tools_tab")),
            tabPanel("Account",    br(), uiOutput("account_tab")),
            uiOutput("admin_tab_panel")
          )
        )
      )
    }
  })

  output$header_widgets <- renderUI({
    req(rv$authed)
    bal <- wallet_bal()
    tagList(
      span(class = "arc-name", rv$name),
      span(class = "arc-bal",  sprintf("%.1f FP", bal))
    )
  })

  # ── Auth helpers ──────────────────────────────────────────────────────────────
  coalesce_str <- function(a, b) if (!is.na(a %||% NA) && nzchar(a %||% "")) a else b

  do_login <- function(row) {
    rv$authed   <- TRUE
    rv$user_id  <- row$user_id[1]
    rv$name     <- coalesce_str(row$display_name[1] %||% "", row$user_id[1])
    rv$section  <- row$section[1] %||% ""
    rv$is_admin <- isTRUE(as.integer(row$is_admin[1] %||% 0L) == 1L)
    rv$is_demo  <- isTRUE(as.integer(row$is_demo[1]  %||% 0L) == 1L)
  }

  issue_cookie <- function(user_id) {
    tok <- make_token()
    store_token(tok, user_id)
    rv$token <- tok
    expires  <- format(Sys.time() + SESSION_DAYS * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    session$sendCustomMessage("set_arcade_cookie", list(token = tok, expires = expires))
  }

  clear_cookie <- function() {
    delete_token(rv$token %||% "")
    rv$token <- NULL
    session$sendCustomMessage("set_arcade_cookie", list(token = ""))
  }

  # ── Cookie auto-login ─────────────────────────────────────────────────────────
  observeEvent(input$auth_cookie, {
    if (rv$authed) return()
    tok <- input$auth_cookie %||% ""
    if (!nzchar(tok)) return()
    row <- lookup_token(tok)
    if (!nrow(row)) return()
    if (isTRUE(as.integer(row$active[1] %||% 1L) == 0L)) return()
    rv$token <- tok
    do_login(row)
  }, ignoreInit = FALSE)

  # ── Manual login ──────────────────────────────────────────────────────────────
  observeEvent(input$login_btn, {
    u <- trimws(input$login_user %||% "")
    p <- input$login_pw %||% ""
    if (!nzchar(u) || !nzchar(p)) {
      showNotification("Enter username and password.", type = "error"); return()
    }
    row <- db_query(
      "SELECT user_id, display_name, pw_hash, is_admin, section, active,
              COALESCE(is_demo,0) AS is_demo
       FROM users WHERE LOWER(user_id) = LOWER(?);", list(u))
    if (!nrow(row) || !bcrypt::checkpw(p, row$pw_hash[1])) {
      showNotification("Incorrect username or password.", type = "error"); return()
    }
    if (isTRUE(as.integer(row$active[1] %||% 1L) == 0L)) {
      showNotification("This account has been archived. Contact your instructor.", type = "error")
      return()
    }
    do_login(row)
    if (!isTRUE(as.integer(row$is_demo[1] %||% 0L) == 1L))
      issue_cookie(row$user_id[1])
  })

  # ── Demo auto-login ───────────────────────────────────────────────────────────
  observeEvent(input$demo_btn, {
    row <- db_query(
      "SELECT user_id, display_name, is_admin, section, active,
              COALESCE(is_demo,0) AS is_demo
       FROM users WHERE user_id = 'demo';")
    if (!nrow(row)) {
      showNotification("Demo account not available.", type = "error"); return()
    }
    do_login(row)
    # No cookie for demo — ephemeral session only
  })

  # ── Logout ────────────────────────────────────────────────────────────────────
  observeEvent(input$logout_btn, {
    clear_cookie()
    rv$authed  <- FALSE; rv$user_id  <- NULL; rv$name    <- NULL
    rv$section <- NULL;  rv$is_admin <- FALSE; rv$is_demo <- FALSE
    rv$game_detail_id <- NULL
  })

  # ── Polls ─────────────────────────────────────────────────────────────────────
  arcade_poll <- reactivePoll(3000, session,
    checkFunc = function()
      db_query("SELECT updated_at FROM arcade_state WHERE id=1;")$updated_at[1] %||% "",
    valueFunc = function()
      db_query("SELECT * FROM arcade_state WHERE id=1;")
  )

  olig_poll <- reactivePoll(3000, session,
    checkFunc = function()
      db_query("SELECT updated_at FROM olig_settings WHERE id=1;")$updated_at[1] %||% "",
    valueFunc = function() {
      list(
        settings = db_query("SELECT * FROM olig_settings WHERE id=1;"),
        my_sub   = if (!is.null(rv$user_id))
          db_query(
            "SELECT s.*, p.payout
             FROM olig_submissions s
             LEFT JOIN olig_payouts p ON p.round=s.round AND p.user_id=s.user_id
             WHERE s.user_id=?
             ORDER BY s.round DESC LIMIT 1;",
            list(rv$user_id))
          else data.frame()
      )
    }
  )

  wallet_poll <- reactivePoll(6000, session,
    checkFunc = function() {
      if (is.null(rv$user_id)) return("")
      r1 <- db_query("SELECT MAX(created_at) ts FROM ledger WHERE user_id=?;",
                     list(rv$user_id))$ts[1] %||% ""
      r2 <- tryCatch(
        db_query("SELECT MAX(created_at) ts FROM token_ledger WHERE user_id=?;",
                 list(rv$user_id))$ts[1] %||% "",
        error = function(e) "")
      paste(r1, r2)
    },
    valueFunc = function() {
      if (is.null(rv$user_id)) return(list(
        ledger = data.frame(), gs = data.frame(), step = 0.5,
        tokens_earned = 0, tokens_on_hand = 0))
      ledger <- db_query(
        "SELECT amount, purpose, meta, created_at FROM ledger
         WHERE user_id=? ORDER BY created_at DESC LIMIT 30;",
        list(rv$user_id))
      gs <- db_query(
        "SELECT gs.*, es.round_open, es.round AS ex_round
         FROM game_state gs
         LEFT JOIN exam_state es ON es.exam_id=gs.active_exam
         WHERE gs.id=1;")
      step <- db_query("SELECT pledge_step FROM settings WHERE id=1;")$pledge_step[1] %||% 0.5
      tokens_earned <- tryCatch(
        as.numeric(db_query(
          "SELECT COALESCE(SUM(amount),0) t FROM token_ledger
           WHERE user_id=? AND earning=1 AND amount>0;",
          list(rv$user_id))$t[1] %||% 0),
        error = function(e) 0)
      tokens_on_hand <- tryCatch(
        as.numeric(db_query(
          "SELECT COALESCE(SUM(amount),0) t FROM token_ledger WHERE user_id=?;",
          list(rv$user_id))$t[1] %||% 0),
        error = function(e) 0)
      list(ledger = ledger, gs = gs, step = step,
           tokens_earned = tokens_earned, tokens_on_hand = tokens_on_hand)
    }
  )

  # Poll for job market data (Today + Job Market tabs)
  jobs_poll <- reactivePoll(8000, session,
    checkFunc = function() {
      uid <- rv$user_id
      if (is.null(uid)) return("")
      r1 <- tryCatch(
        db_query("SELECT MAX(updated_at) ts FROM weekly_rounds;")$ts[1] %||% "",
        error = function(e) "")
      r2 <- tryCatch(
        db_query("SELECT MAX(created_at) ts FROM job_assignments;")$ts[1] %||% "",
        error = function(e) "")
      paste(uid, r1, r2, sep = "|")
    },
    valueFunc = function() {
      uid <- rv$user_id
      empty <- list(round = data.frame(), my_assign = data.frame(),
                    categories = data.frame(), posts = data.frame(),
                    my_wage_bids = data.frame(), my_app_bids = data.frame())
      if (is.null(uid)) return(empty)

      round <- tryCatch(
        db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
        error = function(e) data.frame())

      if (!nrow(round)) return(empty)
      rid <- round$id[1]

      my_assign <- tryCatch(db_query(
        "SELECT jp.job_name, ja.assigned_wage, wr.label AS round_label
         FROM job_assignments ja
         JOIN job_posts jp ON jp.id=ja.job_post_id
         JOIN weekly_rounds wr ON wr.id=ja.round_id
         WHERE ja.user_id=? AND ja.round_id=?
         ORDER BY ja.created_at DESC LIMIT 1;",
        list(uid, rid)), error = function(e) data.frame())

      categories <- tryCatch(db_query(
        "SELECT DISTINCT jc.id, jc.name, jc.default_wage, jc.description
         FROM job_categories jc
         JOIN job_posts jp ON jp.category_id=jc.id
         WHERE jp.round_id=? AND COALESCE(jp.active,1)=1
         ORDER BY jc.display_order, jc.name;",
        list(rid)), error = function(e) data.frame())

      posts <- tryCatch(db_query(
        "SELECT jp.id, jp.job_name, jp.slots,
                COALESCE(jp.wage_override, jc.default_wage) AS wage,
                jc.name AS category_name,
                COALESCE(fill.n, 0) AS filled
         FROM job_posts jp
         LEFT JOIN job_categories jc ON jc.id=jp.category_id
         LEFT JOIN (
           SELECT job_post_id, COUNT(*) n FROM job_assignments
           WHERE status='assigned' GROUP BY job_post_id
         ) fill ON fill.job_post_id=jp.id
         WHERE jp.round_id=? AND COALESCE(jp.active,1)=1
         ORDER BY jp.display_order, jp.job_name;",
        list(rid)), error = function(e) data.frame())

      my_wage_bids <- tryCatch(db_query(
        "SELECT category_id, min_wage FROM wage_bids WHERE user_id=? AND round_id=?;",
        list(uid, rid)), error = function(e) data.frame())

      my_app_bids <- tryCatch(db_query(
        "SELECT category_id, tickets FROM application_bids WHERE user_id=? AND round_id=?;",
        list(uid, rid)), error = function(e) data.frame())

      list(round = round, my_assign = my_assign, categories = categories,
           posts = posts, my_wage_bids = my_wage_bids, my_app_bids = my_app_bids)
    }
  )

  wallet_bal <- reactive({
    wp <- wallet_poll()
    if (!length(wp$ledger) || !nrow(wp$ledger)) return(0)
    as.numeric(sum(wp$ledger$amount, na.rm = TRUE))
  })

  # Preserve typed input values across poll-triggered re-renders.
  observe({ if (!is.null(input$bp_contrib)) rv$bp_contrib_val <- input$bp_contrib })
  observe({ if (!is.null(input$pd_choice))  rv$pd_choice_val  <- input$pd_choice  })

  # ── Today tab ─────────────────────────────────────────────────────────────────
  output$today_tab <- renderUI({
    req(rv$authed)
    active <- arcade_poll()$active_game[1] %||% ""
    jp     <- jobs_poll()

    tagList(
      # Brief context
      div(class = "tab-howto",
        "Your daily snapshot: active class game, your job assignment, prevailing wages, and job pool fill rates."
      ),

      # Active game (conditional)
      if (nzchar(active)) {
        ginfo <- game_info(active)
        div(class = "today-active-slot",
          div(class = "slot-header",
              "▶ Active Game",
              span(class = "badge-live", "LIVE")),
          div(style = "font-weight:700;font-size:1rem;margin-bottom:.2rem;",
              if (!is.null(ginfo)) ginfo$label else active),
          div(style = "color:#888;font-size:.84rem;", "A game is running now."),
          div(style = "margin-top:.65rem;",
            actionButton("go_to_games", "Go to Games tab →",
                         class = "btn btn-sm btn-primary"))
        )
      },

      # My Job Today
      div(class = "sec-label", "My Job Today"),
      if (nrow(jp$my_assign)) {
        r <- jp$my_assign[1, ]
        div(class = "job-tile",
          div(class = "job-tile-name",
              "\U0001f4cb ", r$job_name %||% "—"),
          div(class = "job-tile-meta",
              r$round_label %||% "Current round",
              if (!is.na(r$assigned_wage %||% NA))
                paste0("  ·  Wage: ", sprintf("%.1f FP", as.numeric(r$assigned_wage)))
              else "")
        )
      } else {
        div(style = "color:#999;font-size:.9rem;padding:.4rem 0;",
            "No assignment yet this round. Visit ", tags$strong("Job Market"), " to submit your bids.")
      },

      # Prevailing Wages
      div(class = "sec-label", "Prevailing Wages"),
      if (nrow(jp$posts)) {
        div(class = "today-card",
          tags$table(class = "wage-tbl",
            tags$tbody(lapply(seq_len(nrow(jp$posts)), function(i) {
              r <- jp$posts[i, ]
              tags$tr(
                tags$td(r$job_name %||% r$category_name %||% ""),
                tags$td(if (!is.na(r$wage %||% NA))
                          sprintf("%.1f FP", as.numeric(r$wage))
                        else "—")
              )
            }))
          )
        )
      } else {
        div(style = "color:#999;font-size:.9rem;", "No jobs configured for the current round.")
      },

      # Job Pools
      div(class = "sec-label", "Job Pools"),
      if (nrow(jp$posts)) {
        div(class = "pool-grid",
          lapply(seq_len(nrow(jp$posts)), function(i) {
            r    <- jp$posts[i, ]
            fill <- as.integer(r$filled %||% 0)
            slots <- as.integer(r$slots %||% 0)
            full <- fill >= slots && slots > 0
            div(class = paste("pool-card", if (full) "pool-card-full"),
              div(class = "pool-card-name", r$job_name %||% r$category_name %||% ""),
              div(class = "pool-card-fill",
                  if (slots > 0) sprintf("%d / %d filled%s", fill, slots, if (full) " ✓" else "")
                  else if (fill > 0) sprintf("%d assigned", fill)
                  else "No slots set")
            )
          })
        )
      } else {
        div(style = "color:#999;font-size:.9rem;", "Pool data will appear here once jobs are configured.")
      },

      # Placeholder for future daily plan markdown
      div(style = "margin-top:1.5rem;"),
      tags$details(style = "font-size:.83rem;color:#888;",
        tags$summary(style = "cursor:pointer;color:#951829;font-weight:600;",
                     "How to use this site"),
        tags$ul(style = "margin:.5rem 0 0;padding-left:1.1rem;",
          tags$li(tags$strong("Job Market"), " — submit wage bids or ticket allocations before the round closes."),
          tags$li(tags$strong("Games"), " — your instructor activates a game for class; you can also play electively."),
          tags$li(tags$strong("Account"), " — track your Flex Pass balance, tokens, pledges, and history.")
        )
      )
    )
  })

  # Navigate to Games tab from Today
  observeEvent(input$go_to_games, {
    updateTabsetPanel(session, "arc_tabs", selected = "Games")
  })

  # ── Job Market tab ────────────────────────────────────────────────────────────
  output$job_market_tab <- renderUI({
    req(rv$authed)
    jp <- jobs_poll()

    tagList(
      div(class = "tab-howto",
        "Submit bids for your class job each round. The mode (random / wage bid / ticket allocation) is set by your instructor."
      ),

      # Round info pill
      if (nrow(jp$round)) {
        r    <- jp$round[1, ]
        mode <- r$assignment_mode %||% "random"
        div(style = "display:flex;gap:.4rem;align-items:center;margin-bottom:.8rem;flex-wrap:wrap;",
          span(class = "badge-mode",
               switch(mode,
                 random             = "Mode: random assignment",
                 wage_bidding       = "Mode: wage bidding",
                 application_bidding = "Mode: ticket allocation",
                 paste("Mode:", mode))),
          if (nzchar(r$label %||% ""))
            span(style = "font-size:.83rem;color:#888;", r$label),
          if (nzchar(r$bid_close_date %||% ""))
            span(style = "font-size:.83rem;color:#888;",
                 paste0("Closes: ", r$bid_close_date))
        )
      },

      # Current assignment
      div(class = "sec-label", "Your Assignment This Round"),
      if (nrow(jp$my_assign)) {
        r <- jp$my_assign[1, ]
        div(class = "jm-assignment",
          div(style = "font-weight:700;font-size:1rem;",
              "\U0001f4cb ", r$job_name %||% ""),
          div(style = "color:#888;font-size:.83rem;margin-top:.15rem;",
              if (!is.na(r$assigned_wage %||% NA))
                paste0("Wage: ", sprintf("%.1f FP", as.numeric(r$assigned_wage)))
              else "Wage pending")
        )
      } else {
        div(style = "color:#999;font-size:.9rem;", "No assignment for this round yet.")
      },

      # Bid form
      div(class = "sec-label", "Submit Bids"),
      uiOutput("jm_bid_form"),

      # History
      div(class = "sec-label", "Recent History"),
      uiOutput("jm_history"),

      # Link to full app
      div(style = "margin-top:1.2rem;font-size:.83rem;color:#888;",
        "For detailed results and instructor setup, open the ",
        tags$a(href = "/class-job-market/", target = "_blank", "Class Job Market app"), "."
      )
    )
  })

  output$jm_bid_form <- renderUI({
    req(rv$authed)
    jp   <- jobs_poll()
    if (!nrow(jp$round)) return(div(style = "color:#999;", "No active round configured."))

    r    <- jp$round[1, ]
    mode <- r$assignment_mode %||% "random"
    cats <- jp$categories

    # Determine bid window
    today   <- Sys.Date()
    open_d  <- tryCatch(as.Date(r$bid_open_date %||% NA),  error = function(e) as.Date(NA))
    close_d <- tryCatch(as.Date(r$bid_close_date %||% NA), error = function(e) as.Date(NA))
    window_open   <- !is.na(open_d) && !is.na(close_d) && today >= open_d && today <= close_d
    window_future <- !is.na(open_d) && today < open_d
    window_past   <- !is.na(close_d) && today > close_d

    if (mode == "random") {
      return(div(class = "jm-card",
        tags$p(style = "color:#555;margin:0;",
               "Assignments this round are random — no bids required. Your job will be announced after the round closes.")))
    }

    if (window_future) {
      return(div(class = "alert alert-info",
                 paste0("Bidding opens on ", format(open_d, "%B %d"), ".")))
    }
    if (window_past) {
      return(div(class = "alert alert-secondary",
                 paste0("Bidding closed on ", format(close_d, "%B %d"), ". See your assignment above.")))
    }
    if (!window_open && (!is.na(open_d) || !is.na(close_d))) {
      return(div(class = "alert alert-secondary", "Bidding is not open right now."))
    }
    if (!nrow(cats)) {
      return(div(style = "color:#999;", "No job categories available for this round."))
    }

    if (mode == "wage_bidding") {
      tagList(
        tags$p(style = "color:#555;font-size:.88rem;",
               "Enter the minimum wage you'd accept for each job category.",
               "The instructor takes the cheapest bids and reveals the result in class."),
        div(class = "jm-card",
          lapply(seq_len(nrow(cats)), function(i) {
            cat <- cats[i, ]
            prev_bid <- if (nrow(jp$my_wage_bids)) {
              m <- jp$my_wage_bids[jp$my_wage_bids$category_id == cat$id, , drop = FALSE]
              if (nrow(m)) as.numeric(m$min_wage[1]) else as.numeric(cat$default_wage %||% 0)
            } else as.numeric(cat$default_wage %||% 0)
            div(class = "jm-bid-row",
              div(class = "jm-bid-label",
                  cat$name,
                  if (nzchar(cat$description %||% ""))
                    tags$small(style = "color:#aaa;display:block;", cat$description)),
              div(class = "jm-bid-input",
                  numericInput(paste0("wb_", cat$id), NULL,
                               value = prev_bid, min = 0, step = 0.5))
            )
          }),
          div(style = "margin-top:.65rem;",
            actionButton("submit_wage_bids", "Save wage bids",
                         class = "btn btn-primary"))
        )
      )

    } else if (mode == "application_bidding") {
      tickets_total <- as.integer(r$tickets_per_student %||% 10)
      tagList(
        tags$p(style = "color:#555;font-size:.88rem;",
               sprintf("Allocate up to %d participation tickets across job categories.", tickets_total),
               "More tickets in a category = higher odds of being assigned there."),
        div(class = "jm-card",
          lapply(seq_len(nrow(cats)), function(i) {
            cat <- cats[i, ]
            prev_tickets <- if (nrow(jp$my_app_bids)) {
              m <- jp$my_app_bids[jp$my_app_bids$category_id == cat$id, , drop = FALSE]
              if (nrow(m)) as.integer(m$tickets[1]) else 0L
            } else 0L
            div(class = "jm-bid-row",
              div(class = "jm-bid-label", cat$name,
                  if (nzchar(cat$description %||% ""))
                    tags$small(style = "color:#aaa;display:block;", cat$description)),
              div(class = "jm-bid-input",
                  numericInput(paste0("at_", cat$id), NULL,
                               value = prev_tickets, min = 0, max = tickets_total, step = 1))
            )
          }),
          div(style = "margin-top:.65rem;",
            actionButton("submit_app_bids", "Save ticket allocation",
                         class = "btn btn-primary"))
        )
      )
    } else {
      div(style = "color:#999;", paste("Unsupported mode:", mode))
    }
  })

  output$jm_history <- renderUI({
    req(rv$authed)
    rows <- tryCatch(db_query(
      "SELECT wr.label AS round_label, jp.job_name, ja.assigned_wage,
              ja.created_at
       FROM job_assignments ja
       JOIN job_posts jp ON jp.id=ja.job_post_id
       JOIN weekly_rounds wr ON wr.id=ja.round_id
       WHERE ja.user_id=?
       ORDER BY ja.created_at DESC LIMIT 6;",
      list(rv$user_id)), error = function(e) data.frame())
    if (!nrow(rows))
      return(div(style = "color:#999;font-size:.88rem;", "No assignment history yet."))
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(
        tags$th("Round"), tags$th("Job"), tags$th(style = "text-align:right;", "Wage")
      )),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        tags$tr(
          tags$td(r$round_label %||% ""),
          tags$td(r$job_name %||% ""),
          tags$td(style = "text-align:right;",
                  if (!is.na(r$assigned_wage %||% NA))
                    sprintf("%.1f FP", as.numeric(r$assigned_wage))
                  else "—")
        )
      }))
    )
  })

  # Wage bid submit
  observeEvent(input$submit_wage_bids, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) {
      showNotification("Demo mode — bids are not saved.", type = "warning"); return()
    }
    jp   <- isolate(jobs_poll())
    cats <- jp$categories
    if (!nrow(jp$round) || !nrow(cats)) {
      showNotification("No active round.", type = "error"); return()
    }
    rid  <- jp$round$id[1]
    saved <- 0L
    for (i in seq_len(nrow(cats))) {
      cat_id <- cats$id[i]
      val    <- input[[paste0("wb_", cat_id)]]
      if (!is.null(val) && !is.na(val) && as.numeric(val) >= 0) {
        db_exec(
          "INSERT INTO wage_bids(round_id, category_id, user_id, min_wage)
           VALUES(?,?,?,?)
           ON CONFLICT(round_id, category_id, user_id)
           DO UPDATE SET min_wage=excluded.min_wage, submitted_at=CURRENT_TIMESTAMP;",
          list(rid, cat_id, rv$user_id, as.numeric(val)))
        saved <- saved + 1L
      }
    }
    showNotification(sprintf("Saved %d wage bid%s.", saved, if (saved == 1) "" else "s"),
                     type = "message")
  })

  # Application ticket submit
  observeEvent(input$submit_app_bids, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) {
      showNotification("Demo mode — bids are not saved.", type = "warning"); return()
    }
    jp   <- isolate(jobs_poll())
    cats <- jp$categories
    if (!nrow(jp$round) || !nrow(cats)) {
      showNotification("No active round.", type = "error"); return()
    }
    rid            <- jp$round$id[1]
    tickets_budget <- as.integer(jp$round$tickets_per_student[1] %||% 10L)
    total_alloc    <- 0L
    vals           <- list()
    for (i in seq_len(nrow(cats))) {
      cat_id <- cats$id[i]
      val    <- as.integer(input[[paste0("at_", cat_id)]] %||% 0L)
      if (is.na(val) || val < 0) val <- 0L
      vals[[as.character(cat_id)]] <- val
      total_alloc <- total_alloc + val
    }
    if (total_alloc > tickets_budget) {
      showNotification(
        sprintf("Total tickets (%d) exceeds your budget (%d).", total_alloc, tickets_budget),
        type = "error"); return()
    }
    for (cat_id_str in names(vals)) {
      cat_id <- as.integer(cat_id_str)
      db_exec(
        "INSERT INTO application_bids(round_id, category_id, user_id, tickets)
         VALUES(?,?,?,?)
         ON CONFLICT(round_id, category_id, user_id)
         DO UPDATE SET tickets=excluded.tickets, submitted_at=CURRENT_TIMESTAMP;",
        list(rid, cat_id, rv$user_id, vals[[cat_id_str]]))
    }
    showNotification(
      sprintf("Saved ticket allocation (%d / %d used).", total_alloc, tickets_budget),
      type = "message")
  })

  # ── Games tab ─────────────────────────────────────────────────────────────────
  # Set up game detail toggle observers at server start (inputs may not exist yet;
  # observeEvent with ignoreNULL=TRUE handles that safely).
  lapply(GAMES, function(g) {
    observeEvent(input[[paste0("gd_", g$id)]], {
      rv$game_detail_id <- if (identical(rv$game_detail_id, g$id)) NULL else g$id
    }, ignoreNULL = TRUE)
  })

  output$games_tab <- renderUI({
    req(rv$authed)
    active <- arcade_poll()$active_game[1] %||% ""

    tagList(
      div(class = "tab-howto",
          "Your instructor activates a game for class; you can also play any game electively. Click a game row to read how it works."
      ),

      # Active slot
      if (nzchar(active)) {
        div(class = "slot-card",
          div(class = "slot-header", "▶ Active Now", span(class = "badge-live", "LIVE")),
          uiOutput("active_slot_inner")
        )
      } else {
        div(class = "slot-card",
          div(class = "slot-header", "▶ Active Game Slot"),
          div(class = "no-game", "No game is active right now.")
        )
      },

      # Full catalog
      div(class = "sec-label", "All Games"),
      tagList(lapply(GAMES, function(g) {
        is_live     <- identical(g$id, active)
        is_expanded <- identical(rv$game_detail_id, g$id)
        type_label  <- switch(g$type,
          either  = "Either/or",
          session = "Session only",
          g$type)

        div(class = paste("game-list-item", if (is_expanded) "is-expanded"),
          div(class = "game-list-header",
            div(class = "game-list-label", g$label,
                if (is_live) span(class = "badge-live", "LIVE")),
            span(class = "badge-type", type_label),
            actionButton(paste0("gd_", g$id),
                         if (is_expanded) "▴" else "▾",
                         class = "btn btn-sm btn-outline-secondary",
                         style = "padding:.1rem .45rem;font-size:.8rem;")
          ),
          if (is_expanded) {
            div(class = "game-list-detail",
              tags$p(g$desc),
              div(class = "game-list-actions",
                if (is_live && g$embedded)
                  tags$em(style = "color:#951829;", "↑ Embedded in the active slot above")
                else if (!g$embedded)
                  tags$a(href = g$url, target = "_blank",
                         class = "btn btn-sm btn-primary", "Open game →")
                else if (!is_live)
                  tags$em(style = "color:#aaa;",
                          "Available when instructor activates it")
              )
            )
          }
        )
      }))
    )
  })

  output$active_slot_inner <- renderUI({
    req(rv$authed)
    active <- arcade_poll()$active_game[1] %||% ""
    if (!nzchar(active)) return(div(class = "no-game", "No game active."))

    ginfo <- game_info(active)
    if (is.null(ginfo)) return(div(class = "no-game", "Unknown game."))

    if (ginfo$embedded) {
      switch(active,
        bonus_pot         = uiOutput("embedded_bonus_pot"),
        prisoners_dilemma = uiOutput("embedded_pd"),
        price_war         = uiOutput("embedded_pd"),
        div(class = "no-game", "Embedded UI coming soon.")
      )
    } else {
      div(class = "launch-card",
        div(class = "launch-info",
          div(class = "launch-title", ginfo$label),
          div(class = "launch-desc",  ginfo$desc)
        ),
        tags$a(class = "btn-launch", href = ginfo$url, target = "_blank", "Launch →")
      )
    }
  })

  # ── Embedded: Bonus Pot ───────────────────────────────────────────────────────
  output$embedded_bonus_pot <- renderUI({
    req(rv$authed)
    op     <- olig_poll()
    s      <- op$settings
    if (!nrow(s)) return(div(class = "no-game", "Bonus Pot not configured yet."))

    status <- s$round_status[1] %||% "pending"
    round  <- as.integer(s$current_round[1] %||% 1L)
    mult   <- as.numeric(s$bonus_multiplier[1] %||% 1.5)
    cap    <- as.numeric(s$contrib_cap[1] %||% 0)
    bal    <- wallet_bal()
    sub    <- op$my_sub

    prev_contrib <- if (nrow(sub) && as.integer(sub$round[1]) == round)
      as.numeric(sub$contribute[1] %||% 0) else 0
    max_c <- if (cap > 0) min(cap, floor(bal * 2) / 2) else floor(bal * 2) / 2

    tagList(
      tags$p(
        tags$strong("Round "), round, " · ",
        tags$strong("Status: "),
        span(style = if (status == "open") "color:#1a6e3c;font-weight:600;"
                     else "color:#b00020;font-weight:600;", toupper(status)),
        " · ", tags$strong("Multiplier: "), sprintf("%.1f×", mult)
      ),
      if (status == "open") {
        tagList(
          tags$p(style = "color:#555;font-size:.9em;",
            "Decide how many flex passes to contribute. If the group contributes generously, everyone earns back more — but individual incentives cut the other way."),
          fluidRow(
            column(5,
              numericInput("bp_contrib", "Your contribution (FP):",
                           value = isolate(rv$bp_contrib_val) %||% prev_contrib,
                           min = 0, max = max(0, max_c), step = 0.5)),
            column(4, tags$br(), tags$br(),
                   actionButton("bp_submit", "Submit", class = "btn btn-primary"))
          ),
          tags$p(style = "color:#888;font-size:.82em;",
            sprintf("Balance: %.1f FP%s",
                    bal, if (cap > 0) sprintf("  ·  Cap: %.1f FP/round", cap) else ""))
        )
      } else if (status == "closed") {
        div(class = "alert alert-warning", "Round is closed. Results coming soon.")
      } else if (status == "revealed") {
        payout <- if (nrow(sub) && as.integer(sub$round[1]) == round)
          sub$payout[1] else NA
        div(class = "alert alert-success",
          tags$strong("Round revealed! "),
          if (!is.na(payout)) sprintf("Your payout: %.1f FP.", as.numeric(payout))
          else "Check your Account tab for the credit.")
      } else {
        div(class = "no-game", "Round not open yet.")
      }
    )
  })

  observeEvent(input$bp_submit, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) {
      showNotification("Demo mode — submission not saved.", type = "warning"); return()
    }
    op  <- isolate(olig_poll())
    s   <- op$settings
    if (!nrow(s) || s$round_status[1] != "open") {
      showNotification("Round is not open.", type = "error"); return()
    }
    contrib <- as.numeric(input$bp_contrib %||% 0)
    if (is.na(contrib) || contrib < 0) {
      showNotification("Enter a valid contribution.", type = "error"); return()
    }
    bal <- isolate(wallet_bal())
    if (contrib > bal) {
      showNotification(sprintf("Not enough FP (balance: %.1f).", bal), type = "error"); return()
    }
    cap <- as.numeric(s$contrib_cap[1] %||% 0)
    if (cap > 0 && contrib > cap) {
      showNotification(sprintf("Exceeds round cap of %.1f FP.", cap), type = "error"); return()
    }
    db_exec(
      "INSERT INTO olig_submissions(round, user_id, section, choice, contribute)
       VALUES(?,?,?,?,?)
       ON CONFLICT(round, user_id) DO UPDATE
         SET contribute=excluded.contribute, section=excluded.section;",
      list(as.integer(s$current_round[1]), rv$user_id, rv$section %||% "", "contribute", contrib))
    db_exec("UPDATE olig_settings SET updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification(sprintf("Submitted %.1f FP contribution.", contrib), type = "message")
  })

  # ── Embedded: Prisoner's Dilemma + Price War ──────────────────────────────────
  output$embedded_pd <- renderUI({
    req(rv$authed)
    op     <- olig_poll()
    s      <- op$settings
    if (!nrow(s)) return(div(class = "no-game", "Game not configured."))

    active <- isolate(arcade_poll())$active_game[1] %||% ""
    is_pw  <- identical(active, "price_war")
    status <- s$round_status[1] %||% "pending"
    round  <- as.integer(s$current_round[1] %||% 1L)
    scale  <- as.numeric(s$pd_scale[1] %||% 0.1)
    pts    <- as.numeric(s$pd_payoff_points[1] %||% 10)
    sub    <- op$my_sub
    prev   <- if (nrow(sub) && as.integer(sub$round[1]) == round)
      as.character(sub$choice[1] %||% "") else ""

    c_lbl <- if (is_pw) "Low Price (compete)" else "Defect"
    d_lbl <- if (is_pw) "High Price (collude)" else "Cooperate"

    tagList(
      tags$p(
        tags$strong("Round "), round, " · ",
        tags$strong("Status: "),
        span(style = if (status == "open") "color:#1a6e3c;font-weight:600;"
                     else "color:#b00020;font-weight:600;", toupper(status))
      ),
      if (status == "open") {
        tagList(
          tags$p(style = "color:#555;font-size:.9em;",
            if (is_pw)
              "Choose your pricing strategy. Both firms choosing High earns more collectively — but one firm can always do better by undercutting."
            else
              "Choose to cooperate or defect. Both cooperating pays more overall — but defecting pays more for you individually."),
          radioButtons("pd_choice", "Your choice:",
                       choices  = c(d_lbl, c_lbl),
                       selected = {
                         sv <- isolate(rv$pd_choice_val)
                         if (!is.null(sv) && nzchar(sv)) sv
                         else if (nzchar(prev)) prev
                         else character(0)
                       },
                       inline = TRUE),
          actionButton("pd_submit", "Submit", class = "btn btn-primary"),
          tags$p(style = "color:#888;font-size:.82em;margin-top:.5rem;",
                 sprintf("Payoffs scale: %.1f × %.1f pts = %.1f FP per unit.", pts, scale, pts * scale))
        )
      } else if (status == "revealed") {
        payout <- if (nrow(sub) && as.integer(sub$round[1]) == round)
          sub$payout[1] else NA
        div(class = "alert alert-success",
          tags$strong("Round revealed! "),
          if (!is.na(payout)) sprintf("Your payout: %.1f FP.", as.numeric(payout))
          else "Check your Account tab for the credit.")
      } else {
        div(class = "alert alert-warning", "Round closed. Results coming soon.")
      }
    )
  })

  observeEvent(input$pd_submit, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) {
      showNotification("Demo mode — submission not saved.", type = "warning"); return()
    }
    op <- isolate(olig_poll())
    s  <- op$settings
    if (!nrow(s) || s$round_status[1] != "open") {
      showNotification("Round is not open.", type = "error"); return()
    }
    ch <- input$pd_choice
    if (is.null(ch) || !nzchar(ch %||% "")) {
      showNotification("Make a choice first.", type = "error"); return()
    }
    db_exec(
      "INSERT INTO olig_submissions(round, user_id, section, choice)
       VALUES(?,?,?,?)
       ON CONFLICT(round, user_id) DO UPDATE
         SET choice=excluded.choice, section=excluded.section;",
      list(as.integer(s$current_round[1]), rv$user_id, rv$section %||% "", ch))
    db_exec("UPDATE olig_settings SET updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Choice submitted.", type = "message")
  })

  # ── Tools tab ─────────────────────────────────────────────────────────────────
  output$tools_tab <- renderUI({
    req(rv$authed)

    # Split into simulations vs graded semester tools
    sims   <- Filter(function(t) !isTRUE(t$graded), TOOLS)
    graded <- Filter(function(t) isTRUE(t$graded),  TOOLS)

    tagList(
      div(class = "tab-howto",
          "Always available — explore these to review and apply concepts from class. None require an active session."
      ),
      div(class = "sec-label", "Simulations"),
      div(class = "tools-grid",
        lapply(sims, function(t) {
          div(class = "tool-card",
            div(class = "tool-card-label", t$label),
            div(class = "tool-card-desc",  t$desc),
            div(class = "tool-card-foot",
              tags$a(href = t$url, target = "_blank",
                     class = "btn btn-sm btn-outline-secondary", "Open →"))
          )
        })
      ),
      if (length(graded)) {
        tagList(
          div(class = "sec-label", "Semester Tools"),
          div(class = "tools-grid",
            lapply(graded, function(t) {
              div(class = "tool-card",
                div(class = "tool-card-label", t$label,
                    span(class = "badge-graded", style = "margin-left:.4rem;", "Graded")),
                div(class = "tool-card-desc",  t$desc),
                div(class = "tool-card-foot",
                  tags$a(href = t$url, target = "_blank",
                         class = "btn btn-sm btn-primary", "Open →"))
              )
            })
          )
        )
      }
    )
  })

  # ── Account tab (merged Wallet + Profile) ─────────────────────────────────────
  output$account_tab <- renderUI({
    req(rv$authed)
    wp  <- wallet_poll()
    bal <- wallet_bal()
    gs  <- wp$gs
    step <- as.numeric(wp$step %||% 0.5)

    pending_pledge <- if (nrow(gs)) {
      pid <- db_query(
        "SELECT pledge FROM pledges WHERE user_id=? AND exam_id=? AND round=?;",
        list(rv$user_id,
             gs$active_exam[1] %||% "exam1",
             as.integer(gs$ex_round[1] %||% 1L)))$pledge[1]
      as.numeric(pid %||% 0)
    } else 0
    round_open <- nrow(gs) && isTRUE(as.integer(gs$round_open[1]) == 1L)

    # Job history: try new table first, fall back to job_log
    new_jobs <- tryCatch(db_query(
      "SELECT jp.job_name AS job, ja.created_at AS logged_date, ja.assigned_wage AS wage
       FROM job_assignments ja
       JOIN job_posts jp ON jp.id=ja.job_post_id
       WHERE ja.user_id=?
       ORDER BY ja.created_at DESC LIMIT 8;",
      list(rv$user_id)), error = function(e) data.frame())
    old_jobs <- if (!nrow(new_jobs))
      db_query("SELECT job, logged_date FROM job_log WHERE display_name=?
                ORDER BY created_at DESC LIMIT 5;", list(rv$name))
    else data.frame()
    job_rows <- if (nrow(new_jobs)) new_jobs else old_jobs

    tagList(
      div(class = "tab-howto",
          "Your financial and identity summary. Flex Passes are used for games and pledges; Participation Tokens track engagement and determine grade thresholds."
      ),

      # Three balance tiles
      div(class = "bal-tiles",
        div(class = "bal-tile bal-tile-fp",
          div(class = "bal-tile-label", "Flex Passes"),
          div(class = "bal-tile-val",   sprintf("%.1f", bal)),
          div(class = "bal-tile-sub",   "on hand")
        ),
        div(class = "bal-tile bal-tile-toke",
          div(class = "bal-tile-label", "Tokens Earned"),
          div(class = "bal-tile-val",   as.integer(wp$tokens_earned %||% 0)),
          div(class = "bal-tile-sub",   "gross · all time")
        ),
        div(class = "bal-tile bal-tile-toke2",
          div(class = "bal-tile-label", "Tokens On Hand"),
          div(class = "bal-tile-val",   as.integer(wp$tokens_on_hand %||% 0)),
          div(class = "bal-tile-sub",   "after spending")
        )
      ),

      fluidRow(
        # ── Left column: FP pledge + ledger ──
        column(6,
          wellPanel(
            tags$strong("Flex Pass Pledge"),
            if (!round_open) {
              tags$p(style = "color:#888;margin-bottom:0;",
                     if (!nrow(gs)) "Pledging not set up yet."
                     else "Pledging is not open right now.")
            } else {
              tagList(
                if (pending_pledge > 0)
                  div(class = "pending-pledge",
                      sprintf("You have a pending pledge of %.1f FP this round.", pending_pledge)),
                tags$p(style = "color:#555;font-size:.88em;",
                  "Pledge flex passes toward unlocking the next exam question."),
                fluidRow(
                  column(7,
                    numericInput("pledge_amt", "Amount (FP):",
                                 value = isolate(input$pledge_amt) %||% step,
                                 min = step, max = max(step, floor(bal * 2) / 2), step = step)),
                  column(5, tags$br(), tags$br(),
                         actionButton("pledge_btn", "Pledge", class = "btn btn-primary"))
                )
              )
            }
          ),
          div(class = "sec-label", "FP Transaction History"),
          if (nrow(wp$ledger)) {
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Date"), tags$th("Purpose"),
                tags$th(style = "text-align:right;", "Amount")
              )),
              tags$tbody(lapply(seq_len(nrow(wp$ledger)), function(i) {
                r   <- wp$ledger[i, ]
                cls <- if (r$amount >= 0) "cr" else "dr"
                tags$tr(class = cls,
                  tags$td(format(as.POSIXct(r$created_at), "%b %d")),
                  tags$td(paste0(r$purpose %||% "",
                                 if (nzchar(r$meta %||% ""))
                                   paste0(" — ", r$meta) else "")),
                  tags$td(style = "text-align:right;font-weight:600;",
                          sprintf("%+.1f", r$amount))
                )
              }))
            )
          } else {
            div(style = "color:#999;font-size:.9rem;", "No transactions yet.")
          }
        ),

        # ── Right column: profile + job history ──
        column(6,
          div(class = "profile-panel",
            tags$h6(style = "color:#951829;font-weight:700;", "Display Name"),
            textInput("profile_name", NULL, value = rv$name, width = "100%"),
            actionButton("save_name_btn", "Save", class = "btn btn-primary"),
            tags$p(style = "color:#888;font-size:.82em;margin-top:.5rem;",
                   "The name your instructor and classmates see."),
            tags$hr(style = "margin:.75rem 0;"),
            tags$p(tags$strong("Username: "), rv$user_id),
            if (nzchar(rv$section %||% ""))
              tags$p(tags$strong("Section: "), rv$section),
            tags$hr(style = "margin:.75rem 0;"),
            tags$h6(style = "color:#951829;font-weight:700;", "Job History"),
            if (nrow(job_rows)) {
              tags$table(class = "table table-sm",
                tags$tbody(lapply(seq_len(nrow(job_rows)), function(i) {
                  r <- job_rows[i, ]
                  tags$tr(
                    tags$td(r$job %||% ""),
                    tags$td(style = "color:#888;font-size:.83em;",
                            as.character(r$logged_date %||% "")),
                    if (!is.null(r$wage) && !is.na(r$wage %||% NA))
                      tags$td(style = "text-align:right;color:#1a6e3c;font-size:.85em;",
                              sprintf("%.1f FP", as.numeric(r$wage)))
                    else
                      tags$td("")
                  )
                }))
              )
            } else {
              tags$p(style = "color:#999;font-size:.9em;", "No job history yet.")
            }
          )
        )
      )
    )
  })

  observeEvent(input$pledge_btn, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) {
      showNotification("Demo mode — pledge not saved.", type = "warning"); return()
    }
    wp  <- isolate(wallet_poll())
    gs  <- wp$gs
    if (!nrow(gs) || !isTRUE(as.integer(gs$round_open[1]) == 1L)) {
      showNotification("Pledging is not open.", type = "error"); return()
    }
    amt <- as.numeric(input$pledge_amt %||% 0)
    if (is.na(amt) || amt <= 0) {
      showNotification("Enter a pledge amount.", type = "error"); return()
    }
    bal <- isolate(wallet_bal())
    if (amt > bal) {
      showNotification(sprintf("Insufficient balance (%.1f FP).", bal), type = "error"); return()
    }
    exam_id <- gs$active_exam[1] %||% "exam1"
    round   <- as.integer(gs$ex_round[1] %||% 1L)
    db_exec(
      "INSERT INTO pledges(user_id, exam_id, round, pledge)
       VALUES(?,?,?,?)
       ON CONFLICT(user_id, exam_id, round) DO UPDATE
         SET pledge=excluded.pledge, submitted_at=CURRENT_TIMESTAMP;",
      list(rv$user_id, exam_id, round, amt))
    showNotification(sprintf("Pledge of %.1f FP submitted.", amt), type = "message")
  })

  observeEvent(input$save_name_btn, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) {
      showNotification("Demo mode — name not saved.", type = "warning"); return()
    }
    nm <- trimws(input$profile_name %||% "")
    if (!nzchar(nm)) {
      showNotification("Name cannot be blank.", type = "error"); return()
    }
    db_exec("UPDATE users SET display_name=? WHERE user_id=?;", list(nm, rv$user_id))
    rv$name <- nm
    showNotification("Display name updated.", type = "message")
  })

  # ── Admin tab ─────────────────────────────────────────────────────────────────
  output$admin_tab_panel <- renderUI({
    if (!rv$is_admin) return(NULL)
    tabPanel("Admin", br(), uiOutput("admin_content"))
  })

  output$admin_content <- renderUI({
    req(rv$is_admin)
    active <- isolate(arcade_poll())$active_game[1] %||% ""
    s      <- isolate(olig_poll())$settings

    make_group <- function(type_id, heading) {
      gs <- Filter(function(g) g$type == type_id, GAMES)
      if (!length(gs)) return(NULL)
      setNames(sapply(gs, `[[`, "id"),
               paste0(sapply(gs, `[[`, "label"), " [", heading, "]"))
    }
    all_game_choices <- c(
      list("(none)" = ""),
      make_group("either",  "either/or"),
      make_group("session", "session")
    )

    tagList(
      fluidRow(
        column(5,
          wellPanel(
            tags$h6(style = "font-weight:700;color:#951829;", "Active Game Slot"),
            selectInput("admin_game_sel", "Which game is active now?",
                        choices = all_game_choices, selected = active, width = "100%"),
            actionButton("set_active_btn", "Set active game", class = "btn btn-warning"),
            tags$p(style = "font-size:.82em;color:#888;margin-top:.5rem;margin-bottom:0;",
                   "Students see this immediately on their Games tab.")
          )
        ),
        column(7,
          wellPanel(
            tags$h6(style = "font-weight:700;color:#951829;", "Coordination Game Controls"),
            uiOutput("olig_status_display"),
            if (nrow(s)) {
              tagList(
                fluidRow(
                  column(4, actionButton("adm_open",   "Open",   class = "btn btn-success btn-sm btn-block")),
                  column(4, actionButton("adm_close",  "Close",  class = "btn btn-warning btn-sm btn-block")),
                  column(4, actionButton("adm_reveal", "Reveal", class = "btn btn-danger  btn-sm btn-block"))
                ),
                tags$p(style = "font-size:.8em;color:#999;margin-top:.5rem;margin-bottom:0;",
                       "For full setup use the Coordination Games app.")
              )
            }
          )
        )
      ),
      div(style = "font-size:.82em;color:#888;margin-bottom:.75rem;",
          "Job Market admin is in the ",
          tags$a(href = "/class-job-market/", target = "_blank", "Class Job Market app"), "."),
      div(class = "sec-label", "Student Wallet Balances"),
      uiOutput("admin_balances")
    )
  })

  output$olig_status_display <- renderUI({
    req(rv$is_admin)
    s <- olig_poll()$settings
    if (!nrow(s))
      return(tags$p(style = "color:#999;margin-bottom:.5rem;",
                    "Run coordination-games once to initialize settings."))
    tags$p(style = "margin-bottom:.5rem;",
      tags$strong("Game: "), toupper(s$current_game[1] %||% "—"), "   ",
      tags$strong("Round: "), s$current_round[1], "   ",
      tags$strong("Status: "),
      span(style = if (s$round_status[1] == "open") "color:#1a6e3c;font-weight:600;"
                   else "color:#b00020;font-weight:600;",
           toupper(s$round_status[1]))
    )
  })

  observe({
    req(rv$is_admin)
    active <- arcade_poll()$active_game[1] %||% ""
    updateSelectInput(session, "admin_game_sel", selected = active)
  })

  output$admin_balances <- renderUI({
    req(rv$is_admin)
    rows <- db_query("
      SELECT u.display_name, u.user_id, u.section,
             COALESCE(SUM(l.amount), 0) balance
      FROM users u
      LEFT JOIN ledger l ON l.user_id=u.user_id
      WHERE (u.is_admin IS NULL OR u.is_admin=0)
        AND COALESCE(u.active,1)=1
        AND COALESCE(u.is_demo,0)=0
      GROUP BY u.user_id
      ORDER BY u.section, u.display_name;")
    if (!nrow(rows)) return(div(style = "color:#999;", "No students found."))
    tags$table(class = "table table-sm table-hover",
      tags$thead(tags$tr(
        tags$th("Name"), tags$th("Username"), tags$th("Section"),
        tags$th(style = "text-align:right;", "Balance (FP)")
      )),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        tags$tr(
          tags$td(r$display_name %||% r$user_id),
          tags$td(style = "color:#888;font-size:.85em;", r$user_id),
          tags$td(r$section %||% ""),
          tags$td(style = "text-align:right;font-weight:600;", sprintf("%.1f", r$balance))
        )
      }))
    )
  })

  observeEvent(input$set_active_btn, {
    req(rv$is_admin)
    g <- input$admin_game_sel %||% ""
    if (nzchar(g)) {
      db_exec("UPDATE arcade_state SET active_game=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;",
              list(g))
    } else {
      db_exec("UPDATE arcade_state SET active_game=NULL, updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    }
    showNotification(paste("Active game:", if (nzchar(g)) g else "cleared"), type = "message")
  })

  observeEvent(input$adm_open, {
    req(rv$is_admin)
    db_exec("UPDATE olig_settings SET round_status='open', updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Round opened.", type = "message")
  })
  observeEvent(input$adm_close, {
    req(rv$is_admin)
    db_exec("UPDATE olig_settings SET round_status='closed', updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Round closed. Run payouts in the Coordination Games app.",
                     type = "warning", duration = 6)
  })
  observeEvent(input$adm_reveal, {
    req(rv$is_admin)
    db_exec("UPDATE olig_settings SET round_status='revealed', updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Status set to revealed. Run payouts in the Coordination Games app first.",
                     type = "warning", duration = 6)
  })

}

shinyApp(ui, server)
