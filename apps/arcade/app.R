try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
library(shiny)
library(DBI)
library(RSQLite)
library(bcrypt)
library(jsonlite)

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
  db_exec("INSERT INTO arcade_state(id, active_game, assignments_revealed) VALUES(1, NULL, 0);")
try(db_exec("ALTER TABLE arcade_state ADD COLUMN assignments_revealed INTEGER DEFAULT 0;"), silent = TRUE)

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

# Spending infrastructure (shared with class-job-market)
db_exec("CREATE TABLE IF NOT EXISTS problem_sets(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  original_deadline TEXT,
  solutions_posted_at TEXT,
  active INTEGER DEFAULT 1
);")
db_exec("CREATE TABLE IF NOT EXISTS extension_purchases(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  problem_set_id INTEGER,
  user_id TEXT,
  hours REAL,
  cost REAL,
  ledger_id INTEGER,
  purchased_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
db_exec("CREATE TABLE IF NOT EXISTS grade_reweight_requests(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id TEXT,
  from_category TEXT,
  to_category TEXT,
  points INTEGER,
  cost REAL,
  ledger_id INTEGER,
  status TEXT DEFAULT 'pending',
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
db_exec("CREATE TABLE IF NOT EXISTS public_goods(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  description TEXT,
  threshold REAL DEFAULT 0,
  active INTEGER DEFAULT 1
);")
db_exec("CREATE TABLE IF NOT EXISTS public_good_contributions(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  public_good_id INTEGER,
  user_id TEXT,
  amount REAL,
  ledger_id INTEGER,
  contributed_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
db_exec("CREATE TABLE IF NOT EXISTS labor_settings(
  key TEXT PRIMARY KEY,
  value TEXT
);")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('extension_prices_json','{\"24\":3,\"48\":5}');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('reweight_cost_schedule','1:2,2:5,3:9,4:14,5:20');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('grade_reweight_categories','Homework,Midterm,Final');")

# token_ledger table
db_exec("CREATE TABLE IF NOT EXISTS token_ledger(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id TEXT NOT NULL,
  display_name TEXT,
  source_type TEXT,
  source_id INTEGER,
  amount REAL NOT NULL,
  earning INTEGER DEFAULT 1,
  note TEXT,
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")

# Seed fake data for demo account (only if token_ledger is empty for demo)
if (!db_query("SELECT COUNT(*) n FROM token_ledger WHERE user_id='demo';")$n[1]) {
  for (row in list(
    list("demo","Demo User", 8L, 1L,"job",        NA_integer_,"Record Keeper — Wk 1","2024-09-05 10:00:00"),
    list("demo","Demo User", 5L, 1L,"bonus_pot",  NA_integer_,"Round 1 payout",       "2024-09-12 10:00:00"),
    list("demo","Demo User", 6L, 1L,"pd_payout",  NA_integer_,"Prisoner's Dilemma R1","2024-09-19 10:00:00"),
    list("demo","Demo User",12L, 1L,"job",        NA_integer_,"Analyst — Wk 2",       "2024-09-26 10:00:00"),
    list("demo","Demo User", 7L, 1L,"job",        NA_integer_,"Analyst — Wk 3",       "2024-10-03 10:00:00"),
    list("demo","Demo User",-3L, 0L,"extension",  NA_integer_,"48h extension",         "2024-10-04 09:00:00"),
    list("demo","Demo User",-2L, 0L,"public_good",NA_integer_,"Public good #1",        "2024-10-10 09:00:00"),
    list("demo","Demo User", 9L, 1L,"bonus_pot",  NA_integer_,"Round 2 payout",       "2024-10-17 10:00:00")
  )) {
    db_exec("INSERT INTO token_ledger(user_id,display_name,amount,earning,source_type,source_id,note,created_at)
             VALUES(?,?,?,?,?,?,?,?);", row)
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
get_setting <- function(key, default = NULL) {
  r <- db_query("SELECT value FROM labor_settings WHERE key=?;", list(key))
  if (!nrow(r) || is.na(r$value[1])) return(default)
  r$value[1]
}
parse_ext_prices <- function() {
  raw <- tryCatch(get_setting("extension_prices_json", '{"24":3,"48":5}'), error = function(e) '{"24":3,"48":5}')
  tryCatch({
    lst <- jsonlite::fromJSON(raw)
    v <- as.numeric(unlist(lst))
    names(v) <- names(lst)
    v
  }, error = function(e) c("24" = 3, "48" = 5))
}
parse_rw_costs <- function() {
  raw <- tryCatch(get_setting("reweight_cost_schedule", "1:2,2:5,3:9,4:14,5:20"),
                  error = function(e) "1:2,2:5,3:9,4:14,5:20")
  pairs <- strsplit(trimws(raw), ",")[[1]]
  v <- numeric(0)
  for (p in pairs) {
    parts <- strsplit(trimws(p), ":")[[1]]
    if (length(parts) == 2) {
      k <- trimws(parts[1]); val <- as.numeric(trimws(parts[2]))
      if (!is.na(val)) v[k] <- val
    }
  }
  v
}
APP_NAME <- get_config("app_name", "Classroom Economy")

# ── Game catalog ──────────────────────────────────────────────────────────────
# type "either"  — live session slot OR elective use between classes
# type "session" — live during class only
# Semester tools (price-index, flex-pass-app) have moved to the TOOLS list.
GAMES <- list(
  list(id = "bonus_pot",        type = "either",
       label = "Bonus Pot",     embedded = TRUE,
       desc = "Contribute tokens to a shared pot. The group earns back more when participation is high — but individual incentives push the other way."),
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

# ── Demos catalog ─────────────────────────────────────────────────────────────
DEMOS <- list(
  list(id = "indiff-to-demand",  label = "Indifference to Demand",  url = "/indiff-to-demand/",
       desc = "Trace how budget constraints and indifference curves generate a demand curve. Adjust prices and income interactively."),
  list(id = "theory-of-firm",    label = "Theory of the Firm",      url = "/theory-of-firm/",
       desc = "Explore cost curves, profit maximization, and shutdown decisions for a price-taking firm."),
  list(id = "tax-incidence",     label = "Tax Incidence",           url = "/tax-incidence/",
       desc = "See how the burden of an excise tax divides between buyers and sellers depending on supply and demand elasticity."),
  list(id = "price-index",       label = "Price Index",             url = "/price-index/",
       desc = "Build a basket of goods and track prices across waves to measure your personal inflation rate.")
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

/* ── Demos tab ──────────────────────────────────────────────────────────── */
.demos-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
              gap: .65rem; margin-bottom: 1rem; }
.demo-card { background: #fff; border: 1px solid #e8e8e8; border-radius: 10px;
             padding: 1rem 1.1rem; display: flex; flex-direction: column; gap: .3rem; }
.demo-card-label { font-weight: 600; font-size: .95rem; }
.demo-card-desc  { color: #666; font-size: .82rem; flex: 1; }
.demo-card-foot  { display: flex; align-items: center; justify-content: space-between;
                   margin-top: .5rem; }

/* ── Spend tab ──────────────────────────────────────────────────────────── */
.spend-cards { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
               gap: .65rem; margin-bottom: 1rem; }
.spend-card { background: #fff; border: 1px solid #e8e8e8; border-radius: 10px;
              padding: 1rem 1.1rem; display: flex; flex-direction: column; gap: .3rem;
              cursor: default; }
.spend-card-icon  { font-size: 1.5rem; margin-bottom: .2rem; }
.spend-card-label { font-weight: 700; font-size: .95rem; }
.spend-card-desc  { color: #666; font-size: .82rem; flex: 1; }
.spend-card-meta  { color: #888; font-size: .78rem; }
.spend-card-foot  { margin-top: .5rem; }
.spend-form-box { background: #fff; border: 1px solid #e0e0e0; border-radius: 10px;
                  padding: 1.1rem 1.2rem; margin-bottom: .5rem; }
.pg-bar-wrap { background: #f0f0f0; border-radius: 999px; height: 8px;
               margin: .35rem 0 .55rem; overflow: hidden; }
.pg-bar-fill { background: #1a6e3c; height: 100%; border-radius: 999px; }

/* ── Live Tracker ───────────────────────────────────────────────────────── */
.tracker-wrap { overflow-x: auto; }

/* ── Account tab ────────────────────────────────────────────────────────── */
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
    game_detail_id = NULL,
    bp_contrib_val = NULL,
    pd_choice_val  = NULL,
    spend_mode     = NULL    # NULL | "extension" | "reweight" | "pubgood"
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
            tabPanel("Demos",      br(), uiOutput("demos_tab")),
            tabPanel("Spend",      br(), uiOutput("spend_tab")),
            tabPanel("Account",    br(), uiOutput("account_tab")),
            uiOutput("tracker_tab_panel"),
            uiOutput("settings_tab_panel")
          )
        )
      )
    }
  })

  output$header_widgets <- renderUI({
    req(rv$authed)
    bal <- token_bal()
    tagList(
      span(class = "arc-name", rv$name),
      span(class = "arc-bal",  sprintf("%d tokens", as.integer(bal)))
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

  token_poll <- reactivePoll(6000, session,
    checkFunc = function() {
      if (is.null(rv$user_id)) return("")
      tryCatch(
        db_query("SELECT MAX(created_at) ts FROM token_ledger WHERE user_id=?;",
                 list(rv$user_id))$ts[1] %||% "",
        error = function(e) "")
    },
    valueFunc = function() {
      if (is.null(rv$user_id)) return(list(
        ledger = data.frame(), tokens_earned = 0, tokens_on_hand = 0))
      ledger <- tryCatch(db_query(
        "SELECT amount, source_type, note, created_at, earning
         FROM token_ledger WHERE user_id=? ORDER BY created_at DESC LIMIT 60;",
        list(rv$user_id)), error = function(e) data.frame())
      earned  <- tryCatch(as.numeric(db_query(
        "SELECT COALESCE(SUM(amount),0) t FROM token_ledger
         WHERE user_id=? AND earning=1 AND amount>0;",
        list(rv$user_id))$t[1] %||% 0), error = function(e) 0)
      on_hand <- tryCatch(as.numeric(db_query(
        "SELECT COALESCE(SUM(amount),0) t FROM token_ledger WHERE user_id=?;",
        list(rv$user_id))$t[1] %||% 0), error = function(e) 0)
      list(ledger = ledger, tokens_earned = earned, tokens_on_hand = on_hand)
    }
  )

  pubgood_poll <- reactivePoll(10000, session,
    checkFunc = function() {
      tryCatch(
        db_query("SELECT MAX(contributed_at) ts FROM public_good_contributions;")$ts[1] %||% "",
        error = function(e) "")
    },
    valueFunc = function() {
      goods  <- tryCatch(db_query(
        "SELECT * FROM public_goods WHERE COALESCE(active,1)=1 ORDER BY id;"),
        error = function(e) data.frame())
      totals <- tryCatch(db_query(
        "SELECT public_good_id, SUM(amount) AS total
         FROM public_good_contributions GROUP BY public_good_id;"),
        error = function(e) data.frame())
      list(goods = goods, totals = totals)
    }
  )

  tracker_poll <- reactivePoll(5000, session,
    checkFunc = function() {
      if (!isTRUE(rv$is_admin)) return("")
      t1 <- tryCatch(db_query("SELECT MAX(created_at) ts FROM token_ledger;")$ts[1] %||% "", error=function(e)"")
      t2 <- tryCatch(db_query("SELECT MAX(created_at) ts FROM job_assignments;")$ts[1] %||% "", error=function(e)"")
      t3 <- tryCatch(db_query("SELECT assignments_revealed FROM arcade_state WHERE id=1;")$assignments_revealed[1] %||% "0", error=function(e)"")
      paste(t1, t2, t3)
    },
    valueFunc = function() {
      if (!isTRUE(rv$is_admin)) return(list(
        students=data.frame(), subs=data.frame(), assignments=data.frame(),
        round=data.frame(), revealed=FALSE))
      students <- tryCatch(db_query(
        "SELECT u.user_id, u.display_name, u.section,
                COALESCE(SUM(CASE WHEN tl.earning=1 AND tl.amount>0 THEN tl.amount ELSE 0 END),0) AS tokens_earned,
                COALESCE(SUM(tl.amount),0) AS tokens_on_hand
         FROM users u
         LEFT JOIN token_ledger tl ON tl.user_id=u.user_id
         WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1 AND COALESCE(u.is_demo,0)=0
         GROUP BY u.user_id ORDER BY u.section, u.display_name;"),
        error = function(e) data.frame())
      active <- tryCatch(
        db_query("SELECT active_game FROM arcade_state WHERE id=1;")$active_game[1] %||% "",
        error = function(e) "")
      revealed <- tryCatch(
        isTRUE(as.integer(db_query("SELECT COALESCE(assignments_revealed,0) v FROM arcade_state WHERE id=1;")$v[1]) == 1L),
        error = function(e) FALSE)
      subs <- if (nzchar(active %||% "")) {
        tryCatch(db_query(
          "SELECT DISTINCT user_id FROM olig_submissions WHERE round=(
             SELECT current_round FROM olig_settings WHERE id=1);"),
          error = function(e) data.frame())
      } else data.frame()
      round <- tryCatch(db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
      rid <- if (nrow(round)) round$id[1] else NA_integer_
      assignments <- if (!is.na(rid)) {
        tryCatch(db_query(
          "SELECT ja.user_id, u.display_name, u.section, jp.job_name, ja.assigned_wage
           FROM job_assignments ja
           JOIN users u ON u.user_id=ja.user_id
           JOIN job_posts jp ON jp.id=ja.job_post_id
           WHERE ja.round_id=?
           ORDER BY u.section, u.display_name;", list(rid)),
          error = function(e) data.frame())
      } else data.frame()
      list(students=students, subs=subs, assignments=assignments,
           round=round, revealed=revealed)
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

  token_bal <- reactive({ as.numeric(token_poll()$tokens_on_hand %||% 0) })

  my_pub_contrib_data <- reactive({
    req(rv$user_id)
    tryCatch(db_query(
      "SELECT public_good_id, SUM(amount) AS my_total
       FROM public_good_contributions WHERE user_id=? GROUP BY public_good_id;",
      list(rv$user_id)), error = function(e) data.frame())
  })

  token_credit <- function(uid, dname, amount, earning, source_type, source_id = NA, note = "") {
    db_exec(
      "INSERT INTO token_ledger(user_id,display_name,source_type,source_id,amount,earning,note)
       VALUES(?,?,?,?,?,?,?);",
      list(uid, dname, source_type,
           if (is.na(source_id)) NA_integer_ else as.integer(source_id),
           amount, as.integer(earning), note))
    tryCatch(db_query("SELECT last_insert_rowid() AS id;")$id[1], error = function(e) NA_integer_)
  }
  token_debit <- function(uid, dname, amount, source_type, source_id = NA, note = "") {
    token_credit(uid, dname, -abs(amount), 0L, source_type, source_id, note)
  }

  # Preserve typed input values across poll-triggered re-renders.
  observe({ if (!is.null(input$bp_contrib)) rv$bp_contrib_val <- input$bp_contrib })
  observe({ if (!is.null(input$pd_choice))  rv$pd_choice_val  <- input$pd_choice  })

  # ── Today tab ─────────────────────────────────────────────────────────────────
  output$today_tab <- renderUI({
    req(rv$authed)
    arc    <- arcade_poll()
    active <- arc$active_game[1] %||% ""
    revealed <- isTRUE(as.integer(arc$assignments_revealed[1] %||% 0L) == 1L)
    jp     <- jobs_poll()
    mode   <- if (nrow(jp$round)) jp$round$assignment_mode[1] %||% "random" else "random"
    wage_mode <- identical(mode, "wage_bidding")

    tagList(
      div(class = "tab-howto",
        "Your daily snapshot: active class game, your job assignment, and job pools."
      ),

      # Active game
      if (nzchar(active)) {
        ginfo <- game_info(active)
        div(class = "today-active-slot",
          div(class = "slot-header", "▶ Active Game", span(class = "badge-live", "LIVE")),
          div(style = "font-weight:700;font-size:1rem;margin-bottom:.2rem;",
              if (!is.null(ginfo)) ginfo$label else active),
          div(style = "color:#888;font-size:.84rem;", "A game is running now."),
          div(style = "margin-top:.65rem;",
            actionButton("go_to_games", "Go to Games tab →", class = "btn btn-sm btn-primary"))
        )
      },

      # Public Good status
      {
        pg <- pubgood_poll()
        if (nrow(pg$goods)) {
          tagList(
            div(class = "sec-label", "Public Good"),
            lapply(seq_len(nrow(pg$goods)), function(i) {
              good    <- pg$goods[i, ]
              t_row   <- if (nrow(pg$totals)) pg$totals[pg$totals$public_good_id == good$id, , drop = FALSE] else data.frame()
              contrib <- if (nrow(t_row)) as.numeric(t_row$total[1] %||% 0) else 0
              thresh  <- as.numeric(good$threshold %||% 0)
              pct     <- if (thresh > 0) min(100, round(contrib / thresh * 100)) else 0
              div(class = "today-card",
                tags$strong(good$name),
                if (nzchar(good$description %||% ""))
                  tags$p(style = "color:#555;font-size:.86rem;margin:.2rem 0 .4rem;", good$description),
                div(style = "font-size:.82rem;color:#888;margin-bottom:.3rem;",
                    sprintf("Class total: %d / %d tokens (%d%%)",
                            as.integer(contrib), as.integer(thresh), pct)),
                div(class = "pg-bar-wrap",
                    div(class = "pg-bar-fill", style = sprintf("width:%d%%;", pct)))
              )
            })
          )
        }
      },

      # My Job Today — only visible once instructor reveals
      div(class = "sec-label", "My Job Today"),
      if (!revealed) {
        div(class = "today-card",
            style = "color:#888;font-style:italic;",
            "Assignments will be revealed by your instructor at the start of class.")
      } else if (nrow(jp$my_assign)) {
        r <- jp$my_assign[1, ]
        div(class = "job-tile",
          div(class = "job-tile-name", "\U0001f4cb ", r$job_name %||% "—"),
          div(class = "job-tile-meta",
              r$round_label %||% "Current round",
              if (wage_mode && !is.na(r$assigned_wage %||% NA))
                paste0("  ·  Wage: ", sprintf("%d tokens", as.integer(r$assigned_wage)))
              else "")
        )
      } else {
        div(style = "color:#999;font-size:.9rem;padding:.4rem 0;",
            "No assignment for this round yet.")
      },

      # Job Pools — always visible; wages shown only in wage-bidding mode
      div(class = "sec-label", "Job Pools"),
      if (nrow(jp$posts)) {
        div(class = "pool-grid",
          lapply(seq_len(nrow(jp$posts)), function(i) {
            r     <- jp$posts[i, ]
            fill  <- as.integer(r$filled %||% 0)
            slots <- as.integer(r$slots %||% 0)
            full  <- fill >= slots && slots > 0
            div(class = paste("pool-card", if (full) "pool-card-full"),
              div(class = "pool-card-name", r$job_name %||% r$category_name %||% ""),
              div(class = "pool-card-fill",
                  if (slots > 0) sprintf("%d / %d filled%s", fill, slots, if (full) " ✓" else "")
                  else if (fill > 0) sprintf("%d assigned", fill)
                  else "Open"),
              if (wage_mode && !is.na(r$wage %||% NA))
                div(style = "font-size:.72rem;color:#1a6e3c;margin-top:.15rem;",
                    sprintf("Wage: %d tokens", as.integer(r$wage)))
            )
          })
        )
      } else {
        div(style = "color:#999;font-size:.9rem;", "No jobs configured for the current round.")
      },

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
                paste0("Wage: ", sprintf("%d tokens", as.integer(r$assigned_wage)))
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
                    sprintf("%d tokens", as.integer(r$assigned_wage))
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
    bal    <- token_bal()
    sub    <- op$my_sub

    prev_contrib <- if (nrow(sub) && as.integer(sub$round[1]) == round)
      as.numeric(sub$contribute[1] %||% 0) else 0
    max_c <- if (cap > 0) min(cap, floor(bal)) else floor(bal)

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
            "Decide how many tokens to contribute. If the group contributes generously, everyone earns back more — but individual incentives cut the other way."),
          fluidRow(
            column(5,
              numericInput("bp_contrib", "Your contribution (tokens):",
                           value = isolate(rv$bp_contrib_val) %||% prev_contrib,
                           min = 0, max = max(0, max_c), step = 1)),
            column(4, tags$br(), tags$br(),
                   actionButton("bp_submit", "Submit", class = "btn btn-primary"))
          ),
          tags$p(style = "color:#888;font-size:.82em;",
            sprintf("Balance: %d tokens%s",
                    as.integer(bal), if (cap > 0) sprintf("  ·  Cap: %d tokens/round", as.integer(cap)) else ""))
        )
      } else if (status == "closed") {
        div(class = "alert alert-warning", "Round is closed. Results coming soon.")
      } else if (status == "revealed") {
        payout <- if (nrow(sub) && as.integer(sub$round[1]) == round)
          sub$payout[1] else NA
        div(class = "alert alert-success",
          tags$strong("Round revealed! "),
          if (!is.na(payout)) sprintf("Your payout: %d tokens.", as.integer(payout))
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
    bal <- isolate(token_bal())
    if (contrib > bal) {
      showNotification(sprintf("Not enough tokens (balance: %d).", as.integer(bal)), type = "error"); return()
    }
    cap <- as.numeric(s$contrib_cap[1] %||% 0)
    if (cap > 0 && contrib > cap) {
      showNotification(sprintf("Exceeds round cap of %d tokens.", as.integer(cap)), type = "error"); return()
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
                 sprintf("Payoffs scale: %.1f × %.1f pts = %.1f tokens per unit.", pts, scale, pts * scale))
        )
      } else if (status == "revealed") {
        payout <- if (nrow(sub) && as.integer(sub$round[1]) == round)
          sub$payout[1] else NA
        div(class = "alert alert-success",
          tags$strong("Round revealed! "),
          if (!is.na(payout)) sprintf("Your payout: %d tokens.", as.integer(payout))
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

  # ── Demos tab ─────────────────────────────────────────────────────────────────
  output$demos_tab <- renderUI({
    req(rv$authed)
    tagList(
      div(class = "tab-howto",
          "Interactive demonstrations — explore these to review and apply concepts from class. Always available; none require an active session."
      ),
      div(class = "demos-grid",
        lapply(DEMOS, function(d) {
          div(class = "demo-card",
            div(class = "demo-card-label", d$label),
            div(class = "demo-card-desc",  d$desc),
            div(class = "demo-card-foot",
              tags$a(href = d$url, target = "_blank",
                     class = "btn btn-sm btn-outline-secondary", "Open →"))
          )
        })
      )
    )
  })

  # ── Spend tab ─────────────────────────────────────────────────────────────────
  output$spend_tab <- renderUI({
    req(rv$authed)
    bal <- token_bal()

    if (is.null(rv$spend_mode)) {
      # Card picker view
      pg <- pubgood_poll()
      pg_active <- nrow(pg$goods) > 0
      pg_status <- if (pg_active) {
        good   <- pg$goods[1, ]
        t_row  <- if (nrow(pg$totals)) pg$totals[pg$totals$public_good_id == good$id, , drop = FALSE] else data.frame()
        contrib <- if (nrow(t_row)) as.numeric(t_row$total[1] %||% 0) else 0
        thresh  <- as.numeric(good$threshold %||% 0)
        if (thresh > 0) sprintf("Active: %d / %d tokens", as.integer(contrib), as.integer(thresh))
        else "Active goal"
      } else "No active goal"

      tagList(
        div(class = "tab-howto",
            sprintf("Spend your tokens on academic benefits. Spendable balance: %d tokens.", as.integer(bal))),
        div(class = "spend-cards",
          div(class = "spend-card",
            div(class = "spend-card-icon", "\U0001f4c5"),
            div(class = "spend-card-label", "Problem Set Extension"),
            div(class = "spend-card-desc", "Purchase extra time on a problem set before the deadline."),
            div(class = "spend-card-meta", "Cost varies by length"),
            div(class = "spend-card-foot",
                actionButton("open_extension", "Select →", class = "btn btn-sm btn-outline-primary"))
          ),
          div(class = "spend-card",
            div(class = "spend-card-icon", "⚖️"),
            div(class = "spend-card-label", "Grade Reweight"),
            div(class = "spend-card-desc", "Shift grade weight from one category to another."),
            div(class = "spend-card-meta", "Instructor reviews all requests"),
            div(class = "spend-card-foot",
                actionButton("open_reweight", "Select →", class = "btn btn-sm btn-outline-primary"))
          ),
          div(class = "spend-card",
            div(class = "spend-card-icon", "\U0001f91d"),
            div(class = "spend-card-label", "Public Good"),
            div(class = "spend-card-desc", "Contribute toward a class-wide public goal."),
            div(class = "spend-card-meta", pg_status),
            div(class = "spend-card-foot",
                actionButton("open_pubgood", "Select →", class = "btn btn-sm btn-outline-primary"))
          )
        ),
        div(class = "sec-label", "Spending History"),
        uiOutput("spend_history")
      )
    } else {
      tagList(
        actionButton("spend_back", "← Back to options",
                     class = "btn btn-sm btn-link",
                     style = "padding:0;margin-bottom:.75rem;"),
        uiOutput("spend_form"),
        div(class = "sec-label", "Spending History"),
        uiOutput("spend_history")
      )
    }
  })

  observeEvent(input$open_extension, { rv$spend_mode <- "extension" })
  observeEvent(input$open_reweight,  { rv$spend_mode <- "reweight"  })
  observeEvent(input$open_pubgood,   { rv$spend_mode <- "pubgood"   })
  observeEvent(input$spend_back,     { rv$spend_mode <- NULL        })

  output$spend_form <- renderUI({
    req(rv$authed)
    bal <- token_bal()
    mode <- rv$spend_mode %||% ""

    if (mode == "extension") {
      ps_rows <- tryCatch(db_query(
        "SELECT * FROM problem_sets WHERE COALESCE(active,1)=1 ORDER BY original_deadline DESC LIMIT 20;"),
        error = function(e) data.frame())
      prices <- parse_ext_prices()
      if (!nrow(ps_rows) || !length(prices))
        return(div(class = "spend-form-box",
                   "No problem sets are configured yet. Ask your instructor to set them up."))
      tagList(
        div(class = "spend-form-box",
          tags$h6(style = "color:#951829;font-weight:700;", "\U0001f4c5 Problem Set Extension"),
          selectInput("ext_ps", "Problem set:",
                      setNames(ps_rows$id, ps_rows$name)),
          selectInput("ext_hours", "Extension length:",
                      setNames(names(prices),
                               paste0(names(prices), " hours — ", as.integer(prices), " tokens"))),
          uiOutput("ext_cost_preview"),
          actionButton("submit_extension", "Purchase extension", class = "btn btn-warning")
        )
      )

    } else if (mode == "reweight") {
      cats_str <- tryCatch(get_setting("grade_reweight_categories", "Homework,Midterm,Final"),
                           error = function(e) "Homework,Midterm,Final")
      cats  <- trimws(strsplit(cats_str, ",")[[1]])
      costs <- parse_rw_costs()
      max_pts <- if (length(costs)) as.integer(max(as.integer(names(costs)), na.rm = TRUE)) else 5L
      div(class = "spend-form-box",
        tags$h6(style = "color:#951829;font-weight:700;", "⚖️ Grade Reweight"),
        fluidRow(
          column(5, selectInput("rw_from", "Move weight from:", choices = cats)),
          column(5, selectInput("rw_to",   "Move weight to:",   choices = cats))
        ),
        sliderInput("rw_points", "Percentage points to move:",
                    min = 1, max = max_pts, value = 1, step = 1),
        uiOutput("rw_cost_preview"),
        actionButton("submit_reweight", "Submit request", class = "btn btn-warning"),
        tags$p(style = "font-size:.8rem;color:#888;margin-top:.4rem;",
               "Your instructor will review and apply approved requests.")
      )

    } else if (mode == "pubgood") {
      pg <- pubgood_poll()
      if (!nrow(pg$goods))
        return(div(class = "spend-form-box",
                   "No active public good has been configured. Check back when your instructor announces one."))
      mc <- my_pub_contrib_data()
      lapply(seq_len(nrow(pg$goods)), function(i) {
        good    <- pg$goods[i, ]
        t_row   <- if (nrow(pg$totals)) pg$totals[pg$totals$public_good_id == good$id, , drop = FALSE] else data.frame()
        my_row  <- if (nrow(mc)) mc[mc$public_good_id == good$id, , drop = FALSE] else data.frame()
        contrib <- if (nrow(t_row)) as.numeric(t_row$total[1] %||% 0) else 0
        my_tot  <- if (nrow(my_row)) as.numeric(my_row$my_total[1] %||% 0) else 0
        thresh  <- as.numeric(good$threshold %||% 0)
        pct     <- if (thresh > 0) min(100, round(contrib / thresh * 100)) else 0
        div(class = "spend-form-box", style = if (i > 1) "margin-top:.6rem;" else "",
          tags$h6(style = "color:#951829;font-weight:700;", good$name),
          if (nzchar(good$description %||% ""))
            tags$p(style = "color:#555;font-size:.88rem;", good$description),
          div(style = "font-size:.82rem;color:#888;margin-bottom:.2rem;",
              sprintf("Class total: %d / %d tokens (%d%%)",
                      as.integer(contrib), as.integer(thresh), pct)),
          div(class = "pg-bar-wrap",
              div(class = "pg-bar-fill", style = sprintf("width:%d%%;", pct))),
          if (my_tot > 0)
            tags$p(style = "font-size:.82rem;color:#666;margin-bottom:.4rem;",
                   sprintf("Your contributions: %d tokens", as.integer(my_tot)))
        )
      })
    }
  })

  output$ext_cost_preview <- renderUI({
    req(rv$authed)
    prices <- parse_ext_prices()
    hrs    <- as.character(input$ext_hours %||% names(prices)[1])
    cost   <- as.numeric(prices[hrs] %||% 0)
    bal    <- token_bal()
    div(style = "font-size:.86rem;color:#555;margin:.4rem 0 .6rem;",
        sprintf("Cost: %d tokens  ·  Balance: %d  ·  After: %d",
                as.integer(cost), as.integer(bal), as.integer(bal - cost)))
  })

  output$rw_cost_preview <- renderUI({
    req(rv$authed)
    costs <- parse_rw_costs()
    pts   <- as.integer(input$rw_points %||% 1)
    cost  <- as.numeric(costs[as.character(pts)] %||% 0)
    bal   <- token_bal()
    div(style = "font-size:.86rem;color:#555;margin:.4rem 0 .6rem;",
        sprintf("Cost: %d tokens  ·  Balance: %d  ·  After: %d",
                as.integer(cost), as.integer(bal), as.integer(bal - cost)))
  })

  output$spend_history <- renderUI({
    req(rv$authed)
    rows <- tryCatch(db_query(
      "SELECT amount, source_type, note, created_at FROM token_ledger
       WHERE user_id=? AND earning=0 ORDER BY created_at DESC LIMIT 20;",
      list(rv$user_id)), error = function(e) data.frame())
    if (!nrow(rows))
      return(div(style = "color:#999;font-size:.88rem;", "No spending history yet."))
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(
        tags$th("Date"), tags$th("Type"), tags$th("Note"),
        tags$th(style = "text-align:right;", "Tokens")
      )),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        tags$tr(
          tags$td(tryCatch(format(as.POSIXct(r$created_at), "%b %d"), error = function(e) "")),
          tags$td(r$source_type %||% ""),
          tags$td(r$note %||% ""),
          tags$td(style = "text-align:right;font-weight:600;color:#b00020;",
                  as.integer(r$amount))
        )
      }))
    )
  })

  observeEvent(input$submit_extension, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) { showNotification("Demo mode.", type = "warning"); return() }
    prices <- parse_ext_prices()
    hrs    <- as.character(input$ext_hours %||% names(prices)[1])
    cost   <- as.numeric(prices[hrs] %||% 0)
    bal    <- isolate(token_bal())
    if (cost <= 0) { showNotification("Invalid extension option.", type = "error"); return() }
    if (bal < cost) {
      showNotification(sprintf("Not enough tokens (need %d, have %d).", as.integer(cost), as.integer(bal)),
                       type = "error"); return()
    }
    ps_id <- as.integer(input$ext_ps %||% 0)
    if (ps_id <= 0) { showNotification("Select a problem set.", type = "error"); return() }
    lid <- token_debit(rv$user_id, rv$name, cost, "extension", ps_id,
                       note = sprintf("%sh extension", hrs))
    db_exec(
      "INSERT INTO extension_purchases(problem_set_id,user_id,hours,cost,ledger_id) VALUES(?,?,?,?,?);",
      list(ps_id, rv$user_id, as.numeric(hrs), cost, as.integer(lid %||% NA_integer_)))
    showNotification(sprintf("Extension purchased: %s hours for %d tokens.", hrs, as.integer(cost)),
                     type = "message")
    rv$spend_mode <- NULL
  })

  observeEvent(input$submit_reweight, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) { showNotification("Demo mode.", type = "warning"); return() }
    if (identical(input$rw_from, input$rw_to)) {
      showNotification("From and to categories must differ.", type = "error"); return()
    }
    pts  <- as.integer(input$rw_points %||% 1)
    costs <- parse_rw_costs()
    cost <- as.numeric(costs[as.character(pts)] %||% 0)
    bal  <- isolate(token_bal())
    if (cost <= 0) { showNotification("Cost not configured for that point value.", type = "error"); return() }
    if (bal < cost) {
      showNotification(sprintf("Not enough tokens (need %d, have %d).", as.integer(cost), as.integer(bal)),
                       type = "error"); return()
    }
    lid <- token_debit(rv$user_id, rv$name, cost, "grade_reweight", NA,
                       note = sprintf("%s → %s, %d pt", input$rw_from, input$rw_to, pts))
    db_exec(
      "INSERT INTO grade_reweight_requests(user_id,from_category,to_category,points,cost,ledger_id)
       VALUES(?,?,?,?,?,?);",
      list(rv$user_id, input$rw_from, input$rw_to, pts, cost, as.integer(lid %||% NA_integer_)))
    showNotification(
      sprintf("Request submitted (%d tokens spent). Your instructor will review it.", as.integer(cost)),
      type = "message")
    rv$spend_mode <- NULL
  })

  observeEvent(input$submit_pubgood, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) { showNotification("Demo mode.", type = "warning"); return() }
    pg <- isolate(pubgood_poll())
    if (!nrow(pg$goods)) { showNotification("No active public good.", type = "error"); return() }
    good_id <- if (!is.null(input$pg_good_sel)) as.integer(input$pg_good_sel) else pg$goods$id[1]
    amt <- as.integer(input$pg_contrib %||% 0)
    if (is.na(amt) || amt <= 0) { showNotification("Enter a positive amount.", type = "error"); return() }
    bal <- isolate(token_bal())
    if (bal < amt) {
      showNotification(sprintf("Not enough tokens (have %d).", as.integer(bal)), type = "error"); return()
    }
    lid <- token_debit(rv$user_id, rv$name, amt, "public_good", good_id,
                       note = sprintf("Public good #%d", good_id))
    db_exec(
      "INSERT INTO public_good_contributions(public_good_id,user_id,amount,ledger_id) VALUES(?,?,?,?);",
      list(good_id, rv$user_id, amt, as.integer(lid %||% NA_integer_)))
    showNotification(sprintf("Contributed %d tokens to public good.", amt), type = "message")
    rv$spend_mode <- NULL
  })

  # ── Account tab ───────────────────────────────────────────────────────────────
  output$account_tab <- renderUI({
    req(rv$authed)
    tp  <- token_poll()
    bal <- token_bal()

    job_rows <- tryCatch(db_query(
      "SELECT jp.job_name AS job, ja.created_at AS logged_date, ja.assigned_wage AS wage
       FROM job_assignments ja
       JOIN job_posts jp ON jp.id=ja.job_post_id
       WHERE ja.user_id=?
       ORDER BY ja.created_at DESC LIMIT 8;",
      list(rv$user_id)), error = function(e) data.frame())

    tagList(
      div(class = "tab-howto", "Your token summary, transaction history, and profile."),

      div(class = "bal-tiles",
        div(class = "bal-tile bal-tile-toke",
          div(class = "bal-tile-label", "Tokens Earned"),
          div(class = "bal-tile-val",   as.integer(tp$tokens_earned %||% 0)),
          div(class = "bal-tile-sub",   "gross · all time")
        ),
        div(class = "bal-tile bal-tile-toke2",
          div(class = "bal-tile-label", "Tokens On Hand"),
          div(class = "bal-tile-val",   as.integer(bal)),
          div(class = "bal-tile-sub",   "after spending")
        )
      ),

      fluidRow(
        column(6,
          div(class = "sec-label", "Token History"),
          if (nrow(tp$ledger)) {
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Date"), tags$th("Type"), tags$th("Note"),
                tags$th(style = "text-align:right;", "Tokens")
              )),
              tags$tbody(lapply(seq_len(nrow(tp$ledger)), function(i) {
                r   <- tp$ledger[i, ]
                cls <- if (as.numeric(r$amount) >= 0) "cr" else "dr"
                tags$tr(class = cls,
                  tags$td(tryCatch(format(as.POSIXct(r$created_at), "%b %d"), error = function(e) "")),
                  tags$td(r$source_type %||% ""),
                  tags$td(r$note %||% ""),
                  tags$td(style = "text-align:right;font-weight:600;",
                          sprintf("%+d", as.integer(r$amount)))
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
                              sprintf("%d tokens", as.integer(r$wage)))
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

  # ── Live Tracker tab (admin) ──────────────────────────────────────────────────
  output$tracker_tab_panel <- renderUI({
    if (!rv$is_admin) return(NULL)
    tabPanel("Live Tracker", br(), uiOutput("live_tracker_tab"))
  })

  output$live_tracker_tab <- renderUI({
    req(rv$authed, rv$is_admin)
    td       <- tracker_poll()
    active   <- tryCatch(arcade_poll()$active_game[1] %||% "", error = function(e) "")
    revealed <- td$revealed
    s        <- tryCatch(olig_poll()$settings, error = function(e) data.frame())
    olig_str <- if (nrow(s))
      sprintf("%s · R%d · %s", toupper(s$current_game[1] %||% "—"),
              as.integer(s$current_round[1]), toupper(s$round_status[1] %||% "—"))
    else "—"
    round <- td$round
    mode  <- if (nrow(round)) round$assignment_mode[1] %||% "random" else "random"
    wage_mode <- identical(mode, "wage_bidding")
    n_assigned <- nrow(td$assignments)

    tagList(
      div(class = "tab-howto", "Real-time student overview. Updates every 5 seconds."),

      # Status row
      fluidRow(
        column(3, wellPanel(style = "padding:.6rem 1rem;",
          tags$small(class = "text-muted", "Active game"),
          tags$p(style = "margin:0;font-weight:700;",
                 if (nzchar(active)) { gi <- game_info(active); if (!is.null(gi)) gi$label else active } else "None")
        )),
        column(3, wellPanel(style = "padding:.6rem 1rem;",
          tags$small(class = "text-muted", "Coord. game"),
          tags$p(style = "margin:0;font-weight:700;", olig_str)
        )),
        column(3, wellPanel(style = "padding:.6rem 1rem;",
          tags$small(class = "text-muted", "Students"),
          tags$p(style = "margin:0;font-weight:700;", nrow(td$students))
        )),
        column(3, wellPanel(style = "padding:.6rem 1rem;",
          tags$small(class = "text-muted", "Assignments"),
          tags$p(style = "margin:0;font-weight:700;",
                 if (n_assigned > 0) sprintf("%d drawn", n_assigned) else "None drawn")
        ))
      ),

      # Job Draw panel
      wellPanel(
        tags$h6(style = "font-weight:700;color:#951829;margin-bottom:.5rem;", "Job Draw"),
        if (!nrow(round)) {
          tags$p(style = "color:#999;margin:0;", "No active round — set one up in the Class Job Market app.")
        } else {
          mode_label <- switch(mode,
            random              = "Random draw",
            application_bidding = "Weighted lottery (by ticket bids)",
            wage_bidding        = "Lowest-bid draw",
            paste("Mode:", mode))
          tagList(
            tags$p(style = "color:#555;font-size:.88rem;margin-bottom:.6rem;",
                   sprintf("Round: %s  ·  %s", round$label[1] %||% paste("Round", round$id[1]), mode_label)),
            fluidRow(
              column(4,
                actionButton("run_draw_btn", "\U0001f3b2 Draw Jobs",
                             class = "btn btn-primary btn-sm",
                             title = "Randomly assign students to jobs and replace any existing assignments")),
              column(5,
                if (revealed)
                  actionButton("toggle_reveal_btn", "Hide from Students",
                               class = "btn btn-outline-secondary btn-sm")
                else
                  actionButton("toggle_reveal_btn", "Reveal to Students",
                               class = "btn btn-success btn-sm")
              )
            ),
            if (n_assigned > 0) {
              tags$p(style = "font-size:.8rem;color:#888;margin-top:.4rem;margin-bottom:0;",
                     sprintf("%d students assigned · %s",
                             n_assigned,
                             if (revealed) "Visible to students" else "Hidden from students"))
            }
          )
        }
      ),

      # Assignments table (shown once a draw has been run)
      if (n_assigned > 0) {
        tagList(
          div(class = "sec-label", "Current Assignments"),
          div(class = "tracker-wrap",
            tags$table(class = "table table-sm table-hover",
              tags$thead(tags$tr(
                tags$th("Student"), tags$th("Section"), tags$th("Job"),
                if (wage_mode) tags$th(style="text-align:right;", "Wage")
              )),
              tags$tbody(lapply(seq_len(nrow(td$assignments)), function(i) {
                r <- td$assignments[i, ]
                tags$tr(
                  tags$td(r$display_name %||% r$user_id),
                  tags$td(style = "color:#888;", r$section %||% ""),
                  tags$td(style = "font-weight:600;", r$job_name %||% ""),
                  if (wage_mode)
                    tags$td(style = "text-align:right;",
                            if (!is.na(r$assigned_wage %||% NA))
                              sprintf("%d", as.integer(r$assigned_wage))
                            else "—")
                )
              }))
            )
          )
        )
      },

      # Token summary
      div(class = "sec-label", "Student Token Summary"),
      div(class = "tracker-wrap",
        if (!nrow(td$students)) {
          div(style = "color:#999;", "No students found.")
        } else {
          has_subs <- nzchar(active) && nrow(td$subs) > 0
          tags$table(class = "table table-sm table-hover",
            tags$thead(tags$tr(
              tags$th("Student"), tags$th("Section"),
              tags$th(style = "text-align:right;", "Earned"),
              tags$th(style = "text-align:right;", "On Hand"),
              if (has_subs) tags$th(style = "text-align:center;", "Submitted?")
            )),
            tags$tbody(lapply(seq_len(nrow(td$students)), function(i) {
              r  <- td$students[i, ]
              submitted <- has_subs && (r$user_id %in% td$subs$user_id)
              tags$tr(
                tags$td(r$display_name %||% r$user_id),
                tags$td(style = "color:#888;", r$section %||% ""),
                tags$td(style = "text-align:right;", as.integer(r$tokens_earned %||% 0)),
                tags$td(style = "text-align:right;font-weight:600;", as.integer(r$tokens_on_hand %||% 0)),
                if (has_subs)
                  tags$td(style = "text-align:center;",
                          if (submitted) span(style = "color:#1a6e3c;font-weight:700;", "✓")
                          else span(style = "color:#ccc;", "—"))
              )
            }))
          )
        }
      )
    )
  })

  # ── Settings tab (admin) ──────────────────────────────────────────────────────
  output$settings_tab_panel <- renderUI({
    if (!rv$is_admin) return(NULL)
    tabPanel("Settings", br(), uiOutput("settings_tab"))
  })

  output$settings_tab <- renderUI({
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
        column(5, wellPanel(
          tags$h6(style = "font-weight:700;color:#951829;", "Active Game Slot"),
          selectInput("admin_game_sel", "Which game is active now?",
                      choices = all_game_choices, selected = active, width = "100%"),
          actionButton("set_active_btn", "Set active game", class = "btn btn-warning"),
          tags$p(style = "font-size:.82em;color:#888;margin-top:.5rem;margin-bottom:0;",
                 "Students see this immediately on their Games tab.")
        )),
        column(7, wellPanel(
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
                     "For full payout setup use the Coordination Games app.")
            )
          }
        ))
      ),
      wellPanel(
        tags$h6(style = "font-weight:700;color:#951829;", "Configure"),
        selectInput("config_action", NULL, width = "100%", choices = c(
          "— select an action —"        = "",
          "Export Assignments (CSV)"    = "export_assignments",
          "Export Wage Bids (CSV)"      = "export_wage_bids",
          "Export Token Ledger (CSV)"   = "export_tokens",
          "View Token Ledger"           = "view_tokens",
          "View Student Balances"       = "view_balances",
          "Extension Setup"             = "setup_extensions",
          "Public Good Setup"           = "setup_pubgoods",
          "App Settings"                = "app_settings"
        )),
        uiOutput("config_panel")
      )
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

  output$config_panel <- renderUI({
    req(rv$is_admin)
    act <- input$config_action %||% ""
    if (!nzchar(act)) return(NULL)

    if (act == "export_assignments")
      return(downloadButton("dl_assignments", "Download Assignments CSV", class = "btn btn-sm btn-outline-secondary"))
    if (act == "export_wage_bids")
      return(downloadButton("dl_wage_bids", "Download Wage Bids CSV", class = "btn btn-sm btn-outline-secondary"))
    if (act == "export_tokens")
      return(downloadButton("dl_tokens", "Download Token Ledger CSV", class = "btn btn-sm btn-outline-secondary"))

    if (act == "view_tokens") {
      rows <- tryCatch(db_query(
        "SELECT tl.user_id, u.display_name, tl.amount, tl.earning, tl.source_type, tl.note, tl.created_at
         FROM token_ledger tl LEFT JOIN users u ON u.user_id=tl.user_id
         ORDER BY tl.created_at DESC LIMIT 100;"), error = function(e) data.frame())
      if (!nrow(rows)) return(div(style = "color:#999;margin-top:.5rem;", "No entries yet."))
      div(style = "overflow-x:auto;margin-top:.5rem;",
        tags$table(class = "table table-sm table-hover",
          tags$thead(tags$tr(lapply(names(rows), tags$th))),
          tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
            r <- rows[i, ]; tags$tr(lapply(r, function(v) tags$td(as.character(v %||% ""))))
          }))
        )
      )
    } else if (act == "view_balances") {
      rows <- tryCatch(db_query(
        "SELECT u.user_id, u.display_name, u.section,
                COALESCE(SUM(CASE WHEN tl.earning=1 AND tl.amount>0 THEN tl.amount ELSE 0 END),0) AS tokens_earned,
                COALESCE(SUM(tl.amount),0) AS tokens_on_hand
         FROM users u LEFT JOIN token_ledger tl ON tl.user_id=u.user_id
         WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1 AND COALESCE(u.is_demo,0)=0
         GROUP BY u.user_id ORDER BY u.section, u.display_name;"),
        error = function(e) data.frame())
      if (!nrow(rows)) return(div(style = "color:#999;margin-top:.5rem;", "No students found."))
      div(style = "overflow-x:auto;margin-top:.5rem;",
        tags$table(class = "table table-sm table-hover",
          tags$thead(tags$tr(tags$th("User"), tags$th("Name"), tags$th("Section"),
                             tags$th(style="text-align:right;","Earned"),
                             tags$th(style="text-align:right;","On Hand"))),
          tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
            r <- rows[i, ]
            tags$tr(
              tags$td(r$user_id), tags$td(r$display_name %||% ""), tags$td(r$section %||% ""),
              tags$td(style="text-align:right;", as.integer(r$tokens_earned %||% 0)),
              tags$td(style="text-align:right;font-weight:600;", as.integer(r$tokens_on_hand %||% 0))
            )
          }))
        )
      )
    } else if (act == "setup_extensions") {
      ps <- tryCatch(db_query("SELECT * FROM problem_sets ORDER BY original_deadline DESC LIMIT 20;"),
                     error = function(e) data.frame())
      tagList(
        div(style = "margin-top:.5rem;",
          if (nrow(ps)) {
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(tags$th("Name"), tags$th("Deadline"), tags$th("Active"))),
              tags$tbody(lapply(seq_len(nrow(ps)), function(i) {
                r <- ps[i, ]
                tags$tr(tags$td(r$name), tags$td(r$original_deadline %||% ""),
                        tags$td(if (isTRUE(as.integer(r$active %||% 1L)==1L)) "✓" else ""))
              }))
            )
          } else div(style="color:#999;", "No problem sets yet.")
        ),
        tags$h6(style="margin-top:.75rem;", "Add Problem Set"),
        fluidRow(
          column(5, textInput("new_ps_name", "Name:")),
          column(4, dateInput("new_ps_deadline", "Original deadline:")),
          column(3, tags$br(), actionButton("add_ps_btn", "Add", class = "btn btn-sm btn-primary"))
        )
      )
    } else if (act == "setup_pubgoods") {
      pgs <- tryCatch(db_query("SELECT * FROM public_goods ORDER BY id DESC LIMIT 20;"),
                      error = function(e) data.frame())
      tagList(
        div(style = "margin-top:.5rem;",
          if (nrow(pgs)) {
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(tags$th("Name"), tags$th("Threshold"), tags$th("Active"))),
              tags$tbody(lapply(seq_len(nrow(pgs)), function(i) {
                r <- pgs[i, ]
                tags$tr(tags$td(r$name), tags$td(r$threshold %||% ""),
                        tags$td(if (isTRUE(as.integer(r$active %||% 1L)==1L)) "✓" else ""))
              }))
            )
          } else div(style="color:#999;", "No public goods yet.")
        ),
        tags$h6(style="margin-top:.75rem;", "Add Public Good"),
        textInput("new_pg_name", "Name:"),
        textAreaInput("new_pg_desc", "Description:", rows = 2),
        numericInput("new_pg_threshold", "Token threshold:", value = 100, min = 1),
        actionButton("add_pg_btn", "Add", class = "btn btn-sm btn-primary")
      )
    } else if (act == "app_settings") {
      tagList(
        div(style = "margin-top:.5rem;"),
        textInput("new_app_name", "App name:", value = get_config("app_name", "Classroom Economy"),
                  width = "100%"),
        actionButton("save_app_name_btn", "Save", class = "btn btn-sm btn-primary")
      )
    }
  })

  # Download handlers (must be in server, not renderUI)
  output$dl_assignments <- downloadHandler(
    filename = function() paste0("assignments_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT wr.label round, u.display_name student, jp.job_name job,
              ja.assigned_wage wage, ja.assignment_mode, ja.created_at
       FROM job_assignments ja
       JOIN users u ON u.user_id=ja.user_id
       JOIN job_posts jp ON jp.id=ja.job_post_id
       JOIN weekly_rounds wr ON wr.id=ja.round_id
       ORDER BY wr.id DESC, u.display_name;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )
  output$dl_wage_bids <- downloadHandler(
    filename = function() paste0("wage_bids_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT wr.label round, u.display_name student, jc.name category,
              wb.min_wage, wb.submitted_at
       FROM wage_bids wb
       JOIN users u ON u.user_id=wb.user_id
       JOIN job_categories jc ON jc.id=wb.category_id
       JOIN weekly_rounds wr ON wr.id=wb.round_id
       ORDER BY wr.id DESC, u.display_name;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )
  output$dl_tokens <- downloadHandler(
    filename = function() paste0("token_ledger_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT tl.user_id, u.display_name, tl.amount, tl.earning,
              tl.source_type, tl.note, tl.created_at
       FROM token_ledger tl LEFT JOIN users u ON u.user_id=tl.user_id
       ORDER BY tl.created_at DESC;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )

  # Admin action observers
  observeEvent(input$set_active_btn, {
    req(rv$is_admin)
    g <- input$admin_game_sel %||% ""
    if (nzchar(g))
      db_exec("UPDATE arcade_state SET active_game=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;", list(g))
    else
      db_exec("UPDATE arcade_state SET active_game=NULL, updated_at=CURRENT_TIMESTAMP WHERE id=1;")
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
    showNotification("Round closed. Run payouts in the Coordination Games app.", type = "warning", duration = 6)
  })
  observeEvent(input$adm_reveal, {
    req(rv$is_admin)
    db_exec("UPDATE olig_settings SET round_status='revealed', updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Status set to revealed.", type = "warning", duration = 6)
  })
  observeEvent(input$add_ps_btn, {
    req(rv$is_admin)
    nm <- trimws(input$new_ps_name %||% "")
    if (!nzchar(nm)) { showNotification("Enter a name.", type = "error"); return() }
    db_exec("INSERT INTO problem_sets(name, original_deadline) VALUES(?,?);",
            list(nm, as.character(input$new_ps_deadline %||% "")))
    showNotification("Problem set added.", type = "message")
  })
  observeEvent(input$add_pg_btn, {
    req(rv$is_admin)
    nm <- trimws(input$new_pg_name %||% "")
    if (!nzchar(nm)) { showNotification("Enter a name.", type = "error"); return() }
    db_exec("INSERT INTO public_goods(name, description, threshold) VALUES(?,?,?);",
            list(nm, input$new_pg_desc %||% "", as.numeric(input$new_pg_threshold %||% 0)))
    showNotification("Public good added.", type = "message")
  })
  observeEvent(input$save_app_name_btn, {
    req(rv$is_admin)
    nm <- trimws(input$new_app_name %||% "")
    if (!nzchar(nm)) { showNotification("Name cannot be blank.", type = "error"); return() }
    db_exec("UPDATE arcade_config SET value=? WHERE key='app_name';", list(nm))
    showNotification("App name updated (takes effect on next restart).", type = "message")
  })

  # ── Job draw ──────────────────────────────────────────────────────────────────
  observeEvent(input$run_draw_btn, {
    req(rv$is_admin)
    round <- tryCatch(db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                      error = function(e) data.frame())
    if (!nrow(round)) { showNotification("No active round.", type = "error"); return() }
    rid  <- round$id[1]
    mode <- round$assignment_mode[1] %||% "random"

    posts <- tryCatch(db_query(
      "SELECT jp.id, jp.job_name, jp.slots, jp.category_id,
              COALESCE(jp.wage_override, jc.default_wage) AS wage
       FROM job_posts jp
       LEFT JOIN job_categories jc ON jc.id=jp.category_id
       WHERE jp.round_id=? AND COALESCE(jp.active,1)=1;", list(rid)),
      error = function(e) data.frame())
    if (!nrow(posts)) { showNotification("No active job posts for this round.", type = "error"); return() }

    students <- tryCatch(db_query(
      "SELECT user_id, section FROM users
       WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0
       ORDER BY RANDOM();"),
      error = function(e) data.frame())
    if (!nrow(students)) { showNotification("No eligible students found.", type = "error"); return() }

    # Clear existing assignments for this round
    db_exec("DELETE FROM job_assignments WHERE round_id=?;", list(rid))

    # Build assignment pairs based on mode
    pairs <- tryCatch({
      if (mode == "wage_bidding") {
        # For each category: sort bids ascending, assign cheapest bidders to slots
        bids <- db_query(
          "SELECT user_id, category_id, min_wage FROM wage_bids WHERE round_id=? ORDER BY min_wage ASC;",
          list(rid))
        assigned_ids <- character(0)
        result <- list()
        for (i in seq_len(nrow(posts))) {
          p  <- posts[i, ]
          n  <- max(1L, as.integer(p$slots %||% 1L))
          cat_bids <- if (nrow(bids)) bids[bids$category_id == p$category_id & !bids$user_id %in% assigned_ids, ] else data.frame()
          pool_ids <- if (nrow(cat_bids)) cat_bids$user_id else character(0)
          # Fill remaining from unassigned students not already drawn
          other_ids <- setdiff(students$user_id, c(assigned_ids, pool_ids))
          pool_ids  <- c(pool_ids, sample(other_ids))
          drawn <- head(pool_ids, n)
          wages <- if (nrow(cat_bids)) {
            sapply(drawn, function(u) {
              m <- cat_bids[cat_bids$user_id == u, , drop=FALSE]
              if (nrow(m)) as.numeric(m$min_wage[1]) else as.numeric(p$wage %||% NA)
            })
          } else rep(as.numeric(p$wage %||% NA), length(drawn))
          assigned_ids <- c(assigned_ids, drawn)
          for (j in seq_along(drawn))
            result[[length(result)+1]] <- list(uid=drawn[j], post_id=p$id, wage=wages[j])
        }
        result
      } else if (mode == "application_bidding") {
        # Weighted lottery by ticket allocation per category
        bids <- tryCatch(db_query(
          "SELECT user_id, category_id, tickets FROM application_bids WHERE round_id=? AND tickets>0;",
          list(rid)), error=function(e) data.frame())
        assigned_ids <- character(0)
        result <- list()
        for (i in seq_len(nrow(posts))) {
          p  <- posts[i, ]
          n  <- max(1L, as.integer(p$slots %||% 1L))
          cat_bids <- if (nrow(bids))
            bids[bids$category_id == p$category_id & !bids$user_id %in% assigned_ids, ]
          else data.frame()
          # Expand by ticket count for weighted sampling
          pool <- if (nrow(cat_bids)) rep(cat_bids$user_id, cat_bids$tickets) else character(0)
          # All unassigned students eligible as fallback with weight 1
          others <- setdiff(students$user_id, c(assigned_ids, if(nrow(cat_bids)) cat_bids$user_id else character(0)))
          pool <- c(pool, others)
          pool <- pool[!pool %in% assigned_ids]
          drawn <- if (length(pool) > 0) {
            k <- min(n, length(unique(pool)))
            sample(unique(pool), k, prob=tabulate(match(pool, unique(pool))))
          } else character(0)
          assigned_ids <- c(assigned_ids, drawn)
          for (uid in drawn)
            result[[length(result)+1]] <- list(uid=uid, post_id=p$id, wage=as.numeric(p$wage %||% NA))
        }
        result
      } else {
        # Pure random: shuffle students, assign round-robin to slots
        shuffled <- sample(students$user_id)
        slots_list <- do.call(c, lapply(seq_len(nrow(posts)), function(i) {
          p <- posts[i,]
          rep(list(list(post_id=p$id, wage=as.numeric(p$wage %||% NA))),
              max(1L, as.integer(p$slots %||% 1L)))
        }))
        lapply(seq_len(min(length(shuffled), length(slots_list))), function(i)
          list(uid=shuffled[i], post_id=slots_list[[i]]$post_id, wage=slots_list[[i]]$wage))
      }
    }, error = function(e) { message("draw error: ", e$message); list() })

    if (!length(pairs)) { showNotification("Draw produced no assignments.", type = "error"); return() }

    for (p in pairs) {
      db_exec(
        "INSERT OR IGNORE INTO job_assignments(round_id, user_id, job_post_id, assigned_wage, assignment_mode)
         VALUES(?,?,?,?,?);",
        list(rid, p$uid, p$post_id,
             if (is.na(p$wage %||% NA)) NA_real_ else as.numeric(p$wage),
             mode))
    }
    db_exec("UPDATE arcade_state SET assignments_revealed=0, updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification(sprintf("Drew %d assignments (hidden from students).", length(pairs)), type = "message")
  })

  # ── Reveal toggle ─────────────────────────────────────────────────────────────
  observeEvent(input$toggle_reveal_btn, {
    req(rv$is_admin)
    cur <- tryCatch(
      as.integer(db_query("SELECT COALESCE(assignments_revealed,0) v FROM arcade_state WHERE id=1;")$v[1] %||% 0L),
      error = function(e) 0L)
    new_val <- if (isTRUE(cur == 1L)) 0L else 1L
    db_exec("UPDATE arcade_state SET assignments_revealed=?, updated_at=CURRENT_TIMESTAMP WHERE id=1;",
            list(new_val))
    showNotification(
      if (new_val == 1L) "Assignments are now visible to students."
      else "Assignments hidden from students.",
      type = "message")
  })

}

shinyApp(ui, server)
