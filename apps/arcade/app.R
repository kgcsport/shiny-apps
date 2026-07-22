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
db_exec("CREATE TABLE IF NOT EXISTS extension_options(
  id     INTEGER PRIMARY KEY AUTOINCREMENT,
  label  TEXT NOT NULL,
  hours  REAL NOT NULL,
  tokens REAL NOT NULL,
  active INTEGER DEFAULT 1
);")
db_exec("CREATE TABLE IF NOT EXISTS flex_questions(
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  question_text TEXT NOT NULL,
  order_index  INTEGER DEFAULT 0,
  active       INTEGER DEFAULT 1,
  created_at   TEXT DEFAULT CURRENT_TIMESTAMP
);")
db_exec("CREATE TABLE IF NOT EXISTS flex_purchases(
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id      TEXT NOT NULL,
  question_id  INTEGER NOT NULL,
  tokens_spent REAL DEFAULT 0,
  purchased_at TEXT DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(user_id, question_id)
);")
db_exec("CREATE TABLE IF NOT EXISTS labor_settings(
  key TEXT PRIMARY KEY,
  value TEXT
);")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('extension_prices_json','{\"24\":3,\"48\":5}');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('reweight_cost_schedule','1:2,2:5,3:9,4:14,5:20');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('grade_reweight_categories','Homework,Midterm,Final');")
db_exec(paste0("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('grade_categories_json','",
  '[{"name":"Homework","weight":33},{"name":"Midterm","weight":33},{"name":"Final","weight":34}]',
  "');"))
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('half_wage_multiplier','0.5');")
db_exec(paste0("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('participation_event_types','",
  '[{"id":"question","label":"Useful Question","tokens":1},',
  '{"id":"explain","label":"Explanation","tokens":2},',
  '{"id":"correct","label":"Correct Answer","tokens":1}]', "');"))
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('active_section','');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('flex_cost_schedule','2,4,6,8,10');")

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

# Participation events
db_exec("CREATE TABLE IF NOT EXISTS participation_events(
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id   INTEGER,
  user_id    TEXT,
  event_type TEXT,
  tokens     REAL,
  note       TEXT,
  logged_by  TEXT,
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")

# Job assignment outcome tracking (safe on re-run via try)
try(db_exec("ALTER TABLE job_assignments ADD COLUMN outcome TEXT;"), silent = TRUE)
try(db_exec("ALTER TABLE job_assignments ADD COLUMN tokens_awarded INTEGER DEFAULT 0;"), silent = TRUE)
try(db_exec("ALTER TABLE job_assignments ADD COLUMN updated_at TEXT;"), silent = TRUE)

# Job market tables (shared with class-job-market; CREATE IF NOT EXISTS is safe)
db_exec("CREATE TABLE IF NOT EXISTS job_categories(
  id            INTEGER PRIMARY KEY AUTOINCREMENT,
  name          TEXT NOT NULL,
  default_wage  REAL DEFAULT 10,
  description   TEXT,
  display_order INTEGER DEFAULT 99
);")
try(db_exec("ALTER TABLE job_categories ADD COLUMN voluntary INTEGER DEFAULT 0;"), silent = TRUE)
db_exec("CREATE TABLE IF NOT EXISTS weekly_rounds(
  id                  INTEGER PRIMARY KEY AUTOINCREMENT,
  label               TEXT,
  assignment_mode     TEXT DEFAULT 'random',
  bid_open_date       TEXT,
  bid_close_date      TEXT,
  tickets_per_student INTEGER DEFAULT 10,
  created_at          TEXT DEFAULT CURRENT_TIMESTAMP
);")
db_exec("CREATE TABLE IF NOT EXISTS job_posts(
  id            INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id      INTEGER,
  job_name      TEXT NOT NULL,
  category_id   INTEGER,
  slots         INTEGER DEFAULT 1,
  wage_override REAL,
  active        INTEGER DEFAULT 1,
  display_order INTEGER DEFAULT 99,
  created_at    TEXT DEFAULT CURRENT_TIMESTAMP
);")
try(db_exec("ALTER TABLE job_posts ADD COLUMN voluntary INTEGER DEFAULT 0;"), silent = TRUE)
try(db_exec("ALTER TABLE job_posts ADD COLUMN in_draw INTEGER DEFAULT 1;"), silent = TRUE)
try(db_exec("ALTER TABLE job_categories ADD COLUMN description TEXT;"), silent = TRUE)
db_exec("CREATE TABLE IF NOT EXISTS job_assignments(
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id        INTEGER,
  user_id         TEXT,
  job_post_id     INTEGER,
  assigned_wage   REAL,
  assignment_mode TEXT,
  status          TEXT DEFAULT 'assigned',
  created_at      TEXT DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(round_id, user_id)
);")
db_exec("CREATE TABLE IF NOT EXISTS wage_bids(
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id     INTEGER,
  category_id  INTEGER,
  user_id      TEXT,
  min_wage     REAL,
  submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(round_id, category_id, user_id)
);")
db_exec("CREATE TABLE IF NOT EXISTS application_bids(
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id     INTEGER,
  category_id  INTEGER,
  user_id      TEXT,
  tickets      INTEGER DEFAULT 0,
  submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(round_id, category_id, user_id)
);")
db_exec("CREATE TABLE IF NOT EXISTS job_templates(
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  name           TEXT NOT NULL,
  category_id    INTEGER,
  slots          INTEGER DEFAULT 1,
  suggested_wage REAL,
  active         INTEGER DEFAULT 1,
  created_at     TEXT DEFAULT CURRENT_TIMESTAMP
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
  rows <- tryCatch(
    db_query("SELECT id, label, hours, tokens FROM extension_options WHERE COALESCE(active,1)=1 ORDER BY hours DESC;"),
    error = function(e) data.frame())
  if (!nrow(rows)) return(data.frame(id=integer(0), label=character(0), hours=numeric(0), tokens=numeric(0)))
  rows
}

parse_flex_cost <- function(text = NULL) {
  if (is.null(text)) text <- tryCatch(get_setting("flex_cost_schedule", "2,4,6,8,10"), error=function(e)"2,4,6,8,10")
  text <- trimws(text %||% "")
  if (!nzchar(text)) return(NULL)
  tryCatch({
    parts <- as.numeric(strsplit(text, ",")[[1]])
    if (all(!is.na(parts))) return(parts)
    NULL
  }, error = function(e) NULL)
}
question_cost_for_n <- function(n, schedule_text = NULL) {
  tbl <- parse_flex_cost(schedule_text)
  if (is.null(tbl) || length(tbl) == 0) return(2 * n)
  if (n <= length(tbl)) return(tbl[n])
  last <- tbl[length(tbl)]
  step <- if (length(tbl) >= 2) (tbl[length(tbl)] - tbl[length(tbl)-1]) else tbl[1]
  last + step * (n - length(tbl))
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
parse_event_types <- function() {
  default_json <- '[{"id":"question","label":"Useful Question","tokens":1},{"id":"explain","label":"Explanation","tokens":2},{"id":"correct","label":"Correct Answer","tokens":1}]'
  raw <- tryCatch(get_setting("participation_event_types", default_json), error = function(e) default_json)
  tryCatch({
    df <- jsonlite::fromJSON(raw)
    if (is.data.frame(df) && all(c("id","label","tokens") %in% names(df))) df
    else jsonlite::fromJSON(default_json)
  }, error = function(e)
    data.frame(id=c("question","explain","correct"),
               label=c("Useful Question","Explanation","Correct Answer"),
               tokens=c(1,2,1), stringsAsFactors=FALSE))
}

parse_grade_categories <- function() {
  default_json <- '[{"name":"Homework","weight":33},{"name":"Midterm","weight":33},{"name":"Final","weight":34}]'
  raw <- tryCatch(get_setting("grade_categories_json", default_json), error = function(e) default_json)
  tryCatch({
    df <- jsonlite::fromJSON(raw)
    if (is.data.frame(df) && all(c("name","weight") %in% names(df))) df
    else jsonlite::fromJSON(default_json)
  }, error = function(e)
    data.frame(name=c("Homework","Midterm","Final"),
               weight=c(33,33,34), stringsAsFactors=FALSE))
}

compute_clearing_wage <- function(category_id, round_id, slots) {
  if (is.na(category_id %||% NA) || is.na(round_id %||% NA) || is.na(slots %||% NA))
    return(NA_real_)
  n <- max(1L, as.integer(slots))
  bids <- tryCatch(db_query(
    "SELECT min_wage FROM wage_bids
     WHERE round_id=? AND category_id=?
     ORDER BY min_wage ASC;",
    list(as.integer(round_id), as.integer(category_id))),
    error = function(e) data.frame())
  if (!nrow(bids) || nrow(bids) < n) return(NA_real_)
  as.numeric(bids$min_wage[n])
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

/* ── Projector View ─────────────────────────────────────────────────────── */
.proj-wrap   { background:#111; color:#f0f0f0; min-height:100vh;
               padding:2rem 3rem; font-family:system-ui,-apple-system,sans-serif; }
.proj-round  { font-size:2.5rem; font-weight:800; color:#f5c518;
               margin-bottom:1.5rem; letter-spacing:.02em; }
.proj-sec    { font-size:.9rem; font-weight:700; text-transform:uppercase;
               letter-spacing:.12em; color:#888;
               border-bottom:1px solid #333; padding-bottom:.35rem;
               margin:1.4rem 0 .75rem; }
.proj-tbl    { width:100%; border-collapse:collapse; font-size:1.2rem; }
.proj-tbl th { font-size:.8rem; text-transform:uppercase; color:#666; font-weight:600;
               padding:.35rem .6rem; border-bottom:1px solid #333; text-align:left; }
.proj-tbl td { padding:.45rem .6rem; border-bottom:1px solid #1e1e1e; }
.proj-tbl td.num { font-family:ui-monospace,monospace; color:#6fcf7d;
                   font-weight:700; text-align:right; }
.proj-none   { color:#555; font-style:italic; padding:.65rem 0; font-size:1.1rem; }
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
    spend_mode     = NULL,   # NULL | "extension" | "reweight" | "flex_question"
    impersonating  = FALSE,
    orig_state     = NULL,
    draw_preview   = NULL,   # NULL | list of draw pairs for preview
    active_section = get_setting("active_section", "")
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
        if (rv$impersonating)
          div(class = "demo-banner",
              style = "background:#e8f0fe;border-color:#1a56db;color:#1a3a7c;",
              sprintf("\U0001f465 Viewing as %s (student view).", rv$name), " ",
              tags$button(
                type = "button",
                onclick = "Shiny.setInputValue('stop_impersonate_btn',+new Date(),{priority:'event'});",
                style = "background:none;border:1px solid #1a56db;color:#1a3a7c;border-radius:4px;padding:.1rem .5rem;font-size:.82rem;cursor:pointer;margin-left:.4rem;",
                "Stop Impersonating"
              )),
        if (rv$is_demo && !rv$impersonating)
          div(class = "demo-banner",
              "\U0001f50d Demo mode — you're exploring with a fake account. Nothing you do here is saved."),
        div(class = "arc-body",
          tabsetPanel(id = "arc_tabs", type = "tabs", selected = "Today",
            tabPanel("Today",        br(), uiOutput("today_tab")),
            tabPanel("Job Market",   br(), uiOutput("job_market_tab")),
            tabPanel("Games",        br(), uiOutput("games_tab")),
            tabPanel("Demos",        br(), uiOutput("demos_tab")),
            tabPanel("Spend",        br(), uiOutput("spend_tab")),
            tabPanel("Account",      br(), uiOutput("account_tab")),
            tabPanel("Live Tracker", br(), uiOutput("live_tracker_tab")),
            tabPanel("Settings",     br(), uiOutput("settings_tab"))
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
      t2 <- tryCatch(db_query("SELECT MAX(COALESCE(updated_at,created_at)) ts FROM job_assignments;")$ts[1] %||% "", error=function(e)"")
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
          "SELECT ja.id, ja.user_id, u.display_name, u.section, jp.job_name,
                  ja.assigned_wage,
                  COALESCE(ja.outcome,'') AS outcome,
                  COALESCE(ja.tokens_awarded,0) AS tokens_awarded
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

      # Flex Questions progress
      {
        total_q <- tryCatch(
          db_query("SELECT COUNT(*) n FROM flex_questions WHERE COALESCE(active,1)=1;")$n[1],
          error = function(e) 0L)
        if (as.integer(total_q %||% 0L) > 0) {
          owned_n <- tryCatch(
            db_query("SELECT COUNT(*) n FROM flex_purchases WHERE user_id=?;", list(rv$user_id))$n[1],
            error = function(e) 0L)
          next_cost <- question_cost_for_n(as.integer(owned_n %||% 0L) + 1L)
          div(class = "today-card",
            tags$strong("\U0001f4da Questions"),
            tags$p(style = "color:#555;font-size:.86rem;margin:.2rem 0 .3rem;",
                   sprintf("You own %d of %d questions. Next costs %d tokens.",
                           as.integer(owned_n %||% 0L), as.integer(total_q),
                           as.integer(next_cost))),
            actionButton("go_to_spend_fq", "Buy in Spend tab →",
                         class = "btn btn-sm btn-outline-primary",
                         style = "margin-top:.2rem;")
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
  observeEvent(input$go_to_spend_fq, {
    rv$spend_mode <- "flex_question"
    updateTabsetPanel(session, "arc_tabs", selected = "Spend")
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
      owned_count <- tryCatch(
        db_query("SELECT COUNT(*) n FROM flex_purchases WHERE user_id=?;", list(rv$user_id))$n[1],
        error = function(e) 0L)
      total_q <- tryCatch(
        db_query("SELECT COUNT(*) n FROM flex_questions WHERE COALESCE(active,1)=1;")$n[1],
        error = function(e) 0L)
      next_cost <- question_cost_for_n(as.integer(owned_count %||% 0L) + 1L)
      fq_status <- if (total_q == 0) "No questions loaded yet" else
        sprintf("%d / %d purchased · next costs %d tokens",
                as.integer(owned_count %||% 0L), as.integer(total_q),
                as.integer(next_cost))

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
            div(class = "spend-card-icon", "\U0001f4da"),
            div(class = "spend-card-label", "Buy a Question"),
            div(class = "spend-card-desc",
                "Unlock the next exam question. Questions are revealed in order."),
            div(class = "spend-card-meta", fq_status),
            div(class = "spend-card-foot",
                actionButton("open_flex_question", "Select →", class = "btn btn-sm btn-outline-primary"))
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

  observeEvent(input$open_extension,     { rv$spend_mode <- "extension"     })
  observeEvent(input$open_reweight,      { rv$spend_mode <- "reweight"      })
  observeEvent(input$open_flex_question, { rv$spend_mode <- "flex_question" })
  observeEvent(input$spend_back,         { rv$spend_mode <- NULL            })

  output$spend_form <- renderUI({
    req(rv$authed)
    bal <- token_bal()
    mode <- rv$spend_mode %||% ""

    if (mode == "extension") {
      ps_rows <- tryCatch(db_query(
        "SELECT * FROM problem_sets WHERE COALESCE(active,1)=1 ORDER BY original_deadline DESC LIMIT 20;"),
        error = function(e) data.frame())
      opts <- parse_ext_prices()
      if (!nrow(ps_rows) || !nrow(opts))
        return(div(class = "spend-form-box",
                   "No extension options are configured yet. Ask your instructor to set them up."))
      opt_choices <- setNames(opts$id,
                              paste0(opts$label, " (", as.integer(opts$tokens), " tokens)"))
      tagList(
        div(class = "spend-form-box",
          tags$h6(style = "color:#951829;font-weight:700;", "\U0001f4c5 Problem Set Extension"),
          selectInput("ext_ps", "Problem set:",
                      setNames(ps_rows$id, ps_rows$name)),
          selectInput("ext_option", "Extension length:", choices = opt_choices),
          uiOutput("ext_cost_preview"),
          actionButton("submit_extension", "Purchase extension", class = "btn btn-warning")
        )
      )

    } else if (mode == "reweight") {
      cats_df <- tryCatch(parse_grade_categories(),
                          error = function(e)
                            data.frame(name=c("Homework","Midterm","Final"),
                                       weight=c(33,33,34), stringsAsFactors=FALSE))
      cats  <- cats_df$name
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

    } else if (mode == "flex_question") {
      owned <- tryCatch(db_query(
        "SELECT fp.question_id, fq.question_text, fq.order_index
         FROM flex_purchases fp
         JOIN flex_questions fq ON fq.id=fp.question_id
         WHERE fp.user_id=? ORDER BY fq.order_index ASC, fq.id ASC;",
        list(rv$user_id)), error = function(e) data.frame())
      total_q <- tryCatch(
        db_query("SELECT COUNT(*) n FROM flex_questions WHERE COALESCE(active,1)=1;")$n[1],
        error = function(e) 0L)
      n_owned <- nrow(owned)
      next_cost <- question_cost_for_n(n_owned + 1L)
      all_done  <- n_owned >= as.integer(total_q %||% 0L)
      div(class = "spend-form-box",
        tags$h6(style = "color:#951829;font-weight:700;", "\U0001f4da Buy a Question"),
        if (total_q == 0) {
          tags$p(style = "color:#999;", "No questions have been loaded yet.")
        } else if (all_done) {
          tags$p(style = "color:#1a6e3c;font-weight:600;",
                 sprintf("You have purchased all %d questions!", as.integer(total_q)))
        } else {
          tagList(
            tags$p(style = "color:#555;font-size:.88rem;",
                   sprintf("You own %d of %d questions. The next question costs %d tokens.",
                           n_owned, as.integer(total_q), as.integer(next_cost))),
            actionButton("submit_flex_question",
                         sprintf("Buy question #%d (%d tokens)", n_owned + 1L, as.integer(next_cost)),
                         class = "btn btn-warning")
          )
        },
        if (n_owned > 0) {
          tagList(
            tags$hr(),
            tags$strong("Your purchased questions:"),
            lapply(seq_len(n_owned), function(i) {
              div(style = "margin-top:.5rem;padding:.5rem .7rem;background:#f8f8f8;border-radius:4px;",
                  tags$small(style = "color:#888;", sprintf("Question #%d", i)),
                  tags$p(style = "margin:.2rem 0 0;", owned$question_text[i]))
            })
          )
        }
      )
    }
  })

  output$ext_cost_preview <- renderUI({
    req(rv$authed)
    opt_id <- suppressWarnings(as.integer(input$ext_option %||% 0))
    if (is.na(opt_id) || opt_id <= 0) return(NULL)
    opt  <- tryCatch(db_query("SELECT tokens FROM extension_options WHERE id=?;", list(opt_id)),
                     error = function(e) data.frame())
    cost <- if (nrow(opt)) as.numeric(opt$tokens[1]) else 0
    bal  <- token_bal()
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
    opt_id <- suppressWarnings(as.integer(input$ext_option %||% 0))
    if (is.na(opt_id) || opt_id <= 0) { showNotification("Select an extension option.", type = "error"); return() }
    opt <- tryCatch(db_query("SELECT * FROM extension_options WHERE id=?;", list(opt_id)),
                    error = function(e) data.frame())
    if (!nrow(opt)) { showNotification("Invalid extension option.", type = "error"); return() }
    cost <- as.numeric(opt$tokens[1])
    hrs  <- as.numeric(opt$hours[1])
    lbl  <- as.character(opt$label[1])
    bal  <- isolate(token_bal())
    if (cost <= 0) { showNotification("Cost not set for this option.", type = "error"); return() }
    if (bal < cost) {
      showNotification(sprintf("Not enough tokens (need %d, have %d).", as.integer(cost), as.integer(bal)),
                       type = "error"); return()
    }
    ps_id <- as.integer(input$ext_ps %||% 0)
    if (ps_id <= 0) { showNotification("Select a problem set.", type = "error"); return() }
    lid <- token_debit(rv$user_id, rv$name, cost, "extension", ps_id,
                       note = sprintf("%s extension", lbl))
    db_exec(
      "INSERT INTO extension_purchases(problem_set_id,user_id,hours,cost,ledger_id) VALUES(?,?,?,?,?);",
      list(ps_id, rv$user_id, hrs, cost, as.integer(lid %||% NA_integer_)))
    showNotification(sprintf("Extension purchased: %s for %d tokens.", lbl, as.integer(cost)),
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

  observeEvent(input$submit_flex_question, {
    req(rv$authed, rv$user_id)
    if (rv$is_demo) { showNotification("Demo mode.", type = "warning"); return() }
    owned <- tryCatch(db_query(
      "SELECT question_id FROM flex_purchases WHERE user_id=?;", list(rv$user_id)),
      error = function(e) data.frame())
    owned_ids <- if (nrow(owned)) as.integer(owned$question_id) else integer(0)
    nxt <- tryCatch({
      if (length(owned_ids)) {
        q <- sprintf(
          "SELECT id, question_text, order_index FROM flex_questions
           WHERE COALESCE(active,1)=1 AND id NOT IN (%s)
           ORDER BY order_index ASC, id ASC LIMIT 1;",
          paste(owned_ids, collapse=","))
        db_query(q, list())
      } else {
        db_query(
          "SELECT id, question_text, order_index FROM flex_questions
           WHERE COALESCE(active,1)=1 ORDER BY order_index ASC, id ASC LIMIT 1;")
      }
    }, error = function(e) data.frame())
    if (!nrow(nxt)) {
      showNotification("You have purchased all available questions.", type = "message"); return()
    }
    n_owned <- length(owned_ids) + 1L
    cost <- question_cost_for_n(n_owned)
    bal  <- isolate(token_bal())
    if (bal < cost) {
      showNotification(sprintf("Not enough tokens (need %d, have %d).", as.integer(cost), as.integer(bal)),
                       type = "error"); return()
    }
    qid <- as.integer(nxt$id[1])
    lid <- token_debit(rv$user_id, rv$name, cost, "flex_question", qid,
                       note = sprintf("Question #%d", n_owned))
    db_exec(
      "INSERT OR IGNORE INTO flex_purchases(user_id,question_id,tokens_spent) VALUES(?,?,?);",
      list(rv$user_id, qid, cost))
    showNotification(sprintf("Question purchased for %d tokens.", as.integer(cost)), type = "message")
    rv$spend_mode <- "flex_question"
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

  # ── Show/hide admin tabs based on is_admin (hidden when impersonating) ────────
  observe({
    show_admin <- isTRUE(rv$is_admin) && !isTRUE(rv$impersonating)
    if (show_admin) {
      showTab("arc_tabs", "Live Tracker")
      showTab("arc_tabs", "Settings")
    } else {
      hideTab("arc_tabs", "Live Tracker")
      hideTab("arc_tabs", "Settings")
    }
  })

  # ── Impersonation ─────────────────────────────────────────────────────────────
  observeEvent(input$impersonate_uid, {
    req(rv$is_admin, !rv$impersonating)
    uid <- trimws(input$impersonate_uid %||% "")
    if (!nzchar(uid)) return()
    row <- db_query(
      "SELECT user_id, display_name, section, COALESCE(is_demo,0) AS is_demo
       FROM users WHERE user_id=? AND COALESCE(active,1)=1;", list(uid))
    if (!nrow(row)) return()
    rv$orig_state   <- list(user_id=rv$user_id, name=rv$name, section=rv$section,
                            is_admin=rv$is_admin, is_demo=rv$is_demo)
    rv$user_id      <- row$user_id[1]
    rv$name         <- coalesce_str(row$display_name[1] %||% "", row$user_id[1])
    rv$section      <- row$section[1] %||% ""
    rv$is_admin     <- FALSE
    rv$is_demo      <- isTRUE(as.integer(row$is_demo[1] %||% 0L) == 1L)
    rv$impersonating <- TRUE
    showNotification(sprintf("Now viewing as %s.", rv$name), type = "message")
    updateTabsetPanel(session, "arc_tabs", selected = "Today")
  }, ignoreNULL = TRUE)

  observeEvent(input$stop_impersonate_btn, {
    req(rv$impersonating, !is.null(rv$orig_state))
    st <- rv$orig_state
    rv$user_id      <- st$user_id
    rv$name         <- st$name
    rv$section      <- st$section
    rv$is_admin     <- st$is_admin
    rv$is_demo      <- st$is_demo
    rv$impersonating <- FALSE
    rv$orig_state   <- NULL
    updateTabsetPanel(session, "arc_tabs", selected = "Settings")
    showNotification("Returned to admin view.", type = "message")
  })

  observeEvent(input$active_section_sel, {
    req(rv$is_admin)
    sec <- input$active_section_sel %||% ""
    rv$active_section <- sec
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('active_section',?);",
            list(sec))
  }, ignoreNULL = FALSE)

  # ── Student management ────────────────────────────────────────────────────────
  observeEvent(input$archive_uid, {
    req(rv$is_admin, !rv$impersonating)
    uid <- trimws(input$archive_uid %||% "")
    if (!nzchar(uid)) return()
    db_exec("UPDATE users SET active=0 WHERE user_id=?;", list(uid))
    showNotification(sprintf("Archived %s.", uid), type = "warning")
  }, ignoreNULL = TRUE)

  observeEvent(input$restore_uid, {
    req(rv$is_admin, !rv$impersonating)
    uid <- trimws(input$restore_uid %||% "")
    if (!nzchar(uid)) return()
    db_exec("UPDATE users SET active=1 WHERE user_id=?;", list(uid))
    showNotification(sprintf("Restored %s.", uid), type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$create_student_btn, {
    req(rv$is_admin)
    uid <- trimws(input$new_stu_uid %||% "")
    nm  <- trimws(input$new_stu_name %||% "")
    pw  <- input$new_stu_pw %||% ""
    sec <- trimws(input$new_stu_section %||% "")
    if (!nzchar(uid) || !nzchar(pw)) {
      showNotification("Username and password are required.", type = "error"); return()
    }
    if (nchar(pw) < 4) {
      showNotification("Password must be at least 4 characters.", type = "error"); return()
    }
    ex <- db_query("SELECT user_id FROM users WHERE LOWER(user_id)=LOWER(?);", list(uid))
    if (nrow(ex)) { showNotification("Username already exists.", type = "error"); return() }
    db_exec(
      "INSERT INTO users(user_id, display_name, pw_hash, is_admin, section, active, is_demo)
       VALUES(?,?,?,0,?,1,0);",
      list(uid, if (nzchar(nm)) nm else uid, bcrypt::hashpw(pw), sec))
    showNotification(sprintf("Created student %s.", uid), type = "message")
  })

  observeEvent(input$reset_pw_btn, {
    req(rv$is_admin)
    uid <- trimws(input$reset_pw_uid %||% "")
    pw  <- input$reset_pw_new %||% ""
    if (!nzchar(uid) || !nzchar(pw)) {
      showNotification("Username and new password are required.", type = "error"); return()
    }
    if (nchar(pw) < 4) {
      showNotification("Password must be at least 4 characters.", type = "error"); return()
    }
    ex <- db_query("SELECT user_id FROM users WHERE LOWER(user_id)=LOWER(?);", list(uid))
    if (!nrow(ex)) { showNotification("User not found.", type = "error"); return() }
    db_exec("UPDATE users SET pw_hash=? WHERE LOWER(user_id)=LOWER(?);",
            list(bcrypt::hashpw(pw), uid))
    showNotification(sprintf("Password reset for %s.", uid), type = "message")
  })

  # ── Job management ────────────────────────────────────────────────────────────
  observeEvent(input$add_job_cat_btn, {
    req(rv$is_admin)
    nm   <- trimws(input$new_cat_name %||% "")
    wage <- as.numeric(input$new_cat_wage %||% 10)
    desc <- trimws(input$new_cat_desc %||% "")
    if (!nzchar(nm)) { showNotification("Category name required.", type = "error"); return() }
    db_exec(
      "INSERT INTO job_categories(name, default_wage, description) VALUES(?,?,?);",
      list(nm, if (is.na(wage)) 10 else wage, desc))
    showNotification("Job category added.", type = "message")
  })

  observeEvent(input$add_job_post_btn, {
    req(rv$is_admin)
    nm      <- trimws(input$new_post_name %||% "")
    cat_id  <- suppressWarnings(as.integer(input$new_post_cat %||% 0))
    slots   <- max(1L, as.integer(input$new_post_slots %||% 1L))
    wage    <- suppressWarnings(as.numeric(input$new_post_wage))
    in_draw <- as.integer(isTRUE(input$new_post_in_draw))
    vol     <- as.integer(isTRUE(input$new_post_voluntary))
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("Create a round first.", type = "error"); return() }
    if (!nzchar(nm)) { showNotification("Post name required.", type = "error"); return() }
    rid <- rid_row$id[1]
    db_exec(
      "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override, in_draw, voluntary)
       VALUES(?,?,?,?,?,?,?);",
      list(rid, nm,
           if (!is.na(cat_id) && cat_id > 0) cat_id else NA_integer_,
           slots,
           if (!is.null(wage) && !is.na(wage) && wage > 0) wage else NA_real_,
           in_draw, vol))
    showNotification("Job post added.", type = "message")
  })

  observeEvent(input$add_part_type_btn, {
    req(rv$is_admin)
    nm     <- trimws(input$new_pt_name %||% "")
    cat_id <- suppressWarnings(as.integer(input$new_pt_cat %||% 0))
    slots  <- max(1L, as.integer(input$new_pt_slots %||% 99L))
    tokens <- suppressWarnings(as.numeric(input$new_pt_tokens %||% 1))
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("Create a round first.", type = "error"); return() }
    if (!nzchar(nm)) { showNotification("Name required.", type = "error"); return() }
    rid <- rid_row$id[1]
    db_exec(
      "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override, voluntary)
       VALUES(?,?,?,?,?,1);",
      list(rid, nm,
           if (!is.na(cat_id) && cat_id > 0) cat_id else NA_integer_,
           slots,
           if (!is.null(tokens) && !is.na(tokens) && tokens >= 0) tokens else NA_real_))
    showNotification("Participation type added to current round.", type = "message")
  })

  observeEvent(input$add_template_btn, {
    req(rv$is_admin)
    nm    <- trimws(input$new_tpl_name %||% "")
    cat_id <- suppressWarnings(as.integer(input$new_tpl_cat %||% 0))
    slots  <- max(1L, as.integer(input$new_tpl_slots %||% 1L))
    wage   <- suppressWarnings(as.numeric(input$new_tpl_wage))
    if (!nzchar(nm)) { showNotification("Template name required.", type = "error"); return() }
    db_exec(
      "INSERT INTO job_templates(name, category_id, slots, suggested_wage) VALUES(?,?,?,?);",
      list(nm,
           if (!is.na(cat_id) && cat_id > 0) cat_id else NA_integer_,
           slots,
           if (!is.null(wage) && !is.na(wage) && wage >= 0) wage else NA_real_))
    showNotification("Template added.", type = "message")
  })

  observeEvent(input$remove_template_btn, {
    req(rv$is_admin)
    tid <- suppressWarnings(as.integer(input$remove_template_btn %||% 0))
    if (is.na(tid) || tid <= 0) return()
    db_exec("UPDATE job_templates SET active=0 WHERE id=?;", list(tid))
    showNotification("Template removed.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$apply_clearing_wage_btn, {
    req(rv$is_admin)
    ev <- input$apply_clearing_wage_btn
    if (is.null(ev) || is.null(ev$post_id) || is.null(ev$wage)) return()
    post_id <- suppressWarnings(as.integer(ev$post_id))
    wage    <- suppressWarnings(as.numeric(ev$wage))
    if (is.na(post_id) || post_id <= 0 || is.na(wage)) return()
    db_exec("UPDATE job_posts SET wage_override=? WHERE id=?;", list(wage, post_id))
    showNotification(sprintf("Clearing wage %g applied.", wage), type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$toggle_post_active, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$toggle_post_active %||% 0))
    if (is.na(pid) || pid <= 0) return()
    cur <- db_query("SELECT COALESCE(active,1) a FROM job_posts WHERE id=?;", list(pid))
    if (!nrow(cur)) return()
    new_a <- if (isTRUE(as.integer(cur$a[1]) == 1L)) 0L else 1L
    db_exec("UPDATE job_posts SET active=? WHERE id=?;", list(new_a, pid))
  }, ignoreNULL = TRUE)

  observeEvent(input$toggle_post_in_draw, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$toggle_post_in_draw %||% 0))
    if (is.na(pid) || pid <= 0) return()
    cur <- db_query("SELECT COALESCE(in_draw,1) v FROM job_posts WHERE id=?;", list(pid))
    if (!nrow(cur)) return()
    new_v <- if (isTRUE(as.integer(cur$v[1]) == 1L)) 0L else 1L
    db_exec("UPDATE job_posts SET in_draw=? WHERE id=?;", list(new_v, pid))
  }, ignoreNULL = TRUE)

  observeEvent(input$toggle_post_voluntary, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$toggle_post_voluntary %||% 0))
    if (is.na(pid) || pid <= 0) return()
    cur <- db_query("SELECT COALESCE(voluntary,0) v FROM job_posts WHERE id=?;", list(pid))
    if (!nrow(cur)) return()
    new_v <- if (isTRUE(as.integer(cur$v[1]) == 1L)) 0L else 1L
    db_exec("UPDATE job_posts SET voluntary=? WHERE id=?;", list(new_v, pid))
  }, ignoreNULL = TRUE)

  observeEvent(input$unassign_job_btn, {
    req(rv$is_admin)
    aid <- suppressWarnings(as.integer(input$unassign_job_btn %||% 0))
    if (is.na(aid) || aid <= 0) return()
    db_exec("DELETE FROM job_assignments WHERE id=?;", list(aid))
    showNotification("Assignment removed.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$clear_assignments_btn, {
    req(rv$is_admin)
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("No active round.", type = "error"); return() }
    db_exec("DELETE FROM job_assignments WHERE round_id=?;", list(rid_row$id[1]))
    showNotification("All assignments for this round cleared.", type = "message")
  })

  observeEvent(input$edit_cat_btn, {
    req(rv$is_admin)
    ev <- input$edit_cat_btn
    if (is.null(ev) || is.null(ev$id)) return()
    cid  <- suppressWarnings(as.integer(ev$id))
    nm   <- trimws(ev$name %||% "")
    wage <- suppressWarnings(as.numeric(ev$wage))
    desc <- trimws(ev$desc %||% "")
    if (!nzchar(nm)) { showNotification("Category name required.", type = "error"); return() }
    if (is.na(cid) || cid <= 0) return()
    db_exec("UPDATE job_categories SET name=?, default_wage=?, description=? WHERE id=?;",
            list(nm, if (is.na(wage)) 0 else wage, desc, cid))
    showNotification("Category updated.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$create_round_btn, {
    req(rv$is_admin)
    lbl    <- trimws(input$new_round_label %||% "")
    mode   <- input$new_round_mode %||% "random"
    open_d <- as.character(input$new_round_open %||% "")
    cls_d  <- as.character(input$new_round_close %||% "")
    tix    <- max(1L, as.integer(input$new_round_tix %||% 10L))
    if (!nzchar(lbl)) { showNotification("Round label required.", type = "error"); return() }
    db_exec(
      "INSERT INTO weekly_rounds(label, assignment_mode, bid_open_date, bid_close_date, tickets_per_student)
       VALUES(?,?,?,?,?);",
      list(lbl, mode,
           if (nzchar(open_d)) open_d else NA_character_,
           if (nzchar(cls_d))  cls_d  else NA_character_,
           tix))
    showNotification("Round created.", type = "message")
  })

  observeEvent(input$update_round_btn, {
    req(rv$is_admin)
    round <- tryCatch(db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                      error = function(e) data.frame())
    if (!nrow(round)) { showNotification("No round to update.", type = "error"); return() }
    rid    <- round$id[1]
    lbl    <- trimws(input$edit_round_label %||% "")
    mode   <- input$edit_round_mode %||% "random"
    open_d <- as.character(input$edit_round_open %||% "")
    cls_d  <- as.character(input$edit_round_close %||% "")
    tix    <- max(1L, as.integer(input$edit_round_tix %||% 10L))
    if (!nzchar(lbl)) { showNotification("Label required.", type = "error"); return() }
    db_exec(
      "UPDATE weekly_rounds SET label=?, assignment_mode=?, bid_open_date=?, bid_close_date=?,
       tickets_per_student=? WHERE id=?;",
      list(lbl, mode,
           if (nzchar(open_d)) open_d else NA_character_,
           if (nzchar(cls_d))  cls_d  else NA_character_,
           tix, rid))
    showNotification("Round updated.", type = "message")
  })

  observeEvent(input$create_next_round_btn, {
    req(rv$is_admin)
    last <- tryCatch(db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                     error = function(e) data.frame())
    new_label <- if (nrow(last)) {
      lbl <- last$label[1] %||% "Week 1"
      m   <- regmatches(lbl, regexpr("[0-9]+", lbl))
      if (length(m))
        sub(m, as.character(as.integer(m) + 1L), lbl, fixed = TRUE)
      else paste0(lbl, " (2)")
    } else "Week 1"
    mode <- if (nrow(last)) last$assignment_mode[1] %||% "random" else "random"
    db_exec("INSERT INTO weekly_rounds(label, assignment_mode) VALUES(?,?);",
            list(new_label, mode))
    new_rid <- tryCatch(db_query("SELECT last_insert_rowid() AS id;")$id[1],
                        error = function(e) NA_integer_)
    if (is.na(new_rid)) {
      showNotification("Round created but could not retrieve ID.", type = "warning"); return()
    }
    templates <- tryCatch(db_query(
      "SELECT * FROM job_templates WHERE COALESCE(active,1)=1 ORDER BY id;"),
      error = function(e) data.frame())
    if (nrow(templates)) {
      for (i in seq_len(nrow(templates))) {
        t <- templates[i, ]
        db_exec(
          "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override, in_draw, voluntary)
           VALUES(?,?,?,?,?,1,0);",
          list(new_rid, t$name,
               if (!is.na(t$category_id %||% NA)) as.integer(t$category_id) else NA_integer_,
               as.integer(t$slots %||% 1L),
               if (!is.na(t$suggested_wage %||% NA)) as.numeric(t$suggested_wage) else NA_real_))
      }
      showNotification(
        sprintf("Created round '%s' with %d post%s from templates.",
                new_label, nrow(templates), if (nrow(templates) == 1) "" else "s"),
        type = "message")
    } else {
      showNotification(
        sprintf("Created round '%s'. No active templates to copy.", new_label),
        type = "message")
    }
  })

  observeEvent(input$save_rw_setup_btn, {
    req(rv$is_admin)
    costs_str <- trimws(input$rw_costs_input %||% "")
    if (!nzchar(costs_str)) {
      showNotification("Enter a cost schedule.", type = "error"); return()
    }
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('reweight_cost_schedule',?);",
            list(costs_str))
    showNotification("Cost schedule saved.", type = "message")
  })

  observeEvent(input$add_grade_cat_btn, {
    req(rv$is_admin)
    nm     <- trimws(input$new_grade_cat_name %||% "")
    weight <- suppressWarnings(as.numeric(input$new_grade_cat_weight %||% 0))
    if (!nzchar(nm)) {
      showNotification("Category name required.", type = "error"); return()
    }
    if (is.na(weight) || weight < 0 || weight > 100) {
      showNotification("Weight must be between 0 and 100.", type = "error"); return()
    }
    cats_df <- tryCatch(parse_grade_categories(),
                        error = function(e) data.frame(name=character(0), weight=numeric(0)))
    if (nm %in% cats_df$name) {
      showNotification(sprintf("Category '%s' already exists.", nm), type = "error"); return()
    }
    cats_df <- rbind(cats_df, data.frame(name=nm, weight=weight, stringsAsFactors=FALSE))
    db_exec(
      "INSERT OR REPLACE INTO labor_settings(key,value) VALUES('grade_categories_json',?);",
      list(jsonlite::toJSON(cats_df, auto_unbox = FALSE)))
    showNotification(sprintf("Added category '%s' (%.0f%%).", nm, weight), type = "message")
  })

  observeEvent(input$delete_grade_cat, {
    req(rv$is_admin)
    nm <- trimws(input$delete_grade_cat %||% "")
    if (!nzchar(nm)) return()
    cats_df <- tryCatch(parse_grade_categories(),
                        error = function(e) data.frame(name=character(0), weight=numeric(0)))
    cats_df <- cats_df[cats_df$name != nm, , drop = FALSE]
    db_exec(
      "INSERT OR REPLACE INTO labor_settings(key,value) VALUES('grade_categories_json',?);",
      list(jsonlite::toJSON(cats_df, auto_unbox = FALSE)))
    showNotification(sprintf("Removed category '%s'.", nm), type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$add_ext_option_btn, {
    req(rv$is_admin)
    lbl    <- trimws(input$new_ext_label %||% "")
    hrs    <- suppressWarnings(as.numeric(input$new_ext_hours %||% 0))
    tokens <- suppressWarnings(as.numeric(input$new_ext_tokens %||% 0))
    if (!nzchar(lbl)) { showNotification("Label required.", type = "error"); return() }
    if (is.na(hrs) || hrs <= 0) { showNotification("Hours must be positive.", type = "error"); return() }
    if (is.na(tokens) || tokens <= 0) { showNotification("Token cost must be positive.", type = "error"); return() }
    db_exec("INSERT INTO extension_options(label,hours,tokens) VALUES(?,?,?);",
            list(lbl, hrs, tokens))
    showNotification("Extension option added.", type = "message")
  })

  observeEvent(input$delete_ext_option_btn, {
    req(rv$is_admin)
    oid <- suppressWarnings(as.integer(input$delete_ext_option_btn %||% 0))
    if (is.na(oid) || oid <= 0) return()
    db_exec("UPDATE extension_options SET active=0 WHERE id=?;", list(oid))
    showNotification("Extension option removed.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$save_flex_cost_btn, {
    req(rv$is_admin)
    sched <- trimws(input$flex_cost_input %||% "")
    if (!nzchar(sched)) { showNotification("Enter a schedule.", type = "error"); return() }
    parts <- tryCatch(as.numeric(strsplit(sched, ",")[[1]]), error=function(e) NA_real_)
    if (any(is.na(parts))) { showNotification("Invalid schedule — use comma-separated numbers.", type = "error"); return() }
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('flex_cost_schedule',?);",
            list(sched))
    showNotification("Price schedule saved.", type = "message")
  })

  observeEvent(input$add_flex_question_btn, {
    req(rv$is_admin)
    txt <- trimws(input$new_fq_text %||% "")
    if (!nzchar(txt)) { showNotification("Question text required.", type = "error"); return() }
    max_idx <- tryCatch(
      db_query("SELECT COALESCE(MAX(order_index),0) n FROM flex_questions;")$n[1],
      error = function(e) 0L)
    db_exec("INSERT INTO flex_questions(question_text,order_index) VALUES(?,?);",
            list(txt, as.integer(max_idx %||% 0L) + 1L))
    showNotification("Question added.", type = "message")
  })

  observeEvent(input$delete_flex_question_btn, {
    req(rv$is_admin)
    qid <- suppressWarnings(as.integer(input$delete_flex_question_btn %||% 0))
    if (is.na(qid) || qid <= 0) return()
    db_exec("UPDATE flex_questions SET active=0 WHERE id=?;", list(qid))
    showNotification("Question removed.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$upload_flex_questions_btn, {
    req(rv$is_admin)
    f <- input$upload_flex_questions
    if (is.null(f)) { showNotification("Choose a file first.", type = "error"); return() }
    ext <- tolower(tools::file_ext(f$name))
    lines <- tryCatch({
      if (ext == "csv") {
        df <- read.csv(f$datapath, stringsAsFactors = FALSE)
        col <- intersect(c("question_text","question","text"), names(df))
        if (!length(col)) stop("CSV must have a 'question_text' column.")
        df[[col[1]]]
      } else {
        raw <- readLines(f$datapath, warn = FALSE)
        trimws(raw[nzchar(trimws(raw))])
      }
    }, error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL })
    if (is.null(lines)) return()
    lines <- lines[nzchar(trimws(lines))]
    if (!length(lines)) { showNotification("No questions found in file.", type = "warning"); return() }
    if (isTRUE(input$fq_replace_all)) {
      db_exec("UPDATE flex_questions SET active=0;")
    }
    max_idx <- tryCatch(
      db_query("SELECT COALESCE(MAX(order_index),0) n FROM flex_questions;")$n[1],
      error = function(e) 0L)
    base_idx <- as.integer(max_idx %||% 0L)
    for (i in seq_along(lines)) {
      db_exec("INSERT INTO flex_questions(question_text,order_index) VALUES(?,?);",
              list(lines[i], base_idx + i))
    }
    showNotification(sprintf("Uploaded %d questions.", length(lines)), type = "message")
  })

  # quick_award_btn removed — use Settings → Token Admin for awards

  # ── Live Tracker tab (admin) ──────────────────────────────────────────────────
  output$live_tracker_tab <- renderUI({
    req(rv$authed, rv$is_admin)
    td        <- tracker_poll()
    revealed  <- td$revealed
    round     <- td$round
    mode      <- if (nrow(round)) round$assignment_mode[1] %||% "random" else "random"
    wage_mode <- identical(mode, "wage_bidding")

    # Section picker data
    all_sections <- tryCatch(
      sort(unique(Filter(nzchar,
        db_query("SELECT DISTINCT section FROM users WHERE COALESCE(active,1)=1;")$section
          %||% character(0)))),
      error = function(e) character(0))
    sec_choices <- c("(All sections)" = "", setNames(all_sections, all_sections))
    cur_sec <- rv$active_section %||% ""

    # Filter assignments to active section
    assignments_show <- if (nzchar(cur_sec) && nrow(td$assignments))
      td$assignments[td$assignments$section == cur_sec, , drop = FALSE]
    else td$assignments
    n_show <- nrow(assignments_show)

    students_sec <- if (nrow(td$students) && nzchar(cur_sec))
      td$students[td$students$section == cur_sec, , drop = FALSE]
    else td$students

    # Voluntary job posts for participation panel (Panel 2)
    vol_cats <- if (!is.na(rid)) {
      tryCatch(db_query(
        "SELECT jp.id, jp.job_name AS name, COALESCE(jp.wage_override, jc.default_wage, 1) AS tokens
         FROM job_posts jp LEFT JOIN job_categories jc ON jc.id=jp.category_id
         WHERE jp.round_id=? AND COALESCE(jp.voluntary,0)=1 AND COALESCE(jp.active,1)=1
         ORDER BY jp.job_name;", list(rid)),
        error = function(e) data.frame())
    } else data.frame()

    # Build student choices: bidders for current round first
    rid <- if (nrow(round)) round$id[1] else NA_integer_
    bidder_ids <- if (!is.na(rid) && nrow(students_sec)) {
      tryCatch(db_query(
        "SELECT DISTINCT user_id FROM wage_bids WHERE round_id=?;",
        list(rid))$user_id, error = function(e) character(0))
    } else character(0)
    stu_nm  <- students_sec$display_name %||% students_sec$user_id
    stu_sec <- students_sec$section %||% ""
    stu_lbl <- ifelse(nzchar(stu_sec), paste0(stu_nm, " (", stu_sec, ")"), stu_nm)
    stu_choices_raw <- setNames(students_sec$user_id, stu_lbl)
    is_bidder   <- students_sec$user_id %in% bidder_ids
    stu_choices <- c(stu_choices_raw[is_bidder], stu_choices_raw[!is_bidder])

    tagList(
      div(class = "tab-howto",
          "Manage job assignments and log participation during class. Updates every 5 seconds."),

      # Section selector
      fluidRow(
        column(4,
          selectInput("active_section_sel", "Active section:",
                      choices = sec_choices, selected = cur_sec, width = "100%"))
      ),

      # Panel 1: Job Assignments
      wellPanel(
        tags$h6(style = "font-weight:700;color:#951829;margin-bottom:.5rem;",
                "\U0001f4cb Job Assignments"),
        if (!nrow(round)) {
          tags$p(style = "color:#999;margin:0;",
                 "No active round configured. Set one up in Settings → Round Setup.")
        } else {
          mode_label <- switch(mode,
            random              = "Random draw",
            application_bidding = "Weighted lottery (ticket bids)",
            wage_bidding        = "Lowest-bid draw",
            paste("Mode:", mode))
          tagList(
            tags$p(style = "color:#555;font-size:.88rem;margin-bottom:.6rem;",
                   sprintf("Round: %s  ·  %s%s",
                           round$label[1] %||% paste("Round", round$id[1]),
                           mode_label,
                           if (nzchar(cur_sec)) paste0("  ·  Section: ", cur_sec) else "")),
            fluidRow(
              column(3,
                actionButton("run_draw_btn", "\U0001f3b2 Draw Jobs",
                             class = "btn btn-primary btn-sm",
                             title = "Assign students in selected section to jobs")),
              column(3,
                actionButton("preview_draw_btn", "\U0001f441 Preview Draw",
                             class = "btn btn-outline-secondary btn-sm")),
              column(3,
                if (revealed)
                  actionButton("toggle_reveal_btn", "Hide from Students",
                               class = "btn btn-outline-secondary btn-sm")
                else
                  actionButton("toggle_reveal_btn", "Reveal to Students",
                               class = "btn btn-success btn-sm")
              ),
              column(3,
                if (n_show > 0)
                  actionButton("clear_assignments_btn", "\U274c Clear All",
                               class = "btn btn-outline-danger btn-sm",
                               title = "Delete all assignments for current round"))
            ),
            if (n_show > 0)
              tags$p(style = "font-size:.8rem;color:#888;margin-top:.4rem;margin-bottom:0;",
                     sprintf("%d students assigned · %s", n_show,
                             if (revealed) "Visible to students" else "Hidden from students")),
            uiOutput("draw_preview_table")
          )
        }
      ),

      # Assignments table + evaluation
      if (n_show > 0) {
        half_mult <- tryCatch(as.numeric(get_setting("half_wage_multiplier","0.5")),
                              error=function(e) 0.5)
        tagList(
          div(class = "sec-label", "Current Assignments"),
          div(class = "tracker-wrap",
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Student"), tags$th("Section"), tags$th("Job"),
                if (wage_mode) tags$th(style = "text-align:right;", "Wage"),
                tags$th("Outcome"), tags$th("")
              )),
              tags$tbody(lapply(seq_len(n_show), function(i) {
                r  <- assignments_show[i, ]
                oc <- as.character(r$outcome %||% "")
                ta <- as.integer(r$tokens_awarded %||% 0L)
                wage <- if (!is.na(r$assigned_wage %||% NA)) as.numeric(r$assigned_wage) else 0
                tags$tr(
                  tags$td(r$display_name %||% r$user_id),
                  tags$td(style = "color:#888;font-size:.85em;", r$section %||% ""),
                  tags$td(style = "font-weight:600;", r$job_name %||% ""),
                  if (wage_mode)
                    tags$td(style = "text-align:right;font-size:.85em;color:#888;",
                            if (wage > 0) sprintf("%g", wage) else "—"),
                  tags$td(
                    if (ta == 1L) {
                      awarded_amt <- switch(oc,
                        complete = wage, tried = round(wage * half_mult), missed = 0, 0)
                      span(style = "color:#888;font-size:.82rem;",
                           sprintf("%s (+%d)",
                                   switch(oc, complete = "✓", tried = "~",
                                          missed = "✗", oc),
                                   as.integer(awarded_amt)))
                    } else {
                      tagList(
                        tags$button(
                          class = paste("btn btn-xs",
                                        if (oc=="complete") "btn-success" else "btn-outline-success"),
                          style = "padding:.1rem .3rem;font-size:.7rem;margin-right:.1rem;",
                          onclick = sprintf(
                            "Shiny.setInputValue('eval_outcome',{id:%d,outcome:'complete'},{priority:'event'});",
                            as.integer(r$id)), "✓"),
                        tags$button(
                          class = paste("btn btn-xs",
                                        if (oc=="tried") "btn-warning" else "btn-outline-warning"),
                          style = "padding:.1rem .3rem;font-size:.7rem;margin-right:.1rem;",
                          onclick = sprintf(
                            "Shiny.setInputValue('eval_outcome',{id:%d,outcome:'tried'},{priority:'event'});",
                            as.integer(r$id)), "~"),
                        tags$button(
                          class = paste("btn btn-xs",
                                        if (oc=="missed") "btn-danger" else "btn-outline-danger"),
                          style = "padding:.1rem .3rem;font-size:.7rem;",
                          onclick = sprintf(
                            "Shiny.setInputValue('eval_outcome',{id:%d,outcome:'missed'},{priority:'event'});",
                            as.integer(r$id)), "✗")
                      )
                    }
                  ),
                  tags$td(
                    tags$button(
                      class = "btn btn-xs btn-outline-secondary",
                      style = "padding:.1rem .3rem;font-size:.7rem;",
                      title = "Unassign",
                      onclick = sprintf(
                        "Shiny.setInputValue('unassign_job_btn',%d,{priority:'event'});",
                        as.integer(r$id)), "\U2715")
                  )
                )
              }))
            )
          )
        )
      },

      # Panel 2: Voluntary Participation
      wellPanel(
        tags$h6(style = "font-weight:700;color:#951829;margin-bottom:.6rem;",
                "\U0001f64b Voluntary Participation"),
        if (!nrow(vol_cats)) {
          tags$p(style = "color:#999;margin:0;",
                 "No voluntary job posts for this round. Toggle 'Voluntary' on a job post in Settings → Jobs.")
        } else if (!nrow(students_sec)) {
          tags$p(style = "color:#999;margin:0;", "No students in the selected section.")
        } else {
          et_choices <- setNames(vol_cats$id,
                                 paste0(vol_cats$name, " (+", as.integer(vol_cats$tokens), ")"))
          tagList(
            fluidRow(
              column(4,
                selectInput("part_event_type", "Event type:", choices = et_choices)),
              column(5,
                selectInput("part_student_sel", "Student (bidders first):",
                            choices = if (length(stu_choices)) stu_choices
                                      else c("(no students)" = ""))),
              column(3,
                tags$label(" "),
                div(style = "display:flex;gap:.35rem;",
                  actionButton("log_succeed_btn", "Succeed",
                               class = "btn btn-success btn-sm"),
                  actionButton("log_try_btn", "Try",
                               class = "btn btn-warning btn-sm"),
                  actionButton("log_miss_btn", "Miss",
                               class = "btn btn-danger btn-sm")
                )
              )
            )
          )
        }
      ),

      # Student token summary
      div(class = "sec-label",
          sprintf("Students (%d)", nrow(students_sec))),
      div(class = "tracker-wrap",
        if (!nrow(students_sec)) {
          div(style = "color:#999;", "No students found.")
        } else {
          tags$table(class = "table table-sm table-hover",
            tags$thead(tags$tr(
              tags$th("Student"), tags$th("Section"),
              tags$th(style = "text-align:right;", "Earned"),
              tags$th(style = "text-align:right;", "On Hand")
            )),
            tags$tbody(lapply(seq_len(nrow(students_sec)), function(i) {
              r <- students_sec[i, ]
              tags$tr(
                tags$td(r$display_name %||% r$user_id),
                tags$td(style = "color:#888;", r$section %||% ""),
                tags$td(style = "text-align:right;", as.integer(r$tokens_earned %||% 0)),
                tags$td(style = "text-align:right;font-weight:600;",
                        as.integer(r$tokens_on_hand %||% 0))
              )
            }))
          )
        }
      )
    )
  })

  # ── Draw Preview table ────────────────────────────────────────────────────────
  output$draw_preview_table <- renderUI({
    req(rv$is_admin)
    preview <- rv$draw_preview
    if (is.null(preview) || !length(preview)) return(NULL)
    round <- tryCatch(db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                      error = function(e) data.frame())
    wage_mode <- nrow(round) > 0 &&
      identical(round$assignment_mode[1] %||% "random", "wage_bidding")
    rows <- lapply(preview, function(p) {
      u_row  <- tryCatch(db_query("SELECT display_name FROM users WHERE user_id=?;",
                                  list(p$uid)), error=function(e) data.frame())
      jp_row <- tryCatch(db_query("SELECT job_name FROM job_posts WHERE id=?;",
                                  list(p$post_id)), error=function(e) data.frame())
      list(
        student = if (nrow(u_row)) u_row$display_name[1] %||% p$uid else p$uid,
        job     = if (nrow(jp_row)) jp_row$job_name[1] %||% "" else "",
        wage    = p$wage
      )
    })
    div(style = "margin-top:.75rem;",
      div(class = "sec-label",
          sprintf("Draw Preview (%d assignments — not saved)", length(rows))),
      div(style = paste0("padding:.35rem .7rem;border-radius:6px;background:#fff3cd;",
                         "border:1px solid #ffc107;color:#856404;font-size:.85rem;",
                         "margin-bottom:.5rem;"),
          "Preview only — click \"\U0001f3b2 Draw Jobs\" to commit (random draws may differ)."),
      div(class = "tracker-wrap",
        tags$table(class = "table table-sm table-hover",
          tags$thead(tags$tr(
            tags$th("Student"), tags$th("Job"),
            if (wage_mode) tags$th(style = "text-align:right;", "Wage")
          )),
          tags$tbody(lapply(rows, function(r) {
            tags$tr(
              tags$td(r$student),
              tags$td(style = "font-weight:600;", r$job),
              if (wage_mode)
                tags$td(style = "text-align:right;",
                        if (!is.na(r$wage %||% NA))
                          sprintf("%d", as.integer(r$wage)) else "—")
            )
          }))
        )
      )
    )
  })

  # ── Settings tab (admin) ──────────────────────────────────────────────────────
  output$settings_tab <- renderUI({
    req(rv$is_admin)
    wellPanel(
      selectInput("config_action", "Settings section:", width = "100%", choices = c(
        "Jobs"                  = "jobs",
        "Round Setup"           = "round_setup",
        "Students"              = "students",
        "Token Admin"           = "token_admin",
        "Participation Events"  = "participation_events",
        "Exports"               = "exports",
        "Grade Reweighting"     = "grade_reweighting",
        "Extensions"            = "extensions",
        "Flex Questions"        = "flex_questions",
        "Game Controls"         = "game_controls",
        "App Settings"          = "app_settings"
      ), selected = "jobs"),
      uiOutput("config_panel")
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
    act <- input$config_action %||% "jobs"

    if (act == "jobs") {
      rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                          error = function(e) data.frame())
      rid <- if (nrow(rid_row)) rid_row$id[1] else NA_integer_

      all_cats <- tryCatch(
        db_query("SELECT * FROM job_categories ORDER BY display_order, name;"),
        error = function(e) data.frame())
      vol_cats <- if (nrow(all_cats))
        all_cats[as.integer(all_cats$voluntary %||% 0) == 1L, , drop = FALSE]
      else data.frame()
      reg_cats <- if (nrow(all_cats))
        all_cats[as.integer(all_cats$voluntary %||% 0) != 1L, , drop = FALSE]
      else data.frame()

      all_posts <- if (!is.na(rid)) {
        tryCatch(db_query(
          "SELECT jp.id, jp.job_name, jp.slots, jp.category_id, jc.name AS cat_name,
                  COALESCE(jp.wage_override, jc.default_wage) AS eff_wage,
                  COALESCE(jp.active,1) AS active,
                  COALESCE(jp.in_draw,1) AS in_draw,
                  COALESCE(jp.voluntary,0) AS voluntary
           FROM job_posts jp LEFT JOIN job_categories jc ON jc.id=jp.category_id
           WHERE jp.round_id=?
           ORDER BY jp.display_order, jp.job_name;", list(rid)),
          error = function(e) data.frame())
      } else data.frame()

      templates <- tryCatch(db_query(
        "SELECT jt.id, jt.name, jc.name AS cat_name, jt.slots, jt.suggested_wage, jt.active
         FROM job_templates jt LEFT JOIN job_categories jc ON jc.id=jt.category_id
         WHERE COALESCE(jt.active,1)=1 ORDER BY jt.id;"),
        error = function(e) data.frame())

      make_flag_btn <- function(label_on, label_off, input_name, pid, is_on, cls_on, cls_off) {
        tags$button(
          class = paste("btn btn-xs", if (is_on) cls_on else cls_off),
          style = "padding:.1rem .3rem;font-size:.7rem;",
          onclick = sprintf(
            "Shiny.setInputValue('%s',%d,{priority:'event'});", input_name, as.integer(pid)),
          if (is_on) label_on else label_off)
      }

      tagList(

        # ── Job Posts ─────────────────────────────────────────────────────────────
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;",
                "Job Posts (Current Round)"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "In Draw = eligible for random draw (Panel 1). Voluntary = appears in participation panel (Panel 2). A post can be both."),
        if (is.na(rid)) {
          div(style = "color:#999;font-size:.9em;", "Create a round first (Round Setup).")
        } else if (nrow(all_posts)) {
          div(style = "overflow-x:auto;",
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Post"), tags$th("Cat"), tags$th("Slots"),
                tags$th("Wage"), tags$th("Clearing Wage"),
                tags$th("In Draw"), tags$th("Voluntary"), tags$th("Active")
              )),
              tags$tbody(lapply(seq_len(nrow(all_posts)), function(i) {
                r        <- all_posts[i, ]
                is_act   <- isTRUE(as.integer(r$active)   == 1L)
                in_draw  <- isTRUE(as.integer(r$in_draw)  == 1L)
                is_vol   <- isTRUE(as.integer(r$voluntary) == 1L)
                clr_wage <- compute_clearing_wage(r$category_id, rid, as.integer(r$slots %||% 1L))
                tags$tr(
                  tags$td(r$job_name %||% ""),
                  tags$td(style = "color:#888;font-size:.82em;", r$cat_name %||% "—"),
                  tags$td(r$slots %||% 1),
                  tags$td(sprintf("%g", as.numeric(r$eff_wage %||% 0))),
                  tags$td(
                    if (!is.na(clr_wage)) {
                      tags$button(
                        class = "btn btn-xs btn-outline-info",
                        style = "padding:.1rem .35rem;font-size:.7rem;",
                        onclick = sprintf(
                          "Shiny.setInputValue('apply_clearing_wage_btn',{post_id:%d,wage:%g},{priority:'event'});",
                          as.integer(r$id), clr_wage),
                        sprintf("%g ✔", clr_wage))
                    } else span(style = "color:#ccc;", "—")
                  ),
                  tags$td(make_flag_btn("In Draw", "No Draw", "toggle_post_in_draw",
                                        r$id, in_draw, "btn-success", "btn-outline-secondary")),
                  tags$td(make_flag_btn("Voluntary", "Not Vol.", "toggle_post_voluntary",
                                        r$id, is_vol, "btn-info", "btn-outline-secondary")),
                  tags$td(make_flag_btn("Active", "Inactive", "toggle_post_active",
                                        r$id, is_act, "btn-success", "btn-outline-secondary"))
                )
              }))
            )
          )
        } else {
          div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;",
              "No job posts for this round.")
        },

        if (!is.na(rid)) {
          all_cat_choices <- if (nrow(all_cats))
            setNames(all_cats$id, all_cats$name)
          else c("(add categories first)" = "")
          tags$details(
            tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                         "Add job post"),
            div(style = "padding:.5rem 0;",
              fluidRow(
                column(3, textInput("new_post_name", "Post name:")),
                column(2, selectInput("new_post_cat", "Category:", choices = all_cat_choices)),
                column(1, numericInput("new_post_slots", "Slots:", value = 1L, min = 1L, step = 1L)),
                column(2, numericInput("new_post_wage", "Wage:", value = NA, min = 0, step = 1)),
                column(2, tags$br(),
                       checkboxInput("new_post_in_draw", "In draw", value = TRUE),
                       checkboxInput("new_post_voluntary", "Voluntary", value = FALSE)),
                column(2, tags$br(),
                       actionButton("add_job_post_btn", "Add", class = "btn btn-sm btn-primary"))
              )
            )
          )
        },

        # ── Job Categories ────────────────────────────────────────────────────────
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Job Categories"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Categories set the default wage for posts. Edit name, wage, or description inline."),
        if (nrow(all_cats)) {
          tagList(lapply(seq_len(nrow(all_cats)), function(i) {
            r <- all_cats[i, ]
            cid_js <- as.integer(r$id)
            div(style = paste0("border:1px solid #e8e8e8;border-radius:6px;padding:.4rem .7rem;",
                               "margin-bottom:.35rem;background:#fafafa;"),
              tags$details(
                tags$summary(style = "cursor:pointer;",
                  span(style = "font-weight:600;", r$name %||% ""),
                  span(style = "color:#888;font-size:.82em;margin-left:.5rem;",
                       sprintf("%g tokens default", as.numeric(r$default_wage %||% 0))),
                  if (nzchar(r$description %||% ""))
                    span(style = "color:#aaa;font-size:.8em;margin-left:.4rem;",
                         r$description)
                ),
                div(style = "padding:.4rem 0;",
                  fluidRow(
                    column(3, textInput(paste0("edit_cat_name_",  cid_js), "Name:",
                                        value = r$name %||% "")),
                    column(2, numericInput(paste0("edit_cat_wage_",  cid_js), "Default wage:",
                                           value = as.numeric(r$default_wage %||% 0),
                                           min = 0, step = 1)),
                    column(4, textInput(paste0("edit_cat_desc_",  cid_js), "Description:",
                                        value = r$description %||% "")),
                    column(3, tags$br(),
                      tags$button(
                        class = "btn btn-sm btn-primary",
                        onclick = sprintf(paste0(
                          "var n=document.getElementById('edit_cat_name_%d').value;",
                          "var w=document.getElementById('edit_cat_wage_%d').value;",
                          "var d=document.getElementById('edit_cat_desc_%d').value;",
                          "Shiny.setInputValue('edit_cat_btn',{id:%d,name:n,wage:w,desc:d},{priority:'event'});"),
                          cid_js, cid_js, cid_js, cid_js),
                        "Save changes"))
                  )
                )
              )
            )
          }))
        } else div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No categories yet."),

        tags$details(
          tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                       "Add category"),
          div(style = "padding:.5rem 0;",
            fluidRow(
              column(3, textInput("new_cat_name", "Name:")),
              column(2, numericInput("new_cat_wage", "Default wage:", value = 10, min = 0, step = 1)),
              column(4, textInput("new_cat_desc", "Description (optional):")),
              column(3, tags$br(),
                     actionButton("add_job_cat_btn", "Add", class = "btn btn-sm btn-primary"))
            )
          )
        ),

        # ── Templates ─────────────────────────────────────────────────────────────
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Templates"),
        tags$p(style = "color:#555;font-size:.85rem;",
               'Active templates are copied as job posts when you click "Create next round".'),
        if (nrow(templates)) {
          tags$table(class = "table table-sm",
            tags$thead(tags$tr(
              tags$th("Name"), tags$th("Category"), tags$th("Slots"),
              tags$th("Suggested Wage"), tags$th("")
            )),
            tags$tbody(lapply(seq_len(nrow(templates)), function(i) {
              r <- templates[i, ]
              tags$tr(
                tags$td(r$name %||% ""),
                tags$td(style = "color:#888;", r$cat_name %||% "—"),
                tags$td(r$slots %||% 1),
                tags$td(if (!is.na(r$suggested_wage %||% NA))
                            sprintf("%g", as.numeric(r$suggested_wage)) else "—"),
                tags$td(
                  tags$button(
                    class = "btn btn-xs btn-outline-danger",
                    style = "padding:.1rem .35rem;font-size:.72rem;",
                    onclick = sprintf(
                      "Shiny.setInputValue('remove_template_btn',%d,{priority:'event'});",
                      as.integer(r$id)),
                    "Remove"))
              )
            }))
          )
        } else {
          div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No templates yet.")
        },

        {
          tpl_cat_choices <- if (nrow(all_cats))
            setNames(all_cats$id, all_cats$name)
          else c("(no categories)" = "")
          tags$details(
            tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                         "Add template"),
            div(style = "padding:.5rem 0;",
              fluidRow(
                column(3, textInput("new_tpl_name", "Name:")),
                column(3, selectInput("new_tpl_cat", "Category:", choices = tpl_cat_choices)),
                column(2, numericInput("new_tpl_slots", "Slots:", value = 1L, min = 1L, step = 1L)),
                column(2, numericInput("new_tpl_wage", "Suggested wage:", value = NA, min = 0, step = 1)),
                column(2, tags$br(),
                       actionButton("add_template_btn", "Add", class = "btn btn-sm btn-primary"))
              )
            )
          )
        }
      )

    } else if (act == "round_setup") {
      round <- tryCatch(db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
      mode_choices <- c("Random"              = "random",
                        "Wage Bidding"         = "wage_bidding",
                        "Application Bidding"  = "application_bidding")
      tagList(
        if (nrow(round)) {
          r <- round[1, ]
          tagList(
            tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Current Round"),
            div(style = "background:#f8f8f8;border-radius:6px;padding:.7rem 1rem;margin-bottom:.75rem;",
              tags$strong(r$label %||% paste("Round", r$id)),
              tags$span(style = "color:#888;font-size:.85em;margin-left:.5rem;",
                switch(r$assignment_mode %||% "random",
                  random              = "Random assignment",
                  wage_bidding        = "Wage bidding",
                  application_bidding = "Application bidding",
                  r$assignment_mode)),
              if (!is.na(r$bid_open_date %||% NA) || !is.na(r$bid_close_date %||% NA))
                div(style = "font-size:.82em;color:#888;margin-top:.2rem;",
                    sprintf("Bid window: %s – %s",
                            r$bid_open_date %||% "?", r$bid_close_date %||% "?"))
            ),
            tags$details(
              tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                           "Edit current round"),
              div(style = "padding:.5rem 0;",
                textInput("edit_round_label", "Label:", value = r$label %||% ""),
                selectInput("edit_round_mode", "Assignment mode:", choices = mode_choices,
                            selected = r$assignment_mode %||% "random"),
                fluidRow(
                  column(4, dateInput("edit_round_open",  "Bid opens:",
                                      value = tryCatch(as.Date(r$bid_open_date), error = function(e) NA))),
                  column(4, dateInput("edit_round_close", "Bid closes:",
                                      value = tryCatch(as.Date(r$bid_close_date), error = function(e) NA))),
                  column(4, numericInput("edit_round_tix", "Tickets/student:",
                                         value = as.integer(r$tickets_per_student %||% 10L),
                                         min = 1, step = 1))
                ),
                actionButton("update_round_btn", "Update round", class = "btn btn-sm btn-primary")
              )
            )
          )
        } else {
          div(style = "color:#999;font-size:.9em;margin-top:.5rem;", "No rounds yet.")
        },
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Create New Round"),
        textInput("new_round_label", "Label (e.g. Week 3):"),
        selectInput("new_round_mode", "Assignment mode:", choices = mode_choices),
        fluidRow(
          column(4, dateInput("new_round_open",  "Bid opens:")),
          column(4, dateInput("new_round_close", "Bid closes:")),
          column(4, numericInput("new_round_tix", "Tickets/student:", value = 10L, min = 1, step = 1))
        ),
        actionButton("create_round_btn", "Create round", class = "btn btn-sm btn-primary"),
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Auto-Create Next Round"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Increments the week label and copies all active templates as job posts."),
        actionButton("create_next_round_btn", "Create next round",
                     class = "btn btn-sm btn-success")
      )

    } else if (act == "students") {
      students <- tryCatch(db_query(
        "SELECT user_id, display_name, section,
                COALESCE(active,1) AS active, COALESCE(is_admin,0) AS is_admin
         FROM users WHERE COALESCE(is_demo,0)=0 AND COALESCE(active,1)=1
         ORDER BY section, display_name;"),
        error = function(e) data.frame())
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Student Roster"),
        if (nrow(students)) {
          tags$table(class = "table table-sm",
            tags$thead(tags$tr(
              tags$th("Name"), tags$th("Username"), tags$th("Section"), tags$th("Actions")
            )),
            tags$tbody(Filter(Negate(is.null), lapply(seq_len(nrow(students)), function(i) {
              r        <- students[i, ]
              is_adm   <- isTRUE(as.integer(r$is_admin %||% 0L) == 1L)
              is_act   <- isTRUE(as.integer(r$active  %||% 1L) == 1L)
              if (is_adm) return(NULL)
              tags$tr(
                style = if (!is_act) "color:#aaa;" else "",
                tags$td(r$display_name %||% r$user_id,
                        if (!is_act) tags$small(style = "color:#ccc;margin-left:.3rem;", "(archived)")),
                tags$td(style = "color:#888;font-size:.85em;", r$user_id),
                tags$td(style = "color:#888;font-size:.85em;", r$section %||% ""),
                tags$td(
                  if (is_act) {
                    tagList(
                      tags$button(
                        onclick = sprintf(
                          "Shiny.setInputValue('impersonate_uid','%s',{priority:'event'});",
                          r$user_id),
                        class = "btn btn-xs btn-outline-primary",
                        style = "padding:.1rem .35rem;font-size:.72rem;margin-right:.2rem;",
                        "View as"),
                      tags$button(
                        onclick = sprintf(
                          "Shiny.setInputValue('archive_uid','%s',{priority:'event'});",
                          r$user_id),
                        class = "btn btn-xs btn-outline-warning",
                        style = "padding:.1rem .35rem;font-size:.72rem;",
                        "Archive")
                    )
                  } else {
                    tags$button(
                      onclick = sprintf(
                        "Shiny.setInputValue('restore_uid','%s',{priority:'event'});",
                        r$user_id),
                      class = "btn btn-xs btn-outline-secondary",
                      style = "padding:.1rem .35rem;font-size:.72rem;",
                      "Restore")
                  }
                )
              )
            })))
          )
        } else div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No students."),
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Add Student"),
        fluidRow(
          column(3, textInput("new_stu_uid", "Username:")),
          column(3, textInput("new_stu_name", "Display name:")),
          column(2, textInput("new_stu_section", "Section:")),
          column(3, passwordInput("new_stu_pw", "Password:")),
          column(1, tags$br(),
                 actionButton("create_student_btn", "Add", class = "btn btn-sm btn-primary"))
        ),
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Reset Password"),
        fluidRow(
          column(4, textInput("reset_pw_uid", "Username:")),
          column(4, passwordInput("reset_pw_new", "New password:")),
          column(4, tags$br(),
                 actionButton("reset_pw_btn", "Reset", class = "btn btn-sm btn-warning"))
        )
      )

    } else if (act == "exports") {
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Export Data"),
        tags$p(style = "color:#555;font-size:.88rem;", "Download records as CSV files."),
        div(style = "display:flex;flex-wrap:wrap;gap:.5rem;margin-top:.5rem;",
          downloadButton("dl_assignments",          "Assignments",          class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_wage_bids",            "Wage Bids",            class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_app_bids",             "Application Bids",     class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_tokens",               "Token Ledger",         class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_participation_events", "Participation Events", class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_extensions",           "Extension Purchases",  class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_reweight_requests",    "Reweight Requests",    class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_pubgood_contribs",     "Public Good Contribs", class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_flex_purchases",       "Flex Q Purchases",     class = "btn btn-sm btn-outline-secondary"),
          downloadButton("dl_students",             "Students",             class = "btn btn-sm btn-outline-secondary")
        )
      )

    } else if (act == "grade_reweighting") {
      cats_df <- tryCatch(parse_grade_categories(),
                          error = function(e)
                            data.frame(name=character(0), weight=numeric(0)))
      current_costs <- tryCatch(get_setting("reweight_cost_schedule", "1:2,2:5,3:9,4:14,5:20"),
                                error = function(e) "1:2,2:5,3:9,4:14,5:20")
      weight_sum <- if (nrow(cats_df)) sum(as.numeric(cats_df$weight), na.rm = TRUE) else 0
      sum_ok     <- abs(weight_sum - 100) < 0.5

      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;",
                "Grade Categories"),
        tags$p(style = "color:#555;font-size:.88rem;",
               "Define the categories students can reweight. Weights should sum to 100."),

        div(style = sprintf(
          "padding:.35rem .7rem;border-radius:6px;font-size:.85rem;margin-bottom:.6rem;%s",
          if (sum_ok)
            "background:#d4edda;border:1px solid #c3e6cb;color:#155724;"
          else
            "background:#fff3cd;border:1px solid #ffc107;color:#856404;"),
          sprintf("Weight total: %.0f%% %s", weight_sum,
                  if (sum_ok) "✓ sums to 100" else "(should sum to 100)")),

        if (nrow(cats_df)) {
          tags$table(class = "table table-sm",
            tags$thead(tags$tr(
              tags$th("Category"), tags$th("Weight (%)"), tags$th("Remove")
            )),
            tags$tbody(lapply(seq_len(nrow(cats_df)), function(i) {
              r <- cats_df[i, ]
              tags$tr(
                tags$td(r$name),
                tags$td(sprintf("%.0f%%", as.numeric(r$weight %||% 0))),
                tags$td(
                  tags$button(
                    onclick = sprintf(
                      "Shiny.setInputValue('delete_grade_cat','%s',{priority:'event'});",
                      gsub("'", "\\'", r$name, fixed = TRUE)),
                    class = "btn btn-xs btn-outline-danger",
                    style = "padding:.1rem .35rem;font-size:.72rem;",
                    "Remove")
                )
              )
            }))
          )
        } else {
          div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No categories defined yet.")
        },

        tags$details(
          tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                       "Add category"),
          div(style = "padding:.5rem 0;",
            fluidRow(
              column(5, textInput("new_grade_cat_name", "Category name:",
                                  placeholder = "e.g. Quizzes")),
              column(4, numericInput("new_grade_cat_weight", "Weight (%):",
                                     value = 0, min = 0, max = 100, step = 1)),
              column(3, tags$br(),
                     actionButton("add_grade_cat_btn", "Add",
                                  class = "btn btn-sm btn-primary"))
            )
          )
        ),

        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Cost Schedule"),
        textInput("rw_costs_input", "Cost schedule (points:tokens, comma-separated):",
                  value = current_costs, width = "100%"),
        tags$p(style = "color:#888;font-size:.82em;margin-top:-.3rem;",
               "e.g. 1:2,2:5,3:9 means moving 1 pt costs 2 tokens, 2 pts costs 5, etc."),
        actionButton("save_rw_setup_btn", "Save cost schedule",
                     class = "btn btn-sm btn-primary"),
        tags$hr(),
        div(class = "sec-label", "Pending Requests"),
        uiOutput("rw_requests_panel")
      )

    } else if (act == "extensions") {
      ps   <- tryCatch(db_query("SELECT * FROM problem_sets ORDER BY original_deadline DESC LIMIT 20;"),
                       error = function(e) data.frame())
      opts <- tryCatch(db_query("SELECT * FROM extension_options ORDER BY hours DESC;"),
                       error = function(e) data.frame())
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;",
                "Extension Options"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Define the lengths students can purchase. Label is shown to students; Hours is recorded; Tokens is the cost."),
        if (nrow(opts)) {
          tags$table(class = "table table-sm",
            tags$thead(tags$tr(
              tags$th("Label"), tags$th("Hours"), tags$th("Tokens"), tags$th("Active"), tags$th("")
            )),
            tags$tbody(lapply(seq_len(nrow(opts)), function(i) {
              r <- opts[i, ]
              is_active <- isTRUE(as.integer(r$active %||% 1L) == 1L)
              tags$tr(
                tags$td(r$label %||% ""),
                tags$td(sprintf("%g", as.numeric(r$hours))),
                tags$td(sprintf("%g", as.numeric(r$tokens))),
                tags$td(if (is_active) "✓" else ""),
                tags$td(
                  tags$button(
                    class = "btn btn-xs btn-outline-danger",
                    style = "padding:.1rem .35rem;font-size:.72rem;",
                    onclick = sprintf(
                      "Shiny.setInputValue('delete_ext_option_btn',%d,{priority:'event'});",
                      as.integer(r$id)),
                    "Remove"))
              )
            }))
          )
        } else div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No extension options yet."),
        tags$details(
          tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                       "Add option"),
          div(style = "padding:.5rem 0;",
            fluidRow(
              column(3, textInput("new_ext_label", "Label:", placeholder = "e.g. 24-hour")),
              column(2, numericInput("new_ext_hours", "Hours:", value = 24, min = 0.5, step = 0.5)),
              column(2, numericInput("new_ext_tokens", "Token cost:", value = 3, min = 1, step = 1)),
              column(3, tags$br(),
                     actionButton("add_ext_option_btn", "Add", class = "btn btn-sm btn-primary"))
            )
          )
        ),
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Problem Sets"),
        div(style = "margin-top:.25rem;",
          if (nrow(ps)) {
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(tags$th("Name"), tags$th("Deadline"), tags$th("Active"))),
              tags$tbody(lapply(seq_len(nrow(ps)), function(i) {
                r <- ps[i, ]
                tags$tr(tags$td(r$name), tags$td(r$original_deadline %||% ""),
                        tags$td(if (isTRUE(as.integer(r$active %||% 1L) == 1L)) "✓" else ""))
              }))
            )
          } else div(style = "color:#999;", "No problem sets yet.")
        ),
        tags$h6(style = "margin-top:.75rem;", "Add Problem Set"),
        fluidRow(
          column(5, textInput("new_ps_name", "Name:")),
          column(4, dateInput("new_ps_deadline", "Original deadline:")),
          column(3, tags$br(),
                 actionButton("add_ps_btn", "Add", class = "btn btn-sm btn-primary"))
        )
      )

    } else if (act == "flex_questions") {
      fqs <- tryCatch(db_query(
        "SELECT id, question_text, order_index, active FROM flex_questions ORDER BY order_index ASC, id ASC;"),
        error = function(e) data.frame())
      cur_schedule <- tryCatch(get_setting("flex_cost_schedule", "2,4,6,8,10"),
                               error = function(e) "2,4,6,8,10")
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Flex Questions"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Students unlock questions in order by spending tokens. Questions are shown one at a time."),

        # Price schedule
        tags$h6(style = "font-weight:700;margin-top:.75rem;", "Price Schedule"),
        textInput("flex_cost_input", NULL, value = cur_schedule, width = "100%",
                  placeholder = "e.g. 2,4,6,8,10"),
        tags$p(style = "color:#888;font-size:.82em;margin-top:-.4rem;",
               "Comma-separated costs per question in order. Last value repeats for additional questions."),
        actionButton("save_flex_cost_btn", "Save schedule", class = "btn btn-sm btn-primary"),
        tags$hr(),

        # Question table
        tags$h6(style = "font-weight:700;", "Questions"),
        if (nrow(fqs)) {
          tags$table(class = "table table-sm",
            tags$thead(tags$tr(
              tags$th("#"), tags$th("Question"), tags$th("Active"), tags$th("")
            )),
            tags$tbody(lapply(seq_len(nrow(fqs)), function(i) {
              r <- fqs[i, ]
              is_active <- isTRUE(as.integer(r$active %||% 1L) == 1L)
              tags$tr(
                tags$td(style = "color:#888;width:2rem;", i),
                tags$td(style = "font-size:.85rem;max-width:26rem;word-break:break-word;",
                        r$question_text %||% ""),
                tags$td(if (is_active) "✓" else ""),
                tags$td(
                  tags$button(
                    class = "btn btn-xs btn-outline-danger",
                    style = "padding:.1rem .35rem;font-size:.72rem;",
                    onclick = sprintf(
                      "Shiny.setInputValue('delete_flex_question_btn',%d,{priority:'event'});",
                      as.integer(r$id)),
                    "Remove"))
              )
            }))
          )
        } else div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No questions yet."),

        # Manual add
        tags$details(
          tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                       "Add question manually"),
          div(style = "padding:.5rem 0;",
            textAreaInput("new_fq_text", "Question text:", rows = 3, width = "100%"),
            actionButton("add_flex_question_btn", "Add question",
                         class = "btn btn-sm btn-primary")
          )
        ),

        # Upload
        tags$hr(),
        tags$h6(style = "font-weight:700;", "Upload Questions"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Upload a plain-text file (one question per non-empty line) or a CSV with a 'question_text' column."),
        fileInput("upload_flex_questions", NULL,
                  accept = c(".txt", ".md", ".csv", ".yaml", ".yml"),
                  buttonLabel = "Choose file", placeholder = "No file chosen"),
        checkboxInput("fq_replace_all", "Replace all existing questions", value = FALSE),
        actionButton("upload_flex_questions_btn", "Upload", class = "btn btn-sm btn-primary")
      )

    } else if (act == "game_controls") {
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
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Active Game Slot"),
        selectInput("admin_game_sel", "Which game is active now?",
                    choices = all_game_choices, selected = active, width = "100%"),
        actionButton("set_active_btn", "Set active game", class = "btn btn-warning btn-sm"),
        tags$p(style = "font-size:.82em;color:#888;margin-top:.5rem;",
               "Students see this immediately on their Games tab."),
        tags$hr(),
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
      )

    } else if (act == "token_admin") {
      students <- tryCatch(db_query(
        "SELECT user_id, display_name, section
         FROM users
         WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0
         ORDER BY section, display_name;"),
        error = function(e) data.frame())
      sections <- c("All", sort(unique(Filter(nzchar, students$section %||% character(0)))))
      stu_lbl  <- if (nrow(students)) {
        sec_lbl <- students$section %||% ""
        ifelse(nzchar(sec_lbl),
               paste0(students$display_name," (",sec_lbl,")"),
               students$display_name %||% students$user_id)
      } else character(0)
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Bulk Token Award / Deduct"),
        tags$p(style = "color:#555;font-size:.88rem;",
               "Award or deduct tokens from all students or a specific section at once."),
        div(class = "spend-form-box",
          fluidRow(
            column(3, selectInput("bulk_section", "Apply to:", choices = sections)),
            column(2, numericInput("bulk_amount", "Amount (+/-):", value = 1, step = 1)),
            column(5, textInput("bulk_note", "Note:", placeholder = "e.g. class participation")),
            column(2, tags$br(),
                   actionButton("bulk_award_btn", "Apply", class = "btn btn-warning btn-sm"))
          ),
          tags$p(style = "color:#888;font-size:.78rem;margin:.3rem 0 0;",
                 "Positive = award; negative = deduct. Applied to every active non-admin student.")
        ),
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Individual Adjustment"),
        div(class = "spend-form-box",
          if (!nrow(students)) {
            tags$p(style = "color:#999;", "No students found.")
          } else {
            fluidRow(
              column(4, selectInput("indiv_uid", "Student:",
                                    choices = setNames(students$user_id, stu_lbl))),
              column(2, numericInput("indiv_amount", "Amount (+/-):", value = 1, step = 1)),
              column(4, textInput("indiv_note", "Note:", placeholder = "")),
              column(2, tags$br(),
                     actionButton("indiv_award_btn", "Apply", class = "btn btn-warning btn-sm"))
            )
          }
        )
      )

    } else if (act == "participation_events") {
      current_hwm <- tryCatch(as.numeric(get_setting("half_wage_multiplier","0.5")),
                              error = function(e) 0.5)
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Job Wage Settings"),
        numericInput("half_wage_input", "Half-wage multiplier (for Tried outcome):",
                     value = current_hwm, min = 0, max = 1, step = 0.05, width = "280px"),
        tags$p(style = "color:#888;font-size:.78rem;margin-top:-.3rem;",
               "0.5 = 50% of assigned wage for Tried outcome. Must be 0–1."),
        actionButton("save_hwm_btn", "Save multiplier", class = "btn btn-sm btn-primary")
      )

    } else if (act == "app_settings") {
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "App Settings"),
        textInput("new_app_name", "App name:",
                  value = get_config("app_name", "Classroom Economy"), width = "100%"),
        actionButton("save_app_name_btn", "Save", class = "btn btn-sm btn-primary")
      )
    }
  })

  output$rw_requests_panel <- renderUI({
    req(rv$is_admin)
    rows <- tryCatch(db_query(
      "SELECT r.id, u.display_name, r.from_category, r.to_category, r.points,
              r.cost, r.status, r.created_at
       FROM grade_reweight_requests r
       LEFT JOIN users u ON u.user_id=r.user_id
       ORDER BY r.created_at DESC LIMIT 30;"),
      error = function(e) data.frame())
    if (!nrow(rows))
      return(div(style = "color:#999;font-size:.88rem;", "No requests yet."))
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(
        tags$th("Student"), tags$th("From"), tags$th("To"), tags$th("Pts"),
        tags$th("Cost"), tags$th("Status"), tags$th("Date")
      )),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        tags$tr(
          tags$td(r$display_name %||% ""),
          tags$td(r$from_category %||% ""),
          tags$td(r$to_category %||% ""),
          tags$td(r$points %||% ""),
          tags$td(r$cost %||% ""),
          tags$td(style = if (identical(r$status, "pending")) "color:#856404;" else "color:#1a6e3c;",
                  r$status %||% ""),
          tags$td(style = "color:#888;font-size:.82em;",
                  tryCatch(format(as.POSIXct(r$created_at), "%b %d"), error = function(e) ""))
        )
      }))
    )
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
  output$dl_students <- downloadHandler(
    filename = function() paste0("students_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT user_id, display_name, section, COALESCE(active,1) AS active
       FROM users WHERE COALESCE(is_admin,0)=0 AND COALESCE(is_demo,0)=0
       ORDER BY section, display_name;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )
  output$dl_participation_events <- downloadHandler(
    filename = function() paste0("participation_events_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT pe.id, wr.label AS round, u.display_name AS student, pe.event_type,
              pe.tokens, pe.note, pe.logged_by, pe.created_at
       FROM participation_events pe
       LEFT JOIN users u ON u.user_id=pe.user_id
       LEFT JOIN weekly_rounds wr ON wr.id=pe.round_id
       ORDER BY pe.created_at DESC;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )
  output$dl_extensions <- downloadHandler(
    filename = function() paste0("extensions_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT ep.id, ps.name AS problem_set, u.display_name AS student,
              ep.hours, ep.cost, ep.purchased_at
       FROM extension_purchases ep
       LEFT JOIN users u ON u.user_id=ep.user_id
       LEFT JOIN problem_sets ps ON ps.id=ep.problem_set_id
       ORDER BY ep.purchased_at DESC;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )
  output$dl_reweight_requests <- downloadHandler(
    filename = function() paste0("reweight_requests_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT r.id, u.display_name AS student, r.from_category, r.to_category,
              r.points, r.cost, r.status, r.created_at
       FROM grade_reweight_requests r
       LEFT JOIN users u ON u.user_id=r.user_id
       ORDER BY r.created_at DESC;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )
  output$dl_pubgood_contribs <- downloadHandler(
    filename = function() paste0("pubgood_contributions_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT pgc.id, pg.name AS public_good, u.display_name AS student,
              pgc.amount, pgc.contributed_at
       FROM public_good_contributions pgc
       LEFT JOIN users u ON u.user_id=pgc.user_id
       LEFT JOIN public_goods pg ON pg.id=pgc.public_good_id
       ORDER BY pgc.contributed_at DESC;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )
  output$dl_flex_purchases <- downloadHandler(
    filename = function() paste0("flex_purchases_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT fp.id, u.display_name AS student, fq.order_index AS question_num,
              fq.question_text, fp.tokens_spent, fp.purchased_at
       FROM flex_purchases fp
       LEFT JOIN users u ON u.user_id=fp.user_id
       LEFT JOIN flex_questions fq ON fq.id=fp.question_id
       ORDER BY fp.purchased_at DESC;"), error = function(e) data.frame()),
      file, row.names = FALSE)
  )
  output$dl_app_bids <- downloadHandler(
    filename = function() paste0("application_bids_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(tryCatch(db_query(
      "SELECT ab.id, wr.label AS round, jc.name AS category, u.display_name AS student,
              ab.tickets, ab.submitted_at
       FROM application_bids ab
       LEFT JOIN users u ON u.user_id=ab.user_id
       LEFT JOIN job_categories jc ON jc.id=ab.category_id
       LEFT JOIN weekly_rounds wr ON wr.id=ab.round_id
       ORDER BY wr.id DESC, u.display_name;"), error = function(e) data.frame()),
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

  # ── Job Evaluation ────────────────────────────────────────────────────────────
  observeEvent(input$eval_outcome, {
    req(rv$is_admin, !rv$impersonating)
    ev <- input$eval_outcome
    if (is.null(ev) || is.null(ev$id) || is.null(ev$outcome)) return()
    assign_id <- suppressWarnings(as.integer(ev$id))
    outcome   <- as.character(ev$outcome)
    if (is.na(assign_id) || !outcome %in% c("complete","tried","missed")) {
      showNotification("Invalid evaluation.", type = "error"); return()
    }
    row <- db_query(
      "SELECT ja.user_id, u.display_name, ja.assigned_wage,
              COALESCE(ja.tokens_awarded,0) AS tokens_awarded
       FROM job_assignments ja
       JOIN users u ON u.user_id=ja.user_id
       WHERE ja.id=?;", list(assign_id))
    if (!nrow(row)) { showNotification("Assignment not found.", type = "error"); return() }
    if (as.integer(row$tokens_awarded[1]) == 1L) {
      showNotification("Tokens already awarded — outcome cannot be changed.", type = "warning")
      return()
    }
    uid   <- row$user_id[1]
    dname <- row$display_name[1] %||% uid
    wage  <- if (!is.na(row$assigned_wage[1] %||% NA)) as.numeric(row$assigned_wage[1]) else 0
    half_mult <- tryCatch(as.numeric(get_setting("half_wage_multiplier","0.5")), error=function(e) 0.5)
    tokens_to_award <- switch(outcome,
      complete = wage,
      tried    = round(wage * half_mult),
      missed   = 0,
      0)
    db_exec(
      "UPDATE job_assignments SET outcome=?, tokens_awarded=1, updated_at=datetime('now') WHERE id=?;",
      list(outcome, assign_id))
    if (tokens_to_award > 0) {
      token_credit(uid, dname, tokens_to_award, 1L, "job", assign_id,
                   note = sprintf("Job wage (%s)", outcome))
      showNotification(
        sprintf("%s — awarded %d token%s to %s.",
                switch(outcome, complete="Complete", tried="Tried", outcome),
                as.integer(tokens_to_award), if (tokens_to_award == 1) "" else "s", dname),
        type = "message")
    } else {
      showNotification(sprintf("Missed — no tokens for %s.", dname), type = "warning")
    }
  }, ignoreNULL = TRUE)

  # ── Voluntary Participation logging ───────────────────────────────────────────
  .log_participation <- function(outcome_type) {
    req(rv$is_admin, !rv$impersonating)
    uid     <- trimws(input$part_student_sel %||% "")
    post_id <- suppressWarnings(as.integer(input$part_event_type %||% 0))
    if (!nzchar(uid) || is.na(post_id) || post_id <= 0) {
      showNotification("Select a student and event type.", type = "error"); return()
    }
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
    if (!nrow(rid_row)) {
      showNotification("No active round.", type = "error"); return()
    }
    rid   <- rid_row$id[1]
    u_row <- tryCatch(db_query("SELECT display_name FROM users WHERE user_id=?;", list(uid)),
                      error = function(e) data.frame())
    dname <- if (nrow(u_row)) u_row$display_name[1] %||% uid else uid
    post_row <- tryCatch(db_query(
      "SELECT jp.id, COALESCE(jp.wage_override, jc.default_wage, 1) AS tokens
       FROM job_posts jp
       LEFT JOIN job_categories jc ON jc.id = jp.category_id
       WHERE jp.id=? AND jp.round_id=? AND COALESCE(jp.voluntary,0)=1
       LIMIT 1;", list(post_id, rid)),
      error = function(e) data.frame())
    if (!nrow(post_row)) {
      showNotification("No voluntary post found.", type = "error")
      return()
    }
    post_id   <- as.integer(post_row$id[1])
    wage_val  <- as.numeric(post_row$tokens[1] %||% 0)
    half_mult <- tryCatch(as.numeric(get_setting("half_wage_multiplier","0.5")),
                          error=function(e) 0.5)
    tokens_to_award <- switch(outcome_type,
      succeed = wage_val,
      try     = round(wage_val * half_mult),
      miss    = 0, 0)
    db_exec(
      "INSERT INTO job_assignments(round_id, user_id, job_post_id, assigned_wage,
              assignment_mode, outcome, tokens_awarded, updated_at)
       VALUES(?,?,?,?,'voluntary',?,?,datetime('now'))
       ON CONFLICT(round_id, user_id)
       DO UPDATE SET job_post_id=excluded.job_post_id,
                     assigned_wage=excluded.assigned_wage,
                     outcome=excluded.outcome,
                     tokens_awarded=excluded.tokens_awarded,
                     updated_at=excluded.updated_at;",
      list(rid, uid, post_id, wage_val, outcome_type,
           if (tokens_to_award > 0) 1L else 0L))
    if (tokens_to_award > 0)
      token_credit(uid, dname, tokens_to_award, 1L, "participation", post_id,
                   note = sprintf("Participation (%s)", outcome_type))
    showNotification(
      sprintf("%s — %s%s", dname, outcome_type,
              if (tokens_to_award > 0)
                sprintf(" (+%d token%s)", as.integer(tokens_to_award),
                        if (tokens_to_award == 1) "" else "s")
              else " (no tokens)"),
      type = "message")
  }

  observeEvent(input$log_succeed_btn, .log_participation("succeed"), ignoreNULL = TRUE)
  observeEvent(input$log_try_btn,     .log_participation("try"),     ignoreNULL = TRUE)
  observeEvent(input$log_miss_btn,    .log_participation("miss"),    ignoreNULL = TRUE)

  # ── Token Admin ───────────────────────────────────────────────────────────────
  observeEvent(input$bulk_award_btn, {
    req(rv$is_admin, !rv$impersonating)
    section <- input$bulk_section %||% "All"
    amount  <- suppressWarnings(as.numeric(input$bulk_amount %||% 0))
    note    <- trimws(input$bulk_note %||% "")
    if (is.na(amount) || amount == 0) {
      showNotification("Enter a non-zero amount.", type = "error"); return()
    }
    targets <- if (identical(section, "All")) {
      tryCatch(db_query(
        "SELECT user_id, display_name FROM users
         WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0;"),
        error = function(e) data.frame())
    } else {
      tryCatch(db_query(
        "SELECT user_id, display_name FROM users
         WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0
         AND section=?;", list(section)),
        error = function(e) data.frame())
    }
    if (!nrow(targets)) {
      showNotification("No matching students found.", type = "error"); return()
    }
    earning <- if (amount > 0) 1L else 0L
    lbl     <- if (nzchar(note)) note else sprintf("Bulk award (section: %s)", section)
    for (i in seq_len(nrow(targets)))
      token_credit(targets$user_id[i], targets$display_name[i] %||% targets$user_id[i],
                   amount, earning, "bulk_award", note = lbl)
    showNotification(
      sprintf("%s %d token%s to %d student%s.",
              if (amount > 0) "Awarded" else "Deducted",
              abs(as.integer(amount)), if (abs(amount) == 1) "" else "s",
              nrow(targets), if (nrow(targets) == 1) "" else "s"),
      type = "message")
  })

  observeEvent(input$indiv_award_btn, {
    req(rv$is_admin, !rv$impersonating)
    uid    <- trimws(input$indiv_uid %||% "")
    amount <- suppressWarnings(as.numeric(input$indiv_amount %||% 0))
    note   <- trimws(input$indiv_note %||% "")
    if (!nzchar(uid) || is.na(amount) || amount == 0) {
      showNotification("Select a student and enter a non-zero amount.", type = "error"); return()
    }
    u_row  <- db_query("SELECT display_name FROM users WHERE user_id=?;", list(uid))
    dname  <- if (nrow(u_row)) u_row$display_name[1] %||% uid else uid
    earning <- if (amount > 0) 1L else 0L
    token_credit(uid, dname, amount, earning, "individual_adj",
                 note = if (nzchar(note)) note else "individual adjustment")
    showNotification(
      sprintf("%s %d token%s to %s.",
              if (amount > 0) "Awarded" else "Deducted",
              abs(as.integer(amount)), if (abs(amount) == 1) "" else "s", dname),
      type = "message")
  })

  # ── Participation event type + half-wage settings ─────────────────────────────
  observeEvent(input$save_hwm_btn, {
    req(rv$is_admin)
    hwm <- suppressWarnings(as.numeric(input$half_wage_input %||% 0.5))
    if (is.na(hwm) || hwm < 0 || hwm > 1) {
      showNotification("Multiplier must be between 0 and 1.", type = "error"); return()
    }
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('half_wage_multiplier',?);",
            list(as.character(hwm)))
    showNotification(sprintf("Half-wage multiplier set to %.2f.", hwm), type = "message")
  })

  # ── Job draw ──────────────────────────────────────────────────────────────────
  compute_draw_pairs <- function(rid, mode, posts, students) {
    tryCatch({
      if (mode == "wage_bidding") {
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
          pool <- if (nrow(cat_bids)) rep(cat_bids$user_id, cat_bids$tickets) else character(0)
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
  }

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
       WHERE jp.round_id=? AND COALESCE(jp.active,1)=1 AND COALESCE(jp.in_draw,1)=1;",
      list(rid)),
      error = function(e) data.frame())
    if (!nrow(posts)) { showNotification("No active job posts marked 'In Draw' for this round.", type = "error"); return() }

    sec_filter <- rv$active_section %||% ""
    students <- tryCatch(
      if (nzchar(sec_filter))
        db_query(
          "SELECT user_id, section FROM users
           WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1
             AND COALESCE(is_demo,0)=0 AND section=?
           ORDER BY RANDOM();", list(sec_filter))
      else
        db_query(
          "SELECT user_id, section FROM users
           WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0
           ORDER BY RANDOM();"),
      error = function(e) data.frame())
    if (!nrow(students)) { showNotification("No eligible students found.", type = "error"); return() }

    db_exec("DELETE FROM job_assignments WHERE round_id=?;", list(rid))

    pairs <- compute_draw_pairs(rid, mode, posts, students)
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
    rv$draw_preview <- NULL
    showNotification(sprintf("Drew %d assignments (hidden from students).", length(pairs)), type = "message")
  })

  observeEvent(input$preview_draw_btn, {
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
       WHERE jp.round_id=? AND COALESCE(jp.active,1)=1 AND COALESCE(jp.in_draw,1)=1;",
      list(rid)),
      error = function(e) data.frame())
    if (!nrow(posts)) {
      showNotification("No active job posts marked 'In Draw' for this round.", type = "error"); return()
    }
    sec_filter2 <- rv$active_section %||% ""
    students <- tryCatch(
      if (nzchar(sec_filter2))
        db_query(
          "SELECT user_id, section FROM users
           WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1
             AND COALESCE(is_demo,0)=0 AND section=?
           ORDER BY RANDOM();", list(sec_filter2))
      else
        db_query(
          "SELECT user_id, section FROM users
           WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0
           ORDER BY RANDOM();"),
      error = function(e) data.frame())
    if (!nrow(students)) {
      showNotification("No eligible students found.", type = "error"); return()
    }
    pairs <- compute_draw_pairs(rid, mode, posts, students)
    if (!length(pairs)) {
      showNotification("Preview produced no assignments.", type = "error")
      rv$draw_preview <- NULL
      return()
    }
    rv$draw_preview <- pairs
    showNotification(
      sprintf("Preview: %d assignments (not saved to database).", length(pairs)),
      type = "message")
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
