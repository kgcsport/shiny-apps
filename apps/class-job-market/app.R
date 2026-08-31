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
source(file.path(dirname(shared_sqlite), "demo_login.R"))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
nonempty_values <- function(x) {
  x <- as.character(x %||% character(0))
  x[!is.na(x) & nzchar(x)]
}

# ── Google OAuth config ───────────────────────────────────────────────────────
GOOGLE_CLIENT_ID     <- Sys.getenv("GOOGLE_CLIENT_ID", "")
GOOGLE_CLIENT_SECRET <- Sys.getenv("GOOGLE_CLIENT_SECRET", "")
SHINY_APP_URL        <- Sys.getenv("SHINY_APP_URL", "")  # e.g. https://shiny.kylecoombs.com/class-job-market/
GOOGLE_AUTH_ENABLED  <- nzchar(GOOGLE_CLIENT_ID) && nzchar(GOOGLE_CLIENT_SECRET) && nzchar(SHINY_APP_URL)

# ── Database ──────────────────────────────────────────────────────────────────
DB_PATH <- shared_db_path(demo = FALSE)

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
ensure_column <- function(table, column_def) {
  column_name <- strsplit(trimws(column_def), "\\s+")[[1]][1]
  cols <- tryCatch(db_query(sprintf("PRAGMA table_info(%s);", table))$name,
                   error = function(e) character(0))
  if (!column_name %in% cols) {
    try(db_exec(sprintf("ALTER TABLE %s ADD COLUMN %s;", table, column_def)),
        silent = TRUE)
  }
}

# ── Table init ────────────────────────────────────────────────────────────────
db_exec("CREATE TABLE IF NOT EXISTS users(
  user_id      TEXT PRIMARY KEY,
  display_name TEXT,
  pw_hash      TEXT,
  is_admin     INTEGER DEFAULT 0,
  section      TEXT,
  active       INTEGER DEFAULT 1,
  is_demo      INTEGER DEFAULT 0
);")

db_exec("
  CREATE TABLE IF NOT EXISTS arcade_state (
    id          INTEGER PRIMARY KEY CHECK (id = 1),
    active_game TEXT,
    updated_at  TEXT DEFAULT CURRENT_TIMESTAMP
  );
")
ensure_column("arcade_state", "assignments_revealed INTEGER DEFAULT 0")
if (!db_query("SELECT COUNT(*) n FROM arcade_state WHERE id=1;")$n[1])
  db_exec("INSERT INTO arcade_state(id, active_game, assignments_revealed) VALUES(1, NULL, 0);")

db_exec("
  CREATE TABLE IF NOT EXISTS arcade_sessions (
    token      TEXT PRIMARY KEY,
    user_id    TEXT NOT NULL,
    expires_at TEXT NOT NULL,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
  );
")
db_exec("DELETE FROM arcade_sessions WHERE expires_at < CURRENT_TIMESTAMP;")

if (GOOGLE_AUTH_ENABLED) {
  db_exec("CREATE TABLE IF NOT EXISTS oauth_states (
    state TEXT PRIMARY KEY,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
  );")
  db_exec("DELETE FROM oauth_states WHERE created_at < datetime('now','-1 hour');")
}

db_exec("
  CREATE TABLE IF NOT EXISTS arcade_config (
    key   TEXT PRIMARY KEY,
    value TEXT
  );
")
db_exec("INSERT OR IGNORE INTO arcade_config(key,value) VALUES('app_name','Classroom Economy');")

# Ensure these columns exist on the users table (other apps own it, but we add ours)
ensure_column("users", "pw_hash TEXT")
ensure_column("users", "section TEXT")
ensure_column("users", "active INTEGER DEFAULT 1")
ensure_column("users", "is_demo INTEGER DEFAULT 0")

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
try(db_exec("ALTER TABLE grade_reweight_requests ADD COLUMN level TEXT DEFAULT 'category';"), silent=TRUE)
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
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('grade_reweight_max_points','5');")

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
db_exec("CREATE TABLE IF NOT EXISTS live_score_events(
  id                INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id          INTEGER,
  user_id           TEXT,
  job_assignment_id INTEGER,
  job_post_id       INTEGER,
  event_kind        TEXT,
  outcome           TEXT,
  tokens            REAL,
  logged_by         TEXT,
  committed_at      TEXT,
  created_at        TEXT DEFAULT CURRENT_TIMESTAMP
);")
ensure_column("live_score_events", "round_id INTEGER")
ensure_column("live_score_events", "user_id TEXT")
ensure_column("live_score_events", "job_assignment_id INTEGER")
ensure_column("live_score_events", "job_post_id INTEGER")
ensure_column("live_score_events", "event_kind TEXT")
ensure_column("live_score_events", "outcome TEXT")
ensure_column("live_score_events", "tokens REAL")
ensure_column("live_score_events", "logged_by TEXT")
ensure_column("live_score_events", "committed_at TEXT")
ensure_column("live_score_events", "created_at TEXT")

db_exec("CREATE TABLE IF NOT EXISTS assignment_reveals(
  round_id   INTEGER,
  section    TEXT,
  revealed   INTEGER DEFAULT 0,
  timing     TEXT DEFAULT 'start',
  updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (round_id, section)
);")
try(db_exec("ALTER TABLE flex_questions ADD COLUMN exam_tag TEXT;"), silent = TRUE)
db_exec("CREATE TABLE IF NOT EXISTS gradebook_categories(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  weight REAL NOT NULL DEFAULT 0,
  item_count INTEGER NOT NULL DEFAULT 1,
  item_prefix TEXT,
  max_points REAL NOT NULL DEFAULT 100,
  source TEXT DEFAULT 'manual',
  display_order INTEGER DEFAULT 0
);")
db_exec("CREATE TABLE IF NOT EXISTS gradebook_item_names(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  category_id INTEGER NOT NULL,
  item_index INTEGER NOT NULL,
  item_name TEXT NOT NULL,
  item_weight REAL,
  UNIQUE(category_id, item_index)
);")
ensure_column("gradebook_item_names", "item_weight REAL")
db_exec("CREATE TABLE IF NOT EXISTS student_grades(
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id         TEXT NOT NULL,
  assignment_name TEXT NOT NULL,
  score           REAL,
  max_score       REAL,
  grade_pct       REAL,
  week_tag        TEXT,
  uploaded_at     TEXT DEFAULT CURRENT_TIMESTAMP
);")

# Job market tables (shared with class-job-market; CREATE IF NOT EXISTS is safe)
db_exec("CREATE TABLE IF NOT EXISTS job_categories(
  id            INTEGER PRIMARY KEY AUTOINCREMENT,
  name          TEXT NOT NULL,
  default_wage  REAL DEFAULT 10,
  description   TEXT,
  display_order INTEGER DEFAULT 99
);")
ensure_column("job_categories", "default_wage REAL DEFAULT 10")
ensure_column("job_categories", "description TEXT")
ensure_column("job_categories", "display_order INTEGER DEFAULT 99")
ensure_column("job_categories", "voluntary INTEGER DEFAULT 0")
ensure_column("job_categories", "in_draw INTEGER DEFAULT 1")
db_exec("CREATE TABLE IF NOT EXISTS weekly_rounds(
  id                  INTEGER PRIMARY KEY AUTOINCREMENT,
  label               TEXT,
  assignment_mode     TEXT DEFAULT 'random',
  bid_open_date       TEXT,
  bid_close_date      TEXT,
  tickets_per_student INTEGER DEFAULT 10,
  created_at          TEXT DEFAULT CURRENT_TIMESTAMP
);")
ensure_column("weekly_rounds", "assignment_mode TEXT DEFAULT 'random'")
ensure_column("weekly_rounds", "bid_open_date TEXT")
ensure_column("weekly_rounds", "bid_close_date TEXT")
ensure_column("weekly_rounds", "tickets_per_student INTEGER DEFAULT 10")
ensure_column("weekly_rounds", "tokens_revealed INTEGER DEFAULT 1")
ensure_column("weekly_rounds", "tiebreak_method TEXT DEFAULT 'weighted_lottery'")
db_exec("CREATE TABLE IF NOT EXISTS job_posts(
  id            INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id      INTEGER,
  job_name      TEXT NOT NULL,
  category_id   INTEGER,
  slots         INTEGER DEFAULT 1,
  wage_override REAL,
  active        INTEGER DEFAULT 1,
  display_order INTEGER DEFAULT 99,
  selection_time TEXT,
  created_at    TEXT DEFAULT CURRENT_TIMESTAMP
);")
ensure_column("job_posts", "job_name TEXT")
ensure_column("job_posts", "category_id INTEGER")
ensure_column("job_posts", "wage_override REAL")
ensure_column("job_posts", "active INTEGER DEFAULT 1")
ensure_column("job_posts", "display_order INTEGER DEFAULT 99")
ensure_column("job_posts", "voluntary INTEGER DEFAULT 0")
ensure_column("job_posts", "in_draw INTEGER DEFAULT 1")
ensure_column("job_posts", "selection_time TEXT")
ensure_column("job_posts", "created_at TEXT")
ensure_column("job_categories", "selection_time TEXT")
ensure_column("job_categories", "contribution_type TEXT")
ensure_column("job_categories", "purpose TEXT")
ensure_column("job_categories", "expected_output TEXT")
ensure_column("job_categories", "completion_criterion TEXT")
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
ensure_column("job_assignments", "job_post_id INTEGER")
ensure_column("job_assignments", "assigned_wage REAL")
ensure_column("job_assignments", "assignment_mode TEXT")
ensure_column("job_assignments", "status TEXT DEFAULT 'assigned'")
ensure_column("job_assignments", "outcome TEXT")
ensure_column("job_assignments", "tokens_awarded INTEGER DEFAULT 0")
ensure_column("job_assignments", "updated_at TEXT")
ensure_column("job_assignments", "tokens_credited INTEGER DEFAULT 1")
ensure_column("job_assignments", "created_at TEXT")
db_exec("CREATE TABLE IF NOT EXISTS wage_bids(
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id     INTEGER,
  category_id  INTEGER,
  user_id      TEXT,
  min_wage     REAL,
  submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(round_id, category_id, user_id)
);")
ensure_column("wage_bids", "min_wage REAL")
ensure_column("wage_bids", "submitted_at TEXT")
db_exec("CREATE TABLE IF NOT EXISTS application_bids(
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id     INTEGER,
  category_id  INTEGER,
  user_id      TEXT,
  tickets      INTEGER DEFAULT 0,
  submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(round_id, category_id, user_id)
);")
ensure_column("application_bids", "tickets INTEGER DEFAULT 0")
ensure_column("application_bids", "submitted_at TEXT")
db_exec("CREATE TABLE IF NOT EXISTS job_templates(
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  name           TEXT NOT NULL,
  category_id    INTEGER,
  slots          INTEGER DEFAULT 1,
  suggested_wage REAL,
  active         INTEGER DEFAULT 1,
  created_at     TEXT DEFAULT CURRENT_TIMESTAMP
);")
ensure_column("job_templates", "category_id INTEGER")
ensure_column("job_templates", "slots INTEGER DEFAULT 1")
ensure_column("job_templates", "suggested_wage REAL")
ensure_column("job_templates", "active INTEGER DEFAULT 1")
ensure_column("job_templates", "selection_time TEXT")
ensure_column("job_templates", "voluntary INTEGER DEFAULT 0")
ensure_column("job_templates", "in_draw INTEGER DEFAULT 1")
ensure_column("job_templates", "display_order INTEGER DEFAULT 99")
ensure_column("job_templates", "created_at TEXT")

# Compatibility backfills for live databases created by older test/setup code.
backfill_cols <- function(table) {
  tryCatch(db_query(sprintf("PRAGMA table_info(%s);", table))$name,
           error = function(e) character(0))
}
jc_cols <- backfill_cols("job_categories")
if ("wage" %in% jc_cols) {
  db_exec("UPDATE job_categories SET default_wage=wage WHERE wage IS NOT NULL AND (default_wage IS NULL OR default_wage=10);")
}
jp_cols <- backfill_cols("job_posts")
if ("wage" %in% jp_cols) {
  db_exec("UPDATE job_posts SET wage_override=wage WHERE wage IS NOT NULL AND (wage_override IS NULL OR wage_override=10);")
}
db_exec("UPDATE job_posts
           SET job_name=COALESCE((SELECT name FROM job_categories WHERE id=job_posts.category_id), 'Class job')
         WHERE job_name IS NULL OR trim(job_name)='';")
wb_cols <- backfill_cols("wage_bids")
if ("wage" %in% wb_cols) {
  db_exec("UPDATE wage_bids SET min_wage=wage WHERE wage IS NOT NULL AND min_wage IS NULL;")
}
if ("created_at" %in% wb_cols) {
  db_exec("UPDATE wage_bids SET submitted_at=created_at WHERE created_at IS NOT NULL AND submitted_at IS NULL;")
}
ab_cols <- backfill_cols("application_bids")
if ("created_at" %in% ab_cols) {
  db_exec("UPDATE application_bids SET submitted_at=created_at WHERE created_at IS NOT NULL AND submitted_at IS NULL;")
}
ja_cols <- backfill_cols("job_assignments")
if ("wage" %in% ja_cols) {
  db_exec("UPDATE job_assignments SET assigned_wage=wage WHERE wage IS NOT NULL AND assigned_wage IS NULL;")
}
if ("tokens" %in% ja_cols) {
  db_exec("UPDATE job_assignments SET tokens_awarded=CAST(tokens AS INTEGER) WHERE tokens IS NOT NULL AND (tokens_awarded IS NULL OR tokens_awarded=0);")
}
db_exec("UPDATE job_assignments SET status='assigned' WHERE status IS NULL OR trim(status)='';")
# Expected demand for volunteer jobs, posted per round (e.g. at the start of
# class). Used by the 'posted' volunteer clearing rule.
db_exec("CREATE TABLE IF NOT EXISTS volunteer_demand(
  round_id    INTEGER,
  category_id INTEGER,
  demand      INTEGER DEFAULT 1,
  updated_at  TEXT DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (round_id, category_id)
);")
# Job assignment outcome tracking (safe on re-run via try; must come AFTER the
# CREATE TABLE statements above so a fresh database gets the columns too)
# Template-level defaults above make every copied post carry timing/voluntary/in-draw.

# Recurring bid-lock window: bids lock shortly before each class session and
# reopen that evening (all editable in Settings > Round Setup)
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('bid_lock_enabled','1');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('class_days','Mon,Wed');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('class_start_time','12:00');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('bid_lock_lead_min','60');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('bid_reopen_time','17:00');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('class_tz','America/New_York');")
db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('volunteer_clearing_rule','lowest');")

# ── Default job catalog ───────────────────────────────────────────────────────
# Simplified catalog. Categories are the level students bid on: assigned class
# roles, plus the three contribution types they can be cold-called on or
# volunteer for (answering, asking, board work). Whether a job is drawn or
# volunteered is a template/post-level flag, not a separate category.
seed_class_job_defaults <- function(exec_fn = db_exec, query_fn = db_query, ensure_round = FALSE) {
  db_exec <- exec_fn
  db_query <- query_fn

  categories <- list(
    list(name = "Class roles", wage = 2, voluntary = 0L, in_draw = 1L, order = 1L,
         desc = "Recurring per-class jobs assigned by draw early in the semester, then by bids."),
    list(name = "Volunteer", wage = 1, voluntary = 1L, in_draw = 0L, order = 2L,
         desc = "Live participation jobs logged during class."),
    list(name = "Cold Call", wage = 1, voluntary = 0L, in_draw = 1L, order = 3L,
         desc = "In-class cold-call draws")
  )

  # Templates. Every-class jobs are active (auto-copied into each new round);
  # some-session jobs are seeded inactive — toggle Auto-copy on when you want
  # them in the next round. Timing codes: start / during / end / volunteer.
  templates <- list(
    # Every class — assigned at the start of class
    list(name = "Materials summary",    cat = "Class roles", timing = "start", slots = 1L, wage = 2, vol = 0L, draw = 1L, active = 1L, order = 1L),
    list(name = "Last class recap",     cat = "Class roles", timing = "start", slots = 1L, wage = 2, vol = 0L, draw = 1L, active = 1L, order = 2L),
    # Every class — assigned after class
    list(name = "Note taker",           cat = "Class roles", timing = "end",   slots = 1L, wage = 2, vol = 0L, draw = 1L, active = 1L, order = 3L),
    list(name = "Critic/skeptic",       cat = "Class roles", timing = "end",   slots = 1L, wage = 2, vol = 0L, draw = 1L, active = 1L, order = 4L),
    list(name = "Policy/example scout", cat = "Class roles", timing = "end",   slots = 1L, wage = 2, vol = 0L, draw = 1L, active = 1L, order = 5L),
    # Some sessions only
    list(name = "Discussion lead",      cat = "Class roles", timing = "end",   slots = 1L, wage = 2, vol = 0L, draw = 1L, active = 0L, order = 6L),
    list(name = "Cold call: answer a question",     cat = "Cold Call", timing = "during", slots = 1L, wage = 1, vol = 0L, draw = 1L, active = 1L, order = 7L),
    list(name = "Cold call: graph/answer on board", cat = "Cold Call", timing = "during", slots = 1L, wage = 1, vol = 0L, draw = 1L, active = 1L, order = 8L),
    # Volunteering — never drawn; logged live during class
    list(name = "Volunteer: answer a question",      cat = "Volunteer", timing = "volunteer", slots = 99L, wage = 1, vol = 1L, draw = 0L, active = 1L, order = 9L),
    list(name = "Volunteer: ask a question",         cat = "Volunteer", timing = "volunteer", slots = 99L, wage = 1, vol = 1L, draw = 0L, active = 1L, order = 10L),
    list(name = "Volunteer: graph/answer on board",  cat = "Volunteer", timing = "volunteer", slots = 99L, wage = 1, vol = 1L, draw = 0L, active = 1L, order = 11L)
  )

  # Jobs seeded by earlier versions of this app that no longer exist.
  retired_jobs <- c(
    "Previous-class recap", "Policy example scout", "Problem/graph explainer",
    "Forum steward", "Class note taker", "Skeptic/discussant", "Discussion leader",
    "Typo, ambiguity, or broken-link report", "Recommended slide or material change",
    "Alternative presentation of a concept, graph, or example",
    "Data/source verifier for a policy example", "Graph redrawer or figure caption improver",
    "Exam-review question submitter", "Useful forum answer, clarification, or synthesis",
    "Relevant policy example outside an assigned scout job", "Muddiest-point post",
    "Opening recap", "Reading analyst", "Concept explainer", "Class record keeper",
    "Course-material fix or suggestion", "Concept explanation or graph improvement",
    "Forum answer or muddiest-point post", "Policy/data example or source check"
  )
  # Old one-category-per-job categories, mapped onto the new four.
  retired_category_map <- c(
    "Opening recap"                            = "Class roles",
    "Reading analyst"                          = "Class roles",
    "Policy/example scout"                     = "Class roles",
    "Concept explainer"                        = "Cold Call",
    "Class record keeper"                      = "Class roles",
    "Discussion lead"                          = "Class roles",
    "Course-material fix or suggestion"        = "Volunteer",
    "Concept explanation or graph improvement" = "Cold Call",
    "Forum answer or muddiest-point post"      = "Volunteer",
    "Policy/data example or source check"      = "Volunteer"
  )

  cat_id_for <- function(nm) {
    r <- db_query("SELECT id FROM job_categories WHERE lower(name)=lower(?) ORDER BY id LIMIT 1;", list(nm))
    if (nrow(r)) r$id[1] else NA_integer_
  }

  # Categories: insert missing; on existing ones only fill a blank description.
  cat_ids <- list()
  for (cc in categories) {
    cid <- cat_id_for(cc$name)
    if (is.na(cid)) {
      db_exec(
        "INSERT INTO job_categories(name, default_wage, description, voluntary, in_draw, display_order)
         VALUES(?,?,?,?,?,?);",
        list(cc$name, cc$wage, cc$desc, cc$voluntary, cc$in_draw, cc$order))
      cid <- db_query("SELECT last_insert_rowid() AS id;")$id[1]
    } else {
      db_exec("UPDATE job_categories SET description=COALESCE(NULLIF(description,''),?) WHERE id=?;",
              list(cc$desc, cid))
    }
    cat_ids[[cc$name]] <- cid
  }

  # One-time migration from the old catalog (guarded so later restarts never
  # clobber instructor edits to templates or categories).
  migrated <- tryCatch(
    db_query("SELECT value FROM labor_settings WHERE key='job_catalog_v2_migrated';"),
    error = function(e) data.frame())
  first_migration <- !nrow(migrated)

  if (first_migration) {
    for (old_nm in names(retired_category_map)) {
      old_id <- cat_id_for(old_nm)
      new_id <- cat_ids[[retired_category_map[[old_nm]]]]
      if (is.na(old_id) || is.null(new_id) || is.na(new_id) || old_id == new_id) next
      db_exec("UPDATE job_posts SET category_id=? WHERE category_id=?;", list(new_id, old_id))
      db_exec("UPDATE job_templates SET category_id=? WHERE category_id=?;", list(new_id, old_id))
      db_exec("UPDATE OR IGNORE wage_bids SET category_id=? WHERE category_id=?;", list(new_id, old_id))
      db_exec("DELETE FROM wage_bids WHERE category_id=?;", list(old_id))
      db_exec("UPDATE OR IGNORE application_bids SET category_id=? WHERE category_id=?;", list(new_id, old_id))
      db_exec("DELETE FROM application_bids WHERE category_id=?;", list(old_id))
      db_exec("DELETE FROM job_categories WHERE id=?;", list(old_id))
    }
    db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES('job_catalog_v2_migrated','1');")
  }

  # Keep old catalog entries out during the one-time catalog migration only.
  # After that, instructor edits must stick across restarts.
  if (first_migration) {
    for (old_name in retired_jobs) {
      db_exec("UPDATE job_templates SET active=0 WHERE lower(name)=lower(?);", list(old_name))
      db_exec("UPDATE job_posts SET active=0 WHERE lower(job_name)=lower(?);", list(old_name))
    }
  }


  for (tt in templates) {
    cid <- cat_ids[[tt$cat]]
    existing <- db_query("SELECT id FROM job_templates WHERE lower(name)=lower(?) ORDER BY id LIMIT 1;", list(tt$name))
    if (nrow(existing)) {
      if (first_migration) {
        db_exec(
          "UPDATE job_templates
           SET category_id=?, slots=?, suggested_wage=COALESCE(suggested_wage,?),
               selection_time=?, voluntary=?, in_draw=?, display_order=?, active=?
           WHERE id=?;",
          list(cid, tt$slots, tt$wage, tt$timing, tt$vol, tt$draw, tt$order, tt$active, existing$id[1]))
      }
    } else {
      db_exec(
        "INSERT INTO job_templates(name, category_id, slots, suggested_wage, active,
                                   selection_time, voluntary, in_draw, display_order)
         VALUES(?,?,?,?,?,?,?,?,?);",
        list(tt$name, cid, tt$slots, tt$wage, tt$active, tt$timing, tt$vol, tt$draw, tt$order))
    }
  }

  latest_round <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                           error = function(e) data.frame())
  if (isTRUE(ensure_round) && !nrow(latest_round)) {
    db_exec(
      "INSERT INTO weekly_rounds(label, assignment_mode, tiebreak_method, tokens_revealed, tickets_per_student)
       VALUES('Current Class', 'random', 'weighted_lottery', 0, 10);")
    latest_round <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                             error = function(e) data.frame())
  }
  # Make sure the latest round has every active template as a post.
  if (nrow(latest_round)) {
    rid <- latest_round$id[1]
    for (tt in templates) {
      if (!isTRUE(as.integer(tt$active) == 1L)) next
      existing <- db_query(
        "SELECT id FROM job_posts WHERE round_id=? AND lower(job_name)=lower(?) ORDER BY id LIMIT 1;",
        list(rid, tt$name))
      if (!nrow(existing)) {
        db_exec(
          "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override,
                                 in_draw, voluntary, selection_time, display_order)
           VALUES(?,?,?,?,?,?,?,?,?);",
          list(rid, tt$name, cat_ids[[tt$cat]], tt$slots, tt$wage,
               tt$draw, tt$vol, tt$timing, tt$order))
      }
    }
  }
}
seed_class_job_defaults(ensure_round = TRUE)

SESSION_DAYS <- 14L

make_token <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 48L, replace = TRUE), collapse = "")
}
bootstrap_admin_emails <- function() {
  emails <- trimws(strsplit(Sys.getenv("ADMIN_EMAILS", ""), ",", fixed = TRUE)[[1]])
  emails <- gsub("^[\"']|[\"']$", "", emails)
  emails <- unique(tolower(emails[nzchar(emails)]))
  for (email in emails) {
    db_exec(
      "INSERT INTO users(user_id, display_name, pw_hash, is_admin, section, active, is_demo)
       VALUES(?,?,?,1,NULL,1,0)
       ON CONFLICT(user_id) DO UPDATE SET
         is_admin=1,
         active=1,
         is_demo=0,
         display_name=COALESCE(NULLIF(display_name,''), excluded.display_name);",
      list(email, email, bcrypt::hashpw(make_token())))
  }
}
bootstrap_admin_emails()
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

# ── Recurring bid lock ────────────────────────────────────────────────────────
# Bidding is continuous, but on class days bids lock a configurable lead time
# before class starts and reopen that evening. Returns the current state plus
# formatted times for display.
bid_lock_status <- function() {
  fmt_hm <- function(mins) sprintf("%d:%02d %s",
                                   ((mins %/% 60L - 1L) %% 12L) + 1L, mins %% 60L,
                                   if (mins %/% 60L >= 12L) "PM" else "AM")
  parse_hm <- function(x, def) {
    p <- suppressWarnings(as.integer(strsplit(trimws(as.character(x %||% def)), ":")[[1]]))
    if (length(p) < 2 || any(is.na(p[1:2]))) p <- as.integer(strsplit(def, ":")[[1]])
    p[1] * 60L + p[2]
  }
  enabled <- identical(as.character(get_setting("bid_lock_enabled", "1")), "1")
  days    <- trimws(strsplit(as.character(get_setting("class_days", "Mon,Wed")), ",")[[1]])
  start_min  <- parse_hm(get_setting("class_start_time", "12:00"), "12:00")
  lead       <- suppressWarnings(as.integer(get_setting("bid_lock_lead_min", "60")))
  if (is.na(lead) || lead < 0) lead <- 60L
  reopen_min <- parse_hm(get_setting("bid_reopen_time", "17:00"), "17:00")
  lock_min   <- max(0L, start_min - lead)
  tz  <- as.character(get_setting("class_tz", "America/New_York"))
  now <- tryCatch(as.POSIXlt(Sys.time(), tz = tz), error = function(e) as.POSIXlt(Sys.time()))
  today <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[now$wday + 1L]
  is_class_day <- today %in% days
  now_min <- now$hour * 60L + now$min
  locked  <- enabled && is_class_day && now_min >= lock_min && now_min < reopen_min
  list(
    enabled  = enabled,
    locked   = locked,
    days     = days,
    lock_at  = fmt_hm(lock_min),
    class_at = fmt_hm(start_min),
    reopen_at = fmt_hm(reopen_min),
    schedule_label = sprintf(
      "On class days (%s) bids lock at %s — %d minutes before the %s class — and reopen at %s.",
      paste(days, collapse = "/"), fmt_hm(lock_min), lead, fmt_hm(start_min), fmt_hm(reopen_min)),
    locked_msg = sprintf(
      "Bids are locked for today's class (locked at %s, reopen at %s). Your last saved bids will be used.",
      fmt_hm(lock_min), fmt_hm(reopen_min))
  )
}
parse_ext_prices <- function() {
  rows <- tryCatch(
    db_query("SELECT id, label, hours, tokens FROM extension_options WHERE COALESCE(active,1)=1 ORDER BY hours DESC;"),
    error = function(e) data.frame())
  if (!nrow(rows)) return(data.frame(id=integer(0), label=character(0), hours=numeric(0), tokens=numeric(0)))
  rows
}

# Safely evaluate an arithmetic expression (admin-set only) with one named variable
eval_cost_expr <- function(expr_str, var_name, var_value) {
  env <- new.env(parent = baseenv())
  assign(var_name, as.numeric(var_value), envir = env)
  tryCatch(
    max(0, ceiling(as.numeric(eval(parse(text = expr_str), envir = env)))),
    error = function(e) NA_real_
  )
}

parse_flex_cost <- function(text = NULL) {
  if (is.null(text)) text <- tryCatch(get_setting("flex_cost_schedule", "2,4,6,8,10"), error=function(e)"2,4,6,8,10")
  text <- trimws(text %||% "")
  if (!nzchar(text)) return(list(type="table", values=c(2)))
  parts <- suppressWarnings(as.numeric(strsplit(text, ",")[[1]]))
  if (!any(is.na(parts))) return(list(type="table", values=parts))
  list(type="expr", expr=text)
}
question_cost_for_n <- function(n, schedule_text = NULL) {
  n <- max(1L, as.integer(n))
  sched <- parse_flex_cost(schedule_text)
  if (sched$type == "table") {
    tbl <- sched$values
    if (length(tbl) == 0) return(as.integer(2 * n))
    if (n <= length(tbl)) return(as.integer(tbl[n]))
    last <- tbl[length(tbl)]
    step <- if (length(tbl) >= 2) (tbl[length(tbl)] - tbl[length(tbl)-1]) else tbl[1]
    return(as.integer(max(1, last + step * (n - length(tbl)))))
  }
  # Expression: q = questions already owned (0-indexed)
  val <- eval_cost_expr(sched$expr, "q", n - 1L)
  as.integer(if (is.na(val)) 2 * n else max(1, val))
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
# Cost lookup that handles both table (k:v) and expression (variable n) formats
rw_cost_for_n <- function(n, schedule_text = NULL) {
  if (is.null(schedule_text))
    schedule_text <- tryCatch(get_setting("reweight_cost_schedule", "1:2,2:5,3:9,4:14,5:20"),
                              error = function(e) "1:2,2:5,3:9,4:14,5:20")
  tbl <- parse_rw_costs()
  if (length(tbl) > 0) {
    v <- as.numeric(tbl[as.character(n)] %||% NA)
    if (!is.na(v)) return(v)
  }
  # Fall through to expression if no table match (or table didn't parse)
  text <- trimws(schedule_text %||% "")
  if (grepl("[a-zA-Z]", text)) return(eval_cost_expr(text, "n", n))
  NA_real_
}
get_rw_max_points <- function() {
  as.integer(tryCatch(get_setting("grade_reweight_max_points", "5"), error=function(e)"5") %||% 5L)
}

gradebook_item_specs <- function(cat_row, inames_df = data.frame()) {
  n <- max(1L, as.integer(cat_row$item_count %||% 1L))
  cat_weight <- as.numeric(cat_row$weight %||% 0)
  equal_weight <- if (n > 0) cat_weight / n else cat_weight
  prefix <- if (!is.null(cat_row$item_prefix) && !is.na(cat_row$item_prefix) && nzchar(cat_row$item_prefix))
              cat_row$item_prefix else cat_row$name
  overrides <- if (nrow(inames_df))
    inames_df[inames_df$category_id == cat_row$id, , drop = FALSE]
  else data.frame()
  do.call(rbind, lapply(seq_len(n), function(i) {
    ov <- if (nrow(overrides)) overrides[overrides$item_index == i, , drop = FALSE] else data.frame()
    nm <- if (nrow(ov) && nzchar(ov$item_name[1] %||% "")) ov$item_name[1]
          else if (n == 1) cat_row$name
          else paste0(prefix, " ", i)
    custom <- nrow(ov) && "item_weight" %in% names(ov) &&
      !is.na(suppressWarnings(as.numeric(ov$item_weight[1])))
    wt <- if (custom) suppressWarnings(as.numeric(ov$item_weight[1])) else equal_weight
    data.frame(
      item_index = i,
      item_name = nm,
      item_weight = wt,
      custom_weight = custom,
      stringsAsFactors = FALSE
    )
  }))
}

# Returns a named list suitable for selectInput grouped choices: list(CatName = c(item1, item2, ...))
get_all_gradebook_items <- function() {
  cats   <- tryCatch(db_query("SELECT * FROM gradebook_categories ORDER BY display_order, id;"),
                     error = function(e) data.frame())
  inames <- tryCatch(db_query("SELECT * FROM gradebook_item_names ORDER BY category_id, item_index;"),
                     error = function(e) data.frame())
  if (!nrow(cats)) return(list())
  out <- list()
  for (i in seq_len(nrow(cats))) {
    r      <- cats[i, ]
    specs <- gradebook_item_specs(r, inames)
    items <- specs$item_name
    out[[r$name %||% paste0("Cat", i)]] <- setNames(items, items)
  }
  out
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
  # Primary source: gradebook_categories table (same as gradebook builder)
  gb <- tryCatch(db_query(
    "SELECT name, weight FROM gradebook_categories ORDER BY display_order, id;"),
    error = function(e) data.frame())
  if (nrow(gb)) return(gb[, c("name","weight"), drop=FALSE])
  # Legacy fallback: JSON stored in labor_settings
  default_json <- '[{"name":"Homework","weight":33},{"name":"Midterm","weight":33},{"name":"Final","weight":34}]'
  raw <- tryCatch(get_setting("grade_categories_json", default_json), error = function(e) default_json)
  tryCatch({
    df <- jsonlite::fromJSON(raw)
    if (is.data.frame(df) && all(c("name","weight") %in% names(df))) df
    else data.frame(name=character(0), weight=numeric(0))
  }, error = function(e) data.frame(name=character(0), weight=numeric(0)))
}

compute_student_grade <- function(uid) {
  cats      <- tryCatch(db_query(
    "SELECT * FROM gradebook_categories ORDER BY display_order, id;"),
    error = function(e) data.frame())
  inames_df <- tryCatch(db_query(
    "SELECT * FROM gradebook_item_names ORDER BY category_id, item_index;"),
    error = function(e) data.frame())
  grades    <- tryCatch(db_query(
    "SELECT assignment_name, score, max_score, grade_pct FROM student_grades WHERE user_id=?;",
    list(uid)), error = function(e) data.frame())

  if (!nrow(cats)) return(list(cats = data.frame(), items = data.frame(), overall = NA_real_))

  all_items <- data.frame(item_name=character(), cat_idx=integer(),
                          category_name=character(), weight=numeric(),
                          item_weight=numeric(), item_count=integer(), grade_pct=numeric(),
                          score=numeric(), max_score=numeric(),
                          stringsAsFactors=FALSE)
  for (i in seq_len(nrow(cats))) {
    r      <- cats[i, ]
    specs  <- gradebook_item_specs(r, inames_df)
    n      <- nrow(specs)
    for (j in seq_len(n)) {
      nm <- specs$item_name[j]
      matched <- if (nrow(grades)) grades[grades$assignment_name == nm, , drop=FALSE]
                 else data.frame()
      all_items <- rbind(all_items, data.frame(
        item_name     = nm,
        cat_idx       = i,
        category_name = r$name,
        weight        = as.numeric(r$weight %||% 0),
        item_weight   = as.numeric(specs$item_weight[j] %||% 0),
        item_count    = n,
        grade_pct     = if (nrow(matched)) as.numeric(matched$grade_pct[1]) else NA_real_,
        score         = if (nrow(matched)) as.numeric(matched$score[1] %||% NA) else NA_real_,
        max_score     = if (nrow(matched)) as.numeric(matched$max_score[1] %||% NA) else NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  }

  cat_summary <- do.call(rbind, lapply(seq_len(nrow(cats)), function(i) {
    r      <- cats[i, ]
    items  <- all_items[all_items$cat_idx == i, , drop=FALSE]
    graded <- items[!is.na(items$grade_pct), , drop=FALSE]
    cat_wt <- sum(graded$item_weight, na.rm = TRUE)
    cat_avg <- if (nrow(graded) && cat_wt > 0)
      sum(graded$grade_pct * graded$item_weight, na.rm = TRUE) / cat_wt
    else if (nrow(graded)) mean(graded$grade_pct, na.rm = TRUE)
    else NA_real_
    data.frame(
      category     = r$name,
      weight       = as.numeric(r$weight %||% 0),
      graded_weight = cat_wt,
      cat_avg      = cat_avg,
      contribution = if (!is.na(cat_avg)) cat_avg * cat_wt / 100
                     else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  graded_wt   <- sum(cat_summary$graded_weight[!is.na(cat_summary$cat_avg)], na.rm = TRUE)
  graded_cont <- sum(cat_summary$contribution, na.rm = TRUE)
  overall <- if (graded_wt > 0) graded_cont / graded_wt * 100 else NA_real_

  list(cats = cat_summary, items = all_items, overall = overall,
       graded_weight = graded_wt, total_weight = sum(cats$weight, na.rm = TRUE))
}

# Uniform wage paid to every volunteer in a category for a round, derived from
# that round's wage bids (no rationing — the wage applies to whoever volunteers).
# Rules:
#   "lowest" — the lowest bid (k = 1).
#   "demand" — the k-th lowest bid, k = the volunteer post's slots (standing
#              demand for that job over a class session).
#   "posted" — the k-th lowest bid, k = the expected demand posted for this
#              round (set at the start of class in the Live Tracker); falls
#              back to the post's slots when none is posted.
# k is capped at the number of bids. NA when there are no bids (caller falls
# back to the post wage).
volunteer_clearing_wage <- function(round_id, category_id, slots, query_fn = db_query) {
  if (is.na(round_id %||% NA) || is.na(category_id %||% NA)) return(NA_real_)
  bids <- tryCatch(query_fn(
    "SELECT min_wage FROM wage_bids
     WHERE round_id=? AND category_id=? AND min_wage IS NOT NULL
     ORDER BY min_wage ASC;",
    list(as.integer(round_id), as.integer(category_id))),
    error = function(e) data.frame())
  if (!nrow(bids)) return(NA_real_)
  rule <- as.character(get_setting("volunteer_clearing_rule", "lowest"))
  k <- if (identical(rule, "demand")) {
    max(1L, as.integer(slots %||% 1L))
  } else if (identical(rule, "posted")) {
    posted <- tryCatch(query_fn(
      "SELECT demand FROM volunteer_demand WHERE round_id=? AND category_id=?;",
      list(as.integer(round_id), as.integer(category_id))),
      error = function(e) data.frame())
    if (nrow(posted) && !is.na(posted$demand[1] %||% NA)) max(1L, as.integer(posted$demand[1]))
    else max(1L, as.integer(slots %||% 1L))
  } else 1L
  as.numeric(bids$min_wage[min(nrow(bids), k)])
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
.arc-font-ctrl { display:flex; align-items:center; gap:.3rem; font-size:.75rem;
                 opacity:.8; white-space:nowrap; }
.arc-font-ctrl input[type=range] { width:70px; accent-color:#fff; cursor:pointer; }

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
.grade-section { margin: .25rem 0 1.25rem; }
.grade-section .sec-label { margin-bottom: .6rem; }
.grade-overall-row { display:flex; align-items:center; gap:1.25rem; margin-bottom:.85rem; flex-wrap:wrap; }
.grade-overall-tile { background:#fff; border-radius:10px; border:1px solid #e8e8e8;
                      padding:.7rem 1.1rem; text-align:center; min-width:110px; }
.grade-overall-val  { font-size:2rem; font-weight:700; color:#951829; line-height:1.05; }
.grade-overall-lbl  { font-size:.72rem; color:#888; margin-top:.15rem; }
.grade-tbl { font-size:.85rem; width:100%; border-collapse:collapse; }
.grade-tbl th { color:#666; font-weight:600; padding:.28rem .5rem;
                border-bottom:2px solid #eee; text-align:left; }
.grade-tbl td { padding:.28rem .5rem; border-bottom:1px solid #f2f2f2; }
.grade-tbl .cat-row td { font-weight:600; background:#fafafa; }
.grade-tbl .cat-row td:first-child { padding-left:.35rem; }
.grade-tbl .item-row td { color:#555; }
.grade-tbl .item-row td:first-child { padding-left:1.5rem; }
.grade-tbl .total-row td { font-weight:700; border-top:2px solid #ddd; background:#f7f7f7; }
.grade-na { color:#bbb; font-style:italic; }
.rw-preview { margin:.65rem 0 .9rem; background:#f9f9f9; border:1px solid #eee;
              border-radius:8px; padding:.75rem .9rem; }
.rw-preview-title { font-size:.75rem; font-weight:700; color:#888; text-transform:uppercase;
                    letter-spacing:.07em; margin-bottom:.5rem; }
.rw-preview-tbl { font-size:.83rem; width:100%; border-collapse:collapse; }
.rw-preview-tbl th { color:#666; font-weight:600; padding:.25rem .4rem;
                     border-bottom:1px solid #e0e0e0; text-align:left; white-space:nowrap; }
.rw-preview-tbl td { padding:.25rem .4rem; border-bottom:1px solid #f0f0f0; }
.rw-preview-tbl .changed { color:#951829; font-weight:600; }
.rw-preview-tbl .total-row td { font-weight:700; border-top:2px solid #ddd; }
.rw-delta-pos { color:#1a6e3c; font-weight:600; }
.rw-delta-neg { color:#b00020; font-weight:600; }

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
.live-toolbar { position: sticky; top: 0; z-index: 10; background: #fff;
                border: 1px solid #e8e8e8; border-radius: 10px; padding: .75rem;
                margin-bottom: .8rem; box-shadow: 0 2px 10px rgba(0,0,0,.08); }
.live-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: .65rem; }
.live-card { background: #fff; border: 1px solid #e8e8e8; border-radius: 10px;
             padding: .75rem .85rem; }
.live-card-name { font-size: 1.05rem; font-weight: 700; line-height: 1.2; }
.live-card-section { color: #888; font-size: .78rem; margin: .12rem 0 .55rem; }
.live-card-actions { display: grid; grid-template-columns: repeat(3, 1fr); gap: .35rem; }
.live-card-actions .btn { min-height: 42px; white-space: normal; font-weight: 600; }
@media (max-width: 600px) {
  .arc-body { padding-left: .6rem; padding-right: .6rem; }
  .live-grid { grid-template-columns: 1fr; }
  .live-card-name { font-size: 1.15rem; }
}

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

/* ── Google sign-in ─────────────────────────────────────────────────────── */
.btn-google { display:flex; align-items:center; justify-content:center; gap:.55rem;
              width:100%; padding:.65rem 1rem; background:#fff; border:1.5px solid #dadce0;
              border-radius:6px; font-size:.95rem; font-weight:500; color:#3c4043;
              cursor:pointer; transition:background .15s, box-shadow .15s; margin-bottom:.75rem; }
.btn-google:hover { background:#f8f9fa; box-shadow:0 1px 4px rgba(0,0,0,.15); }
.btn-google svg { flex-shrink:0; }
.admin-login-toggle { margin-top:.6rem; }
.admin-login-toggle summary { cursor:pointer; font-size:.82rem; color:#888;
                               text-align:center; padding:.3rem 0; }
.admin-login-toggle summary:hover { color:#555; }
.admin-login-toggle .form-group { margin-top:.6rem; }
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
  Shiny.addCustomMessageHandler('oauth_redirect', function(msg) {
    window.location.href = msg.url;
  });
  Shiny.addCustomMessageHandler('clean_url', function(msg) {
    if (window.history && window.history.replaceState)
      window.history.replaceState({}, document.title, location.pathname);
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

  dm       <- demo_server_init(session, DB_PATH)
  .sandbox <- dm$is_demo
  db_exec  <- dm$db_exec
  db_query <- dm$db_query

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
    cold_call_draw = NULL,
    active_section = get_setting("active_section", ""),
    jobs_ver       = 0L,    # bumped after any job-post or category mutation
    students_ver   = 0L,    # bumped after any student roster mutation
    gradebook_ver  = 0L     # bumped after any gradebook category/item mutation
  )

  # ── Root UI ──────────────────────────────────────────────────────────────────
  output$root_ui <- renderUI({
    if (!rv$authed) {
      # ── Login page ──
      div(class = "login-page",
        div(class = "login-card",
          div(class = "login-logo", paste0("\U0001f393 ", APP_NAME)),
          div(class = "login-tagline",
              if (GOOGLE_AUTH_ENABLED) "Sign in with your school Google account."
              else "Log in with credentials from your instructor."),
          if (GOOGLE_AUTH_ENABLED)
            tags$a(
              href = "/auth/login", class = "btn-google",
              tags$svg(xmlns = "http://www.w3.org/2000/svg", viewBox = "0 0 24 24",
                       height = "18", width = "18",
                tags$path(fill = "#4285F4", d = "M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92c-.26 1.37-1.04 2.53-2.21 3.31v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.09z"),
                tags$path(fill = "#34A853", d = "M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z"),
                tags$path(fill = "#FBBC05", d = "M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z"),
                tags$path(fill = "#EA4335", d = "M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z")
              ),
              "Sign in with Google"
            ),
          if (GOOGLE_AUTH_ENABLED)
            tags$details(class = "admin-login-toggle",
              tags$summary("Sign in with password"),
              textInput("login_user", NULL, placeholder = "Username"),
              passwordInput("login_pw", NULL, placeholder = "Password"),
              actionButton("login_btn", "Sign In →", class = "btn btn-primary btn-block")
            )
          else
            tagList(
              textInput("login_user", NULL, placeholder = "Username"),
              passwordInput("login_pw", NULL, placeholder = "Password"),
              actionButton("login_btn", "Sign In →", class = "btn btn-primary btn-block")
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
              div(class = "preview-card-label", "Games & Demos"),
              div(class = "preview-card-desc", "Live games + interactive demos"))
          ),
          tags$details(class = "login-howto",
            tags$summary("How to get started"),
            tags$ul(
              tags$li(tags$strong("Sign in"), " using your school Google account. Your instructor must add your email to the class roster first."),
              tags$li(tags$strong("Today"), " shows your current job assignment, prevailing wages, and any active class game."),
              tags$li(tags$strong("Job Market"), " is where you submit wage bids or ticket allocations each round."),
              tags$li(tags$strong("Games & Demos"), " shows the active game, the full game catalog, and interactive economic demos — always available."),
              tags$li(tags$strong("Account"), " tracks your Flex Pass balance, Participation Tokens, and transaction history.")
            )
          ),
          demo_login_ui
        )
      )
    } else {
      # ── Authenticated app ──
      tagList(
        div(class = "arc-header",
          div(class = "arc-title", paste0("\U0001f393 ", APP_NAME)),
          uiOutput("header_widgets", inline = TRUE),
          tags$div(class = "arc-font-ctrl",
            tags$span("A"),
            tags$input(
              type  = "range", min = "80", max = "130", value = "100", step = "5",
              title = "Adjust font size",
              oninput = "document.body.style.fontSize = this.value + '%';"
            ),
            tags$span("A", style = "font-size:1.1em;")
          ),
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
        demo_banner_ui(.sandbox),
        if (rv$is_demo && !rv$impersonating)
          div(class = "demo-banner",
              "\U0001f50d Demo mode — you're exploring with a fake account. Nothing you do here is saved."),
        div(class = "arc-body",
          tabsetPanel(id = "arc_tabs", type = "tabs", selected = "Today",
            tabPanel("Today",        br(), uiOutput("today_tab")),
            tabPanel("Job Market",   br(), uiOutput("job_market_tab")),
            tabPanel("Games & Demos", br(), uiOutput("games_tab")),
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
  # Skipped in sandbox mode: the global lookup_token uses the production DB, so a
  # stale cookie would log in a non-admin user and dismiss the login form before
  # ?demo_as= JS can fill it. Demo mode always requires an explicit login.
  observeEvent(input$auth_cookie, {
    if (rv$authed) return()
    if (.sandbox) return()
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
    if (!isTRUE(as.integer(row$is_demo[1] %||% 0L) == 1L) && !.sandbox)
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
      t4 <- tryCatch(db_query("SELECT MAX(created_at || COALESCE(committed_at,'')) ts FROM live_score_events;")$ts[1] %||% "", error=function(e)"")
      t5 <- tryCatch(db_query("SELECT MAX(updated_at) ts FROM assignment_reveals;")$ts[1] %||% "", error=function(e)"")
      t6 <- tryCatch(db_query("SELECT COUNT(*) || '-' || COALESCE(MAX(updated_at),'') ts FROM volunteer_demand;")$ts[1] %||% "", error=function(e)"")
      paste(t1, t2, t3, t4, t5, t6)
    },
    valueFunc = function() {
      if (!isTRUE(rv$is_admin)) return(list(
        students=data.frame(), subs=data.frame(), assignments=data.frame(),
        round=data.frame(), revealed=FALSE, section_reveals=data.frame(),
        pending_scores=data.frame()))
      students <- tryCatch(db_query(
        "SELECT u.user_id, u.display_name, u.section,
                COALESCE(SUM(CASE WHEN tl.earning=1 AND tl.amount>0 THEN tl.amount ELSE 0 END),0) AS tokens_earned,
                COALESCE(SUM(tl.amount),0) AS tokens_on_hand
         FROM users u
         LEFT JOIN token_ledger tl ON tl.user_id=u.user_id
         WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1 AND COALESCE(u.is_demo,0)=0
         GROUP BY u.user_id ORDER BY u.section, u.display_name;"),
        error = function(e) data.frame())
      ja_cols <- tryCatch(db_query("PRAGMA table_info(job_assignments);")$name,
                          error = function(e) character(0))
      lse_cols <- tryCatch(db_query("PRAGMA table_info(live_score_events);")$name,
                           error = function(e) character(0))
      if (nrow(students)) {
        students$tokens_pending <- 0
        if (all(c("tokens_awarded", "tokens_credited") %in% ja_cols)) {
          pending <- tryCatch(db_query(
            "SELECT user_id, SUM(COALESCE(tokens_awarded,0)) AS tokens_pending
             FROM job_assignments
             WHERE COALESCE(tokens_credited,1)=0 AND COALESCE(tokens_awarded,0)>0
             GROUP BY user_id;"),
            error = function(e) data.frame())
          if (nrow(pending)) {
            pend_map <- setNames(as.numeric(pending$tokens_pending %||% 0), pending$user_id)
            students$tokens_pending <- as.numeric(pend_map[students$user_id])
            students$tokens_pending[is.na(students$tokens_pending)] <- 0
          }
        }
      }
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
      section_reveals <- if (!is.na(rid)) {
        tryCatch(db_query(
          "SELECT round_id, section, COALESCE(revealed,0) AS revealed,
                  COALESCE(timing,'start') AS timing, updated_at
           FROM assignment_reveals WHERE round_id=?
           ORDER BY section;", list(rid)),
          error = function(e) data.frame())
      } else data.frame()
      assignments <- if (!is.na(rid)) {
        outcome_expr <- if ("outcome" %in% ja_cols) "COALESCE(ja.outcome,'')" else "''"
        awarded_expr <- if ("tokens_awarded" %in% ja_cols) "COALESCE(ja.tokens_awarded,0)" else "0"
        status_filter <- if ("status" %in% ja_cols) "AND COALESCE(ja.status,'assigned')='assigned'" else ""
        pending_join <- if (all(c("job_assignment_id", "outcome", "tokens", "event_kind", "committed_at") %in% lse_cols)) {
          "LEFT JOIN (
             SELECT lse.job_assignment_id, lse.outcome, lse.tokens
             FROM live_score_events lse
             JOIN (
               SELECT job_assignment_id, MAX(id) AS id
               FROM live_score_events
               WHERE event_kind='assignment' AND committed_at IS NULL
               GROUP BY job_assignment_id
             ) latest ON latest.id=lse.id
           ) pse ON pse.job_assignment_id=ja.id"
        } else ""
        pending_outcome_expr <- if (nzchar(pending_join)) "COALESCE(pse.outcome,'')" else "''"
        pending_tokens_expr  <- if (nzchar(pending_join)) "pse.tokens" else "0"
        tryCatch(db_query(sprintf(
          "SELECT ja.id, ja.user_id, u.display_name, u.section, jp.job_name,
                  ja.assigned_wage,
                  %s AS outcome,
                  %s AS tokens_awarded,
                  %s AS pending_outcome,
                  %s AS pending_tokens
           FROM job_assignments ja
           JOIN users u ON u.user_id=ja.user_id
           JOIN job_posts jp ON jp.id=ja.job_post_id
           %s
            WHERE ja.round_id=? %s
            ORDER BY u.section, u.display_name;",
          outcome_expr, awarded_expr, pending_outcome_expr, pending_tokens_expr,
          pending_join, status_filter), list(rid)),
          error = function(e) data.frame())
      } else data.frame()
      pending_scores <- if (!is.na(rid)) {
        required_lse <- c("id", "round_id", "user_id", "job_assignment_id",
                          "job_post_id", "event_kind", "outcome", "tokens",
                          "created_at", "committed_at")
        if (!all(required_lse %in% lse_cols)) data.frame() else tryCatch(db_query(
          "SELECT lse.id, lse.round_id, lse.user_id, u.display_name, u.section,
                  lse.job_assignment_id, lse.job_post_id, lse.event_kind,
                  lse.outcome, lse.tokens, lse.created_at,
                  COALESCE(jp.job_name, ap.job_name, '') AS job_name
           FROM live_score_events lse
           JOIN users u ON u.user_id=lse.user_id
           LEFT JOIN job_posts jp ON jp.id=lse.job_post_id
           LEFT JOIN job_assignments ja ON ja.id=lse.job_assignment_id
           LEFT JOIN job_posts ap ON ap.id=ja.job_post_id
           WHERE lse.round_id=? AND lse.committed_at IS NULL
           ORDER BY u.section, u.display_name, lse.created_at;", list(rid)),
          error = function(e) data.frame())
      } else data.frame()
      list(students=students, subs=subs, assignments=assignments,
           round=round, revealed=revealed, section_reveals=section_reveals,
           pending_scores=pending_scores)
    }
  )

  # Poll for job market data (Today + Job Market tabs)
  jobs_poll <- reactivePoll(8000, session,
    checkFunc = function() {
      uid <- rv$user_id
      if (is.null(uid)) return("")
      r1 <- tryCatch(
        db_query("SELECT COUNT(*) || '-' || COALESCE(MAX(id),0) ts FROM weekly_rounds;")$ts[1] %||% "",
        error = function(e) "")
      r2 <- tryCatch(
        db_query("SELECT MAX(created_at) ts FROM job_assignments;")$ts[1] %||% "",
        error = function(e) "")
      r3 <- tryCatch(
        db_query("SELECT COUNT(*) || '-' || COALESCE(MAX(id),0) ts FROM job_posts;")$ts[1] %||% "",
        error = function(e) "")
      r4 <- tryCatch(
        db_query("SELECT COUNT(*) || '-' || COALESCE(MAX(updated_at),'') ts FROM volunteer_demand;")$ts[1] %||% "",
        error = function(e) "")
      paste(uid, r1, r2, r3, r4, sep = "|")
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
          WHERE ja.user_id=? AND ja.round_id=? AND COALESCE(ja.status,'assigned')='assigned'
          ORDER BY ja.created_at DESC LIMIT 1;",
        list(uid, rid)), error = function(e) data.frame())

      # Every category with an active post is biddable — including volunteer
      # and cold-call categories, so wage bidding can cover them when it goes
      # into effect later in the semester.
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
          AND COALESCE(jp.in_draw, COALESCE(jc.in_draw,1), 1)=1
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
  # Consume unreleased pending tokens (job_assignments with tokens_credited=0) LIFO.
  # Returns any remainder that wasn't covered by pending tokens.
  consume_pending_tokens <- function(uid, amount) {
    if (amount <= 0) return(0)
    pending <- tryCatch(db_query(
      "SELECT id, tokens_awarded FROM job_assignments
       WHERE user_id=? AND COALESCE(tokens_credited,1)=0 AND COALESCE(tokens_awarded,0)>0
       ORDER BY id DESC;",
      list(uid)), error = function(e) data.frame())
    remaining <- amount
    for (i in seq_len(nrow(pending))) {
      if (remaining <= 0) break
      row <- pending[i, ]
      can_take <- min(remaining, as.numeric(row$tokens_awarded))
      db_exec("UPDATE job_assignments SET tokens_awarded=? WHERE id=?;",
              list(as.numeric(row$tokens_awarded) - can_take, as.integer(row$id)))
      remaining <- remaining - can_take
    }
    remaining
  }

  # Deduct up to amount from a student, consuming pending first then ledger balance.
  # Never goes negative. Returns the amount actually deducted.
  safe_deduct <- function(uid, dname, amount, source_type, note) {
    if (amount <= 0) return(0)
    leftover <- consume_pending_tokens(uid, amount)
    if (leftover > 0) {
      cur_bal <- tryCatch(as.numeric(db_query(
        "SELECT COALESCE(SUM(amount),0) t FROM token_ledger WHERE user_id=?;",
        list(uid))$t[1] %||% 0), error = function(e) 0)
      ledger_deduct <- min(leftover, max(0, cur_bal))
      if (ledger_deduct > 0)
        token_credit(uid, dname, -ledger_deduct, 0L, source_type, note = note)
      amount - leftover + ledger_deduct  # actual total deducted
    } else {
      amount  # all consumed from pending
    }
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
    global_revealed <- isTRUE(as.integer(arc$assignments_revealed[1] %||% 0L) == 1L)
    jp     <- jobs_poll()
    mode   <- if (nrow(jp$round)) jp$round$assignment_mode[1] %||% "random" else "random"
    wage_mode <- identical(mode, "wage_bidding")
    section_revealed <- FALSE
    if (nrow(jp$round)) {
      sec <- trimws(rv$section %||% "")
      if (nzchar(sec)) {
        section_revealed <- tryCatch(
          isTRUE(as.integer(db_query(
            "SELECT COALESCE(revealed,0) v FROM assignment_reveals
             WHERE round_id=? AND section=?;",
            list(jp$round$id[1], sec))$v[1] %||% 0L) == 1L),
          error = function(e) FALSE)
      }
    }
    revealed <- isTRUE(global_revealed || section_revealed)

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
            actionButton("go_to_games", "Go to Games & Demos →", class = "btn btn-sm btn-primary"))
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
          tags$li(tags$strong("Games & Demos"), " — your instructor activates a game for class; play electively or explore the always-on demos."),
          tags$li(tags$strong("Account"), " — track your Flex Pass balance, tokens, pledges, and history.")
        )
      )
    )
  })

  # Navigate to Games tab from Today
  observeEvent(input$go_to_games, {
    updateTabsetPanel(session, "arc_tabs", selected = "Games & Demos")
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

      # Volunteer wages (uniform clearing wage per category in wage-bidding rounds)
      if (nrow(jp$round) &&
          identical(jp$round$assignment_mode[1] %||% "random", "wage_bidding")) {
        vrid <- jp$round$id[1]
        vposts <- tryCatch(db_query(
          "SELECT jp2.job_name, jp2.category_id, jp2.slots,
                  COALESCE(jp2.wage_override, jc.default_wage, 1) AS fallback_wage
           FROM job_posts jp2
           LEFT JOIN job_categories jc ON jc.id=jp2.category_id
           WHERE jp2.round_id=? AND COALESCE(jp2.active,1)=1
             AND (COALESCE(jc.voluntary,0)=1 OR COALESCE(jp2.voluntary,0)=1)
           ORDER BY jp2.display_order, jp2.job_name;", list(vrid)),
          error = function(e) data.frame())
        if (nrow(vposts)) {
          tagList(
            div(class = "sec-label", "Volunteer Wages This Round"),
            div(class = "jm-card",
              tags$p(style = "color:#555;font-size:.83rem;margin:0 0 .4rem;",
                     "Everyone who volunteers for a job earns the same market wage, set by this round's bids."),
              lapply(seq_len(nrow(vposts)), function(vi) {
                vp <- vposts[vi, ]
                cw <- volunteer_clearing_wage(vrid, vp$category_id,
                                              as.integer(vp$slots %||% 1L),
                                              query_fn = db_query)
                w  <- if (!is.na(cw)) cw else as.numeric(vp$fallback_wage %||% 1)
                div(style = "display:flex;justify-content:space-between;font-size:.88rem;padding:.15rem 0;",
                    span(vp$job_name %||% ""),
                    span(style = "font-weight:600;", sprintf("%g tokens", w)))
              })
            )
          )
        }
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

    bl <- bid_lock_status()
    if (bl$locked && !rv$is_admin) {
      return(div(class = "alert alert-warning",
                 tags$strong("Bidding is locked for today's class. "), bl$locked_msg))
    }
    lock_note <- if (bl$enabled)
      div(style = "font-size:.82rem;color:#888;margin-bottom:.5rem;", bl$schedule_label)
    else NULL

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
      return(div(style = "color:#999;", "No job types available for this round."))
    }

    if (mode == "wage_bidding") {
      tagList(
        lock_note,
        tags$p(style = "color:#555;font-size:.88rem;",
               "Enter the minimum wage you'd accept for each job type.",
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
        lock_note,
        tags$p(style = "color:#555;font-size:.88rem;",
               sprintf("Allocate up to %d participation tickets across job types.", tickets_total),
               "More tickets in a job type = higher odds of being assigned there."),
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
    bl <- bid_lock_status()
    if (bl$locked && !rv$is_admin) {
      showNotification(bl$locked_msg, type = "warning"); return()
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
    bl <- bid_lock_status()
    if (bl$locked && !rv$is_admin) {
      showNotification(bl$locked_msg, type = "warning"); return()
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
          "Your instructor activates a game for class; you can also play any game electively. Demos are always available — no active session needed. Click a game row to read how it works."
      ),

      # Active slot
      if (nzchar(active)) {
        div(class = "slot-card",
          div(class = "slot-header",
            "▶ Active Now", span(class = "badge-live", "LIVE"),
            if (isTRUE(rv$is_admin) && !isTRUE(rv$impersonating))
              tags$button(
                style = "float:right;font-size:.75rem;padding:.1rem .45rem;",
                class = "btn btn-xs btn-outline-secondary",
                onclick = "if(confirm('Clear the active game?')) Shiny.setInputValue('clear_active_game_btn',+new Date(),{priority:'event'});",
                "Clear game")
          ),
          uiOutput("active_slot_inner")
        )
      } else {
        div(class = "slot-card",
          div(class = "slot-header", "▶ Active Game Slot"),
          div(class = "no-game", "No game is active right now.")
        )
      },

      # Full game catalog
      div(class = "sec-label", "Games"),
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
      })),

      # Demos section
      div(class = "sec-label", style = "margin-top:1.5rem;", "Demos"),
      tags$p(style = "color:#666;font-size:.85rem;margin-bottom:.75rem;",
             "Interactive demonstrations — always open, no session required."),
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
            sprintf("Spend your tokens on academic benefits. Spendable balance: %d tokens.", as.integer(bal)),
            tags$p(style = "margin:.4rem 0 0;font-size:.85rem;color:#555;",
              tags$b("Note:"),
              " Spending tokens does not reduce your participation grade.",
              " Your participation grade is based on total tokens earned during the semester, not your current balance.")),
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
      max_pts <- get_rw_max_points()
      div(class = "spend-form-box",
        tags$h6(style = "color:#951829;font-weight:700;", "⚖️ Grade Reweight"),
        radioButtons("rw_level", NULL,
          choices  = c("Between categories" = "category",
                       "Between individual assignments" = "assignment"),
          selected = "category", inline = TRUE),
        uiOutput("rw_selectors"),
        sliderInput("rw_points", "Percentage points to move:",
                    min = 1, max = max_pts, value = 1, step = 1),
        uiOutput("rw_cost_preview"),
        uiOutput("rw_grade_preview"),
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

  output$rw_selectors <- renderUI({
    req(rv$authed, identical(rv$spend_mode, "reweight"))
    level <- input$rw_level %||% "category"
    if (level == "assignment") {
      item_choices <- get_all_gradebook_items()
      if (!length(item_choices))
        return(div(style="color:#999;font-size:.88rem;",
                   "No gradebook items defined yet. Add categories in Grades & Gradebook."))
      fluidRow(
        column(5, selectInput("rw_from", "Move weight from:", choices = item_choices)),
        column(5, selectInput("rw_to",   "Move weight to:",   choices = item_choices))
      )
    } else {
      cats_df <- tryCatch(parse_grade_categories(),
                          error = function(e)
                            data.frame(name=c("Homework","Midterm","Final"),
                                       weight=c(33,33,34), stringsAsFactors=FALSE))
      cats <- cats_df$name
      fluidRow(
        column(5, selectInput("rw_from", "Move weight from:", choices = cats)),
        column(5, selectInput("rw_to",   "Move weight to:",   choices = cats))
      )
    }
  })

  output$rw_cost_preview <- renderUI({
    req(rv$authed)
    pts  <- as.integer(input$rw_points %||% 1)
    cost <- rw_cost_for_n(pts) %||% 0
    if (is.na(cost)) cost <- 0
    bal  <- token_bal()
    div(style = "font-size:.86rem;color:#555;margin:.4rem 0 .6rem;",
        sprintf("Cost: %d tokens  ·  Balance: %d  ·  After: %d",
                as.integer(cost), as.integer(bal), as.integer(bal - cost)))
  })

  output$rw_grade_preview <- renderUI({
    req(rv$authed, identical(rv$spend_mode, "reweight"))
    from  <- input$rw_from  %||% ""
    to    <- input$rw_to    %||% ""
    pts   <- as.integer(input$rw_points %||% 1)
    level <- input$rw_level %||% "category"
    if (!nzchar(from) || !nzchar(to) || from == to) return(NULL)

    gb <- compute_student_grade(rv$user_id)
    cats_df <- gb$cats
    if (!nrow(cats_df)) return(NULL)

    # Build revised weights: adjust at category level (for both category and assignment level)
    # For assignment level, the from/to are item names — map them to categories
    if (level == "assignment") {
      items_df <- gb$items
      from_cat <- if (nrow(items_df)) {
        r <- items_df[items_df$item_name == from, , drop=FALSE]
        if (nrow(r)) r$category_name[1] else from
      } else from
      to_cat   <- if (nrow(items_df)) {
        r <- items_df[items_df$item_name == to, , drop=FALSE]
        if (nrow(r)) r$category_name[1] else to
      } else to
    } else {
      from_cat <- from
      to_cat   <- to
    }

    rev_cats <- cats_df
    rev_cats$weight[rev_cats$category == from_cat] <-
      pmax(0, rev_cats$weight[rev_cats$category == from_cat] - pts)
    rev_cats$weight[rev_cats$category == to_cat] <-
      rev_cats$weight[rev_cats$category == to_cat] + pts

    # Compute revised overall
    rev_graded_wt   <- sum(rev_cats$weight[!is.na(cats_df$cat_avg)], na.rm = TRUE)
    rev_graded_cont <- sum(
      rev_cats$weight * ifelse(is.na(cats_df$cat_avg), 0, cats_df$cat_avg) / 100,
      na.rm = TRUE)
    rev_overall <- if (rev_graded_wt > 0) rev_graded_cont / rev_graded_wt * 100 else NA_real_

    fmt_pct <- function(x) if (is.na(x)) span(class="grade-na", "—")
                           else sprintf("%.1f%%", x)

    header_row <- tags$tr(
      tags$th("Category"),
      tags$th("Current wt"),
      tags$th("Revised wt"),
      tags$th("Your score"),
      tags$th("Current pts"),
      tags$th("Revised pts")
    )

    data_rows <- lapply(seq_len(nrow(cats_df)), function(i) {
      cr  <- cats_df[i, ]
      rr  <- rev_cats[i, ]
      wt_changed  <- abs(rr$weight - cr$weight) > 0.001
      cur_cont    <- if (!is.na(cr$cat_avg) && gb$graded_weight > 0)
                       cr$cat_avg * cr$weight / gb$graded_weight
                     else NA_real_
      rev_cont    <- if (!is.na(cr$cat_avg) && rev_graded_wt > 0)
                       cr$cat_avg * rr$weight / rev_graded_wt
                     else NA_real_
      tags$tr(
        tags$td(cr$category),
        tags$td(sprintf("%.4g%%", cr$weight)),
        tags$td(if (wt_changed) span(class="changed", sprintf("%.4g%%", rr$weight))
                else sprintf("%.4g%%", rr$weight)),
        tags$td(fmt_pct(cr$cat_avg)),
        tags$td(fmt_pct(cur_cont)),
        tags$td(fmt_pct(rev_cont))
      )
    })

    cur_grade  <- gb$overall
    delta      <- if (!is.na(rev_overall) && !is.na(cur_grade)) rev_overall - cur_grade else NA_real_
    delta_ui   <- if (is.na(delta)) NULL
                  else if (delta > 0.001) span(class="rw-delta-pos", sprintf(" (+%.2f%%)", delta))
                  else if (delta < -0.001) span(class="rw-delta-neg", sprintf(" (%.2f%%)", delta))
                  else span(style="color:#888;", " (no change)")

    total_row <- tags$tr(class="total-row",
      tags$td(tags$strong("Overall")),
      tags$td(""),
      tags$td(""),
      tags$td(""),
      tags$td(fmt_pct(cur_grade)),
      tags$td(tagList(fmt_pct(rev_overall), delta_ui))
    )

    note <- if (gb$graded_weight < gb$total_weight)
      tags$p(style="font-size:.75rem;color:#aaa;margin:.4rem 0 0;",
             sprintf("Overall shown as average of graded categories (%g%% of total weight).",
                     gb$graded_weight))
    else NULL

    div(class = "rw-preview",
      div(class = "rw-preview-title", "Grade impact preview"),
      div(style = "overflow-x:auto;",
        tags$table(class = "rw-preview-tbl",
          tags$thead(header_row),
          tags$tbody(c(data_rows, list(total_row)))
        )
      ),
      note
    )
  })

  output$account_grade_breakdown <- renderUI({
    req(rv$authed)
    gb <- compute_student_grade(rv$user_id)
    if (!nrow(gb$cats)) return(NULL)

    fmt_pct <- function(x) if (is.na(x)) span(class="grade-na", "—")
                           else sprintf("%.1f%%", x)

    overall_ui <- if (!is.na(gb$overall))
      div(class="grade-overall-row",
        div(class="grade-overall-tile",
          div(class="grade-overall-val", sprintf("%.1f%%", gb$overall)),
          div(class="grade-overall-lbl",
              if (gb$graded_weight < gb$total_weight)
                sprintf("Grade (%.4g%% of total weight graded)", gb$graded_weight)
              else "Overall grade")
        )
      )
    else
      div(style="color:#aaa;font-size:.88rem;margin-bottom:.6rem;", "No grades on file yet.")

    tbl_rows <- lapply(seq_len(nrow(gb$cats)), function(i) {
      cr    <- gb$cats[i, ]
      items <- gb$items[gb$items$cat_idx == i, , drop=FALSE]
      cat_row <- tags$tr(class="cat-row",
        tags$td(cr$category),
        tags$td(sprintf("%.4g%%", cr$weight)),
        tags$td(fmt_pct(cr$cat_avg)),
        tags$td(if (!is.na(cr$contribution)) fmt_pct(cr$contribution) else span(class="grade-na","—"))
      )
      item_rows <- if (cr$weight > 0 && nrow(items) > 0 && any(!is.na(items$grade_pct)))
        lapply(seq_len(nrow(items)), function(j) {
          it   <- items[j, ]
          score_txt <- if (!is.na(it$score) && !is.na(it$max_score))
            sprintf("%.4g / %.4g", it$score, it$max_score)
          else if (!is.na(it$grade_pct)) sprintf("%.1f%%", it$grade_pct)
          else NA_character_
          if (is.na(score_txt)) return(NULL)
          tags$tr(class="item-row",
            tags$td(it$item_name),
            tags$td(""),
            tags$td(fmt_pct(it$grade_pct)),
            tags$td(score_txt)
          )
        })
      else list()
      c(list(cat_row), item_rows)
    })

    total_row <- tags$tr(class="total-row",
      tags$td(tags$strong("Overall")),
      tags$td(sprintf("%.4g%%", gb$total_weight)),
      tags$td(if (!is.na(gb$overall)) tags$strong(sprintf("%.1f%%", gb$overall))
              else span(class="grade-na","—")),
      tags$td("")
    )

    div(class = "grade-section",
      div(class = "sec-label", "Grade Breakdown"),
      overall_ui,
      div(style = "overflow-x:auto;",
        tags$table(class = "grade-tbl",
          tags$thead(tags$tr(
            tags$th("Category / Assignment"),
            tags$th("Weight"),
            tags$th("Score"),
            tags$th("Points")
          )),
          tags$tbody(c(unlist(tbl_rows, recursive=FALSE), list(total_row)))
        )
      )
    )
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
    from  <- input$rw_from %||% ""
    to    <- input$rw_to   %||% ""
    level <- input$rw_level %||% "category"
    if (identical(from, to)) {
      showNotification(
        if (level == "assignment") "From and to assignments must differ."
        else "From and to categories must differ.",
        type = "error"); return()
    }
    pts  <- as.integer(input$rw_points %||% 1)
    cost <- rw_cost_for_n(pts) %||% 0
    if (is.na(cost)) cost <- 0
    bal  <- isolate(token_bal())
    if (cost <= 0) { showNotification("Cost not configured for that point value.", type = "error"); return() }
    if (bal < cost) {
      showNotification(sprintf("Not enough tokens (need %d, have %d).", as.integer(cost), as.integer(bal)),
                       type = "error"); return()
    }
    lid <- token_debit(rv$user_id, rv$name, cost, "grade_reweight", NA,
                       note = sprintf("[%s] %s → %s, %d pt", level, from, to, pts))
    db_exec(
      "INSERT INTO grade_reweight_requests(user_id,from_category,to_category,points,cost,ledger_id,level)
       VALUES(?,?,?,?,?,?,?);",
      list(rv$user_id, from, to, pts, cost, as.integer(lid %||% NA_integer_), level))
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

      uiOutput("account_grade_breakdown"),

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

  # ── Show/hide admin/demo tabs (hidden when impersonating) ────────────────────
  observe({
    show_admin <- isTRUE(rv$is_admin) && !isTRUE(rv$impersonating)
    show_tracker <- (isTRUE(rv$is_admin) || isTRUE(rv$is_demo)) && !isTRUE(rv$impersonating)
    if (show_tracker) showTab("arc_tabs", "Live Tracker") else hideTab("arc_tabs", "Live Tracker")
    if (show_admin)   showTab("arc_tabs", "Settings")    else hideTab("arc_tabs", "Settings")
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
    rv$students_ver <- rv$students_ver + 1L
    showNotification(sprintf("Archived %s.", uid), type = "warning")
  }, ignoreNULL = TRUE)

  observeEvent(input$restore_uid, {
    req(rv$is_admin, !rv$impersonating)
    uid <- trimws(input$restore_uid %||% "")
    if (!nzchar(uid)) return()
    db_exec("UPDATE users SET active=1 WHERE user_id=?;", list(uid))
    rv$students_ver <- rv$students_ver + 1L
    showNotification(sprintf("Restored %s.", uid), type = "message")
  }, ignoreNULL = TRUE)

  user_ref_tables <- c(
    "arcade_sessions", "extension_purchases", "grade_reweight_requests",
    "public_good_contributions", "flex_purchases", "token_ledger",
    "olig_submissions", "olig_payouts", "pledges", "participation_events",
    "live_score_events", "student_grades", "job_assignments",
    "wage_bids", "application_bids"
  )

  update_user_references <- function(old_uid, new_uid, display_name) {
    for (tbl in user_ref_tables) {
      cols <- tryCatch(db_query(sprintf("PRAGMA table_info(%s);", tbl))$name,
                       error = function(e) character(0))
      if ("user_id" %in% cols) {
        db_exec(sprintf("UPDATE %s SET user_id=? WHERE user_id=?;", tbl),
                list(new_uid, old_uid))
      }
    }
    db_exec("UPDATE token_ledger SET display_name=? WHERE user_id=?;",
            list(display_name, new_uid))
  }

  observeEvent(input$edit_student_open, {
    req(rv$is_admin, !rv$impersonating)
    uid <- trimws(input$edit_student_open %||% "")
    if (!nzchar(uid)) return()
    stu <- tryCatch(db_query(
      "SELECT user_id, display_name, section, COALESCE(active,1) AS active
       FROM users
       WHERE user_id=? AND COALESCE(is_admin,0)=0 AND COALESCE(is_demo,0)=0;",
      list(uid)),
      error = function(e) data.frame())
    if (!nrow(stu)) { showNotification("Student not found.", type = "error"); return() }
    showModal(modalDialog(
      title = "Edit Student",
      textInput("edit_student_old_uid", NULL, value = stu$user_id[1]),
      tags$script("$('#edit_student_old_uid').closest('.form-group').hide();"),
      textInput("edit_student_uid", "Username/email:", value = stu$user_id[1]),
      textInput("edit_student_name", "Display name:", value = stu$display_name[1] %||% ""),
      textInput("edit_student_section", "Section:", value = stu$section[1] %||% ""),
      passwordInput("edit_student_pw", "New password (optional):"),
      checkboxInput("edit_student_active", "Active", value = isTRUE(as.integer(stu$active[1] %||% 1L) == 1L)),
      footer = tagList(modalButton("Cancel"), actionButton("save_student_btn", "Save", class = "btn-primary")),
      easyClose = TRUE
    ))
  }, ignoreNULL = TRUE)

  observeEvent(input$save_student_btn, {
    req(rv$is_admin, !rv$impersonating)
    old_uid <- trimws(input$edit_student_old_uid %||% "")
    new_uid <- trimws(input$edit_student_uid %||% "")
    nm <- trimws(input$edit_student_name %||% "")
    sec <- trimws(input$edit_student_section %||% "")
    pw <- input$edit_student_pw %||% ""
    if (!nzchar(old_uid) || !nzchar(new_uid)) {
      showNotification("Username/email is required.", type = "error"); return()
    }
    if (nzchar(pw) && nchar(pw) < 4) {
      showNotification("Password must be at least 4 characters.", type = "error"); return()
    }
    if (!identical(tolower(old_uid), tolower(new_uid))) {
      exists <- tryCatch(db_query(
        "SELECT user_id FROM users WHERE LOWER(user_id)=LOWER(?) AND user_id<>?;",
        list(new_uid, old_uid)),
        error = function(e) data.frame())
      if (nrow(exists)) {
        showNotification("That username/email already exists.", type = "error"); return()
      }
    }
    display_name <- if (nzchar(nm)) nm else new_uid
    db_exec(
      "UPDATE users
       SET user_id=?, display_name=?, section=?, active=?
       WHERE user_id=? AND COALESCE(is_admin,0)=0 AND COALESCE(is_demo,0)=0;",
      list(new_uid, display_name, sec, as.integer(isTRUE(input$edit_student_active)), old_uid))
    update_user_references(old_uid, new_uid, display_name)
    if (nzchar(pw)) {
      db_exec("UPDATE users SET pw_hash=? WHERE user_id=?;",
              list(bcrypt::hashpw(pw), new_uid))
    }
    removeModal()
    rv$students_ver <- rv$students_ver + 1L
    showNotification(sprintf("Updated student %s.", new_uid), type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$create_student_btn, {
    req(rv$is_admin)
    uid <- trimws(input$new_stu_uid %||% "")
    nm  <- trimws(input$new_stu_name %||% "")
    pw  <- input$new_stu_pw %||% ""
    sec <- trimws(input$new_stu_section %||% "")
    if (!nzchar(uid)) {
      showNotification("Username/email is required.", type = "error"); return()
    }
    if (nzchar(pw) && nchar(pw) < 4) {
      showNotification("Password must be at least 4 characters.", type = "error"); return()
    }
    ex <- db_query("SELECT user_id FROM users WHERE LOWER(user_id)=LOWER(?);", list(uid))
    if (nrow(ex)) { showNotification("Username already exists.", type = "error"); return() }
    db_exec(
      "INSERT INTO users(user_id, display_name, pw_hash, is_admin, section, active, is_demo)
       VALUES(?,?,?,0,?,1,0);",
      list(uid, if (nzchar(nm)) nm else uid,
           if (nzchar(pw)) bcrypt::hashpw(pw) else bcrypt::hashpw(make_token()),
           sec))
    rv$students_ver <- rv$students_ver + 1L
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
    rv$students_ver <- rv$students_ver + 1L
    showNotification(sprintf("Password reset for %s.", uid), type = "message")
  })

  output$student_csv_mapper <- renderUI({
    req(rv$is_admin)
    f <- input$upload_students_csv
    if (is.null(f)) return(NULL)
    hdr <- tryCatch(read.csv(f$datapath, nrows = 0, check.names = FALSE),
                    error = function(e) NULL)
    if (is.null(hdr)) {
      return(tags$p(style = "color:#856404;font-size:.85rem;",
                    "Could not read CSV headers. Check the file format."))
    }
    cols <- names(hdr)
    norm_cols <- tolower(trimws(cols))
    guess_col <- function(aliases) {
      hit <- match(aliases, norm_cols)
      if (any(!is.na(hit))) cols[hit[which(!is.na(hit))[1]]] else ""
    }
    optional_choices <- c("Do not import" = "", stats::setNames(cols, cols))
    required_choices <- c("Choose column" = "", stats::setNames(cols, cols))
    tagList(
      tags$p(style = "color:#555;font-size:.85rem;margin-top:.35rem;",
             "Map your CSV columns to the student fields. Password is optional for Google-only accounts."),
      fluidRow(
        column(3, selectInput("student_csv_uid_col", "Username/email:",
                              choices = required_choices,
                              selected = guess_col(c("username","user_id","userid","login","email")))),
        column(3, selectInput("student_csv_name_col", "Display name:",
                              choices = optional_choices,
                              selected = guess_col(c("display_name","name","fullname","full_name")))),
        column(3, selectInput("student_csv_section_col", "Section:",
                              choices = optional_choices,
                              selected = guess_col(c("section","class","group")))),
        column(3, selectInput("student_csv_pw_col", "Password:",
                              choices = optional_choices,
                              selected = guess_col(c("password","pw","pass"))))
      )
    )
  })

  observeEvent(input$bulk_upload_students_btn, {
    req(rv$is_admin)
    f <- input$upload_students_csv
    if (is.null(f)) { showNotification("Choose a CSV file first.", type = "error"); return() }
    do_update <- isTRUE(input$upload_stu_update)
    df <- tryCatch(read.csv(f$datapath, stringsAsFactors = FALSE, colClasses = "character",
                            check.names = FALSE),
                   error = function(e) { showNotification(paste("CSV error:", e$message), type = "error"); NULL })
    if (is.null(df)) return()
    cols <- names(df)
    norm_cols <- tolower(trimws(cols))
    guess_col <- function(aliases) {
      hit <- match(aliases, norm_cols)
      if (any(!is.na(hit))) cols[hit[which(!is.na(hit))[1]]] else ""
    }
    valid_col <- function(x) nzchar(x %||% "") && (x %in% cols)
    uid_col  <- input$student_csv_uid_col %||% guess_col(c("username","user_id","userid","login","email"))
    name_col <- input$student_csv_name_col %||% guess_col(c("display_name","name","fullname","full_name"))
    sec_col  <- input$student_csv_section_col %||% guess_col(c("section","class","group"))
    pw_col   <- input$student_csv_pw_col %||% guess_col(c("password","pw","pass"))
    if (!valid_col(uid_col)) {
      showNotification("Choose the CSV column containing each student's username/email.", type = "error")
      return()
    }
    if (!valid_col(name_col)) name_col <- ""
    if (!valid_col(sec_col))  sec_col  <- ""
    if (!valid_col(pw_col))   pw_col   <- ""
    locked_pw_hash <- bcrypt::hashpw(make_token())
    n_created <- 0L; n_updated <- 0L; n_skipped <- 0L; n_bad_pw <- 0L
    for (i in seq_len(nrow(df))) {
      uid  <- trimws(df[[uid_col]][i] %||% "")
      nm   <- if (nzchar(name_col)) trimws(df[[name_col]][i] %||% "") else ""
      sec  <- if (nzchar(sec_col))  trimws(df[[sec_col]][i]  %||% "") else ""
      pw   <- if (nzchar(pw_col))   trimws(df[[pw_col]][i]   %||% "") else ""
      if (!nzchar(uid)) next
      exists <- nrow(db_query("SELECT user_id FROM users WHERE LOWER(user_id)=LOWER(?);", list(uid))) > 0
      if (!exists) {
        if (nzchar(pw) && nchar(pw) < 4) { n_bad_pw <- n_bad_pw + 1L; next }
        db_exec(
          "INSERT INTO users(user_id,display_name,pw_hash,is_admin,section,active,is_demo)
           VALUES(?,?,?,0,?,1,0);",
          list(uid, if (nzchar(nm)) nm else uid,
               if (nzchar(pw)) bcrypt::hashpw(pw) else locked_pw_hash,
               if (nzchar(sec)) sec else NA_character_))
        n_created <- n_created + 1L
      } else if (do_update) {
        if (nzchar(nm))
          db_exec("UPDATE users SET display_name=? WHERE LOWER(user_id)=LOWER(?);", list(nm, uid))
        if (nzchar(sec))
          db_exec("UPDATE users SET section=? WHERE LOWER(user_id)=LOWER(?);", list(sec, uid))
        if (nzchar(pw) && nchar(pw) >= 4)
          db_exec("UPDATE users SET pw_hash=? WHERE LOWER(user_id)=LOWER(?);",
                  list(bcrypt::hashpw(pw), uid))
        n_updated <- n_updated + 1L
      } else {
        n_skipped <- n_skipped + 1L
      }
    }
    parts <- character(0)
    if (n_created > 0) parts <- c(parts, sprintf("%d created",  n_created))
    if (n_updated > 0) parts <- c(parts, sprintf("%d updated",  n_updated))
    if (n_skipped > 0) parts <- c(parts, sprintf("%d skipped (already exist)", n_skipped))
    if (n_bad_pw  > 0) parts <- c(parts, sprintf("%d skipped (password shorter than 4 chars)", n_bad_pw))
    showNotification(paste("Upload complete:", paste(parts, collapse = ", ")), type = "message",
                     duration = 8)
  })

  # ── Job management ────────────────────────────────────────────────────────────
  observeEvent(input$add_job_cat_btn, {
    req(rv$is_admin)
    nm   <- trimws(input$new_cat_name %||% "")
    wage <- as.numeric(input$new_cat_wage %||% 10)
    desc <- trimws(input$new_cat_desc %||% "")
    if (!nzchar(nm)) { showNotification("Category name required.", type = "error"); return() }
    vol2  <- as.integer(isTRUE(input$new_cat_voluntary))
    idraw <- as.integer(!isTRUE(input$new_cat_not_in_draw))
    db_exec(
      "INSERT INTO job_categories(name, default_wage, description, voluntary, in_draw) VALUES(?,?,?,?,?);",
      list(nm, if (is.na(wage)) 10 else wage, desc, vol2, idraw))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Job category added.", type = "message")
  })

  observeEvent(input$add_job_post_btn, {
    req(rv$is_admin)
    nm      <- trimws(input$new_post_name %||% "")
    cat_id  <- suppressWarnings(as.integer(input$new_post_cat %||% 0))
    slots   <- max(1L, as.integer(input$new_post_slots %||% 1L))
    wage    <- suppressWarnings(as.numeric(input$new_post_wage))
    in_draw <- as.integer(isTRUE(input$new_post_in_draw))
    timing  <- input$new_post_timing %||% "any"
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("Create a round first.", type = "error"); return() }
    if (!nzchar(nm)) { showNotification("Post name required.", type = "error"); return() }
    rid <- rid_row$id[1]
    db_exec(
      "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override, in_draw, selection_time)
       VALUES(?,?,?,?,?,?,?);",
      list(rid, nm,
           if (!is.na(cat_id) && cat_id > 0) cat_id else NA_integer_,
           slots,
           if (!is.null(wage) && !is.na(wage) && wage > 0) wage else NA_real_,
           in_draw, timing))
    rv$jobs_ver <- rv$jobs_ver + 1L
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
      "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override, voluntary, selection_time)
       VALUES(?,?,?,?,?,1,'volunteer');",
      list(rid, nm,
           if (!is.na(cat_id) && cat_id > 0) cat_id else NA_integer_,
           slots,
           if (!is.null(tokens) && !is.na(tokens) && tokens >= 0) tokens else NA_real_))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Participation type added to current round.", type = "message")
  })

  observeEvent(input$add_template_btn, {
    req(rv$is_admin)
    nm    <- trimws(input$new_tpl_name %||% "")
    cat_id <- suppressWarnings(as.integer(input$new_tpl_cat %||% 0))
    slots  <- max(1L, as.integer(input$new_tpl_slots %||% 1L))
    wage   <- suppressWarnings(as.numeric(input$new_tpl_wage))
    timing <- input$new_tpl_timing %||% "any"
    vol    <- as.integer(isTRUE(input$new_tpl_voluntary))
    idraw  <- as.integer(!isTRUE(input$new_tpl_not_in_draw))
    if (!nzchar(nm)) { showNotification("Template name required.", type = "error"); return() }
    db_exec(
      "INSERT INTO job_templates(name, category_id, slots, suggested_wage, selection_time, voluntary, in_draw)
       VALUES(?,?,?,?,?,?,?);",
      list(nm,
           if (!is.na(cat_id) && cat_id > 0) cat_id else NA_integer_,
           slots,
           if (!is.null(wage) && !is.na(wage) && wage >= 0) wage else NA_real_,
           timing, vol, idraw))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Template added.", type = "message")
  })

  job_category_choices <- function(selected_id = NULL) {
    cats <- tryCatch(db_query(
      "SELECT id, name FROM job_categories
       WHERE lower(name) IN ('class roles','volunteer','cold call')
       ORDER BY display_order, name;"),
      error = function(e) data.frame())
    choices <- if (nrow(cats)) setNames(cats$id, cats$name) else c("(no categories)" = "")
    if (!is.null(selected_id) && !is.na(selected_id) &&
        !as.character(selected_id) %in% as.character(unname(choices))) {
      cur <- tryCatch(db_query("SELECT id, name FROM job_categories WHERE id=?;", list(selected_id)),
                      error = function(e) data.frame())
      if (nrow(cur)) choices <- c(setNames(cur$id, paste0(cur$name, " (old)")), choices)
    }
    choices
  }

  job_timing_choices <- c("Any" = "any", "Start" = "start",
                          "During (cold call)" = "during",
                          "End" = "end", "Volunteer" = "volunteer")

  observeEvent(input$edit_job_post_open, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$edit_job_post_open %||% 0))
    if (is.na(pid) || pid <= 0) return()
    post <- tryCatch(db_query(
      "SELECT jp.id, jp.job_name, jp.category_id, jp.slots,
              COALESCE(jp.wage_override, jc.default_wage, 0) AS wage,
              COALESCE(jp.in_draw,1) AS in_draw,
              COALESCE(jp.voluntary,0) AS voluntary,
              COALESCE(jp.active,1) AS active,
              COALESCE(NULLIF(jp.selection_time,''),'any') AS selection_time
       FROM job_posts jp
       LEFT JOIN job_categories jc ON jc.id=jp.category_id
       WHERE jp.id=?;",
      list(pid)),
      error = function(e) data.frame())
    if (!nrow(post)) { showNotification("Job post not found.", type = "error"); return() }
    showModal(modalDialog(
      title = "Edit Job Post",
      numericInput("edit_post_id", NULL, value = pid, min = 1, step = 1),
      tags$script("$('#edit_post_id').closest('.form-group').hide();"),
      textInput("edit_post_name", "Name:", value = post$job_name[1] %||% ""),
      selectInput("edit_post_cat", "Category:", choices = job_category_choices(post$category_id[1]),
                  selected = post$category_id[1]),
      fluidRow(
        column(4, numericInput("edit_post_slots", "Slots:", value = as.integer(post$slots[1] %||% 1L), min = 1, step = 1)),
        column(4, numericInput("edit_post_wage", "Wage:", value = as.numeric(post$wage[1] %||% 0), min = 0, step = 1)),
        column(4, selectInput("edit_post_timing", "Timing:", choices = job_timing_choices,
                              selected = post$selection_time[1] %||% "any"))
      ),
      fluidRow(
        column(4, checkboxInput("edit_post_in_draw", "In draw", value = isTRUE(as.integer(post$in_draw[1]) == 1L))),
        column(4, checkboxInput("edit_post_voluntary", "Voluntary", value = isTRUE(as.integer(post$voluntary[1]) == 1L))),
        column(4, checkboxInput("edit_post_active", "Active", value = isTRUE(as.integer(post$active[1]) == 1L)))
      ),
      footer = tagList(modalButton("Cancel"), actionButton("save_job_post_btn", "Save", class = "btn-primary")),
      easyClose = TRUE
    ))
  }, ignoreNULL = TRUE)

  observeEvent(input$save_job_post_btn, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$edit_post_id %||% 0))
    nm <- trimws(input$edit_post_name %||% "")
    cat_id <- suppressWarnings(as.integer(input$edit_post_cat %||% 0))
    slots <- max(1L, as.integer(input$edit_post_slots %||% 1L))
    wage <- suppressWarnings(as.numeric(input$edit_post_wage %||% 0))
    timing <- input$edit_post_timing %||% "any"
    if (is.na(pid) || pid <= 0 || !nzchar(nm)) {
      showNotification("Name is required.", type = "error"); return()
    }
    db_exec(
      "UPDATE job_posts
       SET job_name=?, category_id=?, slots=?, wage_override=?, selection_time=?,
           in_draw=?, voluntary=?, active=?
       WHERE id=?;",
      list(nm, if (!is.na(cat_id) && cat_id > 0) cat_id else NA_integer_,
           slots, if (!is.na(wage)) wage else NA_real_, timing,
           as.integer(isTRUE(input$edit_post_in_draw)),
           as.integer(isTRUE(input$edit_post_voluntary)),
           as.integer(isTRUE(input$edit_post_active)), pid))
    removeModal()
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Job post updated.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$edit_template_open, {
    req(rv$is_admin)
    tid <- suppressWarnings(as.integer(input$edit_template_open %||% 0))
    if (is.na(tid) || tid <= 0) return()
    tpl <- tryCatch(db_query(
      "SELECT id, name, category_id, slots, suggested_wage,
              COALESCE(active,1) AS active,
              COALESCE(voluntary,0) AS voluntary,
              COALESCE(in_draw,1) AS in_draw,
              COALESCE(NULLIF(selection_time,''),'any') AS selection_time
       FROM job_templates
       WHERE id=?;",
      list(tid)),
      error = function(e) data.frame())
    if (!nrow(tpl)) { showNotification("Template not found.", type = "error"); return() }
    showModal(modalDialog(
      title = "Edit Template",
      numericInput("edit_tpl_id", NULL, value = tid, min = 1, step = 1),
      tags$script("$('#edit_tpl_id').closest('.form-group').hide();"),
      textInput("edit_tpl_name", "Name:", value = tpl$name[1] %||% ""),
      selectInput("edit_tpl_cat", "Category:", choices = job_category_choices(tpl$category_id[1]),
                  selected = tpl$category_id[1]),
      fluidRow(
        column(4, numericInput("edit_tpl_slots", "Slots:", value = as.integer(tpl$slots[1] %||% 1L), min = 1, step = 1)),
        column(4, numericInput("edit_tpl_wage", "Suggested wage:", value = as.numeric(tpl$suggested_wage[1] %||% 0), min = 0, step = 1)),
        column(4, selectInput("edit_tpl_timing", "Timing:", choices = job_timing_choices,
                              selected = tpl$selection_time[1] %||% "any"))
      ),
      fluidRow(
        column(4, checkboxInput("edit_tpl_in_draw", "In draw", value = isTRUE(as.integer(tpl$in_draw[1]) == 1L))),
        column(4, checkboxInput("edit_tpl_voluntary", "Voluntary", value = isTRUE(as.integer(tpl$voluntary[1]) == 1L))),
        column(4, checkboxInput("edit_tpl_active", "Auto-copy", value = isTRUE(as.integer(tpl$active[1]) == 1L)))
      ),
      footer = tagList(modalButton("Cancel"), actionButton("save_template_btn", "Save", class = "btn-primary")),
      easyClose = TRUE
    ))
  }, ignoreNULL = TRUE)

  observeEvent(input$save_template_btn, {
    req(rv$is_admin)
    tid <- suppressWarnings(as.integer(input$edit_tpl_id %||% 0))
    nm <- trimws(input$edit_tpl_name %||% "")
    cat_id <- suppressWarnings(as.integer(input$edit_tpl_cat %||% 0))
    slots <- max(1L, as.integer(input$edit_tpl_slots %||% 1L))
    wage <- suppressWarnings(as.numeric(input$edit_tpl_wage %||% 0))
    timing <- input$edit_tpl_timing %||% "any"
    if (is.na(tid) || tid <= 0 || !nzchar(nm)) {
      showNotification("Name is required.", type = "error"); return()
    }
    db_exec(
      "UPDATE job_templates
       SET name=?, category_id=?, slots=?, suggested_wage=?, selection_time=?,
           in_draw=?, voluntary=?, active=?
       WHERE id=?;",
      list(nm, if (!is.na(cat_id) && cat_id > 0) cat_id else NA_integer_,
           slots, if (!is.na(wage)) wage else NA_real_, timing,
           as.integer(isTRUE(input$edit_tpl_in_draw)),
           as.integer(isTRUE(input$edit_tpl_voluntary)),
           as.integer(isTRUE(input$edit_tpl_active)), tid))
    removeModal()
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Template updated.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$remove_template_btn, {
    req(rv$is_admin)
    tid <- suppressWarnings(as.integer(input$remove_template_btn %||% 0))
    if (is.na(tid) || tid <= 0) return()
    db_exec("DELETE FROM job_templates WHERE id=?;", list(tid))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Template deleted.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$toggle_template_active, {
    req(rv$is_admin)
    tid <- suppressWarnings(as.integer(input$toggle_template_active %||% 0))
    if (is.na(tid) || tid <= 0) return()
    cur <- db_query("SELECT COALESCE(active,1) v FROM job_templates WHERE id=?;", list(tid))
    if (!nrow(cur)) return()
    new_v <- if (isTRUE(as.integer(cur$v[1]) == 1L)) 0L else 1L
    db_exec("UPDATE job_templates SET active=? WHERE id=?;", list(new_v, tid))
    rv$jobs_ver <- rv$jobs_ver + 1L
  }, ignoreNULL = TRUE)

  observeEvent(input$toggle_template_voluntary, {
    req(rv$is_admin)
    tid <- suppressWarnings(as.integer(input$toggle_template_voluntary %||% 0))
    if (is.na(tid) || tid <= 0) return()
    cur <- db_query("SELECT COALESCE(voluntary,0) v FROM job_templates WHERE id=?;", list(tid))
    if (!nrow(cur)) return()
    new_v <- if (isTRUE(as.integer(cur$v[1]) == 1L)) 0L else 1L
    db_exec("UPDATE job_templates SET voluntary=? WHERE id=?;", list(new_v, tid))
    rv$jobs_ver <- rv$jobs_ver + 1L
  }, ignoreNULL = TRUE)

  observeEvent(input$toggle_template_in_draw, {
    req(rv$is_admin)
    tid <- suppressWarnings(as.integer(input$toggle_template_in_draw %||% 0))
    if (is.na(tid) || tid <= 0) return()
    cur <- db_query("SELECT COALESCE(in_draw,1) v FROM job_templates WHERE id=?;", list(tid))
    if (!nrow(cur)) return()
    new_v <- if (isTRUE(as.integer(cur$v[1]) == 1L)) 0L else 1L
    db_exec("UPDATE job_templates SET in_draw=? WHERE id=?;", list(new_v, tid))
    rv$jobs_ver <- rv$jobs_ver + 1L
  }, ignoreNULL = TRUE)

  observeEvent(input$cycle_template_timing_btn, {
    req(rv$is_admin)
    tid <- suppressWarnings(as.integer(input$cycle_template_timing_btn %||% 0))
    if (is.na(tid) || tid <= 0) return()
    cur <- db_query("SELECT COALESCE(NULLIF(selection_time,''),'any') v FROM job_templates WHERE id=?;", list(tid))
    if (!nrow(cur)) return()
    new_v <- switch(as.character(cur$v[1] %||% "any"),
                    any = "start", start = "during", during = "end",
                    end = "volunteer", volunteer = "any", "any")
    db_exec("UPDATE job_templates SET selection_time=? WHERE id=?;", list(new_v, tid))
    rv$jobs_ver <- rv$jobs_ver + 1L
  }, ignoreNULL = TRUE)

  observeEvent(input$apply_clearing_wage_btn, {
    req(rv$is_admin)
    ev <- input$apply_clearing_wage_btn
    if (is.null(ev) || is.null(ev$post_id) || is.null(ev$wage)) return()
    post_id <- suppressWarnings(as.integer(ev$post_id))
    wage    <- suppressWarnings(as.numeric(ev$wage))
    if (is.na(post_id) || post_id <= 0 || is.na(wage)) return()
    db_exec("UPDATE job_posts SET wage_override=? WHERE id=?;", list(wage, post_id))
    rv$jobs_ver <- rv$jobs_ver + 1L
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
    rv$jobs_ver <- rv$jobs_ver + 1L
  }, ignoreNULL = TRUE)

  observeEvent(input$toggle_post_in_draw, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$toggle_post_in_draw %||% 0))
    if (is.na(pid) || pid <= 0) return()
    cur <- db_query("SELECT COALESCE(in_draw,1) v FROM job_posts WHERE id=?;", list(pid))
    if (!nrow(cur)) return()
    new_v <- if (isTRUE(as.integer(cur$v[1]) == 1L)) 0L else 1L
    db_exec("UPDATE job_posts SET in_draw=? WHERE id=?;", list(new_v, pid))
    rv$jobs_ver <- rv$jobs_ver + 1L
  }, ignoreNULL = TRUE)

  .commit_live_score_event <- function(ev) {
    tokens_to_award <- as.numeric(ev$tokens %||% 0)
    outcome <- as.character(ev$outcome %||% "")
    rid <- as.integer(ev$round_id[1])
    uid <- as.character(ev$user_id[1])
    dname <- ev$display_name[1] %||% uid
    rnd_row <- tryCatch(db_query(
      "SELECT COALESCE(tokens_revealed,1) v FROM weekly_rounds WHERE id=?;",
      list(rid)), error=function(e) data.frame())
    tokens_revealed <- if (nrow(rnd_row)) isTRUE(as.integer(rnd_row$v[1]) == 1L) else TRUE

    if (identical(as.character(ev$event_kind[1]), "assignment")) {
      assign_id <- as.integer(ev$job_assignment_id[1])
      cur <- tryCatch(db_query(
        "SELECT COALESCE(tokens_awarded,0) AS tokens_awarded FROM job_assignments WHERE id=?;",
        list(assign_id)), error=function(e) data.frame())
      if (!nrow(cur) || as.integer(cur$tokens_awarded[1] %||% 0L) == 1L) {
        db_exec("UPDATE live_score_events SET committed_at=datetime('now') WHERE id=?;",
                list(as.integer(ev$id[1])))
        return(FALSE)
      }
      db_exec(
        "UPDATE job_assignments SET outcome=?, tokens_awarded=?, tokens_credited=?,
                updated_at=datetime('now') WHERE id=?;",
        list(outcome, tokens_to_award, if (tokens_revealed) 1L else 0L, assign_id))
      if (tokens_to_award > 0 && tokens_revealed) {
        token_credit(uid, dname, tokens_to_award, 1L, "job", assign_id,
                     note = sprintf("Job wage (%s)", outcome))
      }
    } else {
      post_id <- as.integer(ev$job_post_id[1])
      wage_val <- tokens_to_award
      if (outcome %in% c("try", "miss")) {
        post_row <- tryCatch(db_query(
          "SELECT COALESCE(jp.wage_override, jc.default_wage, 1) AS tokens
           FROM job_posts jp LEFT JOIN job_categories jc ON jc.id = jp.category_id
           WHERE jp.id=? LIMIT 1;", list(post_id)),
          error=function(e) data.frame())
        if (nrow(post_row)) wage_val <- as.numeric(post_row$tokens[1] %||% tokens_to_award)
      }
      db_exec(
        "INSERT INTO job_assignments(round_id, user_id, job_post_id, assigned_wage,
                assignment_mode, outcome, tokens_awarded, tokens_credited, updated_at)
         VALUES(?,?,?,?,'voluntary',?,?,?,datetime('now'))
         ON CONFLICT(round_id, user_id)
         DO UPDATE SET job_post_id=excluded.job_post_id,
                       assigned_wage=excluded.assigned_wage,
                       outcome=excluded.outcome,
                       tokens_awarded=excluded.tokens_awarded,
                       tokens_credited=excluded.tokens_credited,
                       updated_at=excluded.updated_at;",
        list(rid, uid, post_id, wage_val, outcome,
             tokens_to_award, if (tokens_revealed) 1L else 0L))
      if (tokens_to_award > 0 && tokens_revealed) {
        token_credit(uid, dname, tokens_to_award, 1L, "participation", post_id,
                     note = sprintf("Participation (%s)", outcome))
      }
    }
    db_exec("UPDATE live_score_events SET committed_at=datetime('now') WHERE id=?;",
            list(as.integer(ev$id[1])))
    TRUE
  }

  observeEvent(input$drop_live_score_btn, {
    req(rv$is_admin, !rv$impersonating)
    eid <- suppressWarnings(as.integer(input$drop_live_score_btn %||% 0))
    if (is.na(eid) || eid <= 0) return()
    db_exec("DELETE FROM live_score_events WHERE id=? AND committed_at IS NULL;", list(eid))
    showNotification("Pending score removed.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$clear_live_scores_btn, {
    req(rv$is_admin, !rv$impersonating)
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error=function(e) data.frame())
    if (!nrow(rid_row)) return()
    cur_sec <- trimws(rv$active_section %||% "")
    if (nzchar(cur_sec)) {
      db_exec(
        "DELETE FROM live_score_events
         WHERE round_id=? AND committed_at IS NULL
           AND user_id IN (SELECT user_id FROM users WHERE section=?);",
        list(rid_row$id[1], cur_sec))
    } else {
      db_exec("DELETE FROM live_score_events WHERE round_id=? AND committed_at IS NULL;",
              list(rid_row$id[1]))
    }
    showNotification("Pending scores cleared.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$commit_live_scores_btn, {
    req(rv$is_admin, !rv$impersonating)
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error=function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("No active round.", type = "error"); return() }
    cur_sec <- trimws(rv$active_section %||% "")
    pending <- if (nzchar(cur_sec)) {
      tryCatch(db_query(
        "SELECT lse.*, u.display_name
         FROM live_score_events lse JOIN users u ON u.user_id=lse.user_id
         WHERE lse.round_id=? AND lse.committed_at IS NULL AND u.section=?
         ORDER BY lse.id;", list(rid_row$id[1], cur_sec)),
        error=function(e) data.frame())
    } else {
      tryCatch(db_query(
        "SELECT lse.*, u.display_name
         FROM live_score_events lse JOIN users u ON u.user_id=lse.user_id
         WHERE lse.round_id=? AND lse.committed_at IS NULL
         ORDER BY lse.id;", list(rid_row$id[1])),
        error=function(e) data.frame())
    }
    if (!nrow(pending)) { showNotification("No pending live scores.", type = "message"); return() }
    applied <- 0L
    for (i in seq_len(nrow(pending))) {
      if (isTRUE(.commit_live_score_event(pending[i, ]))) applied <- applied + 1L
    }
    showNotification(sprintf("Committed %d live score%s.", applied, if (applied == 1L) "" else "s"),
                     type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$toggle_post_voluntary, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$toggle_post_voluntary %||% 0))
    if (is.na(pid) || pid <= 0) return()
    cur <- db_query("SELECT COALESCE(voluntary,0) v FROM job_posts WHERE id=?;", list(pid))
    if (!nrow(cur)) return()
    new_v <- if (isTRUE(as.integer(cur$v[1]) == 1L)) 0L else 1L
    db_exec("UPDATE job_posts SET voluntary=? WHERE id=?;", list(new_v, pid))
    rv$jobs_ver <- rv$jobs_ver + 1L
  }, ignoreNULL = TRUE)

  observeEvent(input$cycle_post_timing_btn, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$cycle_post_timing_btn %||% 0))
    if (is.na(pid) || pid <= 0) return()
    cur <- db_query("SELECT COALESCE(NULLIF(selection_time,''),'any') v FROM job_posts WHERE id=?;", list(pid))
    if (!nrow(cur)) return()
    new_v <- switch(as.character(cur$v[1] %||% "any"),
                    any = "start", start = "during", during = "end",
                    end = "volunteer", volunteer = "any", "any")
    db_exec("UPDATE job_posts SET selection_time=? WHERE id=?;", list(new_v, pid))
    rv$jobs_ver <- rv$jobs_ver + 1L
  }, ignoreNULL = TRUE)

  observeEvent(input$delete_job_post_btn, {
    req(rv$is_admin)
    pid <- suppressWarnings(as.integer(input$delete_job_post_btn %||% 0))
    if (is.na(pid) || pid <= 0) return()
    db_exec("DELETE FROM job_assignments WHERE job_post_id=?;", list(pid))
    db_exec("DELETE FROM job_posts WHERE id=?;", list(pid))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Job post deleted.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$delete_job_cat_btn, {
    req(rv$is_admin)
    cid <- suppressWarnings(as.integer(input$delete_job_cat_btn %||% 0))
    if (is.na(cid) || cid <= 0) return()
    db_exec("UPDATE job_posts SET category_id=NULL WHERE category_id=?;", list(cid))
    db_exec("DELETE FROM job_categories WHERE id=?;", list(cid))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Category deleted.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$delete_round_btn, {
    req(rv$is_admin)
    rid <- suppressWarnings(as.integer(input$delete_round_btn %||% 0))
    if (is.na(rid) || rid <= 0) return()
    db_exec("DELETE FROM job_assignments WHERE round_id=?;", list(rid))
    tryCatch(db_exec("DELETE FROM wage_bids WHERE round_id=?;", list(rid)), error = function(e) NULL)
    db_exec("DELETE FROM job_posts WHERE round_id=?;", list(rid))
    db_exec("DELETE FROM weekly_rounds WHERE id=?;", list(rid))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Round deleted.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$unassign_job_btn, {
    req(rv$is_admin)
    aid <- suppressWarnings(as.integer(input$unassign_job_btn %||% 0))
    if (is.na(aid) || aid <= 0) return()
    db_exec("DELETE FROM job_assignments WHERE id=?;", list(aid))
    showNotification("Assignment removed.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$manual_add_assignment_btn, {
    req(rv$is_admin)
    round <- tryCatch(db_query("SELECT id, assignment_mode FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                      error = function(e) data.frame())
    if (!nrow(round)) { showNotification("No active round.", type = "error"); return() }
    rid <- as.integer(round$id[1])
    uid <- trimws(input$manual_assign_uid %||% "")
    post_id <- suppressWarnings(as.integer(input$manual_assign_post_id %||% 0))
    if (!nzchar(uid) || is.na(post_id) || post_id <= 0) {
      showNotification("Pick a student and job first.", type = "warning")
      return()
    }
    stu <- tryCatch(db_query(
      "SELECT user_id, display_name
       FROM users
       WHERE user_id=? AND COALESCE(active,1)=1 AND COALESCE(is_admin,0)=0
         AND COALESCE(is_demo,0)=0
       LIMIT 1;",
      list(uid)),
      error = function(e) data.frame())
    if (!nrow(stu)) { showNotification("Student is not active.", type = "error"); return() }
    post <- tryCatch(db_query(
      "SELECT jp.id, jp.job_name, COALESCE(jp.wage_override, jc.default_wage, 0) AS wage
       FROM job_posts jp
       LEFT JOIN job_categories jc ON jc.id=jp.category_id
       WHERE jp.id=? AND jp.round_id=? AND COALESCE(jp.active,1)=1
       LIMIT 1;",
      list(post_id, rid)),
      error = function(e) data.frame())
    if (!nrow(post)) { showNotification("Job is not active for the current round.", type = "error"); return() }

    old <- tryCatch(db_query(
      "SELECT id FROM job_assignments WHERE round_id=? AND user_id=? LIMIT 1;",
      list(rid, uid)),
      error = function(e) data.frame())
    if (nrow(old)) {
      db_exec("DELETE FROM live_score_events WHERE job_assignment_id=? AND committed_at IS NULL;",
              list(as.integer(old$id[1])))
    }
    db_exec(
      "INSERT INTO job_assignments(round_id, user_id, job_post_id, assigned_wage,
              assignment_mode, status, outcome, tokens_awarded, tokens_credited, updated_at)
       VALUES(?,?,?,?,?,'assigned','',0,1,datetime('now'))
       ON CONFLICT(round_id, user_id)
       DO UPDATE SET job_post_id=excluded.job_post_id,
                     assigned_wage=excluded.assigned_wage,
                     assignment_mode=excluded.assignment_mode,
                     status='assigned',
                     outcome='',
                     tokens_awarded=0,
                     tokens_credited=1,
                     updated_at=datetime('now');",
      list(rid, uid, post_id,
           if (is.na(post$wage[1] %||% NA)) NA_real_ else as.numeric(post$wage[1]),
           round$assignment_mode[1] %||% "manual"))
    showNotification(
      sprintf("Added %s back to %s.",
              stu$display_name[1] %||% uid,
              post$job_name[1] %||% "the job"),
      type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$draw_cold_call_btn, {
    req(rv$is_admin)
    sec <- trimws(rv$active_section %||% "")
    pool <- tryCatch(
      if (nzchar(sec)) {
        db_query(
          "SELECT u.user_id, u.display_name, u.section,
                  COALESCE(cc.n,0) AS cold_calls
           FROM users u
           LEFT JOIN (
             SELECT lse.user_id, COUNT(*) n
             FROM live_score_events lse
             LEFT JOIN job_posts jp ON jp.id=lse.job_post_id
             WHERE lse.event_kind='cold_call'
                OR lower(COALESCE(jp.selection_time,''))='during'
                OR lower(COALESCE(jp.job_name,'')) LIKE 'cold call:%'
             GROUP BY lse.user_id
           ) cc ON cc.user_id=u.user_id
           WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1
             AND COALESCE(u.is_demo,0)=0 AND u.section=?
           ORDER BY cold_calls ASC, RANDOM()
           LIMIT 1;",
          list(sec))
      } else {
        db_query(
          "SELECT u.user_id, u.display_name, u.section,
                  COALESCE(cc.n,0) AS cold_calls
           FROM users u
           LEFT JOIN (
             SELECT lse.user_id, COUNT(*) n
             FROM live_score_events lse
             LEFT JOIN job_posts jp ON jp.id=lse.job_post_id
             WHERE lse.event_kind='cold_call'
                OR lower(COALESCE(jp.selection_time,''))='during'
                OR lower(COALESCE(jp.job_name,'')) LIKE 'cold call:%'
             GROUP BY lse.user_id
           ) cc ON cc.user_id=u.user_id
           WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1
             AND COALESCE(u.is_demo,0)=0
           ORDER BY cold_calls ASC, RANDOM()
           LIMIT 1;")
      },
      error = function(e) data.frame())
    if (!nrow(pool)) {
      showNotification("No eligible students for a cold call.", type = "warning")
      return()
    }
    rv$cold_call_draw <- list(
      user_id = pool$user_id[1],
      display_name = pool$display_name[1] %||% pool$user_id[1],
      section = pool$section[1] %||% "")
  }, ignoreNULL = TRUE)

  cold_call_post_id <- function(kind, rid) {
    kind <- if (identical(kind, "board")) "board" else "answer"
    pattern <- if (identical(kind, "board")) "%board%" else "%answer%"
    post <- tryCatch(db_query(
      "SELECT id, COALESCE(wage_override, jc.default_wage, 1) AS wage
       FROM job_posts jp
       LEFT JOIN job_categories jc ON jc.id=jp.category_id
       WHERE jp.round_id=?
         AND COALESCE(jp.active,1)=1
         AND lower(COALESCE(jp.selection_time,''))='during'
         AND lower(COALESCE(jp.job_name,'')) LIKE ?
       ORDER BY jp.id
       LIMIT 1;",
      list(rid, pattern)),
      error = function(e) data.frame())
    if (nrow(post)) return(post)
    cat <- tryCatch(db_query(
      "SELECT id, default_wage FROM job_categories
       WHERE lower(name)='cold call'
       ORDER BY id LIMIT 1;"),
      error = function(e) data.frame())
    cat_id <- if (nrow(cat)) as.integer(cat$id[1]) else NA_integer_
    wage <- if (nrow(cat)) as.numeric(cat$default_wage[1] %||% 1) else 1
    name <- if (identical(kind, "board")) "Cold call: graph/answer on board" else "Cold call: answer a question"
    db_exec(
      "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override,
                             in_draw, voluntary, selection_time)
       VALUES(?,?,?,?,?,1,0,'during');",
      list(rid, name, cat_id, 1L, wage))
    new_id <- tryCatch(db_query("SELECT last_insert_rowid() AS id;")$id[1],
                       error = function(e) NA_integer_)
    data.frame(id = new_id, wage = wage)
  }

  record_cold_call <- function(kind) {
    req(rv$is_admin, !rv$impersonating)
    drawn <- rv$cold_call_draw
    if (!is.list(drawn) || !nzchar(drawn$user_id %||% "")) {
      showNotification("Draw a cold-call student first.", type = "warning")
      return()
    }
    round <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                      error = function(e) data.frame())
    if (!nrow(round)) { showNotification("No active round.", type = "error"); return() }
    rid <- as.integer(round$id[1])
    post <- cold_call_post_id(kind, rid)
    if (!nrow(post) || is.na(post$id[1])) {
      showNotification("Could not find or create the cold-call job post.", type = "error")
      return()
    }
    uid <- drawn$user_id
    tokens <- as.numeric(post$wage[1] %||% 1)
    db_exec(
      "INSERT INTO live_score_events(round_id, user_id, job_post_id, event_kind,
              outcome, tokens, logged_by)
       VALUES(?,?,?,'cold_call','succeed',?,?);",
      list(rid, uid, as.integer(post$id[1]), tokens, rv$user_id %||% "admin"))
    rv$cold_call_draw <- NULL
    showNotification(
      sprintf("Queued cold call for %s.", drawn$display_name %||% uid),
      type = "message")
  }

  observeEvent(input$record_cold_call_answer_btn, record_cold_call("answer"), ignoreNULL = TRUE)
  observeEvent(input$record_cold_call_board_btn,  record_cold_call("board"),  ignoreNULL = TRUE)
  observeEvent(input$clear_cold_call_draw_btn, {
    rv$cold_call_draw <- NULL
  }, ignoreNULL = TRUE)

  observeEvent(input$redraw_absent_btn, {
    req(rv$is_admin)
    aid <- suppressWarnings(as.integer(input$redraw_absent_btn %||% 0))
    if (is.na(aid) || aid <= 0) return()
    old <- tryCatch(db_query(
      "SELECT ja.round_id, ja.user_id, ja.job_post_id, ja.assigned_wage,
              ja.assignment_mode, u.display_name, COALESCE(u.section,'') AS section
       FROM job_assignments ja
       JOIN users u ON u.user_id=ja.user_id
       WHERE ja.id=?;", list(aid)),
      error = function(e) data.frame())
    if (!nrow(old)) { showNotification("Assignment not found.", type = "error"); return() }
    if (!is.na(old$assigned_wage[1] %||% NA) && as.numeric(old$assigned_wage[1]) < 0) {
      showNotification("Cannot redraw an already marked absent assignment.", type = "warning")
      return()
    }
    sec <- trimws(old$section[1] %||% "")
    candidates <- tryCatch(
      if (nzchar(sec)) {
        db_query(
          "SELECT u.user_id, u.display_name
           FROM users u
           WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1
             AND COALESCE(u.is_demo,0)=0 AND u.section=?
             AND u.user_id<>?
             AND NOT EXISTS (
               SELECT 1 FROM job_assignments ja
               WHERE ja.round_id=? AND ja.user_id=u.user_id
             )
           ORDER BY RANDOM() LIMIT 1;",
          list(sec, old$user_id[1], old$round_id[1]))
      } else {
        db_query(
          "SELECT u.user_id, u.display_name
           FROM users u
           WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1
             AND COALESCE(u.is_demo,0)=0
             AND u.user_id<>?
             AND NOT EXISTS (
               SELECT 1 FROM job_assignments ja
               WHERE ja.round_id=? AND ja.user_id=u.user_id
             )
           ORDER BY RANDOM() LIMIT 1;",
          list(old$user_id[1], old$round_id[1]))
      },
      error = function(e) data.frame())
    if (!nrow(candidates)) {
      showNotification("No unassigned replacement student found.", type = "warning")
      return()
    }
    db_exec("DELETE FROM live_score_events WHERE job_assignment_id=? AND committed_at IS NULL;", list(aid))
    db_exec(
      "UPDATE job_assignments
       SET status='absent_redrawn', assigned_wage=-ABS(COALESCE(assigned_wage,0)),
           updated_at=datetime('now')
       WHERE id=?;",
      list(aid))
    db_exec(
      "INSERT OR IGNORE INTO job_assignments(round_id, user_id, job_post_id, assigned_wage, assignment_mode)
       VALUES(?,?,?,?,?);",
      list(old$round_id[1], candidates$user_id[1], old$job_post_id[1],
           if (is.na(old$assigned_wage[1] %||% NA)) NA_real_ else abs(as.numeric(old$assigned_wage[1])),
           old$assignment_mode[1] %||% "redraw"))
    showNotification(
      sprintf("Redrew %s's job to %s.",
              old$display_name[1] %||% old$user_id[1],
              candidates$display_name[1] %||% candidates$user_id[1]),
      type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$clear_assignments_btn, {
    req(rv$is_admin)
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("No active round.", type = "error"); return() }
    db_exec("DELETE FROM job_assignments WHERE round_id=?;", list(rid_row$id[1]))
    rv$jobs_ver <- rv$jobs_ver + 1L
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
    vol_cat  <- as.integer(ev$vol %||% 0)
    draw_cat <- as.integer(ev$in_draw %||% 1)
    db_exec("UPDATE job_categories SET name=?, default_wage=?, description=?, voluntary=?, in_draw=? WHERE id=?;",
            list(nm, if (is.na(wage)) 0 else wage, desc, vol_cat, draw_cat, cid))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Category updated.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$create_round_btn, {
    req(rv$is_admin)
    lbl    <- trimws(input$new_round_label %||% "")
    mode   <- input$new_round_mode %||% "random"
    tbrk   <- input$new_round_tiebreak %||% "weighted_lottery"
    tok_rv <- if (isTRUE(input$new_round_delayed_tokens)) 0L else 1L
    open_d <- as.character(input$new_round_open %||% "")
    cls_d  <- as.character(input$new_round_close %||% "")
    tix    <- max(1L, as.integer(input$new_round_tix %||% 10L))
    if (!nzchar(lbl)) { showNotification("Round label required.", type = "error"); return() }
    db_exec(
      "INSERT INTO weekly_rounds(label, assignment_mode, tiebreak_method, tokens_revealed,
                                  bid_open_date, bid_close_date, tickets_per_student)
       VALUES(?,?,?,?,?,?,?);",
      list(lbl, mode, tbrk, tok_rv,
           if (nzchar(open_d)) open_d else NA_character_,
           if (nzchar(cls_d))  cls_d  else NA_character_,
           tix))
    rv$jobs_ver <- rv$jobs_ver + 1L
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
    tbrk   <- input$edit_round_tiebreak %||% "weighted_lottery"
    tok_rv <- if (isTRUE(input$edit_round_delayed_tokens)) 0L else 1L
    open_d <- as.character(input$edit_round_open %||% "")
    cls_d  <- as.character(input$edit_round_close %||% "")
    tix    <- max(1L, as.integer(input$edit_round_tix %||% 10L))
    if (!nzchar(lbl)) { showNotification("Label required.", type = "error"); return() }
    db_exec(
      "UPDATE weekly_rounds SET label=?, assignment_mode=?, tiebreak_method=?, tokens_revealed=?,
       bid_open_date=?, bid_close_date=?, tickets_per_student=? WHERE id=?;",
      list(lbl, mode, tbrk, tok_rv,
           if (nzchar(open_d)) open_d else NA_character_,
           if (nzchar(cls_d))  cls_d  else NA_character_,
           tix, rid))
    rv$jobs_ver <- rv$jobs_ver + 1L
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
    # Carry the last round's settings forward so "advance round" is one click
    db_exec(
      "INSERT INTO weekly_rounds(label, assignment_mode, tiebreak_method, tokens_revealed, tickets_per_student)
       VALUES(?,?,?,?,?);",
      list(new_label, mode,
           if (nrow(last)) last$tiebreak_method[1] %||% "weighted_lottery" else "weighted_lottery",
           if (nrow(last)) as.integer(last$tokens_revealed[1] %||% 0L) else 0L,
           if (nrow(last)) as.integer(last$tickets_per_student[1] %||% 10L) else 10L))
    new_rid <- tryCatch(db_query("SELECT last_insert_rowid() AS id;")$id[1],
                        error = function(e) NA_integer_)
    if (is.na(new_rid)) {
      showNotification("Round created but could not retrieve ID.", type = "warning"); return()
    }
    templates <- tryCatch(db_query(
      "SELECT jt.*,
              COALESCE(jt.in_draw,  COALESCE(jc.in_draw,1))   AS eff_in_draw,
              COALESCE(jt.voluntary, COALESCE(jc.voluntary,0)) AS eff_voluntary,
              COALESCE(NULLIF(jt.selection_time,''), NULLIF(jc.selection_time,''), 'any') AS eff_timing
       FROM job_templates jt
       LEFT JOIN job_categories jc ON jc.id=jt.category_id
       WHERE COALESCE(jt.active,1)=1
       ORDER BY COALESCE(jt.display_order,99), jt.id;"),
      error = function(e) data.frame())
    if (nrow(templates)) {
      for (i in seq_len(nrow(templates))) {
        t <- templates[i, ]
        db_exec(
          "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override,
                                 in_draw, voluntary, selection_time, display_order)
           VALUES(?,?,?,?,?,?,?,?,?);",
          list(new_rid, t$name,
               if (!is.na(t$category_id %||% NA)) as.integer(t$category_id) else NA_integer_,
               as.integer(t$slots %||% 1L),
               if (!is.na(t$suggested_wage %||% NA)) as.numeric(t$suggested_wage) else NA_real_,
               as.integer(t$eff_in_draw %||% 1L),
               as.integer(t$eff_voluntary %||% 0L),
               as.character(t$eff_timing %||% "any"),
               as.integer(t$display_order %||% 99L)))
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
    rv$jobs_ver <- rv$jobs_ver + 1L
  })

  copy_active_templates_to_round <- function(round_id) {
    templates <- tryCatch(db_query(
      "SELECT jt.*,
              COALESCE(jt.in_draw,  COALESCE(jc.in_draw,1))   AS eff_in_draw,
              COALESCE(jt.voluntary, COALESCE(jc.voluntary,0)) AS eff_voluntary,
              COALESCE(NULLIF(jt.selection_time,''), NULLIF(jc.selection_time,''), 'any') AS eff_timing
       FROM job_templates jt
       LEFT JOIN job_categories jc ON jc.id=jt.category_id
       WHERE COALESCE(jt.active,1)=1
       ORDER BY COALESCE(jt.display_order,99), jt.id;"),
      error = function(e) data.frame())
    if (!nrow(templates)) return(0L)
    copied <- 0L
    for (i in seq_len(nrow(templates))) {
      t <- templates[i, ]
      existing <- tryCatch(db_query(
        "SELECT id FROM job_posts WHERE round_id=? AND lower(job_name)=lower(?) LIMIT 1;",
        list(round_id, t$name)),
        error = function(e) data.frame())
      if (nrow(existing)) next
      db_exec(
        "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override,
                               in_draw, voluntary, selection_time, display_order)
         VALUES(?,?,?,?,?,?,?,?,?);",
        list(round_id, t$name,
             if (!is.na(t$category_id %||% NA)) as.integer(t$category_id) else NA_integer_,
             as.integer(t$slots %||% 1L),
             if (!is.na(t$suggested_wage %||% NA)) as.numeric(t$suggested_wage) else NA_real_,
             as.integer(t$eff_in_draw %||% 1L),
             as.integer(t$eff_voluntary %||% 0L),
             as.character(t$eff_timing %||% "any"),
             as.integer(t$display_order %||% 99L)))
      copied <- copied + 1L
    }
    copied
  }

  next_round_label <- function(lbl) {
    lbl <- lbl %||% "Week 1"
    m <- regmatches(lbl, regexpr("[0-9]+", lbl))
    if (length(m)) sub(m, as.character(as.integer(m) + 1L), lbl, fixed = TRUE)
    else paste0(lbl, " (next)")
  }

  create_next_round_from <- function(round) {
    db_exec(
      "INSERT INTO weekly_rounds(label, assignment_mode, tiebreak_method, tokens_revealed, tickets_per_student)
       VALUES(?,?,?,?,?);",
      list(next_round_label(round$label[1] %||% "Week 1"),
           round$assignment_mode[1] %||% "random",
           round$tiebreak_method[1] %||% "weighted_lottery",
           as.integer(round$tokens_revealed[1] %||% 0L),
           as.integer(round$tickets_per_student[1] %||% 10L)))
    new_rid <- tryCatch(db_query("SELECT last_insert_rowid() AS id;")$id[1],
                        error = function(e) NA_integer_)
    if (!is.na(new_rid)) copy_active_templates_to_round(new_rid)
    new_rid
  }

  clone_posts_to_round <- function(posts, target_rid) {
    if (!nrow(posts)) return(posts)
    for (i in seq_len(nrow(posts))) {
      p <- posts[i, ]
      existing <- tryCatch(db_query(
        "SELECT id, COALESCE(wage_override, ?) AS wage
         FROM job_posts
         WHERE round_id=? AND lower(job_name)=lower(?)
         ORDER BY id LIMIT 1;",
        list(as.numeric(p$wage %||% 0), target_rid, p$job_name)),
        error = function(e) data.frame())
      if (!nrow(existing)) {
        db_exec(
          "INSERT INTO job_posts(round_id, job_name, category_id, slots, wage_override,
                                 in_draw, selection_time)
           VALUES(?,?,?,?,?,1,?);",
          list(target_rid, p$job_name,
               if (!is.na(p$category_id %||% NA)) as.integer(p$category_id) else NA_integer_,
               as.integer(p$slots %||% 1L),
               if (is.na(p$wage %||% NA)) NA_real_ else as.numeric(p$wage),
               as.character(p$selection_time %||% "end")))
        existing <- db_query("SELECT last_insert_rowid() AS id;")
      }
      posts$id[i] <- existing$id[1]
    }
    posts
  }

  observeEvent(input$save_bid_lock_btn, {
    req(rv$is_admin)
    valid_hm <- function(x) grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", trimws(x %||% ""))
    days <- input$bl_days %||% character(0)
    ct   <- trimws(input$bl_class_time  %||% "12:00")
    rt   <- trimws(input$bl_reopen_time %||% "17:00")
    lead <- suppressWarnings(as.integer(input$bl_lead_min %||% 60L))
    tz   <- trimws(input$bl_tz %||% "America/New_York")
    if (!valid_hm(ct) || !valid_hm(rt)) {
      showNotification("Times must be 24h HH:MM (e.g. 12:00).", type = "error"); return()
    }
    if (is.na(lead) || lead < 0) lead <- 60L
    if (!tz %in% OlsonNames()) {
      showNotification(sprintf("Unknown time zone '%s'.", tz), type = "error"); return()
    }
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('bid_lock_enabled',?);",
            list(if (isTRUE(input$bl_enabled)) "1" else "0"))
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('class_days',?);",
            list(paste(days, collapse = ",")))
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('class_start_time',?);", list(ct))
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('bid_lock_lead_min',?);",
            list(as.character(lead)))
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('bid_reopen_time',?);", list(rt))
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('class_tz',?);", list(tz))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification("Bid lock schedule saved.", type = "message")
  })

  observeEvent(input$save_rw_setup_btn, {
    req(rv$is_admin)
    costs_str <- trimws(input$rw_costs_input %||% "")
    if (!nzchar(costs_str)) {
      showNotification("Enter a cost schedule.", type = "error"); return()
    }
    max_pts <- max(1L, suppressWarnings(as.integer(input$rw_max_points_input %||% 5L)))
    if (is.na(max_pts)) max_pts <- 5L
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('reweight_cost_schedule',?);",
            list(costs_str))
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('grade_reweight_max_points',?);",
            list(as.character(max_pts)))
    showNotification("Reweighting setup saved.", type = "message")
    rv$gradebook_ver <- rv$gradebook_ver + 1L
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
    parsed <- parse_flex_cost(sched)
    if (parsed$type == "expr") {
      test <- eval_cost_expr(parsed$expr, "q", 0)
      if (is.na(test)) {
        showNotification("Expression error — check syntax (use q for questions owned, e.g. 11+q^2).",
                         type = "error"); return()
      }
    }
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('flex_cost_schedule',?);",
            list(sched))
    showNotification("Price schedule saved.", type = "message")
    rv$gradebook_ver <- rv$gradebook_ver + 1L
  })

  observeEvent(input$add_flex_question_btn, {
    req(rv$is_admin)
    txt  <- trimws(input$new_fq_text %||% "")
    etag <- trimws(input$new_fq_exam %||% "")
    if (!nzchar(txt)) { showNotification("Question text required.", type = "error"); return() }
    max_idx <- tryCatch(
      db_query("SELECT COALESCE(MAX(order_index),0) n FROM flex_questions;")$n[1],
      error = function(e) 0L)
    db_exec("INSERT INTO flex_questions(question_text,order_index,exam_tag) VALUES(?,?,?);",
            list(txt, as.integer(max_idx %||% 0L) + 1L, if (nzchar(etag)) etag else NA_character_))
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
    ext          <- tolower(tools::file_ext(f$name))
    batch_etag   <- trimws(input$upload_fq_exam %||% "")
    parsed <- tryCatch({
      if (ext == "csv") {
        df  <- read.csv(f$datapath, stringsAsFactors = FALSE)
        col <- intersect(c("question_text","question","text"), names(df))
        if (!length(col)) stop("CSV must have a 'question_text' column.")
        etag_col <- intersect(c("exam_tag","exam"), names(df))
        list(
          texts = df[[col[1]]],
          tags  = if (length(etag_col)) df[[etag_col[1]]] else rep(NA_character_, nrow(df))
        )
      } else {
        raw <- readLines(f$datapath, warn = FALSE)
        txts <- trimws(raw[nzchar(trimws(raw))])
        list(texts = txts, tags = rep(NA_character_, length(txts)))
      }
    }, error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL })
    if (is.null(parsed)) return()
    keep  <- nzchar(trimws(parsed$texts))
    texts <- parsed$texts[keep]
    etags <- parsed$tags[keep]
    if (!length(texts)) { showNotification("No questions found in file.", type = "warning"); return() }
    if (isTRUE(input$fq_replace_all)) db_exec("UPDATE flex_questions SET active=0;")
    max_idx  <- tryCatch(
      db_query("SELECT COALESCE(MAX(order_index),0) n FROM flex_questions;")$n[1],
      error = function(e) 0L)
    base_idx <- as.integer(max_idx %||% 0L)
    for (i in seq_along(texts)) {
      tag <- if (!is.na(etags[i]) && nzchar(trimws(etags[i]))) trimws(etags[i])
             else if (nzchar(batch_etag)) batch_etag
             else NA_character_
      db_exec("INSERT INTO flex_questions(question_text,order_index,exam_tag) VALUES(?,?,?);",
              list(texts[i], base_idx + i, tag))
    }
    showNotification(sprintf("Uploaded %d questions.", length(texts)), type = "message")
  })

  # quick_award_btn removed — use Settings → Token Admin for awards

  # ── Live Tracker tab (admin) ──────────────────────────────────────────────────
  output$live_tracker_tab <- renderUI({
    req(rv$authed, rv$is_admin || rv$is_demo)
    td        <- tracker_poll()
    revealed  <- td$revealed
    round     <- td$round
    mode      <- if (nrow(round)) round$assignment_mode[1] %||% "random" else "random"
    wage_mode <- identical(mode, "wage_bidding")

    # Section picker data
    all_sections <- tryCatch(
      sort(unique(nonempty_values(
        db_query("SELECT DISTINCT section FROM users WHERE COALESCE(active,1)=1;")$section))),
      error = function(e) character(0))
    sec_choices <- c("(All sections)" = "", setNames(all_sections, all_sections))
    cur_sec <- rv$active_section %||% ""

    # Filter assignments to active section
    assignments_show <- td$assignments
    if (nzchar(cur_sec) && nrow(assignments_show) && "section" %in% names(assignments_show)) {
      keep <- !is.na(assignments_show$section) & as.character(assignments_show$section) == cur_sec
      assignments_show <- assignments_show[keep, , drop = FALSE]
    }
    n_show <- nrow(assignments_show)

    students_sec <- td$students
    if (nzchar(cur_sec) && nrow(students_sec) && "section" %in% names(students_sec)) {
      keep <- !is.na(students_sec$section) & as.character(students_sec$section) == cur_sec
      students_sec <- students_sec[keep, , drop = FALSE]
    }
    pending_show <- td$pending_scores
    if (nzchar(cur_sec) && nrow(pending_show) && "section" %in% names(pending_show)) {
      keep <- !is.na(pending_show$section) & as.character(pending_show$section) == cur_sec
      pending_show <- pending_show[keep, , drop = FALSE]
    }

    end_pending <- tryCatch(db_query(
      "SELECT ja.id, ja.user_id, u.display_name, u.section,
              COALESCE(wr.label, 'Round ' || ja.round_id) AS round_label,
              jp.job_name, ja.assigned_wage
       FROM job_assignments ja
       JOIN users u ON u.user_id=ja.user_id
       JOIN job_posts jp ON jp.id=ja.job_post_id
       LEFT JOIN weekly_rounds wr ON wr.id=ja.round_id
       WHERE COALESCE(ja.status,'assigned')='assigned'
         AND COALESCE(ja.outcome,'')=''
         AND COALESCE(jp.selection_time,'')='end'
         AND NOT EXISTS (
           SELECT 1 FROM live_score_events lse
           WHERE lse.job_assignment_id=ja.id AND lse.committed_at IS NULL
         )
       ORDER BY ja.created_at DESC, ja.id DESC
       LIMIT 50;"),
      error = function(e) data.frame())
    if (nzchar(cur_sec) && nrow(end_pending) && "section" %in% names(end_pending)) {
      keep <- !is.na(end_pending$section) & as.character(end_pending$section) == cur_sec
      end_pending <- end_pending[keep, , drop = FALSE]
    }

    # Round ID
    rid <- if (nrow(round)) round$id[1] else NA_integer_
    section_revealed <- FALSE
    if (!is.na(rid) && nzchar(cur_sec) && nrow(td$section_reveals)) {
      keep_sr <- !is.na(td$section_reveals$section) &
        as.character(td$section_reveals$section) == cur_sec
      sr <- td$section_reveals[keep_sr, , drop = FALSE]
      section_revealed <- isTRUE(nrow(sr) && as.integer(sr$revealed[1] %||% 0L) == 1L)
    }

    # Voluntary job posts for participation panel (Panel 2)
    vol_cats <- if (!is.na(rid)) {
      tryCatch(db_query(
        "SELECT jp.id, jp.job_name AS name, jp.category_id, jp.slots,
                COALESCE(jp.wage_override, jc.default_wage, 1) AS tokens
         FROM job_posts jp LEFT JOIN job_categories jc ON jc.id=jp.category_id
         WHERE jp.round_id=? AND COALESCE(jp.active,1)=1
           AND (COALESCE(jc.voluntary,0)=1 OR COALESCE(jp.voluntary,0)=1)
         ORDER BY jp.job_name;", list(rid)),
        error = function(e) data.frame())
    } else data.frame()
    # In wage-bidding rounds volunteers are paid the category's clearing wage,
    # so show that amount on the logging buttons instead of the post default.
    if (nrow(vol_cats) && identical(mode, "wage_bidding")) {
      for (vi in seq_len(nrow(vol_cats))) {
        cw <- volunteer_clearing_wage(rid, vol_cats$category_id[vi],
                                      as.integer(vol_cats$slots[vi] %||% 1L),
                                      query_fn = db_query)
        if (!is.na(cw)) vol_cats$tokens[vi] <- cw
      }
    }

    # Build student choices: bidders for current round first
    bidder_ids <- if (!is.na(rid) && nrow(students_sec)) {
      tryCatch(db_query(
        "SELECT DISTINCT user_id FROM wage_bids WHERE round_id=?;",
        list(rid))$user_id, error = function(e) character(0))
    } else character(0)
      stu_nm  <- students_sec$display_name %||% students_sec$user_id
      stu_sec <- students_sec$section %||% ""
      stu_nm[is.na(stu_nm) | !nzchar(stu_nm)] <- students_sec$user_id[is.na(stu_nm) | !nzchar(stu_nm)]
      stu_sec[is.na(stu_sec)] <- ""
      stu_lbl <- ifelse(nzchar(stu_sec), paste0(stu_nm, " (", stu_sec, ")"), stu_nm)
    stu_choices_raw <- setNames(students_sec$user_id, stu_lbl)
    is_bidder   <- students_sec$user_id %in% bidder_ids
    stu_choices <- c(stu_choices_raw[is_bidder], stu_choices_raw[!is_bidder])
    manual_posts <- if (!is.na(rid)) {
      tryCatch(db_query(
        "SELECT jp.id, jp.job_name,
                COALESCE(NULLIF(jp.selection_time,''), NULLIF(jc.selection_time,''), 'any') AS selection_time,
                COALESCE(jp.wage_override, jc.default_wage, 0) AS wage
         FROM job_posts jp
         LEFT JOIN job_categories jc ON jc.id=jp.category_id
         WHERE jp.round_id=? AND COALESCE(jp.active,1)=1
         ORDER BY jp.display_order, jp.job_name;",
        list(rid)),
        error = function(e) data.frame())
    } else data.frame()
    manual_post_choices <- if (nrow(manual_posts)) {
      post_labels <- sprintf("%s [%s, %g token%s]",
                             manual_posts$job_name %||% paste("Job", manual_posts$id),
                             manual_posts$selection_time %||% "any",
                             as.numeric(manual_posts$wage %||% 0),
                             ifelse(as.numeric(manual_posts$wage %||% 0) == 1, "", "s"))
      setNames(manual_posts$id, post_labels)
    } else character(0)

    tagList(
      div(class = "tab-howto",
          "Manage job assignments and log participation during class. Updates every 5 seconds."),

      # Section selector
      fluidRow(
        column(4,
          selectInput("active_section_sel", "Active section:",
                      choices = sec_choices, selected = cur_sec, width = "100%")),
        column(4,
          selectInput("draw_timing_filter", "Draw jobs:",
                      choices = c("All timings" = "all", "Start of class" = "start",
                                  "End/post class" = "end"),
                      selected = "all", width = "100%"))
      ),

      wellPanel(
        tags$h6(style = "font-weight:700;color:#951829;margin-bottom:.6rem;",
                "Cold Call"),
        if (!nrow(students_sec)) {
          tags$p(style = "color:#999;margin:0;", "No students in the selected section.")
        } else {
          drawn <- rv$cold_call_draw
          drawn_uid <- if (is.list(drawn)) drawn$user_id %||% "" else ""
          drawn_name <- if (is.list(drawn)) drawn$display_name %||% drawn_uid else ""
          tagList(
            div(style = "display:flex;gap:.5rem;align-items:center;flex-wrap:wrap;",
              actionButton("draw_cold_call_btn", "Draw Cold Call",
                           class = "btn btn-sm btn-primary"),
              if (nzchar(drawn_uid)) {
                span(style = "font-weight:700;",
                     sprintf("%s", drawn_name))
              } else {
                span(style = "color:#888;font-size:.88rem;", "Draw a student, then record the cold-call type.")
              }
            ),
            if (nzchar(drawn_uid)) {
              div(style = "display:flex;gap:.4rem;flex-wrap:wrap;margin-top:.6rem;",
                actionButton("record_cold_call_answer_btn", "Answering Question",
                             class = "btn btn-sm btn-outline-success"),
                actionButton("record_cold_call_board_btn", "Board Work",
                             class = "btn btn-sm btn-outline-success"),
                actionButton("clear_cold_call_draw_btn", "Clear",
                             class = "btn btn-sm btn-outline-secondary")
              )
            }
          )
        }
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
          {
            tok_rev <- isTRUE(as.integer(round$tokens_revealed[1] %||% 1L) == 1L)
            ja_cols_panel <- tryCatch(db_query("PRAGMA table_info(job_assignments);")$name,
                                      error = function(e) character(0))
            n_pending <- if (!tok_rev && n_show > 0 &&
                             all(c("tokens_credited", "tokens_awarded") %in% ja_cols_panel)) {
              tryCatch(db_query(
                "SELECT COUNT(*) n FROM job_assignments WHERE round_id=? AND COALESCE(tokens_credited,1)=0 AND tokens_awarded>0;",
                list(round$id[1]))$n[1], error=function(e) 0L)
            } else 0L
            tagList(
            tags$p(style = "color:#555;font-size:.88rem;margin-bottom:.6rem;",
                   sprintf("Round: %s  ·  %s%s  ·  Tokens: %s",
                           round$label[1] %||% paste("Round", round$id[1]),
                           mode_label,
                           if (nzchar(cur_sec)) paste0("  ·  Section: ", cur_sec) else "",
                           if (tok_rev) "released" else sprintf("%d pending", as.integer(n_pending)))),
            fluidRow(
              column(2,
                actionButton("run_draw_btn", "\U0001f3b2 Draw Jobs",
                             class = "btn btn-primary btn-sm",
                             title = if (mode == "random") "Draw per-section (select section above first)" else "Assign from this week\'s bids")),
              column(2,
                actionButton("preview_draw_btn", "\U0001f441 Preview Draw",
                             class = "btn btn-outline-secondary btn-sm")),
              column(3,
                selectInput("section_reveal_timing", "Reveal group:",
                            choices = c("Start of class" = "start", "Post class" = "post"),
                            selected = "start", width = "100%")),
              column(2,
                if (nzchar(cur_sec)) {
                  if (section_revealed)
                    actionButton("toggle_section_reveal_btn", "Hide Group",
                                 class = "btn btn-outline-secondary btn-sm",
                                 title = "Hide assignments from the selected section")
                  else
                    actionButton("toggle_section_reveal_btn", "Reveal Group",
                                 class = "btn btn-success btn-sm",
                                 title = "Reveal assignments to the selected section")
                } else {
                  tags$span(style = "color:#999;font-size:.8rem;", "Pick section")
                }),
              column(2,
                if (revealed)
                  actionButton("toggle_reveal_btn", "Hide All",
                               class = "btn btn-outline-secondary btn-sm",
                               title = "Hide job assignments from student view")
                else
                  actionButton("toggle_reveal_btn", "Reveal All",
                               class = "btn btn-success btn-sm",
                               title = "Override groups and reveal every assignment")
              ),
              column(1,
                if (!tok_rev && n_pending > 0)
                  actionButton("release_tokens_btn",
                               "Release",
                               class = "btn btn-warning btn-sm",
                               title = "Credit pending token earnings to students",
                               onclick = "if(!confirm('Release tokens to all students? This cannot be undone.')) return false;")
                else if (n_show > 0)
                  actionButton("clear_assignments_btn", "Clear",
                               class = "btn btn-outline-danger btn-sm",
                               title = "Delete all assignments for current round",
                               onclick = "if(!confirm('Delete all job assignments for this round?')) return false;")
              )
            ),
            if (n_show > 0)
              tags$p(style = "font-size:.8rem;color:#888;margin-top:.4rem;margin-bottom:0;",
                     sprintf("%d students assigned · %s", n_show,
                             if (revealed) "Visible to students" else "Hidden from students")),
            uiOutput("draw_preview_table"),
            tags$hr(),
            tags$h6(style = "font-weight:700;color:#951829;margin-bottom:.4rem;",
                    "Add Assignment Back"),
            if (!length(stu_choices) || !length(manual_post_choices)) {
              tags$p(style = "color:#999;margin:0;font-size:.86rem;",
                     "No eligible students or active jobs available for this round.")
            } else {
              fluidRow(
                column(4, selectInput("manual_assign_uid", "Student:",
                                      choices = stu_choices, width = "100%")),
                column(5, selectInput("manual_assign_post_id", "Job:",
                                      choices = manual_post_choices, width = "100%")),
                column(3, tags$br(),
                       actionButton("manual_add_assignment_btn", "Add Back",
                                    class = "btn btn-sm btn-primary"))
              )
            }
          )}
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
                poc <- as.character(r$pending_outcome %||% "")
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
                    if (nzchar(poc)) {
                      span(class = "badge badge-warning",
                           sprintf("Pending: %s", switch(poc, complete = "complete", tried = "tried", missed = "missed", poc)))
                    } else if (ta == 1L) {
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
                      class = "btn btn-xs btn-outline-warning",
                      style = "padding:.1rem .3rem;font-size:.7rem;margin-right:.1rem;",
                      title = "Mark absent and redraw this job",
                      onclick = sprintf(
                        "if(confirm('Mark this student absent and redraw this job?')){Shiny.setInputValue('redraw_absent_btn',%d,{priority:'event'})}",
                        as.integer(r$id)), "Redraw"),
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

      if (nrow(end_pending) > 0) {
        tagList(
          div(class = "sec-label", "End-of-Class Jobs Needing Grades"),
          div(class = "tracker-wrap",
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Student"), tags$th("Section"), tags$th("Round"),
                tags$th("Job"), tags$th(style = "text-align:right;", "Wage"),
                tags$th("Grade")
              )),
              tags$tbody(lapply(seq_len(nrow(end_pending)), function(i) {
                r <- end_pending[i, ]
                wage <- suppressWarnings(as.numeric(r$assigned_wage %||% 0))
                tags$tr(
                  tags$td(r$display_name %||% r$user_id),
                  tags$td(style = "color:#888;font-size:.85em;", r$section %||% ""),
                  tags$td(style = "color:#888;font-size:.85em;", r$round_label %||% ""),
                  tags$td(style = "font-weight:600;", r$job_name %||% ""),
                  tags$td(style = "text-align:right;font-size:.85em;color:#888;",
                          if (!is.na(wage) && wage > 0) sprintf("%g", wage) else ""),
                  tags$td(
                    tags$button(
                      class = "btn btn-xs btn-outline-success",
                      style = "padding:.1rem .3rem;font-size:.7rem;margin-right:.1rem;",
                      onclick = sprintf(
                        "Shiny.setInputValue('eval_outcome',{id:%d,outcome:'complete'},{priority:'event'});",
                        as.integer(r$id)), "Complete"),
                    tags$button(
                      class = "btn btn-xs btn-outline-warning",
                      style = "padding:.1rem .3rem;font-size:.7rem;margin-right:.1rem;",
                      onclick = sprintf(
                        "Shiny.setInputValue('eval_outcome',{id:%d,outcome:'tried'},{priority:'event'});",
                        as.integer(r$id)), "Tried"),
                    tags$button(
                      class = "btn btn-xs btn-outline-danger",
                      style = "padding:.1rem .3rem;font-size:.7rem;",
                      onclick = sprintf(
                        "Shiny.setInputValue('eval_outcome',{id:%d,outcome:'missed'},{priority:'event'});",
                        as.integer(r$id)), "Missed")
                  )
                )
              }))
            )
          )
        )
      },

      wellPanel(
        tags$h6(style = "font-weight:700;color:#951829;margin-bottom:.6rem;",
                "Live Score Audit"),
        if (!nrow(pending_show)) {
          tags$p(style = "color:#999;margin:0;",
                 "No pending live scores. Taps during class will queue here for review.")
        } else {
          tagList(
            div(style = "display:flex;gap:.4rem;flex-wrap:wrap;margin-bottom:.6rem;",
              actionButton("commit_live_scores_btn",
                           sprintf("Commit %d", nrow(pending_show)),
                           class = "btn btn-primary btn-sm",
                           onclick = "if(!confirm('Commit these pending live scores?')) return false;"),
              actionButton("clear_live_scores_btn", "Clear Pending",
                           class = "btn btn-outline-danger btn-sm",
                           onclick = "if(!confirm('Delete pending live scores for this view?')) return false;")
            ),
            div(class = "tracker-wrap",
              tags$table(class = "table table-sm",
                tags$thead(tags$tr(
                  tags$th("Student"), tags$th("Job"), tags$th("Type"),
                  tags$th("Outcome"), tags$th(style = "text-align:right;", "Tokens"), tags$th("")
                )),
                tags$tbody(lapply(seq_len(nrow(pending_show)), function(i) {
                  r <- pending_show[i, ]
                  tags$tr(
                    tags$td(r$display_name %||% r$user_id),
                    tags$td(r$job_name %||% ""),
                    tags$td(switch(as.character(r$event_kind %||% ""),
                                   assignment = "assigned",
                                   cold_call = "cold call",
                                   "voluntary")),
                    tags$td(r$outcome %||% ""),
                    tags$td(style = "text-align:right;", as.integer(r$tokens %||% 0)),
                    tags$td(tags$button(
                      class = "btn btn-xs btn-outline-secondary",
                      style = "padding:.1rem .3rem;font-size:.7rem;",
                      title = "Remove pending score",
                      onclick = sprintf(
                        "Shiny.setInputValue('drop_live_score_btn',%d,{priority:'event'});",
                        as.integer(r$id)), "\U2715"))
                  )
                }))
              )
            )
          )
        }
      ),

      # Panel 2: Voluntary Participation
      wellPanel(
        tags$h6(style = "font-weight:700;color:#951829;margin-bottom:.6rem;",
                "\U0001f64b Voluntary Participation"),
        if (!nrow(vol_cats)) {
          tags$p(style = "color:#999;margin:0;",
                 "No voluntary job posts yet. Go to Settings → Jobs and mark a job category as Voluntary.")
        } else if (!nrow(students_sec)) {
          tags$p(style = "color:#999;margin:0;", "No students in the selected section.")
        } else {
          et_choices <- setNames(vol_cats$id,
                                 paste0(vol_cats$name, " (+", as.integer(vol_cats$tokens), ")"))
          vol_rule <- as.character(get_setting("volunteer_clearing_rule", "lowest"))
          demand_editor <- if (identical(mode, "wage_bidding") && identical(vol_rule, "posted") && !is.na(rid)) {
            dcats <- tryCatch(db_query(
              "SELECT jc.id, jc.name, MIN(jp.slots) AS slots, vd.demand
               FROM job_posts jp
               JOIN job_categories jc ON jc.id=jp.category_id
               LEFT JOIN volunteer_demand vd ON vd.round_id=jp.round_id AND vd.category_id=jc.id
               WHERE jp.round_id=? AND COALESCE(jp.active,1)=1
                 AND (COALESCE(jc.voluntary,0)=1 OR COALESCE(jp.voluntary,0)=1)
               GROUP BY jc.id, jc.name, vd.demand
               ORDER BY jc.display_order, jc.name;", list(rid)),
              error = function(e) data.frame())
            if (nrow(dcats)) {
              div(style = paste0("background:#f0f4ff;border-left:3px solid #4a6fa5;padding:.5rem .8rem;",
                                 "border-radius:0 4px 4px 0;margin-bottom:.6rem;"),
                tags$b(style = "font-size:.85rem;", "Today's expected demand (posted clearing rule)"),
                tags$p(style = "color:#555;font-size:.8rem;margin:.2rem 0 .4rem;",
                       "Post how many of each volunteer job you expect to take this class. The clearing wage is the k-th lowest bid."),
                fluidRow(lapply(seq_len(nrow(dcats)), function(di) {
                  dr <- dcats[di, ]
                  cur_k <- if (!is.na(dr$demand %||% NA)) as.integer(dr$demand)
                           else max(1L, as.integer(dr$slots %||% 1L))
                  cw <- volunteer_clearing_wage(rid, dr$id, cur_k, query_fn = db_query)
                  column(3,
                    numericInput(paste0("vd_", as.integer(dr$id)),
                                 sprintf("%s%s", dr$name,
                                         if (!is.na(cw)) sprintf(" (wage %g)", cw) else " (no bids)"),
                                 value = cur_k, min = 1, step = 1))
                })),
                actionButton("post_vol_demand_btn", "Post demand", class = "btn btn-sm btn-primary")
              )
            } else NULL
          } else NULL
          tagList(
            demand_editor,
            div(class = "live-toolbar",
              selectInput("part_event_type", "Job:", choices = et_choices, width = "100%")
            ),
            div(class = "live-grid",
              lapply(seq_len(nrow(students_sec)), function(i) {
                r <- students_sec[i, ]
                uid <- r$user_id %||% ""
                div(class = "live-card",
                  div(class = "live-card-name", r$display_name %||% uid),
                  div(class = "live-card-section", r$section %||% ""),
                  div(class = "live-card-actions",
                    tags$button(
                      type = "button",
                      class = "btn btn-success btn-sm",
                      title = "Full credit",
                      onclick = sprintf(
                        "Shiny.setInputValue('part_card_click',{user_id:%s,outcome:'succeed',nonce:Math.random()},{priority:'event'});",
                        jsonlite::toJSON(uid, auto_unbox = TRUE)
                      ),
                      "Succeed"
                    ),
                    tags$button(
                      type = "button",
                      class = "btn btn-warning btn-sm",
                      title = "Partial credit",
                      onclick = sprintf(
                        "Shiny.setInputValue('part_card_click',{user_id:%s,outcome:'try',nonce:Math.random()},{priority:'event'});",
                        jsonlite::toJSON(uid, auto_unbox = TRUE)
                      ),
                      "Try"
                    ),
                    tags$button(
                      type = "button",
                      class = "btn btn-danger btn-sm",
                      title = "No credit",
                      onclick = sprintf(
                        "Shiny.setInputValue('part_card_click',{user_id:%s,outcome:'miss',nonce:Math.random()},{priority:'event'});",
                        jsonlite::toJSON(uid, auto_unbox = TRUE)
                      ),
                      "Miss"
                    )
                  )
                )
              })
            ),
            tags$details(style = "margin-top:.75rem;",
              tags$summary("Manual picker"),
              fluidRow(
                column(6,
                  selectInput("part_student_sel", "Student:",
                              choices = if (length(stu_choices)) stu_choices
                                        else c("(no students)" = ""))),
                column(6,
                  tags$label("Outcome:"),
                  div(style = "display:flex;gap:.35rem;flex-wrap:wrap;",
                    actionButton("log_succeed_btn", "Succeed",
                                 class = "btn btn-success btn-sm",
                                 title = "Full credit: student earns the posted token amount"),
                    actionButton("log_try_btn", "Try",
                                 class = "btn btn-warning btn-sm",
                                 title = "Partial credit: half tokens awarded"),
                    actionButton("log_miss_btn", "Miss",
                                 class = "btn btn-danger btn-sm",
                                 title = "No credit: no tokens awarded")
                  )
                )
              )
            )
          )
        }
      ),

      # Panel 3: Coordination Game — per-section breakdown
      local({
        op     <- olig_poll()
        arc    <- arcade_poll()
        s      <- op$settings
        active <- arc$active_game[1] %||% ""
        if (!nrow(s) || !nzchar(active)) return(NULL)

        cur_round  <- as.integer(s$current_round[1] %||% 1L)
        cur_status <- s$round_status[1] %||% "pending"
        cur_game   <- s$current_game[1] %||% active

        # Per-section submission counts + totals
        subs <- tryCatch(
          db_query(
            "SELECT COALESCE(section,'') AS section,
                    COUNT(*) AS n_sub,
                    SUM(CASE WHEN contribute IS NOT NULL THEN contribute ELSE 0 END) AS total_contrib,
                    SUM(CASE WHEN choice='cooperate' THEN 1 ELSE 0 END) AS n_coop,
                    SUM(CASE WHEN choice='defect'    THEN 1 ELSE 0 END) AS n_defect
             FROM olig_submissions
             WHERE round=?
             GROUP BY COALESCE(section,'')
             ORDER BY COALESCE(section,'');",
            list(cur_round)),
          error = function(e) data.frame())

        totals <- tryCatch(
          db_query(
            "SELECT COALESCE(section,'') AS section, COUNT(*) AS n_total
             FROM users
             WHERE COALESCE(active,1)=1 AND COALESCE(is_demo,0)=0
             GROUP BY COALESCE(section,'')
             ORDER BY COALESCE(section,'');"),
          error = function(e) data.frame())

        if (!nrow(subs) && !nrow(totals)) return(NULL)

        is_bp <- identical(cur_game, "bonus_pot")
        is_pd <- cur_game %in% c("prisoners_dilemma", "price_war")

        wellPanel(
          tags$h6(style = "font-weight:700;color:#951829;margin-bottom:.6rem;",
                  "\U0001f3ae Coordination Game — by Section"),
          tags$p(style = "color:#555;font-size:.85em;margin-bottom:.5rem;",
            tags$strong("Game: "), toupper(cur_game), "  ",
            tags$strong("Round: "), cur_round, "  ",
            tags$strong("Status: "),
            span(style = if (cur_status == "open") "color:#1a6e3c;font-weight:600;"
                         else "color:#b00020;font-weight:600;",
                 toupper(cur_status))
          ),
          if (!nrow(subs)) {
            tags$p(style = "color:#999;margin:0;", "No submissions yet.")
          } else {
            # Merge subs with totals
            merged <- merge(
              subs, totals, by = "section", all = TRUE)
            merged$n_sub     <- as.integer(merged$n_sub %||% 0)
            merged$n_total   <- as.integer(merged$n_total %||% merged$n_sub)
            merged$n_missing <- pmax(0L, merged$n_total - merged$n_sub)

            div(class = "tracker-wrap",
              tags$table(class = "table table-sm table-hover", style = "margin-bottom:0;",
                tags$thead(tags$tr(
                  tags$th("Section"),
                  tags$th(style = "text-align:right;", "Submitted"),
                  tags$th(style = "text-align:right;", "Remaining"),
                  if (is_bp) tags$th(style = "text-align:right;", "Total Contrib."),
                  if (is_bp) tags$th(style = "text-align:right;", "Avg Contrib."),
                  if (is_pd) tags$th(style = "text-align:right;", "Cooperate"),
                  if (is_pd) tags$th(style = "text-align:right;", "Defect")
                )),
                tags$tbody(lapply(seq_len(nrow(merged)), function(i) {
                  row <- merged[i, ]
                  sec_value <- as.character(row$section %||% "")
                  if (is.na(sec_value)) sec_value <- ""
                  sec_label <- if (nzchar(sec_value)) sec_value else "(no section)"
                  n_sub   <- as.integer(row$n_sub   %||% 0)
                  n_total <- as.integer(row$n_total %||% n_sub)
                  n_miss  <- as.integer(row$n_missing %||% 0)
                  t_contrib <- as.numeric(row$total_contrib %||% 0)
                  a_contrib <- if (n_sub > 0) round(t_contrib / n_sub, 1) else 0

                  tags$tr(
                    tags$td(sec_label),
                    tags$td(style = "text-align:right;",
                            sprintf("%d / %d", n_sub, n_total)),
                    tags$td(style = paste0(
                      "text-align:right;",
                      if (n_miss > 0) "color:#b00020;font-weight:600;" else "color:#1a6e3c;"),
                            if (n_miss > 0) as.character(n_miss) else "—"),
                    if (is_bp) tags$td(style = "text-align:right;",
                                       sprintf("%.1f", t_contrib)),
                    if (is_bp) tags$td(style = "text-align:right;",
                                       sprintf("%.1f", a_contrib)),
                    if (is_pd) tags$td(style = "text-align:right;",
                                       as.integer(row$n_coop %||% 0)),
                    if (is_pd) tags$td(style = "text-align:right;",
                                       as.integer(row$n_defect %||% 0))
                  )
                }))
              )
            )
          }
        )
      })
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
        "Grades & Gradebook"    = "gradebook",
        "Exports"               = "exports",
        "Extensions"            = "extensions",
        "Flex Questions"        = "flex_questions",
        "Game Controls"         = "game_controls",
        "App Settings"          = "app_settings",
        "Demo / Testing"        = "sandbox_demo"
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
    rv$jobs_ver  # invalidate when any job/category/template/round mutation fires
    act <- input$config_action %||% "jobs"

    if (act == "jobs") {
      rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                          error = function(e) data.frame())
      rid <- if (nrow(rid_row)) rid_row$id[1] else NA_integer_

      all_cats <- tryCatch(
        db_query(
          "SELECT * FROM job_categories
           WHERE lower(name) IN ('class roles','volunteer','cold call')
           ORDER BY display_order, name;"),
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
                  COALESCE(jp.voluntary,0) AS voluntary,
                  COALESCE(jc.voluntary,0) AS cat_voluntary,
                  COALESCE(NULLIF(jp.selection_time,''), NULLIF(jc.selection_time,''), 'any') AS selection_time
           FROM job_posts jp LEFT JOIN job_categories jc ON jc.id=jp.category_id
           WHERE jp.round_id=?
           ORDER BY jp.display_order, jp.job_name;", list(rid)),
          error = function(e) data.frame())
      } else data.frame()

      templates <- tryCatch(db_query(
        "SELECT jt.id, jt.name, jc.name AS cat_name, jt.slots, jt.suggested_wage,
                COALESCE(jt.active,1) AS active,
                COALESCE(jt.voluntary,0) AS voluntary,
                COALESCE(jt.in_draw,1) AS in_draw,
                COALESCE(NULLIF(jt.selection_time,''),'any') AS selection_time
         FROM job_templates jt LEFT JOIN job_categories jc ON jc.id=jt.category_id
         ORDER BY COALESCE(jt.display_order,99), jt.id;"),
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
        div(style = paste0("background:#f0f4ff;border-left:3px solid #4a6fa5;padding:.5rem .8rem;",
                           "border-radius:0 4px 4px 0;margin-bottom:.6rem;font-size:.85rem;color:#333;"),
          tags$strong("How flags work:"), " ",
          tags$b("\U0001f3b2 In Draw"), " — included when you run the job draw (Panel 1 of Live Tracker). ",
          tags$b("\U0001f64b Voluntary"), " — set at the post or category level; voluntary posts appear in Panel 2 for attendance logging. ",
          "A category can be both in-draw and voluntary."
        ),
        if (is.na(rid)) {
          div(style = "color:#999;font-size:.9em;", "Create a round first (Round Setup).")
        } else if (nrow(all_posts)) {
          div(style = "overflow-x:auto;",
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Post"), tags$th("Cat"), tags$th("Slots"),
                tags$th("Wage"), tags$th("Timing"), tags$th("Clearing Wage"),
                tags$th("In Draw"), tags$th("Voluntary"), tags$th("Active"), tags$th("")
              )),
              tags$tbody(lapply(seq_len(nrow(all_posts)), function(i) {
                r        <- all_posts[i, ]
                is_act   <- isTRUE(as.integer(r$active)  == 1L)
                in_draw  <- isTRUE(as.integer(r$in_draw) == 1L)
                is_vol   <- isTRUE(as.integer(r$voluntary) == 1L) || isTRUE(as.integer(r$cat_voluntary) == 1L)
                clr_wage <- compute_clearing_wage(r$category_id, rid, as.integer(r$slots %||% 1L))
                tags$tr(
                  tags$td(r$job_name %||% ""),
                  tags$td(style = "color:#888;font-size:.82em;", r$cat_name %||% "—"),
                  tags$td(r$slots %||% 1),
                  tags$td(sprintf("%g", as.numeric(r$eff_wage %||% 0))),
                  tags$td(
                    tags$button(
                      class = "btn btn-xs btn-outline-info",
                      style = "padding:.1rem .35rem;font-size:.7rem;",
                      title = "Cycle timing: anytime, start, during (cold call), end, volunteer",
                      onclick = sprintf(
                        "Shiny.setInputValue('cycle_post_timing_btn',%d,{priority:'event'});",
                        as.integer(r$id)),
                      switch(as.character(r$selection_time %||% "any"),
                             start = "Start", during = "During", end = "End",
                             volunteer = "Volunteer", "Any"))
                  ),
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
                  tags$td(make_flag_btn("\U2713 In Draw", "\U2715 Skip Draw", "toggle_post_in_draw",
                                        r$id, in_draw, "btn-success", "btn-outline-secondary")),
                  tags$td(make_flag_btn("\U2713 Voluntary", "\U2715 Required", "toggle_post_voluntary",
                                        r$id, is_vol, "btn-warning", "btn-outline-secondary")),
                  tags$td(make_flag_btn("\U2713 Active", "\U2715 Inactive", "toggle_post_active",
                                        r$id, is_act, "btn-success", "btn-outline-secondary")),
                  tags$td(
                    tags$button(
                      class = "btn btn-xs btn-outline-primary",
                      style = "padding:.1rem .3rem;font-size:.7rem;margin-right:.15rem;",
                      onclick = sprintf(
                        "Shiny.setInputValue('edit_job_post_open',%d,{priority:'event'});",
                        as.integer(r$id)),
                      "Edit"),
                    tags$button(
                      class = "btn btn-xs btn-outline-danger",
                      style = "padding:.1rem .3rem;font-size:.7rem;",
                      onclick = sprintf(
                        "if(confirm('Delete this job post and its assignments?')){Shiny.setInputValue('delete_job_post_btn',%d,{priority:'event'})}",
                        as.integer(r$id)),
                      "\U274c"))
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
                column(2, selectInput("new_post_cat", "Job type:", choices = all_cat_choices)),
                column(1, numericInput("new_post_slots", "Slots:", value = 1L, min = 1L, step = 1L)),
                column(2, selectInput("new_post_timing", "Timing:",
                                      choices = c("Any" = "any", "Start" = "start",
                                                  "During (cold call)" = "during",
                                                  "End" = "end", "Volunteer" = "volunteer"),
                                      selected = "any")),
                column(1, numericInput("new_post_wage", "Wage:", value = NA, min = 0, step = 1)),
                column(1, tags$br(),
                       checkboxInput("new_post_in_draw", "In draw", value = TRUE)),
                column(2, tags$br(),
                       actionButton("add_job_post_btn", "Add", class = "btn btn-sm btn-primary"))
              )
            )
          )
        },

        # ── Job Types ─────────────────────────────────────────────────────────────
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Job Types"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Job types group related posts and set their default wage. Click a row to expand and edit it, then click Save changes (the form closes automatically)."),
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
                    column(2, textInput(paste0("edit_cat_name_",  cid_js), "Name:",
                                        value = r$name %||% "")),
                    column(2, numericInput(paste0("edit_cat_wage_",  cid_js), "Default wage:",
                                           value = as.numeric(r$default_wage %||% 0),
                                           min = 0, step = 1)),
                    column(3, textInput(paste0("edit_cat_desc_",  cid_js), "Description:",
                                        value = r$description %||% "")),
                    column(2, tags$br(),
                      checkboxInput(paste0("edit_cat_vol_",  cid_js), "Voluntary",
                                   value = isTRUE(as.integer(r$voluntary %||% 0L) == 1L)),
                      checkboxInput(paste0("edit_cat_draw_", cid_js), "In draw by default",
                                   value = isTRUE(as.integer(r$in_draw %||% 1L) == 1L))),
                    column(3, tags$br(),
                      div(style = "display:flex;gap:.4rem;flex-wrap:wrap;",
                        tags$button(
                          class = "btn btn-sm btn-primary",
                          onclick = sprintf(paste0(
                            "var n=document.getElementById('edit_cat_name_%d').value;",
                            "var w=document.getElementById('edit_cat_wage_%d').value;",
                            "var d=document.getElementById('edit_cat_desc_%d').value;",
                            "var v=document.getElementById('edit_cat_vol_%d').checked?1:0;",
                            "var dr=document.getElementById('edit_cat_draw_%d').checked?1:0;",
                            "Shiny.setInputValue('edit_cat_btn',{id:%d,name:n,wage:w,desc:d,vol:v,in_draw:dr},{priority:'event'});",
                            "this.closest('details').removeAttribute('open');",
                            "this.textContent='Saved ✓';",
                            "setTimeout(function(b){b.textContent='Save changes';}",
                            ",1500,this);"),
                            cid_js, cid_js, cid_js, cid_js, cid_js, cid_js),
                          "Save changes"),
                        tags$button(
                          class = "btn btn-sm btn-outline-danger",
                          onclick = sprintf(
                            "if(confirm('Delete job type \"%s\"? Posts in this type will have no job type.')){Shiny.setInputValue('delete_job_cat_btn',%d,{priority:'event'})}",
                            r$name %||% "", cid_js),
                          "Delete")))
                  )
                )
              )
            )
          }))
        } else div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No job types yet."),

        tags$details(
          tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                       "Add job type"),
          div(style = "padding:.5rem 0;",
            fluidRow(
              column(3, textInput("new_cat_name", "Name:")),
              column(2, numericInput("new_cat_wage", "Default wage:", value = 10, min = 0, step = 1)),
              column(3, textInput("new_cat_desc", "Description (optional):")),
              column(2, tags$br(),
                checkboxInput("new_cat_voluntary", "Voluntary", value = FALSE),
                checkboxInput("new_cat_not_in_draw", "Exclude from draw", value = FALSE)),
              column(2, tags$br(),
                     actionButton("add_job_cat_btn", "Add", class = "btn btn-sm btn-primary"))
            )
          )
        ),

        # ── Try-Outcome Wage Multiplier ───────────────────────────────────────────
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Try-Outcome Wage Multiplier"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "When a student Tries (partial credit) on an assigned or voluntary job, they earn this fraction of the wage. Default 0.5 = 50%."),
        {
          current_hwm2 <- tryCatch(as.numeric(get_setting("half_wage_multiplier","0.5")), error=function(e) 0.5)
          tagList(
            numericInput("half_wage_input", "Multiplier (0–1):",
                         value = current_hwm2, min = 0, max = 1, step = 0.05, width = "220px"),
            actionButton("save_hwm_btn", "Save", class = "btn btn-sm btn-primary")
          )
        },

        # ── Volunteer Clearing Wage ───────────────────────────────────────────────
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Volunteer Clearing Wage"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "In wage-bidding rounds, every volunteer in a job type is paid the same equilibrium wage from that round's bids — not their own bid, and nobody is rationed out. ",
               tags$b("Lowest bid"), " pays the cheapest bid in the job type. ",
               tags$b("Demand-based"), " pays the k-th lowest bid, where k is the volunteer post's slots — a standing demand you set once. ",
               tags$b("Posted demand"), " is the same k-th-lowest rule, but you post k for today's class in the Live Tracker (Voluntary Participation panel), e.g. at the start of class; it falls back to the post's slots until you post one. ",
               "With no bids in a job type, the post's default wage is used."),
        selectInput("vol_clearing_rule_sel", NULL, width = "420px",
                    choices = c("Lowest bid" = "lowest",
                                "Demand-based (k-th lowest bid, k = post slots)" = "demand",
                                "Posted demand (set k per class in Live Tracker)" = "posted"),
                    selected = as.character(get_setting("volunteer_clearing_rule", "lowest"))),
        actionButton("save_vol_clearing_btn", "Save", class = "btn btn-sm btn-primary"),

        # ── Templates ─────────────────────────────────────────────────────────────
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Templates"),
        tags$p(style = "color:#555;font-size:.85rem;",
               'Templates with Auto-copy ON are copied as job posts each time you click "Create next round" — ',
               "with their timing, wage, slots, and voluntary/in-draw flags. ",
               "Keep every-class jobs ON and toggle some-session jobs (discussion lead, cold calls) on only for the rounds you want them."),
        if (nrow(templates)) {
          div(style = "overflow-x:auto;",
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Name"), tags$th("Category"), tags$th("Slots"), tags$th("Wage"),
                tags$th("Timing"), tags$th("In Draw"), tags$th("Voluntary"),
                tags$th("Auto-copy"), tags$th("")
              )),
              tags$tbody(lapply(seq_len(nrow(templates)), function(i) {
                r <- templates[i, ]
                is_act  <- isTRUE(as.integer(r$active) == 1L)
                in_draw <- isTRUE(as.integer(r$in_draw) == 1L)
                is_vol  <- isTRUE(as.integer(r$voluntary) == 1L)
                tags$tr(
                  style = if (!is_act) "color:#aaa;" else "",
                  tags$td(r$name %||% ""),
                  tags$td(style = "color:#888;", r$cat_name %||% "—"),
                  tags$td(r$slots %||% 1),
                  tags$td(if (!is.na(r$suggested_wage %||% NA))
                              sprintf("%g", as.numeric(r$suggested_wage)) else "—"),
                  tags$td(
                    tags$button(
                      class = "btn btn-xs btn-outline-info",
                      style = "padding:.1rem .35rem;font-size:.7rem;",
                      title = "Cycle timing: anytime, start, during (cold call), end, volunteer",
                      onclick = sprintf(
                        "Shiny.setInputValue('cycle_template_timing_btn',%d,{priority:'event'});",
                        as.integer(r$id)),
                      switch(as.character(r$selection_time %||% "any"),
                             start = "Start", during = "During", end = "End",
                             volunteer = "Volunteer", "Any"))),
                  tags$td(make_flag_btn("\U2713 In Draw", "\U2715 Skip Draw", "toggle_template_in_draw",
                                        r$id, in_draw, "btn-success", "btn-outline-secondary")),
                  tags$td(make_flag_btn("\U2713 Voluntary", "\U2715 Required", "toggle_template_voluntary",
                                        r$id, is_vol, "btn-warning", "btn-outline-secondary")),
                  tags$td(make_flag_btn("\U2713 Auto-copy", "\U2715 Off", "toggle_template_active",
                                        r$id, is_act, "btn-success", "btn-outline-secondary")),
                  tags$td(
                    tags$button(
                      class = "btn btn-xs btn-outline-primary",
                      style = "padding:.1rem .35rem;font-size:.72rem;margin-right:.15rem;",
                      onclick = sprintf(
                        "Shiny.setInputValue('edit_template_open',%d,{priority:'event'});",
                        as.integer(r$id)),
                      "Edit"),
                    tags$button(
                      class = "btn btn-xs btn-outline-danger",
                      style = "padding:.1rem .35rem;font-size:.72rem;",
                      onclick = sprintf(
                        "if(confirm('Delete template \"%s\"? Existing job posts are not affected.')){Shiny.setInputValue('remove_template_btn',%d,{priority:'event'})}",
                        r$name %||% "", as.integer(r$id)),
                      "\U274c"))
                )
              }))
            )
          )
        } else {
          div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No templates yet.")
        },

        {
          tpl_cat_choices <- if (nrow(all_cats))
            setNames(all_cats$id, all_cats$name)
          else c("(no job types)" = "")
          tags$details(
            tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                         "Add template"),
            div(style = "padding:.5rem 0;",
              fluidRow(
                column(3, textInput("new_tpl_name", "Name:")),
                column(3, selectInput("new_tpl_cat", "Job type:", choices = tpl_cat_choices)),
                column(2, numericInput("new_tpl_slots", "Slots:", value = 1L, min = 1L, step = 1L)),
                column(2, numericInput("new_tpl_wage", "Suggested wage:", value = NA, min = 0, step = 1)),
                column(2, selectInput("new_tpl_timing", "Timing:",
                                      choices = c("Any" = "any", "Start" = "start",
                                                  "During (cold call)" = "during",
                                                  "End" = "end", "Volunteer" = "volunteer"),
                                      selected = "any"))
              ),
              fluidRow(
                column(2, checkboxInput("new_tpl_voluntary", "Voluntary", value = FALSE)),
                column(3, checkboxInput("new_tpl_not_in_draw", "Exclude from draw", value = FALSE)),
                column(2, actionButton("add_template_btn", "Add", class = "btn btn-sm btn-primary"))
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
                selectInput("edit_round_tiebreak", "Bid tie-break method:",
                  choices = c(
                    "First submitted (earliest bid wins)"  = "first_submitted",
                    "Random (coin flip among tied bids)"   = "random",
                    "Lowest grade (struggling students first)" = "lowest_grade",
                    "Fewest semester tokens (most behind wins)" = "lowest_tokens",
                    "Weighted lottery (more tickets for fewer tokens)" = "weighted_lottery",
                    "Most misses (most missed events wins)" = "most_misses",
                    "Alphabetical"                         = "alphabetical"
                  ),
                  selected = r$tiebreak_method %||% "weighted_lottery"),
                checkboxInput("edit_round_delayed_tokens",
                  "Delay token reveal (students see pass/try/miss but not amounts until you release)",
                  value = isTRUE(as.integer(r$tokens_revealed %||% 1L) == 0L)),
                div(style = "display:flex;gap:.5rem;margin-top:.3rem;",
                  actionButton("update_round_btn", "Update round", class = "btn btn-sm btn-primary"),
                  tags$button(
                    class = "btn btn-sm btn-outline-danger",
                    onclick = sprintf(
                      "if(confirm('Delete round \"%s\"? This also removes all its job posts, assignments, and bids.')){Shiny.setInputValue('delete_round_btn',%d,{priority:'event'})}",
                      r$label %||% paste("Round", r$id), as.integer(r$id)),
                    "Delete round")
                )
              )
            )
          )
        } else {
          div(style = "color:#999;font-size:.9em;margin-top:.5rem;", "No rounds yet.")
        },
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Create New Round"),
        div(style = paste0("background:#f0f4ff;border-left:3px solid #4a6fa5;padding:.4rem .7rem;",
                           "border-radius:0 4px 4px 0;margin-bottom:.5rem;font-size:.84rem;"),
          tags$b("Random draw"), " runs per section — select a section in Live Tracker before drawing. ",
          tags$b("Bidding"), " collects bids weekly from all students; the draw then resolves ties by the method below."
        ),
        textInput("new_round_label", "Label (e.g. Week 3):"),
        selectInput("new_round_mode", "Assignment mode:", choices = mode_choices),
        selectInput("new_round_tiebreak", "Bid tie-break method:",
          choices = c(
          "First submitted"           = "first_submitted",
          "Random"                    = "random",
          "Lowest grade"              = "lowest_grade",
          "Fewest tokens"             = "lowest_tokens",
          "Weighted lottery"          = "weighted_lottery",
          "Most misses"               = "most_misses",
          "Alphabetical"              = "alphabetical"
        ), selected = "weighted_lottery"),
        checkboxInput("new_round_delayed_tokens",
          "Delay token reveal (students see outcome but not amounts until you release)",
          value = TRUE),
        fluidRow(
          column(4, dateInput("new_round_open",  "Bid opens:")),
          column(4, dateInput("new_round_close", "Bid closes:")),
          column(4, numericInput("new_round_tix", "Tickets/student:", value = 10L, min = 1, step = 1))
        ),
        actionButton("create_round_btn", "Create round", class = "btn btn-sm btn-primary"),
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Auto-Create Next Round"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "A round is one class session. This increments the label, keeps the last round's settings, and copies every Auto-copy template as a job post."),
        actionButton("create_next_round_btn", "Create next round",
                     class = "btn btn-sm btn-success"),

        # ── Bid lock schedule ─────────────────────────────────────────────────
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Bid Lock Schedule"),
        {
          bl <- bid_lock_status()
          tagList(
            tags$p(style = "color:#555;font-size:.85rem;",
                   "Bidding stays open continuously, but on class days it locks before class and reopens that evening. ",
                   if (bl$enabled) tags$b(bl$schedule_label) else tags$b("Currently disabled — bids never lock."),
                   if (bl$locked) span(style = "color:#b00020;font-weight:600;", " Bids are locked right now.")),
            checkboxInput("bl_enabled", "Enable recurring bid lock",
                          value = bl$enabled),
            checkboxGroupInput("bl_days", "Class days:",
                               choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                               selected = bl$days, inline = TRUE),
            fluidRow(
              column(3, textInput("bl_class_time", "Class starts (24h HH:MM):",
                                  value = get_setting("class_start_time", "12:00"))),
              column(3, numericInput("bl_lead_min", "Lock this many min before:",
                                     value = suppressWarnings(as.integer(get_setting("bid_lock_lead_min", "60"))) %||% 60L,
                                     min = 0, step = 5)),
              column(3, textInput("bl_reopen_time", "Reopen at (24h HH:MM):",
                                  value = get_setting("bid_reopen_time", "17:00"))),
              column(3, textInput("bl_tz", "Time zone:",
                                  value = get_setting("class_tz", "America/New_York")))
            ),
            actionButton("save_bid_lock_btn", "Save bid lock schedule",
                         class = "btn btn-sm btn-primary")
          )
        }
      )

    } else if (act == "students") {
      rv$students_ver
      students <- tryCatch(db_query(
        "SELECT user_id, display_name, section,
                COALESCE(active,1) AS active, COALESCE(is_admin,0) AS is_admin
         FROM users WHERE COALESCE(is_demo,0)=0
         ORDER BY section, display_name;"),
        error = function(e) data.frame())
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Student Roster"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Archived students stay here so you can restore them if they were removed by mistake."),
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
                          "Shiny.setInputValue('edit_student_open',%s,{priority:'event'});",
                          jsonlite::toJSON(r$user_id, auto_unbox = TRUE)),
                        class = "btn btn-xs btn-outline-secondary",
                        style = "padding:.1rem .35rem;font-size:.72rem;margin-right:.2rem;",
                        "Edit"),
                      tags$button(
                        onclick = sprintf(
                          "Shiny.setInputValue('impersonate_uid',%s,{priority:'event'});",
                          jsonlite::toJSON(r$user_id, auto_unbox = TRUE)),
                        class = "btn btn-xs btn-outline-primary",
                        style = "padding:.1rem .35rem;font-size:.72rem;margin-right:.2rem;",
                        "View as"),
                      tags$button(
                        onclick = sprintf(
                          "Shiny.setInputValue('archive_uid',%s,{priority:'event'});",
                          jsonlite::toJSON(r$user_id, auto_unbox = TRUE)),
                        class = "btn btn-xs btn-outline-warning",
                        style = "padding:.1rem .35rem;font-size:.72rem;",
                        "Archive")
                    )
                  } else {
                    tagList(
                      tags$button(
                        onclick = sprintf(
                          "Shiny.setInputValue('edit_student_open',%s,{priority:'event'});",
                          jsonlite::toJSON(r$user_id, auto_unbox = TRUE)),
                        class = "btn btn-xs btn-outline-secondary",
                        style = "padding:.1rem .35rem;font-size:.72rem;margin-right:.2rem;",
                        "Edit"),
                      tags$button(
                        onclick = sprintf(
                          "Shiny.setInputValue('restore_uid',%s,{priority:'event'});",
                          jsonlite::toJSON(r$user_id, auto_unbox = TRUE)),
                        class = "btn btn-xs btn-outline-secondary",
                        style = "padding:.1rem .35rem;font-size:.72rem;",
                        "Restore")
                    )
                  }
                )
              )
            })))
          )
        } else div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;", "No students."),
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Add Student"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Use the student's Vassar email as username. Password is optional for Google-only accounts."),
        fluidRow(
          column(3, textInput("new_stu_uid", "Username:")),
          column(3, textInput("new_stu_name", "Display name:")),
          column(2, textInput("new_stu_section", "Section:")),
          column(3, passwordInput("new_stu_pw", "Password (optional):")),
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
        ),
        tags$hr(),
        tags$h6(style = "font-weight:700;color:#951829;", "Bulk Upload Students"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Upload a CSV, then map its columns to username/email, display name, section, and optional password. ",
               "Leave password unmapped for Google-only student accounts."),
        downloadButton("dl_student_template", "Download CSV template",
                       class = "btn btn-sm btn-outline-secondary"),
        tags$br(), tags$br(),
        fileInput("upload_students_csv", NULL, accept = ".csv",
                  buttonLabel = "Choose CSV", placeholder = "No file chosen"),
        uiOutput("student_csv_mapper"),
        checkboxInput("upload_stu_update",
                      "Update existing students (display name + section; password only if provided in CSV)",
                      value = FALSE),
        actionButton("bulk_upload_students_btn", "Upload",
                     class = "btn btn-sm btn-primary")
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
        "SELECT id, question_text, order_index, active, exam_tag FROM flex_questions ORDER BY order_index ASC, id ASC;"),
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
                  placeholder = "e.g. 2,4,6,8,10  or  11+q^2"),
        tags$p(style = "color:#888;font-size:.82em;margin-top:-.4rem;",
          tags$b("Table:"), " comma-separated costs in order (e.g. ", tags$code("2,4,6,8,10"),
          ") — last value repeats beyond the list. ",
          tags$b("Expression:"), " any arithmetic formula in ", tags$code("q"),
          " where q = number of questions already owned (e.g. ", tags$code("11+q^2"), ")."),
        actionButton("save_flex_cost_btn", "Save schedule", class = "btn btn-sm btn-primary"),
        # Live preview of first 8 question costs
        {
          sched_preview <- parse_flex_cost(cur_schedule)
          costs_preview <- sapply(1:8, function(i) question_cost_for_n(i, cur_schedule))
          tags$div(style = "margin-top:.6rem;",
            tags$p(style = "font-size:.82em;color:#555;margin-bottom:.2rem;font-weight:600;",
                   "Preview (first 8 questions):"),
            div(style = "display:flex;gap:.4rem;flex-wrap:wrap;",
              lapply(seq_along(costs_preview), function(i)
                div(style = "background:#f0f4f8;border-radius:5px;padding:.2rem .5rem;font-size:.82rem;text-align:center;min-width:3rem;",
                  div(style = "color:#888;font-size:.72rem;", paste0("Q", i)),
                  div(style = "font-weight:600;", costs_preview[i])
                )
              )
            )
          )
        },
        tags$hr(),

        # Question table
        tags$h6(style = "font-weight:700;", "Questions"),
        if (nrow(fqs)) {
          tags$table(class = "table table-sm",
            tags$thead(tags$tr(
              tags$th("#"), tags$th("Question"), tags$th("Exam"), tags$th("Active"), tags$th("")
            )),
            tags$tbody(lapply(seq_len(nrow(fqs)), function(i) {
              r <- fqs[i, ]
              is_active <- isTRUE(as.integer(r$active %||% 1L) == 1L)
              tags$tr(
                tags$td(style = "color:#888;width:2rem;", i),
                tags$td(style = "font-size:.85rem;max-width:22rem;word-break:break-word;",
                        r$question_text %||% ""),
                tags$td(style = "font-size:.82rem;color:#555;white-space:nowrap;",
                        r$exam_tag %||% "—"),
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
            textInput("new_fq_exam", "Exam (optional):", placeholder = "e.g. Midterm 1"),
            actionButton("add_flex_question_btn", "Add question",
                         class = "btn btn-sm btn-primary")
          )
        ),

        # Upload
        tags$hr(),
        tags$h6(style = "font-weight:700;", "Upload Questions"),
        tags$p(style = "color:#555;font-size:.85rem;",
               "Upload a plain-text file (one question per non-empty line) or a CSV with 'question_text' and optional 'exam_tag' columns."),
        fileInput("upload_flex_questions", NULL,
                  accept = c(".txt", ".md", ".csv", ".yaml", ".yml"),
                  buttonLabel = "Choose file", placeholder = "No file chosen"),
        textInput("upload_fq_exam", "Apply exam tag to all uploaded questions (optional):",
                  placeholder = "e.g. Final — overridden by CSV's exam_tag column"),
        checkboxInput("fq_replace_all", "Replace all existing questions", value = FALSE),
        actionButton("upload_flex_questions_btn", "Upload", class = "btn btn-sm btn-primary")
      )

    } else if (act == "gradebook") {
      rv$gradebook_ver
      cats <- tryCatch(db_query(
        "SELECT * FROM gradebook_categories ORDER BY display_order, id;"),
        error = function(e) data.frame())
      inames <- tryCatch(db_query(
        "SELECT * FROM gradebook_item_names ORDER BY category_id, item_index;"),
        error = function(e) data.frame())
      grade_rows <- tryCatch(db_query(
        "SELECT sg.user_id, u.display_name, u.section, sg.assignment_name,
                sg.score, sg.max_score, sg.grade_pct, sg.week_tag
         FROM student_grades sg LEFT JOIN users u ON u.user_id=sg.user_id
         ORDER BY u.section, u.display_name, sg.assignment_name;"),
        error = function(e) data.frame())
      rw_costs_str  <- tryCatch(get_setting("reweight_cost_schedule", "1:2,2:5,3:9,4:14,5:20"),
                                error = function(e) "1:2,2:5,3:9,4:14,5:20")
      rw_max_pts_cur <- get_rw_max_points()
      sections_df <- tryCatch(db_query(
        "SELECT DISTINCT section FROM users WHERE COALESCE(is_admin,0)=0 AND COALESCE(active,1)=1 AND section IS NOT NULL AND section != '';"),
        error = function(e) data.frame())
      sec_choices <- c("All sections" = "all", sort(sections_df$section %||% character(0)))

      get_item_names_for_cat <- function(cat_row) {
        gradebook_item_specs(cat_row, inames)$item_name
      }

      sec_hdr <- function(n, lbl) tags$h6(
        style = "font-weight:700;border-bottom:1px solid #eee;padding-bottom:.3rem;margin-top:.8rem;",
        sprintf("%d. %s", n, lbl))

      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "Grades & Gradebook"),

        # ── 1. Grade Categories ─────────────────────────────────────────────────
        sec_hdr(1L, "Grade Categories"),
        tags$p(style = "color:#555;font-size:.85rem;",
          "Define categories with weights and items. These drive the gradebook template, grade reweighting, and bid tiebreaks."),

        # Category list
        if (!nrow(cats)) {
          div(style = "color:#999;font-size:.9em;margin-bottom:.5rem;",
              "No categories defined yet. Add one below.")
        } else {
          total_w <- sum(as.numeric(cats$weight %||% 0), na.rm = TRUE)
          tagList(
            div(style = "overflow-x:auto;",
              tags$table(class = "table table-sm",
                tags$tbody(lapply(seq_len(nrow(cats)), function(i) {
                  r       <- cats[i, ]
                  cid_js  <- as.integer(r$id)
                  specs   <- gradebook_item_specs(r, inames)
                  nm_list <- specs$item_name
                  src     <- r$source %||% "manual"
                  is_part <- identical(src, "participation")
                  inp_style <- "width:100%;font-size:.82rem;padding:.15rem .35rem;border:1px solid #ddd;border-radius:4px;"
                  tags$tr(
                    tags$td(colspan = "5",
                      tags$details(
                        tags$summary(style = "cursor:pointer;font-weight:600;",
                          r$name %||% "", " ",
                          tags$small(style = "color:#888;font-weight:400;font-size:.82rem;",
                            sprintf("%.4g%% · %d item%s · %s",
                                    as.numeric(r$weight %||% 0),
                                    as.integer(r$item_count %||% 1),
                                    if (as.integer(r$item_count %||% 1) == 1L) "" else "s",
                                    if (is_part) "auto" else sprintf("max %g", as.numeric(r$max_points %||% 100))))
                        ),
                        div(style = "padding:.5rem .25rem;",
                          # Edit form
                          tags$p(style = "color:#951829;font-size:.82rem;font-weight:600;margin-bottom:.35rem;",
                                 "Edit category:"),
                          div(style = "display:grid;grid-template-columns:repeat(3,1fr);gap:.4rem .7rem;margin-bottom:.45rem;",
                            div(tags$label(style = "font-size:.78rem;color:#555;display:block;", "Name"),
                                tags$input(type="text", id=sprintf("gbcat_name_%d",   cid_js), value=r$name %||% "",                          style=inp_style)),
                            div(tags$label(style = "font-size:.78rem;color:#555;display:block;", "Weight %"),
                                tags$input(type="number", id=sprintf("gbcat_weight_%d", cid_js), value=as.numeric(r$weight %||% 0), min=0, max=100, step=0.5, style=inp_style)),
                            div(tags$label(style = "font-size:.78rem;color:#555;display:block;", "# of items"),
                                tags$input(type="number", id=sprintf("gbcat_count_%d",  cid_js), value=as.integer(r$item_count %||% 1), min=1, step=1, style=inp_style)),
                            div(tags$label(style = "font-size:.78rem;color:#555;display:block;", "Item prefix"),
                                tags$input(type="text", id=sprintf("gbcat_prefix_%d", cid_js), value=r$item_prefix %||% "",                    style=inp_style)),
                            div(tags$label(style = "font-size:.78rem;color:#555;display:block;", "Max pts/item"),
                                tags$input(type="number", id=sprintf("gbcat_max_%d",    cid_js), value=as.integer(r$max_points %||% 100), min=0, step=1, style=inp_style)),
                            div(tags$label(style = "font-size:.78rem;color:#555;display:block;", "Source"),
                                tags$select(id=sprintf("gbcat_source_%d", cid_js), style=inp_style,
                                  tags$option(value="manual",        `selected`=if (!is_part) "selected" else NULL, "Manual entry"),
                                  tags$option(value="participation", `selected`=if ( is_part) "selected" else NULL, "Participation (auto from app)")))
                          ),
                          div(style = "display:flex;gap:.4rem;margin-bottom:.6rem;",
                            tags$button(
                              class = "btn btn-xs btn-primary",
                              style = "padding:.2rem .6rem;font-size:.78rem;",
                              onclick = sprintf(paste0(
                                "var n=document.getElementById('gbcat_name_%d').value;",
                                "var w=document.getElementById('gbcat_weight_%d').value;",
                                "var c=document.getElementById('gbcat_count_%d').value;",
                                "var p=document.getElementById('gbcat_prefix_%d').value;",
                                "var m=document.getElementById('gbcat_max_%d').value;",
                                "var s=document.getElementById('gbcat_source_%d').value;",
                                "Shiny.setInputValue('edit_gb_cat_btn',",
                                "{id:%d,name:n,weight:w,count:c,prefix:p,max:m,source:s},",
                                "{priority:'event'});",
                                "this.closest('details').removeAttribute('open');"),
                                cid_js,cid_js,cid_js,cid_js,cid_js,cid_js,cid_js),
                              "Save changes"),
                            tags$button(
                              class = "btn btn-xs btn-outline-danger",
                              style = "padding:.2rem .5rem;font-size:.78rem;",
                              onclick = sprintf(
                                "if(confirm('Delete this category?')){Shiny.setInputValue('delete_gb_cat_btn',%d,{priority:'event'})}",
                                cid_js),
                              "Delete")
                          ),
                          # Item name overrides
                          tags$hr(style = "margin:.3rem 0;"),
                          tags$p(style = "color:#555;font-size:.82rem;margin-bottom:.3rem;",
                                 "Override item names and weights. Leave weight blank for equal split within the category."),
                          lapply(seq_along(nm_list), function(j) {
                            wt_val <- if (isTRUE(specs$custom_weight[j])) as.numeric(specs$item_weight[j]) else ""
                            div(style = "display:flex;align-items:center;gap:.4rem;margin-bottom:.25rem;",
                              tags$span(style = "font-size:.78rem;color:#888;width:1.8rem;text-align:right;", paste0(j, ".")),
                              tags$input(type="text", id=sprintf("gbi_%d_%d", cid_js, j),
                                         value=nm_list[j],
                                         style="font-size:.82rem;padding:.15rem .35rem;border:1px solid #ddd;border-radius:4px;width:14rem;"),
                              tags$input(type="number", id=sprintf("gbiw_%d_%d", cid_js, j),
                                         value=wt_val, min=0, max=100, step=0.5,
                                         placeholder=sprintf("%.4g", as.numeric(specs$item_weight[j] %||% 0)),
                                         title="Weight percentage points of total grade; blank = equal split",
                                         style="font-size:.82rem;padding:.15rem .35rem;border:1px solid #ddd;border-radius:4px;width:5.8rem;"),
                              tags$button(
                                class="btn btn-xs btn-outline-secondary",
                                style="padding:.1rem .4rem;font-size:.72rem;",
                                onclick=sprintf(paste0(
                                  "var v=document.getElementById('gbi_%d_%d').value;",
                                  "var w=document.getElementById('gbiw_%d_%d').value;",
                                  "Shiny.setInputValue('rename_gb_item_btn',",
                                  "{cat_id:%d,idx:%d,name:v,weight:w},{priority:'event'});"),
                                  cid_js, j, cid_js, j, cid_js, j),
                                "Save"))
                          })
                        )
                      )
                    )
                  )
                }))
              )
            ),
            tags$p(style = sprintf("font-size:.82rem;%s;margin-top:-.4rem;",
                                   if (abs(total_w - 100) < 0.01) "color:#1a6e3c;" else "color:#856404;font-weight:600;"),
                   sprintf("Total weight: %.1f%% %s", total_w,
                           if (abs(total_w - 100) < 0.01) "\U2713" else "(should sum to 100%)"))
          )
        },
        tags$hr(),

        # Add category form
        tags$details(
          tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;font-weight:600;",
                       "Add grade category"),
          div(style = "padding:.5rem 0;",
            fluidRow(
              column(3, textInput("new_gb_name", "Category name:", placeholder = "e.g. Problem Sets")),
              column(2, numericInput("new_gb_weight", "Weight (%):", value = NA, min = 0, max = 100, step = 0.5)),
              column(2, numericInput("new_gb_count", "# of items:", value = 1, min = 1, step = 1)),
              column(2, textInput("new_gb_prefix", "Item prefix:", placeholder = "e.g. Pset")),
              column(2, numericInput("new_gb_max", "Max pts/item:", value = 100, min = 0, step = 1))
            ),
            fluidRow(
              column(4, selectInput("new_gb_source", "Data source:",
                choices = c("Manual entry" = "manual",
                            "Participation tokens (auto from app)" = "participation"))),
              column(2, tags$br(),
                     actionButton("add_gb_cat_btn", "Add", class = "btn btn-sm btn-primary"))
            ),
            tags$p(style = "color:#888;font-size:.78rem;margin:.25rem 0 0;",
                   "Item prefix + number = column name (e.g. 'Pset' → 'Pset 1', 'Pset 2'). Leave blank to use category name.")
          )
        ),
        tags$hr(),

        # ── 2. Upload Grades ────────────────────────────────────────────────────
        sec_hdr(2L, "Upload Grades"),
        tags$p(style = "color:#555;font-size:.85rem;",
          "Required columns: ", tags$code("user_id"), " (or ", tags$code("student_id"),
          ") and ", tags$code("assignment"), " (or ", tags$code("assignment_name"), "). ",
          "Optional: ", tags$code("score"), ", ", tags$code("max_score"), ", ",
          tags$code("grade_pct"), " (0–100). Assignment names should match your item names above."),
        fluidRow(
          column(5, fileInput("grade_file_upload", NULL,
                              accept = c(".csv",".xls",".xlsx"), width = "100%")),
          column(3, textInput("grade_week_tag", "Week tag (optional):", width = "100%")),
          column(2, tags$br(),
                 actionButton("upload_grades_btn", "Upload", class = "btn btn-sm btn-primary")),
          column(2, tags$br(),
                 actionButton("clear_grades_btn", "Clear All",
                              class = "btn btn-sm btn-outline-danger",
                              onclick = "if(!confirm('Delete all grade records?')) return false;"))
        ),

        tags$hr(),

        # ── 3. Grades View & Downloads ──────────────────────────────────────────
        sec_hdr(3L, "Grades View & Downloads"),
        if (!nrow(cats)) {
          tags$p(style = "color:#999;font-size:.9em;", "Define categories first.")
        } else {
          # Build item → category mapping
          item_cat_map <- if (nrow(cats)) do.call(rbind, lapply(seq_len(nrow(cats)), function(i) {
            r       <- cats[i, ]
            specs   <- gradebook_item_specs(r, inames)
            data.frame(item_name = specs$item_name,
                       cat_id    = as.integer(r$id),
                       cat_name  = r$name %||% "",
                       weight    = as.numeric(r$weight %||% 0),
                       item_weight = as.numeric(specs$item_weight %||% 0),
                       source    = r$source %||% "manual",
                       stringsAsFactors = FALSE)
          })) else data.frame()

          tagList(
            # Downloads
            fluidRow(
              column(4, selectInput("gb_template_section", "Section:", choices = sec_choices)),
              column(8, tags$br(),
                downloadButton("dl_gradebook_template", "Blank template",
                               class = "btn btn-sm btn-outline-secondary"),
                " ",
                downloadButton("dl_gradebook_filled", "Filled gradebook",
                               class = "btn btn-sm btn-outline-primary"))
            ),
            tags$p(style = "color:#888;font-size:.78rem;margin-top:.25rem;",
              tags$b("Blank template:"), " headers + participation pre-filled, manual columns empty. ",
              tags$b("Filled gradebook:"), " all uploaded scores filled in, category averages and weighted total appended."),

            # Online grade summary (if grades uploaded)
            if (!nrow(grade_rows)) {
              tags$p(style = "color:#999;font-size:.9em;margin-top:.5rem;",
                     "No grades uploaded yet. Use Upload Grades above.")
            } else {
              # Map grades to categories
              gr_mapped <- if (nrow(item_cat_map))
                merge(grade_rows, item_cat_map, by.x = "assignment_name", by.y = "item_name", all.x = TRUE)
              else grade_rows

              # Unique students in grade data
              stu_ids <- unique(grade_rows$user_id)
              cat_names <- if (nrow(cats)) cats$name %||% character(0) else character(0)

              tagList(
                tags$p(style = "font-size:.85rem;color:#555;margin-bottom:.3rem;",
                  sprintf("%d students · %d grade rows · %d distinct assignments",
                          length(stu_ids), nrow(grade_rows),
                          length(unique(grade_rows$assignment_name)))),
                div(style = "overflow-x:auto;",
                  tags$table(class = "table table-sm",
                    tags$thead(tags$tr(tagList(
                      tags$th("Student"), tags$th("Sec"),
                      lapply(cat_names, function(cn) {
                        w <- cats$weight[cats$name == cn][1] %||% 0
                        tags$th(style = "text-align:right;",
                                sprintf("%s (%.4g%%)", cn, as.numeric(w)))
                      }),
                      tags$th(style = "text-align:right;font-weight:700;", "Wtd Total")
                    ))),
                    tags$tbody(lapply(stu_ids, function(uid) {
                      stu_nm  <- grade_rows$display_name[grade_rows$user_id == uid][1] %||% uid
                      stu_sec <- grade_rows$section[grade_rows$user_id == uid][1] %||% ""
                      stu_gr  <- if (nrow(gr_mapped)) gr_mapped[!is.na(gr_mapped$user_id) & gr_mapped$user_id == uid, , drop=FALSE] else data.frame()
                      wt_num <- 0; wt_den <- 0
                      cat_cells <- lapply(cat_names, function(cn) {
                        cat_gr <- if (nrow(stu_gr) && "cat_name" %in% names(stu_gr))
                          stu_gr[!is.na(stu_gr$cat_name) & stu_gr$cat_name == cn, , drop=FALSE]
                        else data.frame()
                        avg <- NA_real_
                        graded_cat <- if (nrow(cat_gr)) cat_gr[!is.na(cat_gr$grade_pct), , drop=FALSE] else data.frame()
                        if (nrow(graded_cat)) {
                          iw <- as.numeric(graded_cat$item_weight %||% 0)
                          avg <- if (sum(iw, na.rm = TRUE) > 0)
                            sum(graded_cat$grade_pct * iw, na.rm = TRUE) / sum(iw, na.rm = TRUE)
                          else mean(graded_cat$grade_pct, na.rm = TRUE)
                        }
                        if (!is.na(avg)) {
                          w <- sum(as.numeric(graded_cat$item_weight %||% 0), na.rm = TRUE)
                          wt_num <<- wt_num + avg * w
                          wt_den <<- wt_den + w
                        }
                        tags$td(style = "text-align:right;",
                                if (!is.na(avg)) sprintf("%.1f%%", avg) else tags$span(style="color:#ccc;","—"))
                      })
                      wtd <- if (wt_den > 0) sprintf("%.1f%%", wt_num / wt_den) else "—"
                      tags$tr(
                        tags$td(stu_nm),
                        tags$td(style="color:#888;font-size:.82em;", stu_sec),
                        tagList(cat_cells),
                        tags$td(style="text-align:right;font-weight:600;", wtd)
                      )
                    }))
                  )
                ),
                # Raw rows
                tags$details(style = "margin-top:.4rem;",
                  tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;",
                               sprintf("All raw rows (%d)", nrow(grade_rows))),
                  div(style = "overflow-x:auto;max-height:380px;overflow-y:auto;margin-top:.4rem;",
                    tags$table(class = "table table-sm",
                      tags$thead(tags$tr(
                        tags$th("Student"), tags$th("Assignment"), tags$th("Category"),
                        tags$th("Score"), tags$th("Max"), tags$th("%"), tags$th("Week")
                      )),
                      tags$tbody(lapply(seq_len(nrow(grade_rows)), function(i) {
                        r   <- grade_rows[i, ]
                        cat_lbl <- if (nrow(item_cat_map)) {
                          m <- item_cat_map$cat_name[item_cat_map$item_name == (r$assignment_name %||% "")]
                          if (length(m) && nzchar(m[1])) m[1] else tags$span(style="color:#ccc;","—")
                        } else "—"
                        tags$tr(
                          tags$td(style="font-size:.82em;", r$display_name %||% r$user_id),
                          tags$td(r$assignment_name %||% ""),
                          tags$td(style="color:#888;font-size:.82em;", cat_lbl),
                          tags$td(if (!is.na(r$score))    r$score    else "—"),
                          tags$td(if (!is.na(r$max_score)) r$max_score else "—"),
                          tags$td(if (!is.na(r$grade_pct)) sprintf("%.1f%%", r$grade_pct) else "—"),
                          tags$td(style="color:#888;font-size:.82em;", r$week_tag %||% "")
                        )
                      }))
                    )
                  )
                ),
                # Reweight requests per student
                {
                  rw_rows <- tryCatch(db_query(
                    "SELECT r.id, u.display_name, COALESCE(r.level,'category') AS level,
                            r.from_category, r.to_category,
                            r.points, r.cost, r.status, r.created_at
                     FROM grade_reweight_requests r
                     LEFT JOIN users u ON u.user_id=r.user_id
                     ORDER BY r.created_at DESC LIMIT 50;"),
                    error = function(e) data.frame())
                  if (nrow(rw_rows)) {
                    tags$details(style = "margin-top:.4rem;",
                      tags$summary(style = "cursor:pointer;color:#951829;font-size:.88rem;",
                                   sprintf("Student weight adjustment requests (%d)", nrow(rw_rows))),
                      div(style = "overflow-x:auto;margin-top:.4rem;",
                        tags$table(class = "table table-sm",
                          tags$thead(tags$tr(
                            tags$th("Student"), tags$th("Level"), tags$th("From"), tags$th("To"),
                            tags$th("Pts"), tags$th("Cost"), tags$th("Status"), tags$th("Date")
                          )),
                          tags$tbody(lapply(seq_len(nrow(rw_rows)), function(i) {
                            r <- rw_rows[i, ]
                            lv <- r$level %||% "category"
                            tags$tr(
                              tags$td(r$display_name %||% ""),
                              tags$td(style="color:#888;font-size:.82em;",
                                      if (identical(lv,"assignment")) "Assignment" else "Category"),
                              tags$td(r$from_category %||% ""),
                              tags$td(r$to_category %||% ""),
                              tags$td(r$points %||% ""),
                              tags$td(r$cost %||% ""),
                              tags$td(style = if (identical(r$status, "pending")) "color:#856404;" else "color:#1a6e3c;",
                                      r$status %||% ""),
                              tags$td(style = "color:#888;font-size:.82em;",
                                      tryCatch(format(as.POSIXct(r$created_at), "%b %d"), error=function(e)""))
                            )
                          }))
                        )
                      )
                    )
                  }
                }
              )
            }
          )
        },

        tags$hr(),

        # ── 4. Reweighting Setup ────────────────────────────────────────────────
        sec_hdr(4L, "Grade Reweighting Setup"),
        tags$p(style = "color:#555;font-size:.85rem;",
          "Students can spend tokens (in the Spend tab) to shift grade weight between categories defined in Section 1."),
        fluidRow(
          column(8,
            textInput("rw_costs_input", "Cost formula / schedule:",
                      value = rw_costs_str, width = "100%",
                      placeholder = "e.g. 1:2,2:5,3:9  or  2*n+n^2")),
          column(4,
            numericInput("rw_max_points_input", "Max pts student can move:",
                         value = rw_max_pts_cur, min = 1, step = 1, width = "100%"))
        ),
        tags$p(style = "color:#888;font-size:.82em;margin-top:-.3rem;",
          tags$b("Table:"), " pairs of points:tokens (e.g. ", tags$code("1:2,2:5,3:9"),
          " — moving 1 pt costs 2 tokens, 2 pts costs 5, etc.). ",
          tags$b("Expression:"), " any arithmetic formula in ", tags$code("n"),
          " where n = percentage points being moved (e.g. ", tags$code("2*n+n^2"), "). ",
          "The cap controls the slider maximum shown to students."),
        # Preview table for current formula
        {
          n_preview <- seq_len(rw_max_pts_cur)
          costs_preview <- sapply(n_preview, rw_cost_for_n)
          tags$div(style = "margin-top:.5rem;margin-bottom:.6rem;",
            tags$p(style = "font-size:.82em;color:#555;margin-bottom:.2rem;font-weight:600;",
                   "Cost preview:"),
            div(style = "display:flex;gap:.4rem;flex-wrap:wrap;",
              lapply(n_preview, function(i) {
                cv <- costs_preview[i]
                div(style = "background:#f0f4f8;border-radius:5px;padding:.2rem .5rem;font-size:.82rem;text-align:center;min-width:3.5rem;",
                  div(style = "color:#888;font-size:.72rem;", sprintf("%d pt%s", i, if(i==1)""else"s")),
                  div(style = "font-weight:600;", if (!is.na(cv)) as.integer(cv) else tags$span(style="color:#c00;","?"))
                )
              })
            )
          )
        },
        actionButton("save_rw_setup_btn", "Save reweighting setup", class = "btn btn-sm btn-primary")
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
              column(4, actionButton("adm_reveal", "Reveal", class = "btn btn-danger  btn-sm btn-block",
                                     onclick = "if(!confirm('Reveal round results? This cannot be undone.')) return false;"))
            ),
            tags$p(style = "font-size:.8em;color:#999;margin-top:.5rem;margin-bottom:0;",
                   "For full payout setup use the Coordination Games app.")
          )
        }
      )

    } else if (act == "token_admin") {
      students <- tryCatch(db_query(
        "SELECT u.user_id, u.display_name, u.section,
                COALESCE(SUM(tl.amount),0) AS tokens_on_hand
         FROM users u
         LEFT JOIN token_ledger tl ON tl.user_id=u.user_id
         WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1 AND COALESCE(u.is_demo,0)=0
         GROUP BY u.user_id ORDER BY u.section, u.display_name;"),
        error = function(e) data.frame())
      if (nrow(students)) {
        students$tokens_pending <- 0
        ja_cols <- tryCatch(db_query("PRAGMA table_info(job_assignments);")$name,
                            error = function(e) character(0))
        if (all(c("tokens_awarded", "tokens_credited") %in% ja_cols)) {
          pending <- tryCatch(db_query(
            "SELECT user_id, SUM(COALESCE(tokens_awarded,0)) AS tokens_pending
             FROM job_assignments
             WHERE COALESCE(tokens_credited,1)=0 AND COALESCE(tokens_awarded,0)>0
             GROUP BY user_id;"),
            error = function(e) data.frame())
          if (nrow(pending)) {
            pend_map <- setNames(as.numeric(pending$tokens_pending %||% 0), pending$user_id)
            students$tokens_pending <- as.numeric(pend_map[students$user_id])
            students$tokens_pending[is.na(students$tokens_pending)] <- 0
          }
        }
      }
      sections <- c("All", sort(unique(nonempty_values(students$section))))
      stu_lbl  <- if (nrow(students)) {
        sec_lbl <- as.character(students$section %||% "")
        sec_lbl[is.na(sec_lbl)] <- ""
        nm_lbl  <- as.character(students$display_name %||% students$user_id)
        bad_nm  <- is.na(nm_lbl) | !nzchar(nm_lbl)
        nm_lbl[bad_nm] <- students$user_id[bad_nm]
        ifelse(nzchar(sec_lbl),
               paste0(nm_lbl," (",sec_lbl,")"),
               nm_lbl)
      } else character(0)
      tagList(
        if (nrow(students)) {
          div(style = "margin-bottom:.8rem;overflow-x:auto;",
            tags$p(style = "color:#555;font-size:.82rem;margin-bottom:.3rem;",
                   tags$b("On Hand"), " = tokens in the ledger (released). ",
                   tags$b(style="color:#856404;", "Pending"), " = earned from jobs but not yet released — deductions consume these first."),
            tags$table(class = "table table-sm",
              tags$thead(tags$tr(
                tags$th("Student"), tags$th("Section"),
                tags$th(style="text-align:right;", "On Hand"),
                tags$th(style="text-align:right;color:#856404;", "Pending")
              )),
              tags$tbody(lapply(seq_len(nrow(students)), function(i) {
                r    <- students[i, ]
                pend <- as.integer(r$tokens_pending %||% 0)
                if (is.na(pend)) pend <- 0L
                hand <- as.integer(r$tokens_on_hand %||% 0)
                if (is.na(hand)) hand <- 0L
                dname <- r$display_name %||% r$user_id
                if (is.na(dname) || !nzchar(dname)) dname <- r$user_id
                tags$tr(
                  tags$td(dname),
                  tags$td(style="color:#888;font-size:.82em;", r$section %||% ""),
                  tags$td(style="text-align:right;font-weight:600;",
                          hand),
                  tags$td(style="text-align:right;color:#856404;font-style:italic;",
                          if (pend > 0) pend else "—")
                )
              }))
            )
          )
        },
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


    } else if (act == "app_settings") {
      tagList(
        tags$h6(style = "font-weight:700;color:#951829;margin-top:.5rem;", "App Settings"),
        textInput("new_app_name", "App name:",
                  value = get_config("app_name", "Classroom Economy"), width = "100%"),
        actionButton("save_app_name_btn", "Save", class = "btn btn-sm btn-primary")
      )
    } else if (act == "sandbox_demo") {
      demo_settings_panel(.sandbox)
    }
  })

  output$rw_requests_panel <- renderUI({
    req(rv$is_admin)
    rows <- tryCatch(db_query(
      "SELECT r.id, u.display_name, COALESCE(r.level,'category') AS level,
              r.from_category, r.to_category, r.points,
              r.cost, r.status, r.created_at
       FROM grade_reweight_requests r
       LEFT JOIN users u ON u.user_id=r.user_id
       ORDER BY r.created_at DESC LIMIT 30;"),
      error = function(e) data.frame())
    if (!nrow(rows))
      return(div(style = "color:#999;font-size:.88rem;", "No requests yet."))
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(
        tags$th("Student"), tags$th("Level"), tags$th("From"), tags$th("To"), tags$th("Pts"),
        tags$th("Cost"), tags$th("Status"), tags$th("Date")
      )),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
        r  <- rows[i, ]
        lv <- r$level %||% "category"
        tags$tr(
          tags$td(r$display_name %||% ""),
          tags$td(style="color:#888;font-size:.82em;",
                  if (identical(lv,"assignment")) "Assignment" else "Category"),
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
  output$dl_student_template <- downloadHandler(
    filename = function() "student_upload_template.csv",
    content  = function(file) write.csv(
      data.frame(
        username     = c("jsmith@vassar.edu", "jdoe@vassar.edu"),
        display_name = c("Jane Smith", "John Doe"),
        section      = c("101", "102"),
        password     = c("", ""),
        stringsAsFactors = FALSE),
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
      "SELECT r.id, u.display_name AS student, COALESCE(r.level,'category') AS level,
              r.from_category, r.to_category,
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

  output$dl_gradebook_template <- downloadHandler(
    filename = function() paste0("gradebook_template_", Sys.Date(), ".csv"),
    content  = function(file) {
      sec   <- isolate(input$gb_template_section %||% "all")
      cats  <- tryCatch(db_query(
        "SELECT * FROM gradebook_categories ORDER BY display_order, id;"),
        error = function(e) data.frame())
      inames_df <- tryCatch(db_query(
        "SELECT * FROM gradebook_item_names ORDER BY category_id, item_index;"),
        error = function(e) data.frame())
      students <- tryCatch({
        q <- if (identical(sec, "all"))
          "SELECT u.user_id, u.display_name, u.section,
                  COALESCE(SUM(CASE WHEN tl.earning=1 AND tl.amount>0 THEN tl.amount ELSE 0 END),0) AS tokens_earned
           FROM users u LEFT JOIN token_ledger tl ON tl.user_id=u.user_id
           WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1 AND COALESCE(u.is_demo,0)=0
           GROUP BY u.user_id ORDER BY u.section, u.display_name;"
        else
          "SELECT u.user_id, u.display_name, u.section,
                  COALESCE(SUM(CASE WHEN tl.earning=1 AND tl.amount>0 THEN tl.amount ELSE 0 END),0) AS tokens_earned
           FROM users u LEFT JOIN token_ledger tl ON tl.user_id=u.user_id
           WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1 AND COALESCE(u.is_demo,0)=0
             AND u.section=?
           GROUP BY u.user_id ORDER BY u.section, u.display_name;"
        if (identical(sec, "all")) db_query(q) else db_query(q, list(sec))
      }, error = function(e) data.frame())
      if (!nrow(cats) || !nrow(students)) { write.csv(data.frame(), file, row.names=FALSE); return() }

      # Build column names + max-points row
      col_names  <- character(0)
      col_maxpts <- character(0)
      col_weight <- character(0)
      for (i in seq_len(nrow(cats))) {
        r   <- cats[i, ]
        specs <- gradebook_item_specs(r, inames_df)
        n   <- nrow(specs)
        is_part <- identical(r$source %||% "manual", "participation")
        for (j in seq_len(n)) {
          nm <- specs$item_name[j]
          col_names  <- c(col_names,  nm)
          col_maxpts <- c(col_maxpts, if (is_part) "(from app)" else as.character(as.integer(r$max_points %||% 100)))
          col_weight <- c(col_weight, sprintf("%.4g%%", as.numeric(specs$item_weight[j] %||% 0)))
        }
      }

      # Build data frame: header + max-pts row + weight row + student rows
      n_cols   <- length(col_names)
      part_idx <- which(sapply(seq_len(nrow(cats)), function(i)
        identical(cats$source[i] %||% "manual", "participation")))
      # Column offsets: cumulative item counts per category
      cat_col_start <- c(1L, cumsum(as.integer(cats$item_count %||% 1)) + 1L)

      out_rows <- vector("list", nrow(students))
      for (s in seq_len(nrow(students))) {
        stu   <- students[s, ]
        cells <- rep("", n_cols)
        for (pi in part_idx) {
          span_start <- cat_col_start[pi]
          span_end   <- cat_col_start[pi] + as.integer(cats$item_count[pi] %||% 1) - 1L
          cells[span_start:span_end] <- as.character(as.integer(stu$tokens_earned %||% 0))
        }
        out_rows[[s]] <- c(stu$display_name %||% stu$user_id, stu$section %||% "", cells)
      }

      meta_row1 <- c("(Max Points)", "", col_maxpts)
      meta_row2 <- c("(Weight)",     "", col_weight)
      header    <- c("Student", "Section", col_names)
      all_rows  <- c(list(header, meta_row1, meta_row2), out_rows)
      df_out    <- as.data.frame(do.call(rbind, all_rows), stringsAsFactors = FALSE)
      colnames(df_out) <- header
      write.csv(df_out[-1, ], file, row.names = FALSE)
    }
  )

  output$dl_gradebook_filled <- downloadHandler(
    filename = function() paste0("gradebook_filled_", Sys.Date(), ".csv"),
    content  = function(file) {
      sec   <- isolate(input$gb_template_section %||% "all")
      cats  <- tryCatch(db_query(
        "SELECT * FROM gradebook_categories ORDER BY display_order, id;"),
        error = function(e) data.frame())
      inames_df <- tryCatch(db_query(
        "SELECT * FROM gradebook_item_names ORDER BY category_id, item_index;"),
        error = function(e) data.frame())
      students <- tryCatch({
        q_base <- "SELECT u.user_id, u.display_name, u.section,
                          COALESCE(SUM(CASE WHEN tl.earning=1 AND tl.amount>0 THEN tl.amount ELSE 0 END),0) AS tokens_earned
                   FROM users u LEFT JOIN token_ledger tl ON tl.user_id=u.user_id
                   WHERE COALESCE(u.is_admin,0)=0 AND COALESCE(u.active,1)=1 AND COALESCE(u.is_demo,0)=0"
        if (identical(sec, "all"))
          db_query(paste0(q_base, " GROUP BY u.user_id ORDER BY u.section, u.display_name;"))
        else
          db_query(paste0(q_base, " AND u.section=? GROUP BY u.user_id ORDER BY u.section, u.display_name;"), list(sec))
      }, error = function(e) data.frame())
      grade_rows_dl <- tryCatch(db_query(
        "SELECT sg.user_id, sg.assignment_name, sg.score, sg.max_score, sg.grade_pct, sg.week_tag
         FROM student_grades sg;"),
        error = function(e) data.frame())
      if (!nrow(cats) || !nrow(students)) { write.csv(data.frame(), file, row.names=FALSE); return() }

      # Build item column names per category
      col_names  <- character(0)
      col_maxpts <- character(0)
      col_weight <- character(0)
      cat_col_start <- integer(0)
      cur_col <- 1L
      for (i in seq_len(nrow(cats))) {
        cat_col_start[i] <- cur_col
        r   <- cats[i, ]
        specs <- gradebook_item_specs(r, inames_df)
        n   <- nrow(specs)
        is_part <- identical(r$source %||% "manual", "participation")
        for (j in seq_len(n)) {
          nm <- specs$item_name[j]
          col_names  <- c(col_names,  nm)
          col_maxpts <- c(col_maxpts, if (is_part) "(from app)" else as.character(as.integer(r$max_points %||% 100)))
          col_weight <- c(col_weight, sprintf("%.4g%%", as.numeric(specs$item_weight[j] %||% 0)))
        }
        cur_col <- cur_col + n
      }
      n_cols <- length(col_names)

      # Build category average columns
      cat_avg_names <- paste0(cats$name %||% paste0("Cat", seq_len(nrow(cats))), " Avg%")
      total_weight  <- sum(as.numeric(cats$weight %||% 0), na.rm = TRUE)

      header <- c("Student", "Section", col_names, cat_avg_names, "Weighted Total%")
      meta_row1 <- c("(Max Points)", "", col_maxpts, rep("", nrow(cats) + 1L))
      meta_row2 <- c("(Weight)",     "", col_weight,  rep("", nrow(cats) + 1L))

      out_rows <- vector("list", nrow(students))
      for (s in seq_len(nrow(students))) {
        stu   <- students[s, ]
        cells <- rep(NA_character_, n_cols)
        cat_avgs <- rep(NA_character_, nrow(cats))
        wt_num <- 0; wt_den <- 0

        for (i in seq_len(nrow(cats))) {
          r   <- cats[i, ]
          specs <- gradebook_item_specs(r, inames_df)
          n   <- nrow(specs)
          is_part <- identical(r$source %||% "manual", "participation")
          cs  <- cat_col_start[i]

          item_pcts <- numeric(0)
          item_wts  <- numeric(0)
          for (j in seq_len(n)) {
            col_nm <- specs$item_name[j]
            item_wt <- as.numeric(specs$item_weight[j] %||% 0)
            col_idx <- cs + j - 1L
            if (is_part) {
              tok_val <- as.character(as.integer(stu$tokens_earned %||% 0))
              cells[col_idx] <- tok_val
              maxpts <- as.numeric(r$max_points %||% 100)
              if (!is.na(maxpts) && maxpts > 0) {
                pct <- min(100, 100 * as.numeric(stu$tokens_earned %||% 0) / maxpts)
                item_pcts <- c(item_pcts, pct)
                item_wts  <- c(item_wts, item_wt)
              }
            } else if (nrow(grade_rows_dl)) {
              match_rows <- grade_rows_dl[!is.na(grade_rows_dl$user_id) &
                                          grade_rows_dl$user_id == stu$user_id &
                                          !is.na(grade_rows_dl$assignment_name) &
                                          grade_rows_dl$assignment_name == col_nm, , drop=FALSE]
              if (nrow(match_rows) && !is.na(match_rows$grade_pct[1])) {
                pct_val <- as.numeric(match_rows$grade_pct[1])
                cells[col_idx] <- sprintf("%.1f", pct_val)
                item_pcts <- c(item_pcts, pct_val)
                item_wts  <- c(item_wts, item_wt)
              }
            }
          }

          cat_pct <- if (length(item_pcts) > 0 && sum(item_wts, na.rm = TRUE) > 0)
            sum(item_pcts * item_wts, na.rm = TRUE) / sum(item_wts, na.rm = TRUE)
          else if (length(item_pcts) > 0) mean(item_pcts, na.rm = TRUE)
          else NA_real_
          cat_avgs[i] <- if (!is.na(cat_pct)) sprintf("%.1f%%", cat_pct) else ""
          if (!is.na(cat_pct)) {
            w <- sum(item_wts, na.rm = TRUE)
            wt_num <- wt_num + cat_pct * w
            wt_den <- wt_den + w
          }
        }

        wtd_total <- if (wt_den > 0) sprintf("%.2f%%", wt_num / wt_den) else ""
        cells[is.na(cells)] <- ""
        out_rows[[s]] <- c(stu$display_name %||% stu$user_id, stu$section %||% "",
                           cells, cat_avgs, wtd_total)
      }

      all_rows <- c(list(header, meta_row1, meta_row2), out_rows)
      df_out   <- as.data.frame(do.call(rbind, all_rows), stringsAsFactors = FALSE)
      colnames(df_out) <- header
      write.csv(df_out[-1, ], file, row.names = FALSE)
    }
  )

  # Admin action observers
  observeEvent(input$clear_active_game_btn, {
    req(rv$is_admin, !rv$impersonating)
    db_exec("UPDATE arcade_state SET active_game=NULL, updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    showNotification("Active game cleared.", type = "message")
  })
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
              COALESCE(ja.tokens_awarded,0) AS tokens_awarded,
              ja.round_id
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
      "INSERT INTO live_score_events(round_id, user_id, job_assignment_id, event_kind,
              outcome, tokens, logged_by)
       VALUES(?,?,?,'assignment',?,?,?);",
      list(as.integer(row$round_id[1]), uid, assign_id, outcome, tokens_to_award, rv$user_id %||% "admin"))
    showNotification(
      sprintf("%s queued for audit (%d token%s).", dname, as.integer(tokens_to_award),
              if (tokens_to_award == 1) "" else "s"),
      type = "message")
    return()
    # Check whether tokens should be credited now or held until instructor releases
    rnd_row <- tryCatch(db_query("SELECT COALESCE(tokens_revealed,1) v FROM weekly_rounds WHERE id=?;",
                                  list(as.integer(row$round_id[1]))), error=function(e) data.frame())
    tokens_revealed <- if (nrow(rnd_row)) isTRUE(as.integer(rnd_row$v[1]) == 1L) else TRUE
    db_exec(
      "UPDATE job_assignments SET outcome=?, tokens_awarded=?, tokens_credited=?,
              updated_at=datetime('now') WHERE id=?;",
      list(outcome, tokens_to_award, if (tokens_revealed) 1L else 0L, assign_id))
    if (tokens_to_award > 0 && tokens_revealed) {
      token_credit(uid, dname, tokens_to_award, 1L, "job", assign_id,
                   note = sprintf("Job wage (%s)", outcome))
      showNotification(
        sprintf("%s — awarded %d token%s to %s.",
                switch(outcome, complete="Complete", tried="Tried", outcome),
                as.integer(tokens_to_award), if (tokens_to_award == 1) "" else "s", dname),
        type = "message")
    } else if (tokens_to_award > 0) {
      showNotification(
        sprintf("%s — outcome logged (%d tokens pending release).", dname, as.integer(tokens_to_award)),
        type = "message")
    } else {
      showNotification(sprintf("Missed — no tokens for %s.", dname), type = "warning")
    }
  }, ignoreNULL = TRUE)

  # ── Voluntary Participation logging ───────────────────────────────────────────
  .log_participation <- function(outcome_type, uid_override = NULL, post_id_override = NULL) {
    req(rv$is_admin, !rv$impersonating)
    uid     <- trimws(uid_override %||% input$part_student_sel %||% "")
    post_id <- suppressWarnings(as.integer(post_id_override %||% input$part_event_type %||% 0))
    if (!nzchar(uid) || is.na(post_id) || post_id <= 0) {
      showNotification("Select a student and event type.", type = "error"); return()
    }
    rid_row <- tryCatch(db_query("SELECT id, assignment_mode FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
    if (!nrow(rid_row)) {
      showNotification("No active round.", type = "error"); return()
    }
    rid   <- rid_row$id[1]
    u_row <- tryCatch(db_query("SELECT display_name FROM users WHERE user_id=?;", list(uid)),
                      error = function(e) data.frame())
    dname <- if (nrow(u_row)) u_row$display_name[1] %||% uid else uid
    post_row <- tryCatch(db_query(
      "SELECT jp.id, jp.category_id, jp.slots, COALESCE(jp.wage_override, jc.default_wage, 1) AS tokens
       FROM job_posts jp
       LEFT JOIN job_categories jc ON jc.id = jp.category_id
       WHERE jp.id=? AND jp.round_id=?
         AND (COALESCE(jc.voluntary,0)=1 OR COALESCE(jp.voluntary,0)=1)
       LIMIT 1;", list(post_id, rid)),
      error = function(e) data.frame())
    if (!nrow(post_row)) {
      showNotification("No voluntary post found.", type = "error")
      return()
    }
    post_id   <- as.integer(post_row$id[1])
    wage_val  <- as.numeric(post_row$tokens[1] %||% 0)
    # In wage-bidding rounds every volunteer in a category is paid the same
    # equilibrium wage from that round's bids — not their own bid. The rule is
    # either the lowest bid, or (demand-based) the k-th lowest where k is the
    # post's slots: the instructor's demand for that job over the session.
    if (identical(rid_row$assignment_mode[1] %||% "random", "wage_bidding") &&
        !is.na(post_row$category_id[1] %||% NA)) {
      cw <- volunteer_clearing_wage(rid, as.integer(post_row$category_id[1]),
                                    as.integer(post_row$slots[1] %||% 1L),
                                    query_fn = db_query)
      if (!is.na(cw)) wage_val <- cw
    }
    half_mult <- tryCatch(as.numeric(get_setting("half_wage_multiplier","0.5")),
                          error=function(e) 0.5)
    tokens_to_award <- switch(outcome_type,
      succeed = wage_val,
      try     = round(wage_val * half_mult),
      miss    = 0, 0)
    db_exec(
      "INSERT INTO live_score_events(round_id, user_id, job_post_id, event_kind,
              outcome, tokens, logged_by)
       VALUES(?,?,?,'participation',?,?,?);",
      list(rid, uid, post_id, outcome_type, tokens_to_award, rv$user_id %||% "admin"))
    showNotification(
      sprintf("%s queued for audit (%d token%s).", dname, as.integer(tokens_to_award),
              if (tokens_to_award == 1) "" else "s"),
      type = "message")
    return()
    rnd_row2 <- tryCatch(db_query("SELECT COALESCE(tokens_revealed,1) v FROM weekly_rounds WHERE id=?;",
                                   list(rid)), error=function(e) data.frame())
    tokens_revealed2 <- if (nrow(rnd_row2)) isTRUE(as.integer(rnd_row2$v[1]) == 1L) else TRUE
    db_exec(
      "INSERT INTO job_assignments(round_id, user_id, job_post_id, assigned_wage,
              assignment_mode, outcome, tokens_awarded, tokens_credited, updated_at)
       VALUES(?,?,?,?,'voluntary',?,?,?,datetime('now'))
       ON CONFLICT(round_id, user_id)
       DO UPDATE SET job_post_id=excluded.job_post_id,
                     assigned_wage=excluded.assigned_wage,
                     outcome=excluded.outcome,
                     tokens_awarded=excluded.tokens_awarded,
                     tokens_credited=excluded.tokens_credited,
                     updated_at=excluded.updated_at;",
      list(rid, uid, post_id, wage_val, outcome_type,
           tokens_to_award, if (tokens_revealed2) 1L else 0L))
    if (tokens_to_award > 0 && tokens_revealed2) {
      token_credit(uid, dname, tokens_to_award, 1L, "participation", post_id,
                   note = sprintf("Participation (%s)", outcome_type))
      showNotification(
        sprintf("%s — %s (+%d token%s)", dname, outcome_type,
                as.integer(tokens_to_award), if (tokens_to_award == 1) "" else "s"),
        type = "message")
    } else if (tokens_to_award > 0) {
      showNotification(
        sprintf("%s — %s (outcome logged, %d tokens pending release)", dname, outcome_type,
                as.integer(tokens_to_award)),
        type = "message")
    } else {
      showNotification(sprintf("%s — %s (no tokens)", dname, outcome_type), type = "warning")
    }
  }

  observeEvent(input$log_succeed_btn, .log_participation("succeed"), ignoreNULL = TRUE)
  observeEvent(input$log_try_btn,     .log_participation("try"),     ignoreNULL = TRUE)
  observeEvent(input$log_miss_btn,    .log_participation("miss"),    ignoreNULL = TRUE)
  observeEvent(input$part_card_click, {
    click <- input$part_card_click
    req(click$user_id, click$outcome, input$part_event_type)
    .log_participation(click$outcome, click$user_id, input$part_event_type)
  }, ignoreNULL = TRUE)

  # ── Release tokens (delayed reward) ──────────────────────────────────────────
  observeEvent(input$release_tokens_btn, {
    req(rv$is_admin)
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error=function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("No active round.", type="error"); return() }
    rid <- rid_row$id[1]
    pending <- tryCatch(db_query(
      "SELECT ja.id, ja.user_id, ja.tokens_awarded, ja.outcome, ja.job_post_id,
              u.display_name
       FROM job_assignments ja
       JOIN users u ON u.user_id=ja.user_id
       WHERE ja.round_id=? AND COALESCE(ja.tokens_credited,1)=0
         AND ja.tokens_awarded IS NOT NULL AND ja.tokens_awarded > 0;",
      list(rid)), error=function(e) data.frame())
    if (!nrow(pending)) {
      showNotification("No pending tokens to release.", type="message"); return()
    }
    for (i in seq_len(nrow(pending))) {
      r <- pending[i, ]
      token_credit(r$user_id, r$display_name %||% r$user_id,
                   as.numeric(r$tokens_awarded), 1L,
                   "job", as.integer(r$id),
                   note = sprintf("Job/participation (%s) — released", r$outcome %||% ""))
    }
    db_exec("UPDATE job_assignments SET tokens_credited=1 WHERE round_id=? AND COALESCE(tokens_credited,1)=0;",
            list(rid))
    db_exec("UPDATE weekly_rounds SET tokens_revealed=1 WHERE id=?;", list(rid))
    showNotification(sprintf("Released tokens for %d students.", nrow(pending)), type="message")
  }, ignoreNULL=TRUE)

  # ── Grade upload ──────────────────────────────────────────────────────────────
  observeEvent(input$upload_grades_btn, {
    req(rv$is_admin)
    fdata <- input$grade_file_upload
    if (is.null(fdata)) { showNotification("No file selected.", type="error"); return() }
    ext <- tolower(tools::file_ext(fdata$name))
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(fdata$datapath, stringsAsFactors=FALSE)
      } else if (ext %in% c("xls","xlsx")) {
        if (!requireNamespace("readxl", quietly=TRUE)) stop("readxl not available")
        readxl::read_excel(fdata$datapath) |> as.data.frame()
      } else stop("Unsupported file type")
    }, error=function(e) { showNotification(paste("Read error:", e$message), type="error"); NULL })
    if (is.null(df)) return()
    # Flexible column mapping: look for user_id/student_id, assignment, score, max_score/max, grade_pct/pct
    cn <- tolower(names(df))
    uid_col   <- names(df)[cn %in% c("user_id","student_id","userid","id")][1]
    asgn_col  <- names(df)[cn %in% c("assignment","assignment_name","name","task")][1]
    scr_col   <- names(df)[cn %in% c("score","points","earned")][1]
    max_col   <- names(df)[cn %in% c("max_score","max","total","points_possible")][1]
    pct_col   <- names(df)[cn %in% c("grade_pct","pct","percent","grade","percentage")][1]
    week_col  <- names(df)[cn %in% c("week","week_tag","period")][1]
    if (is.na(uid_col) || is.na(asgn_col)) {
      showNotification(
        "File must have columns: user_id (or student_id) and assignment (or assignment_name).",
        type="error"); return()
    }
    # Resolve week tag (optional input or file column)
    week_tag_val <- trimws(input$grade_week_tag %||% "")
    n_ins <- 0L
    for (i in seq_len(nrow(df))) {
      uid    <- as.character(df[[uid_col]][i])
      asgn   <- as.character(df[[asgn_col]][i])
      scr    <- if (!is.na(scr_col))  suppressWarnings(as.numeric(df[[scr_col]][i]))  else NA_real_
      mx     <- if (!is.na(max_col))  suppressWarnings(as.numeric(df[[max_col]][i]))  else NA_real_
      pct    <- if (!is.na(pct_col))  suppressWarnings(as.numeric(df[[pct_col]][i]))  else NA_real_
      wk     <- if (!is.na(week_col)) as.character(df[[week_col]][i]) else week_tag_val
      if (is.na(pct) && !is.na(scr) && !is.na(mx) && mx > 0) pct <- round(100 * scr / mx, 2)
      if (!nzchar(uid) || !nzchar(asgn)) next
      db_exec(
        "INSERT INTO student_grades(user_id, assignment_name, score, max_score, grade_pct, week_tag)
         VALUES(?,?,?,?,?,?);",
        list(uid, asgn, scr, mx, pct, if (nzchar(wk)) wk else NA_character_))
      n_ins <- n_ins + 1L
    }
    showNotification(sprintf("Imported %d grade rows.", n_ins), type="message")
    rv$gradebook_ver <- rv$gradebook_ver + 1L
  }, ignoreNULL=TRUE)

  observeEvent(input$clear_grades_btn, {
    req(rv$is_admin)
    db_exec("DELETE FROM student_grades;")
    showNotification("All grade records cleared.", type="message")
    rv$gradebook_ver <- rv$gradebook_ver + 1L
  }, ignoreNULL=TRUE)

  # ── Gradebook ─────────────────────────────────────────────────────────────────
  observeEvent(input$edit_gb_cat_btn, {
    req(rv$is_admin)
    ev     <- input$edit_gb_cat_btn
    cid    <- suppressWarnings(as.integer(ev$id    %||% 0))
    nm     <- trimws(ev$name   %||% "")
    weight <- suppressWarnings(as.numeric(ev$weight %||% 0))
    count  <- max(1L, suppressWarnings(as.integer(ev$count %||% 1L)))
    prefix <- trimws(ev$prefix %||% "")
    maxpts <- suppressWarnings(as.numeric(ev$max   %||% 100))
    source <- ev$source %||% "manual"
    if (is.na(cid) || cid <= 0 || !nzchar(nm)) {
      showNotification("Category name required.", type = "error"); return()
    }
    db_exec(
      "UPDATE gradebook_categories SET name=?,weight=?,item_count=?,item_prefix=?,max_points=?,source=? WHERE id=?;",
      list(nm, weight, count,
           if (nzchar(prefix)) prefix else NA_character_,
           if (!is.na(maxpts)) maxpts else 100,
           source, cid))
    rv$gradebook_ver <- rv$gradebook_ver + 1L
    showNotification(sprintf("Category '%s' updated.", nm), type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$add_gb_cat_btn, {
    req(rv$is_admin)
    nm     <- trimws(input$new_gb_name %||% "")
    weight <- suppressWarnings(as.numeric(input$new_gb_weight))
    count  <- max(1L, as.integer(input$new_gb_count %||% 1L))
    prefix <- trimws(input$new_gb_prefix %||% "")
    maxpts <- suppressWarnings(as.numeric(input$new_gb_max %||% 100))
    source <- input$new_gb_source %||% "manual"
    if (!nzchar(nm)) { showNotification("Category name required.", type = "error"); return() }
    if (is.na(weight)) { showNotification("Weight (%) required.", type = "error"); return() }
    ord <- tryCatch(
      as.integer(db_query("SELECT COALESCE(MAX(display_order),0)+1 n FROM gradebook_categories;")$n[1]),
      error = function(e) 1L)
    db_exec(
      "INSERT INTO gradebook_categories(name,weight,item_count,item_prefix,max_points,source,display_order)
       VALUES(?,?,?,?,?,?,?);",
      list(nm, weight, count,
           if (nzchar(prefix)) prefix else NA_character_,
           if (!is.na(maxpts)) maxpts else 100,
           source, ord))
    rv$gradebook_ver <- rv$gradebook_ver + 1L
    showNotification(sprintf("Category '%s' added.", nm), type = "message")
  })

  observeEvent(input$delete_gb_cat_btn, {
    req(rv$is_admin)
    cid <- suppressWarnings(as.integer(input$delete_gb_cat_btn %||% 0))
    if (is.na(cid) || cid <= 0) return()
    db_exec("DELETE FROM gradebook_item_names WHERE category_id=?;", list(cid))
    db_exec("DELETE FROM gradebook_categories WHERE id=?;", list(cid))
    rv$gradebook_ver <- rv$gradebook_ver + 1L
    showNotification("Category deleted.", type = "message")
  }, ignoreNULL = TRUE)

  observeEvent(input$rename_gb_item_btn, {
    req(rv$is_admin)
    ev  <- input$rename_gb_item_btn
    cid <- suppressWarnings(as.integer(ev$cat_id %||% 0))
    idx <- suppressWarnings(as.integer(ev$idx %||% 0))
    nm  <- trimws(ev$name %||% "")
    wt_raw <- trimws(as.character(ev$weight %||% ""))
    wt <- suppressWarnings(as.numeric(wt_raw))
    if (!nzchar(wt_raw)) wt <- NA_real_
    if (is.na(cid) || cid <= 0 || is.na(idx) || idx <= 0 || !nzchar(nm)) return()
    if (!is.na(wt) && wt < 0) {
      showNotification("Item weight must be 0 or higher.", type = "error"); return()
    }
    db_exec(
      "INSERT OR REPLACE INTO gradebook_item_names(category_id,item_index,item_name,item_weight) VALUES(?,?,?,?);",
      list(cid, idx, nm, if (!is.na(wt)) wt else NA_real_))
    rv$gradebook_ver <- rv$gradebook_ver + 1L
    showNotification("Item settings saved.", type = "message")
  }, ignoreNULL = TRUE)

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
    lbl <- if (nzchar(note)) note else sprintf("Bulk award (section: %s)", section)
    for (i in seq_len(nrow(targets))) {
      uid_i   <- targets$user_id[i]
      dname_i <- targets$display_name[i] %||% uid_i
      if (amount < 0) {
        safe_deduct(uid_i, dname_i, abs(amount), "bulk_award", lbl)
      } else {
        token_credit(uid_i, dname_i, amount, 1L, "bulk_award", note = lbl)
      }
    }
    showNotification(
      sprintf("%s up to %d token%s to %d student%s (capped at each student's balance).",
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
    lbl    <- if (nzchar(note)) note else "individual adjustment"
    if (amount < 0) {
      actual <- safe_deduct(uid, dname, abs(amount), "individual_adj", lbl)
      showNotification(
        sprintf("Deducted %d token%s from %s%s.",
                as.integer(actual), if (actual == 1) "" else "s", dname,
                if (actual < abs(amount)) sprintf(" (requested %d, capped at balance)", abs(as.integer(amount))) else ""),
        type = "message")
    } else {
      token_credit(uid, dname, amount, 1L, "individual_adj", note = lbl)
      showNotification(
        sprintf("Awarded %d token%s to %s.",
                as.integer(amount), if (amount == 1) "" else "s", dname),
        type = "message")
    }
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

  observeEvent(input$save_vol_clearing_btn, {
    req(rv$is_admin)
    rule <- input$vol_clearing_rule_sel %||% "lowest"
    if (!rule %in% c("lowest", "demand", "posted")) rule <- "lowest"
    db_exec("INSERT OR REPLACE INTO labor_settings(key,value) VALUES('volunteer_clearing_rule',?);",
            list(rule))
    rv$jobs_ver <- rv$jobs_ver + 1L
    showNotification(
      switch(rule,
        demand = "Volunteer clearing wage: k-th lowest bid (k = post slots).",
        posted = "Volunteer clearing wage: k-th lowest bid (k = demand posted per class in Live Tracker).",
        "Volunteer clearing wage: lowest bid."),
      type = "message")
  })

  observeEvent(input$post_vol_demand_btn, {
    req(rv$is_admin)
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error = function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("No active round.", type = "error"); return() }
    rid <- rid_row$id[1]
    dcats <- tryCatch(db_query(
      "SELECT DISTINCT jc.id, jc.name
       FROM job_posts jp
       JOIN job_categories jc ON jc.id=jp.category_id
       WHERE jp.round_id=? AND COALESCE(jp.active,1)=1
         AND (COALESCE(jc.voluntary,0)=1 OR COALESCE(jp.voluntary,0)=1);", list(rid)),
      error = function(e) data.frame())
    if (!nrow(dcats)) { showNotification("No volunteer categories this round.", type = "error"); return() }
    wages <- character(0)
    for (di in seq_len(nrow(dcats))) {
      cid <- as.integer(dcats$id[di])
      v   <- suppressWarnings(as.integer(input[[paste0("vd_", cid)]] %||% NA))
      if (is.na(v) || v < 1) next
      db_exec(
        "INSERT INTO volunteer_demand(round_id, category_id, demand, updated_at)
         VALUES(?,?,?,CURRENT_TIMESTAMP)
         ON CONFLICT(round_id, category_id)
         DO UPDATE SET demand=excluded.demand, updated_at=CURRENT_TIMESTAMP;",
        list(rid, cid, v))
      cw <- volunteer_clearing_wage(rid, cid, v, query_fn = db_query)
      wages <- c(wages, sprintf("%s: k=%d → %s", dcats$name[di], v,
                                if (!is.na(cw)) sprintf("%g tokens", cw) else "no bids"))
    }
    if (!length(wages)) { showNotification("Enter a demand of at least 1.", type = "warning"); return() }
    showNotification(paste("Demand posted.", paste(wages, collapse = " · ")), type = "message")
  })

  # ── Job draw ──────────────────────────────────────────────────────────────────
  # Helper: apply tiebreak ordering within groups of tied bids
  .apply_tiebreak <- function(bids_df, tiebreak) {
    if (!nrow(bids_df)) return(bids_df)
    if (tiebreak == "first_submitted") return(bids_df)  # SQL already ordered by created_at
    if (tiebreak == "random") {
      bids_df[sample(nrow(bids_df)), , drop=FALSE]
    } else if (tiebreak %in% c("lowest_grade", "lowest_tokens", "weighted_lottery", "most_misses")) {
      uids <- bids_df$user_id
      aux <- switch(tiebreak,
        lowest_grade = tryCatch(db_query(
          paste0("SELECT user_id, AVG(grade_pct) AS val FROM student_grades WHERE user_id IN (",
                 paste(sprintf("'%s'", uids), collapse=","), ") GROUP BY user_id;")),
          error=function(e) data.frame()),
        lowest_tokens = tryCatch(db_query(
          paste0("SELECT user_id, COALESCE(SUM(amount),0) AS val FROM token_ledger WHERE user_id IN (",
                 paste(sprintf("'%s'", uids), collapse=","), ") AND earning=1 GROUP BY user_id;")),
          error=function(e) data.frame()),
        weighted_lottery = tryCatch(db_query(
          paste0("SELECT user_id, COALESCE(SUM(amount),0) AS val FROM token_ledger WHERE user_id IN (",
                 paste(sprintf("'%s'", uids), collapse=","), ") AND earning=1 GROUP BY user_id;")),
          error=function(e) data.frame()),
        most_misses = tryCatch(db_query(
          paste0("SELECT user_id, COUNT(*) AS val FROM job_assignments WHERE user_id IN (",
                 paste(sprintf("'%s'", uids), collapse=","), ") AND outcome='missed' GROUP BY user_id;")),
          error=function(e) data.frame())
      )
      val_map <- if (nrow(aux)) setNames(as.numeric(aux$val), aux$user_id) else numeric(0)
      bids_df$sort_val <- sapply(uids, function(u) val_map[u] %||% 0)
      if (tiebreak == "weighted_lottery") {
        max_val <- max(bids_df$sort_val, 1)
        weights <- pmax(max_val - bids_df$sort_val + 1, 1)
        bids_df[sample(nrow(bids_df), prob=weights), , drop=FALSE]
      } else if (tiebreak == "most_misses") {
        bids_df[order(-bids_df$sort_val), , drop=FALSE]
      } else {
        bids_df[order(bids_df$sort_val, na.last=TRUE), , drop=FALSE]
      }
    } else if (tiebreak == "alphabetical") {
      uids_df <- tryCatch(db_query(
        paste0("SELECT user_id, display_name FROM users WHERE user_id IN (",
               paste(sprintf("'%s'", bids_df$user_id), collapse=","), ");")),
        error=function(e) data.frame())
      nm_map <- if (nrow(uids_df)) setNames(uids_df$display_name, uids_df$user_id) else character(0)
      bids_df$sort_name <- sapply(bids_df$user_id, function(u) nm_map[u] %||% u)
      bids_df[order(bids_df$sort_name), , drop=FALSE]
    } else {
      bids_df
    }
  }

  compute_draw_pairs <- function(rid, mode, posts, students, tiebreak = "weighted_lottery") {
    tryCatch({
      if (mode == "wage_bidding") {
        bids_raw <- db_query(
          "SELECT user_id, category_id, min_wage FROM wage_bids WHERE round_id=? ORDER BY min_wage ASC;",
          list(rid))
        # Apply tiebreak within each tied-wage group per category
        bids <- if (nrow(bids_raw) && tiebreak != "first_submitted") {
          do.call(rbind, lapply(split(bids_raw, bids_raw$min_wage), function(grp) {
            .apply_tiebreak(grp, tiebreak)
          }))
        } else bids_raw
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

  filter_posts_for_draw_timing <- function(posts, timing_filter) {
    if (!nrow(posts)) return(posts)
    timing <- tolower(trimws(as.character(posts$selection_time %||% "")))
    if (identical(timing_filter %||% "all", "all")) {
      # Full pre-class draw: leave live cold-call (during-class) posts out —
      # those are drawn one at a time in class with the During filter.
      return(posts[!(timing %in% c("during", "during class")), , drop = FALSE])
    }
    if (identical(timing_filter, "start")) {
      keep <- timing %in% c("start", "start of class")
    } else if (identical(timing_filter, "during")) {
      keep <- timing %in% c("during", "during class")
    } else {
      keep <- timing %in% c("end", "post", "post class", "after class", "end of class or after class")
    }
    posts[keep, , drop = FALSE]
  }

  observeEvent(input$run_draw_btn, {
    req(rv$is_admin)
    round <- tryCatch(db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                      error = function(e) data.frame())
    if (!nrow(round)) { showNotification("No active round.", type = "error"); return() }
    rid    <- round$id[1]
    mode   <- round$assignment_mode[1] %||% "random"
    tbrk   <- round$tiebreak_method[1] %||% "weighted_lottery"

    posts <- tryCatch(db_query(
      "SELECT jp.id, jp.job_name, jp.slots, jp.category_id,
              COALESCE(jp.wage_override, jc.default_wage) AS wage,
              COALESCE(NULLIF(jp.selection_time,''), NULLIF(jc.selection_time,''), 'any') AS selection_time
       FROM job_posts jp
       LEFT JOIN job_categories jc ON jc.id=jp.category_id
       WHERE jp.round_id=? AND COALESCE(jp.active,1)=1 AND COALESCE(jp.in_draw,1)=1;",
      list(rid)),
      error = function(e) data.frame())
    timing_filter <- input$draw_timing_filter %||% "all"
    posts <- filter_posts_for_draw_timing(posts, timing_filter)
    if (!nrow(posts)) { showNotification("No active job posts marked 'In Draw' for this round.", type = "error"); return() }
    target_rid <- rid
    target_label <- round$label[1] %||% paste("Round", rid)

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
    if (identical(timing_filter, "end")) {
      target_rid <- create_next_round_from(round)
      if (is.na(target_rid)) {
        showNotification("Could not create the next round for end-of-class jobs.", type = "error")
        return()
      }
      target_row <- tryCatch(db_query("SELECT label FROM weekly_rounds WHERE id=?;", list(target_rid)),
                             error = function(e) data.frame())
      target_label <- if (nrow(target_row)) target_row$label[1] else paste("Round", target_rid)
      posts <- clone_posts_to_round(posts, target_rid)
    }
    if (!identical(timing_filter, "all")) {
      already <- tryCatch(db_query(
        "SELECT user_id FROM job_assignments
         WHERE round_id=? AND COALESCE(status,'assigned')='assigned';",
        list(target_rid))$user_id, error = function(e) character(0))
      students <- students[!(students$user_id %in% already), , drop = FALSE]
      if (!nrow(students)) { showNotification("No unassigned students available for this timed draw.", type = "warning"); return() }
    }

    if (identical(timing_filter, "all")) {
      db_exec("DELETE FROM job_assignments WHERE round_id=?;", list(target_rid))
    }

    pairs <- compute_draw_pairs(rid, mode, posts, students, tiebreak = tbrk)
    if (!length(pairs)) { showNotification("Draw produced no assignments.", type = "error"); return() }

    for (p in pairs) {
      db_exec(
        "INSERT OR IGNORE INTO job_assignments(round_id, user_id, job_post_id, assigned_wage, assignment_mode)
         VALUES(?,?,?,?,?);",
        list(target_rid, p$uid, p$post_id,
             if (is.na(p$wage %||% NA)) NA_real_ else as.numeric(p$wage),
             mode))
    }
    db_exec("UPDATE arcade_state SET assignments_revealed=0, updated_at=CURRENT_TIMESTAMP WHERE id=1;")
    rv$draw_preview <- NULL
    msg <- if (identical(timing_filter, "end")) {
      sprintf("Drew %d end-of-class assignment%s for %s.",
              length(pairs), if (length(pairs) == 1) "" else "s", target_label)
    } else {
      sprintf("Drew %d assignments (hidden from students).", length(pairs))
    }
    showNotification(msg, type = "message")
  })

  observeEvent(input$preview_draw_btn, {
    req(rv$is_admin)
    round <- tryCatch(db_query("SELECT * FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                      error = function(e) data.frame())
    if (!nrow(round)) { showNotification("No active round.", type = "error"); return() }
    rid  <- round$id[1]
    mode <- round$assignment_mode[1] %||% "random"
    tbrk <- round$tiebreak_method[1] %||% "weighted_lottery"
    posts <- tryCatch(db_query(
      "SELECT jp.id, jp.job_name, jp.slots, jp.category_id,
              COALESCE(jp.wage_override, jc.default_wage) AS wage,
              COALESCE(NULLIF(jp.selection_time,''), NULLIF(jc.selection_time,''), 'any') AS selection_time
       FROM job_posts jp
       LEFT JOIN job_categories jc ON jc.id=jp.category_id
       WHERE jp.round_id=? AND COALESCE(jp.active,1)=1 AND COALESCE(jp.in_draw,1)=1;",
      list(rid)),
      error = function(e) data.frame())
    timing_filter2 <- input$draw_timing_filter %||% "all"
    posts <- filter_posts_for_draw_timing(posts, timing_filter2)
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
    if (!identical(timing_filter2, "all")) {
      already <- tryCatch(db_query(
        "SELECT user_id FROM job_assignments
         WHERE round_id=? AND COALESCE(status,'assigned')='assigned';",
        list(rid))$user_id, error = function(e) character(0))
      students <- students[!(students$user_id %in% already), , drop = FALSE]
      if (!nrow(students)) { showNotification("No unassigned students available for this timed draw.", type = "warning"); return() }
    }
    pairs <- compute_draw_pairs(rid, mode, posts, students, tiebreak = tbrk)
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
  observeEvent(input$toggle_section_reveal_btn, {
    req(rv$is_admin)
    sec <- trimws(rv$active_section %||% "")
    if (!nzchar(sec)) { showNotification("Pick a section first.", type = "warning"); return() }
    rid_row <- tryCatch(db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;"),
                        error=function(e) data.frame())
    if (!nrow(rid_row)) { showNotification("No active round.", type = "error"); return() }
    cur <- tryCatch(db_query(
      "SELECT COALESCE(revealed,0) v FROM assignment_reveals
       WHERE round_id=? AND section=?;", list(rid_row$id[1], sec)),
      error=function(e) data.frame())
    new_val <- if (nrow(cur) && isTRUE(as.integer(cur$v[1] %||% 0L) == 1L)) 0L else 1L
    timing <- input$section_reveal_timing %||% "start"
    db_exec(
      "INSERT INTO assignment_reveals(round_id, section, revealed, timing, updated_at)
       VALUES(?,?,?,?,CURRENT_TIMESTAMP)
       ON CONFLICT(round_id, section)
       DO UPDATE SET revealed=excluded.revealed,
                     timing=excluded.timing,
                     updated_at=CURRENT_TIMESTAMP;",
      list(rid_row$id[1], sec, new_val, timing))
    showNotification(
      sprintf("%s assignments %s (%s).", sec, if (new_val == 1L) "revealed" else "hidden",
              if (identical(timing, "post")) "post class" else "start of class"),
      type = "message")
  }, ignoreNULL = TRUE)

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
