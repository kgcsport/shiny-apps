# tests/setup/seed_test_db.R
#
# Seed all databases with the test participants and settings needed to
# run the manual test scripts in tests/manual/.
#
# Run from the repo root:
#   Rscript tests/setup/seed_test_db.R
#
# Or to target a specific DB path:
#   CONNECT_CONTENT_DIR=/srv/shiny-server/appdata Rscript tests/setup/seed_test_db.R
#
# TEST CREDENTIALS
# ─────────────────────────────────────────────────────────────────────
#  Role       │ user_id     │ password   │ section
# ────────────┼─────────────┼────────────┼────────
#  Admin      │ instructor  │ admin123   │ —
#  Student    │ alice       │ test123    │ S01
#  Student    │ bob         │ test123    │ S01
#  Student    │ carol       │ test123    │ S01
#  Student    │ dan         │ test123    │ S02
#  Student    │ eve         │ test123    │ S02
# ─────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(bcrypt)
})

# ── Path resolution (mirrors shared_db_path() + app DATA_DIR logic) ──────────

shared_db_path <- function() {
  r <- Sys.getenv("CONNECT_CONTENT_DIR", "")
  if (nzchar(r)) return(file.path(r, "data", "class-job-market.sqlite"))
  docker <- "/srv/shiny-server/appdata"
  if (dir.exists(docker)) return(file.path(docker, "data", "class-job-market.sqlite"))
  # Local dev: relative to this script → repo/apps/class-job-market/data/
  script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) ".")
  file.path(script_dir, "..", "..", "apps", "class-job-market", "data", "class-job-market.sqlite")
}

auction_db_path <- function() {
  r <- Sys.getenv("CONNECT_CONTENT_DIR", "")
  if (nzchar(r)) return(file.path(r, "data", "auction.sqlite"))
  docker <- "/srv/shiny-server/appdata"
  if (dir.exists(docker)) return(file.path(docker, "data", "auction.sqlite"))
  script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) ".")
  file.path(script_dir, "..", "..", "apps", "supply-auction-game", "data", "auction.sqlite")
}

open_db <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  con
}

exec <- function(con, sql, p = list()) {
  invisible(if (length(p)) DBI::dbExecute(con, sql, p) else DBI::dbExecute(con, sql))
}
qry <- function(con, sql, p = list()) {
  if (length(p)) DBI::dbGetQuery(con, sql, p) else DBI::dbGetQuery(con, sql)
}

h <- function(pw) bcrypt::hashpw(pw)

cat("── Seeding shared DB:", shared_db_path(), "\n")
con <- open_db(shared_db_path())

# ── 1. SHARED SCHEMA (all apps write their tables here) ──────────────────────

# users (class-job-market owns this)
exec(con, "CREATE TABLE IF NOT EXISTS users (
  user_id      TEXT PRIMARY KEY,
  display_name TEXT,
  is_admin     INTEGER DEFAULT 0
);")
for (col in c("pw_hash TEXT", "section TEXT", "active INTEGER DEFAULT 1"))
  try(exec(con, sprintf("ALTER TABLE users ADD COLUMN %s;", col)), silent = TRUE)

# students (class-job-market)
exec(con, "CREATE TABLE IF NOT EXISTS students (
  user_id      TEXT PRIMARY KEY,
  display_name TEXT,
  section      TEXT,
  active       INTEGER DEFAULT 1
);")

# token_ledger (class-job-market)
exec(con, "CREATE TABLE IF NOT EXISTS token_ledger (
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id      TEXT,
  display_name TEXT,
  round_id     INTEGER,
  source_type  TEXT,
  source_id    INTEGER,
  amount       REAL,
  earning      INTEGER DEFAULT 0,
  note         TEXT,
  created_at   TEXT DEFAULT CURRENT_TIMESTAMP
);")
exec(con, "CREATE INDEX IF NOT EXISTS ix_token_ledger_user ON token_ledger(user_id);")

# labor_settings (class-job-market)
exec(con, "CREATE TABLE IF NOT EXISTS labor_settings (
  key TEXT PRIMARY KEY, value TEXT, updated_at TEXT DEFAULT CURRENT_TIMESTAMP
);")

# weekly_rounds + job tables (class-job-market)
exec(con, "CREATE TABLE IF NOT EXISTS weekly_rounds (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  label TEXT NOT NULL, section TEXT, status TEXT DEFAULT 'open',
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
exec(con, "CREATE TABLE IF NOT EXISTS job_categories (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT, slots INTEGER DEFAULT 1, wage REAL DEFAULT 0,
  section TEXT, active INTEGER DEFAULT 1
);")
exec(con, "CREATE TABLE IF NOT EXISTS job_posts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id INTEGER, category_id INTEGER, slots INTEGER DEFAULT 1, wage REAL DEFAULT 0
);")
exec(con, "CREATE TABLE IF NOT EXISTS wage_bids (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id INTEGER, user_id TEXT, category_id INTEGER, wage REAL,
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
exec(con, "CREATE TABLE IF NOT EXISTS application_bids (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id INTEGER, user_id TEXT, category_id INTEGER, rank INTEGER,
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
exec(con, "CREATE TABLE IF NOT EXISTS job_assignments (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  round_id INTEGER, user_id TEXT, category_id INTEGER,
  wage REAL, tokens REAL DEFAULT 0, outcome TEXT, awarded_ledger_id INTEGER,
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
exec(con, "CREATE TABLE IF NOT EXISTS participation_events (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  event_type TEXT, user_id TEXT, wage REAL, tokens REAL, ledger_id INTEGER,
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
for (col in c("problem_sets", "extension_purchases", "grade_categories",
              "grade_reweight_requests", "public_goods",
              "public_good_contributions", "public_good_questions")) {
  # These are complex tables the app creates on its own; we skip them here.
}

# coordination-games tables
exec(con, "CREATE TABLE IF NOT EXISTS olig_settings (
  id INTEGER PRIMARY KEY CHECK(id=1),
  current_round INTEGER, round_status TEXT, current_game TEXT,
  bonus_multiplier REAL, pd_scale REAL,
  pd_HH_A REAL, pd_HH_B REAL, pd_HL_A REAL, pd_HL_B REAL,
  pd_LH_A REAL, pd_LH_B REAL, pd_LL_A REAL, pd_LL_B REAL,
  section TEXT DEFAULT '', use_section_size INTEGER DEFAULT 1,
  class_size INTEGER, contrib_cap REAL DEFAULT 0,
  updated_at TEXT DEFAULT (CURRENT_TIMESTAMP)
);")
exec(con, "CREATE TABLE IF NOT EXISTS olig_submissions (
  round INTEGER NOT NULL, user_id TEXT NOT NULL, display_name TEXT,
  game TEXT NOT NULL, action TEXT, contribute REAL, section TEXT DEFAULT 'default',
  created_at TEXT DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY (round, user_id)
);")
exec(con, "CREATE TABLE IF NOT EXISTS olig_payouts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  round INTEGER, user_id TEXT, game TEXT, payout REAL, meta TEXT,
  section TEXT DEFAULT 'default', created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
exec(con, "CREATE TABLE IF NOT EXISTS olig_rounds (
  round INTEGER PRIMARY KEY, game TEXT NOT NULL, status TEXT NOT NULL,
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")

# review-quiz tables
exec(con, "CREATE TABLE IF NOT EXISTS quiz_state (
  id INTEGER PRIMARY KEY CHECK(id=1),
  current_q INTEGER DEFAULT 1, revealed INTEGER DEFAULT 0,
  self_paced INTEGER DEFAULT 0, updated_at TEXT DEFAULT (CURRENT_TIMESTAMP)
);")
exec(con, "CREATE TABLE IF NOT EXISTS quiz_questions (
  q_num INTEGER PRIMARY KEY, topic TEXT NOT NULL, text TEXT NOT NULL,
  opt_a TEXT NOT NULL, opt_b TEXT NOT NULL, opt_c TEXT NOT NULL, opt_d TEXT NOT NULL,
  correct TEXT NOT NULL, explain TEXT
);")
exec(con, "CREATE TABLE IF NOT EXISTS quiz_responses (
  response_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id TEXT NOT NULL, q_num INTEGER NOT NULL, answer TEXT NOT NULL,
  submitted_at TEXT DEFAULT (CURRENT_TIMESTAMP), UNIQUE(user_id, q_num)
);")
exec(con, "CREATE TABLE IF NOT EXISTS quiz_aliases (
  user_id TEXT PRIMARY KEY, alias TEXT NOT NULL
);")
exec(con, "CREATE TABLE IF NOT EXISTS quiz_submissions (
  sub_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id TEXT NOT NULL, topic TEXT NOT NULL, text TEXT NOT NULL,
  opt_a TEXT NOT NULL, opt_b TEXT NOT NULL, opt_c TEXT NOT NULL, opt_d TEXT NOT NULL,
  correct TEXT NOT NULL, explain TEXT, status TEXT DEFAULT 'pending',
  created_at TEXT DEFAULT (CURRENT_TIMESTAMP)
);")

# price-index tables
exec(con, "CREATE TABLE IF NOT EXISTS app_state (
  id INTEGER PRIMARY KEY CHECK(id=1),
  current_wave INTEGER DEFAULT 1,
  category_list TEXT, source_list TEXT,
  updated_at TEXT DEFAULT (CURRENT_TIMESTAMP)
);")
exec(con, "CREATE TABLE IF NOT EXISTS basket_items (
  item_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id TEXT, item_name TEXT, store TEXT, category TEXT,
  times_per_month REAL DEFAULT 1,
  created_at TEXT DEFAULT (CURRENT_TIMESTAMP),
  UNIQUE(user_id, item_name, store)
);")
exec(con, "CREATE TABLE IF NOT EXISTS price_records (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  item_id INTEGER, user_id TEXT, price REAL, source TEXT, wave INTEGER,
  recorded_at TEXT DEFAULT (CURRENT_TIMESTAMP)
);")

# class-job-picker tables
exec(con, "CREATE TABLE IF NOT EXISTS job_state (
  section TEXT NOT NULL, job TEXT NOT NULL,
  cycle_id INTEGER DEFAULT 0, bag_json TEXT DEFAULT '[]',
  last_updated TEXT DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (section, job)
);")
exec(con, "CREATE TABLE IF NOT EXISTS job_log (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  logged_date TEXT, section TEXT, job TEXT, display_name TEXT,
  cycle_id INTEGER, created_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
exec(con, "CREATE TABLE IF NOT EXISTS app_settings (
  key TEXT PRIMARY KEY, value TEXT
);")

# demo-kit gallery
exec(con, "CREATE TABLE IF NOT EXISTS gallery_submissions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL, url TEXT NOT NULL, category TEXT, description TEXT,
  submitter_name TEXT, status TEXT DEFAULT 'pending',
  submitted_at TEXT DEFAULT CURRENT_TIMESTAMP
);")

# ── 2. USERS ──────────────────────────────────────────────────────────────────

cat("── Inserting users\n")

users <- list(
  list(id = "instructor", name = "Dr. Instructor", admin = 1L, pw = "admin123", section = NA),
  list(id = "alice",      name = "Alice",           admin = 0L, pw = "test123",  section = "S01"),
  list(id = "bob",        name = "Bob",             admin = 0L, pw = "test123",  section = "S01"),
  list(id = "carol",      name = "Carol",           admin = 0L, pw = "test123",  section = "S01"),
  list(id = "dan",        name = "Dan",             admin = 0L, pw = "test123",  section = "S02"),
  list(id = "eve",        name = "Eve",             admin = 0L, pw = "test123",  section = "S02")
)

for (u in users) {
  ph <- h(u$pw)
  exec(con,
    "INSERT OR REPLACE INTO users(user_id, display_name, is_admin, pw_hash, section, active)
     VALUES(?,?,?,?,?,1);",
    list(u$id, u$name, u$admin, ph, if (is.na(u$section)) NA_character_ else u$section))
  if (u$admin == 0L) {
    exec(con,
      "INSERT OR REPLACE INTO students(user_id, display_name, section, active)
       VALUES(?,?,?,1);",
      list(u$id, u$name, u$section))
  }
  cat(sprintf("   ✓  %-12s (%s)\n", u$id, if (u$admin) "admin" else u$section))
}

# ── 3. LABOR SETTINGS ────────────────────────────────────────────────────────

cat("── Inserting labor settings\n")

settings <- list(
  token_name             = "participation token",
  participation_thresholds = "5,10,15",
  initial_category_wage  = "3",
  reweight_cost_schedule = "1:2,2:5,3:9,4:14,5:20",
  grade_reweight_categories = "Homework,Midterm,Final",
  live_wages_json        = '{"useful question":1,"answer/comment":1,"strong explanation":2,"mistake diagnosis":2}'
)
for (k in names(settings)) {
  exec(con,
    "INSERT OR REPLACE INTO labor_settings(key, value) VALUES(?,?);",
    list(k, settings[[k]]))
}

# ── 4. JOB MARKET: round + categories + posts ─────────────────────────────────

cat("── Inserting job market data\n")

# Job categories (shared across sections for test)
cats <- list(
  list(name = "Note Taker",        slots = 2L, wage = 5.0),
  list(name = "Class Summarizer",  slots = 1L, wage = 4.0),
  list(name = "Question Writer",   slots = 2L, wage = 3.0)
)

# Clear any existing test categories to avoid duplicates on re-run
exec(con, "DELETE FROM job_categories WHERE name IN ('Note Taker','Class Summarizer','Question Writer');")
for (c_ in cats) {
  exec(con, "INSERT INTO job_categories(name, slots, wage, active) VALUES(?,?,?,1);",
       list(c_$name, c_$slots, c_$wage))
}
cat_ids <- qry(con, "SELECT id, name FROM job_categories WHERE name IN ('Note Taker','Class Summarizer','Question Writer');")

# One open round per section
for (sec in c("S01", "S02")) {
  exec(con, "DELETE FROM weekly_rounds WHERE label=? AND section=?;",
       list(paste("Week 1 —", sec), sec))
  exec(con, "INSERT INTO weekly_rounds(label, section, status) VALUES(?,?,'open');",
       list(paste("Week 1 —", sec), sec))
  rid <- qry(con, "SELECT last_insert_rowid() id;")$id[1]
  for (i in seq_len(nrow(cat_ids))) {
    cid  <- cat_ids$id[i]
    cslots <- cats[[i]]$slots
    cwage  <- cats[[i]]$wage
    exec(con, "INSERT INTO job_posts(round_id, category_id, slots, wage) VALUES(?,?,?,?);",
         list(rid, cid, cslots, cwage))
  }
}

round_s01 <- qry(con, "SELECT id FROM weekly_rounds WHERE section='S01' LIMIT 1;")$id[1]
round_s02 <- qry(con, "SELECT id FROM weekly_rounds WHERE section='S02' LIMIT 1;")$id[1]

# ── 5. TOKEN LEDGER: seed starting balances ────────────────────────────────────
#
# Starting balances needed by manual tests:
#   TC-07 / TC-08: Alice needs 5.0 to purchase a 3.0 extension (spends to 2.0)
#   CG-04:         Alice needs ≥ 3.0 for bonus pot contribution
#   CG-06:         Alice needs 1.0 only — run reset_test_db.R --section cg06 before this test
#   TC-09:         Alice has 5.0 earn, 3.0 spent (from TC-07) → 2.0 remaining
#
# Balances seeded here:   alice=5, bob=5, carol=3, dan=5, eve=3

cat("── Seeding token balances\n")

balances <- list(
  list(uid = "alice", name = "Alice", amount = 5.0),
  list(uid = "bob",   name = "Bob",   amount = 5.0),
  list(uid = "carol", name = "Carol", amount = 3.0),
  list(uid = "dan",   name = "Dan",   amount = 5.0),
  list(uid = "eve",   name = "Eve",   amount = 3.0)
)

# Delete any previous seed rows to allow clean re-run
exec(con, "DELETE FROM token_ledger WHERE source_type='test_seed';")

for (b in balances) {
  exec(con,
    "INSERT INTO token_ledger(user_id, display_name, source_type, amount, earning, note)
     VALUES(?,?,'test_seed',?,1,'Starting balance for test suite');",
    list(b$uid, b$name, b$amount))
  cat(sprintf("   ✓  %-8s → %.1f tokens\n", b$uid, b$amount))
}

# ── 6. COORDINATION GAMES: olig_settings singleton ────────────────────────────

cat("── Seeding olig_settings\n")

n <- qry(con, "SELECT COUNT(*) n FROM olig_settings WHERE id=1;")$n[1]
if (n == 0) {
  exec(con, "
    INSERT INTO olig_settings(
      id, current_round, round_status, current_game,
      bonus_multiplier, pd_scale,
      pd_HH_A, pd_HH_B, pd_HL_A, pd_HL_B,
      pd_LH_A, pd_LH_B, pd_LL_A, pd_LL_B,
      section, use_section_size, contrib_cap
    ) VALUES (
      1, 1, 'open', 'pd',
      1.5, 0.1,
      50, 50, 10, 70,
      70, 10, 30, 30,
      'S01', 0, 5.0
    );")
  cat("   ✓  olig_settings created (game=pd, round=1, open, section=S01)\n")
} else {
  cat("   –  olig_settings already exists (skipped)\n")
}

# ── 7. REVIEW QUIZ: questions + state ─────────────────────────────────────────

cat("── Seeding quiz questions\n")

exec(con, "DELETE FROM quiz_questions;")
exec(con, "DELETE FROM quiz_state WHERE id=1;")
exec(con, "INSERT INTO quiz_state(id, current_q, revealed, self_paced) VALUES(1, 1, 0, 0);")

questions <- list(
  list(q=1, topic="Supply & Demand",
       text="If the price of a substitute good rises, what happens to demand for the original good?",
       a="It falls", b="It rises", c="It stays the same", d="It becomes perfectly elastic",
       correct="B",
       explain="When the price of a substitute rises, consumers switch to the original good, shifting demand right."),
  list(q=2, topic="Elasticity",
       text="Which of the following goods is MOST likely to have inelastic demand?",
       a="Luxury sports cars", b="Branded clothing", c="Insulin", d="Restaurant meals",
       correct="C",
       explain="Necessities with no close substitutes, like insulin, tend to have inelastic demand."),
  list(q=3, topic="Market Structure",
       text="In a perfectly competitive market, firms are price:",
       a="makers", b="setters", c="takers", d="discriminators",
       correct="C",
       explain="Individual firms in perfect competition are too small to influence the market price."),
  list(q=4, topic="Game Theory",
       text="In a one-shot Prisoner's Dilemma, the Nash Equilibrium involves:",
       a="Both cooperate", b="Both defect", c="One cooperates, one defects", d="Random mixed strategies",
       correct="B",
       explain="Defect is a dominant strategy for both players, so (Defect, Defect) is the Nash Equilibrium."),
  list(q=5, topic="Cost Theory",
       text="At the quantity where MC = ATC, which of the following is true?",
       a="ATC is rising", b="ATC is at its minimum", c="MC is decreasing", d="Profit is maximised",
       correct="B",
       explain="MC crosses ATC at the minimum of the ATC curve.")
)

for (q in questions) {
  exec(con,
    "INSERT INTO quiz_questions(q_num, topic, text, opt_a, opt_b, opt_c, opt_d, correct, explain)
     VALUES(?,?,?,?,?,?,?,?,?);",
    list(q$q, q$topic, q$text, q$a, q$b, q$c, q$d, q$correct, q$explain))
  cat(sprintf("   ✓  Q%d: %s\n", q$q, q$topic))
}

# ── 8. PRICE INDEX: app_state ─────────────────────────────────────────────────

cat("── Seeding price-index state\n")

n <- qry(con, "SELECT COUNT(*) n FROM app_state WHERE id=1;")$n[1]
if (n == 0) {
  exec(con, "
    INSERT INTO app_state(id, current_wave, category_list, source_list)
    VALUES(1, 1,
      'Beverages,Food,Transport,Housing,Entertainment',
      'Supermarket,Online,Local market,Restaurant,Other');")
  cat("   ✓  app_state created (wave=1)\n")
} else {
  cat("   –  app_state already exists (skipped)\n")
}

# ── 9. CLASS JOB PICKER: job_state for S01 ────────────────────────────────────

cat("── Seeding job_state for class-job-picker\n")

DEFAULT_JOBS <- c("last class summary", "materials summary 1", "materials summary 2",
                  "materials summary 3", "note taker", "cold call", "voluntary answer")
S01_STUDENTS <- c("Alice", "Bob", "Carol")

for (sec in c("S01", "S02")) {
  roster <- if (sec == "S01") S01_STUDENTS else c("Dan", "Eve")
  bag_json <- paste0('["', paste(sample(roster), collapse = '","'), '"]')  # shuffled bag
  for (job in DEFAULT_JOBS) {
    exec(con,
      "INSERT OR IGNORE INTO job_state(section, job, cycle_id, bag_json) VALUES(?,?,1,?);",
      list(sec, job, bag_json))
  }
}
cat(sprintf("   ✓  job_state initialised for S01 (%d jobs) and S02 (%d jobs)\n",
            length(DEFAULT_JOBS), length(DEFAULT_JOBS)))

DBI::dbDisconnect(con)
cat("\n── Shared DB seeded:", shared_db_path(), "\n\n")

# ── 10. SUPPLY AUCTION GAME: auction.sqlite ───────────────────────────────────

cat("── Seeding auction DB:", auction_db_path(), "\n")
acon <- open_db(auction_db_path())

exec(acon, "CREATE TABLE IF NOT EXISTS auction_settings (
  id INTEGER PRIMARY KEY CHECK(id=1),
  item_name TEXT, tick_size REAL, tick_seconds INTEGER,
  start_price REAL, units_available INTEGER, max_price REAL, round INTEGER
);")
exec(acon, "CREATE TABLE IF NOT EXISTS auction_state (
  id INTEGER PRIMARY KEY CHECK(id=1),
  running INTEGER DEFAULT 0, current_price REAL, units_remaining INTEGER,
  started_at TEXT, last_tick_at TEXT, ended_at TEXT
);")
exec(acon, "CREATE TABLE IF NOT EXISTS accepts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  round INTEGER, user_id TEXT, display_name TEXT, price REAL,
  accepted_at TEXT DEFAULT CURRENT_TIMESTAMP
);")
exec(acon, "CREATE INDEX IF NOT EXISTS ix_accepts_round ON accepts(round);")

n <- qry(acon, "SELECT COUNT(*) n FROM auction_settings WHERE id=1;")$n[1]
if (n == 0) {
  exec(acon,
    "INSERT INTO auction_settings(id, item_name, tick_size, tick_seconds, start_price, units_available, max_price, round)
     VALUES (1, 'Fee-free driving pass (one unit)', 1, 3, 20, 5, 99999, 1);")
  exec(acon,
    "INSERT INTO auction_state(id, running, current_price, units_remaining)
     VALUES (1, 0, 20, 5);")
  cat("   ✓  auction_settings and auction_state initialised\n")
} else {
  cat("   –  auction_settings already exists (skipped)\n")
}

DBI::dbDisconnect(acon)
cat("── Auction DB seeded:", auction_db_path(), "\n\n")

cat("════════════════════════════════════════════════════════\n")
cat("All test databases seeded.\n\n")
cat("Login credentials:\n")
cat("  instructor / admin123  (admin)\n")
cat("  alice / test123        (S01, 5 tokens)\n")
cat("  bob   / test123        (S01, 5 tokens)\n")
cat("  carol / test123        (S01, 3 tokens)\n")
cat("  dan   / test123        (S02, 5 tokens)\n")
cat("  eve   / test123        (S02, 3 tokens)\n\n")
cat("NOTE: Before running CG-06 (contribution exceeds balance),\n")
cat("run: Rscript tests/setup/reset_test_db.R --scenario cg06\n")
cat("to set Alice's balance to 1.0 token.\n")
cat("════════════════════════════════════════════════════════\n")
