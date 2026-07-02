try(writeLines(substr(basename(getwd()), 1, 15), "/proc/self/comm"), silent = TRUE)
# app.R - class-job-market
# Classroom labor-market participation token system.

library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(jsonlite)

HAS_BCRYPT <- requireNamespace("bcrypt", quietly = TRUE)
bcrypt_check <- function(password, hash) {
  if (!isTRUE(HAS_BCRYPT)) return(FALSE)
  bcrypt::checkpw(password, hash)
}

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) && !is.na(a[1]) && nzchar(as.character(a[1]))) a else b
}

num0 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), 0, x)
}

int0 <- function(x) {
  x <- suppressWarnings(as.integer(x))
  ifelse(is.na(x), 0L, x)
}

DB_PATH <- local({
  root <- Sys.getenv("CONNECT_CONTENT_DIR", unset = file.path(dirname(normalizePath(getwd())), "flex_pass_actions"))
  file.path(root, "data", "finalqdata.sqlite")
})

conn <- NULL
get_con <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    dir.create(dirname(DB_PATH), recursive = TRUE, showWarnings = FALSE)
    conn <<- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    DBI::dbExecute(conn, "PRAGMA journal_mode = WAL;")
    DBI::dbExecute(conn, "PRAGMA busy_timeout = 5000;")
  }
  conn
}
db_exec <- function(sql, params = NULL) DBI::dbExecute(get_con(), sql, params = params)
db_query <- function(sql, params = NULL) DBI::dbGetQuery(get_con(), sql, params = params)

reg.finalizer(.GlobalEnv, function(e) {
  if (!is.null(conn) && DBI::dbIsValid(conn)) try(DBI::dbDisconnect(conn), silent = TRUE)
}, onexit = TRUE)

setting_defaults <- list(
  token_name = "participation token",
  participation_thresholds = "A:80,B:65,C:50,D:35",
  assignment_mode = "random",
  wage_clearing_rule = "highest_accepted_bid",
  half_wage_multiplier = "0.5",
  tickets_per_round = "10",
  allow_multiple_jobs_per_round = "0",
  unfilled_slot_behavior = "leave_empty",
  extension_good_enabled = "1",
  public_good_enabled = "1",
  reweight_good_enabled = "1",
  extension_prices_json = '{"24":3,"48":5}',
  extension_price_per_hour = "0",
  extension_max_hours = "48",
  extension_allow_after_solutions = "0",
  public_good_multiplier = "1.5",
  public_good_manual_threshold = "",
  reweight_cost_schedule = "1:2,2:5,3:9,4:14,5:20",
  grade_reweight_categories = "Homework,Midterm,Final",
  initial_category_wage = "3",
  live_wages_json = '{"useful question":1,"answer/comment":1,"strong explanation":2,"graph explanation":2,"mistake diagnosis":2,"other":1}'
)

init_db <- function() {
  db_exec("
    CREATE TABLE IF NOT EXISTS users (
      user_id TEXT PRIMARY KEY,
      display_name TEXT,
      is_admin INTEGER DEFAULT 0
    );
  ")
  try(db_exec("ALTER TABLE users ADD COLUMN pw_hash TEXT;"), silent = TRUE)
  try(db_exec("ALTER TABLE users ADD COLUMN section TEXT;"), silent = TRUE)
  try(db_exec("ALTER TABLE users ADD COLUMN active INTEGER DEFAULT 1;"), silent = TRUE)

  db_exec("
    CREATE TABLE IF NOT EXISTS students (
      user_id TEXT PRIMARY KEY,
      display_name TEXT,
      section TEXT,
      active INTEGER DEFAULT 1
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS labor_settings (
      key TEXT PRIMARY KEY,
      value TEXT,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS weekly_rounds (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      label TEXT NOT NULL,
      section TEXT,
      start_date TEXT,
      end_date TEXT,
      assignment_mode TEXT DEFAULT 'random',
      wage_rule TEXT DEFAULT 'highest_accepted_bid',
      tickets_per_student REAL,
      allow_multiple_jobs_per_round INTEGER DEFAULT 0,
      unfilled_slot_behavior TEXT DEFAULT 'leave_empty',
      status TEXT DEFAULT 'draft',
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS job_categories (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      default_wage REAL DEFAULT 3,
      description TEXT,
      active INTEGER DEFAULT 1,
      display_order INTEGER DEFAULT 100
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS job_posts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      round_id INTEGER,
      job_date TEXT,
      job_name TEXT NOT NULL,
      category_id INTEGER,
      description TEXT,
      slots INTEGER DEFAULT 1,
      wage_override REAL,
      active INTEGER DEFAULT 1,
      display_order INTEGER DEFAULT 100,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS job_assignments (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      round_id INTEGER,
      job_post_id INTEGER,
      user_id TEXT,
      display_name TEXT,
      assigned_wage REAL DEFAULT 0,
      wage_rule TEXT,
      assignment_mode TEXT,
      status TEXT DEFAULT 'assigned',
      outcome TEXT,
      awarded_tokens REAL DEFAULT 0,
      awarded_ledger_id INTEGER,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS wage_bids (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      round_id INTEGER,
      category_id INTEGER,
      user_id TEXT,
      min_wage REAL,
      submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(round_id, category_id, user_id)
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS application_bids (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      round_id INTEGER,
      category_id INTEGER,
      user_id TEXT,
      tickets REAL DEFAULT 0,
      submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(round_id, category_id, user_id)
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS participation_events (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      event_date TEXT,
      event_type TEXT,
      user_id TEXT,
      display_name TEXT,
      wage REAL DEFAULT 0,
      multiplier REAL DEFAULT 1,
      tokens REAL DEFAULT 0,
      ledger_id INTEGER,
      note TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS token_ledger (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id TEXT,
      display_name TEXT,
      round_id INTEGER,
      source_type TEXT,
      source_id INTEGER,
      amount REAL,
      earning INTEGER DEFAULT 0,
      note TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")
  db_exec("CREATE INDEX IF NOT EXISTS ix_token_ledger_user ON token_ledger(user_id);")

  db_exec("
    CREATE TABLE IF NOT EXISTS problem_sets (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      original_deadline TEXT,
      solutions_posted_at TEXT,
      active INTEGER DEFAULT 1
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS extension_purchases (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      problem_set_id INTEGER,
      user_id TEXT,
      hours REAL,
      cost REAL,
      ledger_id INTEGER,
      purchased_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS public_goods (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      description TEXT,
      threshold REAL,
      active INTEGER DEFAULT 1,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS public_good_contributions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      public_good_id INTEGER,
      user_id TEXT,
      amount REAL,
      ledger_id INTEGER,
      contributed_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS grade_categories (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      current_weight REAL DEFAULT 0,
      eligible INTEGER DEFAULT 1
    );
  ")

  db_exec("
    CREATE TABLE IF NOT EXISTS grade_reweight_requests (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id TEXT,
      from_category TEXT,
      to_category TEXT,
      points REAL,
      cost REAL,
      ledger_id INTEGER,
      status TEXT DEFAULT 'preview',
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  invisible(lapply(names(setting_defaults), function(k) {
    db_exec("INSERT OR IGNORE INTO labor_settings(key,value) VALUES(?,?)", list(k, setting_defaults[[k]]))
  }))

  sync_students()
  seed_jobs()
}

sync_students <- function() {
  rows <- tryCatch(db_query("
    SELECT user_id, display_name, section, COALESCE(active,1) AS active
    FROM users
    WHERE COALESCE(is_admin,0)=0;
  "), error = function(e) data.frame())
  if (!nrow(rows)) {
    rows <- data.frame(
      user_id = c("student1", "student2", "student3", "student4"),
      display_name = c("Student One", "Student Two", "Student Three", "Student Four"),
      section = "demo",
      active = 1,
      stringsAsFactors = FALSE
    )
  }
  for (i in seq_len(nrow(rows))) {
    db_exec("
      INSERT INTO students(user_id, display_name, section, active) VALUES(?,?,?,?)
      ON CONFLICT(user_id) DO UPDATE SET
        display_name=excluded.display_name, section=excluded.section, active=excluded.active;
    ", list(rows$user_id[i], rows$display_name[i], rows$section[i], int0(rows$active[i])))
  }
}

seed_jobs <- function() {
  cats <- data.frame(
    name = c("summary", "example", "question", "explanation", "diagnosis", "cold-call"),
    wage = c(3, 3, 2, 3, 3, 1),
    ord = seq(10, 60, by = 10),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(cats))) {
    db_exec("
      INSERT OR IGNORE INTO job_categories(name, default_wage, display_order)
      VALUES(?,?,?);
    ", list(cats$name[i], cats$wage[i], cats$ord[i]))
  }
  existing <- db_query("SELECT COUNT(*) n FROM job_posts;")$n[1]
  if (is.na(existing) || existing == 0) {
    rid <- ensure_current_round()
    starter <- data.frame(
      job_name = c("summary of last class", "summary of prep materials", "real-world example",
                   "question writer", "graph explainer", "mistake detective", "cold-call eligible"),
      category = c("summary", "summary", "example", "question", "explanation", "diagnosis", "cold-call"),
      description = c(
        "Briefly summarize the main ideas from the last class.",
        "Summarize the assigned preparation materials.",
        "Bring a real-world example tied to the topic.",
        "Write useful questions for class discussion.",
        "Explain a graph clearly to the class.",
        "Identify and explain a common mistake.",
        "Eligible to be called on as a normal posted job."
      ),
      slots = c(1, 1, 1, 1, 1, 1, 2),
      ord = seq(10, 70, by = 10),
      stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(starter))) {
      cid <- category_id(starter$category[i])
      db_exec("
        INSERT INTO job_posts(round_id, job_name, category_id, description, slots, display_order)
        VALUES(?,?,?,?,?,?);
      ", list(rid, starter$job_name[i], cid, starter$description[i], starter$slots[i], starter$ord[i]))
    }
  }
  invisible(TRUE)
}

get_setting <- function(key, default = "") {
  r <- db_query("SELECT value FROM labor_settings WHERE key=?", list(key))
  if (!nrow(r)) default else as.character(r$value[1] %||% default)
}

set_setting <- function(key, value) {
  db_exec("
    INSERT INTO labor_settings(key,value,updated_at) VALUES(?,?,CURRENT_TIMESTAMP)
    ON CONFLICT(key) DO UPDATE SET value=excluded.value, updated_at=CURRENT_TIMESTAMP;
  ", list(key, as.character(value)))
}

settings_list <- function() {
  r <- db_query("SELECT key,value FROM labor_settings;")
  out <- setting_defaults
  for (i in seq_len(nrow(r))) out[[r$key[i]]] <- as.character(r$value[i])
  out
}

sections <- function() {
  x <- db_query("SELECT DISTINCT section FROM students WHERE COALESCE(active,1)=1 AND section IS NOT NULL AND section!='' ORDER BY section;")$section
  if (!length(x)) "demo" else x
}

roster <- function(section = NULL) {
  if (is.null(section) || !nzchar(section)) {
    db_query("SELECT user_id, display_name, section FROM students WHERE COALESCE(active,1)=1 ORDER BY display_name;")
  } else {
    db_query("SELECT user_id, display_name, section FROM students WHERE section=? AND COALESCE(active,1)=1 ORDER BY display_name;", list(section))
  }
}

category_id <- function(name) {
  r <- db_query("SELECT id FROM job_categories WHERE name=?", list(name))
  if (!nrow(r)) return(NA_integer_)
  as.integer(r$id[1])
}

ensure_current_round <- function() {
  r <- db_query("SELECT id FROM weekly_rounds ORDER BY id DESC LIMIT 1;")
  if (nrow(r)) return(as.integer(r$id[1]))
  mode <- get_setting("assignment_mode", "random")
  db_exec("
    INSERT INTO weekly_rounds(label, section, start_date, end_date, assignment_mode, wage_rule, tickets_per_student,
                              allow_multiple_jobs_per_round, unfilled_slot_behavior)
    VALUES(?,?,?,?,?,?,?,?,?);
  ", list(
    paste("Week of", Sys.Date()), sections()[1], as.character(Sys.Date()), as.character(Sys.Date() + 6),
    mode, get_setting("wage_clearing_rule", "highest_accepted_bid"),
    num0(get_setting("tickets_per_round", 10)),
    int0(get_setting("allow_multiple_jobs_per_round", 0)),
    get_setting("unfilled_slot_behavior", "leave_empty")
  ))
  DBI::dbGetQuery(get_con(), "SELECT last_insert_rowid() id;")$id[1]
}

rounds_df <- function() {
  db_query("SELECT * FROM weekly_rounds ORDER BY id DESC;")
}

active_round_id <- function(input_round) {
  rid <- int0(input_round %||% 0)
  if (rid <= 0) ensure_current_round() else rid
}

jobs_for_round <- function(round_id, active_only = TRUE) {
  sql <- "
    SELECT jp.*, jc.name AS category, jc.default_wage,
           COALESCE(jp.wage_override, jc.default_wage, 0) AS wage
    FROM job_posts jp
    LEFT JOIN job_categories jc ON jc.id=jp.category_id
    WHERE jp.round_id=?
  "
  if (isTRUE(active_only)) sql <- paste(sql, "AND COALESCE(jp.active,1)=1")
  db_query(paste(sql, "ORDER BY jp.display_order, jp.id;"), list(round_id))
}

expanded_slots <- function(jobs) {
  if (!nrow(jobs)) return(jobs[0, ])
  rows <- lapply(seq_len(nrow(jobs)), function(i) {
    n <- max(1L, int0(jobs$slots[i]))
    jobs[rep(i, n), , drop = FALSE]
  })
  out <- bind_rows(rows)
  out$slot_n <- ave(seq_len(nrow(out)), out$id, FUN = seq_along)
  out
}

ledger_add <- function(user_id, display_name, amount, earning, source_type, source_id = NA, round_id = NA, note = "") {
  db_exec("
    INSERT INTO token_ledger(user_id, display_name, round_id, source_type, source_id, amount, earning, note)
    VALUES(?,?,?,?,?,?,?,?);
  ", list(user_id, display_name, ifelse(is.na(round_id), NA, round_id), source_type,
          ifelse(is.na(source_id), NA, source_id), num0(amount), int0(earning), note))
  DBI::dbGetQuery(get_con(), "SELECT last_insert_rowid() id;")$id[1]
}

balances <- function() {
  db_query("
    SELECT s.user_id, s.display_name, s.section,
           COALESCE(SUM(CASE WHEN tl.earning=1 AND tl.amount>0 THEN tl.amount ELSE 0 END),0) AS lifetime_earned,
           COALESCE(SUM(tl.amount),0) AS spendable_balance
    FROM students s
    LEFT JOIN token_ledger tl ON tl.user_id=s.user_id
    WHERE COALESCE(s.active,1)=1
    GROUP BY s.user_id, s.display_name, s.section
    ORDER BY s.section, s.display_name;
  ")
}

student_balance <- function(user_id) {
  r <- db_query("
    SELECT COALESCE(SUM(CASE WHEN earning=1 AND amount>0 THEN amount ELSE 0 END),0) AS lifetime_earned,
           COALESCE(SUM(amount),0) AS spendable_balance
    FROM token_ledger WHERE user_id=?;
  ", list(user_id))
  if (!nrow(r)) data.frame(lifetime_earned = 0, spendable_balance = 0) else r
}

spend_tokens <- function(user_id, amount, source_type, source_id = NA, round_id = NA, note = "") {
  amount <- num0(amount)
  bal <- student_balance(user_id)$spendable_balance[1]
  if (amount <= 0) stop("Amount must be positive.")
  if (bal < amount) stop("Insufficient spendable balance.")
  s <- db_query("SELECT display_name FROM students WHERE user_id=?", list(user_id))
  nm <- if (nrow(s)) s$display_name[1] else user_id
  ledger_add(user_id, nm, -amount, 0, source_type, source_id, round_id, note)
}

clear_wage_market <- function(round_id, rule = "highest_accepted_bid", override_wage = NA) {
  jobs <- jobs_for_round(round_id)
  cats <- jobs %>% group_by(category_id, category) %>% summarise(slots = sum(slots), fallback_wage = max(wage), .groups = "drop")
  bids <- db_query("
    SELECT wb.*, s.display_name, jc.name AS category
    FROM wage_bids wb
    JOIN students s ON s.user_id=wb.user_id
    JOIN job_categories jc ON jc.id=wb.category_id
    WHERE wb.round_id=?
    ORDER BY wb.category_id, wb.min_wage, s.display_name;
  ", list(round_id))
  out <- list()
  for (i in seq_len(nrow(cats))) {
    cat_bids <- bids[bids$category_id == cats$category_id[i], , drop = FALSE]
    cat_bids <- cat_bids[order(cat_bids$min_wage, cat_bids$submitted_at), , drop = FALSE]
    slots <- int0(cats$slots[i])
    accepted <- head(cat_bids, slots)
    rejected <- if (nrow(cat_bids) > slots) cat_bids[(slots + 1):nrow(cat_bids), , drop = FALSE] else cat_bids[0, ]
    common <- switch(rule,
      pay_as_bid = NA_real_,
      highest_accepted_bid = if (nrow(accepted)) max(num0(accepted$min_wage)) else num0(cats$fallback_wage[i]),
      lowest_rejected_bid = if (nrow(rejected)) min(num0(rejected$min_wage)) else if (nrow(accepted)) max(num0(accepted$min_wage)) else num0(cats$fallback_wage[i]),
      median_bid = if (nrow(cat_bids)) median(num0(cat_bids$min_wage)) else num0(cats$fallback_wage[i]),
      instructor_override = num0(override_wage),
      num0(cats$fallback_wage[i])
    )
    if (nrow(accepted)) {
      accepted$accepted <- TRUE
      accepted$clearing_wage <- if (rule == "pay_as_bid") accepted$min_wage else common
      accepted$slots_available <- slots
      out[[length(out) + 1]] <- accepted
    }
    if (nrow(rejected)) {
      rejected$accepted <- FALSE
      rejected$clearing_wage <- if (rule == "pay_as_bid") rejected$min_wage else common
      rejected$slots_available <- slots
      out[[length(out) + 1]] <- rejected
    }
  }
  if (!length(out)) data.frame() else bind_rows(out)
}

assignment_from_wage_preview <- function(round_id, preview) {
  jobs <- expanded_slots(jobs_for_round(round_id))
  if (!all(c("category_id", "accepted", "user_id", "display_name", "clearing_wage") %in% names(preview))) {
    preview <- data.frame(
      category_id = integer(),
      accepted = logical(),
      user_id = character(),
      display_name = character(),
      clearing_wage = numeric(),
      stringsAsFactors = FALSE
    )
  }
  r <- db_query("SELECT * FROM weekly_rounds WHERE id=?", list(round_id))
  sec <- if (nrow(r)) r$section[1] else sections()[1]
  allow_multi <- if (nrow(r)) int0(r$allow_multiple_jobs_per_round[1]) == 1L else FALSE
  unfilled <- if (nrow(r)) r$unfilled_slot_behavior[1] else "leave_empty"
  students <- roster(sec)
  assigned <- list()
  used <- character(0)
  for (i in seq_len(nrow(jobs))) {
    pool <- preview[preview$category_id == jobs$category_id[i] & preview$accepted, , drop = FALSE]
    if (!allow_multi) pool <- pool[!(pool$user_id %in% used), , drop = FALSE]
    if (!nrow(pool) && unfilled == "random_fill") {
      pool2 <- students
      if (!allow_multi) pool2 <- pool2[!(pool2$user_id %in% used), , drop = FALSE]
      if (!nrow(pool2)) next
      pick <- pool2[sample(seq_len(nrow(pool2)), 1), ]
      wage <- num0(jobs$wage[i])
    } else if (!nrow(pool)) {
      next
    } else {
      pick <- pool[1, ]
      wage <- num0(pick$clearing_wage)
    }
    used <- c(used, pick$user_id)
    assigned[[length(assigned) + 1]] <- data.frame(
      round_id = round_id,
      job_post_id = jobs$id[i],
      user_id = pick$user_id,
      display_name = pick$display_name,
      assigned_wage = wage,
      wage_rule = get_setting("wage_clearing_rule", "highest_accepted_bid"),
      assignment_mode = "wage_bidding",
      stringsAsFactors = FALSE
    )
    preview <- preview[!(preview$user_id == pick$user_id & preview$category_id == jobs$category_id[i]), , drop = FALSE]
  }
  if (!length(assigned)) data.frame() else bind_rows(assigned)
}

random_assignments <- function(round_id) {
  r <- db_query("SELECT * FROM weekly_rounds WHERE id=?", list(round_id))
  sec <- if (nrow(r)) r$section[1] else sections()[1]
  allow_multi <- if (nrow(r)) int0(r$allow_multiple_jobs_per_round[1]) == 1L else FALSE
  jobs <- expanded_slots(jobs_for_round(round_id))
  students <- roster(sec)
  if (!nrow(students) || !nrow(jobs)) return(data.frame())
  out <- list(); used <- character(0)
  for (i in seq_len(nrow(jobs))) {
    pool <- students
    if (!allow_multi) pool <- pool[!(pool$user_id %in% used), , drop = FALSE]
    if (!nrow(pool)) break
    pick <- pool[sample(seq_len(nrow(pool)), 1), ]
    used <- c(used, pick$user_id)
    out[[length(out) + 1]] <- data.frame(
      round_id = round_id, job_post_id = jobs$id[i], user_id = pick$user_id,
      display_name = pick$display_name, assigned_wage = num0(jobs$wage[i]),
      wage_rule = "posted", assignment_mode = "random", stringsAsFactors = FALSE
    )
  }
  if (!length(out)) data.frame() else bind_rows(out)
}

application_assignments <- function(round_id) {
  r <- db_query("SELECT * FROM weekly_rounds WHERE id=?", list(round_id))
  sec <- if (nrow(r)) r$section[1] else sections()[1]
  allow_multi <- if (nrow(r)) int0(r$allow_multiple_jobs_per_round[1]) == 1L else FALSE
  unfilled <- if (nrow(r)) r$unfilled_slot_behavior[1] else "leave_empty"
  jobs <- expanded_slots(jobs_for_round(round_id))
  bids <- db_query("
    SELECT ab.*, s.display_name
    FROM application_bids ab JOIN students s ON s.user_id=ab.user_id
    WHERE ab.round_id=? AND ab.tickets>0;
  ", list(round_id))
  students <- roster(sec)
  used <- character(0); out <- list()
  for (i in seq_len(nrow(jobs))) {
    pool <- bids[bids$category_id == jobs$category_id[i], , drop = FALSE]
    if (!allow_multi) pool <- pool[!(pool$user_id %in% used), , drop = FALSE]
    if (!nrow(pool) && unfilled == "random_fill") {
      pool2 <- students
      if (!allow_multi) pool2 <- pool2[!(pool2$user_id %in% used), , drop = FALSE]
      if (!nrow(pool2)) next
      pick <- pool2[sample(seq_len(nrow(pool2)), 1), ]
      tickets <- 0
    } else if (!nrow(pool)) {
      next
    } else {
      pick <- pool[sample(seq_len(nrow(pool)), 1, prob = num0(pool$tickets)), ]
      tickets <- pick$tickets[1]
      bids <- bids[!(bids$user_id == pick$user_id & bids$category_id == jobs$category_id[i]), , drop = FALSE]
    }
    used <- c(used, pick$user_id)
    out[[length(out) + 1]] <- data.frame(
      round_id = round_id, job_post_id = jobs$id[i], user_id = pick$user_id,
      display_name = pick$display_name, assigned_wage = num0(jobs$wage[i]),
      wage_rule = paste("tickets", tickets), assignment_mode = "application_bidding",
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) data.frame() else bind_rows(out)
}

commit_assignments <- function(df) {
  if (!nrow(df)) return(0L)
  db_exec("DELETE FROM job_assignments WHERE round_id=? AND status='assigned' AND awarded_ledger_id IS NULL;", list(df$round_id[1]))
  for (i in seq_len(nrow(df))) {
    db_exec("
      INSERT INTO job_assignments(round_id, job_post_id, user_id, display_name, assigned_wage, wage_rule, assignment_mode)
      VALUES(?,?,?,?,?,?,?);
    ", list(df$round_id[i], df$job_post_id[i], df$user_id[i], df$display_name[i], df$assigned_wage[i], df$wage_rule[i], df$assignment_mode[i]))
  }
  nrow(df)
}

assignments_df <- function(round_id = NULL) {
  params <- NULL
  where <- ""
  if (!is.null(round_id) && int0(round_id) > 0) {
    where <- "WHERE ja.round_id=?"
    params <- list(int0(round_id))
  }
  db_query(paste("
    SELECT ja.*, wr.label AS round_label, jp.job_name, jc.name AS category
    FROM job_assignments ja
    LEFT JOIN weekly_rounds wr ON wr.id=ja.round_id
    LEFT JOIN job_posts jp ON jp.id=ja.job_post_id
    LEFT JOIN job_categories jc ON jc.id=jp.category_id
  ", where, "ORDER BY ja.id DESC;"), params)
}

award_assignment <- function(assignment_id, outcome) {
  a <- db_query("SELECT * FROM job_assignments WHERE id=?", list(assignment_id))
  if (!nrow(a)) stop("Assignment not found.")
  if (!is.na(a$awarded_ledger_id[1])) stop("Assignment already awarded.")
  mult <- switch(outcome,
    Complete = 1,
    Tried = num0(get_setting("half_wage_multiplier", 0.5)),
    Missed = 0,
    0
  )
  tokens <- num0(a$assigned_wage[1]) * mult
  lid <- if (tokens > 0) ledger_add(a$user_id[1], a$display_name[1], tokens, 1, "job_assignment", assignment_id, a$round_id[1], outcome) else NA
  db_exec("
    UPDATE job_assignments
    SET outcome=?, awarded_tokens=?, awarded_ledger_id=?, status='closed', updated_at=CURRENT_TIMESTAMP
    WHERE id=?;
  ", list(outcome, tokens, ifelse(is.na(lid), NA, lid), assignment_id))
  tokens
}

parse_prices <- function() {
  x <- tryCatch(jsonlite::fromJSON(get_setting("extension_prices_json", "{}")), error = function(e) list())
  if (!length(x)) return(data.frame(hours = numeric(), price = numeric()))
  data.frame(hours = as.numeric(names(x)), price = as.numeric(unlist(x)), row.names = NULL)
}

extension_cost <- function(hours) {
  pph <- num0(get_setting("extension_price_per_hour", 0))
  if (pph > 0) return(num0(hours) * pph)
  prices <- parse_prices()
  hit <- prices[prices$hours == num0(hours), , drop = FALSE]
  if (nrow(hit)) hit$price[1] else NA_real_
}

can_buy_extension <- function(ps, hours) {
  if (!nrow(ps)) return("Problem set not found.")
  if (num0(hours) > num0(get_setting("extension_max_hours", 48))) return("Requested hours exceed the configured maximum.")
  now <- Sys.time()
  parse_time <- function(x) {
    x <- as.character(x %||% "")
    if (!nzchar(x)) return(as.POSIXct(NA))
    suppressWarnings(as.POSIXct(x))
  }
  deadline <- parse_time(ps$original_deadline[1])
  sol <- parse_time(ps$solutions_posted_at[1])
  if (!is.na(deadline) && now > deadline) return("Purchases are closed after the original deadline.")
  if (num0(get_setting("extension_allow_after_solutions", 0)) != 1 && !is.na(sol) && now > sol) return("Solutions have posted.")
  TRUE
}

public_good_threshold <- function(pg_id) {
  pg <- db_query("SELECT * FROM public_goods WHERE id=?", list(pg_id))
  if (!nrow(pg)) return(0)
  if (!is.na(num0(pg$threshold[1])) && num0(pg$threshold[1]) > 0) return(num0(pg$threshold[1]))
  manual <- get_setting("public_good_manual_threshold", "")
  if (nzchar(manual)) return(num0(manual))
  enrolled <- db_query("SELECT COUNT(*) n FROM students WHERE COALESCE(active,1)=1;")$n[1]
  enrolled * num0(get_setting("public_good_multiplier", 1.5))
}

reweight_cost <- function(points) {
  sched <- strsplit(get_setting("reweight_cost_schedule", "1:2,2:5,3:9"), ",")[[1]]
  tbl <- data.frame(points = numeric(), cost = numeric())
  for (s in sched) {
    bits <- strsplit(s, ":", fixed = TRUE)[[1]]
    if (length(bits) == 2) tbl <- rbind(tbl, data.frame(points = num0(bits[1]), cost = num0(bits[2])))
  }
  if (!nrow(tbl)) return(num0(points) * 3)
  tbl <- tbl[order(tbl$points), ]
  hit <- tbl[tbl$points >= num0(points), , drop = FALSE]
  if (nrow(hit)) hit$cost[1] else max(tbl$cost) + (num0(points) - max(tbl$points)) * 5
}

eligible_grade_categories <- function() {
  cats <- db_query("
    SELECT * FROM grade_categories
    WHERE COALESCE(eligible,1)=1 AND lower(name) <> 'participation'
    ORDER BY name;
  ")
  if (!nrow(cats)) {
    names <- trimws(strsplit(get_setting("grade_reweight_categories", "Homework,Midterm,Final"), ",")[[1]])
    names <- names[nzchar(names) & tolower(names) != "participation"]
    if (!length(names)) names <- c("Homework", "Midterm", "Final")
    cats <- data.frame(
      name = names,
      current_weight = rep(100 / length(names), length(names)),
      eligible = 1,
      stringsAsFactors = FALSE
    )
  }
  cats
}

validate_reweight <- function(from, to, points) {
  points <- num0(points)
  cats <- eligible_grade_categories()
  if (!nzchar(from %||% "") || !nzchar(to %||% "")) return("Choose both categories.")
  if (identical(from, to)) return("Choose two different categories.")
  if (tolower(from) == "participation" || tolower(to) == "participation") return("Participation cannot be reweighted.")
  if (!(from %in% cats$name) || !(to %in% cats$name)) return("Choose eligible non-participation categories.")
  if (points <= 0) return("Percentage points must be positive.")
  from_weight <- num0(cats$current_weight[cats$name == from][1])
  if (from_weight - points < 0) return("That shift would make a category weight negative.")
  TRUE
}

reweight_preview <- function(from, to, points) {
  ok <- validate_reweight(from, to, points)
  if (!isTRUE(ok)) stop(ok)
  cats <- eligible_grade_categories()
  old_sum <- sum(num0(cats$current_weight))
  cats$new_weight <- cats$current_weight
  cats$new_weight[cats$name == from] <- cats$new_weight[cats$name == from] - num0(points)
  cats$new_weight[cats$name == to] <- cats$new_weight[cats$name == to] + num0(points)
  if (any(num0(cats$new_weight) < 0)) stop("That shift would make a category weight negative.")
  if (abs(sum(num0(cats$new_weight)) - old_sum) > 1e-8) stop("Preview weights do not sum correctly.")
  list(cost = reweight_cost(points), weights = cats)
}

csv_download <- function(table_name, filename) {
  downloadHandler(
    filename = function() filename,
    content = function(file) {
      write.csv(db_query(sprintf("SELECT * FROM %s;", table_name)), file, row.names = FALSE)
    }
  )
}

init_db()

CSS <- "
body { font-size: 15px; }
.btn-primary { background-color:#951829; border-color:#7a1221; }
.btn-success { background-color:#2d6a4f; border-color:#245c43; }
.panel { background:#fff; border:1px solid #ddd; border-radius:8px; padding:12px; margin:10px 0; }
.metric { border:1px solid #ddd; border-radius:8px; padding:10px; min-height:74px; }
.metric .value { font-size:24px; font-weight:700; }
.muted { color:#666; }
.wide-control .form-group { margin-bottom:8px; }
"

login_ui <- fluidPage(
  tags$head(tags$style(HTML(CSS))),
  titlePanel("Class Job Market"),
  wellPanel(
    textInput("login_user", "Username (students only; leave blank for instructor password)"),
    passwordInput("login_pw", "Password"),
    actionButton("login_btn", "Sign in", class = "btn-primary")
  )
)

app_ui <- fluidPage(
  tags$head(tags$style(HTML(CSS))),
  titlePanel("Class Job Market"),
  uiOutput("role_banner"),
  uiOutput("main_tabs")
)

server <- function(input, output, session) {
  SHINY_PASSWORD <- Sys.getenv("SHINY_PASSWORD", "")
  authed_admin <- reactiveVal(!nzchar(SHINY_PASSWORD))
  student_user <- reactiveVal(NULL)
  preview_assignments <- reactiveVal(data.frame())
  refresh_key <- reactiveVal(Sys.time())

  touch <- function() refresh_key(Sys.time())

  output$main_ui <- renderUI({
    if (authed_admin() || !is.null(student_user())) app_ui else login_ui
  })

  observeEvent(input$login_btn, {
    uname <- trimws(input$login_user %||% "")
    pw <- input$login_pw %||% ""
    if (!nzchar(uname)) {
      if (!nzchar(SHINY_PASSWORD) || identical(pw, SHINY_PASSWORD)) {
        authed_admin(TRUE)
      } else {
        showNotification("Incorrect password.", type = "error")
      }
      return()
    }
    row <- tryCatch(db_query("SELECT user_id, display_name, pw_hash, COALESCE(is_admin,0) is_admin, COALESCE(active,1) active FROM users WHERE user_id=?", list(uname)), error = function(e) data.frame())
    if (!nrow(row)) {
      showNotification("Username not found.", type = "error")
    } else if (int0(row$active[1]) == 0) {
      showNotification("This account is archived.", type = "error")
    } else if (!nzchar(row$pw_hash[1] %||% "")) {
      showNotification("No password is set for this account.", type = "error")
    } else if (bcrypt_check(pw, row$pw_hash[1])) {
      if (int0(row$is_admin[1]) == 1L) authed_admin(TRUE) else student_user(row$user_id[1])
    } else {
      showNotification("Incorrect password.", type = "error")
    }
  })

  round_choices <- reactive({
    refresh_key()
    r <- rounds_df()
    setNames(r$id, paste0("#", r$id, " - ", r$label, " (", r$assignment_mode, ")"))
  })

  output$role_banner <- renderUI({
    if (authed_admin()) {
      div(class = "muted", "Instructor view")
    } else {
      uid <- student_user()
      nm <- db_query("SELECT display_name FROM students WHERE user_id=?", list(uid))$display_name[1] %||% uid
      div(class = "muted", paste("Student view:", nm), actionLink("logout", "Log out"))
    }
  })

  observeEvent(input$logout, {
    student_user(NULL)
    authed_admin(!nzchar(SHINY_PASSWORD))
  })

  output$main_tabs <- renderUI({
    if (authed_admin()) {
      tabsetPanel(
        tabPanel("Dashboard", uiOutput("dashboard_ui")),
        tabPanel("Job setup", uiOutput("job_setup_ui")),
        tabPanel("Assignment runner", uiOutput("runner_ui")),
        tabPanel("Wage bids", uiOutput("wage_admin_ui")),
        tabPanel("Application bids", uiOutput("app_admin_ui")),
        tabPanel("Live tracker", uiOutput("live_ui")),
        tabPanel("Job completion", uiOutput("completion_ui")),
        tabPanel("Token ledger", DTOutput("ledger_table")),
        tabPanel("Public goods", uiOutput("public_goods_ui")),
        tabPanel("Extensions", uiOutput("extensions_ui")),
        tabPanel("Grade reweighting", uiOutput("reweight_admin_ui")),
        tabPanel("Settings", uiOutput("settings_ui")),
        tabPanel("CSV exports", uiOutput("exports_ui"))
      )
    } else {
      tabsetPanel(
        tabPanel("My jobs", uiOutput("student_jobs_ui")),
        tabPanel("Wage bids", uiOutput("student_wage_ui")),
        tabPanel("Application bids", uiOutput("student_app_ui")),
        tabPanel("My tokens", uiOutput("student_tokens_ui")),
        tabPanel("Spend tokens", uiOutput("student_spend_ui")),
        tabPanel("Public good status", uiOutput("student_public_ui")),
        tabPanel("Reweighting preview", uiOutput("student_reweight_ui"))
      )
    }
  })

  output$dashboard_ui <- renderUI({
    refresh_key()
    b <- balances()
    r <- rounds_df()
    fluidRow(
      column(3, div(class = "metric", "Active students", div(class = "value", nrow(b)))),
      column(3, div(class = "metric", "Lifetime earned", div(class = "value", round(sum(b$lifetime_earned), 1)))),
      column(3, div(class = "metric", "Spendable", div(class = "value", round(sum(b$spendable_balance), 1)))),
      column(3, div(class = "metric", "Rounds", div(class = "value", nrow(r))))
    ) %>% tagList(
      h4("Balances"),
      DTOutput("balances_table")
    )
  })

  output$balances_table <- renderDT({
    refresh_key()
    datatable(balances(), rownames = FALSE, options = list(pageLength = 25))
  })

  output$job_setup_ui <- renderUI({
    refresh_key()
    cats <- db_query("SELECT * FROM job_categories ORDER BY display_order, name;")
    rounds <- round_choices()
    tagList(
      fluidRow(
        column(4, div(class = "panel",
          h4("Create weekly round"),
          textInput("round_label", "Label", value = paste("Week of", Sys.Date())),
          selectInput("round_section", "Section", choices = sections()),
          dateInput("round_start", "Start date", value = Sys.Date()),
          dateInput("round_end", "End date", value = Sys.Date() + 6),
          selectInput("round_mode", "Assignment mode", c("random", "wage_bidding", "application_bidding"), selected = get_setting("assignment_mode", "random")),
          actionButton("create_round", "Create round", class = "btn-primary")
        )),
        column(4, div(class = "panel",
          h4("Add/edit category"),
          selectInput("category_edit_id", "Existing category", choices = c("New" = "", setNames(cats$id, cats$name))),
          textInput("category_name", "Name"),
          numericInput("category_wage", "Default wage", value = num0(get_setting("initial_category_wage", 3)), min = 0, step = 0.5),
          textAreaInput("category_desc", "Description", rows = 2),
          numericInput("category_order", "Display order", value = 100, step = 1),
          checkboxInput("category_active", "Active", TRUE),
          actionButton("save_category", "Save category", class = "btn-primary")
        )),
        column(4, div(class = "panel",
          h4("Add job post"),
          selectInput("job_round", "Round", choices = rounds),
          dateInput("job_date", "Date if relevant", value = Sys.Date()),
          textInput("job_name", "Job name"),
          selectInput("job_category", "Category", choices = setNames(cats$id, cats$name)),
          textAreaInput("job_desc", "Description", rows = 2),
          numericInput("job_slots", "Slots", value = 1, min = 1, step = 1),
          numericInput("job_wage_override", "Override wage (blank uses category)", value = NA, min = 0, step = 0.5),
          numericInput("job_order", "Display order", value = 100, step = 1),
          checkboxInput("job_active", "Active", TRUE),
          actionButton("save_job", "Save job", class = "btn-success")
        ))
      ),
      h4("Current jobs"),
      DTOutput("jobs_table")
    )
  })

  observeEvent(input$category_edit_id, {
    id <- int0(input$category_edit_id)
    if (id <= 0) return()
    r <- db_query("SELECT * FROM job_categories WHERE id=?", list(id))
    if (nrow(r)) {
      updateTextInput(session, "category_name", value = r$name[1])
      updateNumericInput(session, "category_wage", value = r$default_wage[1])
      updateTextAreaInput(session, "category_desc", value = r$description[1] %||% "")
      updateNumericInput(session, "category_order", value = r$display_order[1])
      updateCheckboxInput(session, "category_active", value = int0(r$active[1]) == 1L)
    }
  })

  observeEvent(input$create_round, {
    db_exec("
      INSERT INTO weekly_rounds(label, section, start_date, end_date, assignment_mode, wage_rule, tickets_per_student,
                                allow_multiple_jobs_per_round, unfilled_slot_behavior)
      VALUES(?,?,?,?,?,?,?,?,?);
    ", list(input$round_label, input$round_section, as.character(input$round_start), as.character(input$round_end),
            input$round_mode, get_setting("wage_clearing_rule", "highest_accepted_bid"),
            num0(get_setting("tickets_per_round", 10)), int0(get_setting("allow_multiple_jobs_per_round", 0)),
            get_setting("unfilled_slot_behavior", "leave_empty")))
    showNotification("Round created.", type = "message")
    touch()
  })

  observeEvent(input$save_category, {
    req(nzchar(input$category_name))
    id <- int0(input$category_edit_id)
    if (id > 0) {
      db_exec("UPDATE job_categories SET name=?, default_wage=?, description=?, active=?, display_order=? WHERE id=?;",
              list(input$category_name, num0(input$category_wage), input$category_desc, int0(input$category_active), int0(input$category_order), id))
    } else {
      db_exec("INSERT INTO job_categories(name, default_wage, description, active, display_order) VALUES(?,?,?,?,?);",
              list(input$category_name, num0(input$category_wage), input$category_desc, int0(input$category_active), int0(input$category_order)))
    }
    showNotification("Category saved.", type = "message")
    touch()
  })

  observeEvent(input$save_job, {
    req(nzchar(input$job_name), input$job_round, input$job_category)
    wage_override <- suppressWarnings(as.numeric(input$job_wage_override))
    if (is.na(wage_override)) wage_override <- NA
    db_exec("
      INSERT INTO job_posts(round_id, job_date, job_name, category_id, description, slots, wage_override, active, display_order)
      VALUES(?,?,?,?,?,?,?,?,?);
    ", list(int0(input$job_round), as.character(input$job_date), input$job_name, int0(input$job_category), input$job_desc,
            int0(input$job_slots), wage_override, int0(input$job_active), int0(input$job_order)))
    showNotification("Job saved.", type = "message")
    touch()
  })

  output$jobs_table <- renderDT({
    refresh_key()
    datatable(db_query("
      SELECT wr.label AS round, jp.id, jp.job_date, jp.job_name, jc.name AS category, jp.description,
             jp.slots, jc.default_wage, jp.wage_override, COALESCE(jp.wage_override,jc.default_wage) AS wage,
             jp.active, jp.display_order
      FROM job_posts jp
      LEFT JOIN weekly_rounds wr ON wr.id=jp.round_id
      LEFT JOIN job_categories jc ON jc.id=jp.category_id
      ORDER BY wr.id DESC, jp.display_order, jp.id;
    "), rownames = FALSE, options = list(pageLength = 20))
  })

  output$runner_ui <- renderUI({
    tagList(
      div(class = "panel",
        selectInput("runner_round", "Round", choices = round_choices()),
        actionButton("preview_random", "Preview random assignment", class = "btn-primary"),
        actionButton("commit_preview", "Commit preview", class = "btn-success")
      ),
      h4("Preview"),
      DTOutput("assignment_preview"),
      h4("Committed assignments"),
      DTOutput("assignments_table")
    )
  })

  observeEvent(input$preview_random, {
    preview_assignments(random_assignments(active_round_id(input$runner_round)))
  })

  observeEvent(input$commit_preview, {
    df <- preview_assignments()
    if (!nrow(df)) {
      showNotification("No preview to commit.", type = "warning")
      return()
    }
    n <- commit_assignments(df)
    showNotification(sprintf("Committed %d assignments.", n), type = "message")
    preview_assignments(data.frame())
    touch()
  })

  output$assignment_preview <- renderDT({
    datatable(preview_assignments(), rownames = FALSE, options = list(pageLength = 20))
  })

  output$assignments_table <- renderDT({
    refresh_key()
    datatable(assignments_df(input$runner_round), rownames = FALSE, options = list(pageLength = 25))
  })

  output$wage_admin_ui <- renderUI({
    tagList(
      div(class = "panel",
        selectInput("wage_round", "Round", choices = round_choices()),
        selectInput("wage_rule", "Rule", c("pay_as_bid", "highest_accepted_bid", "lowest_rejected_bid", "median_bid", "instructor_override"), selected = get_setting("wage_clearing_rule", "highest_accepted_bid")),
        numericInput("wage_override", "Instructor override wage", value = num0(get_setting("initial_category_wage", 3)), min = 0, step = 0.5),
        actionButton("preview_wage", "Preview clearing", class = "btn-primary"),
        actionButton("commit_wage", "Commit wage assignments", class = "btn-success")
      ),
      DTOutput("wage_preview_table")
    )
  })

  wage_preview <- reactiveVal(data.frame())
  observeEvent(input$preview_wage, {
    set_setting("wage_clearing_rule", input$wage_rule)
    wage_preview(clear_wage_market(active_round_id(input$wage_round), input$wage_rule, input$wage_override))
  })
  observeEvent(input$commit_wage, {
    df <- assignment_from_wage_preview(active_round_id(input$wage_round), wage_preview())
    if (!nrow(df)) { showNotification("No accepted wage bids to commit.", type = "warning"); return() }
    commit_assignments(df)
    showNotification("Wage-bid assignments committed.", type = "message")
    touch()
  })
  output$wage_preview_table <- renderDT({
    datatable(wage_preview(), rownames = FALSE, options = list(pageLength = 30))
  })

  output$app_admin_ui <- renderUI({
    tagList(
      div(class = "panel",
        selectInput("app_round", "Round", choices = round_choices()),
        actionButton("preview_app_assign", "Preview ticket assignment", class = "btn-primary"),
        actionButton("commit_app_assign", "Commit ticket assignments", class = "btn-success")
      ),
      DTOutput("app_preview_table")
    )
  })

  observeEvent(input$preview_app_assign, {
    preview_assignments(application_assignments(active_round_id(input$app_round)))
  })
  observeEvent(input$commit_app_assign, {
    df <- preview_assignments()
    if (!nrow(df)) { showNotification("No ticket assignment preview.", type = "warning"); return() }
    commit_assignments(df)
    preview_assignments(data.frame())
    showNotification("Ticket assignments committed.", type = "message")
    touch()
  })
  output$app_preview_table <- renderDT({
    datatable(preview_assignments(), rownames = FALSE, options = list(pageLength = 30))
  })

  live_wages <- reactive({
    x <- tryCatch(jsonlite::fromJSON(get_setting("live_wages_json", "{}")), error = function(e) list(other = 1))
    if (is.null(x[["voluntary contribution"]])) x[["voluntary contribution"]] <- x[["answer/comment"]] %||% 1
    x
  })

  log_live_participation <- function(user_id, event_type, credit = "full", note = "") {
    stu <- db_query("SELECT * FROM students WHERE user_id=?", list(user_id))
    if (!nrow(stu)) stop("Student not found.")
    wage <- num0(live_wages()[[event_type]] %||% 1)
    mult <- switch(credit, full = 1, half = num0(get_setting("half_wage_multiplier", 0.5)), none = 0, 1)
    tokens <- wage * mult
    lid <- if (tokens > 0) ledger_add(stu$user_id[1], stu$display_name[1], tokens, 1, "live_participation", NA, NA, event_type) else NA
    db_exec("
      INSERT INTO participation_events(event_date, event_type, user_id, display_name, wage, multiplier, tokens, ledger_id, note)
      VALUES(?,?,?,?,?,?,?,?,?);
    ", list(as.character(Sys.Date()), event_type, stu$user_id[1], stu$display_name[1], wage, mult, tokens, ifelse(is.na(lid), NA, lid), note))
    tokens
  }

  output$live_ui <- renderUI({
    ev <- names(live_wages())
    stu <- roster()
    tagList(
      div(class = "panel",
        selectInput("live_event", "Event type", choices = ev),
        h4("Quick full-credit log"),
        div(style = "display:flex; flex-wrap:wrap; gap:6px;",
          lapply(seq_len(nrow(stu)), function(i) {
            actionButton(paste0("live_quick_", i), stu$display_name[i])
          })
        ),
        hr(),
        selectizeInput("live_student", "Student", choices = setNames(stu$user_id, paste(stu$display_name, stu$section))),
        radioButtons("live_credit", "Credit", c("Full" = "full", "Half" = "half", "No wage" = "none"), inline = TRUE),
        textInput("live_note", "Note", value = ""),
        actionButton("log_live", "Log tokens", class = "btn-success")
      ),
      DTOutput("live_events_table")
    )
  })

  observeEvent(input$log_live, {
    req(input$live_event, input$live_student)
    tokens <- log_live_participation(input$live_student, input$live_event, input$live_credit, input$live_note)
    showNotification(sprintf("Logged %.1f tokens.", tokens), type = "message")
    touch()
  })

  observe({
    refresh_key()
    stu <- roster()
    lapply(seq_len(nrow(stu)), function(i) {
      observeEvent(input[[paste0("live_quick_", i)]], {
        req(input$live_event)
        tokens <- log_live_participation(stu$user_id[i], input$live_event, "full", "")
        showNotification(sprintf("Logged %.1f tokens for %s.", tokens, stu$display_name[i]), type = "message")
        touch()
      }, ignoreInit = TRUE)
    })
  })

  output$live_events_table <- renderDT({
    refresh_key()
    datatable(db_query("SELECT * FROM participation_events ORDER BY id DESC LIMIT 100;"), rownames = FALSE)
  })

  output$completion_ui <- renderUI({
    refresh_key()
    rows <- assignments_df()
    rows <- rows[is.na(rows$awarded_ledger_id) | rows$status != "closed", , drop = FALSE]
    if (!nrow(rows)) return(p("No open assignments."))
    tagList(lapply(seq_len(nrow(rows)), function(i) {
      id <- rows$id[i]
      div(class = "panel",
        strong(paste(rows$display_name[i], "-", rows$job_name[i])),
        span(class = "muted", sprintf(" wage %.1f, round %s", rows$assigned_wage[i], rows$round_label[i])),
        br(),
        actionButton(paste0("complete_", id), "Complete", class = "btn-success"),
        actionButton(paste0("tried_", id), "Tried"),
        actionButton(paste0("missed_", id), "Missed")
      )
    }))
  })

  observe({
    refresh_key()
    rows <- assignments_df()
    rows <- rows[is.na(rows$awarded_ledger_id) | rows$status != "closed", , drop = FALSE]
    lapply(rows$id, function(id) {
      observeEvent(input[[paste0("complete_", id)]], {
        tokens <- award_assignment(id, "Complete")
        showNotification(sprintf("Awarded %.1f tokens.", tokens), type = "message")
        touch()
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("tried_", id)]], {
        tokens <- award_assignment(id, "Tried")
        showNotification(sprintf("Awarded %.1f tokens.", tokens), type = "message")
        touch()
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("missed_", id)]], {
        award_assignment(id, "Missed")
        showNotification("Marked missed.", type = "message")
        touch()
      }, ignoreInit = TRUE)
    })
  })

  output$ledger_table <- renderDT({
    refresh_key()
    datatable(db_query("SELECT * FROM token_ledger ORDER BY id DESC;"), rownames = FALSE, options = list(pageLength = 30))
  })

  output$public_goods_ui <- renderUI({
    if (int0(get_setting("public_good_enabled", 1)) != 1L) return(p("Public-good token spending is disabled."))
    tagList(
      div(class = "panel",
        h4("Create public good"),
        textInput("pg_name", "Name", value = "Exam-style reveal fund"),
        textAreaInput("pg_desc", "Description", rows = 2),
        numericInput("pg_threshold", "Manual threshold (0 uses formula)", value = 0, min = 0, step = 1),
        actionButton("create_pg", "Create", class = "btn-primary")
      ),
      DTOutput("pg_table")
    )
  })

  observeEvent(input$create_pg, {
    if (int0(get_setting("public_good_enabled", 1)) != 1L) {
      showNotification("Public-good token spending is disabled.", type = "warning")
      return()
    }
    req(nzchar(input$pg_name))
    db_exec("INSERT INTO public_goods(name, description, threshold, active) VALUES(?,?,?,1);",
            list(input$pg_name, input$pg_desc, num0(input$pg_threshold)))
    touch()
  })

  output$pg_table <- renderDT({
    refresh_key()
    pg <- db_query("
      SELECT pg.*, COALESCE(SUM(pgc.amount),0) AS contributed
      FROM public_goods pg
      LEFT JOIN public_good_contributions pgc ON pgc.public_good_id=pg.id
      GROUP BY pg.id
      ORDER BY pg.id DESC;
    ")
    if (nrow(pg)) pg$threshold_current <- vapply(pg$id, public_good_threshold, numeric(1))
    datatable(pg, rownames = FALSE)
  })

  output$extensions_ui <- renderUI({
    if (int0(get_setting("extension_good_enabled", 1)) != 1L) return(p("Problem set extensions are disabled."))
    tagList(
      div(class = "panel",
        h4("Problem set setup"),
        textInput("ps_name", "Problem set", value = "PS1"),
        textInput("ps_deadline", "Original deadline (YYYY-MM-DD HH:MM:SS)", value = format(Sys.time() + 86400, "%Y-%m-%d %H:%M:%S")),
        textInput("ps_solutions", "Solutions posted at (optional)", value = ""),
        actionButton("create_ps", "Save problem set", class = "btn-primary")
      ),
      DTOutput("ps_table"),
      h4("Extension purchases"),
      DTOutput("extension_table")
    )
  })

  observeEvent(input$create_ps, {
    if (int0(get_setting("extension_good_enabled", 1)) != 1L) {
      showNotification("Problem set extensions are disabled.", type = "warning")
      return()
    }
    db_exec("INSERT INTO problem_sets(name, original_deadline, solutions_posted_at, active) VALUES(?,?,?,1);",
            list(input$ps_name, input$ps_deadline, input$ps_solutions))
    touch()
  })
  output$ps_table <- renderDT({ refresh_key(); datatable(db_query("SELECT * FROM problem_sets ORDER BY id DESC;"), rownames = FALSE) })
  output$extension_table <- renderDT({
    refresh_key()
    datatable(db_query("
      SELECT ep.*, ps.name AS problem_set, s.display_name
      FROM extension_purchases ep
      LEFT JOIN problem_sets ps ON ps.id=ep.problem_set_id
      LEFT JOIN students s ON s.user_id=ep.user_id
      ORDER BY ep.id DESC;
    "), rownames = FALSE)
  })

  output$reweight_admin_ui <- renderUI({
    if (int0(get_setting("reweight_good_enabled", 1)) != 1L) return(p("Grade reweighting is disabled."))
    tagList(
      div(class = "panel",
        h4("Grade categories"),
        textInput("gc_name", "Name"),
        numericInput("gc_weight", "Current weight", value = 0, min = 0, max = 100, step = 1),
        checkboxInput("gc_eligible", "Eligible for reweighting", TRUE),
        actionButton("save_gc", "Save category", class = "btn-primary")
      ),
      DTOutput("gc_table"),
      h4("Submitted reweight requests"),
      DTOutput("rw_requests_table")
    )
  })

  observeEvent(input$save_gc, {
    if (int0(get_setting("reweight_good_enabled", 1)) != 1L) {
      showNotification("Grade reweighting is disabled.", type = "warning")
      return()
    }
    req(nzchar(input$gc_name))
    eligible <- int0(input$gc_eligible)
    if (tolower(trimws(input$gc_name)) == "participation") eligible <- 0L
    db_exec("
      INSERT INTO grade_categories(name,current_weight,eligible) VALUES(?,?,?)
      ON CONFLICT(name) DO UPDATE SET current_weight=excluded.current_weight, eligible=excluded.eligible;
    ", list(input$gc_name, num0(input$gc_weight), eligible))
    touch()
  })
  output$gc_table <- renderDT({ refresh_key(); datatable(db_query("SELECT * FROM grade_categories ORDER BY name;"), rownames = FALSE) })
  output$rw_requests_table <- renderDT({ refresh_key(); datatable(db_query("SELECT * FROM grade_reweight_requests ORDER BY id DESC;"), rownames = FALSE) })

  output$settings_ui <- renderUI({
    s <- settings_list()
    tagList(
      div(class = "panel",
        fluidRow(
          column(4, textInput("set_token_name", "Token name", value = s$token_name)),
          column(4, textInput("set_thresholds", "Participation thresholds", value = s$participation_thresholds)),
          column(4, selectInput("set_assignment_mode", "Default assignment mode", c("random", "wage_bidding", "application_bidding"), selected = s$assignment_mode))
        ),
        fluidRow(
          column(3, numericInput("set_half", "Half-wage multiplier", value = num0(s$half_wage_multiplier), min = 0, max = 1, step = 0.05)),
          column(3, numericInput("set_tickets", "Tickets per round", value = num0(s$tickets_per_round), min = 0, step = 1)),
          column(3, checkboxInput("set_multi", "Allow multiple jobs", value = int0(s$allow_multiple_jobs_per_round) == 1L)),
          column(3, selectInput("set_unfilled", "Unfilled slots", c("leave_empty", "random_fill", "instructor_prompt"), selected = s$unfilled_slot_behavior))
        ),
        fluidRow(
          column(4, checkboxInput("set_ext_enabled", "Enable extensions", int0(s$extension_good_enabled) == 1L)),
          column(4, checkboxInput("set_pg_enabled", "Enable public goods", int0(s$public_good_enabled) == 1L)),
          column(4, checkboxInput("set_rw_enabled", "Enable grade reweighting", int0(s$reweight_good_enabled) == 1L))
        ),
        textAreaInput("set_live_wages", "Live spot wages JSON", value = s$live_wages_json, rows = 3),
        textAreaInput("set_ext_prices", "Extension fixed prices JSON", value = s$extension_prices_json, rows = 2),
        fluidRow(
          column(4, numericInput("set_ext_pph", "Extension price per hour (0 uses fixed)", value = num0(s$extension_price_per_hour), min = 0, step = 0.1)),
          column(4, numericInput("set_ext_max", "Max extension hours", value = num0(s$extension_max_hours), min = 0, step = 1)),
          column(4, checkboxInput("set_after_solutions", "Allow after solutions posted", int0(s$extension_allow_after_solutions) == 1L))
        ),
        fluidRow(
          column(4, numericInput("set_pg_mult", "Public-good enrolled multiplier", value = num0(s$public_good_multiplier), min = 0, step = 0.1)),
          column(4, textInput("set_pg_manual", "Public-good manual threshold", value = s$public_good_manual_threshold)),
          column(4, textInput("set_rw_schedule", "Reweight cost schedule", value = s$reweight_cost_schedule))
        ),
        textInput("set_rw_categories", "Eligible reweight category names", value = s$grade_reweight_categories),
        actionButton("save_settings", "Save settings", class = "btn-primary")
      )
    )
  })

  observeEvent(input$save_settings, {
    vals <- list(
      token_name = input$set_token_name,
      participation_thresholds = input$set_thresholds,
      assignment_mode = input$set_assignment_mode,
      half_wage_multiplier = input$set_half,
      tickets_per_round = input$set_tickets,
      allow_multiple_jobs_per_round = int0(input$set_multi),
      unfilled_slot_behavior = input$set_unfilled,
      extension_good_enabled = int0(input$set_ext_enabled),
      public_good_enabled = int0(input$set_pg_enabled),
      reweight_good_enabled = int0(input$set_rw_enabled),
      live_wages_json = input$set_live_wages,
      extension_prices_json = input$set_ext_prices,
      extension_price_per_hour = input$set_ext_pph,
      extension_max_hours = input$set_ext_max,
      extension_allow_after_solutions = int0(input$set_after_solutions),
      public_good_multiplier = input$set_pg_mult,
      public_good_manual_threshold = input$set_pg_manual,
      reweight_cost_schedule = input$set_rw_schedule,
      grade_reweight_categories = input$set_rw_categories
    )
    invisible(lapply(names(vals), function(k) set_setting(k, vals[[k]])))
    showNotification("Settings saved.", type = "message")
    touch()
  })

  output$exports_ui <- renderUI({
    tagList(
      downloadButton("dl_students", "students.csv"),
      downloadButton("dl_rounds", "weekly_rounds.csv"),
      downloadButton("dl_jobs", "job_posts.csv"),
      downloadButton("dl_assignments", "job_assignments.csv"),
      downloadButton("dl_wage_bids", "wage_bids.csv"),
      downloadButton("dl_application_bids", "application_bids.csv"),
      downloadButton("dl_events", "participation_events.csv"),
      downloadButton("dl_ledger", "token_ledger.csv"),
      downloadButton("dl_public_goods", "public_goods.csv"),
      downloadButton("dl_extensions", "extension_purchases.csv"),
      downloadButton("dl_reweights", "grade_reweight_requests.csv")
    )
  })
  output$dl_students <- csv_download("students", "students.csv")
  output$dl_rounds <- csv_download("weekly_rounds", "weekly_rounds.csv")
  output$dl_jobs <- csv_download("job_posts", "job_posts.csv")
  output$dl_assignments <- csv_download("job_assignments", "job_assignments.csv")
  output$dl_wage_bids <- csv_download("wage_bids", "wage_bids.csv")
  output$dl_application_bids <- csv_download("application_bids", "application_bids.csv")
  output$dl_events <- csv_download("participation_events", "participation_events.csv")
  output$dl_ledger <- csv_download("token_ledger", "token_ledger.csv")
  output$dl_public_goods <- csv_download("public_goods", "public_goods.csv")
  output$dl_extensions <- csv_download("extension_purchases", "extension_purchases.csv")
  output$dl_reweights <- csv_download("grade_reweight_requests", "grade_reweight_requests.csv")

  current_student <- reactive({
    req(student_user())
    db_query("SELECT * FROM students WHERE user_id=?", list(student_user()))
  })

  output$student_jobs_ui <- renderUI({
    refresh_key()
    uid <- student_user()
    rows <- assignments_df()
    rows <- rows[rows$user_id == uid, , drop = FALSE]
    tagList(h4("My jobs"), DTOutput("student_jobs_table"))
  })
  output$student_jobs_table <- renderDT({
    refresh_key()
    rows <- assignments_df()
    rows <- rows[rows$user_id == student_user(), c("round_label", "job_name", "category", "assigned_wage", "status", "outcome", "awarded_tokens"), drop = FALSE]
    datatable(rows, rownames = FALSE)
  })

  output$student_wage_ui <- renderUI({
    refresh_key()
    cats <- db_query("SELECT * FROM job_categories WHERE COALESCE(active,1)=1 ORDER BY display_order, name;")
    tagList(
      div(class = "panel",
        selectInput("stu_wage_round", "Round", choices = round_choices()),
        selectInput("stu_wage_cat", "Job category", choices = setNames(cats$id, cats$name)),
        numericInput("stu_min_wage", "Minimum wage", value = 1, min = 0, step = 0.5),
        actionButton("submit_wage_bid", "Submit bid", class = "btn-primary")
      ),
      DTOutput("my_wage_bids")
    )
  })
  observeEvent(input$submit_wage_bid, {
    req(student_user(), input$stu_wage_round, input$stu_wage_cat)
    db_exec("
      INSERT INTO wage_bids(round_id, category_id, user_id, min_wage) VALUES(?,?,?,?)
      ON CONFLICT(round_id,category_id,user_id) DO UPDATE SET min_wage=excluded.min_wage, submitted_at=CURRENT_TIMESTAMP;
    ", list(int0(input$stu_wage_round), int0(input$stu_wage_cat), student_user(), num0(input$stu_min_wage)))
    showNotification("Wage bid saved.", type = "message")
    touch()
  })
  output$my_wage_bids <- renderDT({
    refresh_key()
    datatable(db_query("
      SELECT wr.label AS round, jc.name AS category, wb.min_wage, wb.submitted_at
      FROM wage_bids wb
      LEFT JOIN weekly_rounds wr ON wr.id=wb.round_id
      LEFT JOIN job_categories jc ON jc.id=wb.category_id
      WHERE wb.user_id=? ORDER BY wb.submitted_at DESC;
    ", list(student_user())), rownames = FALSE)
  })

  output$student_app_ui <- renderUI({
    refresh_key()
    cats <- db_query("SELECT * FROM job_categories WHERE COALESCE(active,1)=1 ORDER BY display_order, name;")
    tagList(
      div(class = "panel",
        selectInput("stu_app_round", "Round", choices = round_choices()),
        selectInput("stu_app_cat", "Job pool", choices = setNames(cats$id, cats$name)),
        numericInput("stu_tickets", "Tickets", value = 1, min = 0, step = 1),
        actionButton("submit_app_bid", "Allocate tickets", class = "btn-primary")
      ),
      p(class = "muted", paste("Tickets per round:", get_setting("tickets_per_round", 10))),
      DTOutput("my_app_bids")
    )
  })
  observeEvent(input$submit_app_bid, {
    req(student_user(), input$stu_app_round, input$stu_app_cat)
    uid <- student_user(); rid <- int0(input$stu_app_round)
    existing <- db_query("SELECT COALESCE(SUM(tickets),0) n FROM application_bids WHERE round_id=? AND user_id=? AND category_id<>?;",
                         list(rid, uid, int0(input$stu_app_cat)))$n[1]
    total <- num0(existing) + num0(input$stu_tickets)
    cap <- num0(get_setting("tickets_per_round", 10))
    if (total > cap) {
      showNotification(sprintf("Ticket allocation exceeds %.0f.", cap), type = "error")
      return()
    }
    db_exec("
      INSERT INTO application_bids(round_id, category_id, user_id, tickets) VALUES(?,?,?,?)
      ON CONFLICT(round_id,category_id,user_id) DO UPDATE SET tickets=excluded.tickets, submitted_at=CURRENT_TIMESTAMP;
    ", list(rid, int0(input$stu_app_cat), uid, num0(input$stu_tickets)))
    showNotification("Ticket allocation saved.", type = "message")
    touch()
  })
  output$my_app_bids <- renderDT({
    refresh_key()
    datatable(db_query("
      SELECT wr.label AS round, jc.name AS category, ab.tickets, ab.submitted_at
      FROM application_bids ab
      LEFT JOIN weekly_rounds wr ON wr.id=ab.round_id
      LEFT JOIN job_categories jc ON jc.id=ab.category_id
      WHERE ab.user_id=? ORDER BY ab.submitted_at DESC;
    ", list(student_user())), rownames = FALSE)
  })

  output$student_tokens_ui <- renderUI({
    refresh_key()
    bal <- student_balance(student_user())
    tagList(
      fluidRow(
        column(6, div(class = "metric", "Lifetime earned", div(class = "value", round(bal$lifetime_earned[1], 1)))),
        column(6, div(class = "metric", "Spendable balance", div(class = "value", round(bal$spendable_balance[1], 1))))
      ),
      DTOutput("my_ledger")
    )
  })
  output$my_ledger <- renderDT({
    refresh_key()
    datatable(db_query("SELECT amount, earning, source_type, note, created_at FROM token_ledger WHERE user_id=? ORDER BY id DESC;", list(student_user())), rownames = FALSE)
  })

  output$student_spend_ui <- renderUI({
    s <- settings_list()
    pieces <- list()
    if (int0(s$extension_good_enabled) == 1L) {
      ps <- db_query("SELECT * FROM problem_sets WHERE COALESCE(active,1)=1 ORDER BY id DESC;")
      prices <- parse_prices()
      hours_choices <- if (nrow(prices)) prices$hours else c(24, 48)
      pieces[[length(pieces) + 1]] <- div(class = "panel",
        h4("Problem set extension"),
        selectInput("buy_ps", "Problem set", choices = setNames(ps$id, ps$name)),
        selectInput("buy_hours", "Hours", choices = hours_choices),
        actionButton("buy_extension", "Purchase extension", class = "btn-primary")
      )
    }
    if (int0(s$public_good_enabled) == 1L) {
      pgs <- db_query("SELECT * FROM public_goods WHERE COALESCE(active,1)=1 ORDER BY id DESC;")
      pieces[[length(pieces) + 1]] <- div(class = "panel",
        h4("Public good contribution"),
        selectInput("contrib_pg", "Fund", choices = setNames(pgs$id, pgs$name)),
        numericInput("contrib_amount", "Tokens", value = 1, min = 0, step = 1),
        actionButton("make_contribution", "Contribute", class = "btn-primary")
      )
    }
    if (!length(pieces)) pieces <- list(p("Token goods are currently disabled."))
    tagList(pieces)
  })

  observeEvent(input$buy_extension, {
    if (int0(get_setting("extension_good_enabled", 1)) != 1L) {
      showNotification("Problem set extensions are disabled.", type = "warning")
      return()
    }
    req(student_user(), input$buy_ps, input$buy_hours)
    ps <- db_query("SELECT * FROM problem_sets WHERE id=?", list(int0(input$buy_ps)))
    ok <- can_buy_extension(ps, num0(input$buy_hours))
    if (!isTRUE(ok)) { showNotification(ok, type = "error"); return() }
    cost <- extension_cost(num0(input$buy_hours))
    if (is.na(cost)) { showNotification("No price is configured for those hours.", type = "error"); return() }
    lid <- tryCatch(spend_tokens(student_user(), cost, "extension_purchase", int0(input$buy_ps), NA, paste(input$buy_hours, "hour extension")), error = function(e) e)
    if (inherits(lid, "error")) { showNotification(lid$message, type = "error"); return() }
    db_exec("INSERT INTO extension_purchases(problem_set_id,user_id,hours,cost,ledger_id) VALUES(?,?,?,?,?);",
            list(int0(input$buy_ps), student_user(), num0(input$buy_hours), cost, lid))
    showNotification("Extension purchased.", type = "message")
    touch()
  })

  observeEvent(input$make_contribution, {
    if (int0(get_setting("public_good_enabled", 1)) != 1L) {
      showNotification("Public-good token spending is disabled.", type = "warning")
      return()
    }
    req(student_user(), input$contrib_pg)
    lid <- tryCatch(spend_tokens(student_user(), num0(input$contrib_amount), "public_good_contribution", int0(input$contrib_pg), NA, "public good"), error = function(e) e)
    if (inherits(lid, "error")) { showNotification(lid$message, type = "error"); return() }
    db_exec("INSERT INTO public_good_contributions(public_good_id,user_id,amount,ledger_id) VALUES(?,?,?,?);",
            list(int0(input$contrib_pg), student_user(), num0(input$contrib_amount), lid))
    showNotification("Contribution recorded.", type = "message")
    touch()
  })

  output$student_public_ui <- renderUI({
    if (int0(get_setting("public_good_enabled", 1)) != 1L) return(p("Public-good token spending is disabled."))
    refresh_key()
    pg <- db_query("
      SELECT pg.*, COALESCE(SUM(pgc.amount),0) AS contributed
      FROM public_goods pg
      LEFT JOIN public_good_contributions pgc ON pgc.public_good_id=pg.id
      GROUP BY pg.id ORDER BY pg.id DESC;
    ")
    if (!nrow(pg)) return(p("No public goods are active."))
    tagList(lapply(seq_len(nrow(pg)), function(i) {
      th <- public_good_threshold(pg$id[i])
      div(class = "panel",
        strong(pg$name[i]),
        p(pg$description[i] %||% ""),
        tags$progress(value = min(pg$contributed[i], th), max = th),
        p(sprintf("%.1f / %.1f tokens", pg$contributed[i], th))
      )
    }))
  })

  output$student_reweight_ui <- renderUI({
    if (int0(get_setting("reweight_good_enabled", 1)) != 1L) return(p("Grade reweighting is disabled."))
    cats <- eligible_grade_categories()
    tagList(
      div(class = "panel",
        selectInput("rw_from", "Shift weight from", choices = cats$name),
        selectInput("rw_to", "Shift weight to", choices = cats$name),
        numericInput("rw_points", "Percentage points", value = 1, min = 0, step = 1),
        actionButton("preview_rw", "Preview", class = "btn-primary"),
        actionButton("submit_rw", "Submit and spend", class = "btn-success")
      ),
      uiOutput("rw_preview_ui"),
      tableOutput("rw_preview_weights")
    )
  })

  rw_preview_state <- reactiveVal(NULL)
  observeEvent(input$preview_rw, {
    if (int0(get_setting("reweight_good_enabled", 1)) != 1L) {
      showNotification("Grade reweighting is disabled.", type = "warning")
      return()
    }
    if (identical(input$rw_from, input$rw_to)) {
      showNotification("Choose two different categories.", type = "warning")
      return()
    }
    x <- tryCatch(reweight_preview(input$rw_from, input$rw_to, num0(input$rw_points)), error = function(e) e)
    if (inherits(x, "error")) {
      showNotification(x$message, type = "error")
      return()
    }
    rw_preview_state(x)
  })
  output$rw_preview_ui <- renderUI({
    x <- rw_preview_state()
    if (is.null(x)) return(NULL)
    tagList(
      p(sprintf("Token cost: %.1f", x$cost))
    )
  })
  output$rw_preview_weights <- renderTable({
    x <- rw_preview_state()
    if (is.null(x)) return(NULL)
    x$weights
  }, striped = TRUE, hover = TRUE)
  observeEvent(input$submit_rw, {
    if (int0(get_setting("reweight_good_enabled", 1)) != 1L) {
      showNotification("Grade reweighting is disabled.", type = "warning")
      return()
    }
    if (is.null(rw_preview_state())) { showNotification("Preview first.", type = "warning"); return() }
    x <- tryCatch(reweight_preview(input$rw_from, input$rw_to, num0(input$rw_points)), error = function(e) e)
    if (inherits(x, "error")) { showNotification(x$message, type = "error"); return() }
    lid <- tryCatch(spend_tokens(student_user(), x$cost, "grade_reweight", NA, NA, paste(input$rw_from, "to", input$rw_to)), error = function(e) e)
    if (inherits(lid, "error")) { showNotification(lid$message, type = "error"); return() }
    db_exec("
      INSERT INTO grade_reweight_requests(user_id, from_category, to_category, points, cost, ledger_id, status)
      VALUES(?,?,?,?,?,?, 'submitted');
    ", list(student_user(), input$rw_from, input$rw_to, num0(input$rw_points), x$cost, lid))
    showNotification("Reweighting request submitted.", type = "message")
    touch()
  })
}

ui <- fluidPage(uiOutput("main_ui"))
shinyApp(ui, server)
