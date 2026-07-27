# tests/unit/helpers.R
# Shared test infrastructure: in-memory SQLite with the class-job-market schema.
# Source this at the top of every unit test file.

suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tibble)
  library(tidyr)
})

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b
num0   <- function(x) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), 0, x) }
int0   <- function(x) { x <- suppressWarnings(as.integer(x)); ifelse(is.na(x), 0L, x) }

# Open an in-memory SQLite and create the shared schema.
make_test_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  DBI::dbExecute(con, "
    CREATE TABLE users (
      user_id      TEXT PRIMARY KEY,
      display_name TEXT,
      is_admin     INTEGER DEFAULT 0,
      pw_hash      TEXT,
      section      TEXT,
      active       INTEGER DEFAULT 1
    );")

  DBI::dbExecute(con, "
    CREATE TABLE students (
      user_id      TEXT PRIMARY KEY,
      display_name TEXT,
      section      TEXT,
      active       INTEGER DEFAULT 1
    );")

  DBI::dbExecute(con, "
    CREATE TABLE token_ledger (
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

  DBI::dbExecute(con, "
    CREATE TABLE job_categories (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      name     TEXT,
      slots    INTEGER DEFAULT 1,
      wage     REAL DEFAULT 0
    );")

  DBI::dbExecute(con, "
    CREATE TABLE job_posts (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      round_id    INTEGER,
      category_id INTEGER,
      slots       INTEGER DEFAULT 1,
      wage        REAL DEFAULT 0
    );")

  DBI::dbExecute(con, "
    CREATE TABLE weekly_rounds (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      label      TEXT,
      status     TEXT DEFAULT 'open',
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );")

  DBI::dbExecute(con, "
    CREATE TABLE wage_bids (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      round_id    INTEGER,
      user_id     TEXT,
      category_id INTEGER,
      wage        REAL,
      created_at  TEXT DEFAULT CURRENT_TIMESTAMP
    );")

  DBI::dbExecute(con, "
    CREATE TABLE application_bids (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      round_id    INTEGER,
      user_id     TEXT,
      category_id INTEGER,
      rank        INTEGER,
      created_at  TEXT DEFAULT CURRENT_TIMESTAMP
    );")

  DBI::dbExecute(con, "
    CREATE TABLE job_assignments (
      id                INTEGER PRIMARY KEY AUTOINCREMENT,
      round_id          INTEGER,
      user_id           TEXT,
      category_id       INTEGER,
      wage              REAL,
      tokens            REAL DEFAULT 0,
      outcome           TEXT,
      awarded_ledger_id INTEGER,
      created_at        TEXT DEFAULT CURRENT_TIMESTAMP
    );")

  con
}

# Helpers that mirror the app's db_exec/db_query but take an explicit connection.
exec <- function(con, sql, p = list()) {
  if (length(p)) DBI::dbExecute(con, sql, p) else DBI::dbExecute(con, sql)
}
qry <- function(con, sql, p = list()) {
  if (length(p)) DBI::dbGetQuery(con, sql, p) else DBI::dbGetQuery(con, sql)
}

# Seed a student into both users and students tables.
add_student <- function(con, user_id, display_name, section = "S01") {
  exec(con, "INSERT OR IGNORE INTO users(user_id, display_name, section) VALUES(?,?,?);",
       list(user_id, display_name, section))
  exec(con, "INSERT OR IGNORE INTO students(user_id, display_name, section) VALUES(?,?,?);",
       list(user_id, display_name, section))
}

# Token ledger helpers (mirrors class-job-market functions, conn-explicit).
ledger_add_t <- function(con, user_id, display_name, amount, earning,
                         source_type, source_id = NA, round_id = NA, note = "") {
  exec(con,
    "INSERT INTO token_ledger(user_id, display_name, round_id, source_type,
       source_id, amount, earning, note)
     VALUES(?,?,?,?,?,?,?,?);",
    list(user_id, display_name,
         if (is.na(round_id)) NA else as.integer(round_id),
         source_type,
         if (is.na(source_id)) NA else as.integer(source_id),
         num0(amount), int0(earning), note))
  qry(con, "SELECT last_insert_rowid() id;")$id[1]
}

student_balance_t <- function(con, user_id) {
  r <- qry(con, "
    SELECT COALESCE(SUM(CASE WHEN earning=1 AND amount>0 THEN amount ELSE 0 END),0) AS lifetime_earned,
           COALESCE(SUM(amount),0) AS spendable_balance
    FROM token_ledger WHERE user_id=?;", list(user_id))
  if (!nrow(r)) data.frame(lifetime_earned = 0, spendable_balance = 0) else r
}

spend_tokens_t <- function(con, user_id, amount, source_type, note = "") {
  amount <- num0(amount)
  bal    <- student_balance_t(con, user_id)$spendable_balance[1]
  if (amount <= 0) stop("Amount must be positive.")
  if (bal < amount) stop("Insufficient spendable balance.")
  nm <- qry(con, "SELECT display_name FROM students WHERE user_id=?", list(user_id))
  nm <- if (nrow(nm)) nm$display_name[1] else user_id
  ledger_add_t(con, user_id, nm, -amount, 0L, source_type, note = note)
}

# ── Pure game-logic functions (copied verbatim from app.R) ────────────────────

round_to_half <- function(x) round(x * 2) / 2

pd_pair_payoffs <- function(olig, subs) {
  if (!nrow(subs)) return(tibble())
  s <- subs %>%
    arrange(created_at) %>%
    mutate(idx  = row_number(),
           pair = ceiling(idx / 2),
           role = ifelse(idx %% 2 == 1, "A", "B"))
  if (nrow(s) == 1)
    return(tibble(pair = 1, role = "A", user_id = s$user_id[1],
                  display_name = s$display_name[1], action = s$action[1], payoff = 0))
  if (nrow(s) %% 2 == 1) s <- s %>% slice(1:(nrow(s) - 1))

  payoff_for_pair <- function(a_action, b_action) {
    if (a_action == "High" && b_action == "High") return(c(A = olig$pd_HH_A, B = olig$pd_HH_B))
    if (a_action == "High" && b_action == "Low")  return(c(A = olig$pd_HL_A, B = olig$pd_HL_B))
    if (a_action == "Low"  && b_action == "High") return(c(A = olig$pd_LH_A, B = olig$pd_LH_B))
    return(c(A = olig$pd_LL_A, B = olig$pd_LL_B))
  }

  wide <- s %>%
    select(pair, role, user_id, display_name, action) %>%
    pivot_wider(names_from = role, values_from = c(user_id, display_name, action))

  out <- wide %>%
    rowwise() %>%
    mutate(A_pay = payoff_for_pair(action_A, action_B)[["A"]],
           B_pay = payoff_for_pair(action_A, action_B)[["B"]]) %>%
    ungroup() %>%
    pivot_longer(cols = c(A_pay, B_pay), names_to = "role_pay", values_to = "payoff") %>%
    mutate(role         = ifelse(role_pay == "A_pay", "A", "B"),
           user_id      = ifelse(role == "A", user_id_A, user_id_B),
           display_name = ifelse(role == "A", display_name_A, display_name_B),
           action       = ifelse(role == "A", action_A, action_B)) %>%
    select(pair, role, user_id, display_name, action, payoff)
  out
}

# Simplified bonus_shares for testing (no DB call for section size).
bonus_shares_simple <- function(multiplier, subs, n_denom = NULL) {
  subs <- subs %>%
    mutate(contribute = ifelse(is.na(contribute) | !nzchar(as.character(contribute)),
                               0, as.numeric(contribute)))
  total   <- sum(subs$contribute, na.rm = TRUE)
  pot     <- multiplier * total
  n_denom <- if (!is.null(n_denom)) n_denom else nrow(subs)
  share   <- if (n_denom > 0) pot / n_denom else 0
  subs %>% mutate(total_contrib = total, pot_total = pot,
                  n_denom = n_denom, share_each = share)
}

# weighted_draw reimplemented for testing (no DB call for counts).
weighted_draw_pure <- function(pool, counts, wt_A = 1, wt_B = 2) {
  # counts: named numeric, same names as pool
  wts <- 1 / (wt_A * counts[pool]^wt_B + 1)
  wts[!is.finite(wts)] <- 1
  if (sum(wts) == 0) wts <- rep(1, length(pool))
  sample(pool, 1, prob = wts / sum(wts))
}
