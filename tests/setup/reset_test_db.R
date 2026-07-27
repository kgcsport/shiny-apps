# tests/setup/reset_test_db.R
#
# Reset transactional data between test runs while preserving users,
# settings, and quiz questions.
#
# Usage:
#   Rscript tests/setup/reset_test_db.R              # full reset
#   Rscript tests/setup/reset_test_db.R --scenario cg06   # Alice → 1 token only
#   Rscript tests/setup/reset_test_db.R --scenario fresh  # wipe all ledger rows
#   Rscript tests/setup/reset_test_db.R --app quiz         # clear quiz responses only
#   Rscript tests/setup/reset_test_db.R --app auction      # clear auction round only
#   Rscript tests/setup/reset_test_db.R --app jobmarket    # clear bids + assignments
#   Rscript tests/setup/reset_test_db.R --app coordination # clear olig round data

suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

args <- commandArgs(trailingOnly = TRUE)
scenario <- if ("--scenario" %in% args) args[which(args == "--scenario") + 1] else ""
app      <- if ("--app"      %in% args) args[which(args == "--app")      + 1] else ""

shared_db_path <- function() {
  r <- Sys.getenv("CONNECT_CONTENT_DIR", "")
  if (nzchar(r)) return(file.path(r, "data", "class-job-market.sqlite"))
  docker <- "/srv/shiny-server/appdata"
  if (dir.exists(docker)) return(file.path(docker, "data", "class-job-market.sqlite"))
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
  if (!file.exists(path)) stop("DB not found: ", path, " — run seed_test_db.R first.")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
  con
}
exec <- function(con, sql, p = list()) {
  if (length(p)) DBI::dbExecute(con, sql, p) else DBI::dbExecute(con, sql)
}
qry  <- function(con, sql, p = list()) {
  if (length(p)) DBI::dbGetQuery(con, sql, p) else DBI::dbGetQuery(con, sql)
}

cat("── Reset mode:", if (nzchar(scenario)) scenario else if (nzchar(app)) app else "full", "\n")
con <- open_db(shared_db_path())

# ─────────────────────────────────────────────────────────────────────────────
# SCENARIO: cg06 — set Alice's spendable balance to exactly 1.0
# ─────────────────────────────────────────────────────────────────────────────
if (scenario == "cg06") {
  exec(con, "DELETE FROM token_ledger WHERE user_id='alice';")
  exec(con,
    "INSERT INTO token_ledger(user_id, display_name, source_type, amount, earning, note)
     VALUES('alice','Alice','test_seed',1.0,1,'CG-06 scenario: low balance');")
  cat("   ✓  Alice balance reset to 1.0 token (for CG-06 test)\n")
  DBI::dbDisconnect(con)
  quit(save = "no")
}

# ─────────────────────────────────────────────────────────────────────────────
# SCENARIO: fresh — wipe all ledger rows, restore seed balances
# ─────────────────────────────────────────────────────────────────────────────
if (scenario == "fresh") {
  exec(con, "DELETE FROM token_ledger;")
  balances <- list(
    list(uid="alice",amount=5.0), list(uid="bob",amount=5.0),
    list(uid="carol",amount=3.0), list(uid="dan",amount=5.0),
    list(uid="eve",  amount=3.0)
  )
  dn <- qry(con, "SELECT user_id, display_name FROM users WHERE is_admin=0;")
  for (b in balances) {
    nm <- dn$display_name[dn$user_id == b$uid]
    if (!length(nm)) nm <- b$uid
    exec(con,
      "INSERT INTO token_ledger(user_id, display_name, source_type, amount, earning, note)
       VALUES(?,?,'test_seed',?,1,'Seed balance (fresh reset)');",
      list(b$uid, nm, b$amount))
    cat(sprintf("   ✓  %s → %.1f tokens\n", b$uid, b$amount))
  }
  DBI::dbDisconnect(con)
  cat("── Token ledger reset to seed balances.\n")
  quit(save = "no")
}

# ─────────────────────────────────────────────────────────────────────────────
# APP-SCOPED RESETS
# ─────────────────────────────────────────────────────────────────────────────

if (app == "quiz") {
  exec(con, "DELETE FROM quiz_responses;")
  exec(con, "UPDATE quiz_state SET current_q=1, revealed=0 WHERE id=1;")
  cat("   ✓  quiz_responses cleared, quiz_state reset to Q1\n")
  DBI::dbDisconnect(con)
  quit(save = "no")
}

if (app == "jobmarket") {
  exec(con, "DELETE FROM wage_bids;")
  exec(con, "DELETE FROM application_bids;")
  exec(con, "DELETE FROM job_assignments;")
  # Remove non-seed ledger rows (keep test_seed balance rows)
  exec(con, "DELETE FROM token_ledger WHERE source_type NOT IN ('test_seed');")
  cat("   ✓  wage_bids, application_bids, job_assignments cleared\n")
  cat("   ✓  token_ledger entries (non-seed) removed\n")
  DBI::dbDisconnect(con)
  quit(save = "no")
}

if (app == "coordination") {
  exec(con, "DELETE FROM olig_submissions;")
  exec(con, "DELETE FROM olig_payouts;")
  exec(con, "UPDATE olig_settings SET current_round=1, round_status='open', current_game='pd', section='S01' WHERE id=1;")
  exec(con, "DELETE FROM token_ledger WHERE source_type IN ('coordination_grant','coordination_contrib');")
  cat("   ✓  olig_submissions and olig_payouts cleared\n")
  cat("   ✓  olig_settings reset to round 1, open, pd, S01\n")
  cat("   ✓  coordination token_ledger rows removed\n")
  DBI::dbDisconnect(con)
  quit(save = "no")
}

if (app == "priceindex") {
  exec(con, "DELETE FROM basket_items;")
  exec(con, "DELETE FROM price_records;")
  exec(con, "UPDATE app_state SET current_wave=1 WHERE id=1;")
  cat("   ✓  basket_items and price_records cleared, wave reset to 1\n")
  DBI::dbDisconnect(con)
  quit(save = "no")
}

if (app == "jobpicker") {
  exec(con, "DELETE FROM job_log;")
  exec(con, "UPDATE job_state SET bag_json='[]', cycle_id=0;")
  cat("   ✓  job_log cleared, job_state bags reset\n")
  DBI::dbDisconnect(con)
  quit(save = "no")
}

if (app == "auction") {
  DBI::dbDisconnect(con)
  if (!file.exists(auction_db_path())) {
    cat("   !  auction.sqlite not found — run seed_test_db.R first.\n")
    quit(save = "no")
  }
  acon <- open_db(auction_db_path())
  exec(acon, "DELETE FROM accepts;")
  exec(acon, "UPDATE auction_settings SET round=1 WHERE id=1;")
  exec(acon, "UPDATE auction_state SET running=0, current_price=20, units_remaining=5, started_at=NULL, ended_at=NULL WHERE id=1;")
  cat("   ✓  accepts cleared, auction reset to round 1, price=20\n")
  DBI::dbDisconnect(acon)
  quit(save = "no")
}

# ─────────────────────────────────────────────────────────────────────────────
# FULL RESET — clear all transactional tables, restore seed balances
# ─────────────────────────────────────────────────────────────────────────────
cat("Running FULL reset (all transactional data)\n")

# Job market
exec(con, "DELETE FROM wage_bids;")
exec(con, "DELETE FROM application_bids;")
exec(con, "DELETE FROM job_assignments;")
exec(con, "DELETE FROM participation_events;")

# Tokens: remove earned/spent rows, keep seed rows
exec(con, "DELETE FROM token_ledger WHERE source_type != 'test_seed';")

# Coordination games
exec(con, "DELETE FROM olig_submissions;")
exec(con, "DELETE FROM olig_payouts;")
exec(con, "UPDATE olig_settings SET current_round=1, round_status='open', current_game='pd', section='S01' WHERE id=1;")

# Review quiz
exec(con, "DELETE FROM quiz_responses;")
exec(con, "UPDATE quiz_state SET current_q=1, revealed=0 WHERE id=1;")

# Price index
exec(con, "DELETE FROM basket_items;")
exec(con, "DELETE FROM price_records;")
exec(con, "UPDATE app_state SET current_wave=1 WHERE id=1;")

# Job picker
exec(con, "DELETE FROM job_log;")
exec(con, "UPDATE job_state SET bag_json='[]', cycle_id=0;")

DBI::dbDisconnect(con)
cat("   ✓  Shared DB: all transactional data cleared\n")

# Auction
if (file.exists(auction_db_path())) {
  acon <- open_db(auction_db_path())
  exec(acon, "DELETE FROM accepts;")
  exec(acon, "UPDATE auction_settings SET round=1 WHERE id=1;")
  exec(acon, "UPDATE auction_state SET running=0, current_price=20, units_remaining=5, started_at=NULL, ended_at=NULL WHERE id=1;")
  DBI::dbDisconnect(acon)
  cat("   ✓  Auction DB: accepts cleared, round reset\n")
}

cat("\n── Full reset complete. Users, settings, and quiz questions are intact.\n")
cat("── Seed token balances preserved (alice=5, bob=5, carol=3, dan=5, eve=3).\n")
