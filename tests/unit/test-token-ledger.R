# tests/unit/test-token-ledger.R
# Unit tests for token_ledger operations (student_balance, ledger_add, spend_tokens).
# Run: testthat::test_file("tests/unit/test-token-ledger.R")

here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
source(file.path(here, "helpers.R"), local = TRUE)

# ── Helpers ───────────────────────────────────────────────────────────────────

new_db <- function() {
  con <- make_test_db()
  add_student(con, "u1", "Alice", "S01")
  add_student(con, "u2", "Bob",   "S01")
  add_student(con, "u3", "Carol", "S02")
  con
}

# ── Tests ─────────────────────────────────────────────────────────────────────

test_that("unknown user has zero balance", {
  con <- make_test_db()
  on.exit(DBI::dbDisconnect(con))
  bal <- student_balance_t(con, "nobody")
  expect_equal(bal$lifetime_earned[1],   0)
  expect_equal(bal$spendable_balance[1], 0)
})

test_that("earning row increases lifetime_earned and spendable_balance", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 3, 1L, "job_assignment", round_id = 1L)
  bal <- student_balance_t(con, "u1")
  expect_equal(bal$lifetime_earned[1],   3)
  expect_equal(bal$spendable_balance[1], 3)
})

test_that("spending row reduces spendable_balance but not lifetime_earned", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 5, 1L, "job_assignment")
  ledger_add_t(con, "u1", "Alice", -2, 0L, "extension_purchase")
  bal <- student_balance_t(con, "u1")
  expect_equal(bal$lifetime_earned[1],   5)
  expect_equal(bal$spendable_balance[1], 3)
})

test_that("multiple earning rows accumulate", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 2, 1L, "job_assignment", round_id = 1L)
  ledger_add_t(con, "u1", "Alice", 3, 1L, "live_participation", round_id = 2L)
  bal <- student_balance_t(con, "u1")
  expect_equal(bal$lifetime_earned[1],   5)
  expect_equal(bal$spendable_balance[1], 5)
})

test_that("ledger rows are user-scoped: u1 and u2 balances are independent", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 4, 1L, "job_assignment")
  ledger_add_t(con, "u2", "Bob",   7, 1L, "job_assignment")
  expect_equal(student_balance_t(con, "u1")$spendable_balance[1], 4)
  expect_equal(student_balance_t(con, "u2")$spendable_balance[1], 7)
})

test_that("spend_tokens succeeds when balance is sufficient", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 5, 1L, "job_assignment")
  spend_tokens_t(con, "u1", 3, "extension_purchase")
  bal <- student_balance_t(con, "u1")
  expect_equal(bal$spendable_balance[1], 2)
  expect_equal(bal$lifetime_earned[1],   5)  # unchanged
})

test_that("spend_tokens fails when balance is insufficient", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 2, 1L, "job_assignment")
  expect_error(spend_tokens_t(con, "u1", 5, "extension_purchase"),
               "Insufficient spendable balance")
})

test_that("spend_tokens rejects zero or negative amount", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 5, 1L, "job_assignment")
  expect_error(spend_tokens_t(con, "u1",  0, "extension_purchase"), "must be positive")
  expect_error(spend_tokens_t(con, "u1", -1, "extension_purchase"), "must be positive")
})

test_that("spend_tokens with exact balance succeeds (zero remainder)", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 3, 1L, "job_assignment")
  spend_tokens_t(con, "u1", 3, "extension_purchase")
  expect_equal(student_balance_t(con, "u1")$spendable_balance[1], 0)
})

test_that("coordination_grant (earning=1) raises spendable and lifetime", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  # Simulate coordination-games payout insert pattern
  exec(con,
    "INSERT INTO token_ledger(user_id, display_name, round_id, source_type, amount, earning, note)
     SELECT user_id, display_name, 1, 'coordination_grant', 2.5, 1, 'pd bonus'
     FROM users WHERE user_id=?;", list("u1"))
  bal <- student_balance_t(con, "u1")
  expect_equal(bal$spendable_balance[1], 2.5)
  expect_equal(bal$lifetime_earned[1],   2.5)
})

test_that("coordination_contrib (earning=0, amount<0) reduces spendable only", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 5, 1L, "job_assignment")
  exec(con,
    "INSERT INTO token_ledger(user_id, display_name, round_id, source_type, amount, earning, note)
     SELECT user_id, display_name, 1, 'coordination_contrib', -2, 0, 'bonus contribution'
     FROM users WHERE user_id=?;", list("u1"))
  bal <- student_balance_t(con, "u1")
  expect_equal(bal$lifetime_earned[1],   5)
  expect_equal(bal$spendable_balance[1], 3)
})

test_that("idempotency guard: second coordination_contrib row not inserted", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 10, 1L, "job_assignment")
  # Insert once
  exec(con,
    "INSERT INTO token_ledger(user_id, display_name, round_id, source_type, amount, earning, note)
     SELECT user_id, display_name, 1, 'coordination_contrib', -3, 0, 'test'
     FROM users WHERE user_id=?;", list("u1"))
  # Check idempotency guard (mirrors debit_pending_contributions)
  already <- qry(con,
    "SELECT COUNT(*) n FROM token_ledger WHERE user_id=? AND round_id=? AND source_type='coordination_contrib';",
    list("u1", 1L))$n[1]
  expect_equal(as.integer(already), 1L)
  # Only insert if already == 0 (guard works)
  if (as.integer(already) > 0) {
    # should NOT insert
  } else {
    exec(con,
      "INSERT INTO token_ledger(user_id, display_name, round_id, source_type, amount, earning, note)
       VALUES('u1','Alice',1,'coordination_contrib',-3,0,'duplicate');")
  }
  n_rows <- qry(con,
    "SELECT COUNT(*) n FROM token_ledger WHERE user_id='u1' AND source_type='coordination_contrib';")$n[1]
  expect_equal(as.integer(n_rows), 1L)  # still only one
})

test_that("DELETE coordination_contrib rows reverses contributions", {
  con <- new_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 10, 1L, "job_assignment")
  exec(con,
    "INSERT INTO token_ledger(user_id, display_name, round_id, source_type, amount, earning)
     VALUES('u1','Alice',2,'coordination_contrib',-4,0);")
  expect_equal(student_balance_t(con, "u1")$spendable_balance[1], 6)
  # Round reset (mirrors admin clear)
  exec(con,
    "DELETE FROM token_ledger WHERE round_id=? AND source_type='coordination_contrib';",
    list(2L))
  expect_equal(student_balance_t(con, "u1")$spendable_balance[1], 10)
})
