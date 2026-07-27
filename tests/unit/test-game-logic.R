# tests/unit/test-game-logic.R
# Unit tests for game logic: PD payoffs, bonus shares, job market bidding,
# weighted draw, and security guards.
# Run: testthat::test_file("tests/unit/test-game-logic.R")

here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "tests/unit")
source(file.path(here, "helpers.R"), local = TRUE)

# ── Default PD payoff matrix (matches app defaults) ──────────────────────────
default_olig <- function(section = "S01") {
  data.frame(
    pd_HH_A = 50, pd_HH_B = 50,
    pd_HL_A = 10, pd_HL_B = 70,
    pd_LH_A = 70, pd_LH_B = 10,
    pd_LL_A = 30, pd_LL_B = 30,
    pd_scale = 0.1,
    bonus_multiplier = 1.5,
    use_section_size = 0L,   # use submitter count for pure tests
    section = section,
    stringsAsFactors = FALSE
  )
}

make_subs_pd <- function(...) {
  rows <- list(...)
  tibble(
    user_id      = sapply(rows, `[[`, "user_id"),
    display_name = sapply(rows, `[[`, "display_name"),
    action       = sapply(rows, `[[`, "action"),
    contribute   = NA_real_,
    created_at   = seq_len(length(rows))  # deterministic ordering
  )
}

make_subs_bonus <- function(user_ids, display_names, contributions) {
  tibble(user_id = user_ids, display_name = display_names,
         action = NA_character_, contribute = contributions,
         created_at = seq_along(user_ids))
}

# ── Price War (PD) ────────────────────────────────────────────────────────────

test_that("pd_pair_payoffs: HH gives (50,50)", {
  olig <- default_olig()
  subs <- make_subs_pd(
    list(user_id = "u1", display_name = "Alice", action = "High"),
    list(user_id = "u2", display_name = "Bob",   action = "High")
  )
  res <- pd_pair_payoffs(olig, subs)
  expect_equal(nrow(res), 2)
  expect_equal(res$payoff[res$user_id == "u1"], 50)
  expect_equal(res$payoff[res$user_id == "u2"], 50)
})

test_that("pd_pair_payoffs: LL gives (30,30)", {
  olig <- default_olig()
  subs <- make_subs_pd(
    list(user_id = "u1", display_name = "Alice", action = "Low"),
    list(user_id = "u2", display_name = "Bob",   action = "Low")
  )
  res <- pd_pair_payoffs(olig, subs)
  expect_equal(res$payoff[res$user_id == "u1"], 30)
  expect_equal(res$payoff[res$user_id == "u2"], 30)
})

test_that("pd_pair_payoffs: HL — defector wins (u1 High gets 10, u2 Low gets 70)", {
  olig <- default_olig()
  subs <- make_subs_pd(
    list(user_id = "u1", display_name = "Alice", action = "High"),
    list(user_id = "u2", display_name = "Bob",   action = "Low")
  )
  res <- pd_pair_payoffs(olig, subs)
  expect_equal(res$payoff[res$user_id == "u1"], 10)
  expect_equal(res$payoff[res$user_id == "u2"], 70)
})

test_that("pd_pair_payoffs: LH — defector wins", {
  olig <- default_olig()
  subs <- make_subs_pd(
    list(user_id = "u1", display_name = "Alice", action = "Low"),
    list(user_id = "u2", display_name = "Bob",   action = "High")
  )
  res <- pd_pair_payoffs(olig, subs)
  expect_equal(res$payoff[res$user_id == "u1"], 70)
  expect_equal(res$payoff[res$user_id == "u2"], 10)
})

test_that("pd_pair_payoffs: four players form two pairs independently", {
  olig <- default_olig()
  subs <- make_subs_pd(
    list(user_id = "u1", display_name = "Alice", action = "High"),
    list(user_id = "u2", display_name = "Bob",   action = "High"),
    list(user_id = "u3", display_name = "Carol", action = "Low"),
    list(user_id = "u4", display_name = "Dan",   action = "Low")
  )
  res <- pd_pair_payoffs(olig, subs)
  expect_equal(nrow(res), 4)
  # Pair 1: u1(High) vs u2(High) → both 50
  expect_equal(res$payoff[res$user_id == "u1"], 50)
  expect_equal(res$payoff[res$user_id == "u2"], 50)
  # Pair 2: u3(Low) vs u4(Low) → both 30
  expect_equal(res$payoff[res$user_id == "u3"], 30)
  expect_equal(res$payoff[res$user_id == "u4"], 30)
})

test_that("pd_pair_payoffs: odd player (5th) is dropped", {
  olig <- default_olig()
  subs <- make_subs_pd(
    list(user_id = "u1", display_name = "Alice", action = "High"),
    list(user_id = "u2", display_name = "Bob",   action = "High"),
    list(user_id = "u3", display_name = "Carol", action = "Low"),
    list(user_id = "u4", display_name = "Dan",   action = "Low"),
    list(user_id = "u5", display_name = "Eve",   action = "High")
  )
  res <- pd_pair_payoffs(olig, subs)
  expect_equal(nrow(res), 4)
  expect_false("u5" %in% res$user_id)
})

test_that("pd_pair_payoffs: single player gets payoff = 0", {
  olig <- default_olig()
  subs <- make_subs_pd(
    list(user_id = "u1", display_name = "Alice", action = "High")
  )
  res <- pd_pair_payoffs(olig, subs)
  expect_equal(nrow(res), 1)
  expect_equal(res$payoff[1], 0)
})

test_that("pd_pair_payoffs: empty submissions returns empty tibble", {
  olig <- default_olig()
  subs <- make_subs_pd()
  res <- pd_pair_payoffs(olig, subs)
  expect_equal(nrow(res), 0)
})

# ── Bonus Pot ─────────────────────────────────────────────────────────────────

test_that("bonus_shares: pot = multiplier × total, divided by submitter count", {
  # 3 students each contribute 2: total=6, pot=1.5×6=9, share=9/3=3
  subs <- make_subs_bonus(c("u1","u2","u3"), c("Alice","Bob","Carol"), c(2, 2, 2))
  res  <- bonus_shares_simple(1.5, subs)
  expect_equal(res$pot_total[1],   9)
  expect_equal(res$share_each[1],  3)
  expect_equal(res$n_denom[1],     3)
})

test_that("bonus_shares: unequal contributions — pot and share calculated on total", {
  # u1=1, u2=3, u3=2: total=6, pot=9, share=9/3=3 each (denominator = n submitters)
  subs <- make_subs_bonus(c("u1","u2","u3"), c("Alice","Bob","Carol"), c(1, 3, 2))
  res  <- bonus_shares_simple(1.5, subs)
  expect_equal(res$total_contrib[1], 6)
  expect_equal(res$pot_total[1],     9)
  expect_equal(res$share_each[1],    3)   # all get same share regardless of contribution
})

test_that("bonus_shares: class-size denominator overrides submitter count", {
  # 2 submitters but class has 4: share = pot / 4
  subs <- make_subs_bonus(c("u1","u2"), c("Alice","Bob"), c(2, 2))
  res  <- bonus_shares_simple(1.5, subs, n_denom = 4)
  expect_equal(res$pot_total[1],  6)       # 1.5 × 4
  expect_equal(res$n_denom[1],    4)
  expect_equal(res$share_each[1], 1.5)    # 6 / 4
})

test_that("bonus_shares: NA contribution treated as 0", {
  subs <- make_subs_bonus(c("u1","u2"), c("Alice","Bob"), c(NA, 4))
  res  <- bonus_shares_simple(1.5, subs)
  expect_equal(res$total_contrib[1], 4)
})

test_that("bonus_shares: zero total → pot and share are both 0", {
  subs <- make_subs_bonus(c("u1","u2"), c("Alice","Bob"), c(0, 0))
  res  <- bonus_shares_simple(1.5, subs)
  expect_equal(res$pot_total[1],  0)
  expect_equal(res$share_each[1], 0)
})

test_that("round_to_half: values round to nearest 0.5", {
  expect_equal(round_to_half(1.2),  1.0)
  expect_equal(round_to_half(1.3),  1.5)
  expect_equal(round_to_half(2.74), 2.5)
  expect_equal(round_to_half(2.75), 3.0)
  expect_equal(round_to_half(0),    0.0)
})

# ── Weighted draw ─────────────────────────────────────────────────────────────

test_that("weighted_draw_pure: student with 0 prior jobs most likely to be drawn", {
  pool   <- c("Alice", "Bob", "Carol")
  counts <- c(Alice = 0, Bob = 5, Carol = 5)
  # Repeat 200 times; Alice should win the vast majority
  set.seed(42)
  draws  <- replicate(200, weighted_draw_pure(pool, counts))
  alice_pct <- mean(draws == "Alice")
  expect_gt(alice_pct, 0.8)
})

test_that("weighted_draw_pure: equal counts → uniform distribution roughly", {
  pool   <- c("A", "B", "C")
  counts <- c(A = 2, B = 2, C = 2)
  set.seed(99)
  draws  <- replicate(300, weighted_draw_pure(pool, counts))
  for (name in pool) expect_gt(mean(draws == name), 0.2)
})

test_that("weighted_draw_pure: invalid weights fall back to uniform", {
  pool   <- c("A", "B")
  counts <- c(A = Inf, B = Inf)  # produces 0 weights via 1/(wt_A*Inf^2 + 1)
  set.seed(7)
  draws  <- replicate(100, weighted_draw_pure(pool, counts))
  # Should still work without error and return only valid pool members
  expect_true(all(draws %in% pool))
})

# ── Job market bidding helpers (DB-based) ─────────────────────────────────────

new_market_db <- function() {
  con <- make_test_db()
  add_student(con, "u1", "Alice", "S01")
  add_student(con, "u2", "Bob",   "S01")
  add_student(con, "u3", "Carol", "S01")
  # One round, one category
  exec(con, "INSERT INTO weekly_rounds(id, label, status) VALUES(1, 'Week 1', 'open');")
  exec(con, "INSERT INTO job_categories(id, name, slots, wage) VALUES(1, 'Note Taker', 2, 5.0);")
  exec(con, "INSERT INTO job_posts(id, round_id, category_id, slots, wage) VALUES(1, 1, 1, 2, 5.0);")
  con
}

test_that("wage_bids: students can submit wage bids", {
  con <- new_market_db(); on.exit(DBI::dbDisconnect(con))
  exec(con, "INSERT INTO wage_bids(round_id, user_id, category_id, wage) VALUES(1,'u1',1,4.5);")
  exec(con, "INSERT INTO wage_bids(round_id, user_id, category_id, wage) VALUES(1,'u2',1,5.0);")
  rows <- qry(con, "SELECT * FROM wage_bids WHERE round_id=1 ORDER BY wage;")
  expect_equal(nrow(rows), 2)
  expect_equal(rows$wage[1], 4.5)  # lowest bid first
})

test_that("wage_bids: highest-accepted-bid clearing — top 2 slots filled at max bid", {
  con <- new_market_db(); on.exit(DBI::dbDisconnect(con))
  # 3 bids for 2 slots
  exec(con, "INSERT INTO wage_bids(round_id, user_id, category_id, wage) VALUES(1,'u1',1,3.0);")
  exec(con, "INSERT INTO wage_bids(round_id, user_id, category_id, wage) VALUES(1,'u2',1,4.0);")
  exec(con, "INSERT INTO wage_bids(round_id, user_id, category_id, wage) VALUES(1,'u3',1,5.0);")
  bids <- qry(con, "SELECT * FROM wage_bids WHERE round_id=1 AND category_id=1 ORDER BY wage DESC;")
  slots  <- 2L
  filled <- head(bids, slots)
  market_wage <- filled$wage[nrow(filled)]   # lowest accepted bid = market wage
  expect_equal(nrow(filled), 2)
  expect_equal(filled$user_id, c("u3", "u2"))
  expect_equal(market_wage, 4.0)
})

test_that("application_bids: rank ordering is preserved", {
  con <- new_market_db(); on.exit(DBI::dbDisconnect(con))
  exec(con, "INSERT INTO application_bids(round_id, user_id, category_id, rank) VALUES(1,'u1',1,2);")
  exec(con, "INSERT INTO application_bids(round_id, user_id, category_id, rank) VALUES(1,'u2',1,1);")
  exec(con, "INSERT INTO application_bids(round_id, user_id, category_id, rank) VALUES(1,'u3',1,3);")
  rows <- qry(con, "SELECT user_id FROM application_bids WHERE round_id=1 ORDER BY rank;")
  expect_equal(rows$user_id, c("u2","u1","u3"))
})

test_that("job_assignment: token award inserts ledger row with earning=1", {
  con <- new_market_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 5.0, 1L, "job_assignment",
               source_id = 1L, round_id = 1L, note = "Note Taker — Complete")
  bal <- student_balance_t(con, "u1")
  expect_equal(bal$lifetime_earned[1],   5.0)
  expect_equal(bal$spendable_balance[1], 5.0)
  rows <- qry(con, "SELECT source_type, source_id, earning FROM token_ledger WHERE user_id='u1';")
  expect_equal(rows$source_type[1], "job_assignment")
  expect_equal(rows$earning[1],     1L)
})

test_that("spend_tokens: extension purchase reduces balance", {
  con <- new_market_db(); on.exit(DBI::dbDisconnect(con))
  ledger_add_t(con, "u1", "Alice", 10.0, 1L, "job_assignment")
  spend_tokens_t(con, "u1", 3.0, "extension_purchase", note = "PS1 extension")
  bal <- student_balance_t(con, "u1")
  expect_equal(bal$spendable_balance[1], 7.0)
  # Spending row has earning=0, negative amount
  spend_row <- qry(con,
    "SELECT amount, earning FROM token_ledger WHERE user_id='u1' AND earning=0;")
  expect_equal(spend_row$amount[1],  -3.0)
  expect_equal(spend_row$earning[1],  0L)
})

# ── Eval sanitization (class-job-market question cost rule) ───────────────────

safe_eval_rule <- function(rule) {
  # Mirrors question_cost_for_index() sanitization logic from class-job-market
  sanitized <- gsub("\\b(question|index|q|n)\\b", "0", rule)
  if (!grepl("^[0-9 .+\\-*/^()[:space:]]+$", sanitized)) return(NULL)
  tryCatch(eval(parse(text = sanitized)), error = function(e) NULL)
}

test_that("safe_eval_rule: valid arithmetic is evaluated", {
  expect_equal(safe_eval_rule("11 + 2 * 0^2"), 11)
  expect_equal(safe_eval_rule("5 * 0 + 3"),    3)
  expect_equal(safe_eval_rule("100 / 4"),       25)
})

test_that("safe_eval_rule: variable substitution works", {
  # q → 0, n → 0: 5*q + n → 5*0 + 0 → 0
  expect_equal(safe_eval_rule("5*q + n"), 0)
  # index → 0: 11 + 2*index^2 → 11
  expect_equal(safe_eval_rule("11 + 2*index^2"), 11)
})

test_that("safe_eval_rule: function calls are blocked", {
  expect_null(safe_eval_rule("system('ls')"))
  expect_null(safe_eval_rule("readLines('/etc/passwd')"))
  expect_null(safe_eval_rule("Sys.getenv('SECRET')"))
})

test_that("safe_eval_rule: assignment and semicolons blocked", {
  expect_null(safe_eval_rule("x <- 1; x"))
  expect_null(safe_eval_rule("x = 5"))
})
