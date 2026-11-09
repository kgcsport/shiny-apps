#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
insurance_path <- if (file.exists("apps/class-job-market/insurance.R")) {
  "apps/class-job-market/insurance.R"
} else {
  "/srv/shiny-server/class-job-market/insurance.R"
}
source(insurance_path, local = TRUE)

app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
required_contract <- c(
  "CREATE TABLE IF NOT EXISTS insurance_products",
  "CREATE TABLE IF NOT EXISTS insurance_purchases",
  "CREATE TABLE IF NOT EXISTS insurance_claims",
  "CREATE TABLE IF NOT EXISTS insurance_dividends",
  "UNIQUE(product_id,user_id)",
  "submit_insurance_purchase",
  "cold_call_pass_btn",
  "slide_cold_call_pass_btn",
  "issue_insurance_claim",
  "insurance_dividend",
  "suggested_next_premium"
)
missing <- required_contract[!vapply(required_contract, grepl, logical(1),
                                     x = app_text, fixed = TRUE)]
if (length(missing)) stop("Insurance workflow contract missing: ", paste(missing, collapse = ", "))

con <- dbConnect(SQLite(), ":memory:")
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "CREATE TABLE users(user_id TEXT PRIMARY KEY,display_name TEXT,active INTEGER,is_admin INTEGER,is_demo INTEGER);")
dbExecute(con, "CREATE TABLE token_ledger(id INTEGER PRIMARY KEY AUTOINCREMENT,user_id TEXT,display_name TEXT,source_type TEXT,source_id INTEGER,amount REAL,earning INTEGER,note TEXT);")
dbExecute(con, "CREATE TABLE insurance_products(id INTEGER PRIMARY KEY,scope_key TEXT,round_id INTEGER,benefit REAL,premium REAL,status TEXT,price_step REAL);")
dbExecute(con, "CREATE TABLE insurance_purchases(id INTEGER PRIMARY KEY AUTOINCREMENT,product_id INTEGER,user_id TEXT,premium REAL,ledger_id INTEGER,UNIQUE(product_id,user_id));")
dbExecute(con, "CREATE TABLE insurance_claims(id INTEGER PRIMARY KEY AUTOINCREMENT,product_id INTEGER,user_id TEXT,benefit REAL,pass_event_id INTEGER,ledger_id INTEGER,UNIQUE(product_id,user_id));")
dbExecute(con, "CREATE TABLE insurance_dividends(id INTEGER PRIMARY KEY AUTOINCREMENT,product_id INTEGER,user_id TEXT,amount REAL,ledger_id INTEGER,UNIQUE(product_id,user_id));")

students <- data.frame(user_id = paste0("s", 1:4), display_name = paste("Student", 1:4))
for (i in seq_len(nrow(students))) {
  dbExecute(con, "INSERT INTO users VALUES(?,?,1,0,0);", params = unname(as.list(students[i, ])))
  dbExecute(con, "INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note) VALUES(?,?,'job',5,1,'starting earned balance');", params = unname(as.list(students[i, ])))
}
dbExecute(con, "INSERT INTO insurance_products VALUES(1,'econ342::s01',10,2,1,'open',0.5);")

# Four students purchase once; a duplicate request cannot debit twice.
for (i in seq_len(nrow(students))) {
  inserted <- dbExecute(con, "INSERT OR IGNORE INTO insurance_purchases(product_id,user_id,premium) VALUES(1,?,1);", params = list(students$user_id[i]))
  stopifnot(inserted == 1L)
  purchase_id <- dbGetQuery(con, "SELECT last_insert_rowid() id;")$id[1]
  dbExecute(con, "INSERT INTO token_ledger(user_id,display_name,source_type,source_id,amount,earning,note) VALUES(?,?,'insurance_premium',?,-1,0,'premium');", params = list(students$user_id[i], students$display_name[i], purchase_id))
}
stopifnot(dbExecute(con, "INSERT OR IGNORE INTO insurance_purchases(product_id,user_id,premium) VALUES(1,'s1',1);") == 0L)

# One insured live pass pays once; a second pass cannot create a second claim.
stopifnot(dbExecute(con, "INSERT OR IGNORE INTO insurance_claims(product_id,user_id,benefit,pass_event_id) VALUES(1,'s1',2,99);") == 1L)
claim_id <- dbGetQuery(con, "SELECT last_insert_rowid() id;")$id[1]
dbExecute(con, "INSERT INTO token_ledger(user_id,display_name,source_type,source_id,amount,earning,note) VALUES('s1','Student 1','insurance_claim',?,2,0,'claim');", params = list(claim_id))
stopifnot(dbExecute(con, "INSERT OR IGNORE INTO insurance_claims(product_id,user_id,benefit,pass_event_id) VALUES(1,'s1',2,100);") == 0L)

accounting <- insurance_round_accounting(4, 1, 2, 4)
stopifnot(accounting$profit == 2, accounting$dividend_each == 0.5)
loss_accounting <- insurance_round_accounting(1, 1, 2, 4)
stopifnot(loss_accounting$profit == -1, loss_accounting$dividend_each == 0)
for (i in seq_len(nrow(students))) {
  dbExecute(con, "INSERT INTO insurance_dividends(product_id,user_id,amount) VALUES(1,?,?);", params = list(students$user_id[i], accounting$dividend_each))
  dividend_id <- dbGetQuery(con, "SELECT last_insert_rowid() id;")$id[1]
  dbExecute(con, "INSERT INTO token_ledger(user_id,display_name,source_type,source_id,amount,earning,note) VALUES(?,?,'insurance_dividend',?,?,0,'dividend');", params = list(students$user_id[i], students$display_name[i], dividend_id, accounting$dividend_each))
}

cashflows <- dbGetQuery(con, "SELECT COALESCE(SUM(amount),0) x FROM token_ledger WHERE source_type LIKE 'insurance_%';")$x[1]
gross_earnings <- dbGetQuery(con, "SELECT COALESCE(SUM(amount),0) x FROM token_ledger WHERE earning=1;")$x[1]
stopifnot(abs(cashflows) < 1e-9, gross_earnings == 20)
stopifnot(insurance_zero_profit_premium(2, 4, step = 0.5) == 0.5)
stopifnot(insurance_zero_profit_premium(10, 0, fallback = 1.2, step = 0.5) == 1)

# Exercise the production Shiny observers and shared SQLite persistence. This
# is deliberately multi-session: classroom failures usually happen at the
# browser/database boundary, not in the accounting helpers above.
if (requireNamespace("shiny", quietly = TRUE)) {
  scratch <- tempfile("pass-insurance-")
  dir.create(file.path(scratch, "data"), recursive = TRUE)
  old_content <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
  Sys.setenv(CONNECT_CONTENT_DIR = scratch)
  on.exit({
    if (is.na(old_content)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_content)
    unlink(scratch, recursive = TRUE)
  }, add = TRUE)

  app <- new.env(parent = globalenv())
  source(app_path, local = app)
  # Keep the explicitly selected lecture independent of the host clock.
  app$set_setting("class_days", "")
  app$db_exec("DELETE FROM insurance_dividends;")
  app$db_exec("DELETE FROM insurance_claims;")
  app$db_exec("DELETE FROM insurance_purchases;")
  app$db_exec("DELETE FROM insurance_products;")
  app$db_exec("DELETE FROM scope_active_rounds;")
  app$db_exec("DELETE FROM section_scope_memberships;")
  app$db_exec("DELETE FROM weekly_rounds;")
  app$db_exec("DELETE FROM token_ledger;")

  people <- list(
    c("admin@test.edu", "Instructor", 1L, "01"),
    c("s1@test.edu", "Student 1", 0L, "01"),
    c("s2@test.edu", "Student 2", 0L, "01"),
    c("s3@test.edu", "Student 3", 0L, "01"),
    c("outsider@test.edu", "Other Section", 0L, "02")
  )
  for (person in people) {
    app$db_exec(
      "INSERT OR REPLACE INTO users(user_id,display_name,pw_hash,is_admin,course,section,active,is_demo)
       VALUES(?,?,?,?,?,?,1,0);",
      list(person[1], person[2], as.character(""), as.integer(person[3]), "ECON 342", person[4]))
    app$db_exec("INSERT OR IGNORE INTO user_environments(user_id,course,section) VALUES(?,?,?);",
                list(person[1], "ECON 342", person[4]))
  }
  for (uid in c("s1@test.edu", "s2@test.edu", "s3@test.edu", "outsider@test.edu")) {
    app$db_exec(
      "INSERT INTO token_ledger(user_id,display_name,source_type,amount,earning,note)
       SELECT user_id,display_name,'test_seed',10,1,'classroom resilience fixture'
       FROM users WHERE user_id=?;", list(uid))
  }
  app$db_exec("INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled) VALUES('Insurance class','2026-11-09','wage_bidding',1);")
  round_id <- as.integer(app$db_query("SELECT last_insert_rowid() id;")$id[1])
  app$db_exec("INSERT INTO scope_active_rounds(scope_key,course,sections,round_id) VALUES('econ 342::01','ECON 342','01',?);", list(round_id))
  app$db_exec("INSERT INTO section_scope_memberships(course_key,section_key,scope_key) VALUES('econ 342','01','econ 342::01');")

  set_admin <- function(rv) {
    rv$authed <- TRUE; rv$user_id <- "admin@test.edu"; rv$name <- "Instructor"
    rv$course <- "ECON 342"; rv$section <- "01"; rv$is_admin <- TRUE
    rv$admin_role <- "super_admin"; rv$active_course <- "ECON 342"
    rv$active_section <- "01"; rv$active_sections <- "01"; rv$impersonating <- FALSE
  }
  set_student <- function(rv, uid, name, section = "01") {
    rv$authed <- TRUE; rv$user_id <- uid; rv$name <- name
    rv$course <- "ECON 342"; rv$section <- section; rv$is_admin <- FALSE
    rv$active_course <- "ECON 342"; rv$active_section <- section
    rv$active_sections <- section; rv$is_demo <- FALSE
  }

  # Open through the real instructor control.
  shiny::testServer(app$server, {
    set_admin(rv)
    session$setInputs(insurance_benefit_input = 2, insurance_premium_input = 3,
                      insurance_price_step_input = 0.5,
                      save_insurance_product_btn = 1)
    session$flushReact()
  })
  product <- app$db_query("SELECT * FROM insurance_products;")
  stopifnot(nrow(product) == 1L, product$status[1] == "open",
            product$scope_key[1] == "econ 342::01")
  product_id <- as.integer(product$id[1])

  buy_policy <- function(uid, name, click = 1L) shiny::testServer(app$server, {
    set_student(rv, uid, name)
    session$setInputs(open_insurance = 1L)
    if (identical(click, 1L) && identical(uid, "s1@test.edu"))
      stopifnot(grepl("Buy policy", output$spend_form$html, fixed = TRUE))
    session$setInputs(submit_insurance_purchase = click)
    session$flushReact()
    if (any(app$db_query("SELECT user_id FROM insurance_purchases;")$user_id == uid))
      stopifnot(grepl("Passes on hand for this class period: 1", output$spend_form$html, fixed = TRUE))
  })
  buy_policy("s1@test.edu", "Student 1")
  buy_policy("s1@test.edu", "Student 1", 2L) # browser double-click/retry
  # A legacy deadline cannot close an open market.
  stopifnot(DBI::dbExecute(app$get_con(), "UPDATE insurance_products SET purchase_deadline='2000-01-01 00:00:00' WHERE id=?;",
                           params = list(product_id)) == 1L)
  # A failed debit must roll back coverage so the same student can retry.
  DBI::dbExecute(app$get_con(), "CREATE TRIGGER reject_insurance_debit BEFORE INSERT ON token_ledger
    WHEN NEW.source_type='insurance_premium' BEGIN SELECT RAISE(ABORT, 'test debit failure'); END;")
  buy_policy("s2@test.edu", "Student 2")
  stopifnot(nrow(app$db_query("SELECT * FROM insurance_purchases;")) == 1L,
            nrow(app$db_query("SELECT id FROM token_ledger WHERE source_type='insurance_premium';")) == 1L)
  DBI::dbExecute(app$get_con(), "DROP TRIGGER reject_insurance_debit;")
  buy_policy("s2@test.edu", "Student 2")
  purchases <- app$db_query("SELECT * FROM insurance_purchases ORDER BY user_id;")
  premium_ledger <- app$db_query("SELECT * FROM token_ledger WHERE source_type='insurance_premium';")
  stopifnot(nrow(purchases) == 2L, nrow(premium_ledger) == 2L,
            sum(premium_ledger$amount) == -6, all(premium_ledger$earning == 0L),
            all(purchases$ledger_id == premium_ledger$id[match(purchases$id, premium_ledger$source_id)]))

  # Once anyone buys, a later admin retry cannot rewrite their contract.
  shiny::testServer(app$server, {
    set_admin(rv)
    session$setInputs(insurance_benefit_input = 9, insurance_premium_input = 7,
                      insurance_price_step_input = 0.5,
                      save_insurance_product_btn = 1)
    session$flushReact()
  })
  frozen <- app$db_query("SELECT premium,benefit FROM insurance_products WHERE id=?;", list(product_id))
  stopifnot(frozen$premium[1] == 3, frozen$benefit[1] == 2)

  # The first live pass pays immediately while sales are still open.
  # A repeated pass remains in the audit but cannot pay twice.
  shiny::testServer(app$server, {
    set_admin(rv)
    drawn <- list(user_id = "s1@test.edu", display_name = "Student 1", section = "01")
    rv$cold_call_draw <- drawn
    DBI::dbExecute(app$get_con(), "CREATE TRIGGER reject_insurance_payout BEFORE INSERT ON token_ledger
      WHEN NEW.source_type='insurance_claim' BEGIN SELECT RAISE(ABORT, 'test payout failure'); END;")
    session$setInputs(cold_call_pass_btn = 1)
    stopifnot(identical(rv$cold_call_draw, drawn),
              nrow(app$db_query("SELECT id FROM live_score_events WHERE outcome='pass';")) == 0L,
              nrow(app$db_query("SELECT id FROM insurance_claims;")) == 0L)
    DBI::dbExecute(app$get_con(), "DROP TRIGGER reject_insurance_payout;")
    session$setInputs(cold_call_pass_btn = 2)
    first_payout <- app$db_query("SELECT amount FROM token_ledger WHERE source_type='insurance_claim';")
    stopifnot(identical(as.numeric(first_payout$amount), 2))
    rv$cold_call_draw <- list(user_id = "s1@test.edu", display_name = "Student 1", section = "01")
    session$setInputs(cold_call_pass_btn = 3); session$flushReact()
  })
  claims <- app$db_query("SELECT * FROM insurance_claims;")
  pass_events <- app$db_query("SELECT * FROM live_score_events WHERE event_kind='cold_call' AND outcome='pass';")
  claim_ledger <- app$db_query("SELECT * FROM token_ledger WHERE source_type='insurance_claim';")
  stopifnot(nrow(claims) == 1L, nrow(pass_events) == 2L, nrow(claim_ledger) == 1L,
            claim_ledger$amount[1] == 2, claim_ledger$earning[1] == 0L)

  # Closing sales rejects a new purchase.
  shiny::testServer(app$server, {
    set_admin(rv)
    session$setInputs(close_insurance_sales_btn = 1)
    session$flushReact()
  })
  buy_policy("s3@test.edu", "Student 3")
  stopifnot(nrow(app$db_query("SELECT * FROM insurance_purchases;")) == 2L)

  # An unused purchase remains on hand after sales close for its period.
  shiny::testServer(app$server, {
    set_student(rv, "s2@test.edu", "Student 2")
    session$setInputs(open_insurance = 1L)
    stopifnot(grepl("Passes on hand for this class period: 1", output$spend_form$html, fixed = TRUE))
  })
  DBI::dbExecute(app$get_con(), "INSERT INTO weekly_rounds(label,class_date,assignment_mode,bidding_enabled)
     VALUES('Next class','2026-11-11','wage_bidding',1);")
  next_round <- app$db_query("SELECT last_insert_rowid() id;")$id[1]
  shiny::testServer(app$server, {
    set_admin(rv)
    session$setInputs(active_round_select = next_round)
    stopifnot(active_round_id() == next_round)
    rv$cold_call_draw <- list(user_id = "s2@test.edu", display_name = "Student 2", section = "01")
    session$setInputs(slide_cold_call_pass_btn = 1L)
    later_pass <- app$db_query("SELECT tokens FROM live_score_events WHERE round_id=? AND outcome='pass';",
                                list(next_round))
    stopifnot(nrow(later_pass) == 1L, later_pass$tokens[1] == 0,
              nrow(app$db_query("SELECT id FROM insurance_claims;")) == 1L)
  })
  shiny::testServer(app$server, {
    set_student(rv, "s2@test.edu", "Student 2")
    session$setInputs(open_insurance = 1L)
    stopifnot(grepl("No pass is offered", output$spend_form$html, fixed = TRUE),
              grepl("Passes on hand for the selected class period: 0", output$account_tab$html, fixed = TRUE))
  })

  # Settlement rolls back all dividends on failure, then succeeds once.
  shiny::testServer(app$server, {
    set_admin(rv)
    session$setInputs(active_round_select = round_id)
    DBI::dbExecute(app$get_con(), "CREATE TRIGGER reject_insurance_dividend BEFORE INSERT ON token_ledger
      WHEN NEW.source_type='insurance_dividend'
        AND EXISTS (SELECT 1 FROM token_ledger WHERE source_type='insurance_dividend')
      BEGIN SELECT RAISE(ABORT, 'test dividend failure'); END;")
    session$setInputs(settle_insurance_market_btn = 1L)
    stopifnot(app$db_query("SELECT status FROM insurance_products WHERE id=?;", list(product_id))$status[1] == "closed",
              nrow(app$db_query("SELECT id FROM insurance_dividends;")) == 0L,
              nrow(app$db_query("SELECT id FROM token_ledger WHERE source_type='insurance_dividend';")) == 0L)
    DBI::dbExecute(app$get_con(), "DROP TRIGGER reject_insurance_dividend;")
    session$setInputs(settle_insurance_market_btn = 2L)
    session$setInputs(settle_insurance_market_btn = 3L)
    rv$cold_call_draw <- list(user_id = "s2@test.edu", display_name = "Student 2", section = "01")
    session$setInputs(cold_call_pass_btn = 1L)
    stopifnot(nrow(app$db_query("SELECT id FROM insurance_claims;")) == 1L)
  })
  settled <- app$db_query("SELECT * FROM insurance_products WHERE id=?;", list(product_id))
  dividends <- app$db_query("SELECT * FROM insurance_dividends WHERE product_id=? ORDER BY user_id;", list(product_id))
  dividend_ledger <- app$db_query("SELECT * FROM token_ledger WHERE source_type='insurance_dividend';")
  stopifnot(settled$status[1] == "settled", settled$policies_sold[1] == 2L,
            settled$claims_paid_count[1] == 1L, settled$realized_profit[1] == 4,
            settled$claim_reserve[1] == 0, abs(settled$net_profit[1]) < 1e-8,
            settled$dividend_recipients[1] == 3L, nrow(dividends) == 3L,
            nrow(dividend_ledger) == 3L, !"outsider@test.edu" %in% dividends$user_id,
            all(dividend_ledger$earning == 0L),
            abs(sum(dividend_ledger$amount) - 4) < 1e-8)
  shiny::testServer(app$server, {
    set_student(rv, "s2@test.edu", "Student 2")
    session$setInputs(open_insurance = 1L)
    stopifnot(grepl("Coverage has ended", output$spend_form$html, fixed = TRUE),
              grepl("Passes on hand for the selected class period: 0", output$account_tab$html, fixed = TRUE))
  })
  all_insurance_cash <- app$db_query(
    "SELECT COALESCE(SUM(amount),0) x FROM token_ledger WHERE source_type LIKE 'insurance_%';")$x[1]
  stopifnot(abs(all_insurance_cash) < 1e-8)
}

cat("critical insurance market: PASS\n")
