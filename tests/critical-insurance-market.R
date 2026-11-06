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
  "suggested_next_premium",
  "token_credit(uid, dname, benefit, 0L, \"insurance_claim\"",
  "accounting$dividend_each, 0L,"
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
for (i in seq_len(nrow(students))) {
  dbExecute(con, "INSERT INTO insurance_dividends(product_id,user_id,amount) VALUES(1,?,?);", params = list(students$user_id[i], accounting$dividend_each))
  dividend_id <- dbGetQuery(con, "SELECT last_insert_rowid() id;")$id[1]
  dbExecute(con, "INSERT INTO token_ledger(user_id,display_name,source_type,source_id,amount,earning,note) VALUES(?,?,'insurance_dividend',?,?,0,'dividend');", params = list(students$user_id[i], students$display_name[i], dividend_id, accounting$dividend_each))
}

cashflows <- dbGetQuery(con, "SELECT COALESCE(SUM(amount),0) x FROM token_ledger WHERE source_type LIKE 'insurance_%';")$x[1]
gross_earnings <- dbGetQuery(con, "SELECT COALESCE(SUM(amount),0) x FROM token_ledger WHERE earning=1;")$x[1]
stopifnot(abs(cashflows) < 1e-9, gross_earnings == 20)
stopifnot(insurance_zero_profit_premium(2, 4, step = 0.5) == 0.5)

cat("critical insurance market: PASS\n")
