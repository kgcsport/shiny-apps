#!/usr/bin/env Rscript
# A checkout must record the token debit and purchase atomically. This guards
# against the prior path, which could debit a student then silently lose the
# purchase record (making PS4+ look unlike PS1--PS3 and impossible to refund).
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

con <- dbConnect(SQLite(), tempfile(fileext = ".sqlite"))
on.exit(dbDisconnect(con), add = TRUE)
get_con <- function() con
dbExecute(con, "CREATE TABLE token_ledger(
  id INTEGER PRIMARY KEY AUTOINCREMENT, user_id TEXT, display_name TEXT,
  source_type TEXT, source_id INTEGER, amount REAL, earning INTEGER, note TEXT
);")
dbExecute(con, "CREATE TABLE extension_purchases(
  id INTEGER PRIMARY KEY AUTOINCREMENT, problem_set_id INTEGER, user_id TEXT,
  hours REAL, cost REAL, ledger_id INTEGER, approval_status TEXT
);")

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else "/srv/shiny-server/class-job-market/app.R"
expressions <- parse(app_path)
helper <- new.env(parent = globalenv())
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.symbol(expr[[2]]) && identical(as.character(expr[[2]]), "record_extension_purchase")) {
    eval(expr, helper)
  }
}
stopifnot(exists("record_extension_purchase", helper, inherits = FALSE))

saved <- helper$record_extension_purchase(
  "student@example.edu", "Student", 4L, 24, 8, "approved", "PS4 extension")
stopifnot(saved$purchase_id == 1L, saved$ledger_id == 1L)
ledger <- dbGetQuery(con, "SELECT user_id,source_type,source_id,amount FROM token_ledger;")
purchase <- dbGetQuery(con, "SELECT problem_set_id,user_id,hours,cost,ledger_id,approval_status FROM extension_purchases;")
stopifnot(nrow(ledger) == 1L, nrow(purchase) == 1L,
          ledger$user_id == "student@example.edu", ledger$source_type == "extension",
          ledger$source_id == 4L, ledger$amount == -8,
          purchase$problem_set_id == 4L, purchase$ledger_id == saved$ledger_id)

# The following insert is intentionally rejected. The transaction must undo
# its ledger debit as well; otherwise the student is charged for no purchase.
dbExecute(con, "CREATE TRIGGER reject_problem_set_5 BEFORE INSERT ON extension_purchases
  WHEN NEW.problem_set_id=5 BEGIN SELECT RAISE(ABORT, 'test purchase rejection'); END;")
failed <- try(helper$record_extension_purchase(
  "student@example.edu", "Student", 5L, 24, 8, "approved", "PS5 extension"), silent = TRUE)
stopifnot(inherits(failed, "try-error"),
          nrow(dbGetQuery(con, "SELECT * FROM token_ledger;")) == 1L,
          nrow(dbGetQuery(con, "SELECT * FROM extension_purchases;")) == 1L)
cat("critical extension checkout: OK\n")
