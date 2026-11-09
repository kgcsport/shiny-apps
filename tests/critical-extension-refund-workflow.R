#!/usr/bin/env Rscript
# Exercise the exact claim/finalize state transitions used by student and
# instructor extension refunds. A requested refund must be claimable, and a
# claimed refund must not remain stuck in processing after token credit.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
con <- dbConnect(SQLite(), tempfile(fileext = ".sqlite"))
on.exit(dbDisconnect(con), add = TRUE)
db_exec <- function(sql, params = NULL) {
  if (is.null(params)) dbExecute(con, sql) else dbExecute(con, sql, params = params)
}
db_exec("CREATE TABLE extension_purchases(
  id INTEGER PRIMARY KEY, user_id TEXT, refund_status TEXT,
  refund_error TEXT, approval_status TEXT, refunded_at TEXT,
  refund_ledger_id INTEGER, sync_status TEXT
);")
db_exec("CREATE TABLE token_ledger(
  id INTEGER PRIMARY KEY, user_id TEXT, source_type TEXT, source_id INTEGER
);")

app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
app <- parse(app_path)
needed <- c("extension_refund_claim", "extension_refund_finalize", "recover_completed_extension_refunds")
loaded <- character()
for (expr in app) {
  if (is.call(expr) && identical(as.character(expr[[1]]), "<-") && is.name(expr[[2]]) &&
      as.character(expr[[2]]) %in% needed) {
    eval(expr, envir = globalenv())
    loaded <- c(loaded, as.character(expr[[2]]))
  }
}
stopifnot(setequal(loaded, needed))

# A student's after-deadline request can be approved by the instructor.
db_exec("INSERT INTO extension_purchases VALUES(1,'student-1','requested','', 'approved',NULL,NULL,'synced');")
stopifnot(extension_refund_claim(1L, eligible_statuses=c("not_refunded", "requested", "failed")) == 1L)
stopifnot(dbGetQuery(con, "SELECT refund_status FROM extension_purchases WHERE id=1;")$refund_status == "processing")
stopifnot(extension_refund_finalize(1L, 101L) == 1L)
row <- dbGetQuery(con, "SELECT refund_status,refund_ledger_id,sync_status FROM extension_purchases WHERE id=1;")
stopifnot(row$refund_status == "refunded", row$refund_ledger_id == 101L, row$sync_status == "refunded")

# The ordinary pre-deadline path is restricted to its owner but finalizes from
# processing rather than mistakenly looking for the old not_refunded state.
db_exec("INSERT INTO extension_purchases VALUES(2,'student-2','not_refunded','', 'approved',NULL,NULL,'pending');")
stopifnot(extension_refund_claim(2L, "student-2") == 1L)
stopifnot(extension_refund_claim(2L, "student-2") == 0L)
stopifnot(extension_refund_finalize(2L, 102L) == 1L)
row <- dbGetQuery(con, "SELECT refund_status,refund_ledger_id,sync_status FROM extension_purchases WHERE id=2;")
stopifnot(row$refund_status == "refunded", row$refund_ledger_id == 102L, row$sync_status == "pending")

# The previous production bug left an already credited refund in processing.
# Startup repairs this one record without touching an active, uncredited claim.
db_exec("INSERT INTO extension_purchases VALUES(3,'student-3','processing','', 'approved',NULL,NULL,'synced');")
db_exec("INSERT INTO extension_purchases VALUES(4,'student-4','processing','', 'approved',NULL,NULL,'synced');")
db_exec("INSERT INTO token_ledger VALUES(103,'student-3','extension_refund',3);")
recover_completed_extension_refunds()
rows <- dbGetQuery(con, "SELECT id,refund_status,refund_ledger_id,sync_status FROM extension_purchases WHERE id IN (3,4) ORDER BY id;")
stopifnot(rows$refund_status[1] == "refunded", rows$refund_ledger_id[1] == 103L, rows$sync_status[1] == "refunded")
stopifnot(rows$refund_status[2] == "processing", is.na(rows$refund_ledger_id[2]))
cat("critical extension refund workflow: OK\n")
