#!/usr/bin/env Rscript
# The student Spend workflow needs one independently purchasable product for the
# original submission and one for corrections. This runs the startup migration
# against a scratch SQLite database, including rerun/idempotence behavior.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
con <- dbConnect(SQLite(), tempfile(fileext = ".sqlite"))
on.exit(dbDisconnect(con), add = TRUE)
db_query <- function(sql, params = NULL) {
  if (is.null(params)) dbGetQuery(con, sql) else dbGetQuery(con, sql, params = params)
}
db_exec <- function(sql, params = NULL) {
  if (is.null(params)) dbExecute(con, sql) else dbExecute(con, sql, params = params)
}
db_exec("CREATE TABLE problem_sets(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  original_deadline TEXT,
  solutions_posted_at TEXT,
  active INTEGER DEFAULT 1,
  cloudflare_assignment_id TEXT,
  extension_target TEXT DEFAULT 'submission'
);")

app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
app <- parse(app_path)
needed <- c("default_extension_worker_assignments", "problem_set_number", "ensure_default_extension_products")
loaded <- character()
for (expr in app) {
  if (is.call(expr) && identical(as.character(expr[[1]]), "<-") && is.name(expr[[2]]) &&
      as.character(expr[[2]]) %in% needed) {
    eval(expr, envir = globalenv())
    loaded <- c(loaded, as.character(expr[[2]]))
  }
}
stopifnot(setequal(loaded, needed))

# Preserve existing instructor data while deriving its companion corrections
# product from the solutions date and the same Worker assignment ID.
db_exec("INSERT INTO problem_sets(name,original_deadline,solutions_posted_at,active,cloudflare_assignment_id,extension_target)
         VALUES('PS4','2026-11-01','2026-11-04',1,'econ342-2026-ps04','submission');")
ensure_default_extension_products()
first <- db_query("SELECT name,original_deadline,solutions_posted_at,active,cloudflare_assignment_id,extension_target
                   FROM problem_sets ORDER BY id;")
stopifnot(nrow(first) == 6L)
for (number in 4:6) {
  rows <- first[vapply(first$name, problem_set_number, integer(1)) == number, , drop = FALSE]
  stopifnot(nrow(rows) == 2L)
  stopifnot(setequal(rows$extension_target, c("submission", "self_grading")))
  stopifnot(all(rows$cloudflare_assignment_id == sprintf("econ342-2026-ps%02d", number)))
}
ps4 <- first[vapply(first$name, problem_set_number, integer(1)) == 4L, , drop = FALSE]
stopifnot(ps4$original_deadline[ps4$extension_target == "submission"] == "2026-11-01")
stopifnot(ps4$original_deadline[ps4$extension_target == "self_grading"] == "2026-11-04")
stopifnot(all(first$active[vapply(first$name, problem_set_number, integer(1)) %in% 4:5] == 1L))
stopifnot(all(first$active[vapply(first$name, problem_set_number, integer(1)) == 6L] == 0L))

ensure_default_extension_products()
second <- db_query("SELECT id FROM problem_sets;")
stopifnot(nrow(second) == 6L)
cat("critical default extension products: OK\n")
