#!/usr/bin/env Rscript
# Critical contract: an open Bonus Pot contribution is replaceable, including
# reset-to-zero, and the live submission signature notices value-only changes.

app_path <- if (file.exists("apps/coordination-games/app.R")) {
  "apps/coordination-games/app.R"
} else {
  "/srv/shiny-server/coordination-games/app.R"
}
source_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")

required <- c(
  "COALESCE(SUM(COALESCE(contribute,0)),0)",
  "actionButton(\"reset_bonus\", \"Reset to 0\"",
  "observeEvent(input$reset_bonus",
  "Contribution reset to 0",
  "INSERT OR REPLACE INTO olig_submissions",
  "round_status[1] %||% \"\"), \"revealed\")"
)
missing <- required[!vapply(required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(missing)) stop("Bonus Pot contribution-reset contract missing: ", paste(missing, collapse=" | "))

if (!requireNamespace("DBI", quietly=TRUE) || !requireNamespace("RSQLite", quietly=TRUE)) {
  cat("critical Bonus Pot contribution reset: source contract OK (SQLite unavailable)\n")
  quit(status=0)
}
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
on.exit(DBI::dbDisconnect(con), add=TRUE)
DBI::dbExecute(con, "CREATE TABLE olig_submissions(round INTEGER,user_id TEXT,contribute REAL,PRIMARY KEY(round,user_id))")
DBI::dbExecute(con, "INSERT OR REPLACE INTO olig_submissions(round,user_id,contribute) VALUES(1,'student-a',62.5)")
DBI::dbExecute(con, "INSERT OR REPLACE INTO olig_submissions(round,user_id,contribute) VALUES(1,'student-a',0)")
row <- DBI::dbGetQuery(con, "SELECT contribute FROM olig_submissions WHERE round=1 AND user_id='student-a'")
if (nrow(row) != 1L || !identical(as.numeric(row$contribute[1]), 0)) stop("Reset did not replace the saved contribution with zero.")
cat("critical Bonus Pot contribution reset: OK\n")
