#!/usr/bin/env Rscript
# Critical contract: Class Job Market controls Bonus Pot; the module only plays it.

app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
source_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")
required <- c(
  'label = "Bonus Pot",     embedded = FALSE, url = "/coordination-games/"',
  'actionButton("bp_admin_open", "Open"',
  'actionButton("bp_admin_close", "Close & debit contributions"',
  'actionButton("bp_admin_reveal", "Reveal"',
  'actionButton("bp_admin_payout", "Apply payouts"',
  "current_game='bonus'",
  "coordination_grant",
  "WHERE round=? AND game='bonus'"
)
missing <- required[!vapply(required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(missing)) stop("Bonus Pot command-center contract missing: ", paste(missing, collapse=" | "))
active_marker <- "output$active_slot_inner <- renderUI({"
active_parts <- strsplit(source_text, active_marker, fixed=TRUE)[[1]]
if (length(active_parts) != 2L) stop("Could not isolate Games & Demos active slot.")
active_fragment <- strsplit(active_parts[2], "# ── Embedded: Prisoner", fixed=TRUE)[[1]][1]
if (grepl("bonus_pot_admin_controls", active_fragment, fixed=TRUE)) {
  stop("Games & Demos must not render Bonus Pot setup controls.")
}
settings_marker <- "} else if (act == \"game_controls\") {"
settings_parts <- strsplit(source_text, settings_marker, fixed=TRUE)[[1]]
if (length(settings_parts) != 2L || !grepl("uiOutput(\"bonus_pot_admin_controls\")", settings_parts[2], fixed=TRUE)) {
  stop("Settings must render the Bonus Pot setup controls.")
}
scope_required <- c(
  "pin_admin_session_scope <- function()",
  "pin_admin_session_scope()",
  "Game activation and setup are managed by the course instructor.",
  "req(rv$is_admin, is_super_admin(), !rv$impersonating)"
)
scope_missing <- scope_required[!vapply(scope_required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(scope_missing)) stop("Scoped-admin isolation contract missing: ", paste(scope_missing, collapse=" | "))

module_path <- if (file.exists("apps/coordination-games/app.R")) "apps/coordination-games/app.R" else "/srv/shiny-server/coordination-games/app.R"
module_text <- paste(readLines(module_path, warn=FALSE), collapse="\n")
module_required <- c("lookup_shared_session", "redirect_to_job_market", "Bonus Pot is controlled from Class Job Market", "ALLOW_LEGACY_COORDINATION_ADMIN")
module_missing <- module_required[!vapply(module_required, grepl, logical(1), x=module_text, fixed=TRUE)]
if (length(module_missing)) stop("Bonus Pot module boundary missing: ", paste(module_missing, collapse=" | "))

suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
con <- dbConnect(SQLite(), ":memory:")
on.exit(dbDisconnect(con), add=TRUE)
dbExecute(con, "CREATE TABLE olig_submissions(round INTEGER,user_id TEXT,section TEXT,choice TEXT,contribute REAL,PRIMARY KEY(round,user_id))")
dbExecute(con, "ALTER TABLE olig_submissions ADD COLUMN game TEXT DEFAULT 'bonus'")
dbExecute(con, "ALTER TABLE olig_submissions ADD COLUMN display_name TEXT")
dbExecute(con, "ALTER TABLE olig_submissions ADD COLUMN action TEXT")
dbExecute(con, "INSERT INTO olig_submissions(round,user_id,section,game,choice,contribute) VALUES(1,'student-a','Econ 342-01','bonus','contribute',3) ON CONFLICT(round,user_id) DO UPDATE SET game=excluded.game, choice=excluded.choice, contribute=excluded.contribute, section=excluded.section")
row <- dbGetQuery(con, "SELECT game,choice,contribute FROM olig_submissions WHERE round=1 AND user_id='student-a'")
if (!identical(as.character(row$game), "bonus") || !identical(as.character(row$choice), "contribute") || !identical(as.numeric(row$contribute), 3)) {
  stop("A Class Job Market Bonus Pot contribution is not stored as a Bonus Pot record.")
}
cat("critical Bonus Pot unification fixture: OK\n")
