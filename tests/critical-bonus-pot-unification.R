#!/usr/bin/env Rscript
# Critical contract: Class Job Market controls Bonus Pot; the module only plays it.

app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
source_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")
required <- c(
  'label = "Bonus Pot",     embedded = TRUE, url = "/coordination-games/"',
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
if (!grepl("bonus_pot         = uiOutput(\"embedded_bonus_pot\")", active_fragment, fixed=TRUE)) {
  stop("Active Now must render the embedded Bonus Pot player surface.")
}
if (grepl("bonus_pot_admin_controls", active_fragment, fixed=TRUE) ||
    grepl("Instructor controls", active_fragment, fixed=TRUE)) {
  stop("Active Now must not expose Bonus Pot lifecycle controls.")
}
settings_marker <- "} else if (act == \"game_controls\") {"
settings_parts <- strsplit(source_text, settings_marker, fixed=TRUE)[[1]]
if (length(settings_parts) != 2L || !grepl("uiOutput(\"bonus_pot_admin_controls\")", settings_parts[2], fixed=TRUE)) {
  stop("Settings must expose Bonus Pot lifecycle actions.")
}
if (!grepl("Choose (none) and save to turn the game off.", source_text, fixed=TRUE)) {
  stop("Settings must provide a way to turn off the active game.")
}
scope_required <- c(
  "pin_admin_session_scope <- function()",
  "pin_session_environment()",
  "Game activation and setup are managed by the course instructor.",
  "req(rv$is_admin, is_super_admin(), !rv$impersonating)"
)
scope_missing <- scope_required[!vapply(scope_required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(scope_missing)) stop("Scoped-admin isolation contract missing: ", paste(scope_missing, collapse=" | "))

active_scope_required <- c(
  "CREATE TABLE IF NOT EXISTS scope_arcade_state",
  "active_game_state_for_scope <- function",
  "set_active_game_for_scope <- function",
  "INSERT INTO scope_arcade_state",
  "tab_is_active(\"Today\", \"Games & Demos\", \"Settings\")"
)
active_scope_missing <- active_scope_required[!vapply(active_scope_required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(active_scope_missing)) stop("Environment-scoped active-game contract missing: ", paste(active_scope_missing, collapse=" | "))
if (grepl("UPDATE arcade_state SET active_game", source_text, fixed=TRUE)) stop("Active-game writes must not use global arcade_state.")

module_path <- if (file.exists("apps/coordination-games/app.R")) "apps/coordination-games/app.R" else "/srv/shiny-server/coordination-games/app.R"
module_text <- paste(readLines(module_path, warn=FALSE), collapse="\n")
module_required <- c("lookup_shared_session", "redirect_to_job_market", "Bonus Pot is controlled from Class Job Market", "ALLOW_LEGACY_COORDINATION_ADMIN", "$(document).on('shiny:sessioninitialized', sendAuthCookie);", "setTimeout(sendAuthCookie, 300);")
module_missing <- module_required[!vapply(module_required, grepl, logical(1), x=module_text, fixed=TRUE)]
if (length(module_missing)) stop("Bonus Pot module boundary missing: ", paste(module_missing, collapse=" | "))

suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
con <- dbConnect(SQLite(), ":memory:")
dbExecute(con, "CREATE TABLE scope_arcade_state(scope_key TEXT PRIMARY KEY, active_game TEXT, updated_at TEXT)")
dbExecute(con, "INSERT INTO scope_arcade_state(scope_key,active_game) VALUES('econ-342::01','bonus_pot'),('econ-342::02','price_war')")
first_scope <- dbGetQuery(con, "SELECT active_game FROM scope_arcade_state WHERE scope_key='econ-342::01'")$active_game[1]
second_scope <- dbGetQuery(con, "SELECT active_game FROM scope_arcade_state WHERE scope_key='econ-342::02'")$active_game[1]
if (!identical(as.character(first_scope), "bonus_pot") || !identical(as.character(second_scope), "price_war")) stop("Active games must remain independent by environment scope.")
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
