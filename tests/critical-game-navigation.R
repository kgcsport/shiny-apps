#!/usr/bin/env Rscript

# Regression: Either/or cards must remain direct entry points, while a live
# class round still renders in the Active Now slot. Price War must have its own
# route and own persistence, not Bonus Pot's coordination tables.
app_root <- if (file.exists("apps/class-job-market/app.R")) "apps" else "/srv/shiny-server"
hub_path <- file.path(app_root, "class-job-market", "app.R")
hub <- paste(readLines(hub_path, warn=FALSE), collapse="\n")
price_path <- file.path(app_root, "price-war", "app.R")

if (!file.exists(price_path)) stop("Price War must have its own deployable app route.")
price <- paste(readLines(price_path, warn=FALSE), collapse="\n")
source(price_path, local=TRUE)

required_hub <- c(
  'label = "Bonus Pot",     embedded = TRUE, url = "/bonus-pot/"',
  'label = "Price War",     embedded = TRUE, url = "/price-war/"',
  'identical(g$type, "either") && !is.null(g$url)',
  'tags$iframe(src="/price-war/", title="Price War"',
  "if (targetPath === '/price-war') return '/price-war/';",
  'if (target %in% c("/price-war", "/price-war/")) return("/price-war/")'
)
missing_hub <- required_hub[!vapply(required_hub, grepl, logical(1), x=hub, fixed=TRUE)]
if (length(missing_hub)) stop("Games navigation is missing: ", paste(missing_hub, collapse=", "))
if (grepl('list(id = "prisoners_dilemma"', hub, fixed=TRUE)) stop("Price War must replace the duplicate Prisoner's Dilemma catalog entry.")

for (needle in c("CREATE TABLE IF NOT EXISTS price_war_rounds", "CREATE TABLE IF NOT EXISTS price_war_submissions", "CREATE TABLE IF NOT EXISTS price_war_payouts", "PRIMARY KEY(course,section,round)", "price_war_payout")) {
  if (!grepl(needle, price, fixed=TRUE)) stop("Price War standalone persistence is missing: ", needle)
}
for (needle in c("if (length(p)) dbGetQuery(db(), sql, params=p) else dbGetQuery(db(), sql)", "if (length(p)) dbExecute(db(), sql, params=p) else dbExecute(db(), sql)")) {
  if (!grepl(needle, price, fixed=TRUE)) stop("Price War startup helper must omit empty SQLite parameter lists.")
}
for (needle in c("Shiny.setInputValue('auth_cookie'", "observeEvent(input$auth_cookie", "arcade_sessions s JOIN users u", "issue_shared_cookie <- function", "set_arcade_cookie", "actionButton(\"login_btn\"", "req(rv$admin,nzchar(rv$course),nzchar(rv$section))")) {
  if (!grepl(needle, price, fixed=TRUE)) stop("Price War must use the existing Class Job Market session and selected environment: ", needle)
}
for (needle in c("pair_code_counts <- function(actions)", "levels=c(\"HH\", \"HL\", \"LH\", \"LL\")", "tabPanel(\"Play\"", "tabPanel(\"Results\"", "if(rv$admin) tabs <- c(tabs,list(tabPanel(\"Settings\"", "output$play_ui <- renderUI", "output$results_ui <- renderUI", "output$settings_ui <- renderUI", "game_poll <- reactivePoll", "submission_poll <- reactivePoll", "pw-header", "environment_course_sel", "environment_section_sel")) {
  if (!grepl(needle, price, fixed=TRUE)) stop("Price War must provide student play, aggregate results, and an admin-only Settings tab: ", needle)
}
expected_pairs <- data.frame(pair=c("HH", "HL", "LH", "LL"), count=c(1L, 1L, 1L, 1L))
if (!identical(pair_code_counts(c("High", "High", "High", "Low", "Low", "High", "Low", "Low", "High")), expected_pairs)) {
  stop("Price War pair counts must group completed submissions as HH, HL, LH, and LL.")
}
if (grepl("choose_env(input$course,input$section)", price, fixed=TRUE)) stop("Price War setup must not reference nonexistent course/section inputs.")
if (grepl("olig_settings", price, fixed=TRUE) || grepl("olig_submissions", price, fixed=TRUE) || grepl("olig_payouts", price, fixed=TRUE)) stop("Price War must not use Bonus Pot persistence.")
if (grepl("current_game='pd'", hub, fixed=TRUE)) stop("Activating Price War must not alter Bonus Pot settings.")

cat("critical-game-navigation: PASS\n")
