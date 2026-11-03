#!/usr/bin/env Rscript

# Regression: Either/or cards must remain direct entry points, while a live
# class round still renders in the Active Now slot. Price War must have its own
# route and use the shared player in Price War mode.
app_root <- if (file.exists("apps/class-job-market/app.R")) "apps" else "/srv/shiny-server"
hub_path <- file.path(app_root, "class-job-market", "app.R")
hub <- paste(readLines(hub_path, warn=FALSE), collapse="\n")
price_path <- file.path(app_root, "price-war", "app.R")
bonus_path <- file.path(app_root, "bonus-pot", "app.R")

if (!file.exists(price_path)) stop("Price War must have its own deployable app route.")
price <- paste(readLines(price_path, warn=FALSE), collapse="\n")
bonus <- paste(readLines(bonus_path, warn=FALSE), collapse="\n")

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

if (!grepl('Sys.setenv(COORDINATION_GAME_MODE = "price_war")', price, fixed=TRUE) ||
    !grepl('"..", "bonus-pot", "app.R"', price, fixed=TRUE)) stop("Price War route must load the shared player in Price War mode.")
for (needle in c('GAME_ID <- if (identical(GAME_MODE, "price_war")) "price_war" else "bonus_pot"',
                 'GAME_ROUTE <- if (identical(GAME_MODE, "price_war")) "/price-war/" else "/bonus-pot/"',
                 'identical(as.character(state$active_game[1]), GAME_ID)',
                 'setNames(GAME_DB_VALUE, GAME_TITLE)')) {
  if (!grepl(needle, bonus, fixed=TRUE)) stop("Shared player is missing route isolation: ", needle)
}

cat("critical-game-navigation: PASS\n")
