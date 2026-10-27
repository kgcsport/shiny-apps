#!/usr/bin/env Rscript
# Contract: an enrolled zero-contributor receives the same public-good share.
suppressPackageStartupMessages({ library(dplyr); library(tibble) })

`%||%` <- function(a, b) if (!is.null(a) && length(a) && !is.na(a[1])) a else b
round_to_half <- function(x) round(x * 2) / 2
olig <- data.frame(bonus_multiplier=1.5, use_section_size=1L, section="S01")
get_olig <- function() olig
olig_section <- function(x) as.character(x$section[1])
section_class_size <- function(section) 3L
touch_olig <- function() invisible(NULL)
subs <- tibble(round=1L, user_id="contributor", section="S01", game="bonus", contribute=2)
roster <- tibble(user_id=c("contributor", "zero-a", "zero-b"))
ledger <- list()
db_query <- function(sql, params=NULL) {
  if (grepl("FROM olig_submissions", sql, fixed=TRUE)) return(subs)
  if (grepl("SELECT user_id FROM users", sql, fixed=TRUE)) return(roster)
  data.frame()
}
db_exec <- function(sql, params=NULL) {
  if (grepl("coordination_grant", sql, fixed=TRUE)) ledger[[length(ledger)+1L]] <<- list(user_id=params[[4]], amount=params[[2]])
  invisible(1L)
}

app_root <- if (file.exists("apps/coordination-games/app.R")) "apps" else "/srv/shiny-server"
app <- parse(file.path(app_root, "coordination-games", "app.R"))
for (ex in app) {
  if (is.call(ex) && identical(as.character(ex[[1]]), "<-") && is.name(ex[[2]]) && as.character(ex[[2]]) == "bonus_shares") eval(ex, envir=globalenv())
  if (is.call(ex) && identical(as.character(ex[[1]]), "<-") && is.name(ex[[2]]) && as.character(ex[[2]]) == "apply_payouts") eval(ex, envir=globalenv())
}
result <- apply_payouts(1L, "bonus")
stopifnot(result$ok, length(ledger) == 3L)
stopifnot(identical(sort(vapply(ledger, `[[`, character(1), "user_id")), sort(roster$user_id)))
stopifnot(all(vapply(ledger, `[[`, numeric(1), "amount") == 1))
cjm <- paste(readLines(file.path(app_root, "class-job-market", "app.R"), warn=FALSE), collapse="\n")
stopifnot(grepl("for (uid in as.character(recipients$user_id))", cjm, fixed=TRUE))
cat("Bonus Pot public-good payout: OK\n")
