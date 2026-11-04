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
  if (grepl("token_ledger", sql, fixed=TRUE) && grepl("round_id", sql, fixed=TRUE)) {
    stop("Standalone Bonus Pot must use shared token_ledger.source_id, not a nonexistent round_id column.")
  }
  if (grepl("coordination_grant", sql, fixed=TRUE)) ledger[[length(ledger)+1L]] <<- list(user_id=params[[4]], amount=params[[2]])
  invisible(1L)
}

app_root <- if (file.exists("apps/bonus-pot/app.R")) "apps" else "/srv/shiny-server"
app_text <- paste(readLines(file.path(app_root, "bonus-pot", "app.R"), warn=FALSE), collapse="\n")
app <- parse(file.path(app_root, "bonus-pot", "app.R"))
for (ex in app) {
  if (is.call(ex) && identical(as.character(ex[[1]]), "<-") && is.name(ex[[2]]) && as.character(ex[[2]]) == "bonus_shares") eval(ex, envir=globalenv())
  if (is.call(ex) && identical(as.character(ex[[1]]), "<-") && is.name(ex[[2]]) && as.character(ex[[2]]) == "apply_payouts") eval(ex, envir=globalenv())
  if (is.call(ex) && identical(as.character(ex[[1]]), "<-") && is.name(ex[[2]]) && as.character(ex[[2]]) == "club_choice_results") eval(ex, envir=globalenv())
}
result <- apply_payouts(1L, "bonus")
stopifnot(result$ok, length(ledger) == 3L)
stopifnot(identical(sort(vapply(ledger, `[[`, character(1), "user_id")), sort(roster$user_id)))
stopifnot(all(vapply(ledger, `[[`, numeric(1), "amount") == 1))

# Threshold mode is opt-in: only submitters at or above the chosen threshold
# receive a share, while all submitted contributions still build the pot.
olig <- data.frame(bonus_multiplier=1.5, use_section_size=1L,
                   denominator_mode="threshold", denominator_threshold=2,
                   section="S01")
subs <- tibble(round=2L, user_id=c("high", "low", "zero"), section="S01",
               game="bonus", contribute=c(4, 1, 0))
ledger <- list()
result <- apply_payouts(2L, "bonus")
stopifnot(result$ok, length(ledger) == 1L)
stopifnot(identical(ledger[[1]]$user_id, "high"), identical(ledger[[1]]$amount, 7.5))

# The threshold must not affect the established enrolled-section mode.
olig$denominator_mode <- "section"
olig$denominator_threshold <- 99
shares <- bonus_shares(olig, subs)
stopifnot(identical(shares$n_denom[1], 3L), identical(shares$share_each[1], 2.5))

# Revealed Club Choice results are computed from all submissions at once for
# the projector. This must work for multiple entries, including an exit.
club_snapshot <- data.frame(open_multiplier=1.5, commitment_multiplier=2,
                            stringsAsFactors=FALSE)
club_subs <- tibble(club_choice=c("open", "commitment", "exit"),
                    contribute=c(2, 4, 0), entry_cost=c(0, 3, 0))
club_results <- club_choice_results(club_snapshot, club_subs)
stopifnot(identical(club_results$members, c(1L, 1L)))
stopifnot(identical(club_results$total_contributed, c(2, 7)))
stopifnot(identical(club_results$return_per_member, c(3, 14)))
cjm <- paste(readLines(file.path(app_root, "class-job-market", "app.R"), warn=FALSE), collapse="\n")
stopifnot(!grepl("token_ledger(user_id, display_name, round_id", app_text, fixed=TRUE))
stopifnot(grepl("for (uid in as.character(recipients$user_id))", cjm, fixed=TRUE))
stopifnot(grepl("\"submits at least\"=\"threshold\"", cjm, fixed=TRUE))
stopifnot(grepl("denominator_mode=?,denominator_threshold=?", cjm, fixed=TRUE))
stopifnot(grepl("setNames(c(\"section\", \"submitters\", \"threshold\")", app_text, fixed=TRUE))
stopifnot(!grepl("c(cs_radio_lbl = \"section\"", app_text, fixed=TRUE))
# Regression: save must authorize the exact course/section selected by the
# instructor rather than a hidden previously active environment.
stopifnot(grepl('selectInput("adm_course", "Active Course"' , app_text, fixed=TRUE))
stopifnot(grepl('observeEvent(input$adm_course' , app_text, fixed=TRUE))
stopifnot(grepl('course <- as.character(input$adm_course %||% rv$active_course)' , app_text, fixed=TRUE))
stopifnot(grepl('norm_key(allowed_env$course) == norm_key(course)' , app_text, fixed=TRUE))
stopifnot(grepl('switch_bonus_environment(course, sec)' , app_text, fixed=TRUE))
stopifnot(!grepl('norm_key(allowed_env$course) == norm_key(rv$active_course) & norm_key(allowed_env$section)' , app_text, fixed=TRUE))
# Price War is standalone. Bonus Pot settings must not expose or save its
# retired scale/payoff matrix controls.
stopifnot(!grepl('numericInput("adm_pd_scale"', app_text, fixed=TRUE))
stopifnot(!grepl('h5("PD payoff matrix (A, B)")', app_text, fixed=TRUE))
stopifnot(!grepl('input$adm_pd_scale', app_text, fixed=TRUE))
cat("Bonus Pot public-good payout: OK\n")
