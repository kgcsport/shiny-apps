#!/usr/bin/env Rscript
# Critical contract: student-controlled contribution fields accept hundredths,
# so a fractional wallet balance can be spent without leaving unusable dust.

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else {
  "/srv/shiny-server/class-job-market/app.R"
}
bonus_path <- if (file.exists("apps/bonus-pot/app.R")) {
  "apps/bonus-pot/app.R"
} else {
  "/srv/shiny-server/bonus-pot/app.R"
}

app_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")
bonus_text <- paste(readLines(bonus_path, warn=FALSE), collapse="\n")

app_required <- c(
  "TOKEN_SPEND_STEP <- 0.01",
  "step=TOKEN_SPEND_STEP",
  "requested <- suppressWarnings(round_token_spend(input$flex_contribution",
  "amount <- clamp_token_spend(requested, bal, remaining)",
  "fmt_token_amount(max(0, target-new_total))"
)
bonus_required <- c(
  "round_token_cents <- function(x)",
  'numericInput("bonus_c", "Contribution (participation tokens)"',
  "step = 0.01",
  "c_val <- round_token_cents(input$bonus_c",
  "contribution <- round_token_cents(input$club_contribution",
  "Contribution = fmt_token_cents",
  "Contribution=fmt_token_cents",
  'updateNumericInput(session, "bonus_c", value=0)'
)
missing_app <- app_required[!vapply(app_required, grepl, logical(1), x=app_text, fixed=TRUE)]
missing_bonus <- bonus_required[!vapply(bonus_required, grepl, logical(1), x=bonus_text, fixed=TRUE)]
if (length(missing_app)) stop("Class Job Market cent-spending contract missing: ", paste(missing_app, collapse=" | "))
if (length(missing_bonus)) stop("Bonus Pot cent-spending contract missing: ", paste(missing_bonus, collapse=" | "))
if (grepl('sliderInput("bonus_c"', bonus_text, fixed=TRUE)) stop("Bonus Pot must allow an exact typed contribution, not force a coarse slider.")

extract_assignments <- function(path, names) {
  expressions <- parse(file=path)
  env <- new.env(parent=baseenv())
  for (expr in expressions) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% names) eval(expr, env)
  }
  env
}

app_env <- extract_assignments(app_path, c("TOKEN_SPEND_STEP", "round_token_spend", "clamp_token_spend"))
bonus_env <- extract_assignments(bonus_path, c("round_token_cents", "fmt_token_cents"))
if (!isTRUE(all.equal(app_env$clamp_token_spend(0.37, 0.37, Inf), 0.37))) stop("A 0.37 balance cannot be spent in full.")
if (!isTRUE(all.equal(app_env$clamp_token_spend(1, 0.37, Inf), 0.37))) stop("Spending is not capped at the exact fractional balance.")
if (!isTRUE(all.equal(app_env$clamp_token_spend(1, 10, 0.23), 0.23))) stop("Flex funding is not capped at the exact fractional remainder.")
if (!isTRUE(all.equal(bonus_env$round_token_cents(0.369), 0.37))) stop("Bonus Pot contribution precision is not one hundredth.")
if (!identical(bonus_env$fmt_token_cents(0.37), "0.37")) stop("Fractional Bonus Pot balances are not displayed exactly.")

cat("critical token spend precision: OK\n")
