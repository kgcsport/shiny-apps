round_tokens <- function(x) round(as.numeric(x) + 1e-9, 2)

insurance_loss <- function(balance, loss_type, loss_value) {
  balance <- suppressWarnings(as.numeric(balance))
  loss_value <- suppressWarnings(as.numeric(loss_value))
  if (!is.finite(balance) || balance <= 0 || !is.finite(loss_value) || loss_value < 0)
    return(0)
  raw <- if (identical(loss_type, "percent")) balance * loss_value else loss_value
  round_tokens(min(balance, max(0, raw)))
}

insurance_premium <- function(loss, premium_rate) {
  loss <- suppressWarnings(as.numeric(loss))
  premium_rate <- suppressWarnings(as.numeric(premium_rate))
  if (!is.finite(loss) || loss < 0 || !is.finite(premium_rate) || premium_rate < 0)
    return(NA_real_)
  round_tokens(loss * premium_rate)
}

insurance_purchase <- function(max_premium, posted_premium) {
  max_premium <- suppressWarnings(as.numeric(max_premium))
  posted_premium <- suppressWarnings(as.numeric(posted_premium))
  is.finite(max_premium) && is.finite(posted_premium) &&
    max_premium + 0.005 >= posted_premium
}

insurance_outcome <- function(starting_balance, loss, premium, purchased, shocked) {
  premium_debit <- if (isTRUE(as.logical(purchased))) as.numeric(premium) else 0
  loss_debit <- if (isTRUE(as.logical(shocked))) as.numeric(loss) else 0
  indemnity_credit <- if (isTRUE(as.logical(purchased)) && isTRUE(as.logical(shocked))) as.numeric(loss) else 0
  net_change <- round_tokens(-premium_debit - loss_debit + indemnity_credit)
  list(premium_debit=round_tokens(premium_debit), loss_debit=round_tokens(loss_debit),
       indemnity_credit=round_tokens(indemnity_credit), net_change=net_change,
       end_balance=round_tokens(as.numeric(starting_balance) + net_change))
}

endowment_band <- function(balance) {
  cut(as.numeric(balance), breaks=c(-Inf, 10, 20, 40, 60, Inf), right=FALSE,
      labels=c("Under 10", "10–19.99", "20–39.99", "40–59.99", "60+"))
}

log_sum_exp_two <- function(log_a, log_b) {
  top <- max(log_a, log_b)
  top + log(exp(log_a - top) + exp(log_b - top))
}

cara_wtp <- function(alpha, probability, loss) {
  alpha <- as.numeric(alpha); probability <- as.numeric(probability); loss <- as.numeric(loss)
  if (!is.finite(alpha) || !is.finite(probability) || !is.finite(loss) ||
      probability <= 0 || probability >= 1 || loss <= 0) return(NA_real_)
  if (abs(alpha) < 1e-8) return(probability * loss)
  log_mix <- log_sum_exp_two(log1p(-probability), log(probability) + alpha * loss)
  log_mix / alpha
}

implied_cara <- function(max_premium, probability, loss) {
  wtp <- as.numeric(max_premium); probability <- as.numeric(probability); loss <- as.numeric(loss)
  if (!is.finite(wtp) || !is.finite(probability) || !is.finite(loss) ||
      probability <= 0 || probability >= 1 || loss <= 0 || wtp < 0 || wtp >= loss)
    return(NA_real_)
  fair <- probability * loss
  if (abs(wtp - fair) < 0.005) return(0)
  bound <- 50 / loss
  f <- function(alpha) cara_wtp(alpha, probability, loss) - wtp
  tryCatch(uniroot(f, c(-bound, bound), tol=1e-9)$root, error=function(e) NA_real_)
}

crra_certainty_equivalent <- function(gamma, probability, wealth, loss) {
  gamma <- as.numeric(gamma); probability <- as.numeric(probability)
  wealth <- as.numeric(wealth); loss <- as.numeric(loss)
  bad <- wealth - loss
  if (any(!is.finite(c(gamma, probability, wealth, loss))) || probability <= 0 ||
      probability >= 1 || wealth <= 0 || loss <= 0 || bad <= 0) return(NA_real_)
  if (abs(gamma - 1) < 1e-8)
    return(exp((1-probability) * log(wealth) + probability * log(bad)))
  power <- 1 - gamma
  log_expected_power <- log_sum_exp_two(
    log1p(-probability) + power * log(wealth),
    log(probability) + power * log(bad)
  )
  exp(log_expected_power / power)
}

crra_wtp <- function(gamma, probability, wealth, loss) {
  ce <- crra_certainty_equivalent(gamma, probability, wealth, loss)
  if (is.na(ce)) NA_real_ else wealth - ce
}

implied_crra <- function(max_premium, probability, wealth, loss) {
  wtp <- as.numeric(max_premium); probability <- as.numeric(probability)
  wealth <- as.numeric(wealth); loss <- as.numeric(loss)
  if (any(!is.finite(c(wtp, probability, wealth, loss))) || probability <= 0 ||
      probability >= 1 || wealth <= 0 || loss <= 0 || wealth-loss <= 0 ||
      wtp < 0 || wtp >= loss) return(NA_real_)
  fair <- probability * loss
  if (abs(wtp - fair) < 0.005) return(0)
  f <- function(gamma) crra_wtp(gamma, probability, wealth, loss) - wtp
  tryCatch(uniroot(f, c(-50, 50), tol=1e-8)$root, error=function(e) NA_real_)
}

naive_risk_score <- function(max_premium, probability, loss) {
  max_premium <- as.numeric(max_premium); probability <- as.numeric(probability); loss <- as.numeric(loss)
  if (any(!is.finite(c(max_premium, probability, loss))) || loss <= 0) return(NA_real_)
  (max_premium - probability * loss) / loss
}
