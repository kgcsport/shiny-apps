insurance_round_to_step <- function(value, step = 0.5) {
  value <- suppressWarnings(as.numeric(value))
  step <- suppressWarnings(as.numeric(step))
  if (!is.finite(value) || value < 0) return(NA_real_)
  if (!is.finite(step) || step <= 0) step <- 0.5
  floor(value / step + 0.5) * step
}

insurance_zero_profit_premium <- function(total_claim_cost, policies_sold,
                                          fallback = 0, step = 0.5) {
  total_claim_cost <- suppressWarnings(as.numeric(total_claim_cost))
  policies_sold <- suppressWarnings(as.numeric(policies_sold))
  fallback <- suppressWarnings(as.numeric(fallback))
  if (!is.finite(fallback) || fallback < 0) fallback <- 0
  if (!is.finite(total_claim_cost) || total_claim_cost < 0 ||
      !is.finite(policies_sold) || policies_sold <= 0) {
    return(insurance_round_to_step(fallback, step))
  }
  insurance_round_to_step(total_claim_cost / policies_sold, step)
}

insurance_round_accounting <- function(policies_sold, premium, claims_paid,
                                       dividend_recipients) {
  policies_sold <- max(0, suppressWarnings(as.numeric(policies_sold)))
  premium <- max(0, suppressWarnings(as.numeric(premium)))
  claims_paid <- max(0, suppressWarnings(as.numeric(claims_paid)))
  dividend_recipients <- max(0, suppressWarnings(as.integer(dividend_recipients)))
  revenue <- policies_sold * premium
  profit <- revenue - claims_paid
  distributable_profit <- max(0, profit)
  dividend_each <- if (dividend_recipients > 0) {
    distributable_profit / dividend_recipients
  } else 0
  list(
    revenue = revenue,
    claims_paid = claims_paid,
    profit = profit,
    distributable_profit = distributable_profit,
    dividend_each = dividend_each
  )
}
