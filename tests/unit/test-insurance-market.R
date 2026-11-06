library(testthat)

insurance_path <- if (file.exists(file.path("apps", "class-job-market", "insurance.R"))) {
  file.path("apps", "class-job-market", "insurance.R")
} else {
  file.path("..", "..", "apps", "class-job-market", "insurance.R")
}
source(insurance_path, local = TRUE)

test_that("zero-profit premium uses average claims cost and requested tick", {
  expect_equal(insurance_zero_profit_premium(6, 8, step = 0.5), 1)
  expect_equal(insurance_zero_profit_premium(5, 8, step = 0.5), 0.5)
  expect_equal(insurance_zero_profit_premium(5, 8, step = 0.25), 0.75)
})

test_that("zero-policy rounds retain the fallback premium", {
  expect_equal(insurance_zero_profit_premium(0, 0, fallback = 1.25, step = 0.5), 1.5)
})

test_that("positive profit is fully divided across the active roster", {
  result <- insurance_round_accounting(
    policies_sold = 8, premium = 1, claims_paid = 4,
    dividend_recipients = 16
  )
  expect_equal(result$revenue, 8)
  expect_equal(result$profit, 4)
  expect_equal(result$dividend_each, 0.25)
  expect_equal(result$dividend_each * 16, result$distributable_profit)
})

test_that("losses create no negative dividend", {
  result <- insurance_round_accounting(
    policies_sold = 3, premium = 1, claims_paid = 4,
    dividend_recipients = 10
  )
  expect_equal(result$profit, -1)
  expect_equal(result$dividend_each, 0)
})
