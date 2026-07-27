# Run all unit tests.
# Usage from repo root: Rscript tests/run-unit-tests.R

suppressPackageStartupMessages(library(testthat))

results <- test_dir("tests/unit", reporter = "progress", stop_on_failure = FALSE)
summary(results)

if (any(as.data.frame(results)$failed > 0)) quit(status = 1)
