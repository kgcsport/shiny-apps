# Run all unit tests.
# Usage from repo root: Rscript tests/run-unit-tests.R

suppressPackageStartupMessages(library(testthat))

args0    <- commandArgs(trailingOnly = FALSE)
this_file <- sub("--file=", "", args0[grep("--file=", args0)])
unit_dir  <- file.path(dirname(normalizePath(this_file, mustWork = FALSE)), "unit")
results <- test_dir(unit_dir, reporter = "progress", stop_on_failure = FALSE)
summary(results)

if (any(as.data.frame(results)$failed > 0)) quit(status = 1)
