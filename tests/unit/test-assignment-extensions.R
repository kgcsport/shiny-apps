library(testthat)

repo_root <- normalizePath(file.path(getwd(), "..", ".."), mustWork=TRUE)
helper_env <- new.env(parent=globalenv())
helper_env$`%||%` <- function(a,b) if (!is.null(a) && length(a) && !is.na(a[1])) a else b
sys.source(file.path(repo_root, "apps", "_shared", "assignment_extensions.R"), envir=helper_env)

test_that("Cloudflare extension payload uses persistent user and purchase IDs", {
  row <- data.frame(
    id=42L,
    cloudflare_assignment_id="econ342-2026-ps02",
    user_id="student@vassar.edu",
    extension_target="self_grading",
    hours=18,
    purchased_at="2026-09-30 14:00:00",
    stringsAsFactors=FALSE)
  payload <- helper_env$extension_sync_payload(row)
  expect_identical(payload$sourcePurchaseId, "42")
  expect_identical(payload$assignmentId, "econ342-2026-ps02")
  expect_identical(payload$userId, "student@vassar.edu")
  expect_identical(payload$target, "self_grading")
  expect_identical(payload$hours, 18)
})

test_that("browser code never receives the assignment admin token", {
  app_source <- paste(readLines(file.path(repo_root, "apps", "class-job-market", "app.R"), warn=FALSE), collapse="\n")
  helper_source <- paste(readLines(file.path(repo_root, "apps", "_shared", "assignment_extensions.R"), warn=FALSE), collapse="\n")
  expect_false(grepl("ASSIGNMENT_ADMIN_TOKEN.*Shiny.setInputValue", app_source))
  expect_match(helper_source, "req_headers\\(Authorization=")
})
