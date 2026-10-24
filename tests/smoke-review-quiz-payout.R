#!/usr/bin/env Rscript
# Usage (from shiny-apps/): Rscript tests/smoke-review-quiz-payout.R
# Exercises the Review Sprint token release against an isolated shared SQLite DB.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

root <- tempfile("review-quiz-payout-")
dir.create(root, recursive = TRUE)
root <- normalizePath(root, winslash = "/")
dir.create(file.path(root, "data"))
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
Sys.setenv(CONNECT_CONTENT_DIR = root)

seed_con <- dbConnect(SQLite(), file.path(root, "data", "class-job-market.sqlite"))
dbExecute(seed_con, "CREATE TABLE users (
  user_id TEXT PRIMARY KEY,
  display_name TEXT,
  is_admin INTEGER DEFAULT 0,
  active INTEGER DEFAULT 1
);")
dbDisconnect(seed_con)

quiz_env <- new.env(parent = globalenv())
# The payout helpers do not require DT; this keeps the database smoke test
# runnable in minimal CI environments where the UI package is absent.
quiz_env$library <- function(package, ..., character.only = FALSE) {
  pkg <- if (character.only) package else as.character(substitute(package))
  if (identical(pkg, "DT")) return(invisible(TRUE))
  base::library(pkg, character.only = TRUE, ...)
}
source("apps/review-quiz/app.R", local = quiz_env)

quiz_env$db_exec("CREATE TABLE IF NOT EXISTS arcade_sessions (
                  token TEXT PRIMARY KEY, user_id TEXT NOT NULL,
                  expires_at TEXT NOT NULL, created_at TEXT DEFAULT CURRENT_TIMESTAMP);")
quiz_env$db_exec("DELETE FROM quiz_responses;")
quiz_env$db_exec("DELETE FROM quiz_questions;")
quiz_env$db_exec("DELETE FROM quiz_token_sessions;")
quiz_env$db_exec("DELETE FROM quiz_token_payouts;")
quiz_env$db_exec("DELETE FROM token_ledger;")
quiz_env$db_exec("DELETE FROM users;")
quiz_env$db_exec("INSERT INTO users(user_id,display_name,is_admin,active) VALUES
                  ('ada','Ada',0,1), ('bert','Bert',0,1), ('admin','Admin',1,1);")
quiz_env$db_exec("INSERT INTO arcade_sessions(token,user_id,expires_at)
                  VALUES('shared-session-smoke','ada',datetime('now','+1 day'));")
shared_user <- quiz_env$lookup_shared_session("shared-session-smoke")
stopifnot(nrow(shared_user) == 1L, identical(shared_user$user_id[1], "ada"))
quiz_env$db_exec("INSERT INTO quiz_questions(q_num,topic,text,opt_a,opt_b,opt_c,opt_d,correct,explain) VALUES
                  (1,'Externalities','Question 1','A','B','C','D','A',''),
                  (2,'Externalities','Question 2','A','B','C','D','B','');")
quiz_env$db_exec("INSERT INTO quiz_responses(user_id,q_num,answer) VALUES
                  ('ada',1,'A'), ('ada',2,'B'), ('bert',1,'C'), ('bert',2,'D');")

released <- quiz_env$release_review_sprint(1L, "Smoke sprint", 0.25,
                                            quiz_env$load_questions())
stopifnot(nrow(released$scores) == 2L, identical(unname(released$total), 0.5))

ledger <- quiz_env$db_query("SELECT user_id, amount, earning, source_type, source_id
                             FROM token_ledger ORDER BY user_id;")
stopifnot(nrow(ledger) == 1L,
          identical(ledger$user_id[1], "ada"),
          identical(unname(ledger$amount[1]), 0.5),
          identical(as.integer(ledger$earning[1]), 1L),
          identical(ledger$source_type[1], "review_quiz"),
          identical(as.integer(ledger$source_id[1]), 1L))

payouts <- quiz_env$db_query("SELECT user_id, correct_count, amount
                              FROM quiz_token_payouts ORDER BY user_id;")
stopifnot(nrow(payouts) == 2L,
          identical(payouts$correct_count, c(2L, 0L)),
          identical(payouts$amount, c(0.5, 0)))

second_release <- tryCatch({
  quiz_env$release_review_sprint(1L, "Smoke sprint", 0.25, quiz_env$load_questions())
  NULL
}, error = function(e) e)
stopifnot(inherits(second_release, "error"),
          grepl("already been released", conditionMessage(second_release), fixed = TRUE))

if (!is.null(quiz_env$conn) && DBI::dbIsValid(quiz_env$conn)) DBI::dbDisconnect(quiz_env$conn)
cat("review-quiz payout smoke test passed\n")
