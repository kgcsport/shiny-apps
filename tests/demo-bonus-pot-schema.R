#!/usr/bin/env Rscript
# Regression: an existing demo database predating Club Choice must be upgraded
# before its direct Bonus Pot player/projector session attempts a write or reveal.
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

repo_root <- if (file.exists("apps/class-job-market/app.R")) normalizePath(".") else
  normalizePath(file.path(dirname(commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1]), ".."))
tmp_root <- tempfile("demo-bonus-pot-schema-")
dir.create(file.path(tmp_root, "data"), recursive = TRUE)
old_connect <- Sys.getenv("CONNECT_CONTENT_DIR", unset = NA_character_)
Sys.setenv(CONNECT_CONTENT_DIR = tmp_root)
on.exit({
  if (is.na(old_connect)) Sys.unsetenv("CONNECT_CONTENT_DIR") else Sys.setenv(CONNECT_CONTENT_DIR = old_connect)
  unlink(tmp_root, recursive = TRUE, force = TRUE)
}, add = TRUE)

app <- new.env(parent = globalenv())
old_wd <- setwd(file.path(repo_root, "apps", "class-job-market"))
on.exit(setwd(old_wd), add = TRUE)
sys.source("app.R", envir = app)
prod_path <- file.path(tmp_root, "data", "class-job-market.sqlite")
demo_path <- file.path(tmp_root, "data", "class-job-market-demo.sqlite")
demo_con <- dbConnect(SQLite(), demo_path)
on.exit(dbDisconnect(demo_con), add = TRUE)
dbExecute(demo_con, "CREATE TABLE olig_submissions(
  round INTEGER NOT NULL, user_id TEXT NOT NULL, section TEXT, choice TEXT,
  contribute REAL, PRIMARY KEY(round,user_id));")
dbExecute(demo_con, "CREATE TABLE bonus_club_round_snapshots(
  round INTEGER NOT NULL, section TEXT NOT NULL, PRIMARY KEY(round,section));")

app$demo_db_bootstrap(demo_con, prod_path)
columns <- function(table) dbGetQuery(demo_con, sprintf("PRAGMA table_info(%s);", table))$name
stopifnot(all(c("display_name", "game", "action", "created_at", "club_choice",
                "entry_cost", "starting_balance", "club_final_balance") %in%
              columns("olig_submissions")))
stopifnot(all(c("pool_structure", "open_multiplier", "commitment_multiplier",
                "commitment_entry_cost", "commitment_min_contribution", "allow_exit",
                "finalized_at") %in% columns("bonus_club_round_snapshots")))
# This is the direct Club Choice submission statement.  Its placeholder count
# must match the ten named columns or SQLite rejects every student entry.
dbExecute(demo_con, "INSERT INTO olig_submissions(
  round,user_id,display_name,game,action,contribute,section,club_choice,entry_cost,starting_balance
) VALUES(?,?,?,'bonus',NULL,?,?,?,?,?)",
  list(1L, "alice", "Alice", 2, "S01", "open", 0, 10))
stopifnot(dbGetQuery(demo_con, "SELECT COUNT(*) AS n FROM olig_submissions;")$n[1] == 1L)
cat("Demo Bonus Pot schema migration: OK\n")
