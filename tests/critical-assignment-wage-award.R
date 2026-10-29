#!/usr/bin/env Rscript
# Critical contract: a completed assigned job pays its saved assignment wage.
app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
source_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
commit_start <- regexpr("\\.commit_live_score_event <- function", source_text, perl = TRUE)
if (commit_start[[1]] < 1L) stop("Could not locate assignment audit commit function.")
commit_source <- substr(source_text, commit_start[[1]], commit_start[[1]] + 5000L)
required_commit <- c("SELECT assigned_wage, COALESCE(tokens_awarded,0) AS tokens_awarded FROM job_assignments", "final_wage <- as.numeric(cur$assigned_wage[1] %||% 0)", "complete = final_wage")
missing_commit <- required_commit[!vapply(required_commit, grepl, logical(1), x = commit_source, fixed = TRUE)]
if (length(missing_commit)) stop("Assignment audit wage contract missing: ", paste(missing_commit, collapse = " | "))
if (grepl("current_wage", commit_source, fixed = TRUE)) stop("Assignment audit must not read undeclared current_wage.")
eval_start <- regexpr("# ── Job Evaluation", source_text, fixed = TRUE)
if (eval_start[[1]] < 1L) stop("Could not locate assignment evaluation handler.")
eval_source <- substr(source_text, eval_start[[1]], eval_start[[1]] + 4000L)
if (!grepl("wage  <- as.numeric(row$assigned_wage[1] %||% 0)", eval_source, fixed = TRUE)) stop("Assignment evaluation must queue complete jobs at their assigned wage.")
if (grepl("row$current_wage", eval_source, fixed = TRUE)) stop("Assignment evaluation must not read undeclared current_wage.")
cat("critical assignment wage award: OK\n")

# Repair contract: historical remediation is narrow, idempotent, and opt-in.
repair_required <- c(
  "zeroed_completed_job_repairs <- function()",
  "COALESCE(ja.assigned_wage,0)>0",
  "COALESCE(ja.tokens_awarded,0)=0",
  "lse.event_kind='assignment' AND lse.committed_at IS NOT NULL",
  "tl.source_type='job' AND tl.source_id=ja.id AND tl.amount>0",
  "repair_zeroed_jobs_btn",
  "Repair zeroed jobs",
  "Repair: completed job previously recorded as 0"
)
repair_missing <- repair_required[!vapply(repair_required, grepl, logical(1), x = source_text, fixed = TRUE)]
if (length(repair_missing)) stop("Completed-job zero repair contract missing: ", paste(repair_missing, collapse = " | "))
