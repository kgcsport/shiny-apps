#!/usr/bin/env Rscript
# Critical contract: start/end visibility follows each assignment's saved timing.

app_path <- if (file.exists("apps/class-job-market/app.R")) "apps/class-job-market/app.R" else "/srv/shiny-server/class-job-market/app.R"
source_text <- paste(readLines(app_path, warn=FALSE), collapse="\n")

# Explicit assignment timing wins. A manual "Any" is deliberately treated as
# inherited timing rather than silently exposing an end-of-class job at start.
timing_sql <- "COALESCE(NULLIF(NULLIF(ja.selection_time,''),'any'),"
if (length(gregexpr(timing_sql, source_text, fixed=TRUE)[[1]]) < 2L) {
  stop("Today must use explicit job-assignment timing for both personal and class reveal lists.")
}
required <- c(
  "INSERT OR IGNORE INTO job_assignments(round_id, user_id, job_post_id, assigned_wage, assignment_mode, selection_time)",
  "post_timing <- setNames",
  "sr_timing == today_my_assign$reveal_timing[i]",
  "sr_timing == revealed_jobs$reveal_timing[i]",
  "reveal_timings_for_scope <- function(scope)"
)
missing <- required[!vapply(required, grepl, logical(1), x=source_text, fixed=TRUE)]
if (length(missing)) stop("Assignment reveal timing contract missing: ", paste(missing, collapse=" | "))

cat("critical assignment reveal timing: OK\n")
