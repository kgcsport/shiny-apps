#!/usr/bin/env Rscript
# Worker schedules are authoritative for extension-product dates. A submission
# product uses dueAt; its corrections product uses gradingDueAt.
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (is.atomic(a) && length(a) == 1L && is.na(a))) b else a

app_path <- if (file.exists("apps/class-job-market/app.R")) {
  "apps/class-job-market/app.R"
} else "/srv/shiny-server/class-job-market/app.R"
expressions <- parse(app_path)
helpers <- new.env(parent=globalenv())
needed <- c("worker_schedule_date", "problem_set_schedule_updates")
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) && is.symbol(expr[[2]]) &&
      as.character(expr[[2]]) %in% needed) eval(expr, helpers)
}
stopifnot(setequal(ls(helpers), needed))

products <- data.frame(
  id=c(7L,8L,9L,10L,11L),
  cloudflare_assignment_id=c("econ342-2026-ps04","econ342-2026-ps04",
                             "econ342-2026-ps05","econ342-2026-ps05","unmapped"),
  extension_target=c("submission","self_grading","submission","self_grading","submission"),
  stringsAsFactors=FALSE
)
schedules <- data.frame(
  assignment_id=c("econ342-2026-ps04","econ342-2026-ps05"),
  due_at=c("2026-11-03T22:00:00.000Z","2026-11-17T22:00:00.000Z"),
  solutions_at=c("2026-11-04T14:00:00.000Z","2026-11-18T14:00:00.000Z"),
  grading_due_at=c("2026-11-06T22:00:00.000Z","2026-11-20T22:00:00.000Z"),
  stringsAsFactors=FALSE
)
updates <- helpers$problem_set_schedule_updates(products, schedules)
stopifnot(nrow(updates) == 4L, !any(updates$id == 11L),
          updates$original_deadline[updates$id == 7L] == "2026-11-03",
          updates$original_deadline[updates$id == 8L] == "2026-11-06",
          updates$original_deadline[updates$id == 9L] == "2026-11-17",
          updates$original_deadline[updates$id == 10L] == "2026-11-20",
          updates$solutions_posted_at[updates$id == 8L] == "2026-11-04")
cat("critical extension schedule refresh: OK\n")
