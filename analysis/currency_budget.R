#!/usr/bin/env Rscript
# How many points a section issues over the paid part of the term, and what
# full-credit threshold that supports.
#
#   Rscript analysis/currency_budget.R
#
# Change the numbers in `plan` to try another design. Counts per meeting are
# taken from the fall 2026 class where it has something to say (cold calls per
# meeting, share of students volunteering); how high students bid is not
# known, so three cases are shown.
set.seed(20261125)

plan <- list(
  students = 25, paid_meetings = 18, attendance = 0.92,
  # Upper limit of each job's random-wage draw (not shown to students).
  top = c(note_taker = 10, lecture_summary = 10, reading_summary = 10, answer = 4, board = 6),
  answers_per_meeting = c(1, 2, 2, 3, 3, 3, 4, 5),   # drawn from, per meeting
  boards_per_meeting  = c(0, 0, 1, 1, 1, 2),
  # Share of present students who volunteer in a meeting. Fall: median 0.40,
  # range 0.20 to 0.80 on meetings with any volunteering. One is paid per class.
  volunteer_share = 0.40,
  # A voluntary contribution is paid the day's drawn wage for an answer.
  turns_for_full_credit = 9
)
grid <- function(top) seq(0.5, top, by = 0.5)

one_term <- function(plan, bid_level, bid_spread = 0.6) {
  n <- plan$students
  # Each student's true minimum for each job, as a share of that job's upper
  # limit: median bid_level, with a student-wide component so some students
  # are cheaper than others across the board.
  student <- rnorm(n, sd = bid_spread * sqrt(0.4))
  minimum <- sapply(names(plan$top), function(job)
    round(2 * plan$top[[job]] * bid_level * exp(student + rnorm(n, sd = bid_spread * sqrt(0.6)))) / 2)
  points <- numeric(n); turns <- numeric(n); paid <- numeric(0); unfilled <- 0; slots <- 0
  pay <- function(i, wage) { points[i] <<- points[i] + wage; turns[i] <<- turns[i] + 1; paid <<- c(paid, wage) }
  for (meeting in seq_len(plan$paid_meetings)) {
    present <- which(runif(n) < plan$attendance)
    wage <- sapply(plan$top, function(top) sample(grid(top), 1))
    calls <- c(note_taker = 1, lecture_summary = 1, reading_summary = 1,
               answer = sample(plan$answers_per_meeting, 1), board = sample(plan$boards_per_meeting, 1))
    for (job in names(calls)) for (k in seq_len(calls[[job]])) {
      slots <- slots + 1
      eligible <- present[minimum[present, job] <= wage[[job]]]
      if (!length(eligible)) { unfilled <- unfilled + 1; next }
      pay(eligible[sample.int(length(eligible), 1)], wage[[job]])
    }
    for (i in present[runif(length(present)) < plan$volunteer_share]) pay(i, wage[["answer"]])
  }
  list(points = points, turns = turns, paid = paid, unfilled = unfilled / slots)
}

summarise <- function(plan, bid_level, reps = 300) {
  runs <- lapply(seq_len(reps), function(i) one_term(plan, bid_level))
  typical_wage <- mean(unlist(lapply(runs, `[[`, "paid")))
  threshold <- round(plan$turns_for_full_credit * typical_wage)
  points <- unlist(lapply(runs, `[[`, "points"))
  c(turns_per_student = mean(unlist(lapply(runs, `[[`, "turns"))),
    unfilled_share = mean(sapply(runs, `[[`, "unfilled")),
    typical_wage = typical_wage, threshold = threshold,
    issued_per_section = mean(sapply(runs, function(r) sum(r$points))),
    mean_points = mean(points), p10 = unname(quantile(points, 0.10)), p90 = unname(quantile(points, 0.90)),
    share_at_full_credit = mean(points >= threshold),
    mean_grade = mean(pmin(points / threshold, 1)))
}

cases <- c("Students bid low (median at 25% of the top)" = 0.25,
           "Middle (median at 50% of the top)" = 0.50,
           "Students bid high (median at 75% of the top)" = 0.75)
cat(sprintf("%d students, %d paid meetings, %d paid turns for full credit.\n", plan$students, plan$paid_meetings, plan$turns_for_full_credit))
cat("Upper limits: ", paste(sprintf("%s %g", names(plan$top), plan$top), collapse = ", "), "\n", sep = "")
for (name in names(cases)) {
  s <- summarise(plan, cases[[name]])
  cat(sprintf("\n%s\n", name))
  cat(sprintf("  Paid turns per student over the term: %.1f\n", s[["turns_per_student"]]))
  cat(sprintf("  Job and cold-call slots left unfilled: %.0f%%\n", 100 * s[["unfilled_share"]]))
  cat(sprintf("  Typical wage per paid turn: %.1f  ->  threshold of %d points\n", s[["typical_wage"]], as.integer(s[["threshold"]])))
  cat(sprintf("  Points per student: mean %.0f, 10th to 90th percentile %.0f to %.0f\n", s[["mean_points"]], s[["p10"]], s[["p90"]]))
  cat(sprintf("  Students at full credit: %.0f%%; average participation grade: %.0f%%\n",
              100 * s[["share_at_full_credit"]], 100 * s[["mean_grade"]]))
}
cat("\nWhat moves the result most (middle case):\n")
for (share in c(0.2, 0.4, 0.6)) {
  alt <- plan; alt$volunteer_share <- share; s <- summarise(alt, 0.5, reps = 150)
  cat(sprintf("  %2.0f%% of students volunteer each meeting: %.1f turns each, threshold %d, average grade %.0f%%\n",
              100 * share, s[["turns_per_student"]], as.integer(s[["threshold"]]), 100 * s[["mean_grade"]]))
}
