#!/usr/bin/env Rscript
# Power for the registered primary test (H1): does being assigned to practise
# a job change a student's first sealed bid on that job?
#
#   Rscript analysis/power_simulation.R            # prints the tables
#   POWER_REPS=2000 Rscript analysis/power_simulation.R
#
# Design simulated: per section 5 students get no practice job, 10 get one and
# 10 get two, drawn from 5 core jobs with the number assigned to each job
# balanced. One outcome per student x job. Model: outcome on a practised-job
# indicator with student and job-by-section fixed effects.
#
# Everything is in units of the standard deviation of bids across students
# and jobs within a section-job cell (the "total SD"). rho is the share of
# that variance that is a student's own level, which the student fixed effects
# remove; the rest is student-by-job noise, which is what limits power.
# Fall 2026 bids cannot pin rho down (half were ring bids, the rest sat on the
# posted wage), so a range is reported. Among the 56 usable fall first bids
# the student share was about 0.4.

reps  <- as.integer(Sys.getenv("POWER_REPS", "1000"))
alpha <- 0.05
set.seed(20261125)

core_jobs <- 5L
arms <- c(none = 5L, one = 10L, two = 10L)

# Balanced lottery: every job is practised by the same number of students.
assign_practice <- function(arms, jobs) {
  n <- sum(arms)
  arm <- sample(rep(names(arms), arms))
  turns <- sum(arm == "one") + 2L * sum(arm == "two")
  repeat {
    pool <- sample(rep(seq_len(jobs), length.out = turns))
    practised <- matrix(0L, n, jobs); ok <- TRUE
    for (i in sample(which(arm != "none"))) {
      k <- if (arm[i] == "two") 2L else 1L
      pick <- unique(pool)[seq_len(k)]
      if (length(pick) < k || any(is.na(pick))) { ok <- FALSE; break }
      practised[i, pick] <- 1L
      for (p in pick) pool <- pool[-match(p, pool)]
    }
    if (ok) return(practised)
  }
}

one_study <- function(sections, rho, effect, censor_share = 0) {
  rows <- do.call(rbind, lapply(seq_len(sections), function(s) {
    practised <- assign_practice(arms, core_jobs)
    n <- nrow(practised)
    level <- rnorm(n, sd = sqrt(rho))
    noise <- matrix(rnorm(n * core_jobs, sd = sqrt(1 - rho)), n, core_jobs)
    y <- level + noise + effect * practised
    data.frame(student = paste(s, rep(seq_len(n), core_jobs)), cell = paste(s, rep(seq_len(core_jobs), each = n)),
               practised = as.vector(practised), y = as.vector(y))
  }))
  # Bids above the undisclosed top of the draw carry no more information than
  # "above the top": the highest censor_share of outcomes are set to that cut.
  if (censor_share > 0) { cut <- quantile(rows$y, 1 - censor_share); rows$y <- pmin(rows$y, cut) }
  fit <- summary(lm(y ~ practised + factor(student) + factor(cell), data = rows))$coefficients["practised", ]
  c(estimate = fit[["Estimate"]], reject = fit[["Pr(>|t|)"]] < alpha)
}
power_at <- function(sections, rho, effect, censor_share = 0) {
  out <- replicate(reps, one_study(sections, rho, effect, censor_share))
  c(power = mean(out["reject", ]), se = sd(out["estimate", ]))
}

cat(sprintf("Replications per cell: %d. Two-sided test at %.2f.\n\n", reps, alpha))
cat("1. False-positive rate with no true effect (should be near 0.05)\n")
for (sections in 2:3) cat(sprintf("   %d sections: %.3f\n", sections, power_at(sections, 0.4, 0)[["power"]]))

cat("\n2. Smallest effect detected 80% of the time, in total-SD units\n")
cat("   (2.8 x the standard error of the estimate)\n")
cat(sprintf("   %-10s %-22s %s\n", "sections", "student share (rho)", "smallest effect"))
grid <- expand.grid(sections = 2:3, rho = c(0.2, 0.4, 0.6, 0.8))
grid$mde <- mapply(function(s, r) 2.8 * power_at(s, r, 0)[["se"]], grid$sections, grid$rho)
for (i in seq_len(nrow(grid))) cat(sprintf("   %-10d %-22.1f %.2f\n", grid$sections[i], grid$rho[i], grid$mde[i]))

cat("\n3. Power by true effect (total-SD units), student share 0.4\n")
cat(sprintf("   %-8s %-12s %s\n", "effect", "2 sections", "3 sections"))
for (effect in c(0.1, 0.2, 0.3, 0.4, 0.5))
  cat(sprintf("   %-8.1f %-12.2f %.2f\n", effect, power_at(2, 0.4, effect)[["power"]], power_at(3, 0.4, effect)[["power"]]))

cat("\n4. Cost of bids above the top of the draw (3 sections, student share 0.4, true effect 0.3)\n")
for (share in c(0, 0.1, 0.25, 0.4))
  cat(sprintf("   %2.0f%% of bids above the top: power %.2f\n", 100 * share, power_at(3, 0.4, 0.3, share)[["power"]]))
