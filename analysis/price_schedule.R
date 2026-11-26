#!/usr/bin/env Rscript
# Every price in the classroom economy from a few numbers you set.
#
#   Rscript analysis/price_schedule.R
#
# One rule prices everything a student can buy:
#
#     price in points  =  rate  x  percentage points of course grade it is worth
#
# so a point buys the same expected grade gain whichever perk it is spent on.

you_set <- list(
  # ── Earning ──
  upper_wage_limit   = 10,   # top of the random-wage draw, same for every job (not shown to students)
  full_credit_points = 30,   # earned points that give full participation credit
  volunteer_wage     = 1,    # set wage for an unprompted contribution (one paid per class)
  # ── Spending ──
  # How many percentage points of course grade a typical student's term of
  # points should be able to buy. This fixes the rate.
  grade_points_a_term_buys = 4,
  typical_points_on_hand   = 40,   # from analysis/currency_budget.R for the settings above
  # ── Course facts ──
  participation_weight = 15, # percent of course grade
  problem_set_weight   = 15, # percent of course grade
  problem_sets         = 6,
  problem_set_points   = 100,
  late_points_per_hour = 2,
  final_weight         = 30, # percent of course grade   <-- CONFIRM
  exam_questions       = 12, # on the exam
  exam_answer          = 10, # the student answers this many
  questions_posted     = 24, # available to reveal
  preview_gain         = 0.20, # share of a question's credit a preview adds, after dropping the worst
  decline              = 11,   # larger = a revealed question's chance of appearing falls more slowly
  section_size         = 25,
  reweight_cap         = 5     # most percentage points of weight one student may move in a term
)

p <- you_set
rate <- p$typical_points_on_hand / p$grade_points_a_term_buys          # points per percentage point
price <- function(grade_points) rate * grade_points
show <- function(x) format(round(x, 1), nsmall = 1)

cat(sprintf("Rate: %s points buy 1 percentage point of course grade.\n", show(rate)))
cat(sprintf("Earning side: 1 earned point is worth %.2f percentage points of participation, up to %d points.\n",
            p$participation_weight / p$full_credit_points, p$full_credit_points))

# Extensions: an hour late costs late_points_per_hour of one problem set.
hour_worth <- p$problem_set_weight * p$late_points_per_hour / (p$problem_sets * p$problem_set_points)
cat(sprintf("\nExtension: %s points per hour (an hour is worth %.3f percentage points), any problem set.\n",
            show(price(hour_worth)), hour_worth))
for (h in c(6, 12, 24, 48)) cat(sprintf("  %2d hours: %s points\n", h, show(price(hour_worth * h))))

# Exam questions: chance a revealed question appears, given k revealed in all.
start <- p$exam_questions / p$questions_posted
chance <- function(k) start * (p$decline + 1) / (p$decline + k)
question_worth <- p$final_weight * p$preview_gain / p$exam_answer        # per student, if it appears
k <- seq_len(p$questions_posted)
added <- diff(c(0, k * chance(k)))                                       # extra expected known questions
section_price <- price(question_worth * added * p$section_size)
cat(sprintf("\nExam question, k-th one revealed: chance each revealed question appears = %.2f x %d / (%d + k).\n",
            start, p$decline + 1, p$decline))
cat("  k   chance  known on exam  section price  per student\n")
for (i in c(1, 2, 3, 5, 8, 12, 18, 24)) if (i <= p$questions_posted)
  cat(sprintf("  %-3d %4.0f%%   %5.1f          %6s        %5s\n", i, 100 * chance(i), i * chance(i), show(section_price[i]),
              show(section_price[i] / p$section_size)))
cat(sprintf("  All %d revealed: %s points in total; about %.1f known questions on a %d-question exam.\n",
            p$questions_posted, show(sum(section_price)), p$questions_posted * chance(p$questions_posted), p$exam_questions))

# Reweighting: priced per student from their own scores.
cat(sprintf("\nReweight: %s points x (weight moved, in percentage points) x (score gap as a share).\n", show(rate)))
cat(sprintf("  Example: move %d points of weight with a 30-point score gap: %s points. Cap: %d points of weight per term.\n",
            p$reweight_cap, show(price(p$reweight_cap * 0.30)), p$reweight_cap))
