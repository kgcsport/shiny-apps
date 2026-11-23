# Preregistration Draft: Random Early Job Exposure in a Class-Job Market

Version date: 2026-11-23 (revises the 2026-11-12 draft)

Status: registration draft. It must not be registered until all bracketed
fields are resolved, the software is frozen and tested, and institutional
review requirements are satisfied. Registration must precede the first
training-period assignment.

Change from the 2026-11-12 draft: the confirmatory question is now the causal
effect of randomly assigned early job practice. The market questions in the
earlier draft are retained as descriptive analyses only.

## 1. Title

Learning to Participate: Random Early Exposure to Classroom Jobs and Later
Reservation Wages in an Introductory Course

## 2. Investigators and setting

- Principal investigator/instructor: [NAME]
- Institution: [INSTITUTION]
- Course: [COURSE NUMBER AND TITLE]
- Term: [TERM]
- Sections: [SECTION LABELS]
- Expected enrollment: about 25 students per section, about 50 in total
- Meetings: two per week per section; [DAYS, NUMBER OF WEEKS, AND LENGTH]
- Registration repository/platform: [PLATFORM AND URL]
- Planned registration date: [DATE]

The primary study consists of two sections of the same introductory course.
The sections use the same mechanism and are separate labor markets. Section
comparisons are descriptive because two sections do not provide enough
independent clusters for a section-level causal estimate.

A possible 200-level Data Science implementation is not part of the primary
confirmatory sample unless explicitly added before registration. Otherwise it
will be registered separately as a replication with a course-specific job
catalog.

## 3. Institutional review and consent

- Institutional determination: exempt; [PROTOCOL NUMBER AND DETERMINATION DATE]
- Consent procedure: [PROCEDURE]
- Person obtaining consent: [PERSON]
- Timing relative to grading: [TIMING]
- Data retained for students who decline research use: [OPERATIONAL DATA ONLY]

The course mechanism applies according to the published course policy. Research
participation is separate. A student who declines research use receives the
same jobs, market access, feedback, perks, and grading rules. Their operational
records are excluded from the research dataset to the extent specified by the
institutional determination.

Because the instructor controls grades, the approved plan must address possible
undue influence and specify when the instructor can learn consent status. The
preferred arrangement is that the instructor does not see individual consent
status until final grades are submitted.

## 4. Research questions

Primary question:

1. Does randomly assigned practice in a classroom job during the no-stakes
   training period change the minimum wage the student later requires to do
   that same job?

Secondary questions:

1. Does any effect on bids persist through the first fixed-rule market weeks?
2. Does assigned practice change how often the student later takes and
   successfully completes that job?
3. Does the effect differ by baseline comfort with participation and by job
   family (live/public versus prepared/artifact)?
4. If a no-practice arm exists, does practising any job change bids across all
   jobs?

Descriptive questions carried over from the earlier draft, with no
confirmatory test:

- how minimum acceptable wages differ across jobs;
- how concentrated paid work and earnings are;
- whether bidding continues as students approach the participation threshold;
  and
- how spendable points are used across perks.

The design is a randomized experiment embedded in the course. Students are
randomly assigned which job they practise in the training period. Treatment is
practice with feedback: a student who practises a job also receives an outcome
and up to two diagnostic tags for it, and the two are not separated. Every
student can watch every job being performed, so the estimand is the effect of
performing a job over and above observing it.

Market assignment after the training period is endogenous to bids. Analyses of
market behaviour that do not use the registered random assignment are
descriptive.

## 5. Confirmatory hypotheses

### H1 (primary): own-job practice and the first sealed bid

A student's first sealed minimum-wage bid for a job is lower when the student
was randomly assigned to practise that job than when the student was not.

The test is two-sided. The predicted direction is negative, because practice
is expected to reduce the perceived cost of the job. A positive effect, in
which students learn that a job is harder than expected, is reported with the
same weight.

### H2 (secondary): persistence

The effect in H1 is also present in the student's mean bid for the job across
the fixed-rule market weeks [WEEKS 3-6].

### H3 (secondary): later completion

A student records more `Success` completions of a job during [WINDOW] when the
student was randomly assigned to practise it.

This outcome is counted for every student-job pair and is zero when the
student never holds the job, so it is not conditioned on winning an auction.

### Pre-specified heterogeneity

H1 is re-estimated separately for:

- students above versus at or below the section median of the baseline
  participation-comfort index; and
- live/public versus prepared/artifact jobs, using the classification below.

Pre-specified classification:

- Live/public: [JOB LIST]
- Prepared/artifact: [JOB LIST]
- Excluded/other: [JOB LIST]

Heterogeneity by the feedback outcome received in training is exploratory,
because that outcome is determined after assignment.

## 6. Course mechanism

### 6.1 Baseline survey

Every student completes a baseline survey before the exposure schedule is
generated. Survey close: [DATE/TIME, BEFORE THE FIRST TRAINING ASSIGNMENT].

For each job in the frozen catalog the survey records:

- comfort doing the job, on a [SCALE]; and
- the minimum number of participation points the student would require to do
  it, entered on the same Job Market bid form used in the later market, with
  the same bid step. Practice dates are set to wage-bidding mode so the form
  accepts bids, but the lowest-bid draw is not run: practice jobs come from the
  registered lottery schedule. Baseline bids assign no jobs and pay nothing.

It also records [COMFORT SPEAKING IN CLASS, CONCERN ABOUT BEING WRONG PUBLICLY,
PRIOR PARTICIPATION HABITS, CONFIDENCE WITH QUANTITATIVE MATERIAL; FINAL ITEM
LIST IN APPENDIX E]. The baseline participation-comfort index is [EXACT
CONSTRUCTION].

Baseline responses are hypothetical and unpaid. They are used as covariates
and for the heterogeneity split, never to set wages.

### 6.1a Training period and random exposure

The first [NUMBER; PLANNED AS TWO] weeks contain no purchases or token payouts.
Each training meeting is flagged as a practice date in the app, which stores
every wage for that date as zero, so students see no wage before their first
market bid and practising a job creates no difference in token wealth during
the term.

Practice work is paid once, after the last ordinary class date: a completed
practice job earns the average wage that same job paid on the ordinary dates
in the other section of the course, and a try earns one point. A job that was
not run in the other section earns that section's average across all jobs.
Because the wage comes from the other section, no bid a student places can
change their own practice pay. The two sections are run as separate markets
in the app for this reason. Students are told this rule [WHEN].

Random assignment:

- Unit: student. Randomization is carried out separately within each section.
- Roster: students enrolled and with a completed baseline survey at
  [FREEZE DATE/TIME].
- Arms: each student is assigned to practise exactly [K] of the [J] catalog
  jobs. Every student is assigned; with about 25 students and four training
  meetings per section there is no no-practice arm.
- Balance: arm sizes are fixed in advance and as equal across jobs as slot
  counts allow; [OPTIONAL: BLOCKED ON BASELINE COMFORT-INDEX TERCILE].
- Timing: the training meeting in which each student performs is also drawn at
  random within the student's assigned job.
- Procedure: a seeded script generates the full schedule once. The seed, the
  script commit, and a hash of the schedule are recorded before the first
  training meeting. The schedule is not regenerated.
- Late adds: [EXCLUDED FROM THE CONFIRMATORY SAMPLE AND GIVEN A JOB BY THE SAME
  SCRIPT / OTHER REGISTERED RULE].

Students are told that training jobs are assigned by lottery. Students
practise their assigned job and receive one outcome plus at most two
diagnostic tags.

Absences and replacements:

- A student who is absent keeps the original assignment in the record.
- Make-up rule: [PERFORM AT THE NEXT MEETING / NO MAKE-UP].
- Empty slots are [LEFT EMPTY / FILLED IN THE ORDER OF A PRE-RANDOMIZED RESERVE
  LIST GENERATED BY THE SAME SCRIPT]. A replacement's performance is recorded
  as practice the student was not assigned.
- The instructor does not reassign training jobs at discretion. Any departure
  is logged with a reason.

Outcomes:

- `Success`
- `Try`
- `Fail`
- `Miss`
- `Excused` as a separate administrative state

Final diagnostic-tag dictionary: [LOCKED LIST OR APPENDIX]

The dataset records whether feedback was based on direct instructor observation,
small-group reporting, or a submitted artifact.

### 6.2 One-time calibration

The participation target is fixed before the first consequential bid and does
not rise during the semester.

During the training period the instructor records, by job and meeting:

- named-job slots;
- planned and realized cold-call quantity;
- planned paid-volunteer quantity;
- Success/Try/Fail/Miss frequency; and
- the number of paid roles that could stack for one student.

Definitions:

- `N_s`: active enrollment in section `s` at the calibration freeze
- `R_s`: remaining meetings in section `s`
- `q_js`: planned slots per meeting for job `j`
- `B_j`: maximum permitted wage for job `j`
- `K`: maximum paid events per student per meeting
- `E_max`: maximum grade-relevant points one student can earn in one meeting
- `T`: full-credit gross-point threshold

The early-hit constraint is:

```text
ceiling(T / E_max) >= [MINIMUM NUMBER OR SHARE OF CONSEQUENTAL MEETINGS]
```

The aggregate opportunity check is:

```text
expected total payroll / (sum_s N_s * T) >= [BUFFER, E.G. 1.20]
```

The exact locked calibration calculation, assumptions about clearing wages,
and treatment of Try outcomes are provided in Appendix A before registration.

The target may be lowered once if documented cancellations or technical
failures reduce offered payroll below the registered opportunity floor. It is
never raised. The preregistration must choose one of the following before
launch:

- `[ ]` no adjustment under any circumstances; or
- `[ ]` one-way adjustment using the exact formula in Appendix A.

Any adjustment is reported with the original target, revised target, date,
reason, and the information available at the time.

### 6.3 Wage bidding and allocation

- Bid form: the ordinary Job Market page, where a student enters a minimum
  wage for each job and may update it until each bid lock. Bids stand and carry
  forward between meetings.
- No wage anchor: the app's no-anchor switch is on for the registered sections.
  Bid boxes are empty until the student saves a bid of their
  own. No instructor wage is pre-filled or displayed before bids exist, and no
  points are paid in the training period, so students see no wage before their
  first bid.
- Initial bid sheet: the bids on file at the first bid lock after the market
  opens on [DATE], before any auction result is shown. Every student is asked
  to enter a bid for every job by that lock [HOW: IN CLASS / REQUIRED]. These
  sealed bids are the primary outcome.
- Bidding begins: [DATE / WEEK]
- Bid unit: participation points
- Bid step: [STEP]
- Maximum wage bid: none [TENTATIVE]. A student who does not want a job enters
  a high bid. Very high bids are handled in the analysis, not by the form.
- Allocation: lowest valid bids receive the posted slots
- Pricing: uniform reverse-second-price
- Wage: first unsuccessful bid
- No-loser fallback: [FALLBACK]
- Bid lock: [TIME]
- Tie-break: [REGISTERED RANDOM PROCEDURE]
- Maximum assigned jobs per meeting: [LIMIT]
- Maximum grade-relevant earnings per meeting: [LIMIT]

Individual bids remain private. Assignments and the clearing wage are revealed
according to the published schedule.

### 6.4 Completion and payment

- Success payment: [FULL CLEARING WAGE]
- Try payment: [RULE]
- Fail payment: [RULE]
- Miss payment: zero
- Excused absence: [RULE]
- Deadline and completion criterion by job: [APPENDIX]

Gross positive earning entries determine participation credit. Purchases and
other non-earning ledger entries do not reduce gross earnings.

### 6.5 Participation-grade mapping

- Course-grade weight: 15 percentage points
- Full-credit threshold: [T] gross points
- Mapping below the threshold:

```text
participation contribution = W * min(gross earned points / T, 1)
```

- One point's course-grade equivalent below the threshold: `W / T`

The student interface displays gross earnings, spendable balance, progress to
the threshold, and the grade equivalent below the threshold.

### 6.6 Paid live participation

- Live answers and board work are one job type each, filled either by cold
  call or by volunteers.
- Wage: the frozen clearing wage for the corresponding cold-call job. Volunteer
  jobs are not bid on separately.
- Paid volunteering: each student is paid for at most one voluntary
  contribution per meeting. Later voluntary contributions that meeting are
  welcome and are recorded with their outcome, unpaid.
- There is no limit on how many students may volunteer.
- Special bounties: valid only if announced before the task

### 6.7 Perks and token spending

The complete launch menu is frozen before the first bid:

| Perk | Price or formula | Opens | Closes | Individual/collective | Cap |
|---|---:|---|---|---|---:|
| Automatic extension | [PRICE] | [DATE] | [DATE] | Individual | [CAP] |
| Candidate-question fund | [PRICE SCHEDULE] | [DATE] | [DATE] | Collective | [CAP] |
| [OTHER] | [PRICE] | [DATE] | [DATE] | [TYPE] | [CAP] |

At least one meaningful perk remains available after the final ordinary job
auction. Unspent points [EXPIRE / HAVE THE PRE-SPECIFIED TERMINAL USE]. Spending
never reduces gross participation credit.

Pass Insurance is removed: its per-course switch in the app stays off in the
registered sections for the whole term. Problem-set extensions remain
deterministic purchases and are not insurance.

## 7. Sample

### Inclusion

- students on the frozen randomization roster in the registered sections;
- research consent or other approved basis for inclusion.

Students are analysed according to the job they were assigned, whether or not
they performed it.

### Exclusion

- instructors, administrators, demo users, and synthetic accounts;
- students who decline research participation;
- students who join after the randomization roster is frozen;
- students who withdraw before the initial bid sheet;
- records produced during documented app testing; and
- observations outside the frozen course/section/date scope.

Enrollment determines sample size. No enrollment-based stopping rule is used.
Final sample counts and exclusions will be reported in a flow table by section
and by assigned job.

### Power and precision

Before registration, run a simulation of the exact registered design: final
expected enrollment by section, [J] jobs, [K] assigned jobs per student, the
planned arm sizes, and the H1 model with randomization inference. Take bid
dispersion, the within-student correlation of bids across jobs, and the
correlation between baseline and market bids from the pilot-semester data
where available.

Attach the code and report the minimum detectable effect on the first sealed
bid, in points and in within-job standard deviations, at 80% power and
two-sided alpha 0.05. If the minimum detectable effect is larger than
[THRESHOLD], register the study as a pilot of the design rather than as a
confirmatory test, or add sections or terms before registration, not after
outcomes are observed.

## 8. Variables

### Treatment

- `assigned_ij`: 1 if student `i` was randomly assigned to practise job `j` in
  the training period, from the hashed schedule.
- `performed_ij`: 1 if student `i` actually performed job `j` in the training
  period, whether assigned or as a replacement.

### Primary outcome

- `first_bid_ij`: the student's bid for job `j` on file at the first bid lock
  after the market opens, in participation points. With no bid ceiling, bids
  above the [95TH] percentile of their section and job are set to that
  percentile. The percentile is fixed here, before any data are seen.

### Secondary outcomes

- `mean_bid_ij`: mean locked bid for job `j` across [WEEKS 3-6], with the same
  treatment of very high bids;
- `willing_ij`: indicator that the first bid is at or below the section-job
  median first bid;
- `success_count_ij`: number of `Success` completions of job `j` in [WINDOW];
- `held_ij`: indicator for holding job `j` at least once in [WINDOW]; and
- `volunteer_count_ij`: number of recorded voluntary contributions of job type
  `j` in [WINDOW], paid or unpaid, for live/public jobs.

### Descriptive market variables

- clearing wage and pricing fallback;
- Success/Try/Fail/Miss/Excused and diagnostic tags;
- gross earnings, spendable balance, and threshold attainment date;
- perk debits and contributions by perk;
- instructor override; and
- technical-error indicator.

### Pre-specified covariates

- baseline hypothetical minimum wage for the same student and job;
- baseline comfort for the same student and job;
- section; and
- job identity and job family.

All covariates are measured before random assignment. Do not use training
feedback outcomes, later grades, earnings, or market behaviour as controls in
the confirmatory models.

## 9. Missing and exceptional records

- A student-job pair with no response on the initial bid sheet is missing and
  is not imputed in the primary analysis.
- Report missingness of the primary outcome by assignment status. If it
  differs by more than [X] percentage points, report trimming bounds alongside
  the primary estimate.
- A missing baseline wage is set to the section-job mean and flagged with an
  indicator.
- A Try/Fail/Miss is an observed outcome, not missing completion data.
- Excused absences are excluded from completion-rate denominators and reported.
- Technical duplicate ledger entries are resolved using the app's idempotency
  keys before dataset freeze and documented in an audit table.
- Instructor overrides remain in the dataset with their reasons. Primary
  models use final operational records; sensitivity analyses exclude overrides.

## 10. Analysis plan

### H1 (primary)

The unit of observation is the student-job pair, for every job in the frozen
catalog. Estimate:

```text
first_bid_ij = student_FE_i + job_by_section_FE_js
               + beta * assigned_ij
               + delta * baseline_wage_ij + error_ij
```

`beta` is the intent-to-treat effect of being assigned to practise a job on
the first sealed bid for that job, relative to the same student's bids for
jobs the student was not assigned. Student fixed effects absorb anything that
shifts a student's bids for all jobs, including how the student values a
point.

Inference: the registered p-value comes from randomization inference, using
[10,000] re-draws of the registered assignment script and `beta` as the test
statistic. Standard errors clustered by student and a 95% confidence interval
are also reported.

Because first bids are sealed and submitted before any auction result, this
outcome cannot be affected by other students' market behaviour.

Sensitivity analyses:

- the same model without the baseline covariate;
- bids rank-transformed within job and section;
- the same model on untrimmed bids, and on bids trimmed at the 90th percentile;
- `willing_ij` as the outcome; and
- section-specific estimates.

### Effect of performing the job

Estimate the effect of actually performing the job by two-stage least squares,
with `assigned_ij` as the instrument for `performed_ij` in the H1 model. Report
the first stage and compliance rates by job and section.

### H2 and H3

Re-estimate the H1 model with `mean_bid_ij` (H2) and `success_count_ij` (H3) as
outcomes, using the same randomization inference. These outcomes are observed
after the market opens, so they include any effect that operates through
winning jobs, earning points, or changing clearing wages for other students.
They are interpreted as total effects of assignment within the section market.

### Heterogeneity

Estimate the H1 model with `assigned_ij` interacted with (a) the above-median
baseline comfort indicator and (b) the live/public job indicator. Report the
subgroup effects and the interaction.

### General effect of any practice

Not estimated in this design, because every student is assigned practice. If
a later term registers a no-practice arm: compare the student's mean first bid
across all jobs between students assigned any practice and students assigned
none, with section fixed effects and the student's mean baseline wage. This
comparison is between students and is secondary.

### Descriptive market analyses

Reported by section, without hypothesis tests:

- bid distributions by job and job family, and the within-student live/public
  minus prepared/artifact difference;
- valid-bid rates by 10-percentage-point bins of progress to the threshold;
- top-10% shares and Gini coefficients of paid live contributions and gross
  earnings, unique students paid per meeting, and the number of oversubscribed
  volunteer selections; and
- purchase and contribution rates, timing, and point shares by perk.

For fixed-price individual perks, a purchase establishes valuation at least as
high as the price only when the student had sufficient balance and was
eligible. A nonpurchase is treated as valuation below price only under the same
conditions.

Section comparisons carry no causal language. Two sections are not treated as
a cluster-randomized sample.

## 11. Multiple testing

H1 is the single primary confirmatory test at two-sided alpha 0.05. H2 and H3
are a secondary family; report raw p-values and Benjamini-Hochberg adjusted
q-values within that family. The two heterogeneity interactions are a separate
secondary family treated the same way. The effect of performing the job, the
general effect of any practice, and all market analyses are reported without
confirmatory claims. Anything else is exploratory and labeled as such.

## 12. Data freeze and reproducibility

- Software commit/image: [HASH]
- Job catalog version: [HASH OR APPENDIX]
- Perk configuration version: [HASH OR APPENDIX]
- Calibration freeze timestamp: [TIMESTAMP]
- Analysis plan commit: [HASH]
- Data collection close: [RULE AND DATE]
- De-identification procedure: [PROCEDURE]
- Data storage and access: [LOCATION AND ROLES]
- Retention/destruction schedule: [SCHEDULE]

Before analysis, export an immutable raw snapshot and an audit manifest with
row counts and cryptographic hashes. Write the cleaning script without viewing
outcome summaries beyond validation counts. Preserve a machine-readable table
of exclusions, corrections, and overrides.

## 13. Deviations

Any deviation is recorded with:

- date and decision maker;
- reason;
- whether outcome data had been viewed;
- affected students, sections, dates, or variables; and
- analyses with and without the deviation when possible.

Technical repairs may preserve the registered mechanism but may not silently
change prices, thresholds, allocation, payout, eligibility, or outcome
definitions.

## Appendix A: Calibration worksheet

Complete and freeze this table after the two-week training period and before
the first consequential bid.

| Quantity | Section 1 | Section 2 | Rule/source |
|---|---:|---:|---|
| Active enrollment | [ ] | [ ] | Roster at freeze |
| Remaining meetings | [ ] | [ ] | Course calendar |
| Named-job slots/meeting | [ ] | [ ] | Weeks 1-2 |
| Cold-call slots/meeting | [ ] | [ ] | Registered plan |
| Paid volunteer slots/meeting | [ ] | [ ] | Registered plan |
| Success rate | [ ] | [ ] | Weeks 1-2 |
| Try rate and multiplier | [ ] | [ ] | Weeks 1-2 / rule |
| Wage ceiling | none | none | Tentative |
| Per-class earning ceiling | [ ] | [ ] | Mechanism rule |
| Proposed threshold | [ ] | [ ] | Formula below |
| Earliest possible threshold class | [ ] | [ ] | `ceiling(T/E_max)` |
| Expected payroll/required payroll | [ ] | [ ] | Opportunity check |

Locked target formula:

```text
[INSERT EXACT FORMULA; NO DISCRETIONARY TERMS]
```

## Appendix B: Frozen job catalog

[INSERT NAMES, PURPOSES, TIMING, SLOTS, COMPLETION CRITERIA, JOB FAMILY, BID
CEILING, AND FALLBACK WAGE.]

## Appendix C: Frozen perk catalog

[INSERT PRICES, WINDOWS, CAPS, ELIGIBILITY, REFUND RULES, AND DELIVERY RULES.]

## Appendix D: Diagnostic-tag dictionary

[INSERT FINAL TAGS AND ONE-SENTENCE APPLICATION RULES.]

## Appendix E: Exposure schedule and baseline survey

[INSERT THE ASSIGNMENT SCRIPT COMMIT, SEED-HANDLING RULE, ARM SIZES BY SECTION
AND JOB, THE SCHEDULE HASH, AND THE FINAL BASELINE SURVEY INSTRUMENT.]
