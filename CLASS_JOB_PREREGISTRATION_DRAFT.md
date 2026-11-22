# Preregistration Draft: Class-Job Labor Market

Version date: 2026-11-12

Status: registration draft. It must not be registered until all bracketed
fields are resolved, the software is frozen and tested, and institutional
review requirements are satisfied.

## 1. Title

Reservation Wages, Classroom Work, and Perk-Backed Participation Points in an
Introductory Course

## 2. Investigators and setting

- Principal investigator/instructor: [NAME]
- Institution: [INSTITUTION]
- Course: [COURSE NUMBER AND TITLE]
- Term: [TERM]
- Sections: [SECTION LABELS]
- Expected enrollment: [N BY SECTION]
- Meetings: [DAYS, NUMBER OF MEETINGS, AND LENGTH]
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

- Institutional determination: [IRB PROTOCOL / EXEMPTION / NOT-RESEARCH
  DETERMINATION]
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

Primary questions:

1. How do students' minimum acceptable wages differ across authentic classroom
   jobs?
2. How concentrated are paid classroom work and token earnings under the
   auction and live-participation limits?
3. Do students continue to submit informative bids as gross earnings approach
   the participation threshold?
4. How are spendable points used across individual and collective class perks?

Secondary questions:

1. How do bids change after prior experience and feedback on the same job?
2. How do bids, completion, purchases, and threshold timing differ descriptively
   between sections?
3. How does the distribution of unpaid volunteering compare with paid live
   participation?

The design is a repeated-measures observational market study. Auction assignment
is endogenous to bids. Except for explicitly randomized tie-breaking and
oversubscribed volunteer selection, analyses are not interpreted as causal
treatment effects.

## 5. Confirmatory hypotheses

All directional hypotheses must be retained, revised, or removed before
registration. A removed hypothesis cannot be restored after observing outcome
data.

### H1: Job-specific reservation wages

Within students, minimum acceptable wages differ across pre-specified job
families. The primary contrast is:

> live/public-performance jobs versus prepared or artifact-producing jobs.

Direction: live/public-performance jobs have higher median bids.

Pre-specified classification:

- Live/public: [JOB LIST]
- Prepared/artifact: [JOB LIST]
- Excluded/other: [JOB LIST]

### H2: Experience and subsequent bids

After a prior `Success` on a job, a student's next bid for the same job is lower
than the student's prior bid for that job.

This is interpreted as a within-student association, not a causal learning
effect, because job experience is selected by bids and allocation.

### H3: Continued bidding near the threshold

The probability of submitting a valid bid remains positive after a student has
earned at least 80% of the participation threshold.

Primary estimand: the valid-bid rate for eligible student-job opportunities at
less than 80% versus at least 80% of the threshold. This is descriptive; perk
demand and time trends may both affect the comparison.

### H4: Concentration safeguard

No student receives more than the pre-specified per-class paid-live limit, and
the semester share of paid live contributions held by the highest-earning 10%
of students is [BENCHMARK OR MARK AS DESCRIPTIVE].

Unless an external benchmark is specified before registration, the second
part is descriptive and not a hypothesis test.

## 6. Course mechanism

### 6.1 Training period

The first [NUMBER; PLANNED AS TWO] weeks contain no bids, purchases, or token
payouts. Students practice authentic jobs and receive one outcome plus at most
two diagnostic tags.

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

- Bidding begins: [DATE / WEEK]
- Bid unit: participation points
- Bid step: [STEP]
- Maximum wage bid: [B]
- Above-cap response: `Not willing at this wage`
- Allocation: lowest valid bids receive the posted slots
- Pricing: uniform reverse-second-price
- Wage: first unsuccessful bid, bounded by the registered maximum
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

- Course-grade weight: [W] percentage points
- Full-credit threshold: [T] gross points
- Mapping below the threshold:

```text
participation contribution = W * min(gross earned points / T, 1)
```

- One point's course-grade equivalent below the threshold: `W / T`

The student interface displays gross earnings, spendable balance, progress to
the threshold, and the grade equivalent below the threshold.

### 6.6 Paid live participation

- Paid answer slots per meeting: [NUMBER OR SCHEDULE]
- Paid board-work slots per meeting: [NUMBER OR SCHEDULE]
- Per-student paid-live limit per meeting: [LIMIT]
- Wage: frozen clearing wage for the corresponding cold-call job
- Oversubscription: random selection with priority for fewer prior paid live
  contributions
- Contributions after paid slots fill: permitted but unpaid
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

Pass Insurance is [REMOVED / RETAINED; CHOOSE BEFORE REGISTRATION]. If removed,
problem-set extensions remain deterministic purchases and are not insurance.

## 7. Sample

### Inclusion

- enrolled students in the registered sections;
- research consent or other approved basis for inclusion;
- at least one eligible post-training class meeting.

### Exclusion

- instructors, administrators, demo users, and synthetic accounts;
- students who decline research participation;
- students who withdraw before [PRE-SPECIFIED DATE OR RULE];
- records produced during documented app testing; and
- observations outside the frozen course/section/date scope.

Enrollment determines sample size. No enrollment-based stopping rule is used.
Final sample counts and exclusions will be reported in a flow table by section.

### Power and precision

Before registration, run a simulation using final expected enrollment, job
counts, meetings, within-student bid correlation, and plausible bid dispersion.
Attach code and report the minimum detectable standardized within-student job
contrast for H1 at 80% power and two-sided alpha 0.05. If confirmatory power is
inadequate, label the study a descriptive pilot rather than changing the sample
after outcomes are observed.

## 8. Variables

### Primary outcomes

1. `min_wage`: submitted minimum acceptable wage for an eligible student-job
   opportunity.
2. `valid_bid`: indicator for a numeric bid within bounds; `not willing` is a
   distinct registered response.
3. `paid_live_count`: paid live contributions per student and meeting.
4. `gross_earned`: cumulative positive grade-relevant earnings.
5. `perk_spending`: point debits and contributions by perk.

### Secondary outcomes

- assignment and job family;
- clearing wage and pricing fallback;
- Success/Try/Fail/Miss/Excused;
- diagnostic tags;
- spendable balance;
- unpaid live contributions;
- threshold attainment date;
- instructor override; and
- technical-error indicator.

### Pre-specified covariates

- section;
- class date or meeting index;
- job identity and job family;
- prior assignment and prior outcome for the same job;
- cumulative paid opportunities before the meeting;
- gross earnings as a fraction of threshold before bidding; and
- [OTHER PRE-TREATMENT COVARIATES, OR NONE].

Do not use final course performance, later grades, or post-market behavior as
control variables in the primary bid models.

## 9. Missing and exceptional records

- A saved `Not willing at this wage` response is not a missing bid.
- No saved response is missing unless logs establish a technical failure.
- Missing bids are not imputed.
- A Try/Fail/Miss is an observed outcome, not missing completion data.
- Excused absences are excluded from completion-rate denominators and reported.
- Technical duplicate ledger entries are resolved using the app's idempotency
  keys before dataset freeze and documented in an audit table.
- Instructor overrides remain in the dataset with their reasons. Primary
  models use final operational records; sensitivity analyses exclude overrides.

## 10. Analysis plan

### H1

Fit a student fixed-effect model among valid numeric bids:

```text
min_wage_ijt = student_FE_i + job_family_j + meeting_FE_t + error_ijt
```

Primary coefficient: live/public minus prepared/artifact. Use standard errors
clustered by student. Report the coefficient, 95% confidence interval, within-
student standardized effect, medians, and full bid distributions.

Sensitivity analyses:

- mixed-effects model with student random intercept;
- ordinal/censored model including `Not willing at this wage` above the cap;
- section-specific estimates; and
- exact job indicators instead of job families.

### H2

Among consecutive valid bids by the same student for the same job, calculate:

```text
next bid - prior bid
```

Compare changes after prior Success with changes after other observed outcomes,
including student-job fixed effects and meeting index. Report as association.

### H3

For every eligible student-job opportunity, compare valid-bid rates when the
student begins the bidding window below 80% versus at or above 80% of `T`.
Estimate a linear probability model with student and meeting fixed effects and
job controls. Also plot bid rates in 10-percentage-point threshold bins.

### H4 and concentration

Verify the per-class cap directly. Report, by section and over time:

- top-10% share of paid live contributions;
- top-10% share of gross earnings;
- Gini coefficients for both distributions;
- unique students paid per meeting; and
- the number of oversubscribed volunteer selections.

Unless a benchmark is filled above, these are descriptive.

### Perk demand

Report purchase/contribution rates, timing, and point shares by perk. For fixed-
price individual perks, a purchase establishes valuation at least as high as
the price only when the student had sufficient balance and was eligible. A
nonpurchase is treated as valuation below price only under the same balance and
eligibility conditions. Do not label other nonpurchases as zero willingness to
pay.

### Section comparisons

Report identical summaries by section. Do not attach causal language or use
the section count as if it were a large cluster-randomized sample.

## 11. Multiple testing

H1 is the primary confirmatory test at two-sided alpha 0.05. H2 and H3 are a
secondary family; report raw p-values and Benjamini-Hochberg adjusted q-values
within that family. H4 is confirmatory only if a numeric benchmark is registered;
otherwise it is descriptive. All other analyses are exploratory and labeled as
such.

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
| Wage ceiling | [ ] | [ ] | Same unless justified |
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
