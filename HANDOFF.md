# Handoff: Class Job Market, spring experiment

Written 2026-11-27 at commit `3554ffe` on `main`, for a new session starting
with no memory of the long chat that produced this. Read this first, then
`AGENTS.md` and `SHINY_APPS_PLAN.md` (Decision Log and Questions for Kyle).

## Who and what

Kyle teaches economics and runs a classroom points economy in this app
(`apps/class-job-market/app.R`, one large R Shiny file). Fall 2026 was a pilot
in one course (Econ 342, 15 active students). Spring is a preregistered
experiment in two Intro sections of 25 plus a Data Science section, 12 weeks,
two classes a week.

The research question: does being randomly given a job in a practice period
change what a student later bids to do that job?

## How the class runs. Get this right.

Kyle corrected these more than once. Do not drift from them.

- **Jobs pay students.** A bid is the fewest points a student would accept to
  do a job. Never describe a bid as a cost or price the student pays.
- **Every bid job, each class:** a wage is drawn at random. Everyone whose bid
  is at or below it is in the pool. One is picked at random. The winner is
  paid the **drawn wage**, not their bid. This is a BDM mechanism with a
  lottery among eligible bidders; in the app it is the `random_wage` pricing
  rule. Paying the winner's own bid is not incentive compatible; do not
  suggest it.
- **No anchor.** Students see no wage before bidding. The top of the draw is
  not announced. Any number students can see next to a bid box is a problem.
- **A blank bid** means the student is not in that job's market.
- **Unfilled is fine.** If no bid clears the drawn wage, the job stays empty.
- **Cold calls** are two bid jobs (answer a question; board work). The kind is
  chosen first, then a student is drawn from that job's pool.
- **Stepping in to answer** when no bid cleared a cold call's wage is a fill-in
  for that cold call: paid its drawn wage.
- **Volunteering means asking a question, nothing else.** It pays one token,
  once per class. Nobody bids on it and no wage is drawn for it.
- **A fair try pays half** of what the finished work would have paid. A miss
  pays nothing. A class may choose the lowest wage a draw can produce (0.5, or
  the wage floor) for a try instead (Settings → Tokens & spending → Economy
  settings). From 2026-09-09 until this session the app paid a flat 1 for a
  try; that was not the intended rule and is gone.
- **Practice dates** (first three weeks) run exactly like any other date,
  except everybody is in the pool whatever they bid, and no points are paid:
  students get feedback. There is no separate lottery, schedule, seed, or
  fixed no-practice group. Kyle rejected that as pageantry. Being drawn on a
  practice date is the random assignment.
- **Earned points** set the participation grade (15% of the course). Spending
  points never lowers the grade.

## How to work with Kyle

- Say things plainly and briefly. He said "just say things" when an answer was
  dressed up.
- Do not over-build. Twice this session a simple request became a mechanism
  he then had removed.
- When he states how something works, check the code for what it actually
  does before agreeing or explaining.
- He wants round numbers and few dials.
- Work on `main`, no branches. Do not open pull requests unless asked.
- Follow `AGENTS.md`: record decisions in the Decision Log, add Work Log
  entries, put open course decisions under Questions for Kyle, add a
  regression test with each change to `apps/class-job-market`, and never
  silently change economics or grading for a running class.
- His email and students' data are private. Uploaded exports must not be
  committed. Fall exports with a `student` column contain first names.

## Settled design

| Item | Decision |
|---|---|
| Mechanism | Random wage (BDM), lottery among bids at or below the wage, drawn wage paid |
| Wage range | One upper limit for every job, not announced. Draw is in half-point steps from 0.5 |
| Practice | Three weeks (six meetings); draws from the whole class; no points |
| Practice pay | Paid at term end at the average wage that job paid. Intro sections: from the other section. Data Science: from its own section |
| Core jobs (primary analysis) | Note taker, prior lecture summary, reading summary, cold call: answer, cold call: board work |
| Other jobs | Policy news every week (students prepare and bid). A generic "Odd job" for one-off roles. Discussion leader and skeptic are dropped |
| Cold calls | Drawn only from students whose bid cleared that job's wage; every answer is paid if right, half for a fair try |
| Questions | One token, once per class |
| Blank or above-limit bids | Recorded at the limit; shares reported; analysis rerun without them |
| Adds and drops | Kept in the sample; adds happen within the practice period |
| Pass Insurance | Off |
| Perk prices | One rule: price = rate x percentage points of course grade the perk is worth |
| Extension | Problem sets are 15% of the grade, six sets of 100 points, 2 points off per hour late, so an hour is worth 0.05 grade points |
| Final exam | 25% of the grade; students answer 10 of 12 five-point questions; 24 candidate questions posted to reveal; a revealed question appears with chance 6 / (11 + number revealed) |
| Reweighting | Stays in the app with an on/off switch and a toggle between a flat price and each student's own gain; cap of 5 points of weight |
| Perk windows | None. No purchases during practice |
| IRB | Exempt under 45 CFR 46.104(d)(1), confirmed 2026-11-15, no protocol number. Kyle will update the form: the design changed since he filed it |
| Registration | AEA RCT Registry (OSF also acceptable), by the week before the semester |
| Second-price auction | Kyle may run it as a teaching demonstration; the rule exists in the app. On experiment sections it is a deviation unless registered or run in the last week |

## Open decisions. Ask, do not assume.

1. **The economy's numbers.** Not settled. The last direction Kyle gave was a
   threshold of **100** and an extension of **40 points per 24 hours** (rate
   about 33.3 points per grade point). The upper wage limit to go with that is
   undecided; it depends on how many answers per class he pays for. Fall ran
   at about 14 answers a class; at that pace a limit near 18 gives an average
   participation grade near 83%, and fewer answers need a higher limit. Rerun
   the budget before recommending, because the half-pay and one-token rules
   changed since the last run.
2. **The preregistration, rehearsal plan, and app defaults still say limit 10,
   threshold 30, rate 10.** They are stale. Update them together once item 1
   is settled.
3. **Exam question pool:** across the course, or section pools with a separate
   exam per section. Students in different sections can share a revealed
   question, so section pools need section-specific exams.
4. **How much a preview helps** on an exam question. Provisionally 20% of the
   question's credit. Question prices are proportional to it.
5. **Reweighting in spring:** on or off, and flat or per-student pricing.
6. **Freeze dates:** a feature freeze and a full freeze. The first class date
   is not known to this document.
7. **Try pay in spring:** half the wage (the default) or the lowest draw. Not
   chosen for the experiment sections yet.

## What Kyle still has to supply

- The baseline survey (a Google Form). Ask for it as a printed PDF plus the
  response sheet's column headers.
- The IRB form update.
- Roughly 35 blanks in `CLASS_JOB_PREREGISTRATION_DRAFT.md` (institution,
  course and section labels, survey scale and index, data storage, and so on).

## What exists

### App (all on `main`, all with tests)

- `random_wage` pricing rule; redraws use the bids recorded at the first draw.
- Settings in seven pages; each opens with one line of help.
- Three levels of settings: section, course, app-wide. Economy settings save
  for the section.
- **Economy settings** (Settings → Tokens & spending): upper wage limit,
  full-credit points, tokens for asking a question, the price rate, course
  facts, the reweight pricing toggle, and the question pool choice. Apply
  writes the existing extension, exam-question, and reweight price settings.
- Wage and bidding rules (Settings → Class day): no-anchor switch, one paid
  question per class, cold calls from bidders only, a wage floor (unused, left
  at Kyle's request).
- Single-job draw; Redraw (nothing recorded against the student), Pass, Absent.
- Practice dates: draw from everyone, zero wage, end-of-term practice pay.
- Run sheet and deviations log on the fast tracker.
- Masked exports; research tables (student x job x date, with
  `practice_assigned` and `practice_jobs_n`; offers and cold calls; class
  deviations).
- Student-page preservation tests, required before any change to student
  navigation. Student navigation is frozen; do not reorganize it before spring.
- Shared change detection (one loop for the app) and a separate sandbox
  database per session.

### Fast pages (`student-lite/`)

A small separate service using the app's own rules. `/student-lite/` is the
student page (points, today's jobs once shown, bids). `/student-lite/tracker`
runs a class: run sheet, draw, show or hide, cold calls by kind, scoring,
undo, commit. Kyle has it running in production and said it works.

### Analysis (`analysis/`)

- `power_simulation.R`: smallest detectable effect about 0.28 SD with three
  sections, 0.34 with two, if a student's own level is 40% of bid variance.
- `currency_budget.R`: points issued per section. **Its volunteer assumptions
  are out of date** (it predates "volunteering is asking a question").
- `price_schedule.R`: the perk price arithmetic, also built into the app.
- Not written yet: the analysis script for the registered tests.

### Documents

- `docs/class-job-market/INSTRUCTOR_GUIDE.md`, `STUDENT_FAQ.md`
- `SPRING_REHEARSAL_PLAN.md` (stale numbers, see open item 2)
- `CLASS_JOB_PREREGISTRATION_DRAFT.md`, `CLASS_JOB_EXPERIMENT_DESIGN.md`,
  `CLASS_JOB_STUDENT_EXPLANATION.md`
- `POLLING_AUDIT.md`

### Operations

- `scripts/staging.sh up <image>`: staging on port 8081 with its own database.
- `scripts/reclaim.sh promote`: pins production to the newest built image.
  This is production, not staging.
- `scripts/reclaim.sh lite`: starts or updates the fast pages. The checkout and
  the promoted image must be the same commit.
- `scripts/backup.sh` and `scripts/backup.sh restore-test`: written, never run
  against Docker, cron, or rclone.
- Images build on pushes to `main` that touch `apps/`, `tests/`, `scripts/`,
  the Dockerfile, or the entrypoint.

## State of deployment

Unknown exactly. Kyle promoted some version during this session and started
the fast pages. Many commits since then are not promoted, including the
half-pay rule, which changes fall pay for tries at wages other than 2. Ask
what production is running before advising a promote. He planned to run the
current version for the last two weeks of fall.

## What fall data showed

- Half of fall bids were 20 or more (a bidding ring); the rest sat on the
  posted wage of 2. Fall bids cannot calibrate power.
- Two students held 32 of 59 drawn jobs under lowest-bid-wins.
- About 14 volunteered answers per active class from about 7 students, against
  1 or 2 questions asked. About 70% of fall points came from volunteering.
- Kyle says fall execution was loose: wage bidding started late and he took
  volunteers where a draw was planned.

## Not yet verified

- Backup script end to end.
- The Economy settings box for questions, and the help lines, in a browser.
- A course-wide question pool with students in two sections contributing.
- The main Live Tracker's "Draw for an answer" and "Draw for board work"
  buttons in a browser (the rule behind them is tested).
- A full practice day and paid day in the sandbox with spring settings
  (`SPRING_REHEARSAL_PLAN.md`).

## Suggested next steps

1. Settle the economy numbers with a fresh budget run, then update the
   preregistration, rehearsal plan, and guide together.
2. Get the survey; go through the preregistration blanks in one pass.
3. Write the analysis script against the research tables and run it on
   sandbox data.
4. Kyle runs the rehearsal; fix what it finds; freeze.

## Running the tests in a fresh cloud session

R is not preinstalled and CRAN is unreachable.

```bash
apt-get update && apt-get install -y r-base-core r-cran-dbi r-cran-rsqlite r-cran-shiny r-cran-jsonlite r-cran-digest r-cran-httpuv
```

The app needs `bcrypt`, which cannot be installed. Make a local stand-in
package with three functions and install it into a private library:

```r
gensalt <- function(log_rounds = 12) "stub"
hashpw  <- function(password, salt = gensalt()) paste0("stub$", password)
checkpw <- function(password, hash) identical(paste0("stub$", password), hash)
```

with `DESCRIPTION` (Package: bcrypt, Version: 0.0.1) and `NAMESPACE`
(`export(hashpw,checkpw,gensalt)`), then `R CMD INSTALL --library=<dir> bcrypt`
and run tests with `R_LIBS=<dir>`. If an apt package name above is wrong,
search with `apt-cache search r-cran`.

Run with a UTF-8 locale or two suites fail for encoding reasons only:

```bash
export LC_ALL=C.UTF-8 R_LIBS=<dir>
for f in tests/critical-*.R tests/smoke-class-job-market.R tests/student-lite.R; do Rscript "$f" >/dev/null 2>&1 && echo "ok $f" || echo "FAIL $f"; done
```

All 37 pass at this commit. Test notes: inside `shiny::testServer`, call the
app's top-level functions as `app$fn`; `<<-` does not reach outside the block,
so pass results out through an environment; a nested `testServer` is not
allowed.
