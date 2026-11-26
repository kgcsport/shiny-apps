# Spring rehearsal plan

One fake practice day and one fake paid day, run in the demo sandbox with the
spring settings, before the software is frozen. It takes about an hour. The
goal is to find a wrong setting or a missing column now, not in week 2.

Run it on the version you intend to freeze. If a step fails, stop and note
what you saw; do not work around it.

## Before you start

- [ ] The version under test is promoted to staging or production.
- [ ] You are signed in to the demo sandbox as the instructor, with at least
      six sandbox students in one section.
- [ ] A second browser (or a phone) is signed in as a sandbox student.

## 1. Settings (10 minutes)

Settings → Tokens & spending → Economy settings, for the sandbox section:

- [ ] Upper wage limit 10, full participation credit 30, volunteer wage 1,
      rate 10. The preview shows 0.5 points per extension hour.
- [ ] Reweighting pricing and the exam-question pool are set the way you
      registered them. Press Apply.

Settings → Class day → Wage and bidding rules:

- [ ] Wage auction pricing is the random-wage rule.
- [ ] "Hide instructor wages from students until they bid" is on.
- [ ] "Pay each student for at most one voluntary contribution per class" is on.
- [ ] "Draw paid cold calls only from students who bid" is on.
- [ ] "Lowest wage a random-wage draw can produce" is 0.

Settings → Jobs:

- [ ] The five core jobs, policy news, and the volunteer jobs are posted. Their
      own wage numbers no longer matter: the upper wage limit sets the draw.

Settings → Tokens & spending → What students can spend tokens on:

- [ ] Pass Insurance is off. Reweighting is on or off as registered.

As the student:

- [ ] The bid boxes are empty and no wage appears anywhere on Today or Job Market.

## 2. Practice day (15 minutes)

- [ ] Mark the class date as practice (no points).
- [ ] Student: enter bids on two jobs, leave one blank. They save.
- [ ] Fast tracker: the run sheet shows five steps, all "to do".
- [ ] Draw jobs. Jobs go to students at random, including ones who did not bid.
- [ ] Show jobs to students. The student sees their job with instructions and
      no wage.
- [ ] Mark one student Absent on a job. It is redrawn to someone else.
- [ ] Draw a cold call for an answer, then for board work. Anyone can be drawn.
- [ ] Record two volunteers.
- [ ] Score every job, then Commit.
- [ ] No student's points changed. The run sheet shows every step done.

## 3. Paid day (20 minutes)

- [ ] Move to a new class date that is not practice.
- [ ] Students: three bid low (1 to 3), two bid high (9 to 10), one leaves a job
      blank.
- [ ] Draw jobs. Only students whose bid is at or below the drawn wage hold
      jobs; some jobs may be unfilled, and that is expected.
- [ ] Each job's wage is the drawn wage, not the winner's bid.
- [ ] Draw one unfilled job on its own.
- [ ] Redraw one job. The first student has no absence recorded.
- [ ] Draw for an answer: only a student who cleared the answer wage is drawn.
      Record it. Try "Draw for board work" as well.
- [ ] Record three volunteers, one of them twice. The second is unpaid.
- [ ] Log one deviation ("Took a volunteer instead of a cold call").
- [ ] Score every job, Commit. Points match the drawn wages and 1 per volunteer.
- [ ] Student: buys a 6-hour extension for 3 points and contributes to an exam
      question.

## 4. Exports (10 minutes)

Settings → Data & tools, with "mask student IDs" ticked:

- [ ] Student × job × class date: one row per student, job and date;
      `practice_assigned` is 1 for the jobs drawn on the practice day;
      `bid_at_draw` and `in_lottery` are filled on the paid day; a blank bid is
      blank.
- [ ] Offers and cold calls: each cold call is on its own job (answer or
      board), with the wage and the student's bid.
- [ ] Class deviations: the one you logged, with its time.
- [ ] Participation events: the volunteers and cold calls.
- [ ] No file contains a name or an email address.

## 5. Analysis (5 minutes)

- [ ] Send the masked files; the analysis script reads them and produces the
      primary estimate without errors. (The script is still to be written.)

## 6. Afterwards

- [ ] Run `scripts/backup.sh`, then `scripts/backup.sh restore-test`.
- [ ] Note the commit you tested. That commit is what gets frozen.

## Freeze dates

- Feature freeze: [DATE, two weeks before the first class]. After this, only
  fixes for something this rehearsal or the fall class turned up.
- Full freeze: [DATE, the week before the first class, with registration].
  After this, production stays on one image for the term; a fix needs a test
  and a note in the deviations section of the paper.
