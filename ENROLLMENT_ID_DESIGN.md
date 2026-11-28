# Enrollment IDs and the Cloudflare gradebook sync: design

Draft, 2026-11-28. Requested by Kyle. Nothing here is built. It is a design to
react to; the open questions at the end decide its shape.

## The problem

Records are tied together by text that was never meant to be an ID.

| Thing | Identified today by | What goes wrong |
|---|---|---|
| A person | `users.user_id` (their sign-in email) | Fine. This stays. |
| A course | The course name as typed, e.g. `ECON 342` | No term. Spring's ECON 342 is the same "course" as fall's; old students are kept apart only by an `active` flag. |
| A student in a course | `user_environments(user_id, course, section)` | Section is part of the key, so a section change looks like a new membership. Nothing else refers to it. |
| A grade | course + student + item **name** | Works since 2026-11-27, but the item is still a name. |
| Tokens, reweights, exam drops, sync overrides, extension purchases | student alone | A student in two courses has one balance, and a reweight in one course applies in the other. |
| Problem sets (extension products) | one list for the whole app | Every course sees the same problem sets. |
| A Worker assignment → gradebook item | A pattern match on the title ("problem set 3" → `PS3`), else the name | Breaks on any item that is not a numbered problem set. |
| A Worker student → app student | `externalId` equal to the email, else **display name** | Two students with the same name, or a nickname, file a grade under the wrong person. |
| Which course a Worker belongs to | Nothing. One Worker address for the whole app | A second course cannot use the sync. |

## The design: three IDs

**1. Offering.** One teaching of a course in a term.

```
course_offerings(
  offering_id   TEXT PRIMARY KEY,   -- e.g. econ342-2026fa, written once, never changed
  course        TEXT NOT NULL,      -- the name shown and used today, e.g. ECON 342
  term          TEXT NOT NULL,      -- e.g. 2026fa
  active        INTEGER DEFAULT 1,
  worker_origin TEXT                -- this offering's Cloudflare Worker, if it has one
)
```

**2. Enrollment.** One student in one offering. This is the "course-user ID".

```
enrollments(
  enrollment_id INTEGER PRIMARY KEY,
  offering_id   TEXT NOT NULL,
  user_id       TEXT NOT NULL,
  section       TEXT,               -- a fact about the enrollment, not part of its identity
  status        TEXT DEFAULT 'active',   -- active, dropped
  added_at      TEXT, dropped_at TEXT,
  UNIQUE(offering_id, user_id)
)
```

A section change updates one row. A drop marks one row. An add within the
practice period is a new row with its date, which the experiment needs anyway.

**3. Item key.** A short fixed name for a gradebook item within an offering,
e.g. `ps3`, `final`, `policy-brief`. The item's display name can change; the
key does not. It replaces every name match.

The person's ID stays the sign-in email. The masked research ID stays derived
from the person, so one student has one masked ID across courses.

## What each record hangs on afterwards

| Record | Key |
|---|---|
| Grade | enrollment + item key |
| Reweight request, exam drop, sync override, extension purchase | enrollment |
| Gradebook categories and items | offering |
| Problem sets / extension products | offering |
| Exam question funds | offering (+ section if section-specific) + exam item key |
| Token ledger | enrollment, **if** tokens are per course (open question 2) |
| Class dates, jobs, bids, draws | unchanged: already tied to a class date, which belongs to a course and section |

## The sync contract with Cloudflare

Each offering has its own Worker address (or one Worker serves several and
says which offering each row is for). The export the app reads carries, per
row:

```
offeringId    econ342-2027sp
itemKey       ps3
externalId    the student's sign-in email
assignmentId  the Worker's own ID, kept for the log
score, maxPoints, gradePct, status, submittedAt   (as now)
```

The app then files a grade with two exact lookups and no guessing:

1. enrollment = (offeringId, externalId). No match: the row is reported as
   unmatched. **No fallback to display name.**
2. item = (offeringId, itemKey). No match: reported as unmatched. **No pattern
   match on the title.**

The extension push to the Worker gains `offeringId` and uses the same email.
The admin token moves from one app-wide secret to one per offering.

I have not seen the Worker's code; it is not in this repository. Whether it
can send `offeringId` and `itemKey`, and whether `externalId` is always the
sign-in email, needs checking there.

## Getting there without breaking the running class

Each phase is separately staged and has its own migration test on a fall-style
database. None changes what a student in one course sees.

1. **Tables only.** Create offerings and enrollments and fill them from today's
   users and memberships. Nothing reads them yet. No behavior change.
2. **Item keys and the sync.** Add `item_key` to gradebook items, filled from
   today's names. The sync uses offering + item key when a row carries them and
   the current matching otherwise, and lists every row that needed the old
   matching, so the Worker can be fixed row by row. Then remove the old matching.
3. **Student-only records move to the enrollment:** reweights, exam drops,
   sync overrides, extension purchases, and problem sets per offering.
4. **Tokens,** if they are to be per course.
5. **Terms.** Start each new term as a new offering instead of reusing the
   course name and archiving students.

Phases 1 and 2 are what a second course needs to use the sync. Phases 3 to 5
only matter once a student is in two courses or a course runs a second term in
the same database.

## Open questions for Kyle

1. **Terms.** Should spring's ECON 342 be a new offering, separate from fall's,
   with fall's grades and tokens kept under the fall offering? (I recommend yes.)
2. **Tokens.** One balance per course, or one wallet a student carries across
   courses? Per course is cleaner for the experiment: points earned in one
   section's economy cannot be spent in another's.
3. **Workers.** One Worker per course, or one Worker for all courses that
   labels each row with its offering?
4. **The Worker's student ID.** Is `externalId` always the sign-in email? If
   students sign in to the Worker with a different address, enrollment needs a
   second, verified ID.
5. **Timing.** Will a spring course other than ECON 342 use the sync? If yes,
   phases 1 and 2 are needed before the feature freeze. If no, all of this can
   wait until after spring's freeze lifts.
