# Adaptation Guide

Generated classroom tools are most useful when instructors can adapt them. Make one change at a time, test with fake data after every major change, and keep a copy of the last working version.

## Changing Activity Rules

Change the visible rules and the code that enforces them together. For example, if a game changes from five rounds to eight rounds, update both the instructor setup screen and the validation logic.

Ask the AI agent to point to the exact files and functions that define the rules.

## Changing Scoring Or Payoff Formulas

Keep formulas easy to inspect. Put parameters such as multipliers, weights, caps, or bonus values in a configuration area rather than burying them in unrelated code.

Do not overwrite original records when grading or scoring rules change. Save raw choices first, then recalculate summaries from those raw records.

## Changing What Students See

Student-facing screens should show only what students need for the activity. Avoid showing private instructor notes, full rosters, individual scores, or other students' private choices unless that is intentionally part of the activity.

## Changing What The Instructor Sees

Instructor-facing controls should show setup, current state, next actions, reset/test controls, and exports. If a control affects saved data, label it clearly.

## Changing Stored Data Fields

Add fields deliberately. Useful fields often include `timestamp`, `session_id`, `round_number`, `participant_id`, `group_id`, `action`, `parameters`, `outcome`, and `score_or_payoff`.

Keep raw data separate from summaries. Avoid replacing raw events with aggregate-only records.

## Changing Export Formats

CSV is easiest to inspect. SQLite is useful when the tool has multiple related tables. JSON is useful for nested settings or event parameters.

After changing exports, open the exported file manually and confirm that the fields, rows, and values make sense.

## Replacing Fake Data With Real Local Data

Do not put real student PII into AI prompts. Keep any real roster on an instructor-controlled machine or server. If possible, use local IDs in the app and keep the student-name mapping outside the app.

Before live use, delete test records and confirm that the app is reading the intended local data source.

## Testing After Each Change

After every major change, test:

- launch;
- fake data loading;
- student actions;
- instructor controls;
- scoring or outcomes;
- exports;
- reset behavior;
- recovery after refresh or restart.

## Preserving A Working Version

Keep a copy of the last working version before asking the AI agent for major changes. Use Git commits, copied folders, or tagged releases. If a revision breaks the tool, return to the last working version and make a smaller change.
