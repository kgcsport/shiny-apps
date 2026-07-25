# Blank Classroom Activity Starter Spec

## Activity Overview

Describe the classroom activity, the learning goal, and why a custom tool is useful.

## Minimal Viable Version

List the smallest version that would let you test the classroom flow with fake data.

## Data Model

List the tables or files needed. Include raw event-level records before summaries.

Suggested fields:

- timestamp
- session_id
- round_number
- participant_id
- group_id
- action
- parameters
- outcome
- score_or_payoff
- notes

## Screens Needed

List only the screens needed for the first working prototype.

Possible screens:

- setup/configuration screen
- student-facing screen
- public display
- private instructor controls
- data export screen
- reset/test screen

## Testing Requirements

Describe what must work before trying the tool live. Include missing inputs, incorrect values, refresh/restart behavior, data export, and reset behavior.

## Export Requirements

Describe what raw data and summaries should be exportable. Prefer CSV, JSON, or SQLite unless there is a reason to use something else.
