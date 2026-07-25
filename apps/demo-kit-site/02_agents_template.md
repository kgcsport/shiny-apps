# Generic AGENTS.md or CLAUDE.md Template

Copy this file into a new classroom tool project as `AGENTS.md`, `CLAUDE.md`, or the equivalent instruction file for your agentic AI coding tool.

## Project Goal

You are helping build a lightweight classroom teaching tool. The tool may be a game, simulation, classroom activity, randomizer, scoring system, polling activity, market exercise, or instructor dashboard.

The goal is not to build a commercial edtech platform. The goal is to build a reliable, inspectable, instructor-controlled tool for a specific classroom routine.

## Design Priorities

1. Fit the instructor's teaching goal.
2. Build the smallest working version first.
3. Prefer boring, readable code.
4. Use fake data during development.
5. Do not require real student PII.
6. Separate public student-facing displays from private instructor controls.
7. Keep rules, parameters, and scoring logic easy to inspect.
8. Store raw event-level data.
9. Export plain CSV or SQLite tables.
10. Document setup, testing, and classroom use.

## Privacy Requirements

Do not ask for real student names, emails, grades, accommodations, demographic information, or private notes. Use fake data and placeholder IDs such as `S001`, `S002`, and `S003`.

Keep the mapping between real students and local IDs outside the AI workflow. Do not add analytics, telemetry, external APIs, or cloud services for classroom data unless the instructor explicitly asks for them and understands the privacy implications.

## Data Principles

Record raw events before summaries. Keep raw data inspectable. Do not overwrite original records when scoring rules change.

A useful event log might include:

```text
timestamp
session_id
round_number
participant_id
group_id
action
parameters
outcome
score_or_payoff
notes
```

Use CSV, JSON, or SQLite when possible. Prefer clear table names and plain field names over opaque storage.

## Interface Principles

Where relevant, separate:

- public display mode;
- private instructor controls;
- setup/configuration screen;
- data export screen;
- test/reset mode.

The student-facing display should avoid showing private information. Instructor controls should make the current state of the activity clear.

## Testing Requirements

Use fake data to test the complete classroom flow. Test setup, launch, student actions, instructor actions, missing inputs, scoring, exports, reset behavior, and recovery after refresh or restart.

## Documentation Requirements

Document:

- how to install dependencies;
- how to run the tool locally;
- how to load fake data;
- how to reset test data;
- how the rules and scoring work;
- where data are stored;
- how to export raw data and summaries;
- what should be tested before classroom use.

## What To Avoid

Avoid:

- hardcoded real student names;
- hidden scoring logic;
- opaque databases;
- unnecessary authentication;
- external APIs for classroom data;
- over-engineered frameworks;
- tools that only work in one instructor's private environment.
