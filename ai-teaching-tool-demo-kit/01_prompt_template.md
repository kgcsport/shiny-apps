# Prompt Template for an Agentic AI Coding Tool

Copy this prompt into Claude Code, Codex, Cursor, or another agentic AI coding tool. Fill in the fields before sending it.

## Reusable Prompt

```text
I want you to build the smallest working prototype of a classroom teaching tool.

This is for a specific instructor-owned classroom activity, not a commercial edtech platform. Build a local, inspectable tool that I can test with fake data before considering classroom use.

Course:
Class size:
Activity type:
Learning goal:
Student actions:
Instructor actions:
Public display:
Private instructor controls:
Scoring or payoff rules:
Data to save:
Data not to save:
Export format:
Preferred tech stack:
Local or hosted use:
Known constraints:

Requirements:
- Use fake data only.
- Do not ask for real student names, emails, grades, accommodations, demographic information, or private notes.
- Build a minimal working version first.
- Keep activity rules, parameters, and scoring logic transparent.
- Store raw data before summaries.
- Use local CSV, JSON, or SQLite storage unless I request something else.
- Create student-facing and instructor-facing views when needed.
- Include a reset or test mode.
- Include exportable raw data.
- Avoid unnecessary frameworks or external services.
- Write a README that explains how to install, run, test, reset, and export data from the prototype.

Please start by inspecting the project structure, then propose the smallest viable implementation plan. After that, implement it and test it with fake data.
```

## Completed Example: Public Goods Game

```text
I want you to build the smallest working prototype of a classroom teaching tool.

This is for a specific instructor-owned classroom activity, not a commercial edtech platform. Build a local, inspectable tool that I can test with fake data before considering classroom use.

Course: ECON101 Principles of Microeconomics
Class size: 30 students
Activity type: Public goods game
Learning goal: Help students see the tension between private incentives and group outcomes.
Student actions: Each fake student chooses how many tokens to contribute to a group account each round.
Instructor actions: Start a session, set number of rounds, set group size, set endowment, set multiplier, advance rounds, reveal group results, export data.
Public display: Round status, group totals, anonymized payoff summary, and class-level contribution trend.
Private instructor controls: Setup screen, round controls, reset/test mode, data export, and raw event log viewer.
Scoring or payoff rules: Each student starts each round with 10 tokens. Private payoff is tokens kept plus equal share of group contributions multiplied by 1.6.
Data to save: timestamp, session_id, round_number, participant_id, group_id, action, contribution, endowment, multiplier, group_total, payoff.
Data not to save: real names, emails, grades, demographic information, accommodations, or private notes.
Export format: CSV files for raw events and round summaries.
Preferred tech stack: R Shiny with local CSV or SQLite storage.
Local or hosted use: Local first.
Known constraints: Must run on my laptop with fake data. Student display should not show individual choices.

Requirements:
- Use fake data only.
- Do not ask for real student names, emails, grades, accommodations, demographic information, or private notes.
- Build a minimal working version first.
- Keep activity rules, parameters, and scoring logic transparent.
- Store raw data before summaries.
- Use local CSV, JSON, or SQLite storage unless I request something else.
- Create student-facing and instructor-facing views when needed.
- Include a reset or test mode.
- Include exportable raw data.
- Avoid unnecessary frameworks or external services.
- Write a README that explains how to install, run, test, reset, and export data from the prototype.

Please start by inspecting the project structure, then propose the smallest viable implementation plan. After that, implement it and test it with fake data.
```
