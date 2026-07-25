# AI Teaching Tool Demo Kit

## Purpose

This kit helps instructors use agentic AI to build small, instructor-owned classroom games and teaching tools.

Examples include:

- public goods games;
- auctions;
- prediction tasks;
- matching games;
- policy calculators;
- simulations;
- randomized discussion tools;
- participation or class jobs tools;
- scoring dashboards.

## Main Claim

The goal is not to build a commercial-grade edtech platform. The goal is to build local teaching software that fits a specific course activity.

AI-generated apps often fail as consumer products because they lack polish, maintenance, discoverability, support, and scalable quality control. Classroom teaching tools operate in a different context. They serve known users, bounded classroom routines, and limited maintenance horizons. The instructor can explain the activity live, inspect the rules, test the data flow, and revise the tool after use.

In this setting, good enough software can be genuinely useful. The key advantage is not market scale. The key advantage is instructor control.

The instructor controls:

- the activity rules;
- the student-facing display;
- the instructor controls;
- the scoring or payoff logic;
- the data structure;
- the privacy boundary;
- the exports;
- the revision process.

Use fake data during development. Do not paste real student personally identifiable information into AI tools. Test the output before classroom use. Store data locally or on an instructor-controlled system. The instructor should be able to inspect and export the data.

## Who This Is For

The kit is for instructors who:

- have some coding or data-management familiarity;
- want more control than fixed edtech platforms provide;
- want to run classroom activities that are not well supported by existing tools;
- want inspectable rules and exportable data;
- are willing to test a tool before using it live.

## What This Is Not

The kit does not:

- guarantee production-grade software;
- replace careful testing;
- remove the need for instructor judgment;
- require uploading real student data to AI tools;
- assume one specific tech stack.

## Recommended Workflow

1. Pick a classroom activity.
2. Fill out `01_prompt_template.md`.
3. Use fake data.
4. Ask an agentic AI coding tool to build the smallest working prototype.
5. Test with `04_testing_checklist.md`.
6. Revise based on failures.
7. Complete `05_review_form.md`.
8. Only then consider live classroom use.

## Optional Examples From the Parent Repository

This folder was incubated inside `kgcsport/shiny-apps`, which contains instructor-built Shiny teaching tools. These are optional examples only; the kit does not depend on them.

- `apps/supply-auction-game`: auction-style classroom activity for supply and bidding behavior.
- `apps/class-job-market`: class jobs or participation market that illustrates matching incentives and student choices.
- `apps/class-job-picker`: lightweight tool for assigning or selecting classroom jobs.
- `apps/excise-tax-game`: tax policy activity with instructor-controlled parameters and outcomes.
- `apps/coordination-games`: coordination-game examples for strategic interaction.
- `apps/review-quiz`: quiz-style classroom review tool.
- `apps/club-insurance-game`: classroom game for insurance, risk pooling, and group decision-making.

Avoid copying private data or course-specific assumptions from existing apps into a new tool. Treat them as design references, not templates that must be followed.
