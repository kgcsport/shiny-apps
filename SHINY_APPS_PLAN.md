# Shiny Apps Plan

This repository is a classroom Shiny app suite for semester-long course systems and short in-class games. The long-run goal is one coherent teaching platform: students should be able to use the same identity, token/account balance, class section, and semester context across repeated class meetings, while instructors can still launch one-off games when a particular lesson needs them.

## Current Structure

- `apps/` contains deployable Shiny apps. Each app is self-contained enough to run under Shiny Server, but several apps share helpers and database conventions.
- `apps/class-job-market/` is the main semester-long classroom economy app. It combines login, student wallet/token state, job markets, live class tracking, gradebook-style views, and embedded classroom activities.
- `apps/_shared/` contains shared app infrastructure, including demo/sandbox login and database bootstrapping.
- Root deployment files such as `Dockerfile`, `docker-compose.yml`, and Shiny Server config define the Reclaim/GHCR deployment path.
- Planning and operational notes live in root markdown files. Keep detailed migrations and audits there rather than burying them in code comments.

## Product Direction

The app suite should support two kinds of classroom experiences:

- Semester-long systems: persistent mechanisms that accumulate meaning over weeks, such as token balances, jobs, gradebook records, public goods, flex questions, class sections, and instructor live tracking.
- One-off games: focused lesson activities that can be launched for a single class period, such as auctions, coordination games, quizzes, experiments, or market simulations.

The design principle is that one-off games should feed the semester-long structure when useful. A game can stand alone for a concept demo, but it should also be able to write participation, outcomes, tokens, reflection prompts, or completion records back into the course economy.

## Class Job Market

`apps/class-job-market/app.R` is currently the hub app. It should remain the place where an instructor can run the course economy during class from a phone or laptop.

Core responsibilities:

- Authenticate students and admins.
- Track students, sections, and demo/sandbox users.
- Maintain token ledger and job assignment state.
- Manage job categories, job posts, templates, voluntary jobs, draw eligibility, and start/end timing.
- Support live class workflows: draw jobs, reveal assignments by group, mark live scores, audit before committing, and redraw absent-student assignments.
- Provide student-facing views for current jobs, job pools, wallet state, and semester activities.

Future work should keep this hub coherent. Avoid turning every new classroom idea into a new top-level workflow if it can be represented as a job, event, token action, or one-off game result inside the semester economy.

## One-Off Games

One-off games should be built as reusable classroom modules with clear boundaries:

- Setup: instructor chooses parameters, section/group, and whether results should affect tokens or records.
- Play: students interact with the activity during class.
- Results: instructor sees a compact summary suitable for discussion.
- Integration: optional write-back to token ledger, participation events, job outcomes, or gradebook records.

When adding or revising a one-off game, prefer explicit handoff points to the semester app rather than implicit database side effects. The instructor should be able to tell whether a game is just for discussion or will affect persistent records.

## Demo Kit / Gamebuilder

`apps/demo-kit-site/` is a related side project for AI-assisted one-off game creation. It is a vibe-coded app for quickly generating classroom demo games with API keys, then using the generated app as a starting point for further development.

This should grow alongside the main Shiny suite, but it has a different role:

- Rapidly prototype one-off games or classroom demos before deciding whether they belong in the semester-long structure.
- Provide instructions for porting generated apps into Claude Code, Codex, or a similar coding workflow for continued development.
- Serve as an experimentation layer for new game ideas, UI patterns, and lightweight teaching tools.

Future work should make it easier to graduate successful Demo Kit outputs into the main app suite, with clear expectations about persistence, authentication, token write-back, and instructor controls.

## Sandbox/Demo Mode

Sandbox mode is for testing the live class experience without touching student records. It should mirror the main course setup where that helps testing:

- Job categories, templates, rounds, and job posts should be copied from the main class database.
- Demo users, scoring, token events, and live interactions should remain isolated in the demo database.
- Sandbox should be realistic enough to test phone workflows, live scoring, volunteering, revealing jobs, and draw behavior.

## Architecture Notes

- Keep schema changes idempotent with `CREATE TABLE IF NOT EXISTS` and guarded `ALTER TABLE` statements.
- Prefer shared helpers in `apps/_shared/` for cross-app login, demo behavior, and deployment-sensitive conventions.
- Keep app-level behavior near the relevant Shiny server/UI code unless it is genuinely reused across apps.
- Treat the SQLite database as the course state source of truth for now.
- Keep admin live workflows mobile-friendly: compact controls, low-friction confirmation for destructive actions, and clear audit/commit steps where mistakes are likely.

## Future Plans

- Multi-tenant architecture is a future platform direction, not the current app assumption. Use `MULTI_TENANT_TODO.md` as the planning document for `class_id` isolation, tenant-aware tables, per-course configuration, and Reclaim Cloud self-serve deployment.
- Polling/performance guidance belongs in `POLLING_AUDIT.md`. New live polling should follow that document's general pattern: avoid sub-second database polling unless a game truly requires it, document interval rationale, and keep Reclaim/Docker resource use in mind.
- Continue separating demo/sandbox behavior from production records while making sandbox setup mirror real course configuration.
- Build export/import and course reset workflows only after the multi-tenant shape is clearer.

## Near-Term Build Priorities

- Stabilize the class-job-market hub around live teaching workflows.
- Make job setup easier to maintain: fewer defaults, clearer timing, clean in-draw/voluntary controls, and sensible demo mirroring.
- Add write-back contracts for one-off games so participation and outcomes can feed the semester economy.
- Improve admin observability: what changed, who committed it, and what is still pending.
- Keep deployment predictable through GitHub/GHCR and Reclaim Docker Compose.
