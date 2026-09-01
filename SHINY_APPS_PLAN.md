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

- [x] Stabilize the class-job-market hub around live teaching workflows. *(2026-08-26: round setup, template auto-copy, and settings-panel reactivity fixed; see Work Log.)*
- [x] Make job setup easier to maintain: fewer defaults, clearer timing, clean in-draw/voluntary controls, and sensible demo mirroring. *(2026-08-26: simplified 4-category / 11-template catalog with one-time migration; template-level timing/voluntary/in-draw/auto-copy controls.)*
- [ ] Add write-back contracts for one-off games so participation and outcomes can feed the semester economy.
- [ ] Improve admin observability: what changed, who committed it, and what is still pending.
- [ ] Keep deployment predictable through GitHub/GHCR and Reclaim Docker Compose.

## Current State — Class Job Market (2026-08-26)

This section documents the expected behavior of `apps/class-job-market` after the simplification pass (branch `claude/class-job-market-simplify-szx2ai`). Treat it as the reference for how the job market is supposed to work.

### Job catalog

Four job categories. Categories are the level students bid on; whether a job is drawn or volunteered is a post/template-level flag, not a category.

| Category | Voluntary | In draw | Purpose |
|---|---|---|---|
| Class roles | no | yes | Recurring assigned per-class jobs |
| Answer a question | no | yes | Cold call or volunteer |
| Ask a question | yes | no | Volunteer only |
| Board work | no | yes | Graph/answer on the board — cold call or volunteer |

Eleven seeded templates. Templates carry name, category, slots, wage, timing, voluntary, in-draw, and an Auto-copy flag (`active`):

- Every class, auto-copy ON, assigned at start of class: **Materials summary**, **Last class recap**.
- Every class, auto-copy ON, assigned after class: **Note taker**, **Critic/skeptic**, **Policy/example scout**.
- Some sessions, seeded auto-copy OFF (toggle on for rounds that need them): **Discussion lead** (end), **Cold call: answer a question** (during), **Cold call: graph/answer on board** (during).
- Volunteering, auto-copy ON, never drawn, 99 slots: **Volunteer: answer a question**, **Volunteer: ask a question**, **Volunteer: graph/answer on board**.

Timing codes on posts/templates: `any` / `start` / `during` / `end` / `volunteer`. All names/wages/flags are editable in Settings → Jobs.

A one-time migration (guarded by the `job_catalog_v2_migrated` key in `labor_settings`) runs at app startup: old seeded per-job categories are merged into the new four (posts, templates, and bids move with them, then the old categories are deleted), and old seeded job names are deactivated. Instructor-created categories are untouched. After the marker is set, restarts only insert missing rows and never overwrite instructor edits.

### Rounds

A **round = one class session**. Settings → Round Setup:

- Create/edit/delete rounds (label, assignment mode, bid window dates, tickets, tie-break, delayed token reveal).
- **Create next round**: increments the label, carries the previous round's mode/tie-break/tickets/token settings, and copies every Auto-copy template as a job post with its timing/wage/slots/voluntary/in-draw flags.
- Assignment modes: `random` (first ~2 weeks), `application_bidding` (point/ticket bids), `wage_bidding` (lowest-wage bids).

### Bid lock schedule

Bidding is continuous but locks around class sessions (Settings → Round Setup → Bid Lock Schedule; stored in `labor_settings`):

- Defaults: class days Mon/Wed, class starts 12:00, lock 60 minutes before (11:00 AM), reopen 5:00 PM, timezone America/New_York, enabled.
- Enforced server-side on both wage-bid and ticket-bid submission; students see a lock banner (and the schedule when open) in the Job Market tab. Admins are exempt.

### Draws and cold calls

Live Tracker → Job Assignments:

- Draw filter: All timings / Start of class / During class (cold call) / End-post class.
- The **All timings** draw clears and redraws the full round but **excludes `during` posts** — cold calls are drawn live in class with the During filter, which draws incrementally among students who do not yet have a job this round (one job per student per round is enforced by the DB).
- In `wage_bidding` mode, draws take the cheapest bids per category and pay each drawn student their bid. Tie-breaks per the round's method.

### Volunteer clearing wage

In `wage_bidding` rounds, every volunteer in a category is paid the **same equilibrium wage** derived from that round's bids — never their own bid, and nobody is rationed out. Rule selected in Settings → Jobs → Volunteer Clearing Wage (`volunteer_clearing_rule`):

1. **Lowest bid** (default): the cheapest bid in the category (k = 1).
2. **Demand-based**: the k-th lowest bid, k = the volunteer post's slots (standing demand).
3. **Posted demand**: the k-th lowest bid, k = expected demand posted per round in the Live Tracker's Voluntary Participation panel (e.g. at the start of class); falls back to post slots until posted. Stored in `volunteer_demand(round_id, category_id)`.

k is capped at the number of bids; with no bids the post's default wage is used. Students see "Volunteer Wages This Round" on the Job Market tab in wage-bidding rounds. Because bids lock before class, the clearing wage is fixed for the session. In `random`/`application_bidding` rounds, volunteers earn the post's default wage.

### Fixed bugs (context for why things looked broken before)

- The admin Settings panel now refreshes after every mutation (rounds/categories/posts/templates); previously most handlers never invalidated it, so create/delete looked like no-ops.
- The student job poll watched a nonexistent `weekly_rounds.updated_at` column; new rounds/posts never appeared. It now keys on round/post/demand counts and ids.
- `ALTER TABLE weekly_rounds` migrations ran before its `CREATE TABLE`, so fresh databases lacked `tokens_revealed`/`tiebreak_method` and round creation failed until a restart.

### Testing

`tests/smoke-class-job-market.R` (run `Rscript tests/smoke-class-job-market.R` from repo root; needs DBI + RSQLite) exercises the seed migration, template auto-copy, idempotence across restarts, bid-lock windows, and all three clearing-wage rules against a scratch SQLite DB by extracting the relevant functions from `app.R`. The testthat suite in `tests/unit/` has 20 pre-existing failures unrelated to this work (regex/locale issues in the test environment).

## Decision Log

- **2026-08-26** — Job catalog simplified to 4 categories / 11 templates matching the actual class routine. Categories represent contribution types (the bidding level); cold-call vs volunteer is a post-level mechanism flag, so "Cold call: answer" and "Volunteer: answer" share one category and one bid.
- **2026-08-26** — Volunteer pay in wage-bidding rounds is a uniform clearing wage (no per-student pay, no rationing — anyone may volunteer at the going wage). Three switchable rules (lowest bid / k = post slots / k = posted per-class demand) so Kyle can experiment during the semester; students always bid their reservation wage regardless of rule.
- **2026-08-26** — Bid lock is a recurring schedule (lock before class, reopen that evening) layered on top of optional per-round bid open/close dates, matching the Mon/Wed 12pm teaching schedule.
- **2026-08-26** — Some-session jobs (discussion lead, cold calls) are seeded as templates with Auto-copy OFF rather than deleted or always-on; the instructor toggles them per round.

## Work Log

- **2026-08-31** (Codex) - Added an authenticated `?view=cold-call-slide`
  mode to `class-job-market` that exposes a compact cold-call draw/record
  panel for embedding in Reveal/Quarto slides while preserving the existing
  `live_score_events` write path.

- **2026-08-26** (Claude Code, branch `claude/class-job-market-simplify-szx2ai`) — Fixed settings-panel reactivity, student-poll invalidation keys, and fresh-DB migration ordering in class-job-market. Replaced the seeded job catalog with the simplified 4-category/11-template model plus a one-time data migration. Added template-level timing/voluntary/in-draw/auto-copy (carried into rounds by "Create next round"), a `during` timing with a cold-call draw filter, the recurring bid-lock schedule, the volunteer clearing-wage engine with three rules and a per-class posted-demand editor, student-facing volunteer wages, and a scratch-DB smoke test.

## Next actions

1. Sandbox rehearsal before the next class: enter Demo Mode, create a round, run start/end draws, toggle a cold-call template on, draw with the During filter, post volunteer demand, and log volunteers from a phone.
2. When switching to bidding mid-semester, set the round's assignment mode (wage or application bidding) and verify the bid lock engages at 11:00 AM on a class day; confirm students see volunteer wages.
3. Design the write-back contract for one-off games (participation/tokens into the semester economy) — the next unchecked build priority.

## Questions for Kyle

- In `application_bidding` (point-bid) rounds, volunteers currently earn the post's default wage — clearing wages only apply under wage bidding. Intended until wage bidding starts, or should point bids also price volunteering somehow?
- Should students see the posted demand k itself, or only the implied wage (current behavior)?
- One job per student per round is enforced, so cold-call draws only select students without a job that round. Keep, or should cold calls be stackable on top of assigned jobs?
