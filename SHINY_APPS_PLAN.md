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

- [x] Add a reusable persistent live-word-cloud question manager for slide-embedded classroom polling. *(2026-09-13: questions are created and edited in the instructor screen, routed by stable URL key, and require no app-code or slide-source change after embedding.)*
- [x] Stabilize the class-job-market hub around live teaching workflows. *(2026-08-26: round setup, template auto-copy, and settings-panel reactivity fixed; see Work Log.)*
- [x] Apply Pass Insurance only to the original class period, including existing purchases, and limit new purchases to the next scheduled class.
- [x] Repair Pass Insurance checkout and allow live purchases until the instructor closes sales or settles the market.
- [x] Add a four-class Pass Insurance market to Class Job Market with advance purchase, one live-pass claim, pooled zero-profit repricing, equal class dividends, and an instructor run sheet. *(2026-11-06: implementation and focused regressions complete; immutable-image staging remains required before production promotion.)*
- [x] Make job setup easier to maintain: fewer defaults, clearer timing, clean in-draw/voluntary controls, and sensible demo mirroring. *(2026-08-26: simplified 4-category / 11-template catalog with one-time migration; template-level timing/voluntary/in-draw/auto-copy controls.)*
- [x] Add interactive policy-project rubrics with private drafts, team release, student Account feedback, and optional gradebook write-back. *(2026-10-07: presentation, progress/revision, and written-brief forms use five point anchors with Missing separated from Incomplete.)*
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

### Date-based class sessions

Rounds are no longer an instructor-facing workflow. Settings → Job Market Controls uses a **Class date** plus direct toggles for assignment mode, multiple jobs, auction pricing, bidding state, tickets, tie-break, and token reveal. Choosing a date immediately switches Today, Job Market, Live Tracker, and Jobs. A new date carries the prior date's job posts and standing wage/ticket bids forward; existing assignments and score events remain attached to their original date. Internal `round_id` columns remain only as compatibility keys.

Assignment modes are `random`, `application_bidding` (point/ticket bids), and `wage_bidding` (lowest-wage bids).

### Bid lock schedule and manual override

Bidding is continuous between classes. Settings → Job Market Controls → Bid Lock Schedule stores the recurring class schedule in `labor_settings`:

- Defaults: class days Mon/Wed, class starts 12:00, lock 60 minutes before (11:00 AM), reopen 5:00 PM, timezone America/New_York, enabled.
- The first main-job or cold-call draw freezes every active post's wage in `class_wage_snapshots` and closes bidding. With the recurring schedule enabled, bids reopen at the configured time; with it disabled, they remain closed until manually reopened.
- **Open now** is an explicit instructor override of both scheduled and draw-triggered locks.
- Both wage and ticket submission handlers enforce the effective lock server-side; students see the lock status in Job Market.

### Draws and cold calls

Live Tracker → Job Assignments:

- Draw filter: All timings / Start of class / During class (cold call) / End-post class.
- The **All timings** draw clears and redraws the full assigned-job round but **excludes `during` posts**. Each lecture can allow multiple distinct regular jobs per student or limit each student to one, in which case the editable job clearing order determines which post gets first claim. Live cold calls and voluntary contributions are independent, repeatable score events and may stack with assigned jobs or each other.
- In `wage_bidding` mode, draws take the cheapest bids per category. Each class date can pay winners their own bids or use uniform reverse-second-price pricing, where every winner on a post receives the next losing bid (with the posted wage as the no-loser fallback).
- In `random` and `application_bidding` modes, every assignment snapshots the post's configured default wage when selected. A completed job pays that fixed amount, a genuine try pays 1 token, and a miss pays 0. Clearing wages never leak into these modes.
- **Add Assignment Back** is a repair for an accidental removal: it restores a normal assignment on the selected class date, snapshots the post wage, appears in that date's assignment and Today job-pool counts, and ages into the overdue table normally.

### Volunteer clearing wage

With wage bidding, every volunteer in a category is paid the **same equilibrium wage** derived from the selected class date's bids — never their own bid, and nobody is rationed out. The first draw snapshots this wage, so later bid edits cannot change volunteer or cold-call pay during class. Rule selected in Settings → Jobs → Volunteer Clearing Wage (`volunteer_clearing_rule`):

1. **Lowest bid** (default): the cheapest bid in the category (k = 1).
2. **Demand-based**: the k-th lowest bid, k = the volunteer post's slots (standing demand).
3. **Posted demand**: the k-th lowest bid, k = expected demand posted for the class date in Live Tracker; it falls back to post slots until posted. Stored in `volunteer_demand(round_id, category_id)`.

k is capped at the number of bids; with no bids the post's default wage is used. Students see "Volunteer Wages For This Class" on Job Market. In random or application-bidding mode, volunteers earn the snapshotted post default.

### Fixed bugs (context for why things looked broken before)

- The admin Settings panel now refreshes after every mutation (rounds/categories/posts/templates); previously most handlers never invalidated it, so create/delete looked like no-ops.
- The student job poll watched a nonexistent `weekly_rounds.updated_at` column; new rounds/posts never appeared. It now keys on round/post/demand counts and ids.
- `ALTER TABLE weekly_rounds` migrations ran before its `CREATE TABLE`, so fresh databases lacked `tokens_revealed`/`tiebreak_method` and round creation failed until a restart.

### Testing

`tests/smoke-class-job-market.R` (run `Rscript tests/smoke-class-job-market.R` from repo root; needs DBI + RSQLite) exercises the seed migration, template auto-copy, idempotence across restarts, bid-lock windows, and all three clearing-wage rules against a scratch SQLite DB by extracting the relevant functions from `app.R`. The testthat suite in `tests/unit/` has 20 pre-existing failures unrelated to this work (regex/locale issues in the test environment).

## Decision Log

- **2026-11-11** — Repair gradebook item editing, approved grade reweights, and policy-project dates. **Scope:** make gradebook item name/weight controls deliver a durable non-submit event; apply approved category and assignment reweight requests to each student’s displayed grade calculation; add scoped instructor controls for the progress-report and final-brief dates with ECON 342 defaults matching `PubEcon/course-data/fall2026.yml` (November 23 and December 15, 2026). Live synchronization with a course page/GitHub repository is future integration work, not part of this repair. **Invariants:** pending and rejected requests do not affect grades; pricing, cumulative caps, token debits/refunds, reveal gates, exam drops, stored raw grades, rubric scores, team presentation dates, and course/section isolation remain unchanged. **Acceptance:** an instructor renames/reweights a policy-project item and the saved values persist after rerender; approving a category or individual-assignment request immediately changes that student’s effective weights and grade while another student’s grade is unchanged; an instructor can save scoped policy dates and students/admins see the same milestones, including the November 23 progress report before the Zoom meeting. **Dependency check:** preserve `gradebook_categories`, `gradebook_item_names`, `grade_reweight_requests`, `compute_student_grade()`, assessment reveal exclusions, policy team assignments, and the existing rubric workflow. **Verification:** `Rscript --vanilla tests/critical-grade-reweighting.R`, parser/diff checks, Class Job Market smoke tests, and the full Docker build; manually verify instructor Grades & Gradebook/Policy Rubrics and student Account/Assignments in the staged immutable image before production promotion.

- **2026-11-10** — Add a recession firing action to the existing Class Job Market cold-call draw. **Scope:** an instructor may fire the currently drawn student from either Live Tracker or the slide panel; the action records a zero-token recession firing, pays that student's existing one-time Pass Insurance benefit when eligible, and redraws. Creating another insurance product, changing premiums/benefits, or removing the existing pass/answer/board/absence actions is out of scope. **Invariants:** only an authorized, non-impersonating instructor can record the firing; the selected course/section and class date remain authoritative; uninsured firings pay zero; a policy pays at most once across passes and recession firings; insurance cashflows remain non-earning; existing cold-call and settlement workflows remain unchanged. **Acceptance:** after drawing a student, the instructor selects **Recession — Fire & Redraw**; an insured student receives one benefit and a persisted firing event, an uninsured student receives no benefit, the fired student is excluded from the immediate redraw, and repeated insured events cannot duplicate the claim. **Dependency check:** preserve the existing `cold_call_draw`, `cold_call_last`, `issue_insurance_claim`, Pass Insurance tables, and both instructor control surfaces. **Verification:** extend `tests/critical-insurance-market.R` through the production Shiny observer, run that focused regression, the Class Job Market smoke test, parser checks, and the Docker publication build; manually verify both instructor controls in an immutable staged image before production promotion.

- **2026-11-09** — Limit Pass Insurance checkout to the next scheduled class in the student’s course and section market. Include today until settlement, allow purchases during class while sales are open, and keep purchase eligibility independent of the instructor’s selected lecture. Keep one purchase per student per class and reject attempts to buy a farther-out or stale offer before debiting tokens. Verify the current class, an intervening class without an offer, and the next offer after settlement.

- **2026-11-09** — Limit Pass Insurance to the class period associated with the purchased offer, including existing purchases. Keep open and closed sales eligible for redemption in the original period, end coverage at settlement, and preserve prior purchase, payout, and dividend history. New settlements distribute any positive profit after paid claims. Verify same-period redemption, rejected redemption in a different period and after settlement, and atomic payout rollback through Shiny and SQLite.

- **2026-11-09** — Carry unused Pass Insurance across class dates in the same market. Keep existing sales and purchase limits, scoped balances, the purchased benefit, and the live pass and redraw workflow. A saved pass survives closing or settling its offer and pays at most once, from the oldest unused purchase. Reserve full outstanding payouts before distributing dividends. Verify a purchase, settlement, date switch with no new offer, first redemption, repeat pass, and a failed payout through the real Shiny observers and SQLite.

- **2026-11-09** — Repair Pass Insurance checkout in Class Job Market. Preserve scoped eligibility, existing coverage, one policy per student and class date, and non-earning premiums. Allow purchases during class until the instructor closes sales or settles the market. The first insured live pass can pay while sales are still open, including for legacy offers with stored deadlines. A purchase and its debit must be saved together, with a retry available after a database failure. Verify through the real Spend panel and Buy policy observer against SQLite, including a rejected debit, repeat purchase, claims, and settlement.
- **2026-11-09** — Put Risk & Insurance student-view controls inside the instructor-only **Settings** tab. **Scope:** relocate the existing scoped impersonation selector and stop action; round mechanics, results, authentication, and persistence are out of scope. **Invariants:** only authorized instructors can impersonate; the target must remain an active student in the selected course/section; impersonation never changes authentication or stored game state; Play continues to show the selected student's experience; Results remains anonymous; and round lifecycle controls remain unavailable while impersonating. **Acceptance:** an instructor opens Settings, starts a student view, sees that student's Play surface, returns to Settings, and stops the student view; a student never sees Settings or the controls. **Dependency check:** preserve the existing `risk_impersonate_*`, `player_user`, `player_name`, and `impersonating` boundaries and update both Settings-tab visibility and the former title-card consumer. **Verification:** extend `tests/risk-insurance-game.R` to render Settings before, during, and after impersonation; run the focused test and parser check, with Docker build and manual browser smoke required before deployment.
- **2026-11-09** — Cloudflare assignment timing is authoritative for mapped extension products. Scope: an instructor-controlled refresh reads the existing authenticated Worker assignment list and updates only the local deadline display/approval metadata for mapped products. Invariants: no app startup request depends on Cloudflare availability; purchases, mappings, active flags, and Worker schedules are never changed by this refresh; submission products use Worker `dueAt`, while corrections products use `gradingDueAt`. Verification: pure PS4/PS5 mapping fixture, parser, existing extension checkout/product/refund tests, smoke, and Docker publication build.
- **2026-11-09** — Problem-set extension checkout is one atomic local transaction: the token debit and `extension_purchases` row both commit or both roll back. Scope: extension purchases only. Invariants: a failed local save never sends a Worker sync and never leaves a student charged without a refundable purchase record; original-submission and corrections products remain separate. Corrections products target `self_grading`, which extends only the corrections deadline; original problem-set products target `submission`. Verification: a focused SQLite fixture exercises a PS4 save plus an intentionally rejected insert/rollback, alongside parser, extension-product/refund, smoke, and container-build checks.
- **2026-11-09** — Extension refunds use an explicit claim → remote revoke when needed → ledger credit → finalize state machine. **Scope:** student pre-deadline refunds, instructor approval of requested refunds, retry after a failed revoke, and recovery of the prior build’s already-credited `processing` rows. **Invariants:** a remote entitlement is revoked before a synced purchase is refunded; ledger credit is never duplicated; an uncredited in-flight processing row is never auto-refunded. **Acceptance:** an instructor can approve a student’s requested refund, a direct student refund ends as refunded rather than processing, a failed revoke is retryable, and only a processing row with a matching immutable `extension_refund` ledger entry is recovered at startup. **Verification:** scratch-SQLite transition fixture, parser, Class Job Market smoke, Docker publication gate.

- **2026-11-09** — Assignment extensions are one Worker target per product. **Scope:** startup now supplies separate submission and self-grading/corrections products for PS4–PS6, preserving any existing assignment record and its dates. **Invariants:** no existing product, deadline, Worker mapping, purchase, or approval state is overwritten; a student sees only active products; a correction extension is sent as `self_grading`, never as a submission extension. **Acceptance:** PS4 and PS5 each expose both purchase types with their verified Worker IDs; PS4’s corrections product inherits its existing solutions date; rerunning startup does not duplicate rows; PS6 is visible to administrators but inactive until its Worker route exists, so students cannot buy an extension that would fail. **Verification:** scratch-SQLite migration fixture, parser, Class Job Market smoke test, and Docker publication gate.

- **2026-11-07** — Declutter the student **Account** tab into obvious native accordion sections and make assignment reminders reflect assignment-portal completion rather than waiting for a grade import. **Scope:** an open **Needs your attention** section plus separate **Jobs**, **Tokens**, **Grades & feedback**, and **Profile** sections; problem-set reminders become vertical task cards and refresh from the existing private Worker review API when Account opens. A verified PDF advances the reminder to self-grading; a final submitted self-grade removes it. **Out of scope:** assignment deadlines/policies, assignment-portal submission behavior, grade calculations/reveal gates, job state, token balances, profile persistence, and the five top-level tabs. **Invariants:** all existing Account outputs and actions remain available exactly once; Worker credentials stay server-only; course/section and impersonation behavior remain unchanged; unresolved jobs and unrevealed grades remain visible; a Worker outage falls back to the current grade-record test and never hides pending work. **Acceptance:** a student can identify each Account area from a clickable summary; a never-submitted problem set shows the problem-set deadline, a PDF-only submission shows the self-grading step, a final Worker submission disappears immediately after Account status refresh, and a matching recorded grade still suppresses the reminder when Worker status is unavailable. **Dependency check:** preserve the existing output IDs and save-name handler, update every consumer of the pending-problem-set query, and reuse `assignment_review_assignments()` plus `assignment_review_gradebook_export()` without exposing their payloads. **Verification:** pure status-filter cases, a realistic SQLite/student Account workflow with mocked Worker responses, existing gradebook/navigation/Class Job Market smoke tests, full Docker build, and an immutable-image student staging smoke before production promotion.

- **2026-11-06** — Replace the legacy in-memory Clubs & Insurance prototype at `/club-insurance-game/` with a persistent **Risk & Insurance** activity using Class Job Market identity, environments, and participation-token balances. **Scope:** instructor-configured rounds with either a proportional balance loss or fixed token loss, a posted full-insurance premium, a student maximum-premium choice that determines purchase, independent loss draws, optional real-token settlement, and revealed aggregate analytics by starting-endowment band. **Out of scope:** Pass Insurance pricing and claims, job bids/wages, grades, Bonus Pot, private health-risk types, and automatic activation of any class environment. **Invariants:** admins are not players unless impersonating; course/section data remain isolated; choices are replaceable only while open; settlement and every ledger write are idempotent; names and individual WTP/parameters stay instructor-only; public results use aggregates; token losses and premiums never count as earnings. **Acceptance:** the instructor can run one percent-loss and one fixed-loss round; a student sees their snapshotted balance, loss, expected loss, premium, and resulting purchase before submitting; settlement applies the configured stakes once; revealed results report purchase rates by endowment range, an asset-ignoring risk-premium rank, and implied CARA/CRRA parameters with undefined cases labeled rather than fabricated. **Dependency check:** preserve the existing route and catalog ID, update the shared-login return allowlist and active-slot embed, and replace every consumer of the retired in-memory state. **Verification:** pure risk-math tests, seeded SQLite round/choice/settlement regression, route/navigation contract, parser and startup checks, full Docker build, isolated instructor/student staging smoke, and rendered social-insurance slides before production promotion.

- **2026-11-06** — Student-entered token contributions use 0.01-token precision. **Scope:** Candidate Question Fund contributions in Class Job Market and Shared Pot/Club Choice contributions in Bonus Pot. **Out of scope:** wages and bids, grade points, instructor-defined prices/caps, insurance pricing, and the existing 0.5-token payout rounding. **Invariants:** balances remain private where already private; contributions retain the same scope, persistence, replacement, close, debit, and payout behavior; neither app creates a new token ledger. **Acceptance:** a student with 0.37 tokens can type and spend 0.37, the saved contribution is 0.37, and the displayed balance/contribution preserves that precision without changing fixed-price products. **Dependency check:** all consumers of the Flex and Bonus Pot contribution inputs were updated, including reset and saved-state messaging; payout rounding remains separate. **Verification:** focused precision contract, parser checks, relevant Bonus Pot regressions, Class Job Market smoke tests, Docker image build, then a staged student smoke before production promotion.

- **2026-11-06** — Add Pass Insurance as a dated purchase inside the existing Class Job Market. **Scope:** one pooled contract per course/section scope and selected class-date round, advance student purchase, one benefit on an insured live pass, experience-based pooled repricing, and equal distribution of positive realized profit. A separate existing poll may be used for the later adverse-selection vote. **Out of scope:** altering bids, job draws, cold-call clearing wages, ordinary answer/board awards, absences, the option to pass, or building a new game/app. **Invariants:** all existing instructor and student job-market paths remain available; course/section isolation is preserved; premium, claim, and dividend cashflows change spendable balances but not gross participation earnings; duplicate purchases, claims, settlements, and dividends are idempotent. **Acceptance:** an instructor opens sales for a selected date; a student buys once before the deadline; an uninsured pass pays zero; an insured pass records the zero-token pass, pays once, and redraws; settlement freezes the audit, returns positive profit equally to all active scoped students, and suggests the next premium as cumulative claims cost per policy rounded to the configured increment. **Dependency check:** the feature reuses the existing token ledger, active round, scoped roster, Spend tab, Live Tracker, slide cold-call panel, and spending enablement; none is renamed or removed. **Verification:** focused helper and SQLite workflow tests, startup schema assertions, parser check, existing Class Job Market smoke, Live Score Audit, assignment-wage, and game-navigation tests, then immutable-image staging and manual instructor/student smoke before production promotion.


- **2026-11-04** — Grade-reweight caps are cumulative per student: pending and approved requests share one percentage-point allowance across category-level and individual-assignment reallocations; rejected requests do not consume it. Class-job assignment visibility is independently controlled by its stored class date, section, and start/end timing; selecting **All assignments** is the only action that reveals both groups.

- **2026-11-03** — Add Club Choice as a Bonus Pot round structure, not a new app. It preserves `shared_pot` as the default and snapshots Club Choice multipliers, membership cost, minimum contribution, and exit rule at close. Commitment membership cost is included in that club's pooled contribution. Student balances stay private; revealed results are aggregate by club. Scope excludes Price War and does not alter existing shared-pot payout semantics.

- **2026-11-03** — Price War is a clean-slate standalone game. Its rounds, submissions, and payout idempotency are stored only in `price_war_rounds`, `price_war_submissions`, and `price_war_payouts`, each keyed by course, section, and round. Bonus Pot remains on its existing persistence model and is out of scope for this change. Class Job Market retains only scoped activation and links to Price War's dedicated setup. Acceptance: a Price War setup or payout cannot overwrite Bonus Pot settings; two course/section pairs may retain separate rounds with the same round number; students directly visiting Price War only submit to the active scoped round.

- **2026-11-03** — Finish the coordination-game split with two player routes: `/bonus-pot/` is exclusively the contribution public-good game and `/price-war/` is exclusively the High/Low-price Prisoner's-Dilemma variant. Scope: game catalog, direct routes, shared-login return allowlists, active-slot embeds, standalone player-mode guards, and regression coverage. Invariants: both games retain the shared Class Job Market database/session and scoped active-game state; activating a game still controls the shared live round; inactive `either` cards remain safely openable and never accept a mismatched live-game submission. Acceptance: a student can open either game from Games & Demos without activation; an activated Bonus Pot or Price War appears in Active Now and its own route; Google/password/shared-cookie login returns to the chosen game. Verification: parser checks plus focused navigation and existing Bonus Pot payout/unification fixtures.

- **2026-11-03** — Rename the deployed Bonus Pot route from `/coordination-games/` to `/bonus-pot/` before extracting Price War as its own application. Scope: route, OAuth/login return allowlists, Class Job Market embedding, demo links, Docker parse gates, tests, and active lecture/QR links. The legacy `/coordination-games/` route remains a redirect only, so existing QR codes and bookmarks reach Bonus Pot; it owns no authentication or game state. Invariants: Bonus Pot continues to use the shared Class Job Market session and records; all maintained links use `/bonus-pot/`; neither persistent data nor the separate-game data-model migration changes in this step. Acceptance: a direct `/bonus-pot/` visit, a Class Job Market iframe, and OAuth return all land on Bonus Pot; a direct legacy-route visit redirects there. Verification: focused Bonus Pot/return-path checks, R parser checks, and a rendered Public Economics deck.

- **2026-11-02** — Bonus Pot now has an explicit third denominator mode, **submits at least**. It saves a nonnegative token threshold and divides the multiplied pot only among submitters at or above that threshold. Scope: this changes Bonus Pot denominator selection only; Price War, contribution debiting, section scoping, and token-ledger idempotence are out of scope. Invariants: enrolled-section mode still pays every active non-admin, non-demo student in the selected section; contributor mode still pays every submitter; the threshold is ignored unless its third mode is selected. Acceptance: an instructor selects the third option and threshold, a student can still submit below it, and only qualifying submitters receive the share on payout. Dependency check: both Class Job Market and standalone Coordination Games controls, student messaging, schema migration, and payout paths use the same persisted mode. Verification: `Rscript --vanilla tests/bonus-pot-public-good.R`, the two focused critical Bonus Pot fixtures, parser checks, and `Rscript --vanilla tests/smoke-class-job-market.R` pass. Staging completed with published image `1ff91feb0f5ded8353ec388bfdb20dd39f96b15a`: HTTP health, published-image payout regression, and SQLite schema migration passed. Production promotion is blocked only by Reclaim SSH public-key access.

- **2026-11-02** — Standalone Coordination Games authorizes instructor configuration against the explicit course/section pair selected in its admin form, not against hidden prior session state. Scope: this repairs selection and authorization UX only; it does not broaden role grants or alter game/payout persistence. Invariants: super-admins retain access to all active non-demo course/section environments; scoped admins see only their assigned environments; an unauthorized pair is still rejected. Acceptance: an authorized admin can choose any managed course, sees only that course’s managed sections, saves a permitted pair, and the saved pair becomes the active session environment. Verification: the focused Bonus Pot regression includes the selector/handler contract; `Rscript --vanilla tests/bonus-pot-public-good.R`, `Rscript --vanilla tests/admin-roles.R`, and an R parser check pass. Published image `2192943bdf0760dce7a9dc8d090e392c65600f65` was staged healthy with HTTP 200, in-container parse, a seeded database, and the focused regression passing.
- **2026-10-26** — Adopt a non-regression protocol for Class Job Market changes: identify preserved and new behavior before coding; add a workflow-level regression fixture for every production bug; test historical dates and selected course/section scope where relevant; stage the immutable image; and report the exact verification evidence before calling a change resolved. Pending assignments and pending Live Score Audit events must remain visible and actionable until committed, returned, or deleted.

- **2026-10-27** — Administrator status means management access, not automatic student-game eligibility. Across Class Job Market, default student rosters, class-size denominators, draws, and collective-action games exclude `is_admin=1`; a future instructor control may explicitly include administrators for the rare intentional case.

- **2026-10-27** — Replace the global boolean-admin assumption with auditable role assignments: `super_admin` may manage every course, `course_admin` may manage all sections of one course, and `section_admin` may manage explicitly assigned sections. The legacy `is_admin` field remains synchronized as a compatibility flag, while new authorization derives from `admin_assignments`. Existing admins and `ADMIN_EMAILS` bootstrap identities become `super_admin`s; roster imports may only grant roles when performed by a super-admin and never overwrite an existing super-admin account.


- **2026-10-26** — Use each Flex Question’s existing Exam tag as a persistent batch control. Instructors may activate/deactivate one question or every question under an exam tag; inactive questions stay visible for administration and retain their funding history, but are excluded from the active student funding sequence.

- **2026-10-25** — Use Class Job Market as the universal authentication gateway for protected course apps. A protected app validates the existing shared `arcade_token`; otherwise it sends the browser to Class Job Market with an allowlisted local return path. The OAuth relay retains that path through Google sign-in, and password/cookie sign-in in Class Job Market returns to it as well. New protected games join this pipeline by adding an explicit return-path allowlist entry rather than creating an app-specific login.

- **2026-10-25** — Keep local staging independent of the deployment `.env`: the staging overlay treats that file as optional and uses its own mounted SQLite data. Production compose continues to require the deployment environment file, so a workbench test cannot silently become a production configuration.

- **2026-10-23** — Treat Class Job Market as the single authentication authority for Review Quiz. Review Quiz validates the existing shared `arcade_token` session when available; if deployment routing prevents that cookie from arriving, its fallback checks the same shared Class Job Market account rather than trapping students in a redirect loop. Review Quiz does not issue or maintain a second persistent session.

- **2026-10-23** — Make Review Quiz token awards an explicit instructor release for each quiz session: the instructor chooses the earned-tokens-per-correct rate, previews every student award, and commits it once to the shared ledger as an `earning=1` event. This preserves the distinction between gross grade-relevant token earnings and later spending or policy charges.

- **2026-10-16** — Treat an unrevealed assessment as absent from the student-facing grade calculation, rather than suppressing the whole Gradebook. The student continues to see a coherent current grade based on revealed work; revealing the assessment adds its linked item and recalculates that visible grade.

- **2026-10-16** — Let instructors repair or change an assessment’s linked gradebook item after import. This association is display metadata only: it tells the student-facing reveal gate which weighted grade effect to withhold, without reuploading data or rewriting score rows.

- **2026-10-16** — Assessment-display choices remain editable after import. Instructors can switch a scoped assessment between immediate display and explicit student reveal without reuploading or rewriting grades; turning the gate on clears past acknowledgements so it takes effect uniformly.

- **2026-10-16** — Make assessment-grade disclosure an import-level instructor choice. When enabled, students must explicitly reveal the assessment; its score/details and any linked weighted-gradebook impact remain hidden until then. Persist that reveal per student and import so it is a one-time acknowledgement rather than a per-session hurdle.

- **2026-10-16** — Require an explicit assessment-summary target for weighted-gradebook write-back: match the assessment name, choose an existing item, skip gradebook write-back, or deliberately add a new item to a selected manual category. This prevents an uploaded assessment label from silently overwriting a weighted course-grade component while keeping the intended Midterm workflow one step.

- **2026-10-16** — Treat a mapped `overall_exam_note` as a per-student text field, distinct from the assessment-wide and cap-adjustment notes. It appears in the student’s assessment overview and is not coerced to a score or buried in the numeric subpart table.

- **2026-10-16** — Store uploaded exam/assessment summaries separately from the weighted course-grade rows. A summary import is scoped to the active class and selected sections, maps a stable student identifier plus instructor-chosen columns, and retains the source labels so students can see a concise final-grade overview and opt into subpart detail. Assessment-wide notes and cap-adjustment notes are entered outside the spreadsheet and persist with the import.

- **2026-10-14** — Treat slide-embedded class-job controls as a top-level OAuth entry point. Google OAuth cannot run inside the Reveal iframe; preserve only the validated class-job-market return path and optional `view=cold-call-slide` state through the login callback.

- **2026-10-07** — Grade policy-project criteria with five editable point anchors
  rather than wide score bands: Excellent = full credit, Proficient = 85%,
  Developing = 70%, Incomplete = 50%, and Missing = 0. Zero is a separate
  missing-work state, not part of the Incomplete band.
- **2026-10-07** — Keep one private draft and one released snapshot per
  course/team/component. Saving a draft does not change student-visible
  feedback; releasing publishes the shared rubric to all team members and may
  write its score to an existing gradebook item.

- **2026-10-05** - Represent the ECON 342 midterm bank as twelve candidates in three four-question pools. Pool A contributes 3 questions (75% each), Pool B contributes 2 (50% each), and Pool C contributes 1 (25% each); every pool spans the same four topic strands.

- **2026-10-02** - Treat one course plus one explicitly selected section set as the operational scope. One section is the default; choosing several intentionally pools their roster, job-market round and controls, tracker eligibility, and Flex funding, while choosing all sections must be explicit. Question-bank text may be course-wide, but prices, contributions, and unlocks are scope-specific.

- **2026-10-02** — Make exam-candidate questions a class-wide provision-point
  public good, separate from the exam bonus pot. Students contribute any whole
  number of tokens to the next locked question; reaching its threshold reveals
  it to everyone. Thresholds may be entered as an explicit list or as a formula
  using zero-indexed public-question count q and active class size N; the active
  threshold is snapshotted when its first contribution arrives.

- **2026-10-01** — Keep the class-job-market app authoritative for extension
  purchases and the Cloudflare assignment service authoritative for assignment
  timing. Sync purchases server-to-server with the existing admin bearer token,
  persistent student ids, and purchase ids for idempotency. A submission
  extension shifts the due date, solution release, and self-grading close;
  a corrections extension shifts only the self-grading close.

- **2026-09-29** — Make regular-job multiplicity and wage-auction pricing
  lecture-level controls. When multiple jobs are disabled, posts clear in the
  editable clearing order and each winner leaves the remaining regular-job
  pool; cold calls stay independently eligible. Wage bidding can use either
  pay-as-bid or a uniform reverse-second-price rule: the lowest bidders win and
  receive the next losing bid, falling back to the posted wage when every bid
  wins.

- **2026-09-29** — Allow a student to hold multiple distinct class jobs in one
  lecture. Jobs clear independently in an instructor-controlled order; the same
  student/job pair remains unique. The default order is Last class recap,
  Materials summary, Note taker, Critic/skeptic, Policy/example scout, then
  instructor-added jobs.

- **2026-09-29** — Treat cold calls as repeatable participation events, not
  exclusive class-job assignments. Students remain eligible for cold calls even
  when they hold a materials summary or another regular job; only absences and
  duplicate selection within the same cold-call batch constrain eligibility.

- **2026-09-29** — Store an explicit active lecture ID instead of treating
  the newest database row as current. Switching lectures preserves posts,
  assignments, bids, and history; all live operational views follow the selected
  lecture, while creating a new lecture makes it active automatically.


- **2026-10-02** — Treat every revealed, unfinished, non-volunteer assignment whose indexed class date is before today as overdue. Today shows the full reveal-aware backlog in **Last Class Jobs Still Pending**, sorted by and labeled with its original class date; it is no longer limited to one preceding session.

- **2026-09-28** — Store wage bids per individual active job post, including
  regular class roles, volunteer jobs, and cold-call jobs. Retain old
  category-level bids only as a read-compatible migration fallback; draws and
  clearing wages use post-specific bids whenever they exist.

- **2026-09-28** — Reconcile every job-market column required by the current
  sandbox before seeding fake jobs. Copying `CREATE TABLE IF NOT EXISTS` from
  production is insufficient for older demo tables and must not be treated as
  a schema migration.

- **2026-09-28** — Seed the demo database once with a synthetic DEMO 101 point-
  bidding round instead of repeatedly mirroring the live job catalog. Later
  demo sessions preserve rehearsal bids and edits; resetting the disposable
  demo database recreates the fake market.

- **2026-09-28** — Give each round three explicit bidding states: **Open now**,
  **Open on schedule**, and **Closed**. Open now is an instructor override and
  must bypass both stored hours and the recurring class-time lock. Scheduled
  mode enforces both; auto-created next rounds begin closed.

- **2026-09-28** — Keep one global class announcement in App Settings and show
  it near the top of Today for every signed-in student. Saving a blank message
  hides the card; open sessions poll for changes so no restart is required.

- **2026-09-25** — Point/ticket bids are accepted only for the active point-bidding
  round and open bid window, must be non-negative whole numbers within budget,
  and apply only to categories with a non-during drawable post. Allocation reads
  bids only from the instructor's selected course/section roster.

- **2026-09-24** — Route server-funded Demo Kit generations to GPT-6 Luna
  and user-funded OpenAI generations and exports to GPT-6 Sol. Use the
  Responses API for direct OpenAI traffic, retain Chat Completions for
  OpenRouter, and keep GPT-6 Astra configurable as an escalation model.

- **2026-09-24** — Treat each student-assignment grade as current state, not
  attempt history. Reimports and manual corrections overwrite the prior value;
  legacy duplicates collapse to the newest record.

- **2026-09-23** — Grade-upload templates use the same long-form schema as
  the importer: one row per student and assignment, keyed by persistent
  `user_id`. Display names and sections are reference-only; grading may use
  either `score` plus `max_score` or a 0–100 `grade_pct`.

- **2026-09-23** — Keep the Live Tracker's chosen class and section stable
  across polling re-renders. Reveal/Hide always targets the selected timing
  group; when no section is active, Reveal opens a non-demo section picker and
  applies the reveal immediately.

- **2026-09-23** — Put student job status first on Account, keep grade details
  below the account/profile content, and reserve Today for start/end jobs.
  During-class cold calls remain in their dedicated live workflow.

- **2026-09-23** — Preserve assigned jobs as student-visible history across
  later class draws. Derive display state from committed and pending outcomes:
  unscored is Outstanding, complete is Completed, tried is Tried, missed is Not
  completed, and absent redraws remain explicit rather than disappearing.

- **2026-09-21** — Never split short control labels character by character.
  Wrap buttons only between words, keep native file-picker buttons on one line,
  and allocate grid width according to the control rather than leaving unused
  columns. Compact repeated scoring actions may use keyed single-letter labels.


- **2026-09-21** — Allow class-job-market browser clients to reconnect to a new
  Shiny session after a network interruption or mobile background suspension.
  Authentication already survives through the persistent session cookie, and
  live scores are database-backed, so automatic reconnection is safer and less
  disruptive than requiring an instructor to reload during class.

- **2026-09-21** — Let enlarged controls reflow instead of shrinking their text.
  Widen the desktop class-job-market canvas to 1100px, minimize phone edge
  gutters, wrap button groups and modal actions, and stack dense launch cards
  on phones. Use shorter labels where the surrounding panel already supplies
  the context, while retaining explanatory tooltips for ambiguous controls.

- **2026-09-21** — Use a 110% readable default and persistent 90–140% text
  control for the class-job-market app. Keep primary controls at least 44px
  tall, increase phone targets to 48px where space permits, and expose the text
  control on mobile instead of hiding it. Projector-specific layouts remain
  separately sized.

- **2026-09-21** — Price assignment extensions with one instructor-configured
  convex power curve instead of discrete duration products. Students choose
  hours directly within configured limits; the recorded purchase retains the
  exact hours and charged token cost. Treat extension targets as editable
  assignments with CSV upsert support.

- **2026-09-21** — Treat sandbox credentials as canonical disposable test
  state. Reapply demo usernames, password hashes, roles, sections, and active
  flags on every sandbox bootstrap instead of preserving stale user rows.
  Reconcile older sandbox user schemas before credential reset so live-only
  columns cannot make authentication fail as an apparent bad password.

- **2026-09-21** — Treat Today as an unfinished-work view. An assignment rolls
  off as soon as an outcome enters Audit, and manual Add Back records are
  hidden from Today by default unless explicitly marked visible for their
  scheduled date.

- **2026-09-17** — Provide contextual, collapsible tutorial notes rather than
  a blocking guided tour. A single header switch controls all help notes and
  remembers the browser's preference locally.

- **2026-09-14** — Draw cold calls uniformly with replacement from the active
  course/section roster. Track absence per round and exclude marked-absent
  students immediately; prior call counts do not change draw eligibility.

- **2026-09-14** — Store concise instructions on individual job posts and
  templates, copy template instructions into each new round, and expose them
  through collapsed details so job lists remain compact.

- **2026-09-14** — Use one section-scoped reveal toggle with Start, End, and
  All scopes. The timing rows are authoritative; retire the legacy global
  override that could reveal assignments outside the selected group.

- **2026-09-14** — Treat assignment timing as a reveal/workflow distinction,
  not a lecture-index shift. End-of-class jobs such as note taking belong to
  the lecture that just ended; creating the next lecture remains an explicit
  Round Setup action.

- **2026-09-13** — Use static JavaScript for deterministic browser-only
  visualizers and Cloudflare Durable Objects for anonymous shared classroom
  rooms. Keep Reclaim as the authoritative home for the roster, token ledger,
  longitudinal student records, and Google integrations; do not add
  bidirectional synchronization merely to reduce Shiny memory.

- **2026-09-13** — Audited all 17 apps in
  `CLOUDFLARE_MIGRATION_AUDIT.md` and built `live-wordcloud` as the first
  Worker/Durable Object pilot. The old Shiny route remains available until the
  replacement survives a two-phone classroom test.

- **2026-09-13** — Treat every explicit Submit click as a visible word-cloud
  response. Use a browser/session/click submission key only for idempotence;
  do not silently replace an earlier response from the same browser.

- **2026-09-13** — Refresh only cloud results on the three-second poll timer;
  keep question metadata and the response form stable so active typing is never
  replaced. Put response clearing on the poll page behind password confirmation.

- **2026-09-13** — Separate live-poll participation from projection: stable
  poll URLs show the response form on student phones, while `display=1` iframe
  URLs show only the continuously refreshed word cloud and response count.

- **2026-09-13** — Store live-poll questions as SQLite data and address each
  one through a stable poll URL key. Instructors create, edit, open, close,
  and clear questions in the password-protected app screen; slide sources only
  need the generated embed URL.

- **2026-09-09** — Enforce one assigned job per student per round, but allow
  unlimited cold-call and voluntary score events. Each repeatable event has its
  own audit row, ledger source ID, commit state, and delayed-credit state.

- **2026-09-09** — Treat an assignment wage as live until its audit event is
  committed. Pending displays and Complete outcomes use the job post's current
  effective wage; commit snapshots that wage into `job_assignments` and the
  ledger permanently.

- **2026-09-09** — Use a fixed outcome scale for all jobs: Complete earns the
  job wage, Tried earns exactly 1 token, and Missed earns 0. Do not round a
  fractional wage for Tried.

- **2026-09-09** — Store Live Tracker assignments under `user_id`. Bulk input
  may identify a student by exact display name or user ID for convenience;
  `user_id` wins when both CSV columns are populated, and unmatched or
  ambiguous names must be reported rather than guessed.

- **2026-09-09** — Use one round-trip CSV as the Live Tracker outage
  fallback: download the scoped roster and job reference before class, record
  jobs and optional outcomes offline, then bulk-import it into the normal
  Pending and Audit lifecycle when service returns.

- **2026-09-08** — Store final ECON 342 policy-team assignments in the app's
  SQLite database and update them through an instructor-only CSV import or
  per-student manual editor. Raw preferences and blackout dates remain outside
  the Shiny app.

- **2026-09-08** — Treat Live Tracker rows as an exclusive lifecycle: an
  assignment is pending until an outcome moves it to audit, audit can return it
  to pending, and committing removes it from the live workflow. Start- and
  end-of-class reveal states are independent per section.

- **2026-09-02** — Record slide-panel cold calls as standalone live-score
  events against the existing Cold Call category, with `answer` or `board` as
  the outcome; never create per-round job posts from the cold-call control.

- **2026-08-26** — Job catalog simplified to 4 categories / 11 templates matching the actual class routine. Categories represent contribution types (the bidding level); cold-call vs volunteer is a post-level mechanism flag, so "Cold call: answer" and "Volunteer: answer" share one category and one bid.
- **2026-08-26** — Volunteer pay in wage-bidding rounds is a uniform clearing wage (no per-student pay, no rationing — anyone may volunteer at the going wage). Three switchable rules (lowest bid / k = post slots / k = posted per-class demand) so Kyle can experiment during the semester; students always bid their reservation wage regardless of rule.
- **2026-08-26** — Bid lock is a recurring schedule (lock before class, reopen that evening) layered on top of optional per-round bid open/close dates, matching the Mon/Wed 12pm teaching schedule.
- **2026-08-26** — Some-session jobs (discussion lead, cold calls) are seeded as templates with Auto-copy OFF rather than deleted or always-on; the instructor toggles them per round.

## Work Log

- **2026-11-11** (Codex) — Repaired Class Job Market grade weighting and policy-project dates. Approved category and assignment reweight requests now feed the affected student grade calculation, while pending and rejected requests remain inert. Gradebook item Save controls now emit durable non-submit events; renames migrate stored grades and linked gradebook references, and item weights persist. Added scoped policy milestone controls with ECON 342 defaults of November 23, 2026 for the progress report before the Zoom meeting and December 15, 2026 for the final brief. The production-observer regression, gradebook/settings regressions, smoke suite, parser/diff checks, and all 35 Docker publication steps pass in local image `eef2732b22c2`. No staging or production environment was changed. The longer-term course-page/GitHub `course_data` integration remains a planned follow-up.

- **2026-11-10** (Codex) — Added **Recession — Fire & Redraw** to both Class Job Market instructor cold-call surfaces. The action records a committed zero-token recession firing, pays the currently drawn student's existing one-time Pass Insurance benefit when eligible, and immediately redraws without that student; uninsured firings receive no payment, and the policy's existing uniqueness boundary prevents a second claim across passes and firings. Updated student/instructor copy and the classroom rehearsal. The focused production-observer regression covers insured and uninsured firings, redraw exclusion, persistence, non-earning cashflows, idempotent settlement, and section isolation; the Class Job Market smoke suite and all 34 Docker publication steps pass in local image `db8f78bbbb53`. No staging or production environment was changed.

- **2026-11-09** (Codex) — Restricted Pass Insurance sales to the earliest upcoming scheduled class in the student’s market, including the current class before settlement. Spend shows the purchase date separately from the selected lecture. Checkout validates the exact offer before debiting. The regression covers a farther offer, an intervening class without an offer, and purchase for the next class after settlement.

- **2026-11-09** (Codex) — Limited all Pass Insurance purchases to the original class period. Closing sales preserves coverage for that period and settlement ends it. Settlement and dividend payouts now commit together with the settled status. Extended the Shiny regression to check both pass buttons, class-period and settlement boundaries, and payout and dividend rollback.

- **2026-11-09** (Codex) — Added a Pass Insurance balance to Account and Spend. An unused purchase survives class-date changes and settlement in its class market. The live pass and any payout commit together. Settlement records full reserves for unused passes before calculating dividends. The workflow regression covers redemption from a settled offer on a later date, payout rollback, and one-time use.

- **2026-11-09** (Codex) — Fixed a Pass Insurance crash caused by parsing a blank sales deadline. Sales now follow the market status, including for older offers with a stored deadline. The policy and token debit commit together. Expanded the SQLite workflow regression to render the student form, retry a rejected debit, pay the first live pass while sales are open, and verify duplicate protection and settlement. Run the manual staging rehearsal before production promotion.
- **2026-11-09** (Codex) — Moved Risk & Insurance's instructor **View as student** selector and stop action from the always-visible title card into **Settings**. Settings remains available to the authenticated instructor during student view, opens the selected student's Play surface, and locks round lifecycle controls until the instructor stops viewing. The existing course/section validation and player identity boundary are unchanged. The focused Risk & Insurance workflow regression now renders Settings before, during, and after impersonation; parser, focused regression, and all 34 Docker publication steps pass in local image `cc00ffa88f63`. No staging or production environment was changed.
- **2026-11-09** (Codex) — Added **Refresh dates from assignment portal** to Settings → Spending Options → Assignment Extensions. It imports PS4/PS5 (and future mapped assignments) from the Worker’s due/solutions/self-grading schedule instead of requiring manual dates, without making a risky remote request at app startup.
- **2026-11-09** (Codex) — Made extension checkout atomic: token debit and local purchase are now one SQLite transaction, so an insert failure rolls back the debit and is surfaced to the student. Added a focused PS4/PS5 rollback regression to Docker publication gates. Startup now repairs only numbered products named “Corrections” to `self_grading`; the original PS1–PS3 problem-set products remain `submission`.
- **2026-11-09** (Codex) — Repaired extension-refund persistence. The old direct-refund handler moved a purchase to `processing` and then checked for its old status, so it could leave an already credited refund permanently stranded. Instructor approval likewise rejected the valid `requested` state. Refunds now claim eligible states atomically, finalize only from `processing`, treat revoked remote entitlements as refunded, retry safe failed revocations, and recover only old processing rows backed by a matching immutable token-ledger refund. Added a scratch-SQLite workflow regression and Docker gate.

- **2026-11-09** (Codex) — Added idempotent default extension products for PS4–PS6: each has a submission product and a separate corrections/self-grading product, preserving existing dates and Worker mappings. Verified the deployed Worker routes `econ342-2026-ps04` and `econ342-2026-ps05`; PS6 currently returns 404, so its two preconfigured products remain inactive until the matching Worker assignment is published and the instructor enables them. Added a scratch-SQLite migration regression to the Docker publication gate. Parser, focused migration test, and Class Job Market smoke test pass; Docker build remains the final local publication check.

- **2026-11-07** (Codex) — Decluttered the student Account tab into five obvious native accordions: **Needs your attention** (open by default), **Jobs**, **Tokens**, **Grades & feedback**, and **Profile**. Replaced the horizontal problem-set deadline/solutions table with vertical task cards backed by the assignment portal’s actual PDF and final self-grading states: PDF receipt advances the card to **Finish self-grading**, and final submission removes it without waiting for a grade import. A server-only refresh reuses the private Worker review API; outages retain the conservative recorded-grade fallback and never hide unresolved work. Added a direct Account-to-Assignments link, preserved every prior output/action exactly once, and made fractional token history amounts visible. The focused regression exercises helpers plus the exact rendered student workflow against disposable SQLite and mocked Worker responses. Local image `8bcf808ef995` passed all 28 Docker publication gates; the exact image returned HTTP 200 on isolated localhost staging and passed the in-container student Account workflow with clean application logs. No production containers or data were changed.

- **2026-11-06** (Codex) — Replaced the legacy in-memory Clubs & Insurance prototype with the persistent **Risk & Insurance** activity at the existing `/club-insurance-game/` route. Instructors can run proportional-balance-loss and fixed-token-loss rounds, post full-insurance premiums, settle independent bad-state draws against real tokens, and reveal aggregate purchase rates by endowment band, an asset-ignoring risk-premium rank, and implied CARA/CRRA parameters; individual choices and estimates remain instructor-only. Integrated the activity with Class Job Market identity, environments, active-game navigation, and deep links, and added a seeded end-to-end instructor/student settlement regression. Local image `3da5aa5e62e3` passed all 27 Docker publication gates; the exact image returned HTTP 200 for both the hub and game on isolated localhost staging, and its in-container open/purchase/close/settle/reveal test passed against a disposable shared database. No production containers or data were changed.

- **2026-11-06** (Codex) — Allowed student-entered Candidate Question Fund, Shared Pot, and Club Choice contributions in 0.01-token increments, including exact spend-down of fractional balances. Kept wages, fixed prices, instructor controls, and Bonus Pot payout rounding unchanged. Added a focused source-and-helper regression and Docker build gate. Local image `45ead85f1e7e` passed every publication gate and an isolated shared-database staging smoke returned HTTP 200 for both Class Job Market and Bonus Pot; the in-container precision contract passed.

- **2026-11-06** (Codex) — Added dated Pass Insurance to Class Job Market without changing the standing job-bid, assignment, cold-call wage, answer, board-work, absence, or live-pass routines. Students may buy one policy before class; an insured live pass pays once and redraws; settlement returns positive realized profit equally to the active scoped student roster and proposes the next pooled zero-profit premium from cumulative claim cost per policy. Premiums, claims, and dividends remain non-earning ledger entries. Added a four-class instructor run sheet, focused pricing tests, an in-memory purchase/claim/dividend workflow regression, startup schema assertions, and an immutable-image build gate. Focused insurance tests, app parsing, the Class Job Market smoke suite, Live Score Audit, assignment-wage, and game-navigation regressions pass; the broad startup suite reaches the new schema assertions but retains its documented unrelated failures. Local image `b7ed5781c124` passes every Docker publication gate.


- **2026-11-04** — Grade reweight requests are now limited to one pending request per student with both server-side validation and a SQLite trigger to stop concurrent submissions. Instructor rejection requires a student-visible note and records an idempotent token refund; Settings also now has an explicit **Turn off active game** button that clears only the selected environment’s active-game slot.

- **2026-11-04** — Sealed unrevealed game diagnostics from student/projector Results: Price War pair counts and Bonus Pot submission totals/contributions now appear only in each game’s administrator Settings diagnostic. Public results remain blank until the instructor marks the round revealed.

- **2026-11-04** — Repaired stale demo-sandbox compatibility for Bonus Pot Club Choice: bootstrap now migrates the shared game tables and columns used by submissions and snapshots. The Club Choice projector also renders a stable empty-results table instead of failing when a revealed round has no recorded entries.

- **2026-11-04** (Codex) — Removed the retired Prisoner’s Dilemma scale and payoff-matrix controls from Bonus Pot Settings. Its parameter save now updates only the Bonus Pot multiplier and contribution cap; legacy PD columns remain untouched for historical compatibility.

- **2026-11-04** (Codex) — Fixed the revealed Club Choice projector failure: its aggregate-results helper treated multi-student submission columns as scalar values, causing the revealed-only render error. It now normalizes full vectors for club, contribution, and entry-cost fields; the focused Bonus Pot regression covers Open, Commitment, and Exit entries in the same revealed round.

- **2026-11-04** (Codex) — Replaced the URL-based sandbox switch with a normal **DEMO 101** course and S01/S02 demo roster. Existing super-admins select it in the ordinary Class / section picker; `demo-instructor` / `admin123` is a course admin for **DEMO 101 only**, while demo students use their own accounts. This preserves ordinary course-scoped interactions without granting the demo instructor access to any live course.

- **2026-11-04** (Codex) — Repaired the isolated sandbox platform: bootstrap now gives the sandbox instructor its required super-admin role and assigns every sandbox account to the Demo course/section environments, so selecting **DEMO 101** exposes the normal instructor Settings workflow. The container entrypoint now forwards `DEMO_MODE` into Shiny’s R environment, ensuring the dedicated demo service routes Class Job Market and game apps to `*-demo.sqlite`, never the live database. The sandbox bootstrap regression verifies the role and all Demo environments.

- **2026-11-04** (Codex) — Clarified Club Choice labels in Bonus Pot: each club radio option now states its current multiplier, including the Commitment Club multiplier beside its membership cost; Exit now reads **“no loss, no gain.”**

- **2026-11-04** (Codex) — Price War’s student surface now uses explicit **High**, **Low**, and **Withdraw** buttons. It displays the authenticated player’s saved choice (or that they have not yet played), permits changing or withdrawing only while the scoped round is open, and keeps that player-specific refresh independent of classmates’ simultaneous submissions. The scratch-SQLite workflow regression now verifies saved-choice status and withdrawal.

- **2026-11-04** (Codex) — Embedded Price War now takes its selected course/section from Class Job Market and suppresses its standalone banner/switcher there. The active-panel iframe forwards the existing admin impersonation target as a validated student-player context. Settings now shows an instructor-only, pre-reveal diagnostic with each saved entry, HH/HL/LH/LL counts, and unpaired-entry count; the workflow regression asserts that diagnostic after two student submissions.

- **2026-11-04** (Codex) — Rebuilt Price War’s reactive boundaries to match Bonus Pot: the Play form listens only to round/status changes, Results alone polls submissions, and Settings is a separate admin surface. Added a scratch-SQLite workflow fixture covering two simultaneous student submissions plus an instructor setting update.

- **2026-11-04** (Codex) — Added the Classroom Economy header to Price War, with the authenticated user and a scoped class/section picker matching the Bonus Pot pattern. The picker exposes only environments authorized for that user and refreshes the selected game state.

- **2026-11-04** (Codex) — Enforced the Grade Reweight percentage cap as a cumulative allowance across pending and approved category/assignment requests, with remaining allowance shown to students and server-side validation for stale forms. Centralized class-job reveal filtering by class date, section, and start/end timing so selected Start, End, and All reveal groups are independent. Extended the Class Job Market SQLite smoke fixture for both regressions; smoke test and parser check pass.

- **2026-11-04** (Codex) — Preserved the selected Price War tab across its live refreshes, so instructors can remain on Results or Settings while submission and status polling continues.

- **2026-11-04** (Codex) — Added an executable Price War pair-count regression for HH, HL, LH, and LL results, including an unpaired submission that must be excluded until matched.

- **2026-11-04** (Codex) — Restored Price War’s classroom layout: all users have separate Play and aggregate Results tabs, while the round configuration and payout controls are behind an admin-only Settings tab. Results report completed HH, HL, LH, and LL pair counts without exposing student identities.

- **2026-11-04** (Codex) — Made direct Price War entry use the shared Bonus Pot/Class Job Market login contract: Google sign-in returns to Price War, password sign-in creates the same 14-day `arcade_token` session cookie, and either route makes the user signed in across the classroom apps.

- **2026-11-04** (Codex) — Restored Price War’s Class Job Market single sign-on: the standalone app now sends the shared `arcade_token` cookie to its server and validates it as `auth_cookie`, matching the Bonus Pot contract. Its round-save handler now persists to the selected environment instead of nonexistent form fields.

- **2026-11-04** (Codex) — Updated the critical Bonus Pot regression fixture after the control-surface consolidation: it now requires Settings to direct instructors to `/bonus-pot/` and rejects the retired duplicate lifecycle form. This restores the Docker/GitHub build gate while retaining the single-source setup design.

- **2026-11-03** (Codex) — Consolidated Bonus Pot administration into its game panel. Class Job Market Settings now retains only the active-game selector and links instructors to Bonus Pot for round setup, lifecycle, and Club Choice rules, preventing the formerly divergent duplicate forms from overwriting each other.

- **2026-11-03** (Codex) — Repaired Price War startup: its SQLite wrappers passed an empty parameter list to statements with no placeholders, which made initialization exit before the UI rendered. The wrappers now omit parameters when none are supplied; direct local app startup and the focused navigation regression pass.

- **2026-11-03** (Codex) — Added the Club Choice Bonus Pot mode: open/commitment/exit selection, private balance validation, optional commitment minimum, contribution-plus-membership debit, close-time rule snapshots, separate club pooling/payouts, and an aggregate revealed comparison. Existing shared-pot tests continue to pass.

- **2026-11-03** (Codex) — Fully detached Price War from the unused Bonus Pot/legacy coordination state. `/price-war/` now owns course-section-round keyed round, submission, and payout tables, including per-student payout idempotency; it keeps the shared authentication and token ledger only. Class Job Market no longer changes `olig_settings` when activating Price War and directs setup/tracking to the standalone route. Focused navigation, parser, Bonus Pot-unification, and public-good payout checks pass.

- **2026-11-03** (Codex) — Completed the coordination-game split. Added a deployable `/price-war/` entry point that runs the shared player in Price War mode, while `/bonus-pot/` is constrained to Bonus Pot. Games & Demos now provides direct **Open game** links for inactive `Either/or` cards; activation remains only for projecting/running the shared live round in Active Now. Added Price War to the strict Class Job Market post-login allowlist, replaced the duplicate standalone Prisoner's Dilemma catalog card with Price War, and made the active slot iframe the new route. Added `tests/critical-game-navigation.R`, updated the existing mode-aware login assertion, and made Docker parse/test the new app. Parser checks plus focused navigation, Bonus Pot unification, and public-good payout tests pass; local image `sha256:0d42e906ea1eda728df739bfadebc786d094ee2133a5c3f84d83d843325def40` builds successfully. Production/staging remains a separate deployment step.

- **2026-11-03** (Codex) — Renamed the live Bonus Pot module to `/bonus-pot/` and updated the Class Job Market iframe, shared-login return allowlists, demo links, Docker parse gates, focused tests, documentation, and Public Economics/Religion lecture sources. `/coordination-games/` is now a compatibility-only redirect to `/bonus-pot/`; it does not own state. Focused R payout/privacy/reset/unification tests, all three app parser checks, and the Demo Kit OAuth return-path test pass. Public Economics lecture rendering is verified. Immutable image `4232dccc80a7ab76546601c800b4fe17917cb350` was staged successfully: root and `/bonus-pot/` returned HTTP 200, `/coordination-games/` served the redirect script, both route apps parsed in-container, and the published-image Bonus Pot payout/unification fixtures passed.

- **2026-11-02** (Codex) — Fixed standalone Coordination Games admin environment switching. The form now shows an explicit Active Course selector, restricts Active Section to authorized sections in that course, validates the selected pair rather than stale hidden session state, and activates that pair only after a successful save. Added a regression guard to the focused Bonus Pot test. Focused Bonus Pot and admin-role tests plus an app parser check pass. Published image `2192943bdf0760dce7a9dc8d090e392c65600f65` was staged successfully: healthy container, HTTP 200, seeded database, in-container app parse, and focused Bonus Pot regression all pass.
- **2026-11-02** (Codex) — Corrected the standalone Coordination Games denominator radio label: the first choice now renders the actual section-size label (for example, `Section class size (24 enrolled)`) rather than the internal variable name `cs_radio_lbl`. Added a regression assertion. The published image `6a3b44b6a13be96d614c9d1dd19ef5c7c4b6f0cd` was staged successfully: healthy container, HTTP 200, Bonus Pot payout fixture, and label-source verification all pass.

- **2026-11-02** (Codex) — Added an opt-in Bonus Pot denominator mode, **submits at least**, with an instructor-entered token threshold. Only qualifying submitters receive the divided multiplied pot; below-threshold submissions remain valid and still contribute to the pot. The legacy enrolled-section and all-submitter modes retain their prior payout rules. Added idempotent SQLite migration fields, synchronized both instructor control surfaces, and extended `tests/bonus-pot-public-good.R` to verify threshold payout and the unchanged enrolled-section result. Focused fixtures, app parsing, and the Class Job Market smoke test pass. The published SHA was staged in an isolated container, with HTTP 200, healthy container state, the published-image payout fixture, and migration-column verification all passing. Production promotion awaits Reclaim SSH public-key access.

- **2026-10-27** — Aligned Bonus Pot with the public-good game used in ECON 160: when the enrolled-section denominator is selected, the multiplied pot is now paid to every active, non-admin, non-demo student in that section—including zero contributors. The fallback remains submitter-only when no enrolled roster is available. Added `tests/bonus-pot-public-good.R`, which verifies that one contributor and two zero-contributors each receive the same (1.5\times2/3=1) token share; the Docker build runs it.

- **2026-10-27** — Replaced boolean-only administrator state in Class Job Market with durable role assignments. Legacy and `ADMIN_EMAILS` administrators migrate to `super_admin`; `course_admin` and `section_admin` assignments limit the signed-in administrator’s course/section picker and roster workflows. Super-admins can grant roles in Settings → Students or import `admin_role` in a CSV; imports never overwrite an existing super-admin account. Added `tests/admin-roles.R`, which verifies legacy migration, scoped grants, compatibility synchronization, multiple roles, and protection of the final super-admin. That fixture, app parsing, and the collective-action fixture pass. The broader Job Market smoke fixture currently has a pre-existing mismatch at its volunteer-clearing-wage assertion: the current implementation deliberately returns the frozen cold-call wage/default instead of the older category-bid rule; this role-only diff does not touch that code path.

- **2026-10-27** — Confirmed that existing student rosters, draws, game denominators, Flex funding, and bulk awards already filter `is_admin=0`. Closed the legacy embedded-game loophole: admins now see a management message and server-side submission handlers reject their Bonus Pot, Prisoner’s Dilemma, and Price War choices. The new Collective Action engine freezes a course/section roster from the same active/non-demo/non-admin predicate by default; its focused SQLite fixture verifies that an instructor and an outside-course student are excluded.

- **2026-10-27** — Added a Collective Action prototype engine beside the legacy Bonus Pot. It creates course/section-scoped sessions and rounds, freezes a round roster, supports linear and threshold mechanisms, uses one UPSERT-backed choice per student, and settles immutable outcomes and roster-wide payouts. `tests/collective-action-prototype.R` covers revised choices, a linear payout, a threshold unlock, and post-lock rejection. The prototype has no student/instructor UI or deployment approval yet.


- **2026-10-26** — Added `tests/critical-live-score-audit.R`, a SQLite fixture with unresolved Oct. 7 and Oct. 12 assignment scores in a selected section, plus a committed control event and an out-of-scope record. The Dockerfile now runs it during image build, so a bad cross-date audit query blocks GHCR publication.

- **2026-10-26** — Diagnosed Live Score Audit visibility across dates. The audit query had removed its round placeholder but still passed an empty parameter list, which the DB helper swallowed as an empty result. Restored a no-parameter query and aligned Commit all/Clear pending with the displayed cross-date, selected-scope queue. Added a source-level regression assertion; broader fixture coverage is now required by the non-regression protocol.

- **2026-10-26** (Codex) - Added individual Activate/Deactivate controls and Exam-tag batch Activate all/Deactivate all controls to Flex Questions. The admin table now includes inactive questions for restoration and editing, while student funding continues to use only active candidates. R parsing, diff checks, and a disposable SQLite activation test pass.
- **2026-10-25** (Codex) - Repaired the Review Quiz Admin empty-session state: before any student submits an answer, the Review Sprint payout summary now displays zero correct responses instead of attempting to total an empty vector and failing the Admin render.

- **2026-10-25** (Codex) - Made Review Quiz use the Class Job Market gateway rather than a second credential screen. Expanded the OAuth relay’s strict return-path allowlist to include Review Quiz, while retaining the existing cold-call route and rejection of external/unrecognized paths. Added return handling for Google, password, and shared-cookie Job Market sign-in, and covered the allowlist with the demo-kit test suite.

- **2026-10-25** (Codex) - Started the immutable `661d153` image on the isolated staging port, seeded its separate database, confirmed HTTP health and app parsing, confirmed the Review Quiz auth handoff is served as executable JavaScript, and ran the payout/session regression against the staged database.

- **2026-10-23** (Codex) - Replaced the Review Quiz credential form with Class Job Market single sign-on: a valid existing shared session logs the student into the quiz automatically, while an unauthenticated student is sent to Class Job Market to sign in. Added an isolated lookup regression to the Review Sprint payout smoke test.

- **2026-10-23** (Codex) - Made Token Admin individual adjustments accept quarter-token increments and display fractional awards accurately. Added a selected-student earned-token award, so a verified three-person Learning Under Noise Taboo crew can be credited in one action rather than three.

- **2026-10-23** (Codex) - Added the Review Sprint payout bridge to `apps/review-quiz`: persistent quiz-session keys, a preview-and-confirm instructor release, auditable payout rows, and shared-wallet credits for correct responses. A session cannot be paid twice, and reset/new-bank actions advance to a fresh session. Added an isolated SQLite smoke test covering score calculation, ledger write-back, zero-score audit rows, and duplicate-release prevention; made the release transaction use the sandbox database in demo mode.

- **2026-10-19** (Codex, `feature/lightweight-js-apps`) - Implemented the
  first isolated lightweight static pilots: browser-only Tax Incidence and
  Airplane Production tools, a minimal Cloudflare Worker asset scaffold and
  health route, pure-logic/state/CSV/page tests, and a local Worker smoke
  check. The preview has no production hostname or Cloudflare deployment, and
  does not access the Shiny database or course records.

- **2026-10-19** (Codex, `feature/lightweight-js-apps`) - Created an isolated
  lightweight-apps planning branch and added `LIGHTWEIGHT_APPS_PLAN.md`. The
  plan follows the existing Cloudflare audit: plain JavaScript/TypeScript for
  deterministic browser tools, Durable Objects for anonymous temporary rooms,
  and no migration or synchronization of the authoritative Shiny course
  database until separately decided.

- **2026-10-16** (Codex) - Changed assessment reveal behavior so student Gradebook remains visible: unrevealed linked items are excluded from the displayed calculation until revealed. Also made Account → Pending assignments exclude active problem sets once the student has a recorded grade for the matching gradebook item. App parsing and diff checks pass.
- **2026-10-16** (Codex) - Added a per-import post-upload gradebook-item selector and Save link control in Assessment display settings. It repairs legacy imports that lack a stored link, enabling the reveal gate to withhold the appropriate overall-grade effect without touching grades. App parsing and diff checks pass.
- **2026-10-16** (Codex) - Added post-import Assessment display settings in the Gradebook panel. Each import in the active class/section scope can now toggle between immediate display and required reveal without altering summary values or gradebook rows; enabling the gate resets prior reveals. App parsing and diff checks pass.
- **2026-10-16** (Codex) - Added the per-assessment “students must click Reveal grade” import option. A locked assessment shows only a Reveal button; its linked course-grade breakdown is also withheld until the student reveals it. Reveals persist per student and assessment. R parsing and a disposable SQLite reveal-flow check pass.
- **2026-10-16** (Codex) - Put each student’s final assessment grade behind a closed “Reveal grade” control; score-detail expansion remains a separate action.
- **2026-10-16** (Codex) - Restyled the student assessment-detail disclosure as a bordered “Show score details” control so its clickability is obvious.
- **2026-10-16** (Codex) - Added assessment-summary final-grade write-back controls: an explicit item dropdown, exact-name matching, and a deliberate new-item path. The importer now writes mapped final scores, maximums, and percentages to the selected gradebook item and protects them as manual imports.
- **2026-10-16** (Codex) - Made `overall_exam_note` a first-class text mapping in the assessment-summary importer. It auto-selects the matching column in a midterm summary and appears as individual feedback on the corresponding student Account card.
- **2026-10-16** (Codex) - Added an optional assessment-wide note field to the mapped summary importer. It is independent of the cap-adjustment note and appears to students before the collapsed score-detail table.
- **2026-10-16** (Codex) - Added a section-scoped mapped assessment-summary importer to Class Job Market. It accepts CSV/XLSX, auto-suggests midterm-summary columns, supports arbitrary additional subpart columns, safely skips unmatched roster rows, persists an optional cap-adjustment note, and shows each student’s final grade at a glance with details collapsed in Account. App parsing and diff checks pass; existing smoke/startup suites retain unrelated pre-existing failures.
- **2026-10-14** (Codex) - Repaired the slide-embedded cold-call login after the Google OAuth migration: the sign-in button now leaves the iframe, nginx forwards the requested return path, the relay stores a validated return path and restores the compact `view=cold-call-slide` panel after authentication, and roster matching is case-insensitive.
- **2026-10-14** (Codex) - Removed username interpolation from class-job-market tiebreak SQL: all user-id lists now use bound SQLite placeholders. Added Shiny proxy request-size and browser-response protections that preserve normal Shiny WebSockets and slide embeds. The app parses cleanly; the existing smoke test still stops at its volunteer-clearing-wage assertion.
- **2026-10-07** (Codex) - Fixed the first-load Policy Rubrics panel crash:
  an ungraded team now supplies an explicit empty score data frame before the
  criterion cards render. Expanded the server test to render the real Settings
  panel before exercising draft, release, and gradebook write-back.

- **2026-10-07** (Codex) - Added interactive instructor grading for the policy
  presentation, progress/revision report, and written brief. Rubrics support
  editable five-level anchors, criterion and overall feedback, private drafts,
  released team snapshots, student Account delivery, and optional gradebook
  write-back. Added startup/scoring assertions, a server-level
  draft/release/write-back test, and a manual QA script. The container run
  passed all new coverage; two unrelated pre-existing startup expectations
  remain failing.

- **2026-10-05** (Codex) - Added topic and draw-probability metadata to Flex Questions, exposed 75/50/25 percent chances to students and admins, and extended private CSV import validation and startup schema tests. The question text remains in the private PubEcon repository rather than the public app repository.

- **2026-10-02** (Codex) - Added persistent course/section scopes with one-, many-, and explicit-all selection; per-scope active job rounds; scoped tracker/draw/redraw/audit rosters; scoped roster, token, gradebook, public-good, coordination-game tracker, and export defaults; course-specific Flex banks; and scope-specific Flex formulas, contributions, thresholds, and reveals. Legacy blank scope settings now select the first actual course and section instead of silently including everyone.

- **2026-10-02** (Codex) - Rebuilt Flex Questions as the Candidate Question
  Fund: added shared contribution and unlock state, class-wide progress/reveals,
  token-ledger debits, instructor contribution exports, and immediate refreshes.
  Settings now accepts either comma-separated thresholds or formulas in q and N,
  with an eight-question live preview. Added schema and pricing regression
  assertions; base-R checks confirmed the convex example produces 20, 25, 40,
  and 65 tokens at N=20.

- **2026-10-02** (Codex) - Made assignment-time wages authoritative in random and point-allocation modes, keeping clearing prices exclusive to wage bidding. Changed Add Assignment Back into a normal selected-date repair that always refreshes Today and job-pool counts. Expanded Last Class Jobs Still Pending from one prior session to every revealed overdue assignment and added its indexed class date to the table.

- **2026-10-01** (Codex) - Connected extension purchases to the generic
  Cloudflare assignment API. Added per-problem-set Cloudflare assignment and
  extension-target mapping, persisted sync status/errors, automatic sync after
  purchase, an admin purchases table with student/user ids and retry controls,
  CSV round-tripping for the new fields, server-only credential handling, and
  focused unit/startup coverage. Failed or unconfigured syncs do not reverse or
  duplicate the token purchase and remain visible for safe retry.

- **2026-09-30** (Codex) - Replaced instructor-facing rounds with date-based Job Market Controls. Selecting a new class date carries job posts and standing bids forward while preserving dated history. Added immutable draw-time wage snapshots for every active post, including volunteer and cold-call wages; both main-job and cold-call Draw close bidding until the configured reopen time (or manual reopen), while Open now remains an explicit override.

- **2026-09-30** (Codex) - Scoped Job Market bid polling to the logged-in student so one student saving wage bids cannot rebuild classmates' dynamic forms and replace their unsaved entries with defaults or zeroes.

- **2026-09-30** (Codex) - Made the Active Lecture selector switch immediately instead of requiring a separate, easily missed button. Random assignment now forces bidding closed on create, update, and startup reconciliation while preserving existing bids for a later switch back. Fixed Live Tracker cache invalidation so changes to a lecture's assignment mode immediately replace its prior wage- or point-bidding display and drive the next draw. Reconciled browser-restored lecture selections on initialization so the selector and active lecture details cannot point at different lecture IDs.

- **2026-09-29** (Codex) - Added per-lecture controls for allowing multiple
  regular jobs and choosing pay-as-bid or uniform reverse-second-price wage
  allocation. Applied the multiplicity rule across wage, point, and random
  draws; kept cold calls independent; made auction-assigned wages authoritative
  through scoring; and carried both settings into newly created lectures.

- **2026-09-29** (Codex) - Migrated assignment uniqueness from one row
  per student/lecture to one row per student/job/lecture, updated random, wage,
  and point allocators plus imports/redraws/student views for multiple jobs, and
  added editable clearing order to current posts and reusable templates.

- **2026-09-29** (Codex) - Removed existing regular-job assignees from the
  timed-draw exclusion only for during-class cold calls, kept start/end job
  exclusivity, and added regression coverage for the timing-specific rule.

- **2026-09-29** (Codex) - Added an Active Lecture selector to Round Setup,
  routed Today, Job Market, Live Tracker, Jobs, draws, scoring, imports, and
  reveal actions through the selected lecture, and blocked switches while a
  draw preview or uncommitted live scores exist.


- **2026-09-28** (Codex) - Added a reveal-aware Last Class Jobs section
  to Today for unfinished non-volunteer assignments from the immediately prior
  round. Today now distinguishes the newest round from carryover work, while
  Live Tracker retains access to the pending assignment round.

- **2026-09-28** (Codex) - Fixed wage bidding to render and save one minimum-
  wage submission for every active job post, calculate draws and clearing wages
  from those post-specific bids, preserve legacy category bids as a fallback,
  and added fresh/legacy schema plus same-category per-job regression coverage.

- **2026-09-28** (Codex) - Replaced the misleading bid-open checkbox with
  Open now / Open on schedule / Closed states. Open now now bypasses expired or
  future hours and the recurring class lock in both rendered forms and
  server-side wage/point submission handlers; existing value `1` rounds map
  directly to the manual override.

- **2026-09-28** (Codex) - Repaired the deployed sandbox upgrade regression:
  legacy job tables now gain the current category, round, post, template, bid,
  and assignment columns before fake-job seeding. Added an old-schema
  reproduction that verifies fake jobs plus the actual Add Job Type and Add Job
  Post writes, without deleting existing sandbox records.

- **2026-09-28** (Codex) - Added a non-destructive, one-time demo job-market
  seed with four fake bidding categories/posts, an open ten-ticket practice
  round, DEMO 101 rosters, and regressions proving later sessions neither
  duplicate the round nor erase a submitted rehearsal bid.

- **2026-09-28** (Codex) - Added per-round manual bidding enable/pause controls
  and exact opening/closing hours, made wage and point saves enforce them,
  refreshed open student sessions after round edits, preserved legacy date-only
  windows, and added schema/time-window regressions.

- **2026-09-28** (Codex) - Added a persistent class-announcement editor to App
  Settings and a student-facing announcement card on Today, including blank-to-
  clear behavior, live polling, a length guard, and startup/source regressions.

- **2026-09-25** (Codex) - Launch-hardened point bidding: fixed cross-section
  assignments, removed volunteer/during-only categories from the point form,
  enforced mode/date/budget rules server-side, prevented failed draws from
  clearing prior assignments, repaired a missing catalog-seed SQL parameter,
  and made shared helpers testable in isolated app environments. Expanded the
  scratch-DB regression test; all 210 unit assertions and the market smoke test
  pass.

- **2026-09-24** (Codex) - Migrated Demo Kit direct OpenAI traffic to the
  Responses API with GPT-6 Luna/Sol workload routing, typed streaming and
  output parsing, environment overrides, updated UI copy, and Node contract
  tests. Migrated the secondary R generator to GPT-6 and retained OpenRouter
  on its existing Chat Completions contract.

- **2026-09-24** (Codex) - Added a case-insensitive unique grade key, startup
  cleanup that keeps the newest duplicate, CSV upserts, and a manual grade
  editor that loads and replaces an existing student-assignment grade.

- **2026-09-23** (Codex) - Replaced the incompatible wide blank gradebook
  download with an upload-ready long-form CSV containing prefilled user IDs,
  assignment names, and maximum scores. Added an on-screen column rubric and
  made imports ignore untouched rows, validate percentages, and honor the
  upload-panel week tag when a row leaves week_tag blank.

- **2026-09-23** (Codex) - Stabilized Live Tracker class/section selection,
  filtered demo identities from its pickers, restored a persistent
  start/end/all Reveal/Hide toggle, and added a section-selection prompt when
  Reveal is clicked without an active section.

- **2026-09-23** (Codex) - Moved Your Jobs to a prominent Account panel above
  balances and profile content, moved Grade Breakdown to the bottom, added the
  explicit "No assigned job." Today state, and removed during-class cold-call
  assignments from Today's job boxes.

- **2026-09-23** (Codex) - Expanded Account → Job History to 20 assignments
  with round/date context, status badges, pending-review states, and earned or
  potential token values. Connected Account to the assignment poll so new draws
  and scores appear automatically, and added status-model regression coverage.

- **2026-09-21** (Codex) - Removed arbitrary mid-word button wrapping, gave the
  Live Tracker Clear/Release slot the unused width beside Reveal/Hide, replaced
  repeated volunteer outcomes with large S/T/M buttons and a visible key,
  standardized file inputs on a nonbreaking Browse label, and restored spacing
  between checkbox controls and their labels. Updated responsive assertions.


- **2026-09-21** (Codex) - Enabled Shiny's supported automatic new-session
  reconnection for class-job-market. Phones can now recover after their browser
  suspends the WebSocket without a manual page reload; the existing login cookie
  restores the instructor account and persisted live-score records remain
  available. Added a startup regression assertion for reconnect enablement.

- **2026-09-21** (Codex) - Reflowed the enlarged class-job-market controls on a
  wider desktop canvas with tighter phone gutters, wrapping button groups and
  modal actions, stacked phone launch cards, and flexible bid inputs. Shortened
  crowded action labels while preserving context and tooltips, and added static
  regression assertions for the responsive width and wrapping rules.

- **2026-09-21** (Codex) - Increased the class-job-market default text scale,
  repaired and exposed the persistent text-size slider on phones, enlarged
  buttons and form fields to accessible touch targets, and increased dense
  mobile table text. Preserved compact projector styling and added static
  regression assertions for the accessibility rules.

- **2026-09-21** (Codex) - Added a prefilled manual policy-group editor to
  Settings → Students. Instructors can select one student and update the team,
  presentation date, course unit, interests, assigned rank, and allocation
  seed; the student Account profile refreshes immediately. Manual edits and CSV
  imports now share one tested upsert path.

- **2026-09-21** (Codex) - Replaced fixed extension options with a validated
  convex pricing formula, student hours slider, configurable shortcut buttons,
  and live balance preview. Added assignment edit controls, active-state and
  deadline/solutions-date maintenance, unlimited assignment visibility, and a
  downloadable CSV bulk upsert workflow with row-level validation. Added
  startup coverage for settings defaults and representative convex prices.

- **2026-09-21** (Codex) - Fixed locked demo logins by changing sandbox user
  seeding from insert-only to an idempotent credential reset, then repaired a
  deployed-only failure where an older sandbox `users` table lacked columns
  selected by the current login path. Bootstrap now migrates those columns
  before resetting and verifies Alice and instructor credentials.

- **2026-09-21** (Codex) - Made Today and job-pool fill counts show only
  unfinished assignments and refresh immediately when an outcome is queued.
  Added a work date and opt-in Today visibility to Add Assignment Back so
  historical corrections do not clutter the current class view.

- **2026-09-17** (Codex) - Added a persistent Show help / Hide help switch and
  concise tutorial sequences for Today, Job Market, Live Tracker, and Settings.
  Help visibility is stored in browser local storage and remains responsive on
  phone-sized instructor screens.

- **2026-09-14** (Codex) - Replaced least-called-first cold-call selection
  with uniform random draws with replacement. Added round-specific absence
  tracking plus Absent & Redraw controls in both tracker and slide views, and
  connected assignment-redraw absences to the same exclusion list.

- **2026-09-14** (Codex) - Added optional quick descriptions to job posts and
  templates, including add/edit forms and round-to-round copying. Student
  assignment, revealed-job, and job-pool views now show those instructions on
  click without expanding every card by default.

- **2026-09-14** (Codex) - Consolidated assignment visibility into a stable
  Start/End/All selector and one reveal/hide toggle. Preserved the selection
  across tracker refreshes, made All update both timing groups, and removed the
  legacy global override that revealed every section and timing at once.

- **2026-09-14** (Codex) - Fixed end-of-class draws so note-taking and other
  end-timed jobs remain indexed to the lecture just completed instead of
  silently creating and attaching themselves to the next round. Added a smoke
  regression check and clarified the draw confirmation.

- **2026-09-13** (Codex) - Reproduced production persistence with a submit,
  full reload, and separate display session. Removed the misleading one-row-
  per-browser behavior so successive submissions now append and remain visible
  instead of silently replacing the browser's previous answer.

- **2026-09-13** (Codex) - Fixed the live poll erasing in-progress answers by
  separating metadata/form invalidation from result polling. Added a compact
  Clear responses control with modal password confirmation, and passed a real
  browser test covering five seconds of typing, submission, and clearing.
  Also propagated `SHINY_PASSWORD` and `LIVE_POLL_BASE_URL` into Shiny worker
  environments; Shiny Server's login worker otherwise discarded both variables.

- **2026-09-13** (Codex) - Added a scrollbar-free `display=1` projection mode
  for the live word cloud and changed generated Quarto iframe snippets to use
  it. The exact container passed its smoke test and a 720-by-420 browser check
  with content height equal to viewport height.

- **2026-09-13** (Codex) - Generalized the slide word cloud into a reusable
  poll manager. Added persistent question definitions, URL routing, instructor
  create/edit/open/close controls, per-question clearing, generated embed URLs,
  and multi-poll isolation tests.

- **2026-09-13** (Codex) - Added apps/live-wordcloud, a slide-sized
  dependency-light response form and collision-free flex word cloud. Added
  response normalization, idempotent submission keys, SQLite persistence, live
  three-second refresh, password-protected clearing, and a smoke test covering
  validation, aggregation, and response replacement.

- **2026-09-09** (Codex) - Fixed the bulk importer to classify cold-call and
  voluntary job posts as repeatable score events. Multiple completed CSV rows
  for one student now enter Audit independently; only regular assigned jobs
  trigger already-committed and duplicate-assignment checks.

- **2026-09-09** (Codex) - Removed cross-type student exclusion guards from
  cold calls and voluntary participation, stopped voluntary commits from
  overwriting assigned jobs, and added per-event delayed-credit tracking so
  repeated events commit and release independently.

- **2026-09-09** (Codex) - Verified that Token Admin individual adjustments
  remain available after job commit for signed balance corrections, preserve
  the locked job record, and create a separate audited ledger entry; clarified
  this workflow in the UI.

- **2026-09-09** (Codex) - Changed job wages from draw-time snapshots to
  commit-time snapshots: Today, Pending, and Audit reflect current post wages,
  while audit commit locks the final wage and award. Fixed committed-state
  checks to use the outcome rather than mistaking a token amount for a Boolean,
  so both two-token completions and zero-token misses lock correctly.

- **2026-09-09** (Codex) - Replaced half-wage rounding with the fixed
  Complete=wage, Tried=1, Missed=0 rule across live grading, bulk imports,
  voluntary jobs, and outcome previews; removed the obsolete multiplier UI.

- **2026-09-09** (Codex) - Added all currently revealed start/end job
  assignments to Today, scoped to the student's section or the instructor's
  active class/section, so the instructor can project Today instead of Live
  Tracker.

- **2026-09-09** (Codex) - Moved bulk/offline recovery into a collapsed panel
  beneath Live Score Audit and clarified that known outcomes should be imported
  as `complete`, `tried`, or `missed`; only pending work stays blank.

- **2026-09-09** (Codex) - Tightened offline and bulk job recovery so imports
  persist `user_id`; then added pasted CSV support and exact name-or-ID input
  with explicit unmatched and ambiguous-student reporting.

- **2026-09-09** (Codex) - Added pasted-list and fallback-CSV bulk job
  imports, a scoped downloadable offline sheet, exact student and job
  validation, outcome normalization, and protection for Audit and committed
  assignments.

- **2026-09-08** (Codex) - Added a policy-group assignment table, instructor
  CSV importer, and student Account-profile card showing team, presentation
  date, course unit, and group members.

- **2026-09-08** (Codex) - Rebuilt the Live Tracker flow around disjoint
  pending, volunteer-eligible, and audit panels; moved audit to the bottom,
  added explicit audit-to-pending returns and duplicate guards, and separated
  start and end assignment reveals.

- **2026-09-02** (Codex) - Removed cold-call job-post creation from the
  embedded slide workflow. Draws now queue a standalone `cold_call` score with
  an `answer` or `board` outcome and use the existing Cold Call category wage.

- **2026-08-31** (Codex) - Added an authenticated `?view=cold-call-slide`
  mode to `class-job-market` that exposes a compact cold-call draw/record
  panel for embedding in Reveal/Quarto slides while preserving the existing
  `live_score_events` write path.

- **2026-08-26** (Claude Code, branch `claude/class-job-market-simplify-szx2ai`) — Fixed settings-panel reactivity, student-poll invalidation keys, and fresh-DB migration ordering in class-job-market. Replaced the seeded job catalog with the simplified 4-category/11-template model plus a one-time data migration. Added template-level timing/voluntary/in-draw/auto-copy (carried into rounds by "Create next round"), a `during` timing with a cold-call draw filter, the recurring bid-lock schedule, the volunteer clearing-wage engine with three rules and a per-class posted-demand editor, student-facing volunteer wages, and a scratch-DB smoke test.

## Next actions

1. Stage immutable image `eef2732b22c2`; manually verify instructor item rename and weight save, reweight approval, the affected student grade, and both policy milestone dates before production promotion.
2. Define the per-course source contract for loading `course_data` from the course page or GitHub, including repository/path configuration, caching, authentication, validation, and failure behavior; then replace the duplicated policy-date defaults with that source.
3. Publish the PS6 Cloudflare Worker assignment, then enable the preconfigured PS6 and PS6 corrections extension products in Settings → Spending Options → Assignment Extensions.

## Questions for Kyle

- Should students see the posted demand k itself, or only the implied wage (current behavior)?
- For ECON 160, what should the uploaded course and section labels be (for example, `ECON 160 Fall 2026` / `Religion module`)?


## Non-regression protocol for Class Job Market

For any change to apps/class-job-market, treat current instructor and student workflows as a compatibility contract. Before implementation, record: (1) behavior that must remain unchanged, (2) requested new behavior, and (3) concrete verification cases.

Required verification:

1. Add or update a regression test for every production defect. Use a realistic SQLite fixture when state spans dates, sections, bidding modes, or historical records.
2. Test the exact workflow path, not only helper functions or startup. A swallowed database error must be surfaced by the test.
3. Run the focused regression test plus relevant smoke tests before deployment.
4. Stage the immutable image and manually verify the affected instructor and student workflow before production promotion.
5. Do not call a change resolved until the tested behavior, test command, staging image, and deployment commit are stated.

Critical invariants: unresolved assignments and pending scores remain visible and actionable across class dates; course or section scope may filter records but must not strand them; data migrations preserve historical records; and destructive actions require confirmation plus a recovery path.

- **2026-11-07** — Make Risk & Insurance Results a role-invariant anonymous market dashboard. Scope: both students and instructors see the same revealed-round overview, aggregate comparison table, and histograms for WTP, implied CARA, and implied CRRA; histograms can be split by any combination of round settings or starting-endowment band. Student-specific choices and realized outcomes remain on Play, with instructors using the existing scoped impersonation control when they need to inspect that view. Invariants: Results never contains a named or own-outcome panel, unrevealed rounds remain excluded, undefined CARA/CRRA values are omitted rather than imputed, and Settings stays instructor-only. Acceptance: an instructor and a student see the same market controls; selecting WTP/CARA/CRRA and one or more setting dimensions redraws anonymous histograms; a revealed student's net change appears on Play only. Dependency check: plots and tables consume the same `all_revealed_analytics()` rows. Verification: expanded `tests/risk-insurance-game.R`, parser checks, and the full Docker suite pass in local image `0114924c4ab9`.
- **2026-11-07** — Authorized instructors can switch or turn off the selected environment's active game directly from Games & Demos. Scope: this adds a second control surface for the existing scoped active-game value; it does not move game setup, settlement, or round data out of their existing controls. Invariants: students and impersonated views cannot mutate the active game; Settings → Game Controls remains functional; switching affects only the selected class/section scope; and clearing preserves game records. Acceptance: selecting a game on Games & Demos persists it and displays Active Now, while selecting No active game clears the scoped value and removes the active-game window entirely. Dependency check: both controls call one validated activation function and use the same `scope_arcade_state` record. Verification: `Rscript --vanilla tests/critical-games-page-activation.R`, existing game-navigation and authorization regressions, parser check, and full image build before deployment.
- **2026-11-07** — Risk & Insurance now supports in-game instructor impersonation and configurable revealed-results comparisons. Scope: an authorized instructor can view/stop viewing as any active student in the selected environment; revealed results can be selected by round and grouped by round, loss rule, loss size, bad-state probability, premium rate, real-balance application, and starting-endowment band. Invariants: impersonation cannot cross course/section scope, it does not alter authentication, sealed results remain sealed, and comparisons read persisted choices/outcomes without recomputing settlements. Acceptance: the instructor enters and exits a student view inside the game, selects either revealed round, and can combine setting dimensions in the comparison table. Dependency check: player identity continues to use the existing `player_user`/`player_name` boundary and all comparison rows come only from `status='revealed'` rounds. Verification: expanded `Rscript --vanilla tests/risk-insurance-game.R` includes the revealed Demo Alice impersonation path; parser checks and the full Docker suite pass in local image `ff9ecc71df49`.

- **2026-11-07** — Added a classroom-resilience gate for both insurance activities without changing game mechanics. Scope: automated tests now exercise the real Pass Insurance and Risk & Insurance Shiny observers across fresh sessions, and a two-device rehearsal covers live reconnect/projector behavior. Invariants: one purchase, claim, outcome, and dividend per applicable student/round; other sections remain isolated; closed contracts are immutable; all insurance cashflows remain non-earning; an uninsured live pass remains valid; and repeated clicks or settlement do not duplicate money. Acceptance: a browser retry creates no second debit, a second insured pass creates no second claim, a late buyer is rejected, profit reaches the full active in-scope roster including nonbuyers, and refresh/reconnect preserves saved state. Dependency check: the fixtures use the production scope, purchase, cold-call pass, settlement, reveal, and token-ledger paths. Verification: `Rscript --vanilla tests/critical-insurance-market.R` and `Rscript --vanilla tests/risk-insurance-game.R`; staging/two-device rehearsal remains required before production classroom use per `tests/INSURANCE_CLASSROOM_REHEARSAL.md`.
- **2026-11-07** — Simplified instructor Settings without changing spending, grading, roster, or token behavior. Scope: Pass Insurance, Assignment Extensions, Exam Questions, and Grade Reweighting now live under one Spending Options workspace; the standalone top-level entries are removed; Student maintenance forms are collapsed; and the roster name shows current tokens on hand plus gross positive earning tokens. Invariants: every existing input/output ID and observer remains live, scope selection remains session-local, Gradebook retains grade configuration and request review, archived students remain recoverable, and balance/gross values are read-only ledger aggregates. Acceptance: each spending subpanel renders under the shared navigation, reweight pricing no longer renders in Gradebook, all Student maintenance sections remain discoverable, and a student with 3 gross earned and 1 spent displays `2 on hand · 3 gross`. Dependency check: all save handlers were preserved and Docker now renders every consolidated panel against a disposable SQLite database. Verification: focused Settings and insurance tests, token-precision regression, the Class Job Market smoke fixture, parser checks, and full Docker build passed; local image `79f1e6eb2890`. Staging and the two-device classroom rehearsal remain separate pre-production gates.

### Required change contract

Before changing production behavior, add a concise contract to the relevant plan entry or pull-request description:

1. **Scope:** state exactly what is changing and what is explicitly out of scope.
2. **Invariants:** list the existing behaviors that must remain true, including every affected tab, role, class/section environment, and persistence boundary.
3. **Acceptance scenarios:** write concrete instructor and student workflows with their expected result.
4. **Dependency check:** before deleting, renaming, or relocating reactive or local state, search every consumer and either preserve the value or update every use.
5. **Verification:** add or update a regression test for the changed path, run it with the relevant focused tests, then run the Docker image build. For UI/state changes, complete a manual smoke test of the affected role and tab before production deployment.

If the intended behavior or a scope decision is ambiguous, stop and ask rather than inferring it. A change is not resolved merely because it parses or a helper test passes; the contract scenarios must pass end-to-end.


- **2026-10-27** — Class Job Market is the command center for modular games: it owns which game is active and every instructor lifecycle action that changes persistent state. A standalone module owns only player UI and game mechanics against that shared state. Bonus Pot follows this boundary; its module uses the Class Job Market session cookie and has no reachable instructor setup, settlement, or backup controls.

- **2026-10-27** — Games & Demos is player-facing only: configuration is in Settings. Scoped admins are pinned to `admin_assignments` for their session and cannot inherit or persist the instructor’s global course/section or change global game state.

- **2026-10-27** — Replaced the inherited global active-class behavior with session-local environments. `user_environments` preserves multiple student memberships while retaining `users.course`/`users.section` as the compatibility home record. The header only lists environments authorized by the signed-in student or admin role; switching never persists a global class selection. Docker now runs the role/environment migration fixture.

- **2026-10-27** — Made the banner the only visible active class/section selector; Live Tracker reads that session-local environment and no longer offers a competing picker. Active games now persist by that environment rather than as a global last-admin selection. Bonus Pot lifecycle controls live only in Settings → Game Controls; Active Now embeds the student player/projector surface. The standalone module uses the Class Job Market session cookie and provides responsive cards, touch-friendly actions, participation-token terminology, and browser-persistent text-size controls.

- **2026-10-27** — Replaced the banner’s combined environment selector with a closed-by-default compact “Class / section” control containing separate class and section dropdowns.

- **2026-10-28** — Standalone Bonus Pot now uses the Classroom Economy red banner and the same closed-by-default, two-dropdown class/section picker. The picker is session-local and restricted to the signed-in user’s authorized environments; it never silently changes the shared live round. A selected section only accepts play when Bonus Pot is active for that scope, while an authorized instructor can explicitly apply round settings from the standalone admin panel.

- **2026-10-28** — Token Admin’s individual adjustment roster includes active non-demo administrator accounts (including the signed-in administrator), but bulk and crew awards remain student-only. The adjustment handler verifies the target is inside the administrator’s current authorized class/section scope before writing the audited ledger correction.

## Frozen Navigation Consolidation Plan (2026-10-28)

This is a **design contract, not an implementation task**. Do not reorganize
student-facing navigation until the preservation tests below are written and
passing.

### Student navigation

The eventual student navigation has five tabs, each representing a distinct way
to engage with the course:

| Tab | Purpose | Preserved content |
|---|---|---|
| **Account** | Personal status | A compact Today subsection, token balance/history, grades, and personal job history. |
| **Participation** | In-class labor market | Job pools, bids, revealed draws, current assignments, and active-game entry points. |
| **Spend** | Token choices | Extensions, reweighting, flex-question funding, and refund/approval requests. |
| **Assignments** | Course work | Problem-set status/deadlines, policy-brief topic/question, presentation, progress report, final submission, and relevant assessment information. |
| **Games & Demos** | Interactive activities | Active game and the elective demo catalog. |

**Live Tracker** and **Settings** remain administrator-only. They must be
server-side authorization guarded, not merely hidden in the browser.

### Safe implementation constraints

- This is a UI and navigation refactor only: no grade, job, bid, token, roster,
  policy-team, or game state may be migrated, recomputed, or deleted.
- Do not render the same Shiny output ID in two places. Extract reusable UI
  helpers for Today/Participation components instead.
- Preserve every current action handler and database query unless a dedicated
  regression test shows the replacement is behaviorally identical.
- Update every programmatic tab navigation (`updateTabsetPanel`), active-tab
  poll gate, help link, and return path alongside the visible tab labels.
- Maintain the current scoped course/section rules for students and admins.

### Required regression checks before release

1. A student sees exactly Account, Participation, Spend, Assignments, and Games
   & Demos; an authorized admin additionally sees Live Tracker and Settings.
2. Participation still permits the applicable wage/ticket bid flow, shows the
   correct reveal state, and exposes assignment instructions and active-game
   entry points.
3. Account retains Today-status, token balance/history, grades, and job history.
4. Assignments retains problem-set deadlines and all student/admin Policy Brief
   workflows, including topic review and rubric navigation.
5. Direct client-side attempts to select an admin tab cannot expose admin data or
   mutate it.
6. Existing records render from their existing tables with no data migration.
7. All navigation buttons and polling paths resolve to a live destination after
