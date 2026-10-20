# Shiny/Reclaim to JavaScript/Cloudflare audit

## Decision rule

Reclaim remains the right home for the authoritative course system: shared
student identity, token balances, longitudinal records, Google integrations,
and workflows that instructors may need to inspect or repair directly in
SQLite. Cloudflare is the better home for calculators, projected visuals, and
short-lived room-based games that do not write to that authoritative database.

Use three Cloudflare shapes:

1. **Static JavaScript:** all state stays in one browser. Host as Worker static
   assets or Pages; no database or server process.
2. **Durable Object:** a temporary class room or poll needs shared live state.
   The object persists small state, serializes concurrent actions, and
   hibernates between requests.
3. **D1/R2 application:** durable per-student records or file uploads are
   required, but the app does not need the Reclaim SQLite database.

Do not create bidirectional synchronization between Cloudflare and the shared
Reclaim SQLite database merely to save RAM. If an activity must credit tokens
or use the canonical roster, keep it on Reclaim until the whole authoritative
course platform has a deliberate migration plan.

## Current cost baseline

Loading only `live-wordcloud` in the current Shiny image measured about 154 MiB
of container memory. The spawned R worker reported roughly 160 MiB RSS and
Shiny Server's Node process roughly 62 MiB RSS; shared pages mean these process
figures do not add directly to the container total. Each separately active
Shiny app can spawn another R worker.

## App classification

| App | Current state/dependencies | Recommendation | Priority |
|---|---|---|---|
| `live-wordcloud` | Small shared SQLite tables; no roster or ledger dependency | **Move now to one Durable Object.** Preserve questions and responses until password-protected clear; no always-on process. | Pilot underway |
| `airplanes-game` | One browser's editable table and plot; no database | **Static JavaScript.** Use an editable HTML table plus SVG/canvas chart. | High |
| `indifference-to-demand` | Deterministic consumer-choice math and plots; no database | **Static JavaScript.** Port formulas and render SVG/canvas charts client-side. | High |
| `tax-incidence` | Deterministic incidence/welfare calculations and plot; no database | **Static JavaScript.** Strong slide-embed candidate. | High |
| `theory-of-firm` | Deterministic demand/cost optimization and plots; no database | **Static JavaScript.** Port numerical root/grid calculation and charts. | High |
| `sloman-trading-game` | Instructor-operated, session-only round state and timer | **Static JavaScript** with localStorage export/import. Use a Durable Object only if teams will enter quantities from separate devices. | High |
| `restricted-seller-game` | Explicitly session-only 15-minute exercise | **Static JavaScript** if operated from one screen; Durable Object if buyers/sellers submit separately. | High |
| `bonus-entry` | Process-global pooled responses, reveal chart, CSV download | **Durable Object.** It needs a shared class room but not the course database. | High |
| `club-insurance-game` | Pooled roster, balances, choices, shocks, and CSV; no external DB | **Durable Object.** A room object maps naturally to its round state. | High |
| `excise-tax-game` | Room codes, private cards, orders, rounds, SQLite; no shared roster/ledger | **Durable Object.** One object per room; browser tokens remain anonymous. | High |
| `review-quiz` | Real-time quiz plus shared users/auth database | **Conditional.** Make a standalone room-code Durable Object version if results need not track participation; otherwise keep on Reclaim. | Medium |
| `supply-auction-game` | Own auction state plus shared auth/DB and optional Drive backup | **Split decision.** A no-credit room-code version fits Durable Objects; retain the current app when roster/token integration matters. | Medium |
| `demo-kit-site` | LLM proxy, generated-game persistence, existing Node companion service | **Keep on Reclaim for now.** It is already moving toward Node; D1/R2 is possible later but is not a Shiny-memory quick win. | Medium |
| `price-index` | Shared authentication, longitudinal student baskets, SQLite, Drive backup | **Keep on Reclaim.** Consider D1 only as part of an authoritative student-data migration. | Low |
| `coordination-games` | Shared login, sections, token debits/payouts, Google backup | **Keep on Reclaim.** Its teaching value depends on the canonical ledger. | Low |
| `class-job-picker` | Shared users, persistent job history, admin console, Google Sheets write-back | **Keep on Reclaim.** A future D1 migration should be coordinated with `class-job-market`. | Low |
| `class-job-market` | Authoritative users, OAuth sessions, jobs, grades, token ledger, admin workflows | **Keep on Reclaim.** This is the persistence boundary, not an incremental port. | Do not pivot piecemeal |

## Recommended sequence

1. Finish and classroom-test `live-wordcloud` as the Durable Object pilot.
2. Port the four deterministic visualizers: `tax-incidence`,
   `indifference-to-demand`, `theory-of-firm`, and `airplanes-game`.
3. Port `bonus-entry`, `club-insurance-game`, and `excise-tax-game` using a
   reusable room/admin/clear Durable Object shell.
4. Decide whether `review-quiz` and `supply-auction-game` need participation
   credit. Only standalone versions move.
5. Leave the shared roster/ledger apps on Reclaim until there is a single
   migration plan for the authoritative database.

## Exit criteria for each pivot

- Same classroom flow and economics as the Shiny version.
- Stable phone URL, projected display URL, and password-protected reset.
- No page reloads while a student is entering data.
- Export available before any intentionally ephemeral state is cleared.
- Browser test with at least two simultaneous participants.
- Old Shiny route retained until the replacement survives one real class.
