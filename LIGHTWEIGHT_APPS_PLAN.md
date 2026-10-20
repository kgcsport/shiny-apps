# Lightweight JavaScript Apps Plan

## Purpose

Build a separate, low-RAM delivery path for teaching tools that do not need
the authoritative course database. This work lives on
`feature/lightweight-js-apps` until its first classroom-tested demo is ready
to merge. Existing Shiny routes remain the production fallback throughout.

This plan implements the boundaries established in
`CLOUDFLARE_MIGRATION_AUDIT.md`; it does not migrate the course platform.

## Architecture decision

Use TypeScript running on a Cloudflare Worker, with plain ES-module browser
JavaScript and hand-written SVG/HTML/CSS. The initial implementation must not
add React, a charting library, or a Node server to the request path.

| Activity shape | Runtime | Storage | First candidates |
|---|---|---|---|
| One browser, deterministic | Static Worker assets | Browser memory; optional `localStorage` | `tax-incidence`, `airplanes-game`, `indifference-to-demand`, `theory-of-firm` |
| Temporary shared classroom | Worker + one Durable Object per room | Durable Object storage | `live-wordcloud`, then `bonus-entry` |
| Course record / canonical identity | Existing Shiny/Reclaim system | Existing SQLite and integrations | `class-job-market` and all roster/ledger apps |

No bidirectional synchronization with the Shiny SQLite database will be
introduced. A lightweight activity that later needs participation or token
credit stays standalone until a deliberate authoritative-platform migration
defines that contract.

## Repository shape

Create `lightweight-apps/` when implementation begins:

```text
lightweight-apps/
  src/
    worker.ts                 # asset routing and API boundary
    rooms/live-wordcloud.ts   # Durable Object
    static/tax-incidence.ts   # pure calculation + SVG renderer
    static/airplanes-game.ts
    shared/                   # validation, room IDs, export helpers
  public/
    tax-incidence/index.html
    airplanes-game/index.html
    styles.css
  test/
  wrangler.jsonc
  package.json
```

Keep activity mathematics in pure functions, separate from DOM code, so the
R/Shiny version and JavaScript port can be compared with fixture tests.

## Delivery phases

### 0. Baseline and harness

- [x] Add the minimal Worker/JavaScript project, local development command,
  deployment configuration, and a static health route. *(2026-10-19: static
  Worker scaffold has no production deployment or hostname.)*
- [x] Add unit tests for pure calculations and static-page smoke checks for the
  two initial static pages. *(2026-10-19: `npm test` passes; browser-device
  checks remain part of Phase 1.)*
- [ ] Record image/process memory and cold-start measurements for the existing
  Shiny routes and the replacement URLs.
- [ ] Add separate preview and production environments; no production route
  changes in this phase.

### 1. Static browser pilot

- [x] Port `tax-incidence`: sliders, numerical results, welfare areas, labels,
  and responsive projected/phone layout. *(2026-10-20: formula fixtures pass;
  accepted for demo after local review.)*
- [x] Port `airplanes-game`: editable table, add/reset, SVG scatter plot, CSV
  export, and `localStorage` recovery. *(2026-10-19: state/CSV fixtures pass;
  browser-device checks remain.)*
- [ ] Compare fixed input fixtures with the R output and manually test Chrome,
  Safari/iOS, and a projected 16:9 display.
- [ ] Publish preview-only URLs and use them in one live class while retaining
  the Shiny URLs as the default.

### 2. Shared-room pilot

- [ ] Implement `live-wordcloud` as one Durable Object per stable poll ID,
  preserving current semantics: questions, open/closed state, repeated visible
  submissions, instructor-password clear, phone entry URL, and display iframe.
- [ ] Use WebSocket push (with hibernation) instead of the current three-second
  poll. Keep a simple HTTP fallback/reconnect path.
- [ ] Test two simultaneous phones, reset/export, page reconnect, and iframe
  embedding before a classroom trial.

### 3. Reusable room shell

- [ ] Extract room creation, instructor secret verification, state export,
  clear confirmation, reconnect, and broadcast behavior from the word-cloud
  pilot.
- [ ] Use that shell for `bonus-entry`, then choose one of
  `club-insurance-game` or `excise-tax-game` based on the next teaching need.
- [ ] Decide each activity's data-retention policy explicitly before release.

### 4. Promotion decision

- [ ] Review reliability, classroom feedback, accessibility, cost, and actual
  memory/CPU use after the pilots.
- [ ] Keep successful routes on the lightweight service; retain their Shiny
  counterparts for one semester or until an explicit retirement decision.
- [ ] Merge only the independently deployable service, tests, and deployment
  documentation after a successful demo. Do not merge a partial migration of
  the course database.

## Parity and acceptance criteria

Every port must preserve the intended economics and class flow; changes to
teaching content require a separate decision. Before a route can replace a
Shiny default it must meet all of these:

- A stable phone URL and projected-display URL, where the app has two views.
- No reload or loss of typed input during normal live updates.
- Keyboard-usable controls, readable contrast, and responsive phone layout.
- An export before clearing intentionally ephemeral state.
- Unit fixtures matching the relevant R calculations or state transitions.
- A browser test with at least two simultaneous participants for shared rooms.
- One actual class use with the Shiny route still available as fallback.

## Explicit non-goals

- Replacing `class-job-market`, `price-index`, `coordination-games`,
  `class-job-picker`, or their Google/SQLite/OAuth integration.
- Moving grades, roster data, tokens, or student identities into a second
  datastore.
- Adding a cross-service sync merely to reduce RAM.
- Removing any existing Shiny app during this branch's pilot period.

## Immediate next actions

1. Confirm the Cloudflare account/project and preview hostname to use for this
   isolated service.
2. Scaffold Phase 0 and port `tax-incidence` as the first parity-tested static
   page.
3. Classroom-test that preview alongside the current Shiny route before
   starting the shared-room migration.
