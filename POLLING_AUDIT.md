# Polling Audit

Small Reclaim/Docker efficiency audit, July 2026. No broad rewrites were made.

| App | File | Interval | Change made | Rationale |
|---|---|---:|---|---|
| arcade | `apps/arcade/app.R` | 3000ms | Added interval comments | Admin game selector and olig state are classroom-live but not sub-second. |
| arcade | `apps/arcade/app.R` | 6000ms | Added interval comments | Wallet/pledge state can lag slightly to reduce DB reads. |
| excise-tax-game | `apps/excise-tax-game/app.R` | 2500ms | Added interval comment | Lightweight room/game refresh; already above 1000ms. |
| price-index | `apps/price-index/app.R` | 8000ms | Added interval comments | Wave and aggregate prices are not latency-sensitive. |
| bonus-pot | `apps/bonus-pot/app.R` | 1200ms | Added interval comments | Live in-class game state; already above 1000ms. |
| flex_pass_actions | `apps/flex_pass_actions/app.R` | 3500ms | Added interval comment | Live game state/settings; already above 1000ms. |
| flex_pass_actions | `apps/flex_pass_actions/app.R` | 3600000ms | Added interval comment | Hourly check for daily backup. |
| flex_pass_actions | `apps/flex_pass_actions/helpers.R` | caller-supplied | Added helper comment | Cached DB polling helper used by callers. |
| supply-auction-game | `apps/supply-auction-game/app.R` | 500ms -> 1000ms | Raised interval and added comment | Reduced constant tick work; one-second classroom auction ticks are sufficient. |
| supply-auction-game | `apps/supply-auction-game/app.R` | 250ms -> 1000ms | Raised interval and added comment | Reduced live state DB polling; still updates once per second. |
| sloman-trading-game | `apps/sloman-trading-game/app.R` | 1000ms | Added JS interval comment | Browser-only countdown; no DB polling. |
| review-quiz | `apps/review-quiz/app.R` | 2000ms | Added interval comments | Live quiz state/responses; already above 1000ms. |
| review-quiz | `apps/review-quiz/app.R` | 5000ms | Added interval comments | Question bank and pending submissions are admin-ish views. |

Search terms used: `invalidateLater`, `reactivePoll`, `reactiveTimer`, `setInterval`.

## Class Job Market shared change detection (November 2026, branch `feature/shared-polling`)

`apps/class-job-market/app.R` had eleven `reactivePoll` timers per session,
each running its own queries, plus an export page that rebuilt itself every
five seconds. They are replaced by one app-wide loop:

- Once a second, for the whole app, read two SQLite counters that touch no
  table: `PRAGMA data_version` (commits by any other connection or process)
  and `total_changes()` (writes on the app's own connection).
- When they move, tell every session to run its real checks, at most once per
  three seconds. A change during that gap is held, never dropped.
- A session also re-checks at once when it writes, or when something it reads
  in the session changes, such as the open tab.
- Once a minute every session re-checks regardless, for things that change
  with the clock.
- The loop does nothing while no one is connected.
- `SHARED_POLLING=0` restores the independent timers.
- The export page has a Refresh button and no timer.

Measured on one machine, students signed in and left on the bid page, CPU as a
share of one core over 30 seconds:

| Students | Old timers, idle | Shared loop, idle | Old timers, a write every second | Shared loop, a write every second |
|---:|---:|---:|---:|---:|
| 1 | 1.2% | 0.3% | | |
| 10 | 10.1% | 0.4% | | |
| 25 | 22.8% | 0.4% | 24.0% | 24.8% |

A job post added by another process appeared on an open student page in about
one second (0.8 to 1.3 s over three tries); the old job timer ran every eight
seconds. Memory per session is unchanged at roughly 17 MB.

Known properties: any write anywhere wakes every session, so a class that is
writing constantly sees no saving; all sessions re-check in the same instant
after a change; the loop is per R process.

