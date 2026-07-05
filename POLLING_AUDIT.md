# Polling Audit

Small Reclaim/Docker efficiency audit, July 2026. No broad rewrites were made.

| App | File | Interval | Change made | Rationale |
|---|---|---:|---|---|
| arcade | `apps/arcade/app.R` | 3000ms | Added interval comments | Admin game selector and olig state are classroom-live but not sub-second. |
| arcade | `apps/arcade/app.R` | 6000ms | Added interval comments | Wallet/pledge state can lag slightly to reduce DB reads. |
| excise-tax-game | `apps/excise-tax-game/app.R` | 2500ms | Added interval comment | Lightweight room/game refresh; already above 1000ms. |
| price-index | `apps/price-index/app.R` | 8000ms | Added interval comments | Wave and aggregate prices are not latency-sensitive. |
| coordination-games | `apps/coordination-games/app.R` | 1200ms | Added interval comments | Live in-class game state; already above 1000ms. |
| flex_pass_actions | `apps/flex_pass_actions/app.R` | 3500ms | Added interval comment | Live game state/settings; already above 1000ms. |
| flex_pass_actions | `apps/flex_pass_actions/app.R` | 3600000ms | Added interval comment | Hourly check for daily backup. |
| flex_pass_actions | `apps/flex_pass_actions/helpers.R` | caller-supplied | Added helper comment | Cached DB polling helper used by callers. |
| supply-auction-game | `apps/supply-auction-game/app.R` | 500ms -> 1000ms | Raised interval and added comment | Reduced constant tick work; one-second classroom auction ticks are sufficient. |
| supply-auction-game | `apps/supply-auction-game/app.R` | 250ms -> 1000ms | Raised interval and added comment | Reduced live state DB polling; still updates once per second. |
| sloman-trading-game | `apps/sloman-trading-game/app.R` | 1000ms | Added JS interval comment | Browser-only countdown; no DB polling. |
| review-quiz | `apps/review-quiz/app.R` | 2000ms | Added interval comments | Live quiz state/responses; already above 1000ms. |
| review-quiz | `apps/review-quiz/app.R` | 5000ms | Added interval comments | Question bank and pending submissions are admin-ish views. |

Search terms used: `invalidateLater`, `reactivePoll`, `reactiveTimer`, `setInterval`.
