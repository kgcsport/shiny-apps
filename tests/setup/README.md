# Test Database Setup

## Requirements

```r
install.packages(c("DBI", "RSQLite", "bcrypt"))
```

## First-time setup

Run once before any manual testing:

```bash
Rscript tests/setup/seed_test_db.R
```

This creates two SQLite files and populates them with the test participants below.
The script is **idempotent** — re-running it is safe and will not duplicate users.

## Test credentials

| user_id | password | role | section | starting tokens |
|---|---|---|---|---|
| `instructor` | `admin123` | Admin | — | — |
| `alice` | `test123` | Student | S01 | 5.0 |
| `bob` | `test123` | Student | S01 | 5.0 |
| `carol` | `test123` | Student | S01 | 3.0 |
| `dan` | `test123` | Student | S02 | 5.0 |
| `eve` | `test123` | Student | S02 | 3.0 |

## Resetting between test runs

**Full reset** (all transactional data, seed balances preserved):
```bash
Rscript tests/setup/reset_test_db.R
```

**Per-app resets:**
```bash
Rscript tests/setup/reset_test_db.R --app jobmarket      # bids, assignments, non-seed ledger rows
Rscript tests/setup/reset_test_db.R --app coordination   # olig submissions/payouts, coordination ledger rows
Rscript tests/setup/reset_test_db.R --app quiz           # quiz responses, reset to Q1
Rscript tests/setup/reset_test_db.R --app priceindex     # basket items, price records, wave → 1
Rscript tests/setup/reset_test_db.R --app jobpicker      # job_log, reset bag state
Rscript tests/setup/reset_test_db.R --app auction        # accepts, round → 1, price → 20
```

**Scenario-specific:**
```bash
# CG-06 (contribution exceeds balance): Alice must have exactly 1 token
Rscript tests/setup/reset_test_db.R --scenario cg06

# Restore all seed balances from scratch
Rscript tests/setup/reset_test_db.R --scenario fresh
```

## Database locations

On the production Shiny Server:

| Database | Path |
|---|---|
| Shared (all apps) | `/srv/shiny-server/appdata/data/class-job-market.sqlite` |
| Supply auction | `/srv/shiny-server/appdata/data/auction.sqlite` |

On Posit Connect, set `CONNECT_CONTENT_DIR` and the scripts resolve to
`$CONNECT_CONTENT_DIR/data/` automatically.

For local development (no env vars, no Docker path), the scripts write to
`apps/class-job-market/data/class-job-market.sqlite` and
`apps/supply-auction-game/data/auction.sqlite` relative to the repo root.

## What gets seeded

| Table | Content |
|---|---|
| `users` | 1 admin + 5 students with bcrypt-hashed passwords |
| `students` | 5 students (mirrors users for job market roster) |
| `labor_settings` | Token name, wage defaults, reweight schedule |
| `weekly_rounds` | One open round per section (S01 and S02) |
| `job_categories` | Note Taker (×2, $5), Class Summarizer (×1, $4), Question Writer (×2, $3) |
| `job_posts` | 2 posts per category per round |
| `token_ledger` | Starting balances (alice=5, bob=5, carol=3, dan=5, eve=3) |
| `olig_settings` | Round 1, open, PD game, section=S01, pd_scale=0.1 |
| `quiz_questions` | 5 micro/macro economics questions |
| `quiz_state` | Current Q=1, not revealed |
| `app_state` | Wave=1, standard categories/sources |
| `job_state` | Default jobs initialised for S01 and S02 |
| `auction_settings` | Item: "Fee-free driving pass", start_price=20, tick=1, 5 units |
| `auction_state` | Not running, price=20, 5 units remaining |
