# Bonus Pot

This app **shares the same SQLite DB + login** as Class Job Market.

## Requirements
The shared DB must contain (from your flex-pass app):
- `users(user_id, display_name, pw_hash, is_admin)`
- `settings(id=1, initial_fp, ...)`
- `ledger(...)`

This app adds:
- `olig_settings`, `olig_rounds`, `olig_submissions`, `olig_payouts`

## Important accounting rules
- Bonus Pot contributions are debited once when the instructor closes the round.
- The multiplied pot is credited on reveal through the shared `token_ledger`.
- The instructor may divide the pot among every enrolled student in the section, all submitters, or only submitters at or above an explicitly selected token threshold. The threshold is ignored in the other two modes.

All credits are rounded to the nearest 0.5 token.

## Pointing to the shared DB
By default it uses the same path as your other app:
- `${CONNECT_CONTENT_DIR}/data/finalqdata.sqlite` (Posit)
- otherwise `./data/finalqdata.sqlite`

You can override with:
- `DB_PATH_OVERRIDE=/path/to/finalqdata.sqlite`

## Deploy in Docker/Shiny Server
Mount the same data volume used by your flex-pass app so both apps see the same DB.
