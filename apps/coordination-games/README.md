# Oligopoly Game (Integrated)

This app **shares the same SQLite DB + login** as your flex-pass app.

## Requirements
The shared DB must contain (from your flex-pass app):
- `users(user_id, display_name, pw_hash, is_admin)`
- `settings(id=1, initial_fp, ...)`
- `ledger(...)`

This app adds:
- `olig_settings`, `olig_rounds`, `olig_submissions`, `olig_payouts`

## Important accounting rules
- Bonus Pot contributions are **debited immediately** to the ledger as `purpose='oligopoly_contrib'`.
- On reveal, each student gets credited their share via `purpose='grant'` with a **negative** amount.
- PD payouts are credited via `purpose='grant'` as well (no debits).

All credits are **rounded to nearest 0.5**.

## Pointing to the shared DB
By default it uses the same path as your other app:
- `${CONNECT_CONTENT_DIR}/data/finalqdata.sqlite` (Posit)
- otherwise `./data/finalqdata.sqlite`

You can override with:
- `DB_PATH_OVERRIDE=/path/to/finalqdata.sqlite`

## Deploy in Docker/Shiny Server
Mount the same data volume used by your flex-pass app so both apps see the same DB.
