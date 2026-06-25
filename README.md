Shiny apps I build largely for classroom use.

## Deployment

`docker-compose up --build` (see `docker-compose.yml`, `Dockerfile`,
`Dockerfile.base`) runs every app under one Shiny Server container, sharing
one `data/finalqdata.sqlite` (mounted via the `appdata` volume).

**Nothing is required to get a working deployment.** On first boot with an
empty database, set these two env vars once to create your own admin login,
then unset them — every later boot uses the account already in the DB:

- `BOOTSTRAP_ADMIN_USER`
- `BOOTSTRAP_ADMIN_PASSWORD`
- `BOOTSTRAP_ADMIN_NAME` (optional display name; defaults to the username)

Once you have an admin account, manage the whole roster from the
`flex_pass_actions` app's Admin tab (create/archive/restore users, reset
passwords, grant flex passes) — no further configuration needed.

Everything else is **optional**, for legacy/extra functionality only:

| Env var | Used by | What it enables |
|---|---|---|
| `GOOGLE_SERVICE_ACCOUNT_JSON` / `GOOGLE_APPLICATION_CREDENTIALS` | class-job-picker, coordination-games, flex_pass_actions, price-index, supply-auction-game | Google Drive backups + Sheets sync |
| `FLEX_PASS_SHEET_ID` | coordination-games, flex_pass_actions | Manage the roster via a Google Sheet instead of the in-app admin panel |
| `FLEX_PASS_FOLDER_ID` | coordination-games, flex_pass_actions, price-index | Off-site Drive backup of the shared DB |
| `AUCTION_FOLDER_ID` | supply-auction-game | Off-site Drive backup of the auction DB |
| `CLASS_JOB_SHEET_ID` | class-job-picker | One-off historical import / summary write-back to Sheets |
| `PUB_ECON_FOLDER_ID`, `FINALQ_SHEET_ID`, `CRED_CSV`, `CRED_PATH` | flex_pass_actions | Legacy alternate credential sources |
| `DB_PATH_OVERRIDE` | coordination-games | Override the shared DB path |
| `SHINY_PASSWORD` | bonus-entry, class-job-picker, club-insurance-game | Shared admin password for those apps |
| `CONNECT_CONTENT_DIR` | nearly every app | Where the shared `data/` dir lives (set once for the whole container) |

See `MULTI_TENANT_TODO.md` for the (not yet started) plan to let other
professors run their own classes on one shared deployment.
