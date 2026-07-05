Shiny apps I build largely for classroom use.

## Deployment

`docker-compose up --build` (see `docker-compose.yml`, `Dockerfile`,
`Dockerfile.base`) runs every app under one Shiny Server container, sharing
one `data/finalqdata.sqlite` (mounted via the `appdata` volume).

**One env var is required**, because each app's own directory under
`/srv/shiny-server/` is root-owned and not writable by the `shiny` user —
without this, apps can't even create their local SQLite file and 500 on
first request:

- `CONNECT_CONTENT_DIR=/srv/shiny-server/appdata` — point every app at the
  writable, volume-mounted `appdata` directory instead of its own
  read-only source folder. Set this once for the whole container.

On first boot with an empty database, also set these two env vars once to
create your own admin login, then unset them — every later boot uses the
account already in the DB:

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

## Tuning worker/session settings

Shiny Server reads worker/session settings at container startup. Change these
in `.env` or `docker-compose.yml`, then restart with
`docker-compose up -d --build`.

- `SHINY_SIMPLE_SCHEDULER` controls sessions per R worker before starting a
  new worker.
- `SHINY_APP_IDLE_TIMEOUT` controls how long an app process stays alive after
  its last session exits.
- `SHINY_APP_SESSION_TIMEOUT` disconnects inactive browser sessions. Set it
  to blank or `0` to omit the setting.

Commented presets live in `docker-compose.yml`:

| Profile | Scheduler | Idle timeout | Session timeout |
|---|---:|---:|---:|
| low memory | 40 | 60 | 600 |
| balanced | 30 | 60 | 900 |
| responsiveness | 15 | 30 | 1200 |

## Diagnostics

Run `scripts/diagnose.sh` from the repo root on the Docker host. It prints
`docker stats --no-stream`, appdata disk use, recent Shiny logs, and SQLite
file sizes. Override paths with `APPDATA=/path/to/appdata` or
`SHINY_LOG_DIR=/path/to/logs` if needed.

See `MULTI_TENANT_TODO.md` for the (not yet started) plan to let other
professors run their own classes on one shared deployment.
