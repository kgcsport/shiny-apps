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

The Cloudflare problem-set extension bridge additionally requires the following
server-side setting in `.env`:

- `ASSIGNMENT_ADMIN_TOKEN` — the same machine token configured on the assignment
  Worker. It is used only by the Shiny server and is never sent to a browser.
- `ASSIGNMENT_API_ORIGIN` is optional and defaults to the ECON 342 assignment
  Worker. Set it only when pointing the app at another deployment.

In **Settings → Extensions**, map each purchasable item to its Cloudflare
`assignment-id` and choose whether it extends the initial problem-set schedule or
only the self-grading/corrections close. The purchase table shows every student,
persistent user ID, hours, and sync result; failed syncs can be retried without
charging the student again.

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

## Reclaim deployment procedure

GitHub Actions builds and publishes the Docker image. On the Reclaim host, do
not rebuild the image locally. Pull the published image and recreate the
services:

```bash
cd ~/shiny-server
docker compose pull
docker compose up -d
docker compose ps -a
```

Then verify the public app:

```bash
curl -I https://shiny.kylecoombs.com/class-job-market/
```

Use `docker compose logs --tail=200 shiny` for Shiny errors and
`docker compose logs --tail=200 nginx` for proxy errors. Changes to
`docker-compose.yml`, `nginx.conf`, or other bind-mounted host files still
require updating the checkout on the host before `docker compose up -d`; app
source changes are delivered by `docker compose pull`.

### Staging before production

The app image is published with both `:latest` and an immutable SHA tag. Test a
candidate without touching production using the separate staging service:

```bash
cd ~/shiny-server
scripts/staging.sh up ghcr.io/kgcsport/shiny-apps:sha-<commit>
scripts/staging.sh smoke
scripts/staging.sh seed        # optional: populate the separate staging DB
```

Staging listens only on `127.0.0.1:8081` and uses `staging-appdata/`, not the
production database. From your laptop, inspect it through an SSH tunnel:

```bash
ssh -N -L 8081:127.0.0.1:8081 root@YOUR_RECLAIM_HOST
```

Then open `http://127.0.0.1:8081/`. This is intentionally not a public URL;
if you later add a hostname, put it behind Cloudflare Access or equivalent
authentication rather than relying on an unlisted URL.

Only promote the exact image that passed staging:

```bash
scripts/reclaim.sh promote ghcr.io/kgcsport/shiny-apps:sha-<commit>
```

Promotion pins `SHINY_IMAGE` in the host `.env`, saves a timestamped backup,
and recreates Shiny/nginx. `scripts/staging.sh down` removes only the staging
container; its separate database remains on disk.

For recurring host tasks, use the checked-in wrapper instead of retyping the
commands:

```bash
cd ~/shiny-server
scripts/reclaim.sh shiny       # deploy Shiny + nginx
scripts/reclaim.sh demo-kit    # deploy login/demo-kit
scripts/reclaim.sh health      # public HTTP check
scripts/reclaim.sh status      # container status
scripts/reclaim.sh sync        # one manual grade sync
scripts/reclaim.sh logs        # recent Shiny/nginx logs
```

`scripts/reclaim.sh all` pulls and recreates the full stack. It uses
`git pull --ff-only`, never rebuilds images locally, and does not modify `.env`.

## External uptime monitoring

Reclaim’s status page monitors Reclaim infrastructure, not this individual
Shiny app. For app-specific alerts, use an external HTTP monitor such as
Better Stack Uptime:

1. Create an **HTTP** monitor named `Class Job Market`.
2. Monitor `https://shiny.kylecoombs.com/class-job-market/` (include the
   trailing slash so the monitor does not test the redirect endpoint).
3. Expect HTTP status `200` and use an hourly check interval.
4. Add an email alert contact; requiring two failed checks helps avoid alerts
   for a transient request failure.
5. Add a recurring maintenance window from **00:00 to 06:00** in the
   `America/New_York` timezone, every day. Because the provider starts the
   stack at the beginning of that window's end boundary, schedule the host
   start at least 10–15 minutes before 06:00 (or extend the monitor maintenance
   window to 06:15) so the first check is not a cold-start 502.

This suppresses expected overnight alerts while notifying the instructor when
the app fails during teaching hours. **This is an alert-suppression window,
not a server shutdown schedule.** Keep the Reclaim Stop/Start toggles OFF for
the live class app. Stopping the host creates a real outage while the app is
starting, and students must not be asked to use a cache-busting URL. If you
choose to stop it anyway, start it at least 15 minutes before class access and
expect a brief outage at startup; a fixed UTC schedule will also shift by one
hour across daylight-saving time.

Better Stack’s setup and maintenance window documentation is at:

- https://betterstack.com/docs/uptime/monitoring-start/
- https://betterstack.com/docs/uptime/pausing-monitors-and-maintenances/

## Tuning worker/session settings

Shiny Server reads worker/session settings at container startup. Change these
in `.env` or `docker-compose.yml`, then restart with
`docker-compose up -d --build`.
The container renders `shiny-server.conf.template` to `/tmp/shiny-server.conf`
at startup so deployments with read-only `/etc` still work.

- `SHINY_SIMPLE_SCHEDULER` controls sessions per R worker before starting a
  new worker.
- `SHINY_APP_IDLE_TIMEOUT` controls how long an app process stays alive after
  its last session exits.
- `SHINY_APP_SESSION_TIMEOUT` is accepted for compatibility, but this Shiny
  Server build rejects `app_session_timeout`; the entrypoint omits the
  directive and writes a comment instead.

Commented presets live in `docker-compose.yml`:

| Profile | Scheduler | Idle timeout | Session timeout |
|---|---:|---:|---:|
| low memory | 40 | 60 | omitted |
| balanced | 30 | 60 | omitted |
| responsiveness | 15 | 30 | omitted |

## Diagnostics

Run `scripts/diagnose.sh` from the repo root on the Docker host. It prints
`docker stats --no-stream`, appdata disk use, recent Shiny logs, and SQLite
file sizes. Override paths with `APPDATA=/path/to/appdata` or
`SHINY_LOG_DIR=/path/to/logs` if needed.

See `MULTI_TENANT_TODO.md` for the (not yet started) plan to let other
professors run their own classes on one shared deployment.
