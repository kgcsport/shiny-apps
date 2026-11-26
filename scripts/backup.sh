#!/usr/bin/env bash
# Nightly copy of the class database, kept on the server and optionally sent
# off the server.
#
#   scripts/backup.sh            make a backup now
#   scripts/backup.sh restore-test   check the newest backup opens and count its rows
#
# Run it from cron on the server, for example at 2:15 every night:
#   15 2 * * * cd /path/to/shiny-apps && scripts/backup.sh >> shiny-logs/backup.log 2>&1
#
# Off-server copy: install rclone, run `rclone config` once to connect a Google
# Drive or Dropbox account, then set in .env the remote and folder to copy to:
#   BACKUP_REMOTE=gdrive:class-job-market-backups
# Without BACKUP_REMOTE the backup stays on the server only, which does not
# protect against losing the server.
#
# The copy is made by the database itself (VACUUM INTO), so it is consistent
# even while the app is in use. Backups contain student records: keep the
# destination private.
set -Eeuo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"
[[ -f .env ]] && { set -a; . ./.env; set +a; }
KEEP_DAYS="${BACKUP_KEEP_DAYS:-30}"
DEST="appdata/backups"
IN_CONTAINER="/srv/shiny-server/appdata"
COMPOSE=(docker compose)
export COMPOSE_IGNORE_ORPHANS=True
mkdir -p "$DEST"

run_r() { "${COMPOSE[@]}" exec -T shiny Rscript --vanilla -e "$1"; }

case "${1:-backup}" in
  backup)
    stamp="$(date +%Y%m%d-%H%M%S)"
    for db in class-job-market class-job-market-demo; do
      [[ -f "appdata/data/$db.sqlite" ]] || continue
      out="$IN_CONTAINER/backups/$db-$stamp.sqlite"
      run_r "con <- DBI::dbConnect(RSQLite::SQLite(), '$IN_CONTAINER/data/$db.sqlite'); DBI::dbExecute(con, \"VACUUM INTO '$out'\"); ok <- DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), '$out'), 'PRAGMA integrity_check')[[1]]; cat('$db', ok, '\n'); if (!identical(ok, 'ok')) quit(status = 1)"
      gzip -f "$DEST/$db-$stamp.sqlite"
      echo "$(date -Is) saved $DEST/$db-$stamp.sqlite.gz"
    done
    find "$DEST" -name '*.sqlite.gz' -mtime +"$KEEP_DAYS" -delete
    if [[ -n "${BACKUP_REMOTE:-}" ]]; then
      command -v rclone >/dev/null || { echo "BACKUP_REMOTE is set but rclone is not installed" >&2; exit 1; }
      rclone copy "$DEST" "$BACKUP_REMOTE" --include '*.sqlite.gz' --max-age "${KEEP_DAYS}d"
      echo "$(date -Is) copied to $BACKUP_REMOTE"
    else
      echo "$(date -Is) BACKUP_REMOTE is not set: this backup is on the server only" >&2
    fi
    ;;
  restore-test)
    newest="$(ls -t "$DEST"/class-job-market-2*.sqlite.gz 2>/dev/null | head -1 || true)"
    [[ -n "$newest" ]] || { echo "No backup found in $DEST" >&2; exit 1; }
    gunzip -c "$newest" > "$DEST/restore-test.sqlite"
    run_r "con <- DBI::dbConnect(RSQLite::SQLite(), '$IN_CONTAINER/backups/restore-test.sqlite'); cat('integrity:', DBI::dbGetQuery(con, 'PRAGMA integrity_check')[[1]], '\n'); for (t in c('users','weekly_rounds','job_wage_bids','job_assignments','token_ledger','live_score_events')) cat(sprintf('%-20s %d rows\n', t, DBI::dbGetQuery(con, paste('SELECT COUNT(*) n FROM', t))\$n))"
    rm -f "$DEST/restore-test.sqlite"
    echo "Checked $newest"
    ;;
  *) echo "Usage: scripts/backup.sh [backup|restore-test]" >&2; exit 2 ;;
esac
