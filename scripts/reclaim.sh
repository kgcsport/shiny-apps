#!/usr/bin/env bash
set -Eeuo pipefail

# Run from the Reclaim checkout (normally ~/shiny-server). GitHub Actions builds
# images; this script only pulls and recreates services on the host.
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"
COMPOSE=(docker compose)
PUBLIC_URL="${PUBLIC_URL:-https://shiny.kylecoombs.com/class-job-market/}"

usage() {
  cat <<'EOF'
Usage: scripts/reclaim.sh <command>

Commands:
  all       Pull the published images and recreate the full stack
  shiny     Pull/recreate Shiny and nginx
  demo-kit  Pull/recreate the login/demo-kit service
  nginx     Recreate nginx after nginx.conf or maintenance.html changes
  status    Show service/container status
  health    Check the public Class Job Market URL
  sync      Run the Cloudflare gradebook sync once
  logs      Show recent Shiny and nginx logs
EOF
}

command="${1:-}"
case "$command" in
  all)
    git pull --ff-only
    "${COMPOSE[@]}" pull shiny demo-kit
    "${COMPOSE[@]}" up -d shiny demo-kit nginx
    ;;
  shiny)
    git pull --ff-only
    "${COMPOSE[@]}" pull shiny
    "${COMPOSE[@]}" up -d shiny nginx
    ;;
  demo-kit)
    git pull --ff-only
    "${COMPOSE[@]}" pull demo-kit
    "${COMPOSE[@]}" up -d demo-kit
    ;;
  nginx)
    git pull --ff-only
    "${COMPOSE[@]}" up -d nginx
    ;;
  status)
    "${COMPOSE[@]}" ps -a
    ;;
  health)
    command -v curl >/dev/null || { echo "curl is required for health checks" >&2; exit 1; }
    curl --fail --silent --show-error --max-time 20 -o /dev/null -w 'HTTP %{http_code} %{url_effective}\n' "$PUBLIC_URL"
    ;;
  sync)
    "${COMPOSE[@]}" exec -T shiny Rscript /srv/shiny-server/scripts/sync-cloudflare-gradebook.R
    ;;
  logs)
    "${COMPOSE[@]}" logs --tail=150 shiny nginx
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac
