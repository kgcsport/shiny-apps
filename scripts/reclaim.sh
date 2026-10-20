#!/usr/bin/env bash
set -Eeuo pipefail

# Run from the Reclaim checkout (normally ~/shiny-server). GitHub Actions builds
# images; this script only pulls and recreates services on the host.
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"
COMPOSE=(docker compose)
# Probe the local origin, not the public Cloudflare hostname. Reclaim's own
# outbound request can be rejected by Cloudflare even when student traffic is
# healthy. The Host header selects the same Nginx virtual host students use.
PUBLIC_URL="${PUBLIC_URL:-http://127.0.0.1/class-job-market/}"
AUTH_URL="${AUTH_URL:-http://127.0.0.1/auth/login}"
PUBLIC_HOST="${PUBLIC_HOST:-shiny.kylecoombs.com}"

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
  promote   Pin production to an immutable Shiny image tag
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
    "${COMPOSE[@]}" up -d demo-kit nginx
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
    curl --fail --silent --show-error --max-time 20 -H "Host: $PUBLIC_HOST" -o /dev/null -w 'HTTP %{http_code} %{url_effective}\n' "$PUBLIC_URL"
    # A working OAuth relay redirects to Google; a 502/503 indicates the
    # otherwise-independent demo-kit service is unavailable.
    curl --fail --silent --show-error --max-time 20 -H "Host: $PUBLIC_HOST" -o /dev/null -w 'HTTP %{http_code} %{url_effective}\n' "$AUTH_URL"
    ;;
  sync)
    "${COMPOSE[@]}" exec -T shiny Rscript /srv/shiny-server/scripts/sync-cloudflare-gradebook.R
    ;;
  logs)
    "${COMPOSE[@]}" logs --tail=150 shiny nginx
    ;;
  promote)
    image="${2:-}"
    [[ "$image" =~ ^ghcr.io/kgcsport/shiny-apps:[A-Za-z0-9._/-]+$ ]] || {
      echo "Usage: scripts/reclaim.sh promote ghcr.io/kgcsport/shiny-apps:<full-commit-sha>" >&2
      exit 2
    }
    [[ -f .env ]] || { echo ".env is required on the Reclaim host" >&2; exit 1; }
    backup=".env.before-promote.$(date +%Y%m%d%H%M%S)"
    cp .env "$backup"
    tmp="$(mktemp)"
    awk -v image="$image" 'BEGIN{found=0} /^SHINY_IMAGE=/{print "SHINY_IMAGE=" image; found=1; next} {print} END{if(!found) print "SHINY_IMAGE=" image}' .env > "$tmp"
    mv "$tmp" .env
    if ! "${COMPOSE[@]}" pull shiny || ! "${COMPOSE[@]}" up -d shiny nginx; then
      cp "$backup" .env
      echo "Promotion failed; restored $backup" >&2
      exit 1
    fi
    echo "Promoted $image (previous .env saved as $backup)."
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac
