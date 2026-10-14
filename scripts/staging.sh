#!/usr/bin/env bash
set -Eeuo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"
PORT="${STAGING_PORT:-8081}"
IMAGE_DEFAULT="ghcr.io/kgcsport/shiny-apps:latest"
COMPOSE=(docker compose -f docker-compose.yml -f docker-compose.staging.yml)

usage() {
  cat <<'USAGE'
Usage: scripts/staging.sh <command> [image-tag]

Commands:
  up [tag]   Pull and start staging using an immutable image tag
  smoke      Run container health, HTTP, and app-parse checks
  status     Show staging container status
  logs       Show recent staging logs
  seed       Seed the separate staging database with test data
  down       Stop/remove the staging service (data volume remains)

Examples:
  scripts/staging.sh up ghcr.io/kgcsport/shiny-apps:sha-<commit>
  scripts/staging.sh smoke
  scripts/staging.sh seed
USAGE
}

image_for() {
  if [[ -n "${1:-}" ]]; then printf '%s' "$1"; else printf '%s' "${SHINY_IMAGE:-$IMAGE_DEFAULT}"; fi
}

wait_healthy() {
  local container status i
  container="$(${COMPOSE[@]} ps -q shiny-staging)"
  if [[ -z "$container" ]]; then echo "staging container was not created" >&2; return 1; fi
  for i in $(seq 1 40); do
    status="$(docker inspect -f '{{if .State.Health}}{{.State.Health.Status}}{{else}}{{.State.Status}}{{end}}' "$container" 2>/dev/null || true)"
    case "$status" in
      healthy) return 0 ;;
      unhealthy|exited|dead) docker compose -f docker-compose.yml -f docker-compose.staging.yml logs --tail=120 shiny-staging >&2; return 1 ;;
    esac
    sleep 3
  done
  echo "staging did not become healthy within 120 seconds" >&2
  docker compose -f docker-compose.yml -f docker-compose.staging.yml logs --tail=120 shiny-staging >&2 || true
  return 1
}

case "${1:-}" in
  up)
    image="$(image_for "${2:-}")"
    [[ "$image" =~ ^[A-Za-z0-9._:/-]+$ ]] || { echo "invalid image tag" >&2; exit 2; }
    SHINY_IMAGE="$image" "${COMPOSE[@]}" pull shiny-staging
    SHINY_IMAGE="$image" "${COMPOSE[@]}" up -d shiny-staging
    wait_healthy
    echo "staging is running on http://127.0.0.1:${PORT}/"
    ;;
  smoke)
    command -v curl >/dev/null || { echo "curl is required" >&2; exit 1; }
    curl --fail --silent --show-error --max-time 20 -o /dev/null -w "HTTP %{http_code} http://127.0.0.1:${PORT}/\n" "http://127.0.0.1:${PORT}/"
    "${COMPOSE[@]}" exec -T shiny-staging Rscript --vanilla -e 'parse(file="/srv/shiny-server/class-job-market/app.R"); cat("staging app parse OK\n")'
    ;;
  status) "${COMPOSE[@]}" ps -a ;;
  logs) "${COMPOSE[@]}" logs --tail=160 shiny-staging ;;
  seed) "${COMPOSE[@]}" exec -T shiny-staging Rscript /srv/shiny-server/tests/setup/seed_test_db.R ;;
  down) "${COMPOSE[@]}" rm -sf shiny-staging ;;
  *) usage >&2; exit 2 ;;
esac
