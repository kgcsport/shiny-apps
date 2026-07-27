#!/usr/bin/env bash
# scripts/rtest.sh — run R test tasks inside the running shiny container
#
# Usage:
#   ./scripts/rtest.sh seed            # seed production test databases
#   ./scripts/rtest.sh seed-demo       # seed demo sandbox databases (*-demo.sqlite)
#   ./scripts/rtest.sh unit            # run all unit tests
#   ./scripts/rtest.sh reset           # full transactional reset (production)
#   ./scripts/rtest.sh reset quiz      # reset one app's data
#   ./scripts/rtest.sh reset cg06      # scenario: set Alice to 1 token
#   ./scripts/rtest.sh reset-demo      # full reset of demo sandbox
#   ./scripts/rtest.sh reset-demo quiz # reset one app in demo sandbox
#   ./scripts/rtest.sh shell           # drop into an R session in the container
#   ./scripts/rtest.sh shell-demo      # R session in the demo container

set -euo pipefail

SERVICE="${COMPOSE_SERVICE:-shiny}"
DEMO_SERVICE="${COMPOSE_DEMO_SERVICE:-shiny-demo}"
TESTS_DIR="/srv/shiny-server/tests"

run() {
  docker compose exec "$SERVICE" Rscript "$@"
}

run_demo() {
  docker compose exec -e DEMO_MODE=1 "$DEMO_SERVICE" Rscript "$@"
}

reset_target() {
  local svc="$1"; local mode="$2"; shift 2
  local target="${1:-}"
  if [[ -z "$target" ]]; then
    echo "Full reset (all transactional data)..."
    docker compose exec ${mode:+-e DEMO_MODE=1} "$svc" Rscript "$TESTS_DIR/setup/reset_test_db.R"
  else
    case "$target" in
      cg06|fresh)
        echo "Scenario reset: $target"
        docker compose exec ${mode:+-e DEMO_MODE=1} "$svc" Rscript "$TESTS_DIR/setup/reset_test_db.R" --scenario "$target"
        ;;
      jobmarket|coordination|quiz|priceindex|jobpicker|auction)
        echo "App reset: $target"
        docker compose exec ${mode:+-e DEMO_MODE=1} "$svc" Rscript "$TESTS_DIR/setup/reset_test_db.R" --app "$target"
        ;;
      *)
        echo "Unknown reset target: $target"
        echo "App targets:      jobmarket coordination quiz priceindex jobpicker auction"
        echo "Scenario targets: cg06 fresh"
        exit 1
        ;;
    esac
  fi
}

cmd="${1:-help}"

case "$cmd" in
  seed)
    echo "Seeding production databases..."
    run "$TESTS_DIR/setup/seed_test_db.R"
    ;;

  seed-demo)
    echo "Seeding demo sandbox databases (*-demo.sqlite)..."
    run_demo "$TESTS_DIR/setup/seed_test_db.R"
    ;;

  unit)
    echo "Running unit tests..."
    run "$TESTS_DIR/run-unit-tests.R"
    ;;

  reset)
    reset_target "$SERVICE" "" "${2:-}"
    ;;

  reset-demo)
    reset_target "$DEMO_SERVICE" "1" "${2:-}"
    ;;

  shell)
    echo "Opening R session in container (Ctrl+D to exit)..."
    docker compose exec "$SERVICE" R
    ;;

  shell-demo)
    echo "Opening R session in demo container (Ctrl+D to exit)..."
    docker compose exec "$DEMO_SERVICE" R
    ;;

  help|*)
    cat <<'EOF'
Usage: ./scripts/rtest.sh <command> [target]

Commands:
  seed                  Seed production DBs with users, questions, balances, settings
  seed-demo             Seed demo sandbox DBs (*-demo.sqlite) — same data, separate files
  unit                  Run all testthat unit tests
  reset                 Full reset — wipe transactional data, keep users/settings
  reset <app>           Reset one app (production):
                          jobmarket | coordination | quiz | priceindex | jobpicker | auction
  reset <scenario>      Scenario setup (production):
                          cg06   — set Alice to 1 token (precondition for CG-06)
                          fresh  — wipe all ledger rows, restore seed balances
  reset-demo            Full reset of demo sandbox
  reset-demo <app>      Reset one app in demo sandbox
  reset-demo <scenario> Scenario setup in demo sandbox
  shell                 Interactive R session in the production container
  shell-demo            Interactive R session in the demo container

Demo sandbox:
  Start:  docker compose up -d shiny-demo
  URL:    http://your-server:8080   (port set by DEMO_PORT env var, default 8080)
  The quick-login panel is always visible on the demo instance (no ?demo=1 needed).

Test credentials (after seed / seed-demo):
  instructor / admin123   (admin)
  alice / test123         (S01, 5 tokens)
  bob   / test123         (S01, 5 tokens)
  carol / test123         (S01, 3 tokens)
  dan   / test123         (S02, 5 tokens)
  eve   / test123         (S02, 3 tokens)

Override the compose service name:
  COMPOSE_SERVICE=myservice ./scripts/rtest.sh seed
  COMPOSE_DEMO_SERVICE=my-demo ./scripts/rtest.sh seed-demo
EOF
    ;;
esac
