#!/usr/bin/env bash
# scripts/rtest.sh — run R test tasks inside the running shiny container
#
# Usage:
#   ./scripts/rtest.sh seed          # seed test databases (run once)
#   ./scripts/rtest.sh unit          # run all unit tests
#   ./scripts/rtest.sh reset         # full transactional reset
#   ./scripts/rtest.sh reset quiz    # reset one app's data
#   ./scripts/rtest.sh reset cg06    # scenario: set Alice to 1 token
#   ./scripts/rtest.sh shell         # drop into an R session in the container

set -euo pipefail

SERVICE="${COMPOSE_SERVICE:-shiny}"
TESTS_DIR="/srv/shiny-server/tests"

run() {
  docker compose exec "$SERVICE" Rscript "$@"
}

cmd="${1:-help}"

case "$cmd" in
  seed)
    echo "Seeding test databases..."
    run "$TESTS_DIR/setup/seed_test_db.R"
    ;;

  unit)
    echo "Running unit tests..."
    run "$TESTS_DIR/run-unit-tests.R"
    ;;

  reset)
    target="${2:-}"
    if [[ -z "$target" ]]; then
      echo "Full reset (all transactional data)..."
      run "$TESTS_DIR/setup/reset_test_db.R"
    else
      # Detect whether it's an --app or --scenario argument
      case "$target" in
        cg06|fresh)
          echo "Scenario reset: $target"
          run "$TESTS_DIR/setup/reset_test_db.R" --scenario "$target"
          ;;
        jobmarket|coordination|quiz|priceindex|jobpicker|auction)
          echo "App reset: $target"
          run "$TESTS_DIR/setup/reset_test_db.R" --app "$target"
          ;;
        *)
          echo "Unknown reset target: $target"
          echo "App targets:      jobmarket coordination quiz priceindex jobpicker auction"
          echo "Scenario targets: cg06 fresh"
          exit 1
          ;;
      esac
    fi
    ;;

  shell)
    echo "Opening R session in container (Ctrl+D to exit)..."
    docker compose exec "$SERVICE" R
    ;;

  help|*)
    cat <<'EOF'
Usage: ./scripts/rtest.sh <command> [target]

Commands:
  seed                  Seed test DBs with users, questions, balances, settings
  unit                  Run all testthat unit tests
  reset                 Full reset — wipe transactional data, keep users/settings
  reset <app>           Reset one app:
                          jobmarket | coordination | quiz | priceindex | jobpicker | auction
  reset <scenario>      Scenario setup:
                          cg06   — set Alice to 1 token (precondition for CG-06)
                          fresh  — wipe all ledger rows, restore seed balances
  shell                 Interactive R session inside the container

Test credentials (after seed):
  instructor / admin123   (admin)
  alice / test123         (S01, 5 tokens)
  bob   / test123         (S01, 5 tokens)
  carol / test123         (S01, 3 tokens)
  dan   / test123         (S02, 5 tokens)
  eve   / test123         (S02, 3 tokens)

Override the compose service name:
  COMPOSE_SERVICE=myservice ./scripts/rtest.sh seed
EOF
    ;;
esac
