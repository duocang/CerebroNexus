#!/usr/bin/env bash
# Local entry point for the same test matrix used by GitHub Actions.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

MODE="${1:-full}"
case "$MODE" in
  air)
    command -v air >/dev/null || { echo "air not found (brew install air)"; exit 2; }
    air format .
    air format --check .
    exit 0
    ;;
  fast) VALIDATION_MODE="tests" ;;
  full) VALIDATION_MODE="full" ;;
  *)
    echo "Usage: scripts/precheck.sh [air|fast|full]" >&2
    exit 2
    ;;
esac

command -v air >/dev/null || { echo "air not found (brew install air)"; exit 2; }
command -v Rscript >/dev/null || { echo "Rscript not found"; exit 2; }

air format .
air format --check .
exec Rscript scripts/run-local-validation.R --mode "$VALIDATION_MODE"
