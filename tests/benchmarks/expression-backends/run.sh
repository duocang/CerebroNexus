#!/usr/bin/env bash
# H5 / BPCells / embedded expression-storage comparison, on Linux or WSL.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
case "${1:-run}" in
  -h|--help|help)
    cat <<'HELP'
Usage: bash tests/benchmarks/expression-backends/run.sh [quick|run|status|resume DIRECTORY]
Compare H5, BPCells and embedded expression storage: construction, size,
startup, memory and materialized gene reads. Uses the project's Nix environment.
quick: 1,000 cells per source, one build and access process per backend.
run (default): full-source measurements, followed by the scale study.
Both modes download and cache the complete source files on first use.
See tests/benchmarks/expression-backends/README.md before interpreting historical records.
HELP
    ;;
  *) exec bash "$here/scripts/run.sh" "$@" ;;
esac
