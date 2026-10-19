#!/usr/bin/env bash
# Browser performance and functional validation, on Linux or WSL.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
action="${1:-help}"
if (( $# )); then shift; fi
case "$action" in
  pages|release) exec bash "$here/scripts/run.sh" "$action" "$@" ;;
  smoke) exec bash "$here/scripts/smoke/run.sh" "$@" ;;
  -h|--help|help)
    cat <<'HELP'
Usage: bash tests/benchmarks/viewer/run.sh COMMAND [OPTIONS]
  pages --artifact FILE    Compare page performance at Git revisions
  release --artifact FILE  Compare each revision's own Viewer Pack
  smoke FILE.crb [...]     Check Viewer functionality
Use pages --help or release --help for comparison options.
Run inside the project's Linux R environment; see tests/benchmarks/viewer/README.md.
HELP
    ;;
  *) printf 'Unknown Viewer workflow: %s (use --help)\n' "$action" >&2; exit 2 ;;
esac
