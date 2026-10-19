#!/usr/bin/env bash
# Independent end-to-end Viewer validation for one or more real CRB artifacts.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../../.." && pwd -P)"
HERE="$ROOT/tests/benchmarks/viewer/scripts/smoke"
if [ "$#" -lt 1 ]; then
  echo "usage: tests/benchmarks/viewer/scripts/smoke/run.sh <file.crb> [more.crb ...]" >&2
  exit 2
fi

RESULT_DIR="${VIEWER_VALIDATION_RESULT_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark/viewer-smoke/$(date -u +%Y%m%dT%H%M%SZ)}"
# Reserve a fresh directory before installing or writing any logs.
mkdir -p -- "$(dirname "$RESULT_DIR")"
mkdir -- "$RESULT_DIR" || {
  echo "choose a new result directory; previous smoke results are preserved: $RESULT_DIR" >&2
  exit 1
}
WORK="$(mktemp -d "${TMPDIR:-/tmp}/cerebro-viewer-validation.XXXXXX")"
cleanup() { rm -rf -- "$WORK"; }
trap cleanup EXIT INT TERM
mkdir -p "$WORK/rlib"

export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null
export NOT_CRAN=true
export VIEWER_VALIDATION_LIBRARY="$WORK/rlib"

echo "==> installing the current checkout"
R CMD INSTALL --no-docs --no-byte-compile --library="$VIEWER_VALIDATION_LIBRARY" "$ROOT" \
  > "$RESULT_DIR/install.log" 2>&1 || {
  tail -n 30 "$RESULT_DIR/install.log" >&2
  exit 1
}

echo "==> validating $# Viewer artifact(s)"
Rscript "$HERE/validate.R" "$RESULT_DIR" "$@"
echo "==> report: $RESULT_DIR/summary.md"
