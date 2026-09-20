#!/usr/bin/env bash
# Independent end-to-end Viewer validation for one or more real CRB artifacts.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
HERE="$ROOT/tests/viewer-validation"
if [ "$#" -lt 1 ]; then
  echo "usage: tests/viewer-validation/run.sh <file.crb> [more.crb ...]" >&2
  exit 2
fi

RESULT_DIR="${VIEWER_VALIDATION_RESULT_DIR:-$HERE/result/$(date -u +%Y%m%dT%H%M%SZ)}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/cerebro-viewer-validation.XXXXXX")"
cleanup() { rm -rf -- "$WORK"; }
trap cleanup EXIT INT TERM
mkdir -p "$RESULT_DIR" "$WORK/rlib"

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
