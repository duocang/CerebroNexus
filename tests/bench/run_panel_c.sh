#!/usr/bin/env bash

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BENCH_ROOT="$REPO/tests/bench"
PART="${BENCH_PANEL_C_PART:-all}"

run_c1() {
  BENCH_PROFILE=panel_c1 \
    BENCH_RESULT_ROOT="$BENCH_ROOT/result/panel-c1" \
    "$BENCH_ROOT/run_sweep.sh"
}

run_c2() {
  BENCH_PROFILE=panel_c2 \
    BENCH_RESULT_ROOT="$BENCH_ROOT/result/panel-c2" \
    "$BENCH_ROOT/run_sweep.sh"
}

case "$PART" in
  all)
    run_c1
    run_c2
    Rscript "$BENCH_ROOT/src/42_write_panel_c_report.R" "$BENCH_ROOT/result"
    Rscript "$BENCH_ROOT/src/43_draw_panel_c_figure.R" "$BENCH_ROOT/result"
    ;;
  c1)
    run_c1
    ;;
  c2)
    run_c2
    ;;
  *)
    echo "BENCH_PANEL_C_PART must be c1 or c2" >&2
    exit 1
    ;;
esac
