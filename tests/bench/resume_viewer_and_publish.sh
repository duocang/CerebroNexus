#!/usr/bin/env bash

set -euo pipefail

[ "$#" -eq 1 ] || {
  echo "usage: $0 <preserved-scratch>" >&2
  exit 2
}

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export BENCH_ROOT="$REPO/tests/bench"
SCRATCH="$(cd "$1" && pwd -P)"
MARKER="$SCRATCH/.cerebro-benchmark-scratch"
STAGE="$SCRATCH/result"
SCHEDULE_TSV="$SCRATCH/05_schedule.tsv"
VIEWER_CSV="$STAGE/21_viewer.csv"
LOG_DIR="$STAGE/logs"
BENCH_LIB="$SCRATCH/rlib"
MANIFEST="$STAGE/run_manifest.csv"
RESULT_ROOT="${BENCH_RESULT_ROOT:-$REPO/tests/bench/result/publication-full}"

[ -f "$MARKER" ] || {
  echo "refusing unverified scratch directory: $SCRATCH" >&2
  exit 1
}
[ -f "$SCHEDULE_TSV" ] || {
  echo "missing schedule: $SCHEDULE_TSV" >&2
  exit 1
}
[ -f "$MANIFEST" ] || {
  echo "missing run manifest: $MANIFEST" >&2
  exit 1
}

manifest_value() {
  Rscript -e \
    'x<-read.csv(commandArgs(TRUE)[1]); key<-commandArgs(TRUE)[2]; cat(x$value[x$key==key][1])' \
    "$MANIFEST" "$1"
}

export BENCH_SCRATCH="$SCRATCH"
export BENCH_LIB
export BENCH_PROFILE="$(manifest_value profile)"
export BENCH_RUN_ID="$(manifest_value run_id)"
export BENCH_STUDY_ID="$(manifest_value study_id)"
export BENCH_THREADS="${BENCH_THREADS:-1}"
export NOT_CRAN=true
export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null
export R_LIBS_USER="$SCRATCH/r-user-library"
export OMP_NUM_THREADS="$BENCH_THREADS"
export OPENBLAS_NUM_THREADS="$BENCH_THREADS"
export MKL_NUM_THREADS="$BENCH_THREADS"

[ "$BENCH_PROFILE" = "panel_c2" ] || {
  echo "scratch profile is not panel_c2: $BENCH_PROFILE" >&2
  exit 1
}

expected=0
missing=0
while IFS=$'\t' read -r profile source tier comparison export_repeat order_position backend access_repeats; do
  [ "$profile" = "panel_c2" ] || continue
  expected=$((expected + 1))
  tag="${source}_${tier}_${backend}_r${export_repeat}"
  crb="$SCRATCH/export/$tag/bench.crb"
  if [ ! -f "$crb" ]; then
    echo "missing retained Viewer artifact: $crb" >&2
    missing=$((missing + 1))
  fi
done < "$SCHEDULE_TSV"

[ "$expected" -gt 0 ] && [ "$missing" -eq 0 ] || {
  echo "cannot resume Viewer: expected=$expected missing=$missing" >&2
  exit 1
}
echo "==> verified $expected retained CRB artifacts"

mkdir -p "$BENCH_LIB" "$LOG_DIR"
echo "==> installing current branch into $BENCH_LIB"
R CMD INSTALL --no-docs --no-byte-compile --library="$BENCH_LIB" "$REPO" \
  > "$LOG_DIR/resume_viewer_install.log" 2>&1 || {
  tail -n 30 "$LOG_DIR/resume_viewer_install.log" >&2 || true
  exit 1
}

echo "==> running disposable Viewer smoke test"
Rscript "$BENCH_ROOT/src/04_check_webgpu.R"

if [ -f "$VIEWER_CSV" ]; then
  backup="$SCRATCH/21_viewer.failed-before-resume.csv"
  cp -f -- "$VIEWER_CSV" "$backup"
  echo "==> backed up failed Viewer rows to $backup"
fi
rm -f -- "$VIEWER_CSV"

printf 'key,value\noriginal_run_id,%s\noriginal_git_sha,%s\nviewer_resume_git_sha,%s\nviewer_resumed_at,%s\n' \
  "$BENCH_RUN_ID" \
  "$(manifest_value git_sha)" \
  "$(git -C "$REPO" rev-parse HEAD)" \
  "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  > "$STAGE/viewer_resume_manifest.csv"

echo "==> rerunning Viewer validation only"
while IFS=$'\t' read -r profile source tier comparison export_repeat order_position backend access_repeats; do
  [ "$profile" = "panel_c2" ] || continue
  tag="${source}_${tier}_${backend}_r${export_repeat}"
  crb="$SCRATCH/export/$tag/bench.crb"
  query_plan="$SCRATCH/query-plans/${source}_${tier}.rds"
  log="$LOG_DIR/viewer_resume_$tag.log"
  echo "==> [$tag] Viewer interaction validation"
  Rscript "$BENCH_ROOT/src/21_measure_viewer.R" \
    "$source" "$tier" "$backend" "$export_repeat" "$crb" \
    "$VIEWER_CSV" "$query_plan" > "$log" 2>&1
  tail -n 3 "$log" | sed 's/^/    /'
done < "$SCHEDULE_TSV"

echo "==> checking combined backend and resumed Viewer measurements"
Rscript "$BENCH_ROOT/src/30_check_measurements.R" "$STAGE"

echo "==> writing report"
Rscript "$BENCH_ROOT/src/40_write_report.R" "$STAGE" 2>&1 \
  | tee "$LOG_DIR/report.log"

echo "==> drawing publication figures"
Rscript "$BENCH_ROOT/src/41_draw_figures.R" "$STAGE" "$STAGE/figures" \
  > "$LOG_DIR/figures.log" 2>&1 || {
  tail -n 30 "$LOG_DIR/figures.log" >&2 || true
  exit 1
}

echo "==> checking report and figures"
Rscript "$BENCH_ROOT/src/50_check_outputs.R" "$STAGE"

echo "==> publishing immutable result run"
Rscript "$BENCH_ROOT/src/60_publish_results.R" \
  "$STAGE" "$RESULT_ROOT" "$BENCH_RUN_ID"

echo "==> resumed Viewer run complete: $RESULT_ROOT/runs/$BENCH_RUN_ID"
