#!/usr/bin/env bash

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
export BENCH_ROOT="$REPO/tests/bench"
export BENCH_THREADS="${BENCH_THREADS:-1}"
export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null
RESULT_ROOT="${BENCH_RESULT_ROOT:-$BENCH_ROOT/result/publication-full}"
STUDY_WORK_ROOT="${BENCH_STUDY_WORK_ROOT:-$BENCH_ROOT/study-work}"

if [ -z "${BENCH_SOURCE_CACHE:-}" ]; then
  echo "BENCH_SOURCE_CACHE is required for the publication-full study" >&2
  exit 1
fi
if [ -z "${BENCH_STORAGE_DESCRIPTION:-}" ]; then
  echo "BENCH_STORAGE_DESCRIPTION is required for the publication-full study" >&2
  exit 1
fi
if [ -n "${BENCH_SOURCES_ONLY:-}${BENCH_SOURCES_EXTRA:-}" ]; then
  echo "publication-full owns the exact two-source design; source overrides are not allowed" >&2
  exit 1
fi
mkdir -p "$BENCH_SOURCE_CACHE"
source_cache_path=$(cd "$BENCH_SOURCE_CACHE" && pwd -P)
case "$source_cache_path/" in
  "$REPO/"*)
    echo "BENCH_SOURCE_CACHE must be outside the Git checkout" >&2
    exit 1
    ;;
esac

head_sha=$(git -C "$REPO" rev-parse HEAD)
export BENCH_STUDY_ID="${BENCH_STUDY_ID:-$(date -u +%Y%m%dT%H%M%SZ)-${head_sha:0:12}-publication-full}"
if [[ ! "$BENCH_STUDY_ID" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]]; then
  echo "unsafe BENCH_STUDY_ID: $BENCH_STUDY_ID" >&2
  exit 1
fi

tracked=$(git -C "$REPO" status --porcelain --untracked-files=no)
untracked=$(git -C "$REPO" ls-files --others --exclude-standard -- \
  . ":(exclude,glob)tests/bench/result/**" \
  ":(exclude,glob)tests/bench/study-work/**")
if [ -n "$tracked$untracked" ]; then
  echo "publication-full requires a clean Git worktree" >&2
  exit 1
fi

WORK="$STUDY_WORK_ROOT/$BENCH_STUDY_ID"
PHASES="$WORK/phases"
OUTPUT="$WORK/output"
OUTPUT_MARKER="$WORK/.publication-output-complete"
export R_LIBS_USER="$WORK/report-r-user-library"
mkdir -p "$PHASES"
if [ ! -e "$WORK/.cerebro-publication-study" ]; then
  : > "$WORK/.cerebro-publication-study"
fi

run_phase() {
  local name=$1
  local profile=$2
  local phase_root="$PHASES/$name"
  local run_id="$BENCH_STUDY_ID-$name"
  local current=""
  if [ -f "$phase_root/CURRENT" ]; then
    current=$(< "$phase_root/CURRENT")
  fi
  if [ "$current" = "$run_id" ] && [ -d "$phase_root/runs/$run_id" ]; then
    echo "==> reusing completed $name phase: $run_id"
    return
  fi
  if [ -n "$current" ]; then
    echo "unexpected $name phase CURRENT: $current" >&2
    exit 1
  fi
  echo "==> running publication-full phase $name"
  BENCH_PROFILE="$profile" \
    BENCH_RUN_ID="$run_id" \
    BENCH_RESULT_ROOT="$phase_root" \
    "$BENCH_ROOT/run_sweep.sh"
}

run_phase ab publication
run_phase c1 panel_c1
run_phase c2 panel_c2

completed_output_sha=""
if [ -f "$OUTPUT_MARKER" ]; then
  completed_output_sha=$(< "$OUTPUT_MARKER")
fi
if [ -d "$OUTPUT" ] && [ "$completed_output_sha" = "$head_sha" ]; then
  echo "==> reusing complete derived output"
else
  if [ -e "$OUTPUT" ]; then
    if [ -f "$WORK/.cerebro-publication-study" ] && \
      [[ "$(basename "$WORK")" == "$BENCH_STUDY_ID" ]]; then
      echo "==> rebuilding incomplete derived output from frozen phases"
      rm -rf -- "$OUTPUT"
    else
      echo "refusing to replace unverified study output: $OUTPUT" >&2
      exit 1
    fi
  fi
  mkdir -p "$OUTPUT/phases"
  for phase in ab c1 c2; do
    run_id=$(< "$PHASES/$phase/CURRENT")
    mkdir "$OUTPUT/phases/$phase"
    cp -R "$PHASES/$phase/runs/$run_id/." "$OUTPUT/phases/$phase/"
  done

  Rscript "$BENCH_ROOT/src/42_write_panel_c_report.R" \
    "$OUTPUT/phases" "$OUTPUT"
  Rscript "$BENCH_ROOT/src/43_draw_panel_c_figure.R" \
    "$OUTPUT/phases" "$OUTPUT"

fi

for required in \
  study_manifest.csv environment_comparison.csv source_provenance.csv \
  query_plan_metrics.csv query_panel.csv combined_metrics.csv backend_ratios.csv \
  correctness.csv summary.md \
  figures/expression_backend_benchmark_publication_full.png; do
  test -s "$OUTPUT/$required"
done
printf '%s\n' "$head_sha" > "$OUTPUT_MARKER"

Rscript "$BENCH_ROOT/src/60_publish_results.R" \
  "$OUTPUT" "$RESULT_ROOT" "$BENCH_STUDY_ID"

if [ "${BENCH_KEEP_STUDY_WORK:-0}" != "1" ] && \
  [ -f "$WORK/.cerebro-publication-study" ] && \
  [[ "$(basename "$WORK")" == "$BENCH_STUDY_ID" ]]; then
  rm -rf -- "$WORK"
fi
echo "==> published: $RESULT_ROOT/runs/$BENCH_STUDY_ID"
