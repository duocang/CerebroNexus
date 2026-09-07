#!/usr/bin/env bash
#
# Repeated, correctness-checked expression-backend benchmark on real data.
#
#   BENCH_PROFILE=quick tests/bench/run_sweep.sh
#   BENCH_PROFILE=standard tests/bench/run_sweep.sh
#   BENCH_PROFILE=publication tests/bench/run_sweep.sh
#   BENCH_KEEP=1 tests/bench/run_sweep.sh
#
# Every run is staged under a uniquely created scratch directory. Validated
# result files are published as an immutable run and CURRENT is changed last,
# so an interrupted run never deletes or replaces the previous evidence.

set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export BENCH_ROOT="$REPO/tests/bench"
RESULT_ROOT="${BENCH_RESULT_ROOT:-$BENCH_ROOT/result}"
export BENCH_PROFILE="${BENCH_PROFILE:-quick}"
export BENCH_THREADS="${BENCH_THREADS:-1}"

case "$BENCH_THREADS" in
  ''|*[!0-9]*|0)
    echo "BENCH_THREADS must be a positive integer" >&2
    exit 1
    ;;
esac
export OMP_NUM_THREADS="$BENCH_THREADS"
export OPENBLAS_NUM_THREADS="$BENCH_THREADS"
export MKL_NUM_THREADS="$BENCH_THREADS"
export VECLIB_MAXIMUM_THREADS="$BENCH_THREADS"
export BLIS_NUM_THREADS="$BENCH_THREADS"
export RCPP_PARALLEL_NUM_THREADS="$BENCH_THREADS"

source "$BENCH_ROOT/lib/source_cache.sh"

SCRATCH_PARENT="${BENCH_SCRATCH_PARENT:-${TMPDIR:-/tmp}}"
mkdir -p "$SCRATCH_PARENT"
SCRATCH="$(mktemp -d "$SCRATCH_PARENT/cerebro-bench.XXXXXX")" || exit 1
SCRATCH_MARKER="$SCRATCH/.cerebro-benchmark-scratch"
: > "$SCRATCH_MARKER"
export BENCH_LIB="$SCRATCH/rlib"
# Publication evidence must not load packages or startup hooks from the caller's
# personal R installation. Nix-provided site libraries remain available.
export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null
export R_LIBS_USER="$SCRATCH/r-user-library"
export BENCH_RUN_ID="${BENCH_RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)-$(git -C "$REPO" rev-parse --short=12 HEAD)-$BENCH_PROFILE}"

STAGE="$SCRATCH/result"
LOG_DIR="$STAGE/logs"
SCHEDULE="$STAGE/05_schedule.csv"
SCHEDULE_TSV="$SCRATCH/05_schedule.tsv"
EXPORT_CSV="$STAGE/10_export.csv"
ACCESS_CSV="$STAGE/20_access.csv"
CRASH_CSV="$STAGE/crashes.csv"
SOURCE_MANIFEST="$STAGE/source_manifest.csv"

cleanup() {
  local code=$?
  trap - EXIT INT TERM
  if [ "${BENCH_KEEP:-0}" = "1" ]; then
    echo "==> keeping scratch at $SCRATCH (BENCH_KEEP=1)"
  elif [ -f "$SCRATCH_MARKER" ] && [[ "$(basename "$SCRATCH")" == cerebro-bench.* ]]; then
    echo "==> removing scratch $SCRATCH"
    rm -rf -- "$SCRATCH"
  else
    echo "!! refusing to remove unverified scratch path: $SCRATCH" >&2
    code=1
  fi
  exit "$code"
}
trap cleanup EXIT INT TERM

mkdir -p "$STAGE" "$LOG_DIR" "$SCRATCH/sources" "$SCRATCH/query-plans" \
  "$BENCH_LIB" "$R_LIBS_USER"
printf '%s\n' 'run_id,profile,source,n_cells,backend,export_repeat,order_position,stage,exit_code' > "$CRASH_CSV"
printf '%s\n' 'run_id,source,url,bytes,sha256' > "$SOURCE_MANIFEST"

echo "==> run:      $BENCH_RUN_ID"
echo "==> profile:  $BENCH_PROFILE"
echo "==> scratch:  $SCRATCH"
echo "==> results:  $RESULT_ROOT"
df -h "$SCRATCH" | tail -1

echo "==> inspecting source dimensions (metadata only)"
Rscript "$BENCH_ROOT/src/01_inspect_data.R" "$STAGE/00_probe.csv" 2>&1 \
  | tee "$LOG_DIR/probe.log" || exit 1
Rscript "$BENCH_ROOT/src/02_record_environment.R" "$STAGE/run_manifest.csv" || exit 1
Rscript "$BENCH_ROOT/src/03_plan_runs.R" "$SCHEDULE" "$SCHEDULE_TSV" || exit 1

RESOURCE_SCRIPT="04_check_resources.R"
BUILD_SCRIPT="10_export_backend.R"
if [ "$BENCH_PROFILE" = "panel_c2" ]; then
  RESOURCE_SCRIPT="04_check_full_resources.R"
  BUILD_SCRIPT="11_build_full_backend.R"
fi

echo "==> checking whether this machine can run the plan"
Rscript "$BENCH_ROOT/src/$RESOURCE_SCRIPT" \
  "$STAGE/00_probe.csv" "$SCHEDULE" "$STAGE/run_manifest.csv" \
  "$STAGE/resource_check.csv" || exit 1

echo "==> installing branch under test into $BENCH_LIB"
R CMD INSTALL --no-docs --no-byte-compile --library="$BENCH_LIB" "$REPO" \
  > "$LOG_DIR/install.log" 2>&1 || {
  echo "!! install failed, see $LOG_DIR/install.log"
  tail -20 "$LOG_DIR/install.log"
  exit 1
}

SOURCES=$(Rscript -e 'source(file.path(Sys.getenv("BENCH_ROOT"), "config", "sources.R")); cat(bench_active_sources(), sep="\n")')

for src in $SOURCES; do
  url=$(Rscript -e "source(file.path(Sys.getenv('BENCH_ROOT'), 'config', 'sources.R')); cat(BENCH_SOURCES[['$src']]\$url)")
  expected_bytes=$(Rscript -e "source(file.path(Sys.getenv('BENCH_ROOT'), 'config', 'sources.R')); cat(BENCH_SOURCES[['$src']]\$expected_bytes)")

  echo "==> [$src] fetching $(basename "${url%%\?*}")"
  if ! bench_fetch_source "$url" "$expected_bytes" "$SCRATCH/sources"; then
    echo "!! download failed for $src; validation will preserve the previous run"
    continue
  fi
  file=$BENCH_FETCHED_FILE
  bytes=$BENCH_FETCHED_BYTES
  sha256=$BENCH_FETCHED_SHA256
  printf '"%s","%s","%s",%s,"%s"\n' \
    "$BENCH_RUN_ID" "$src" "$url" "$bytes" "$sha256" >> "$SOURCE_MANIFEST"
  echo "    local copy: $bytes bytes, sha256 ${sha256:0:12}..."

  while IFS=$'\t' read -r profile row_source tier comparison export_repeat order_position backend access_repeats; do
    [ "$row_source" = "$src" ] || continue
    tag="${src}_${tier}_${backend}_r${export_repeat}"
    query_plan="$SCRATCH/query-plans/${src}_${tier}.rds"
    out_dir="$SCRATCH/export/$tag"
    crb="$out_dir/bench.crb"

    echo "==> [$tag] build (position $order_position)"
    Rscript "$BENCH_ROOT/src/$BUILD_SCRIPT" \
      "$src" "$tier" "$backend" "$export_repeat" "$order_position" \
      "$SCRATCH" "$EXPORT_CSV" "$query_plan" \
      > "$LOG_DIR/export_$tag.log" 2>&1
    rc=$?
    tail -3 "$LOG_DIR/export_$tag.log" | sed 's/^/    /'
    if [ "$rc" -ne 0 ]; then
      echo "    !! export process died (exit $rc)"
      printf '"%s","%s","%s",%s,"%s",%s,%s,"export",%s\n' \
        "$BENCH_RUN_ID" "$BENCH_PROFILE" "$src" "$tier" "$backend" \
        "$export_repeat" "$order_position" "$rc" >> "$CRASH_CSV"
      rm -rf -- "$out_dir"
      continue
    fi

    if [ -f "$crb" ]; then
      for access_repeat in $(seq 1 "$access_repeats"); do
        echo "==> [$tag] access repeat $access_repeat/$access_repeats"
        Rscript "$BENCH_ROOT/src/20_measure_backend.R" \
          "$src" "$tier" "$backend" "$export_repeat" "$order_position" \
          "$access_repeat" "$crb" "$ACCESS_CSV" "$query_plan" \
          > "$LOG_DIR/access_${tag}_a${access_repeat}.log" 2>&1
        rc=$?
        tail -2 "$LOG_DIR/access_${tag}_a${access_repeat}.log" | sed 's/^/    /'
        if [ "$rc" -ne 0 ]; then
          echo "    !! access process died (exit $rc)"
          printf '"%s","%s","%s",%s,"%s",%s,%s,"access-%s",%s\n' \
            "$BENCH_RUN_ID" "$BENCH_PROFILE" "$src" "$tier" "$backend" \
            "$export_repeat" "$order_position" "$access_repeat" "$rc" >> "$CRASH_CSV"
        fi
      done
    fi
    rm -rf -- "$out_dir"
  done < "$SCHEDULE_TSV"

  rm -f -- "$file"
done

echo "==> checking measurements"
Rscript "$BENCH_ROOT/src/30_check_measurements.R" "$STAGE" || exit 1

echo "==> writing report"
Rscript "$BENCH_ROOT/src/40_write_report.R" "$STAGE" 2>&1 \
  | tee "$LOG_DIR/report.log" || exit 1

if [ "$BENCH_PROFILE" = "publication" ]; then
  echo "==> drawing publication figures"
  Rscript "$BENCH_ROOT/src/41_draw_figures.R" "$STAGE" "$STAGE/figures" \
    > "$LOG_DIR/figures.log" 2>&1 || {
    tail -20 "$LOG_DIR/figures.log"
    exit 1
  }
fi

echo "==> checking report and figures"
Rscript "$BENCH_ROOT/src/50_check_outputs.R" "$STAGE" || exit 1

echo "==> publishing immutable result run"
Rscript "$BENCH_ROOT/src/60_publish_results.R" "$STAGE" "$RESULT_ROOT" "$BENCH_RUN_ID" || exit 1

echo "==> done: $RESULT_ROOT/runs/$BENCH_RUN_ID"
