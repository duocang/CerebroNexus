#!/usr/bin/env bash

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd -P)"
BENCH_ROOT="$REPO/tests/benchmarks/expression-backends/scripts"
CACHE_ROOT="${BENCH_CACHE_ROOT:-${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark}"
STATE_DIR="$CACHE_ROOT/runner"
PID_FILE="$STATE_DIR/benchmark.pid"
EXIT_FILE="$STATE_DIR/benchmark.exit"
LOG_FILE="$STATE_DIR/benchmark.log"
SCRIPT="$BENCH_ROOT/run.sh"

bench_sha256_file() {
  local path=$1
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$path" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$path" | awk '{print $1}'
  elif command -v openssl >/dev/null 2>&1; then
    openssl dgst -sha256 "$path" | awk '{print $NF}'
  else
    echo "no SHA-256 implementation found" >&2
    return 1
  fi
}

bench_fetch_source() {
  local url=$1
  local expected_bytes=$2
  local scratch_dir=$3
  local expected_sha=${4:-}
  local name
  local file
  local part
  local bytes
  local sha
  local recorded_sha
  local cache_root
  name=$(basename "${url%%\?*}")
  mkdir -p "$scratch_dir"

  if [ -n "${BENCH_SOURCE_CACHE:-}" ]; then
    mkdir -p "$BENCH_SOURCE_CACHE"
    cache_root=$(cd "$BENCH_SOURCE_CACHE" && pwd) || return 1
    file="$cache_root/$name"
    if [ -f "$file" ]; then
      if [ ! -f "$file.sha256" ]; then
        echo "cached source has no SHA-256 sidecar: $file" >&2
        return 1
      fi
      recorded_sha=$(tr -d '[:space:]' < "$file.sha256")
      sha=$(bench_sha256_file "$file") || return 1
      bytes=$(wc -c < "$file" | tr -d '[:space:]')
      if [ "$sha" != "$recorded_sha" ] || [ "$bytes" != "$expected_bytes" ]; then
        echo "cached source failed checksum or size validation: $file" >&2
        return 1
      fi
      if [ -n "$expected_sha" ] && [ "$sha" != "$expected_sha" ]; then
        echo "cached source differs from the pinned SHA-256: $file" >&2
        return 1
      fi
    else
      part="$file.part"
      curl -fL --retry 3 --retry-delay 5 --continue-at - \
        -o "$part" "$url" || return 1
      bytes=$(wc -c < "$part" | tr -d '[:space:]')
      if [ "$bytes" != "$expected_bytes" ]; then
        echo "downloaded source has unexpected size: $bytes != $expected_bytes" >&2
        return 1
      fi
      sha=$(bench_sha256_file "$part") || return 1
      if [ -n "$expected_sha" ] && [ "$sha" != "$expected_sha" ]; then
        echo "downloaded source differs from the pinned SHA-256" >&2
        return 1
      fi
      mv "$part" "$file"
      printf '%s\n' "$sha" > "$file.sha256.tmp"
      mv "$file.sha256.tmp" "$file.sha256"
    fi
    BENCH_FETCHED_FILE="$scratch_dir/$name"
    ln -sfn "$file" "$BENCH_FETCHED_FILE"
  else
    BENCH_FETCHED_FILE="$scratch_dir/$name"
    part="$BENCH_FETCHED_FILE.part"
    curl -fL --retry 3 --retry-delay 5 --continue-at - \
      -o "$part" "$url" || return 1
    bytes=$(wc -c < "$part" | tr -d '[:space:]')
    if [ "$bytes" != "$expected_bytes" ]; then
      echo "downloaded source has unexpected size: $bytes != $expected_bytes" >&2
      return 1
    fi
    sha=$(bench_sha256_file "$part") || return 1
    if [ -n "$expected_sha" ] && [ "$sha" != "$expected_sha" ]; then
      echo "downloaded source differs from the pinned SHA-256" >&2
      return 1
    fi
    mv "$part" "$BENCH_FETCHED_FILE"
  fi

  BENCH_FETCHED_BYTES=$bytes
  BENCH_FETCHED_SHA256=$sha
}

# OOM kills recorded for this process's cgroup-v2 group; empty when unreadable.
bench_oom_kills() {
  local group
  group=$(sed -n 's/^0:://p' /proc/self/cgroup 2>/dev/null) || return 0
  awk '$1 == "oom_kill" { print $2 }' "/sys/fs/cgroup${group}/memory.events" \
    2>/dev/null || true
}

# True only when the OOM-kill count is readable and grew since $1.
bench_oom_killed_since() {
  local after
  after=$(bench_oom_kills)
  [ -n "$1" ] && [ -n "$after" ] && [ "$after" -gt "$1" ]
}

# Shared finalization for normal completion and retrying a completed measurement.
finish_profile() {
  local stage=$1 result_root=$2 run_id=$3
  # Once archived, the run is immutable. Regenerating reports or PDFs (which
  # embed a creation date) would no longer match it, so a retry after a failed
  # activation only lets the publisher re-verify the archive and activate it.
  if [ ! -d "$result_root/runs/$run_id" ]; then
    mkdir -p "$stage/logs"
    Rscript "$BENCH_ROOT/benchmark_cli.R" validate "$stage" || return 1
    Rscript "$BENCH_ROOT/benchmark_cli.R" report "$stage" \
      > "$stage/logs/report.log" 2>&1 || return 1
    if [ "$BENCH_PROFILE" = "scale" ] || [ "$BENCH_PROFILE" = "panel_c2" ]; then
      Rscript "$BENCH_ROOT/benchmark_cli.R" figure "$stage" "$stage/figures" \
        > "$stage/logs/figures.log" 2>&1 || {
        tail -20 "$stage/logs/figures.log"
        return 1
      }
    fi
    Rscript "$BENCH_ROOT/benchmark_cli.R" evidence "$stage" || return 1
    Rscript "$BENCH_ROOT/benchmark_cli.R" check "$stage" || return 1
  fi
  # The publisher archives this run and activates its latest images with CURRENT.
  if ! Rscript "$BENCH_ROOT/benchmark_cli.R" publish "$stage" "$result_root" "$run_id"; then
    # Also restore the old pair if the publisher was killed without running R's
    # exit handlers. Leave the measurements staged for a later retry.
    Rscript "$BENCH_ROOT/benchmark_cli.R" recover-publication "$result_root" || return 1
    return 1
  fi
}

run_profile_engine() (
set +e

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
export BENCH_ROOT="$REPO/tests/benchmarks/expression-backends/scripts"
RESULT_ROOT="${BENCH_RESULT_ROOT:-$REPO/tests/benchmarks/results/expression-backends}"
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

SCRATCH_PARENT="${BENCH_SCRATCH_PARENT:-${TMPDIR:-/tmp}}"
mkdir -p "$SCRATCH_PARENT"
SCRATCH="$(mktemp -d "$SCRATCH_PARENT/cerebro-bench.XXXXXX")" || exit 1
SCRATCH_MARKER="$SCRATCH/.cerebro-benchmark-scratch"
: > "$SCRATCH_MARKER"
export BENCH_SCRATCH="$SCRATCH"
export BENCH_LIB="$SCRATCH/rlib"
# Benchmark evidence must not load packages or startup hooks from the caller's
# personal R installation. Nix-provided site libraries remain available.
export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null
export NOT_CRAN=true
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
QUERY_PLAN_MANIFEST="$STAGE/query_plan_manifest.csv"
QUERY_PANEL="$STAGE/query_panel.csv"

cleanup() {
  local code=$?
  trap - EXIT INT TERM
  if [ "${BENCH_KEEP:-0}" = "1" ]; then
    echo "==> keeping scratch at $SCRATCH (BENCH_KEEP=1)"
  elif [ "$code" -ne 0 ] && [ "${BENCH_KEEP_ON_FAILURE:-1}" = "1" ]; then
    echo "==> keeping failed-run scratch at $SCRATCH (BENCH_KEEP_ON_FAILURE=1)"
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
Rscript "$BENCH_ROOT/benchmark_cli.R" inspect "$STAGE/00_probe.csv" 2>&1 \
  | tee "$LOG_DIR/probe.log" || exit 1
Rscript "$BENCH_ROOT/benchmark_cli.R" environment "$STAGE/run_manifest.csv" || exit 1
Rscript "$BENCH_ROOT/benchmark_cli.R" plan "$SCHEDULE" "$SCHEDULE_TSV" || exit 1

RESOURCE_COMMAND="resources"
BUILD_COMMAND="export"
if [ "$BENCH_PROFILE" = "scale" ] || \
   [ "$BENCH_PROFILE" = "panel_c2" ]; then
  RESOURCE_COMMAND="full-resources"
  BUILD_COMMAND="build-full"
fi

echo "==> checking whether this machine can run the plan"
Rscript "$BENCH_ROOT/benchmark_cli.R" "$RESOURCE_COMMAND" \
  "$STAGE/00_probe.csv" "$SCHEDULE" "$STAGE/run_manifest.csv" \
  "$STAGE/resource_check.csv" || exit 1

echo "==> installing branch under test into $BENCH_LIB"
R CMD INSTALL --no-docs --no-byte-compile --library="$BENCH_LIB" "$REPO" \
  > "$LOG_DIR/install.log" 2>&1 || {
  echo "!! install failed, see $LOG_DIR/install.log"
  tail -20 "$LOG_DIR/install.log"
  exit 1
}

SOURCES=$(Rscript -e 'source(file.path(Sys.getenv("BENCH_ROOT"), "benchmark.R")); cat(bench_active_sources(), sep="\n")')

for src in $SOURCES; do
  url=$(Rscript -e "source(file.path(Sys.getenv('BENCH_ROOT'), 'benchmark.R')); cat(BENCH_SOURCES[['$src']]\$url)")
  expected_bytes=$(Rscript -e "source(file.path(Sys.getenv('BENCH_ROOT'), 'benchmark.R')); cat(BENCH_SOURCES[['$src']]\$expected_bytes)")
  expected_sha=$(Rscript -e "source(file.path(Sys.getenv('BENCH_ROOT'), 'benchmark.R')); cat(BENCH_SOURCES[['$src']]\$expected_sha256)")

  echo "==> [$src] fetching $(basename "${url%%\?*}")"
  if ! bench_fetch_source \
    "$url" "$expected_bytes" "$SCRATCH/sources" "$expected_sha"; then
    echo "!! download failed for $src; validation will preserve the previous run"
    continue
  fi
  file=$BENCH_FETCHED_FILE
  bytes=$BENCH_FETCHED_BYTES
  sha256=$BENCH_FETCHED_SHA256
  printf '"%s","%s","%s",%s,"%s"\n' \
    "$BENCH_RUN_ID" "$src" "$url" "$bytes" "$sha256" >> "$SOURCE_MANIFEST"
  echo "    local copy: $bytes bytes, sha256 ${sha256:0:12}..."

  tiers=$(awk -F '\t' -v source="$src" '$2 == source {print $3}' \
    "$SCHEDULE_TSV" | sort -n -u)
  for tier in $tiers; do
    query_plan="$SCRATCH/query-plans/${src}_${tier}.rds"
    echo "==> [$src / $tier] preparing frozen query plan"
    Rscript "$BENCH_ROOT/benchmark_cli.R" query-plan \
      "$src" "$tier" "$SCRATCH" "$query_plan" "$QUERY_PLAN_MANIFEST" \
      "$QUERY_PANEL" \
      > "$LOG_DIR/query_plan_${src}_${tier}.log" 2>&1 || {
      tail -20 "$LOG_DIR/query_plan_${src}_${tier}.log"
      exit 1
    }
  done

  while IFS=$'\t' read -r profile row_source tier comparison export_repeat order_position backend access_repeats; do
    [ "$row_source" = "$src" ] || continue
    tag="${src}_${tier}_${backend}_r${export_repeat}"
    query_plan="$SCRATCH/query-plans/${src}_${tier}.rds"
    out_dir="$SCRATCH/export/$tag"
    crb="$out_dir/bench.crb"
    build_command="$BUILD_COMMAND"
    missing_ok=0
    if [ "$BENCH_PROFILE" = "scale" ] && \
       [ "$backend" = "embedded" ]; then
      build_command="export"
      missing_ok=1
    fi

    oom_before=""
    if [ "$missing_ok" = "1" ]; then
      oom_before=$(bench_oom_kills)
    fi
    echo "==> [$tag] build (position $order_position)"
    Rscript "$BENCH_ROOT/benchmark_cli.R" "$build_command" \
      "$src" "$tier" "$backend" "$export_repeat" "$order_position" \
      "$SCRATCH" "$EXPORT_CSV" "$query_plan" \
      > "$LOG_DIR/export_$tag.log" 2>&1
    rc=$?
    tail -3 "$LOG_DIR/export_$tag.log" | sed 's/^/    /'
    # A Scale embedded build killed by the memory cap is a resource outcome,
    # not a crash. Only an observed cgroup OOM kill qualifies.
    if [ "$rc" -ne 0 ] && bench_oom_killed_since "$oom_before" && \
       BENCH_EXPORT_OOM_KILLED="killed at the Scale memory cap (cgroup oom_kill, exit $rc)" \
         Rscript "$BENCH_ROOT/benchmark_cli.R" export \
         "$src" "$tier" "$backend" "$export_repeat" "$order_position" \
         "$SCRATCH" "$EXPORT_CSV" "$query_plan" \
         >> "$LOG_DIR/export_$tag.log" 2>&1; then
      echo "    !! embedded build killed at the Scale memory cap; recorded as SKIPPED(resource)"
      if [ "${BENCH_KEEP:-0}" != "1" ]; then
        rm -rf -- "$out_dir"
      fi
      continue
    fi
    if [ "$rc" -ne 0 ]; then
      echo "    !! export process died (exit $rc)"
      printf '"%s","%s","%s",%s,"%s",%s,%s,"export",%s\n' \
        "$BENCH_RUN_ID" "$BENCH_PROFILE" "$src" "$tier" "$backend" \
        "$export_repeat" "$order_position" "$rc" >> "$CRASH_CSV"
      if [ "${BENCH_KEEP:-0}" != "1" ]; then
        rm -rf -- "$out_dir"
      fi
      continue
    fi

    if [ ! -f "$crb" ]; then
      if [ "$missing_ok" = "1" ]; then
        if [ "${BENCH_KEEP:-0}" != "1" ]; then
          rm -rf -- "$out_dir"
        fi
        continue
      fi
      echo "    !! export succeeded but artifact is missing: $crb" >&2
      printf '"%s","%s","%s",%s,"%s",%s,%s,"export-artifact",1\n' \
        "$BENCH_RUN_ID" "$BENCH_PROFILE" "$src" "$tier" "$backend" \
        "$export_repeat" "$order_position" >> "$CRASH_CSV"
      exit 1
    fi

    for access_repeat in $(seq 1 "$access_repeats"); do
      echo "==> [$tag] access repeat $access_repeat/$access_repeats"
      Rscript "$BENCH_ROOT/benchmark_cli.R" access \
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

    if [ "${BENCH_KEEP:-0}" != "1" ]; then
      rm -rf -- "$out_dir"
    fi
  done < "$SCHEDULE_TSV"

  if [ "${BENCH_KEEP:-0}" != "1" ]; then
    rm -f -- "$file"
  fi
done

echo "==> validating, drawing and publishing results"
finish_profile "$STAGE" "$RESULT_ROOT" "$BENCH_RUN_ID" || exit 1

echo "==> done: $RESULT_ROOT/runs/$BENCH_RUN_ID"
)

read_pid() {
  local pid=""
  [ -f "$PID_FILE" ] || return 1
  read -r pid < "$PID_FILE" || true
  case "$pid" in
    ''|*[!0-9]*) return 1 ;;
    *) printf '%s\n' "$pid" ;;
  esac
}

is_running() {
  local pid
  pid=$(read_pid) || return 1
  kill -0 "$pid" 2>/dev/null
}

show_status() {
  local code pid
  if is_running; then
    pid=$(read_pid)
    printf 'benchmark running: PID=%s\n' "$pid"
    ps -p "$pid" -o pid=,etime=,stat=,cmd= || true
  elif [ -f "$EXIT_FILE" ]; then
    read -r code < "$EXIT_FILE" || code="unknown"
    printf 'benchmark finished: exit=%s\n' "$code"
  else
    printf 'benchmark is not running\n'
  fi
  printf 'log: %s\n' "$LOG_FILE"
}

require_clean() {
  local dirty
  dirty=$(git -C "$REPO" status --porcelain --untracked-files=all -- \
    . ":(exclude,glob)tests/benchmarks/results/expression-backends/**")
  [ -z "$dirty" ] || {
    printf '%s\n' "$dirty" >&2
    printf 'benchmark requires a clean Git worktree\n' >&2
    return 1
  }
}

run_profile() {
  local profile="$1"
  local result_name="$2"
  local head_sha study_id memory_snapshot result_root
  require_clean
  head_sha=$(git -C "$REPO" rev-parse HEAD)
  result_root="$(bench_result_root "$result_name" "${BENCH_SCHEDULE_FILTER:-}")"
  study_id="$(date -u +%Y%m%dT%H%M%SZ)-${head_sha:0:12}-$result_name${BENCH_SCHEDULE_FILTER:+-supplement}"
  printf '==> running %s\n' "$result_name"
  if [ "$profile" = "scale" ]; then
    # One launch-time budget for the entire serial study. A cgroup enforces the
    # cap for child R processes as well; never silently run without the limit.
    command -v systemd-run >/dev/null || { echo 'Scale requires systemd-run for its memory cap' >&2; return 1; }
    memory_snapshot=$(Rscript -e 'source(commandArgs(TRUE)[1]); a <- bench_available_memory_bytes(); cat(sprintf("%.0f %.0f", a, bench_scale_memory_budget(a)))' "$BENCH_ROOT/benchmark.R") || return 1
    read -r BENCH_AVAILABLE_MEMORY_BYTES BENCH_MEMORY_LIMIT_BYTES <<< "$memory_snapshot"
    export BENCH_AVAILABLE_MEMORY_BYTES BENCH_MEMORY_LIMIT_BYTES
    printf '==> Scale available RAM=%s bytes; 99%% hard cap=%s bytes; swap=0\n' "$BENCH_AVAILABLE_MEMORY_BYTES" "$BENCH_MEMORY_LIMIT_BYTES"
    BENCH_PROFILE="$profile" BENCH_STUDY_ID="$study_id" BENCH_RUN_ID="$study_id" \
      BENCH_RESULT_ROOT="$result_root" \
      systemd-run --user --scope --quiet \
        -p "MemoryMax=$BENCH_MEMORY_LIMIT_BYTES" -p MemorySwapMax=0 \
        bash "$SCRIPT" _profile
    return $?
  fi
  BENCH_PROFILE="$profile" \
    BENCH_STUDY_ID="$study_id" \
    BENCH_RUN_ID="$study_id" \
    BENCH_RESULT_ROOT="$result_root" \
    "$SCRIPT" _profile
}

run_all() {
  case "${1:-run}" in
    quick) run_profile quick quick ;;
    run|scale) run_profile scale scale ;;
    full) run_profile panel_c2 full ;;
    *) printf 'unknown benchmark mode: %s\n' "$1" >&2; return 2 ;;
  esac
}

# Results directory for a study. A filtered schedule (BENCH_SCHEDULE_FILTER) is a
# supplement: it is published beside the study and never replaces its CURRENT.
bench_result_root() {
  local base="$REPO/tests/benchmarks/results/expression-backends"
  if [ -n "${2:-}" ]; then
    printf '%s\n' "$base/supplements/$1"
  else
    printf '%s\n' "$base/$1"
  fi
}

manifest_value() {
  Rscript -e 'm <- read.csv(commandArgs(TRUE)[1], stringsAsFactors = FALSE); value <- m$value[m$key == commandArgs(TRUE)[2]]; if (length(value) != 1L || is.na(value) || !nzchar(value)) quit(status = 1L); cat(value)' "$1" "$2"
}

resume_inside() {
  local scratch stage manifest result_name
  scratch="${BENCH_RECOVERY_SCRATCH:-}"
  [ -f "$scratch/.cerebro-benchmark-scratch" ] || {
    printf 'unsafe benchmark scratch directory: %s\n' "$scratch" >&2
    return 1
  }
  stage="$scratch/result"
  manifest="$stage/run_manifest.csv"
  [ -f "$manifest" ] || {
    printf 'missing run manifest: %s\n' "$manifest" >&2
    return 1
  }

  export BENCH_ROOT="$REPO/tests/benchmarks/expression-backends/scripts"
  export BENCH_SCRATCH="$scratch"
  export BENCH_LIB="$scratch/rlib"
  export R_LIBS_USER="$scratch/r-user-library"
  export R_ENVIRON_USER=/dev/null
  export R_PROFILE_USER=/dev/null
  export NOT_CRAN=true
  export BENCH_RUN_ID="$(manifest_value "$manifest" run_id)"
  export BENCH_STUDY_ID="$(manifest_value "$manifest" study_id)"
  export BENCH_PROFILE="$(manifest_value "$manifest" profile)"
  case "$BENCH_PROFILE" in
    quick) result_name=quick ;;
    panel_c2) result_name=full ;;
    scale) result_name=scale ;;
    *)
      printf 'unsupported benchmark profile: %s\n' "$BENCH_PROFILE" >&2
      return 1
      ;;
  esac
  local filter result_root
  filter="$(manifest_value "$manifest" schedule_filter 2>/dev/null || true)"
  result_root="$(bench_result_root "$result_name" "$filter")"

  finish_profile "$stage" "$result_root" "$BENCH_RUN_ID"
  printf '0\n' > "$EXIT_FILE"
  printf 'benchmark result published: %s\n' "$result_root/runs/$BENCH_RUN_ID"
}

resume_run() {
  local scratch command
  [ "$#" -eq 1 ] && [ -n "$1" ] || {
    printf 'usage: %s resume <scratch-directory>\n' "$SCRIPT" >&2
    return 2
  }
  command -v nix-shell >/dev/null 2>&1 || {
    printf 'nix-shell is required\n' >&2
    return 1
  }
  scratch="$(cd "$1" && pwd -P)"
  [ -f "$scratch/.cerebro-benchmark-scratch" ] || {
    printf 'not a benchmark scratch directory: %s\n' "$scratch" >&2
    return 1
  }
  require_clean
  export BENCH_RECOVERY_SCRATCH="$scratch"
  printf -v command 'bash %q _resume' "$SCRIPT"
  bench_isolate_r
  nix-shell "$REPO/default.nix" -A shell --arg withBPCells true --run "$command"
}

# Every benchmark R process, including the Scale memory snapshot taken before
# the scratch library exists, must see only the Nix site libraries. A caller's
# R_LIBS, R_LIBS_SITE, user library, ~/.Renviron or ~/.Rprofile can otherwise
# load packages built for another R. Call before nix-shell, whose setup hooks
# append to an inherited R_LIBS_SITE rather than replacing it.
bench_isolate_r() {
  unset R_LIBS R_LIBS_SITE
  export R_LIBS_USER=/nonexistent R_ENVIRON_USER=/dev/null R_PROFILE_USER=/dev/null
}

worker() {
  local code command tmp_exit
  printf '==> benchmark mode: %s\n' "${1:-run}"
  printf -v command 'bash %q _inside %q' "$SCRIPT" "${1:-run}"
  bench_isolate_r
  set +e
  nix-shell "$REPO/default.nix" -A shell --arg withBPCells true --run "$command"
  code=$?
  set -e
  tmp_exit="$EXIT_FILE.$$"
  printf '%s\n' "$code" > "$tmp_exit"
  mv -f -- "$tmp_exit" "$EXIT_FILE"
  exit "$code"
}

[ "${BASH_SOURCE[0]}" = "$0" ] || return 0

ACTION="${1:-run}"
case "$ACTION" in
  status)
    mkdir -p "$STATE_DIR"
    show_status
    exit 0
    ;;
  _worker)
    worker "${2:-run}"
    ;;
  _inside)
    run_all "${2:-run}"
    exit 0
    ;;
  _profile)
    run_profile_engine
    exit $?
    ;;
  _resume)
    resume_inside
    exit $?
    ;;
  resume)
    resume_run "${2:-}"
    exit $?
    ;;
  run|quick|scale|full)
    ;;
  *)
    printf 'usage: %s [quick|run|scale|full|status|resume <scratch-directory>]\n' "$0" >&2
    exit 2
    ;;
esac

export BENCH_THREADS="${BENCH_THREADS:-1}"
export BENCH_SOURCE_CACHE="${BENCH_SOURCE_CACHE:-$CACHE_ROOT/sources}"
export BENCH_SCRATCH_PARENT="${BENCH_SCRATCH_PARENT:-$CACHE_ROOT/scratch}"
export BENCH_STORAGE_DESCRIPTION="${BENCH_STORAGE_DESCRIPTION:-host=$(hostname); local benchmark storage}"

mkdir -p "$STATE_DIR" "$BENCH_SOURCE_CACHE" "$BENCH_SCRATCH_PARENT"
for external_dir in "$STATE_DIR" "$BENCH_SOURCE_CACHE" "$BENCH_SCRATCH_PARENT"; do
  external_dir=$(cd "$external_dir" && pwd -P)
  case "$external_dir/" in
    "$REPO/"*)
      printf 'benchmark cache and scratch must be outside the Git checkout: %s\n' "$external_dir" >&2
      exit 1
      ;;
  esac
done
if is_running; then
  show_status
  exit 1
fi
command -v nix-shell >/dev/null 2>&1 || {
  printf 'nix-shell is required\n' >&2
  exit 1
}

require_clean

if [ -f "$LOG_FILE" ]; then
  mkdir -p "$STATE_DIR/logs"
  mv -- "$LOG_FILE" "$STATE_DIR/logs/$(date -u +%Y%m%dT%H%M%S%N)-$$.log"
fi
rm -f -- "$PID_FILE" "$EXIT_FILE"
nohup "$SCRIPT" _worker "$ACTION" > "$LOG_FILE" 2>&1 < /dev/null &
pid=$!
printf '%s\n' "$pid" > "$PID_FILE"
sleep 2
if ! kill -0 "$pid" 2>/dev/null; then
  tail -n 40 "$LOG_FILE" >&2 || true
  exit 1
fi

printf 'benchmark started: PID=%s\n' "$pid"
printf 'log: %s\n' "$LOG_FILE"
printf 'status: bash %s status\n' "$SCRIPT"
