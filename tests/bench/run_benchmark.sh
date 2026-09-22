#!/usr/bin/env bash

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
BENCH_ROOT="$REPO/tests/bench"
CACHE_ROOT="${BENCH_CACHE_ROOT:-${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark}"
STATE_DIR="$CACHE_ROOT/runner"
PID_FILE="$STATE_DIR/benchmark.pid"
EXIT_FILE="$STATE_DIR/benchmark.exit"
LOG_FILE="$STATE_DIR/benchmark.log"
SCRIPT="$BENCH_ROOT/run_benchmark.sh"

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
    . ":(exclude,glob)tests/bench/result/**")
  [ -z "$dirty" ] || {
    printf '%s\n' "$dirty" >&2
    printf 'benchmark requires a clean Git worktree\n' >&2
    return 1
  }
}

run_profile() {
  local profile="$1"
  local result_name="$2"
  local head_sha study_id
  require_clean
  head_sha=$(git -C "$REPO" rev-parse HEAD)
  study_id="$(date -u +%Y%m%dT%H%M%SZ)-${head_sha:0:12}-$result_name"
  printf '==> running %s\n' "$result_name"
  BENCH_PROFILE="$profile" \
    BENCH_STUDY_ID="$study_id" \
    BENCH_RUN_ID="$study_id" \
    BENCH_RESULT_ROOT="$BENCH_ROOT/result/$result_name" \
    "$BENCH_ROOT/_benchmark_profile.sh"
}

run_all() {
  run_profile panel_c2 publication-full
  run_profile publication_scale publication-scale
}

worker() {
  local code command tmp_exit
  printf '==> running full benchmark, then scale benchmark\n'
  printf -v command 'bash %q _inside' "$SCRIPT"
  set +e
  nix-shell "$REPO/default.nix" -A shell --run "$command"
  code=$?
  set -e
  tmp_exit="$EXIT_FILE.$$"
  printf '%s\n' "$code" > "$tmp_exit"
  mv -f -- "$tmp_exit" "$EXIT_FILE"
  exit "$code"
}

ACTION="${1:-run}"
case "$ACTION" in
  status)
    mkdir -p "$STATE_DIR"
    show_status
    exit 0
    ;;
  _worker)
    worker
    ;;
  _inside)
    run_all
    exit 0
    ;;
  run)
    ;;
  *)
    printf 'usage: %s [run|status]\n' "$0" >&2
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

rm -f -- "$PID_FILE" "$EXIT_FILE" "$LOG_FILE"
nohup "$SCRIPT" _worker > "$LOG_FILE" 2>&1 < /dev/null &
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
