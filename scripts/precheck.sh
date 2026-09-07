#!/usr/bin/env bash
# Local checks mirroring the CI test groups without changing the worktree.
set -uo pipefail

cd "$(git rev-parse --show-toplevel)"

failures=()

run_check() {
  local name="$1"
  local log
  local status
  shift
  log="$(mktemp)"

  if "$@" >"$log" 2>&1; then
    rm -f "$log"
    printf '[%s passed]\n' "$name"
    return 0
  else
    status=$?
  fi

  printf '\n[%s failed; exit %s]\n' "$name" "$status" >&2
  cat "$log" >&2
  rm -f "$log"
  return "$status"
}

record_check() {
  local name="$1"
  if run_check "$@"; then
    return 0
  fi
  failures+=("$name")
  return 0
}

run_group() {
  record_check "tests: $1" Rscript scripts/run-test-shard.R --group "$1" --shard 1 --shards 1
}

cpu_count() {
  sysctl -n hw.ncpu 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1
}

logic_shard_count() {
  local workers="$1"
  local shards=1
  while (((shards + 1) * (shards + 1) <= workers)); do
    ((shards++))
  done
  printf '%s\n' "$shards"
}

run_parallel_test_groups() {
  local workers
  local logic_shards
  local shard
  local index
  local pids=()
  local labels=()

  workers="$(cpu_count)"
  [[ "$workers" =~ ^[1-9][0-9]*$ ]] || workers=1
  logic_shards="${CEREBRO_PRECHECK_LOGIC_SHARDS:-$(logic_shard_count "$workers")}"
  [[ "$logic_shards" =~ ^[1-9][0-9]*$ ]] || {
    echo "CEREBRO_PRECHECK_LOGIC_SHARDS must be a positive integer." >&2
    exit 2
  }

  for ((shard = 1; shard <= logic_shards; shard++)); do
    run_check "tests: logic shard $shard/$logic_shards" Rscript scripts/run-test-shard.R --group logic --shard "$shard" --shards "$logic_shards" &
    pids+=("$!")
    labels+=("tests: logic shard $shard/$logic_shards")
  done
  run_check "tests: process-sensitive" Rscript scripts/run-test-shard.R --group process-sensitive --shard 1 --shards 1 &
  pids+=("$!")
  labels+=("tests: process-sensitive")

  for index in "${!pids[@]}"; do
    if wait "${pids[$index]}"; then
      :
    else
      failures+=("${labels[$index]}")
    fi
  done
}

finish() {
  if ((${#failures[@]})); then
    printf '\nPrecheck failures: %s\n' "${failures[*]}" >&2
    exit 1
  fi
}

case "${1:-fast}" in
  air)
    command -v air >/dev/null || { echo "air not found (brew install air)"; exit 2; }
    record_check "air format" air format --check .
    ;;
  fast)
    record_check "R CMD INSTALL" R CMD INSTALL .
    run_parallel_test_groups
    ;;
  full)
    record_check "R CMD INSTALL" R CMD INSTALL .
    run_parallel_test_groups
    record_check "tests: browser" env CEREBRO_RUN_BROWSER_TESTS=true Rscript scripts/run-test-shard.R --group browser --shard 1 --shards 1
    record_check "devtools check" Rscript -e "devtools::check(args = c('--no-tests'), vignettes = TRUE, error_on = 'warning')"
    record_check "pkgdown build" Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE, dest_dir = 'pkgdown-site')"
    ;;
  *)
    echo "Usage: scripts/precheck.sh [air|fast|full]" >&2
    exit 2
    ;;
esac

finish
