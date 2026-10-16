#!/usr/bin/env bash
set -euo pipefail
task=/mnt/c/Users/wangx/AppData/Local/Temp/cerebro-scale-assembled-20261016
repo=/mnt/c/Users/wangx/.codex/worktrees/benchmark-clock-smoke/CerebroNexus
if [ "${1:-}" = scope-probe ]; then
  cg=$(awk -F: '$1 == "0" { print $3 }' /proc/self/cgroup)
  test "$(cat "/sys/fs/cgroup$cg/memory.max")" = 67108864
  test "$(cat "/sys/fs/cgroup$cg/memory.swap.max")" = 0
  echo 'PASS: systemd scope enforces MemoryMax=64MiB and MemorySwapMax=0 (shell probe only)'
  exit 0
fi
systemd-run --user --scope --quiet -p MemoryMax=67108864 -p MemorySwapMax=0 bash "$task/offline-checks.sh" scope-probe
source "$repo/tests/benchmarks/expression-backends/scripts/run.sh"
run_profile() { printf '%s %s\n' "$1" "$2"; }
test "$(run_all run)" = 'scale scale'
test "$(run_all scale)" = 'scale scale'
test "$(run_all full)" = 'panel_c2 full'
echo 'PASS: default and Scale dispatch call only Scale; optional Full remains separate (mocked dispatcher)'
