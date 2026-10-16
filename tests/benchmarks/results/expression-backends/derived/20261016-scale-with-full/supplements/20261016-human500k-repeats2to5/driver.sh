#!/usr/bin/env bash
set -euo pipefail
source /mnt/c/Users/wangx/AppData/Local/Temp/cerebro-human500-fill-20261016T092703Z/env.sh
source "$TASK/memory.env"
finish() { rc=$?; echo "$rc" > "$TASK/runner.exit"; date -Is > "$TASK/runner.finished"; }
trap finish EXIT
systemd-run --user --wait --pipe --unit=cerebro-human500-fill-20261016t092703z-measure -p MemoryMax="$BENCH_MEMORY_LIMIT_BYTES" -p MemorySwapMax=0 -p OOMPolicy=kill -p RuntimeMaxSec=18000 -p Nice=10 -p IOWeight=10 /nix/var/nix/profiles/default/bin/nix-shell "$REPO/default.nix" -A shell --run "bash $TASK/inside.sh" > "$TASK/runner.log" 2>&1
date -Is > "$TASK/runner.complete"
