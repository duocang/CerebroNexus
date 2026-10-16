#!/usr/bin/env bash
set -euo pipefail
source /mnt/c/Users/wangx/AppData/Local/Temp/cerebro-human500-fill-20261016T092703Z/env.sh
printf '%s  %s\n' aeca0480ab8941a7e4cf6b0ff6dc8c5f9d0de376466d65ca8198dc873f1cb16f "$BENCH_SOURCE_CACHE/d27fb144-f105-46c2-b36f-f51421f74e4e.h5ad" | sha256sum -c - > "$TASK/source-sha256.log"
nix-shell "$REPO/default.nix" -A shell --run "Rscript $TASK/preflight.R" > "$TASK/preflight.log" 2>&1
