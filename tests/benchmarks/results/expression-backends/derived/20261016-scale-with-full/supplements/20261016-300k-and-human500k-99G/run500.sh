#!/usr/bin/env bash
set -euo pipefail
source /mnt/c/Users/wangx/AppData/Local/Temp/cerebro-supplement-20261016-081232/env.sh
export SUPPLEMENT_EVIDENCE="$EXTRA/human500-99G"
export SUPPLEMENT_SCRATCH=/home/wang/.cache/cerebronexus-benchmark/supplement-human500k-99G-20261016-081232
export BENCH_RUN_ID=20261016T061232Z-81d11e8aedfa-human500k-embedded-99G
mkdir "$SUPPLEMENT_SCRATCH"
mkdir "$SUPPLEMENT_SCRATCH/sources"
ln -s "$BENCH_SOURCE_CACHE/d27fb144-f105-46c2-b36f-f51421f74e4e.h5ad" "$SUPPLEMENT_SCRATCH/sources/"
cli="$BENCH_ROOT/benchmark_cli.R"
Rscript "$SUPPLEMENT_EVIDENCE/build.R"
for rep in 1 2; do
  Rscript "$cli" access human_pfc_hbcc 500000 embedded 1 1 "$rep" "$SUPPLEMENT_SCRATCH/bench.crb" "$SUPPLEMENT_EVIDENCE/20_access.csv" "$SUPPLEMENT_EVIDENCE/query_plan.rds"
done
Rscript "$SUPPLEMENT_EVIDENCE/validate.R"
date -Is
