#!/usr/bin/env bash
set -euo pipefail
source /mnt/c/Users/wangx/AppData/Local/Temp/cerebro-human500-fill-20261016T092703Z/env.sh
source "$TASK/memory.env"
for k in 2 3 4 5; do
 export BUILD_REPEAT="$k"
 export SUPPLEMENT_EVIDENCE="$TASK/rep-$k"
 export SUPPLEMENT_SCRATCH="/home/wang/.cache/cerebronexus-benchmark/human500-fill-20261016T092703Z/rep-$k"
 mkdir -p "$(dirname "$SUPPLEMENT_SCRATCH")"
 mkdir "$SUPPLEMENT_SCRATCH"
 mkdir "$SUPPLEMENT_SCRATCH/sources"
 ln -s "$BENCH_SOURCE_CACHE/d27fb144-f105-46c2-b36f-f51421f74e4e.h5ad" "$SUPPLEMENT_SCRATCH/sources/"
 date -Is > "$SUPPLEMENT_EVIDENCE/started.txt"
 Rscript "$TASK/build.R" > "$SUPPLEMENT_EVIDENCE/build.log" 2>&1
 for a in 1 2; do
  Rscript "$BENCH_ROOT/benchmark_cli.R" access human_pfc_hbcc 500000 embedded "$k" "$k" "$a" "$SUPPLEMENT_SCRATCH/bench.crb" "$SUPPLEMENT_EVIDENCE/20_access.csv" "$SUPPLEMENT_EVIDENCE/query_plan.rds" > "$SUPPLEMENT_EVIDENCE/access-$a.log" 2>&1
 done
 Rscript "$TASK/validate.R" > "$SUPPLEMENT_EVIDENCE/validation.log" 2>&1
 date -Is > "$SUPPLEMENT_EVIDENCE/complete.txt"
 echo "REPLICATE $k COMPLETE $(date -Is)"
done
