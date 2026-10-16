#!/usr/bin/env bash
set -euo pipefail
source /mnt/c/Users/wangx/AppData/Local/Temp/cerebro-supplement-20261016-081232/env.sh
export BENCH_RUN_ID=20261016T061232Z-81d11e8aedfa-scale-300k
export BENCH_SCRATCH=/home/wang/.cache/cerebronexus-benchmark/supplement-300k-20261016-081232
export STAGE="$EXTRA/scale300"
mkdir "$BENCH_SCRATCH"
mkdir -p "$BENCH_SCRATCH/sources" "$BENCH_SCRATCH/query-plans" "$STAGE/logs"
ln -s "$STAGE" "$BENCH_SCRATCH/result"
for name in 1M_neurons_filtered_gene_bc_matrices_h5.h5 d27fb144-f105-46c2-b36f-f51421f74e4e.h5ad; do
  test -r "$BENCH_SOURCE_CACHE/$name"
  ln -s "$BENCH_SOURCE_CACHE/$name" "$BENCH_SCRATCH/sources/$name"
done
cli="$BENCH_ROOT/benchmark_cli.R"
Rscript "$EXTRA/prepare300.R"
Rscript "$cli" environment "$STAGE/run_manifest.csv"
Rscript "$cli" full-resources "$STAGE/00_probe.csv" "$STAGE/05_schedule.csv" "$STAGE/run_manifest.csv" "$STAGE/resource_check.csv"
for src in mouse_brain_e18 human_pfc_hbcc; do
  echo "QUERY_PLAN $src 300000 $(date -Is)"
  Rscript "$cli" query-plan "$src" 300000 "$BENCH_SCRATCH" "$BENCH_SCRATCH/query-plans/${src}_300000.rds" "$STAGE/query_plan_manifest.csv" "$STAGE/query_panel.csv" > "$STAGE/logs/query_$src.log" 2>&1
  while IFS=$'\t' read -r profile row_source tier comparison rep position backend access_repeats; do
    [ "$row_source" = "$src" ] || continue
    tag="${src}_${tier}_${backend}_r${rep}"
    plan="$BENCH_SCRATCH/query-plans/${src}_300000.rds"
    cmd=build-full
    [ "$backend" != embedded ] || cmd=export
    echo "BUILD $tag $(date -Is)"
    Rscript "$cli" "$cmd" "$src" "$tier" "$backend" "$rep" "$position" "$BENCH_SCRATCH" "$STAGE/10_export.csv" "$plan" > "$STAGE/logs/build_$tag.log" 2>&1
    crb="$BENCH_SCRATCH/export/$tag/bench.crb"
    if [ ! -f "$crb" ]; then
      echo "MISSING_ARTIFACT $tag; inspect outcome; no retry"
      continue
    fi
    for access in 1 2; do
      echo "ACCESS $tag $access $(date -Is)"
      Rscript "$cli" access "$src" "$tier" "$backend" "$rep" "$position" "$access" "$crb" "$STAGE/20_access.csv" "$plan" > "$STAGE/logs/access_${tag}_a${access}.log" 2>&1
    done
  done < "$STAGE/05_schedule.tsv"
done
Rscript "$cli" validate "$STAGE" > "$STAGE/validation.log" 2>&1
Rscript "$EXTRA/validate300.R"
echo "SCALE300_VALIDATED $(date -Is)"
