# Real-data expression-backend benchmark

This directory compares the `embedded`, `bpcells`, and `h5` backends on public single-cell matrices. This page explains how to run it. Read [METHODOLOGY.md](METHODOLOGY.md) for the experimental design and [RESULTS.md](RESULTS.md) before interpreting any number.

> **Current status:** Panel A/B is published as immutable run
> `20260907T172844Z-9c6ab101e4a5-publication`. Panel C runs incrementally and
> never rewrites that evidence.

## Quick start

```bash
# Smallest correctness and harness check
BENCH_PROFILE=quick tests/bench/run_sweep.sh

# Repeated local review
BENCH_PROFILE=standard tests/bench/run_sweep.sh

# Repeated evidence plus staged figures
BENCH_PROFILE=publication tests/bench/run_sweep.sh

# Explicit memory-boundary experiment; normally rejected on a 32 GiB host
BENCH_PROFILE=stress tests/bench/run_sweep.sh

# C1 (400k/300k) followed by C2 (both complete sources)
BENCH_SOURCE_CACHE="/shared/cerebro-benchmark-sources" \
  tests/bench/run_panel_c.sh
```

Set `BENCH_RESULT_ROOT` for exploratory runs so generated evidence does not
dirty the checkout that will later produce publication evidence. Set
`BENCH_SOURCE_CACHE` to reuse the two checksum-verified source downloads.

```bash
BENCH_RESULT_ROOT="$TMPDIR/cerebro-quick-results" \
  BENCH_SOURCE_CACHE="/shared/cerebro-benchmark-sources" \
  BENCH_SOURCES_ONLY=mouse_brain_e18 \
  BENCH_PROFILE=quick tests/bench/run_sweep.sh
```

## HPC publication run

Use the pinned Nix environment and an exclusive node. `BENCH_THREADS` defaults
to one and is propagated to the common BLAS/OpenMP thread controls; keep the
same value for every compared backend.

```bash
git clone --filter=blob:none --single-branch \
  --branch paper/real-data-benchmark \
  https://github.com/duocang/CerebroNexus.git
cd CerebroNexus

nix-shell default.nix -A shell

git status --short                 # must print nothing
git rev-parse HEAD                 # record the exact code under test

BENCH_THREADS=1 \
  BENCH_SOURCE_CACHE="/shared/cerebro-benchmark-sources" \
  BENCH_SCRATCH_PARENT="${SLURM_TMPDIR:-/fast/local/scratch/$USER}" \
  BENCH_PROFILE=publication tests/bench/run_sweep.sh
```

After validation, `tests/bench/result/CURRENT` names the immutable result. Add
only that pointer and its run directory when returning evidence to Git.

```bash
run_id=$(< tests/bench/result/CURRENT)
git add tests/bench/result/CURRENT "tests/bench/result/runs/$run_id"
git commit -m "docs(bench): publish real-data evidence"
git push origin paper/real-data-benchmark
```

Limit a run to one source when developing the harness:

```bash
BENCH_SOURCES_ONLY=mouse_brain_e18 \
  BENCH_PROFILE=quick tests/bench/run_sweep.sh
```

## Incremental Panel C

`run_panel_c.sh` reuses checksum-verified files in `BENCH_SOURCE_CACHE`. C1
publishes 18 builds and 36 access processes under `result/panel-c1/`. C2
publishes 12 streamed builds and 24 access processes under `result/panel-c2/`.
Running one part does not rerun A/B or the other part:

```bash
BENCH_PANEL_C_PART=c1 tests/bench/run_panel_c.sh
BENCH_PANEL_C_PART=c2 tests/bench/run_panel_c.sh
```

After both parts validate, an unqualified run also writes the derived combined
summary, manifest, and figure under `result/panel-c/`.

For a disconnect-safe remote run:

```bash
mkdir -p /home/xuesong/benchmark-logs
tmux new-session -d \
  -s cerebro-panel-c \
  -c /home/xuesong/Projects/CerebroNexus \
  'BENCH_THREADS=1 BENCH_SOURCE_CACHE=/home/xuesong/.cache/cerebro-benchmark-sources BENCH_SCRATCH_PARENT=/tmp nix-shell default.nix -A shell --run "tests/bench/run_panel_c.sh" 2>&1 | tee /home/xuesong/benchmark-logs/panel-c.log; exec zsh'

tail -f /home/xuesong/benchmark-logs/panel-c.log
```

`BENCH_ALLOW_UNSAFE=1` bypasses the resource gate. Use it only for an intentional stress run. Normal runs must not silently skip unsafe tiers.

## What happens before a download

The command first:

1. inspects source dimensions;
2. records the machine and Git revision;
3. creates the requested run plan; and
4. checks estimated memory, sparse-index, and free-disk limits.

If the plan is unsafe, it stops with the source, cell tier, estimated memory, safe budget, and reason. No complete source file or backend export has started at that point.

## Profiles

| profile | purpose | large boundary tiers |
|---|---|:---:|
| `quick` | verify the harness and correctness gate | no |
| `standard` | repeated local comparison | no |
| `publication` | repeated article evidence and figures | no |
| `stress` | opt-in host memory-boundary experiment | yes |

## Outputs

Validated runs are immutable under `result/runs/<run-id>/`. `result/CURRENT` contains the run used by report and plotting tools. A failed or interrupted run leaves the previous pointer unchanged.

The 2026-07-30 single-run pilot is retained under `result/archive/pilot-2026-07-30/`; it is superseded and cannot support current performance claims.

## Plain-language script map

| script | meaning |
|---|---|
| `01_inspect_data.R` | find out how large the sources are |
| `02_record_environment.R` | record the code and machine under test |
| `03_plan_runs.R` | list the requested backend runs |
| `04_check_resources.R` | stop before running a plan that will not fit |
| `04_check_full_resources.R` | check the out-of-core full-source plan |
| `10_export_backend.R` | export one backend in a fresh process |
| `11_build_full_backend.R` | stream one complete source into BPCells or H5 |
| `20_measure_backend.R` | measure and correctness-check one backend |
| `30_check_measurements.R` | reject incomplete or incorrect measurements |
| `40_write_report.R` | generate the Markdown result report |
| `41_draw_figures.R` | generate publication figures |
| `42_write_panel_c_report.R` | combine immutable A/B, C1, and C2 runs |
| `43_draw_panel_c_figure.R` | draw the combined Panel C figure |
| `50_check_outputs.R` | ensure the report package is complete |
| `60_publish_results.R` | publish immutably and update `CURRENT` last |

The two default public sources are 10x mouse brain E18 (4.2 GB) and the HBCC human prefrontal-cortex atlas (14.2 GB). Publication comparisons use 50k and 150k cells from both sources; the resource preflight therefore requires a high-memory host. The MSSM cohort is opt-in through `BENCH_SOURCES_EXTRA=human_pfc_mssm`.
