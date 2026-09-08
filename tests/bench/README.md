# Real-data expression-backend benchmark

This directory compares Cerebro's `embedded`, `bpcells`, and `h5` expression
backends on two public single-cell matrices. The publication workflow is one
complete study: A/B, C1, and C2 are acquired on the same machine, filesystem,
code revision, dependency set, source files, and thread count.

> **Current status:** historical evidence has been removed. No publication
> result is current until `run_publication_full.sh` completes and publishes a
> new immutable study.

Read [METHODOLOGY.md](METHODOLOGY.md) for the design and
[RESULTS.md](RESULTS.md) before interpreting generated values.

## Publication run

Use a clean checkout, the pinned Nix environment, an exclusive high-memory
node, persistent checksum-verified source cache, and local scratch storage.

```bash
nix-shell default.nix -A shell

git status --short       # must print nothing
git rev-parse HEAD       # record the code under test

BENCH_THREADS=1 \
  BENCH_SOURCE_CACHE=/persistent/cerebro-benchmark-sources \
  BENCH_SCRATCH_PARENT=/fast/local/scratch \
  BENCH_STORAGE_DESCRIPTION="local NVMe; ext4; model=<model>" \
  tests/bench/run_publication_full.sh
```

The wrapper owns the exact study design and rejects source-selection overrides.
It runs:

| phase | cells | backends | builds | access processes |
|---|---|---|---:|---:|
| A/B | 50k and 150k from mouse and human | embedded, bpcells, h5 | 36 | 72 |
| C1 | mouse 400k; human 300k | embedded, bpcells, h5 | 18 | 36 |
| C2 | complete mouse and human sources | bpcells, h5 | 12 | 24 |

For every source/tier, a deterministic query plan is prepared in a separate
process before any timed build; the exact 12-gene panel is retained as CSV.
Each phase publishes into private study work;
only after all phases, provenance checks, correctness checks, tables, and the
combined figure succeed is the complete bundle copied to
`result/publication-full/runs/<study-id>/`. `CURRENT` is updated last.

Set `BENCH_STUDY_ID` to resume a stopped study with the same code and settings.
Completed phases are reused and derived output is rebuilt. Set
`BENCH_KEEP_STUDY_WORK=1` to retain the private phase directories after a
successful publication.

For a disconnect-safe remote run:

```bash
tmux new-session -d -s cerebro-benchmark \
  -c /path/to/CerebroNexus \
  'nix-shell default.nix -A shell --run "BENCH_THREADS=1 BENCH_SOURCE_CACHE=/persistent/cache BENCH_SCRATCH_PARENT=/local/scratch BENCH_STORAGE_DESCRIPTION=local-nvme-ext4 tests/bench/run_publication_full.sh" 2>&1 | tee /path/to/benchmark.log'
```

## Harness development

These profiles are for correctness and harness development, not the final
article evidence:

```bash
BENCH_PROFILE=quick tests/bench/run_sweep.sh
BENCH_PROFILE=standard tests/bench/run_sweep.sh
BENCH_PROFILE=stress tests/bench/run_sweep.sh
```

Use external result and source-cache directories during development:

```bash
BENCH_RESULT_ROOT="$TMPDIR/cerebro-quick-results" \
  BENCH_SOURCE_CACHE=/persistent/cerebro-benchmark-sources \
  BENCH_SOURCES_ONLY=mouse_brain_e18 \
  BENCH_PROFILE=quick tests/bench/run_sweep.sh
```

`BENCH_ALLOW_UNSAFE=1` is only for an intentional stress experiment. Normal
runs stop rather than silently omit unsafe tiers.

## Script map

| script | purpose |
|---|---|
| `run_publication_full.sh` | acquire and publish the complete study |
| `run_sweep.sh` | run one internal phase or development profile |
| `01_inspect_data.R` | inspect source dimensions and sparsity |
| `02_record_environment.R` | record code, machine, storage, and dependencies |
| `03_plan_runs.R` | write the deterministic schedule |
| `04_check_resources.R` | gate sampled in-memory tiers |
| `04_check_full_resources.R` | gate full-source out-of-core tiers |
| `05_prepare_query_plan.R` | prepare and freeze the untimed query plan |
| `10_export_backend.R` | build one sampled backend in a fresh process |
| `11_build_full_backend.R` | build one full-source backend in a fresh process |
| `20_measure_backend.R` | measure and correctness-check one fresh access process |
| `30_check_measurements.R` | reject incomplete, failed, or inconsistent rows |
| `40_write_report.R` / `41_draw_figures.R` | write internal phase outputs |
| `42_write_panel_c_report.R` / `43_draw_panel_c_figure.R` | validate and combine the complete study |
| `50_check_outputs.R` | validate an internal phase package |
| `60_publish_results.R` | publish immutably and update `CURRENT` last |

The default sources are the 10x 1.3-million-cell mouse brain E18 dataset and
the 1.49-million-cell PsychAD HBCC human prefrontal-cortex dataset. Stable
identifiers, landing pages, byte sizes, and acquired SHA-256 values are stored
with every final study.
