# Real-data expression-backend benchmark

The publication workflow compares CerebroNexus on two complete public single-cell matrices. It does not truncate either source or use 50k, 150k, or one-million-cell samples as publication evidence.

> **Current status:** historical evidence has been retired. No publication result is current until `run_publication_full.sh` completes and publishes a new immutable run.

Read [METHODOLOGY.md](METHODOLOGY.md) for the protocol and [RESULTS.md](RESULTS.md) before interpreting generated values.

## Publication run

Use a clean checkout, the pinned Nix environment, an exclusive high-memory node, a persistent checksum-verified source cache, and local scratch storage.

```bash
nix-shell default.nix -A shell

git status --short
git rev-parse HEAD

BENCH_THREADS=1 \
  BENCH_SOURCE_CACHE=/persistent/cerebro-benchmark-sources \
  BENCH_SCRATCH_PARENT=/fast/local/scratch \
  BENCH_STORAGE_DESCRIPTION="local NVMe; ext4; model=<model>" \
  tests/bench/run_publication_full.sh
```

The publication wrapper rejects source overrides and runs exactly this grid:

| sources | cells | backends | builds | access processes | Viewer processes |
|---|---:|---|---:|---:|---:|
| 10x mouse brain E18 | 1,306,127 | bpcells, h5 | 6 | 12 | 6 |
| PsychAD HBCC human PFC | 1,486,324 | bpcells, h5 | 6 | 12 | 6 |

`embedded` is recorded as not representable because each complete matrix exceeds the 32-bit non-zero index limit of `Matrix::dgCMatrix`; it is not attempted on a smaller substitute.

Each source has one frozen 12-gene query plan. Runtime measurements cover full-cell single-gene and 12-gene reads plus deterministic reverse-ordered, non-contiguous reads of up to 100,000 cells. CRBs are written with the default qs2 codec, BPCells uses CerebroNexus's production gene-major writer, and fresh-process startup uses `readCerebro()`.

Every build is also exercised by a fresh standalone App/browser process. Publication requires all cells to render with WebGPU, no renderer error or context loss, and successful hover, box selection, zoom, gene switching, and Linked Views. Timings and JavaScript heap use are therefore replicated three times per source/backend pair.

Validated runs are published under `result/publication-full/runs/<run-id>/`; `CURRENT` changes last.

### Remote rerun on the benchmark host

The launcher updates `paper/real-data-benchmark` from `origin` with a fast-forward only, removes the previous publication result and scratch directories, preserves the downloaded source cache, and starts the complete benchmark with `nohup`.

```bash
cd /home/xuesong/Projects/CerebroNexus
git fetch origin paper/real-data-benchmark
git switch paper/real-data-benchmark
git pull --ff-only origin paper/real-data-benchmark
bash tests/bench/update_and_run_publication_full.sh
```

Use `bash tests/bench/update_and_run_publication_full.sh status` for the current PID or final exit status. The launcher prints the absolute log path when it starts.

## Harness development

`quick`, `standard`, and `stress` remain sampled smoke/development profiles for changing the harness. They are not publication evidence and are not called by `run_publication_full.sh`.

```bash
BENCH_RESULT_ROOT="$TMPDIR/cerebro-quick-results" \
  BENCH_SOURCE_CACHE=/persistent/cerebro-benchmark-sources \
  BENCH_SOURCES_ONLY=mouse_brain_e18 \
  BENCH_PROFILE=quick tests/bench/run_sweep.sh
```

The older `benchmark_million_cell_*`, `prepare_viewer_1m_*`, and `benchmark_viewer_1m_pages.R` scripts preserve historical PR0-PR5 engineering comparisons only. They are not part of the current paper benchmark.

## Script map

| script | purpose |
|---|---|
| `run_publication_full.sh` | run the exact complete-source publication protocol |
| `run_sweep.sh` | execute and immutably publish one profile |
| `01_inspect_data.R` | inspect source dimensions and sparsity |
| `02_record_environment.R` | record code, machine, storage, and dependencies |
| `03_plan_runs.R` | write the deterministic schedule |
| `04_check_full_resources.R` | gate complete-source out-of-core runs |
| `05_prepare_query_plan.R` | freeze the untimed query plan and reference fingerprints |
| `11_build_full_backend.R` | build one complete-source backend and production CRB |
| `20_measure_backend.R` | measure hydrated startup and expression access |
| `21_measure_viewer.R` | run one full standalone Viewer observation |
| `30_check_measurements.R` | reject incomplete, failed, or inconsistent evidence |
| `40_write_report.R` / `41_draw_figures.R` | generate the report and publication overview |
| `50_check_outputs.R` | validate the report package |
| `60_publish_results.R` | publish immutably and update `CURRENT` last |
