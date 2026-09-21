# Real-data expression-backend benchmark

The backend benchmark has two independent publication workflows. The scale study measures the same two public sources at 1k, 10k, 50k, 100k, 500k, and 1m cells. The full-source study separately measures every cell in each source. Results and `CURRENT` pointers are never shared between them.

This workflow measures expression backends only. Viewer validation is an
independent smoke test under [`../viewer-validation`](../viewer-validation/)
and cannot pass, fail, publish, or replace backend evidence.

> **Current status:** historical evidence has been retired. No publication result is current until `run_publication_full.sh` completes and publishes a new immutable run.

Read [METHODOLOGY.md](METHODOLOGY.md) for the protocol and [RESULTS.md](RESULTS.md) before interpreting generated values. [PRELIMINARY_BACKEND_RESULTS.md](PRELIMINARY_BACKEND_RESULTS.md) records the rounded backend observations recovered from the last browser-gate-aborted run; it is not publication evidence.

## Publication run

On the configured benchmark host, the complete update-and-run interface is one
command:

```bash
bash tests/bench/update_and_run_publication_full.sh
```

It clears the previous local publication result and scratch directories before
starting a fresh acquisition, while preserving the checksum-verified source
cache. Use the same command with `status` to inspect a running or completed
acquisition.

Use a clean checkout, the pinned Nix environment, an exclusive high-memory node, a persistent checksum-verified source cache, and local scratch storage.

A failed run keeps its scratch directory by default; set
`BENCH_KEEP_ON_FAILURE=0` only when automatic cleanup is desired.

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

| sources | cells | backends | builds | access processes |
|---|---:|---|---:|---:|
| 10x mouse brain E18 | 1,306,127 | bpcells, h5 | 10 | 20 |
| PsychAD HBCC human PFC | 1,486,324 | bpcells, h5 | 10 | 20 |

`embedded` is recorded as not representable because each complete matrix exceeds the 32-bit non-zero index limit of `Matrix::dgCMatrix`; it is not attempted on a smaller substitute.

Each source has one frozen 12-gene query plan. Runtime measurements cover full-cell single-gene and 12-gene reads plus deterministic reverse-ordered, non-contiguous reads of up to 100,000 cells. CRBs are written with the default qs2 codec, BPCells uses CerebroNexus's production gene-major writer, and fresh-process startup uses `readCerebro()`.

Every source/backend pair has five independent builds. Each built artifact is
opened by two independent access processes. Backend order alternates by repeat.

## Publication scale run

The scale study is intentionally separate from the complete-source study. It uses BPCells and H5 at all six fixed cell-count tiers and attempts embedded storage through 500k cells, with five independent builds and two access processes per successful build. An embedded failure is retained with empty metrics and does not invalidate the mandatory BPCells/H5 results. Run it from the same clean checkout and reuse the same external source cache:

```bash
BENCH_THREADS=1 \
  BENCH_SOURCE_CACHE=/home/xuesong/.cache/cerebronexus-benchmark/sources \
  BENCH_SCRATCH_PARENT=/home/xuesong/.cache/cerebronexus-benchmark/scratch \
  BENCH_STORAGE_DESCRIPTION="local NVMe; ext4; model=<model>" \
  bash tests/bench/run_publication_scale.sh
```

Its immutable results are written under
`tests/bench/result/publication-scale/`; it neither reads nor replaces
`tests/bench/result/publication-full/`.

Validated runs are published under the selected workflow's
`result/publication-{scale,full}/runs/<run-id>/` directory; its own `CURRENT`
changes last. Every published run includes `evidence_manifest.csv`, which
records the byte size and MD5 checksum of every raw table, log, report, and
figure in the evidence package.

### Remote rerun on the benchmark host

The launcher updates `paper/real-data-benchmark` from `origin` with a fast-forward only, preserves previous immutable publication runs, removes transient study and scratch directories, preserves the downloaded source cache, and starts the complete benchmark with `nohup`.

```bash
cd /home/xuesong/Projects/CerebroNexus
git fetch origin paper/real-data-benchmark
git switch paper/real-data-benchmark
git pull --ff-only origin paper/real-data-benchmark
bash tests/bench/update_and_run_publication_full.sh
```

Use `bash tests/bench/update_and_run_publication_full.sh status` for the current PID or final exit status. The launcher prints the absolute log path when it starts.

If all timed measurements finished but a later validation, reporting, or
figure step failed, do not repeat the acquisition. Finalize the retained,
marker-protected scratch directory with:

```bash
bash tests/bench/resume_publication_from_scratch.sh /path/to/cerebro-bench.XXXXXX
```

The recovery command reruns only validation, reporting, figures, checksums, and
immutable publication. It never reruns builds or access measurements.

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
| `run_publication_scale.sh` | run the independent nine-tier scale protocol |
| `run_sweep.sh` | execute and immutably publish one profile |
| `01_inspect_data.R` | inspect source dimensions and sparsity |
| `02_record_environment.R` | record code, machine, storage, and dependencies |
| `03_plan_runs.R` | write the deterministic schedule |
| `04_check_full_resources.R` | gate complete-source out-of-core runs |
| `05_prepare_query_plan.R` | freeze the untimed query plan and reference fingerprints |
| `11_build_full_backend.R` | build one complete-source backend and production CRB |
| `20_measure_backend.R` | measure hydrated startup and expression access |
| `30_check_measurements.R` | reject incomplete, failed, or inconsistent evidence |
| `40_write_report.R` / `41_draw_figures.R` | generate the report and publication overview |
| `49_write_evidence_manifest.R` | inventory and checksum the complete evidence package |
| `50_check_outputs.R` | validate all raw evidence, reports, figures, and checksums |
| `60_publish_results.R` | publish immutably and update `CURRENT` last |
