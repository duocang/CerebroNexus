# Real-data expression-backend benchmark

The backend benchmark runs two profiles in sequence. The full-source profile measures every cell in each source, then the scale profile measures the same sources at 1k, 10k, 50k, 100k, 500k, and 1m cells. Results and `CURRENT` pointers are never shared between them.

This workflow measures expression backends only. Viewer validation is an
independent smoke test under [`../viewer-validation`](../viewer-validation/)
and cannot pass, fail, publish, or replace backend evidence.

> **Current status:** historical evidence has been retired. No result is current until `run_benchmark.sh` completes both benchmark profiles.

Read [METHODOLOGY.md](METHODOLOGY.md) for the protocol and [RESULTS.md](RESULTS.md) before interpreting generated values. [PRELIMINARY_BACKEND_RESULTS.md](PRELIMINARY_BACKEND_RESULTS.md) records the rounded backend observations recovered from the last browser-gate-aborted run; it is not publication evidence.

## Run the benchmark

Use a clean checkout on an exclusive high-memory Linux host with `nix-shell` available. One command starts the complete benchmark in the background; the full-source profile runs first and the six-tier scale profile runs second:

```bash
bash tests/bench/run_benchmark.sh
```

The launcher uses `$HOME/.cache/cerebronexus-benchmark/` for its source cache, scratch space, PID, exit status, and log. It preserves checksum-verified downloads and failed-run scratch directories. Optional `BENCH_THREADS`, `BENCH_SOURCE_CACHE`, `BENCH_SCRATCH_PARENT`, and `BENCH_STORAGE_DESCRIPTION` overrides remain available for unusual hosts.

```bash
bash tests/bench/run_benchmark.sh status
tail -f "$HOME/.cache/cerebronexus-benchmark/runner/benchmark.log"
```

The full-source profile rejects source overrides and runs exactly this grid:

| sources | cells | backends | builds | access processes |
|---|---:|---|---:|---:|
| 10x mouse brain E18 | 1,306,127 | bpcells, h5 | 10 | 20 |
| PsychAD HBCC human PFC | 1,486,324 | bpcells, h5 | 10 | 20 |

`embedded` is recorded as not representable because each complete matrix exceeds the 32-bit non-zero index limit of `Matrix::dgCMatrix`; it is not attempted on a smaller substitute.

Each source has one frozen 12-gene query plan. Runtime measurements cover full-cell single-gene and 12-gene reads plus deterministic reverse-ordered, non-contiguous reads of up to 100,000 cells. CRBs are written with the default qs2 codec, BPCells uses CerebroNexus's production gene-major writer, and fresh-process startup uses `readCerebro()`.

Every source/backend pair has five independent builds. Each built artifact is
opened by two independent access processes. Backend order alternates by repeat.

## Scale profile

The scale profile follows the complete-source profile automatically. It uses BPCells and H5 at all six fixed cell-count tiers and attempts embedded storage through 500k cells, with five independent builds and two access processes per successful build. An embedded failure is retained with empty metrics and does not invalidate the mandatory BPCells/H5 results.

Its immutable results are written under
`tests/bench/result/publication-scale/`; it neither reads nor replaces
`tests/bench/result/publication-full/`.

Validated runs are published under the selected workflow's
`result/publication-{scale,full}/runs/<run-id>/` directory; its own `CURRENT`
changes last. Every published run includes `evidence_manifest.csv`, which
records the byte size and MD5 checksum of every raw table, log, report, and
figure in the evidence package.

If all timed measurements finished but a later validation, reporting, or
figure step failed, do not repeat the acquisition. Finalize the retained,
marker-protected scratch directory with:

```bash
bash tests/bench/resume_publication_from_scratch.sh /path/to/cerebro-bench.XXXXXX
```

The recovery command reruns only validation, reporting, figures, checksums, and
immutable publication. It never reruns builds or access measurements.

## Harness development

`quick`, `standard`, and `stress` remain sampled smoke/development profiles for changing the harness. They are not publication evidence and are not called by the public launcher.

```bash
BENCH_RESULT_ROOT="$TMPDIR/cerebro-quick-results" \
  BENCH_SOURCE_CACHE=/persistent/cerebro-benchmark-sources \
  BENCH_SOURCES_ONLY=mouse_brain_e18 \
  BENCH_PROFILE=quick tests/bench/_benchmark_profile.sh
```

The older `benchmark_million_cell_*`, `prepare_viewer_1m_*`, and `benchmark_viewer_1m_pages.R` scripts preserve historical PR0-PR5 engineering comparisons only. They are not part of the current paper benchmark.

## Script map

| script | purpose |
|---|---|
| `run_benchmark.sh` | public background launcher for the complete benchmark |
| `_benchmark_profile.sh` | private single-profile execution engine |
| `benchmark.R` | source definitions, schedules, I/O, validation, and reporting helpers |
| `benchmark_cli.R` | execute one isolated benchmark stage |
