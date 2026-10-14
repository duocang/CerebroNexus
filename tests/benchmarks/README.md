# Benchmarks

Run commands from the repository root in Linux or WSL. Use Linux paths such as `/data/brain.crb` or `/mnt/c/data/brain.crb`.

## Expression storage: H5 / BPCells / embedded

```bash
# Choose this for a small correctness check: 1,000 cells per source.
bash tests/benchmarks/expression-backends/run.sh quick

# OR choose this for performance measurements: full datasets, then size tiers.
bash tests/benchmarks/expression-backends/run.sh

# Check whether the background run is active, finished or failed.
bash tests/benchmarks/expression-backends/run.sh status
```

Choose one of the first two commands; do not start them together. Both enter the project Nix environment and run in the background. The first use downloads complete source files (about 18 GB total); later runs reuse verified downloads.

The full run measures build time, file size, memory, startup and gene reads on mouse and human data, with five independent builds and two access processes per build. It then compares sizes from 1,000 to 1 million cells. `quick` runs each backend once to check correctness; its numbers are not sufficient for performance conclusions.

```bash
# Follow progress; Ctrl+C here stops viewing the log, not the benchmark.
tail -f "${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark/runner/benchmark.log"
```

Results are saved under `results/expression-backends/`, separately for `quick`, `full` and `scale`. Each run has its own directory; previous results remain available. Full and scale runs also generate report figures. See [backend instructions](expression-backends/README.md) for reading results, recovering a failed final step and exporting paper figures.

## Viewer: page speed and basic functionality

```bash
# Enter the project R environment for these foreground commands.
nix-shell default.nix -A shell

# Compare page response times using a prepared page-test CRB.
bash tests/benchmarks/viewer/run.sh pages --artifact /data/brain-pages.crb

# Check that an existing CRB loads and basic interactions work.
bash tests/benchmarks/viewer/run.sh smoke /data/brain.crb
```

Viewer runs also need Chrome/Chromium. See [Viewer instructions](viewer/README.md) for data preparation and revision comparisons.

## Files

This continues the benchmarks previously under `tests/bench/`. The old `run_sweep.sh`, `config/`, `lib/` and numbered `src/` stages are consolidated into `expression-backends/scripts/`; there is one backend workflow to maintain. The existing Viewer, CRB and Ren diagnostics moved to `viewer/scripts/` because they measure page behavior or investigate prepared artifacts. Older July pilot records remain in Git history under `tests/bench/result/archive/`; the saved September runs are under `results/expression-backends/`.

- `expression-backends/` and `viewer/`: one `run.sh` entry point each; implementation files are in `scripts/`.
- `results/`: saved backend and Viewer measurements. The existing records are historical; see the workflow guides before using their numbers.
- Downloads, temporary files and runner logs stay outside the repository. This benchmark directory is excluded from R package builds.
