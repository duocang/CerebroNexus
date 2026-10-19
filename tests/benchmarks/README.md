# Benchmarks

Run commands from the repository root in Linux or WSL. Use Linux paths such as `/data/brain.crb` or `/mnt/c/data/brain.crb`.

The H5 / BPCells / embedded expression-storage benchmark, its results and the paper figures live in [CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code).

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

The Viewer, CRB and Ren diagnostics previously under `tests/bench/` are in `viewer/scripts/`, with one `run.sh` entry point. Saved Viewer results are kept in CerebroNexus_paper_code (`topics/viewer-performance/data`). Downloads, temporary files and runner logs stay outside the repository. This benchmark directory is excluded from R package builds.
