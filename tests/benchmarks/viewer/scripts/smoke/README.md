# Viewer smoke check

Check that a real CRB loads and supports rendering, selection, zoom and gene switching. Run from the repository root in Linux or WSL with Chrome/Chromium available:

```bash
nix-shell default.nix -A shell
bash tests/benchmarks/viewer/run.sh smoke /data/brain.crb

# Check several files in one run.
bash tests/benchmarks/viewer/run.sh smoke /data/brain.crb /data/pfc.crb
```

The command prints the report location. By default, each run gets a directory under `${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark/viewer-smoke/` containing `summary.md`, `viewer_validation.csv`, screenshots and `install.log`.

Set `VIEWER_VALIDATION_RESULT_DIR` to choose another output directory; it must not already exist, so previous reports and screenshots are preserved. `VIEWER_VALIDATION_TIMEOUT` changes the per-step timeout in milliseconds (default: 300000).

This checks functionality. It does not change backend benchmark results or measure repeated page-performance trials.
