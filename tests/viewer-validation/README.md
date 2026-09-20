# Independent Viewer validation

This smoke validation answers one question: can a real CRB be opened as a
standalone CerebroNexus Viewer and complete the core render, selection,
zoom, and gene-switch interactions?

It is intentionally separate from `tests/bench`. A Viewer failure never
invalidates backend timing evidence, and running this command never reads or
updates the benchmark `CURRENT` pointer.

From the pinned project environment, run:

```bash
nix-shell default.nix -A shell
bash tests/viewer-validation/run.sh /path/to/brain.crb /path/to/pfc.crb
```

The command creates one timestamped directory under
`tests/viewer-validation/result/` containing:

- `summary.md`: a short pass/fail table;
- `viewer_validation.csv`: machine-readable details and timings;
- one Overview screenshot per artifact;
- `install.log`: installation diagnostics.

Set `VIEWER_VALIDATION_RESULT_DIR` to write elsewhere and
`VIEWER_VALIDATION_TIMEOUT` to change the default five-minute step timeout.
The result is functional evidence from one recorded Chrome/host combination,
not a replicated browser-performance benchmark.
