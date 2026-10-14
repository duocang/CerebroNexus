# Backend benchmark figures

Create paper figures from a completed backend run. Run from the repository root in the project R environment:

```bash
nix-shell default.nix -A shell
RESULTS=tests/benchmarks/results/expression-backends/full
RUN="$RESULTS/runs/$(cat "$RESULTS/CURRENT")"
OUT="paper/figures/output/$(basename "$RUN")-$(date -u +%Y%m%dT%H%M%S)"
Rscript paper/figures/draw_expression_backend_benchmark.R "$RUN" "$OUT"
```

Use a fresh, complete `full` run with `access_contract = materialized-v2`. The script rechecks measurement validity, evidence checksums and the complete cell counts of both datasets. Quick, scale, incomplete and retained September 22 results are rejected. See [backend instructions](../../tests/benchmarks/expression-backends/README.md) to run new measurements.

The script needs Arial, `pdffonts`, ggplot2, patchwork and systemfonts. It writes a 178 × 132 mm vector PDF with embedded fonts, a 350 dpi colour TIFF, a figure legend, the plotted CSV inputs, a copy of the plotting script, a font check and a checksum list to `$OUT`. The output directory must be new or empty and outside the input run; existing exports and benchmark results are preserved.

Figures show medians and observed ranges. Build and access counts come from the run; the access processes share built artifacts, so their count is not the number of independent builds.

These exports are prepared for manuscript review, not automatically certified for publication. [Bioinformatics author guidelines](https://academic.oup.com/bioinformatics/pages/author-guidelines) specify 350 dpi for colour/halftone artwork and 1200 dpi for line drawings. Use the vector PDF for editing and check the required final format for your target journal. Inspect every panel at final size for readable text, alignment, clipping and overlapping labels, and confirm the legend matches the measurements. No fresh, corrected full-dataset figure has yet completed that rendered review.
