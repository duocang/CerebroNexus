# Figure QA notes

The initial stock alignment helper converted unresolved gtable null units to zero. Its first audit correctly reported NOT AUDITABLE and blocked export. The local `measure_rendered_panels.R` now measures actual rendered panel viewports at the final 240 x 260 mm device size. It does not relax tolerances or alter the shared skill. The same backend-neutral auditor passes all six row/column comparison groups at 1.5 pt, with zero failures, warnings or exemptions.

All eight panels use consistent backend colours, actual cell counts, matched y limits between the two datasets for each metric, and log axes. Full points are rings at the exact full size. New 300k points use five-build summaries and join the scale lines. The new Human embedded 500k point is a disconnected hollow diamond, with no between-build error bar: one build and two nested read processes. Nearby full rings in low-RSS panels reflect close observed values; nothing is shifted or hidden. Very narrow min-max ranges may lie within markers. No artificial uncertainty is added. The 300k/500k tick labels are rotated to preserve separation.

| Panels | Distinct evidence role | Center / spread | Unit | Visual review |
|---|---|---|---|---|
| a, b | Single-gene interactivity across scales | Median / min-max of five within-build process medians | Build; two nested read processes | Endpoint labels, lines, markers, axes and range bars inspected |
| c, d | Materialized multi-gene workload | Same as a, b | Build; two nested read processes | Full endpoint rank changes preserved; no extrapolated values |
| e, f | Loaded memory feasibility | Same as a, b | Build; two nested read processes | Read-memory label explicitly distinguishes build peak; nearby endpoint rings intentional |
| g, h | Disk cost | Median / min-max over five builds | Build | GiB conversion checked; full disk totals sourced from exports |

Static audit warnings are reviewed as follows: R source is actually parsed and executed successfully; numeric x-axis abbreviations are not improperly capitalized legend categories; this request specifies PNG/PDF and is not a journal TIFF submission; 350 dpi follows the established benchmark export and exceeds 300 dpi; dimensions are explicitly 240 x 260 mm; positivity is enforced with stopifnot for both x and all plotted y summaries even if the static detector does not recognize it. The static audit detects an optional SVG code path, but svglite is not installed and no scientific SVG is delivered. The final standard PDF provides editable text. Alignment SVGs are diagnostic rectangles only. An initial direct Cairo overwrite on the Windows mount failed; the final export uses a local temporary PDF then copies the completed PDF to the output directory.

The caption discloses nesting, range definition, changing query genes, reused full observations, supplemental 300k acquisition, one-build 500k feasibility and missing embedded tiers. No universal speed law or statistical significance is claimed. The figure uses two original runs and two supplemental acquisitions; smoke-test data are excluded. Resource policy differences (300k cap 90 GiB, 500k cap 99 GiB; no task swap; low priority) are disclosed in README and limit cross-run causal performance interpretation.

## Final rendered checks

The final PDF uses R's standard PDF device (Helvetica, useDingbats=FALSE). The earlier Cairo export produced merged text-trace boxes in the collision auditor. Re-exporting through the standard device resolved those geometry findings: final collision audit has 0 failures and 0 warnings. Alignment has 0 failures and 0 warnings across all six comparison groups.

The provided low-level PDF font auditor reads only Tf operands and reports 1 pt for this R 4.6 PDF, ignoring the text matrices. Its unmodified report is retained in `pdf-text-audit.json`; it is not presented as a pass. A second audit uses PyMuPDF's effective rendered span sizes, including transformations: all 139 text spans are editable/extractable and the minimum effective font size is 8 pt, exceeding the 5 pt floor. See `pdf-text-effective.json`. No font threshold was lowered.

## Supplemental numeric and source checks

The original 190 outcome rows (185 successful builds) and 370 read processes are checked before adding any supplement. The 300k supplement contributes exactly 30 successful builds and 60 reads; all six source/backend combinations have five builds and each build two independent accesses. The 500k supplement contributes one build and two accesses, with no duplicated keys. Its exact prefix/query fingerprint and all row, block, subset-row and subset-block fingerprints agree with original HDF5/BPCells 500k evidence. Combined total: 221 outcomes, 216 successes, 432 accesses and 16416 raw timings. All durations and raw-summary relationships pass the repository timing validator.

All original five Human 500k preflight skips are preserved as exclusions, not replaced by the successful feasibility trial. No original measured observation was removed. No embedded full endpoint was invented. Archived supplemental inputs have their own SHA256 inventory, verified before each replot.

The post-exit service-result files may show unloaded transient-unit defaults (MemoryMax=infinity, MemoryPeak not set). They are preserved as captured, not used as evidence of the enforced run-time limits. The launch scripts, live systemctl readings during acquisition and final systemd-run logs establish 90/99 GiB enforcement, zero task swap, and a final 500k cgroup peak around 97.3 GiB. R-reported 500k process peak is 99503.41 MiB; it differs from the whole-cgroup peak by definition.

Visual review of final panels a-h: 300k placement and line joins are correct; all four Human 500k diamonds are at 500000, disconnected and without range bars; per-metric shared limits include every observation; full labels and x coordinates remain exact; tick/caption/legend separation and panel geometry pass. Small range bars remain intentionally unswollen.

The static source checker has 15 passes, 6 reviewed warnings and no failures. Its Cairo/SVG detections indicate supported optional code paths, not the chosen final export device or existence of an SVG deliverable. Final artifacts are PDF and PNG. Original results, including CURRENT files, were rechecked against all 691 backup hashes and are unchanged by this postprocessing.
