# Additional quantification and remaining costs

2026-10-11. Current production SHA:
`a85b00775ec995660fae3293a57004eadcb20394`.
Comparison source: `d40e785a409f8d3bede26cba0e54114ff947432c`.
Existing branch/worktree; no production edits, checkout changes or push.

## Method

`../quantify-more.R` reads function definitions from both revisions without
changing the checkout. WSL R 4.6.1, scRepertoire 2.9.3; Ren 1,462,702 cells,
194 repertoire samples / 503,432 receptor rows. Same local CRB used in prior
reports. The small real result-table fixture is the packaged Demo's 3,213 x 8
stored marker table. A separate 100,000 x 5 table is explicitly synthetic.

All new measurements are elapsed **R function/operation timings**, using
`nanonext::mclock()`. No browser was timed, no ready gate changed, no new profile
or full-Viewer benchmark was run. Three sequential baseline/current pairs per
comparison, with baseline first. They are bounded diagnostics, not randomized
trials or formal browser A/B. Initialization/GC/system scheduling can affect
individual values; outliers are retained. Initial CRB/sidecar/namespace reads
are single fresh-process observations, not cold OS/file-cache medians.

## Quantified changes

Milliseconds. Raw records: `timings.csv`; summaries: `medians.csv`.

| Operation | Before median | Current median | Interpretation |
|---|---:|---:|---|
| SizeDist linkage redraw, preparation + ggplot build | 2059 | 105 | Previous three-pair evidence in `../timings.csv`; 94.9% less elapsed time. Not rerun or pooled here. |
| Same-field IR tab change demanding annotated data | 202 | 2 | Actual reactive expressions; baseline 193/462/202, current 2/3/2. Four demanded tab states including initial demand invoke annotation 4 vs 1 times. |
| Groups wide presentation after counts already exist | 29 | 8 | Baseline recount+pivot 29/52/22; current format-only 16/8/8. First required count remains. |
| Paired Scatter ggplot object, gene/both/first two samples/total dot size | 68 | 63 | Baseline 75/68/66; current 57/306/63. Only 5 ms median difference; **no stable speedup claim** with this outlier. |
| Demo stored marker table, formatting and coloring off | 21 | 7 | Baseline 21/183/6; current 7/356/5. Large outliers in both arms: **not evidence of a reliable end-to-end improvement**. |
| Synthetic 100k-row table, same disabled features | 26 | 8 | Baseline 19/29/26; current 8/7/9. 18 ms median saving on a labelled stress fixture, not a real result-page benchmark. |
| IR control retention | Not timed | Not timed | Correctness fix. No attribution of seconds saved without an actual remount/recalculation trace. |

Output checks pass: metadata values identical, Groups wide tables identical,
Paired Scatter built plot layers equal, table data/options equal with equivalent
alignment callbacks. The first diagnostic stopped because it required byte-
identical DT callback strings. Inspection found only the order of independent
per-column alignment clauses differs. The harness now compares sorted clauses
and requires all other widget content to be identical. `run.log` preserves that
initial harness failure; `tables.log` is the successful table/remaining-analysis
continuation. The incomplete first table pair is replaced in `timings.csv` by
the complete continuation series; earlier completed measurements are retained.
This required **no production repair**.

## Remaining costs at current HEAD

| Stage / condition | Measured ms | Interpretation |
|---|---:|---|
| Read Ren CRB in a fresh process | 8752, one observation | Dataset-open cost, not a cost on every analysis tab. Includes work inside `readCerebro`; package load is excluded. |
| First complete repertoire sidecar hydration | 5438, one observation | Still needed by raw analytical paths such as SizeDist and Diversity. Existing packed default Abundance/Homeostasis avoid this requirement. Earlier fresh-worker reader checks already rejected hard-coded threading; no retry here. |
| First scRepertoire namespace load | 3031, one observation | Only once per process if a path needs the package. Native SizeDist and packed default paths no longer force it. |
| SizeDist exact profile construction | median 1622; 1576/2892/1622 | Required on first demand and real data/chain/group/threshold changes; linkage-only changes reuse the compact result. |
| SizeDist plot-object construction | median 74; 355/74/63 | Includes first invocation overhead; this is before ggplotly conversion. |
| SizeDist ggplotly conversion | median 119; 151/119/116 | Includes its own plot build/layout; not the same timing boundary as the preceding linkage A/B. |
| SizeDist Plotly JSON serialization | median 16; 39/15/16 | JSON was 206,472–206,474 bytes. This is JSON text size, not measured total WebSocket/network transfer. |
| Homeostasis gene / both chains / severity grouping | **median 3945; 5441/3945/3748** | Current unchanged scRepertoire fallback on already-loaded, pre-annotated raw data. Plot creation included; ggplotly/JSON/browser excluded. This is the clearest remaining multi-second analytical cost. |
| Diversity strict / both / 20 bootstraps, exported table | median 268; 317/261/268 | Data and namespace already loaded. Fixed seed per invocation; no uncertainty calculation removed. Not the complete page renderer or proof that all Diversity settings are cheap. |
| getMetaData after readCerebro | 0 at millisecond resolution | Already-loaded access, **not** evidence that initial metadata decoding is free. |

These stage timings are not an end-to-end click latency budget. Do not add
single first-process samples to different warm medians or to prior browser
results. Exact already-cached full plots can avoid underlying work altogether;
the measurements force computation and do not show every revisit recomputes.

The previous genuine Brave A/B remains separate: default Abundance painted
median 7375.1 -> 3276.7 ms; default Homeostasis 4339.2 -> 2326.1 ms, measured
before this follow-up and documented in `../../ir-latency/fix/RESULTS.md`.
Those values are not new current-HEAD browser measurements and do not describe
the non-default severity-grouped path above. Browser frame acknowledgement can
still add waiting; the existing RAF investigation remains closed.

## Disposition

1. Highest concrete remaining analysis candidate: severity-grouped Homeostasis.
   It needs its own short profile/equality experiment before selecting an exact
   grouped-count implementation. The current default histogram path does not
   cover this grouping. Do not claim its 3.945 seconds has been fixed.
2. First raw-data demand remains costly. Shifting hydration to startup merely
   moves the wait. Adding prepared summaries for more analyses would require a
   separate dataset/pack design decision; no new cache or export contract here.
3. Groups, Paired Scatter and table classification are small costs in these
   fixtures. No evidence justifies a large shared-layer rewrite.
4. Marker, Most expressed, Enriched and Extra use stored results; their rendering
   and formatting costs are not evidence of rerunning upstream analysis. No new
   seconds-scale computational bottleneck for those pages was established here.
5. No reopening of Gene/Linked/Trajectory/HLA/Spatial optimization. No tests of
   unrelated pages, no automatic benchmark continuation, and no production code
   changes in this quantification task.
