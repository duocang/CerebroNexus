# Repeated Viewer work: bounded fixes

Date: 2026-10-11. Branch: `perf/pr6-ui-accessibility-polish`.
Baseline: `d40e785a409f8d3bede26cba0e54114ff947432c`.
Candidate: the source changes committed with this report; smoke source hashes
are in `smoke/source.csv`. No branch/worktree creation or push.

This implements the six findings in `../repeated-work-audit/RESULTS.md` after
the user requested all of them. It does not reopen the closed Viewer performance
topics or change statistical definitions.

## Changes

| Finding | Change | Boundary |
|---|---|---|
| SizeDist repeats profile construction on linkage changes | Keep the current compact profile in one dataset-dependent reactive; rebuild only for data/chain/group/threshold changes. Remove ignored clone-call cache dependency. Load scRepertoire only in the small-data analytical fallback. | Strict-clone definition and existing 100,000-row native/fallback boundary unchanged. No multi-key profile cache. |
| IR controls reset on remount | Retain finite numeric values within control bounds, logical values including false, and strings including empty text. | Valid analysis preferences can persist across datasets, as selects already do. Analysis results remain dataset-scoped. Threshold interpretation is unchanged. |
| Same-field tab changes repeat metadata joins | A control-only observer updates a reactive value only when requested fields change. | No eager repertoire/metadata read. Actual dataset, grouping and requested-field changes still invalidate. |
| Groups repeats counting for display variants | Share one current contingency table across bar, Sankey, percentage and optional table outputs. | Existing count/order/NA/factor semantics preserved; presentation formatting stays separate. |
| Paired Scatter wrangles unused samples | Pass only x/y samples plus any third dot-size sample. | Invalid selection names retain existing fallback behavior. No scientific calculation changes. |
| Table formatter scans unused classifications | Skip numeric-value classification only when both number formatting and color highlighting are off. | Numeric values, alignment and remaining table behavior preserved. No unmeasured speedup claimed. |

## Quantification

Environment: WSL R, Ren 1,462,702-cell CRB with 194 immune-repertoire samples.
Same loaded raw repertoire used for both implementations. Candidate compact
profile prepared once, then three interleaved baseline/candidate linkage changes.
Timing includes profile preparation where applicable and ggplot building, not
serialization, browser paint or whole-page load. No ready definition changed.

| Linkage | Baseline ms | Candidate ms | Plot-layer equality |
|---|---:|---:|---|
| ward.D2 | 2059 | 148 | TRUE |
| complete | 2362 | 105 | TRUE |
| average | 1745 | 92 | TRUE |
| Median | **2059** | **105** | |

The measured repeated operation is **94.9% faster**. First entry still performs
the necessary profile preparation. The retained candidate object is 30,120
bytes; named clone counts are discarded from the retained result. This does
not eliminate their temporary allocation during initial preparation and is
not a browser-heap or process-RSS claim.

The reactive regression reproduces the audit's four same-field tab states:
annotation calls fall from four to one. Group/threshold/dataset changes still
refresh their respective results. Groups display variants likewise use one
counting invocation and refresh after dataset change. Paired Scatter's earlier
small timing benefit remains bounded by the audit; no new full-page claim.

Raw evidence: `quantify.R`, `quantify.log`, `timings.csv`. Baseline functions are
read using `git show` without changing the checkout.

## Validation

`run-tests.R`: **593 assertions passed, 0 failed, 0 skipped, 0 warnings**.
Evidence: `tests.log`, `tests.csv`.

Coverage includes reactive invalidation, dataset changes, actual IR control
construction, native/fallback renderer gating, exact SizeDist plot geometry,
Groups count/percentage/wide results, third-sample scatter dot sizes, formatting
feature gates, existing IR/Viewer Pack compatibility and result-page controls.

Real Windows Brave, default viewport 1424 x 1483, served by WSL R at local port
8947 using the existing `../ir-latency/server.R` harness and passive probe.
This is a functional smoke run, not browser performance A/B. Ren and packaged
Demo were used. Screenshots and DOM snapshots accompany this report.

- Ren SizeDist displayed a real dendrogram; changing ward.D2 to complete worked.
- Clone-call change followed by Homeostasis/Abundance navigation worked.
- Custom Homeostasis thresholds `0.0002, 0.002, 0.02, 0.2, 1` survived leaving
  and returning. Abundance's checked proportion option survived remounting.
- Ren to Demo switched to 1,476 cells. Groups bar, Sankey and optional table
  displayed Demo samples only, with totals 481/508/487. Percentage view showed
  matching ratios (sample_1 first category 105/481 = 21.83%).
- Returning to Ren restored its 1,462,702-cell dataset and displayed the Ren
  SizeDist dendrogram again with complete linkage; no Demo sample labels leaked.
- Captured browser warning/error list is empty (`browser-console.json`). No new
  R error occurred. Test-only tab and local service were closed after acceptance.

Evidence includes `sizedist-brave.png`, `groups-brave.png`, `*-controls.txt`,
`groups-*.txt`, `smoke.log`, and `smoke/browser.jsonl`. Passive Abundance/
Homeostasis timings in the latter are incidental and not used as performance
conclusions: the passive probe also carries a pending click across dataset
navigation, so its long mixed-session interval is not an operation benchmark.
The R log contains existing Set2 palette-size warnings for Ren;
these are not reported as a warning-free R session.

## Scope and closeout

No changes to expression precision, reader, RGB, geometry, wire protocol,
generation/epoch framework, ready/paint confirmation, RAF or BPCells. No new
full-column metadata expansion, static resources or startup payload. Existing
Gene/Linked/Trajectory/HLA/Spatial stop decisions remain in force. No attempt
to claim all Viewer pages are faster, or to resolve the pre-existing upstream
strict/TRB analysis limitation.
