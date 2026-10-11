# PR0–PR6 and current Viewer integration audit

**Follow-up:** The initial mechanism audit below is historical. The later
dirty-work audit and `eb432a11` selectively integrated Spatial hidden-work,
Linked palette and picker cleanup, and HLA retirement fixes. Their exact
disposition is in `OLD_BRANCH_DISPOSITION.md`; final acceptance and retirement
are recorded in `FINAL_ACCEPTANCE.md`.

Audit date: 2026-10-10. Base: `perf/pr6-ui-accessibility-polish` at `cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303`. This branch descends from `perf/pr5-million-cell-final-optimization` at `f57a7c1a` and all PR0–PR4 tips. PR6 adds UI, accessibility, dataset presets and linked/spatial layout changes; it is not a pure presentation change. Historic benchmark numbers are screening evidence only under protocol v1.

## Branch inventory

| Branch | Tip | Actual contribution relevant here |
| --- | --- | --- |
| `perf/pr0-million-cell-crb-qs2` | `99d305c0` | Thin CRB, qs2, lazy large immune repertoire. |
| `perf/pr1-backend-hot-paths` | `35128c51` | Backend access and compatibility contracts. |
| `perf/pr2-million-cell-viewer-webgpu` | `5ad9ed40` | Large projection WebGPU and stable-ID selection. |
| `perf/pr3-startup-optimization` | `8f38ee56` | Lazy session initialization and BPCells attachment. |
| `perf/pr4-million-cell-page-performance` | `a78b5454` | Suspended inactive modules and page readiness. |
| `perf/pr5-million-cell-final-optimization` | `f57a7c1a` | Viewer Pack, canonical index, static resources, progressive specialist frames, geometry and GPU reuse. |
| `perf/pr6-ui-accessibility-polish` | `cbd65fe0` | PR5 inheritance plus accessibility, responsive layout, selector and preset fixes; linked/spatial drawing changed. |
| `卧槽/pr1_new_crb` | `41045238` | Thin CRB v2, fingerprint, safe clone, BPCells storage order. |
| `卧槽/pr2_viewer_ui_fixes` | `9565225b` | Selector and long legend fixes. |
| `卧槽/pr4_viewer_lifecycle` | `ad285b71` | Hidden HLA, RGB batching, HLA parsing, annotation and graph guard. |
| `卧槽/pr5_viewer_requests` | `ad285b71` plus dirty worktree | Same committed tip; uncommitted hover, selector, lifecycle and table fixes. |

## Keep

| Mechanism | PR6 state | Current state | Decision and evidence |
| --- | --- | --- | --- |
| Thin CRB v2, stable fingerprint, safe session clone | Present in `R/cerebro_io.R`, `R/class-Cerebro.R`, `inst/viewer/utility_functions.R` | Earlier implementation | Keep PR6; duplicate port would regress its Viewer Pack lifecycle. |
| Integer cell index and BPCells storage order | Present in `expressionProjectionCachedRows` and CRB runtime, with canonical order recovery | Present | Keep PR6 data path. Reject barcode round trips and whole metadata copies. |
| RGB batch read | One `expressionProjectionCachedRows` call for unique RGB genes | Current `b5b0c9a2` also batches | Keep PR6; current mechanism is duplicate. |
| Viewer Pack/static resources/validation reuse | Present in `R/viewer_pack.R`, `inst/viewer/core/viewer_pack.R` and static resource catalog | Current branch lacks Pack | Keep PR6 resource architecture; immutable geometry and descriptors are reused. |
| Geometry/GPU frame reuse | PR5 commits `65d83e18`, `c28ede3f`, `2c351281` inherited | Current slower path lacks it | Keep PR6; do not recreate geometry on color, selection, Linked or trajectory changes. |
| Trajectory canonical static frame | PR5 `9fab5c9c`, `bcb9b11a` inherited | Current has different page code | Keep PR6 pending final page check. |
| HLA distinct lineage scoring | Already `table(v)`/unique labels in `R/hla_typing.R` | `ad285b71` uses another distinct-label implementation | Keep PR6; no repeat experiment. |
| HLA metadata barcode join | `hla_annotate_ir_metadata` already concatenates sample barcodes and runs one `match()` | Current `860e07c5` independently introduces one batched match | Keep PR6; its active path already has the mechanism. |
| PR6 UI and long legends | Present after PR5 | Current branch fixes older layout | Keep PR6 unless page correctness reveals a regression. |

## Replace

| Mechanism | PR6 state | Current state | Decision and evidence |
| --- | --- | --- | --- |
| HLA segment parser | Per-sample output and repeated assembly | Current vector token parsing with bounded column concatenation | Port parser only. Ren 503,432 repertoire rows: exact TRA/TRB output; one Level-1 process had 1,907,080 KiB baseline vs 1,679,016 KiB candidate peak. Not a page median. |
| HLA oversized graph guard | Guard after wide aggregation | Current counts node keys first | Port small guard and key reuse; exact guard/text equivalence test. |
| Hidden HLA initialization | Some graph paths gated; mounted settings/parameter outputs remain eager | `f33af5e1` gates active page | Port missing output gates, preserve cached parsed data. |
| Projection hover formatting | `formatC` across 1M rows twice | Current dirty branch fixes optional QC fields but still formats whole columns | Format distinct counts once and restore order, plus optional metric handling. 1M micro: 40.555 s baseline, 8.007 s candidate, byte-identical 232,049,616-byte output. |
| Gene color wire precision | Sparse/dense f32 expression colors | Current uses R doubles | Retain sparse geometry protocol, encode expression colors as f64; preserve NA/zero and tiny nonzero differences. |

## Delete / do not repeat

| Experiment | Disposition |
| --- | --- |
| PR5 reverted shared codec/component attempts `3c876b6c`, `d115c04c`, `b3b58017` | Already reverted in PR6; do not reintroduce. |
| `sparse-f32-v1`/`dense-f32-v1` expression colors and 16-bit quantization | Remove precision-reducing color transport; geometry may remain f32 as a separate, established rendering contract. |
| Generic BPCells reader/chunking, HLA vector parser memory regression, graph formatter with no stable full-page gain | Do not cherry-pick or rerun from historic micro wins. A candidate needs Level-1 page/RSS evidence under v1. |
| Current branch's wholesale replacement of Pack/progressive runtime | Reject; `git diff --stat` against PR6 removes `R/viewer_pack.R` and thousands of JS/test lines. Port only mechanisms. |

## Add / verify

| Mechanism | State | Gate |
| --- | --- | --- |
| Dataset selector pending-choice/nonce handling | Current dirty `dataset-context.js` has a small improvement absent from PR6 | Port with matching server message and reconnect tests, then Level-1 dataset-switch check. |
| DataTables core collision | Current dirty tests identify second core registration in Gene ID/HLA tables | Port the narrow `style="bootstrap"` use in Gene ID and HLA tables. The HLA Data & QC UI now keeps its table hosts stable while the sample count updates as text; the HLA app contract passes. |
| HLA annotation batch join | PR6 already performs one match over the eligible samples in both package and bundled core | No port. Check its result as part of Ren correctness. |
| Spatial appearance/background fixes | PR6 already has changed linked/spatial layout; current branch differs | No Spatial code port by default. Run separate fixed grid only if a Spatial change is accepted. |

The dirty `卧槽/pr5_viewer_requests` checkout was inspected without modifying it. Its `serverSideGeneSelector` and request wrappers duplicate PR6's existing dataset-context/generation handling; PR6 keeps the simpler selector. The uncommitted `viewer-resources.js` adds broad runtime monkey patches and lacks an isolated page/RSS gain under this protocol, so it was not copied. Its palette-return and hidden-linked-work findings remain useful profiling leads, not accepted mechanisms. Current Spatial edits and its old full Viewer transport are likewise excluded. The narrow dataset pending/nonce and HLA table fixes were the only dirty lifecycle/UI changes ported.

## Integration plan

- **Base:** PR6 `cbd65fe0`; it has PR5 core and PR6 UI. Revert specific PR6 regressions only when page A/B proves them.
- **Remove:** f32 expression color transport and dead reverted experiments.
- **Port:** Current HLA parser, early graph guard, hidden-page gate, optional hover metadata, and proven lifecycle/data-table fixes.
- **Reimplement:** Distinct-value hover formatting in the existing helper; preserve exact text. Use PR6 canonical geometry and Pack.
- **Benchmark:** Fixed v2 WebGL2 protocol, 1M for general pages, Ren for HLA/expression, Spatial only if edited; one Level-1 pair before any Level-2 three-pair A/B. V1 preflight is retained separately.
- **Acceptance:** Exact cell/count/order/fingerprint/color/NA/zero/hover/selection checks, affected-page usable/complete/revisit, RSS and payload. Final Level 3 only for high-risk lifecycle or large wins.

## Final audit outcome

PR6 retains the complete PR0–PR5 ancestry, but inherited f32 expression-color wire values were a precision defect; the clean branch uses f64 for expression colors and retains the established f32 geometry contract. No reproducible PR6-versus-PR5 page regression was established under one frozen protocol. The 1M Level-1 HLA page screen showed no page-speed benefit from the Ren parser, so no three-pair page test was spent on it. Ren parser's exact-output three-pair targeted gain is accepted for the real-data fallback. Projection hover's exact-output CPU gain is accepted as a hotspot fix, without claiming a Viewer Pack first-page gain. The Gene-entry and Linked secondary paths still have measurable cost. A five-pair final v2 A/B finds Projection/Gene entry latency essentially unchanged versus PR6 and no obvious peak-RSS regression in a separate resource pair; see `RESULTS.md` for the raw identities and scoped observations.
