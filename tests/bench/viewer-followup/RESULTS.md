# Focused Viewer acceptance — 2026-10-10

## Latest bounded closeout (production baseline `4d8e96de`)

See [the final Gene / Linked / Trajectory closeout](gene-diagnosis/CLOSEOUT.md)
and [the preceding painted-readiness acceptance](brave/stability/RESULTS.md).
The historical results below are retained with their original conditions.
The final round made no production change: dimension-only prewarm failed the
second-scale gate, independent Brave RAF controls also reproduced ~1 Hz, and
no new Linked/Trajectory candidate justified another full A/B series. Stable
Gene <=3 seconds remains INCONCLUSIVE. Do not compare the final mixed-session
diagnostic directly with the earlier fresh-page or draw-only timings.

Production candidate: `af738383672b72121cb63d3ef9f025dab95720ce`. Baseline: `e8662e47a884bd07f7483dd02328d4ebbcd34573`. Branch remains `卧槽/pr6_clean_perf`; no new branch or worktree.

## Gene final gate

**ACCEPTED** for first-operation latency, exact values and bounded resource use. Five interleaved B/C pairs; fresh R/browser profiles and warm filesystem, 1M Mouse only. This is the fixed Chrome/SwiftShader series, not cold OS, Brave or hardware GPU.

Harness SHA-256: `b22e263ce5edb7f1c10bb9619e6cd9a6049088a79360097661d1ce1551d46b38`. Dataset SHA-256: `cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38`.

| Metric | Baseline median [min, max] | Candidate median [min, max] |
| --- | ---: | ---: |
| First usable (s) | 7.038 [6.862, 7.296] | 2.496 [2.319, 2.522] |
| Visible complete (s) | 7.559 [7.339, 7.771] | 3.344 [3.280, 3.538] |
| R peak (MiB) | 1,893.027 [1,892.688, 1,893.168] | 1,910.641 [1,880.434, 1,936.785] |
| Browser tree peak (MiB) | 2,062.730 [2,059.992, 2,079.496] | 2,041.512 [2,038.117, 2,057.570] |
| Operation websocket bytes | 23,256,460.000 [23,256,456.000, 23,256,460.000] | 1,054,940.000 [1,054,940.000, 1,054,940.000] |
| Messages | 150.000 [150.000, 150.000] | 80.000 [80.000, 80.000] |

First usable is 64.5% lower (2.82x faster); visible complete is 55.8% lower. R peak median increases 17.6 MiB (0.9%; worst observed candidate peak is 43.6 MiB above baseline maximum), while browser peak median falls 21.2 MiB. No extra cache or precision tradeoff was introduced. The later long-session check is separate from this peak screen.

The 22.2 MB identity core is deferred until an actual point hover or brush, not discarded. First-operation traffic drops 95.46%; eventual identity/hover traffic still exists when requested. The 873,212-byte Gad1 primary color message remains lossless f64.

### Accepted changes

- Request Gene stable IDs on point hover or brush instead of empty-frame activation. The pending-hover guard allows that first request while content is unavailable, then restores the existing hover-disabled behavior.
- Preserve the existing recolor shortcut when the same progressive frame has unchanged coordinate references, wire token and point count before IDs arrive. Legacy/mismatched frames retain fallback guards; pending selections are reported only after stable IDs arrive.
- Render the empty selection counter from existing validated first-frame metadata. Nonempty selections still use full metadata and preserve the exact summary.

No production reader, precision, wire codec, Viewer Pack, HLA, Spatial, generic Projection or Trajectory implementation was changed.

### Correctness

**ACCEPTED**: focused R suites cover exact dense/sparse f64 values, NA, zero, duplicate gene, Mean, Separate, RGB and dataset-selector behavior. Browser fixtures cover pending IDs, actual-point demand, deduplication, hover-disabled behavior, stale-frame cancellation, unchanged geometry, exact pending-frame values and guarded fallback.

On the real 1M fixture, the browser reconstructed all Gad1 values and matched the backend f64 byte SHA-256 `bc287a68bddfc08d9f2155270a9f0b18fdfa827ac9b6652485c60b90fa060020`. Actual hover recovered the exact barcode and counts. Native browser mouse drag selected 447 distinct IDs in canonical order. The final frozen interaction probe exited successfully.

## Gene stage profile

**INCONCLUSIVE** for an additive reconstruction of the historical 7.14 s sample: that old run did not contain operation-stage probes. New diagnostic traces are kept separate from the five-pair gate. R elapsed/wall time showed a clock step relative to browser performance.now in one diagnostic run; never use its cross-clock subtraction as transfer or queue time. The accepted diagnostic v3 uses `nanonext::mclock()` and start/end browser handshakes. Their offset intervals intersect within 4.9 ms. Its instrumented first usable is 7.604 s; this is explanatory evidence, not a replacement timing series.

| Non-overlapping phase in the v3 baseline diagnostic | Duration (s) |
| --- | ---: |
| Click to websocket send | 0.008 |
| Send to R input observer (transport plus queued R work) | 2.975 |
| Input observer to actual expression preparation | 1.218 |
| Expression preparation | 2.058 |
| Expression ready to browser color decode start | 0.222 |
| Browser primary color decode | 0.002 |
| Apply/color/draw through primary usable event | 1.121 |
| Total | 7.604 |

Nested costs must not be added to that table. Pending empty-selection metadata work takes 1.795 s total, of which about 0.567 s overlaps the click wait. A startup one-cell warmup takes 1.569 s (1.498 s in backend/cell access, 0.058 s in its BPCells slice). After the input arrives, automatic identity-core preparation/send takes 1.064 s including 0.819 s wire packing; it sends 22,194,382 bytes and browser decode takes 0.115 s, overlapping R expression work.

Actual expression preparation includes 1.224 s in `viewerExpressionCells` and 0.700 s in row access/materialization. The full canonical selection is normalized to NULL by the compatibility path; it retains backend/column-name overhead but does not perform a redundant barcode match/reorder on this NULL path. There is one full Gad1 row read plus the separate startup one-cell warmup, not two full Gad1 reads. One dense numeric row is necessary; no all-gene dense matrix is materialized.

Color settings take 0.029 s; primary update 0.097 s includes sparse f64 preparation 0.022 s, recolor send 0.030 s and wire packing 0.010 s. Primary geometry remains unchanged and the operation sends a recolor. Full hover text is not rebuilt. Decode is directly measured; the 1.121 s browser interval includes apply/color/draw and event frames, not isolated GPU drawing. Serialize and network transfer are not independently identifiable below the measured packing/receive boundaries.

Primary acknowledgement arrives at 7.614 s. Summary data takes 0.043 s starting at 7.626 s and expression-by-group chart preparation takes 0.220 s starting at 7.757 s. These are already after primary usability. Empty selected summaries and hidden secondary charts do not read another expression row. Thus visible secondary charts explain part of complete, but are not the dominant first-usable blocker. The accepted patch removes premature identity and empty-counter work, while preserving this existing primary/secondary boundary.

## Repeated operations

**ACCEPTED** for bounded repeated operation correctness; **INCONCLUSIVE** for improving its one-time range-computation tail. The single resource pair is not a formal multi-run performance claim:

| Operation | Baseline (s) | Candidate (s) |
| --- | ---: | ---: |
| Second gene | 1.942 | 1.958 |
| First cached return | 0.956 | 2.177 |
| Mean, three genes | 1.799 | 1.909 |
| Separate, three genes | 1.168 | 1.161 |
| RGB, three genes | 2.038 | 2.130 |

A bounded alternating-cache follow-up reproduces the first cached-return cost (B 1.002 s, C 2.310 s). Subsequent returns are B 0.954–1.092 s and C 0.951–1.065 s. A separate sampling-only candidate run is 2.315 s; 134 of 226 10-ms CPU samples are in `expressionValueRange -> range -> .rangeNum`. No BPCells read appears in the sampled cached path. The exact reason this range call has a one-time allocation/runtime cost is not isolated; this evidence does not justify another cache or reader rewrite. This tradeoff is retained explicitly, not removed as an outlier.

After all six operations, R/browser RSS is B 2,119.36/2,045.51 MiB and C 2,005.51/1,928.73 MiB. RGB operation traffic is B 27,035,110 and C 4,840,633 bytes because core identity remains deferred; RGB values/read mechanism are unchanged.

## Linked completion tail

**INCONCLUSIVE** for improvement; no Linked production patch retained. A single instrumented actual-browser diagnostic explains the tail, but is not pooled with the old 1.82/6.81 s observation or the uninstrumented confirmation.

- Progressive supplement preparation: 3.481 s inclusive. Fallback builder: 3.448 s, including canonical metadata preparation (argument evaluation includes `getMetaData`) 2.256 s and trajectory mapping 1.122 s. The compact-only shortcut correctly declines this trajectory-bearing dataset.
- Supplement send: 0.358 s including wire packing 0.276 s. Its lossless payload is 16,002,272 bytes; browser decode 0.188 s and reported decode-to-draw 0.525 s.
- A later clone supplement is 601,240 bytes, decode 0.005 s and decode-to-draw 0.251 s. Total observed websocket traffic through the diagnostic is 16,612,887 bytes / 58 messages.
- Primary uses static 8 MB coordinates and 1 MB metadata resources. Server supplement excludes the existing primary UMAP (`setdiff`); the large extra coordinates belong to supplementary spaces. Browser structural merge rebuilds spaces and reports six position uploads across the whole diagnostic. Small color patches exist (largest traced preparation 0.057 s); they are not another full primary wire payload.
- Diagnostic primary/first complete events are 3.905/9.858 s; later redraws continue through 12.738 s. These instrumented timings demonstrate the distinction between first usable, complete and subsequent messages, not a production performance regression estimate. No visible errors or stale dataset event appeared.

A narrower metadata/trajectory builder or lazily loaded secondary spaces could reduce this cost, but would change a mature multi-modality path and its lifecycle. There is no small, verified winning candidate here. A new protocol/cache rewrite is outside this focused pass. Uninstrumented frozen-harness confirmation: first usable 2.275 s, complete 7.828 s, revisit 0.048 s. Correctness/visible pixels pass, but the strict 2 s first-visit budget fails (exit 1). This single sample does not establish a regression or gain relative to historical 1.82/6.81; the production Linked path is unchanged. There is no measured Linked improvement to claim. Post-primary websocket traffic is 16,605,484 bytes. The diagnostic and confirmation independently identify the same supplementary-data tail.

## Trajectory

**ACCEPTED** for the tested point-count/mapping and revisit correctness; **INCONCLUSIVE** for meeting the historical first-visit target. The frozen one-page harness measured first usable 3.355 s, complete 3.356 s and revisit 0.093 s. First visit fails its 3 s budget (harness exit 1), while correctness passes: 981,807/981,807 expected renderable cells, `marker_guided / E18_neurogenesis`, matching fingerprint and visible pixels, no renderer errors. This failure is retained.

The observed first path has 2.867 s server preparation, 0.306 s serialize/transfer and 8,838,651-byte primary payload. It is a dynamic frame: current default color selects a metadata grouping, whereas `trajectory_static_first_frame` deliberately requires `State`. Thus this default path does not exercise the historical state-frame timing. `git diff cbd65fe0 -- inst/viewer/trajectory` is empty: canonical geometry, excluded indices and static frames remain present, and no Trajectory production code was changed in this pass. The fixture manifest contains the correct canonical UMAP and 18,193 excluded indices. Do not compare the default dynamic-frame sample to historical state-frame 1.5/0.6 s as an identical A/B.

The bounded session confirms default grouping after Gene at 3.205 s. Switching to State yields a 3,248-byte primary frame with `canonical-projection-v1`, `exclude_uint32`, a projection cache hit, 72,772-byte excluded-index resource and 981,807 state codes/points. That proves the mature mechanism is active under its intended condition. However the real switch takes 5.887 s because the previous dynamic frame's auxiliary result arrives at 5.669 s first. Inherited event `serverPrepareMs` refers to the older request and is not this switch's preparation time. Neither this operation nor the default first visit establishes the historical 0.6 s warm target. This existing auxiliary queue/default-color distinction is a remaining bottleneck, not a reason to replace the canonical/static architecture.

## Bounded session and reconnect

**ACCEPTED** for completed finite page/dataset operations and stale-context checks; **INCONCLUSIVE** for a general leak-free claim. The run completes Gene/Trajectory/Linked visits, repeated genes, B→A→B→A dataset changes, a seeded three-cell Linked selection reset, exact dataset fingerprints/generations and rejection of prior contexts. It then reloads into a new epoch and correctly loads A; the controller's `wait_for_idle` fails after document reload. The separate event-based recovery probe below covers the remaining Gene render without repeating the finite session.

| RSS (MiB) | Start (small B loaded) | Peak across run | End before disconnect (1M A loaded) |
| --- | ---: | ---: | ---: |
| R | 1,330.043 | 2,398.742 | 2,368.223 |
| Browser process tree | 1,733.637 | 2,407.117 | 2,304.363 |

R does not return to its small-dataset startup RSS after loading the million-cell dataset and all tested views. It falls from 2,121.031 to 2,087.883 MiB during the first pass, then is approximately 2,284 MiB through B and 2,356–2,368 MiB during the final A operations. Browser RSS falls from 2,352.102 to 2,218.277 MiB on return to A; after reload/data readiness it is 1,965.457 MiB. The observed sequence is not continuously one-way growth, but RSS alone cannot separate retained live objects from allocator high-water marks.

No old dataset becomes active or retains the seeded selection. Source review shows the four-row identity-keyed Gene LRU remains bounded. Browser immutable resource Maps survive dataset invalidation, while view state/descriptors are reset; this finite two-dataset run does not prove an upper bound across arbitrarily many distinct datasets. No heavy heap profiling was added.

An immediate-input diagnostic found that a gene choice issued while a revisited selector restores can be overwritten by its prior value. The settled-operation run waits for a real primary/cached frame before allowing control restoration to finish. This narrow input window remains a known lifecycle limitation; do not describe the entire lifecycle as unconditionally passed.

The separate frozen `reconnect.R` probe exits 0: reload creates a new epoch, A/Gad1 renders exactly 1,000,000 points with the expected label, and the old epoch is rejected. Its RSS/timings are not pooled with the finite-session run.

Standalone Shiny reports `allowReconnect=false`: same-session resumable reconnect is **BLOCKED** by the launch environment. Reload into a new epoch is a separate recovery operation, not resumable reconnect.

## Rejected directions

| Candidate | Evidence | Conclusion |
| --- | --- | --- |
| Gene cache tries integer BPCells entry before compatibility normalization | Three pairs: B usable 7.356 [5.028, 8.287] s; C 6.744 [6.585, 8.135] s. Paired improvements +0.772, +0.152, -1.717 s. No stable whole-operation gain despite initial screen. Patch fully reversed. | REJECTED |
| Initial lazy-core draft | A real hover test exposed the preexisting early hover-disabled return blocking the first core request. The accepted version fixes this and has native hover/selection proof. Draft timings are not pooled with the gate. | REJECTED |
| Precision reduction, f32 wire, 16-bit quantization, generic reader/chunking, shared codec rewrite | Not attempted. Mature mechanisms and prior rejection decisions are preserved. | REJECTED |

## Evidence identity and limits

The same checkout was restored sequentially from exact baseline/candidate snapshots. Raw HEAD is e8662e47 for gate pairs 1–3 and af738383 for 4–5; per-run source hashes and the extra counter/browser source sidecars identify the actual arm. All ten runs share one harness hash, dataset hash and browser version. No tests or other benchmarks ran concurrently.

Full diagnostic traces, rejected patch and original runner snapshots are retained under `C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-10-pr6-followup`. The compact evidence manifest is added with this report. The existing final protocol supplies host/CPU/RAM, BPCells layout, CRB schema and static-asset identities. No Ren, Spatial, HLA, Groups or Immune benchmark was rerun.


## Final disposition and remaining work

- **ACCEPTED**: production `af738383` — three narrow Gene changes; final five-pair first-operation gate and exact-value/native interaction checks.
- **REJECTED**: integer-reader entry experiment (inconsistent full-operation gain), initial hover-blocked draft, and any precision-lowering or broad rewrite direction.
- **INCONCLUSIVE**: Linked improvement (no patch), default Trajectory first-visit target, one-time cached-return range cost, and arbitrary multi-dataset retained-resource bounds.
- **BLOCKED**: same-session resumable reconnect in standalone Shiny; supported reload recovery is **ACCEPTED**.

Worth further work only with a separate scoped task: the first cached-return range/allocation cost; Linked metadata/trajectory supplement and browser space rebuild; non-State Trajectory preparation/auxiliary queue; the immediate-input selector restoration window; and an explicit bound for immutable browser resource caches across many different datasets. No evidence justifies another optimization pass over Spatial, HLA, generic Projection, geometry codecs or RGB reads.

`evidence/EVIDENCE_SHA256.json` hashes the included raw records and frozen harnesses. `lifecycle.R` is the executed finite-session harness, including its documented post-reload controller limitation; use `reconnect.R` for the completed event-based recovery check. The production commit is independent of these diagnostic scripts. No production changes were made after the five-pair candidate was frozen.
