# Remaining Viewer hotspots — 2026-10-10

Starting commit: 28444813. Same branch/worktree. Native Windows Brave, WSL R;
not comparable with historical Chrome/SwiftShader timings. See PROTOCOL.md and
gene-summary.json. No broad Viewer audit, Ren, Spatial, HLA or Projection sweep.

## Gene: ACCEPTED relative improvement; INCONCLUSIVE stable 3-second target

One production call changes from debounceAfterFirst(raw, 50) to
shiny::debounce(raw, 50). The upstream 200 ms gene-selection debounce, exact
expression reads, four-row cache, rendering, geometry, wire format, primary
acknowledgement and secondary-output boundary remain unchanged. This removes
an extra prepared-value relay observer. Rprof attributed 137 ten-ms samples to
observeEvent(delayed()) between expression readiness and primary preparation;
the precise internal share of equality comparison versus observer evaluation
was not independently isolated.

Three interleaved pairs: gene-L1-B-review / gene-debounce-C, pair2-B/C, pair3-B/C.

| Metric | Baseline median [min,max] | Candidate median [min,max] |
|---|---:|---:|
| First usable, ms | 4020.3 [3650.9,4125.0] | 2227.0 [2086.1,3620.1] |
| Visible complete, ms | 4767.9 [4397.9,6121.8] | 3725.8 [2834.3,5682.7] |
| R process peak, MiB | 1954.23 [1948.27,1956.03] | 1960.59 [1932.64,1967.18] |
| Browser JS heap peak, MiB | 147.74 [145.67,159.41] | 146.66 [129.60,157.47] |
| Operation websocket bytes | ~1052884 | ~1052881 |
| Expression ready -> primary preparation, ms | 1461 [377,1598] | 86 [85,95] |
| R input -> primary send end, ms | 2672 [1715,2815] | 1426 [1363,1439] |

All three paired first/complete results improve. Paired median first is 44.6%
lower; complete 21.9% lower. R median peak rises 6.36 MiB (0.33%), with overlapping
ranges; no material memory increase is established. JS heap is not browser RSS.
All-Brave RSS is sampled for pair 2 only and includes unrelated tabs; it cannot
establish an isolated Viewer-process peak. Small websocket differences include
control/header messages; expression precision and primary payload are unchanged.

Two additional fresh-process final candidate confirmations complete five candidate
runs, not five paired A/B runs. Their first times are 4651.6 and 5352.4 ms; complete
6714.0 and 7384.9 ms. Across all five candidates first median/range is **3620.1
[2086.1,5352.4] ms**, complete **5682.7 [2834.3,7384.9] ms**. Do not hide these slow
samples or compare the later five-candidate median as if it were paired with the
earlier three baselines. The improvement is retained; consistently <=3 s is not
achieved. Entry-to-input timing is recorded per run; immediate inputs can queue
behind the existing first-page one-cell backend warmup. No cached Gad1 operation
is substituted. Native frame cadence also varies (some ~1 s frame gaps) despite
foreground confirmation; its cause is not established and readiness was unchanged.

### What happened to the reported seven seconds?

This exact Brave baseline did not reproduce a seven-second primary; it reproduced
3.65-4.13 s in the paired baselines, with longer complete and slow final-candidate
samples. There is no defensible additive reconstruction of a different historical
seven-second observation. A representative baseline (gene-L1-B-review) is:

| Stage | Measured cost / finding |
|---|---|
| Physical key/click -> browser input | Not independently captured; stimulus is first Gad1 input event |
| Browser input -> R input / queue | Clock-offset interval only where a valid probe exists; no exact network split |
| R input -> row-cache preparation | 312 ms, includes selection scheduling/debounce |
| Row cache / expression access | 858 ms inclusive, of which viewerExpressionValues 847 ms |
| Expression ready -> primary preparation | 1461 ms; 137 CPU samples in delayed-value relay observer |
| Primary build/recolor/send | 41 ms inclusive; sparse f64 preparation 17 ms, wire packing 7 ms |
| Remaining input-to-painted interval | ~1348 ms combined queue/transfer/browser/paint; not all CPU |
| Primary first usable | 4020.3 ms |
| Expression-by-group preparation | After primary acknowledgement, 153 ms in plotExpressionSummary |
| Visible complete | 4767.9 ms |

The biggest measured blocks are the prepared-value relay observer, the combined
browser/transport/queue remainder, and expression-row access. In the slow final5
candidate R input -> send is still only 1494 ms: first-page backend work before
input handling and subsequent paint/acknowledgement account for the remaining
interval. The trace contains a separate 1304 ms one-cell prewarm. Do not add that
entire prewarm blindly to the operation if it started before the user's input.

Dataset/generation checks, descriptor/projection lookup and color setup remain
small/reused; no million-barcode rematch/reorder or geometry rebuild is introduced
by a gene recolor. The canonical full-selection compatibility path normalizes to
NULL; the prior rejected integer-reader experiment was not repeated. One full
Gad1 row is materialized, plus the separate startup one-cell warmup. Summary uses
the same reactive values. One dense numeric row is needed, not an all-gene dense
matrix. Hover IDs/text are requested on interaction, not built for 1M cells before
primary. Statistics/by-group/selected outputs are after primary acknowledgement;
hidden outputs are not silently counted as complete. Decode, field application,
GPU upload and first draw are not independently separable from inherited recolor
timing fields in these runs; those old page-scoped fields are excluded. No claim
uses unsynchronized Windows/WSL wall-clock subtraction as transfer time.

### Correctness and rejected candidate

R suites pass: exact dense/sparse f64 transport, NA, zero, duplicate gene, Mean,
Separate, RGB, selector/dataset behavior, pending IDs, stale frame and incremental
order/fallback checks. One R test wrapper expected an obsolete success string;
the JS fixture itself passed, and the wrapper was corrected and rerun successfully.
Actual Brave shows 1M Gad1 points (range 0-10), Mean(Gad1,Slc17a7) 0-5, two Separate
panels with 1M points each, and RGB Gad1/Slc17a7/Snap25. A real brush selects 717
cells. Hover identifies ACGAGGAAGTGCGTGA-29 (4121 transcripts,1875 genes). Return
retains RGB and the 717-cell selection; switching to Demo shows 1476 points,
empty expression and zero selection. Old tooltip text can remain in a hidden DOM
node; screenshot inspection confirms it is not displayed over the new dataset.

Removing the one-cell prewarm was **REJECTED**: first appeared faster, but moved
~1.18 s of initialization onto expression preparation and worsened complete to
7.14 s. No stable causal benefit was established. Its patch is retained as evidence,
and the production warmup is restored. No reader, precision, geometry or other
Viewer page experiment was bundled into the accepted Gene change.

Instrumentation limitations: earlier empty selector event values were retained by
reference in the harness and later mutated by Selectize. Only the unique operation
containing the exact Gad1 render key, corroborated by R gene_input, is used. Some
clock responses cannot match fractional IDs after JSON numeric rounding; those
probes are excluded. These issues do not change browser-local operation timings.
Frozen executed harnesses are gene-frozen-server.R and gene-frozen.js. Final5's
first-operation evidence is preserved in .first.json before correctness operations;
its later R peak includes those operations and is excluded from first-operation
resource statistics. Profiling samples are diagnostic, not a complete allocation
or transport decomposition.

## Linked: ACCEPTED for payload and geometry work, not latency (Level 1 only)

Baseline f4e4fe3b; candidate is the following Linked commit. Both use the same
remaining-server.R / remaining.js revision (frozen as linked-frozen-*), fresh R
process, fresh native Brave session, Demo -> Mouse -> Linked, dataset SHA above.
One B/C pair only, as requested. No R preparation optimization was attempted.

| Metric | Baseline | Candidate |
|---|---:|---:|
| First painted/usable | 4.712 s | 5.805 s |
| Complete, final initial frame including clone | 9.713 s | 12.055 s |
| Supplement bytes | 16,002,268 | 74,900 |
| Supplement decode | 134.9 ms | 1.0 ms |
| Supplement decode-to-draw | 433.8 ms | 344.9 ms |
| GPU position uploads | 6 | 5 |
| GPU upload time | 18.4 ms | 13.7 ms |
| Primary apply | 203.3 ms | 203.7 ms |
| Supplement R preparation | 2.513 s | 2.683 s |
| Initial operation JS heap peak | 314.06 MiB | 225.25 MiB |
| R process peak observed before correctness | 1720.82 MiB | 1705.31 MiB |

The candidate emits an intermediate complete flag at 9.021 s before the clone's
last draw at 12.055 s. We report the latter, not the earlier flag. Frame gaps near
one second persist despite the user's foreground confirmation. Their cause is
unresolved. Browser extension translation is visibly active in the existing
profile in both arms; it was not changed mid-test. Whole-page latency is worse
in this one pair; no claim of a reliable latency improvement or a sub-2s page.
Retention meets the user's explicit alternative gate: large payload reduction
AND less browser geometry work. No more Linked performance experiments.

The candidate's first diagnostic Save was clicked after primary paint but before
supplement completion; this stopped Rprof and added a control acknowledgement.
Therefore its complete number is an observed diagnostic result, NOT a valid
formal A/B latency estimate. First paint precedes that Save. Exact binary bytes,
decode and upload counts support the payload/rebuild gate independently. We do
not rerun an expensive series to promote the unsupported latency claim.

The 16 MB comprises two 1M x/y f32 geometry copies (8 MB each), not expression
values. Both exactly match the rounded canonical UMAP on present rows; one has
18,193 excluded cells, the other none. The existing validated Viewer Pack frame
declares canonical_projection. We send its projection reference and zero-based
excluded indices, retaining legacy arrays for unvalidated/custom/3D geometry.
The browser shares identity arrays and cached unit coordinates; exclusion uses
an exact masked copy. Existing dataset fingerprint/token checks and fallback
remain in front of hydration. No coordinate precision or expression precision
change, no new resource cache. Primary/static 8 MB geometry and 1 MB metadata
remain unchanged. Clone payload remains approximately 601 KB.

Real Brave rendered four panels; an E18 trajectory brush selected 22,279 cells
consistently in all panels. Targeted R/JS tests pass for exact exclusion, sharing,
unchanged source arrays, malformed subsets, missing projection, order mismatch
and 3D fallback. Offline full-1M comparison is linked-coordinate-audit.json.
The audit normalizes R AsIs attributes before numeric equality; the original
attribute-sensitive check was false despite zero numerical difference.
Physical click/socket send, separate typed-allocation/reorder durations and
isolated Brave process RSS are not measured. decode-to-draw includes application
and scheduling, not pure CPU. No attempt is made to assign all 6-12 s to JS CPU.

## Trajectory: ACCEPTED for removing unsolicited auxiliary work (Level 1)

Baseline 97f91ac5, candidate is the following Trajectory commit. One fresh-process
B/C, same native Brave / remaining harness / dataset. Default entry -> State
after settling -> Data info -> revisit. Browser-local timestamps are authoritative;
inherited requestAt/serverPrepare on recolors refer to page entry and must NOT be
interpreted as a 52-second State computation.

| Metric | Baseline | Candidate |
|---|---:|---:|
| Default page click -> first drawn frame | 4.8274 s | 4.6582 s |
| Warm State input -> first drawn frame | 0.7938 s | 0.8248 s |
| Revisit click -> cached draw | 0.2627 s | 0.1918 s |
| Default primary binary | 8,838,647 B | 8,838,647 B |
| Automatic auxiliary binary | 38,481,596 B | 0 B |
| Aux after first requested State interaction | already sent | 38,481,596 B |

This is not a demonstrated first-draw speed improvement. State remains near the
observed ~0.8 s path, and fresh does not meet 1.5-2 s. The change removes an entire
unrequested build/serialize/decode pass and its competing CPU/transfer work.
The existing final-draw/readiness logic is unchanged. Hover/selection is available
on demand, but full stable-ID/metadata resolution is not instantaneous: the
candidate State request takes 2.490 s from recorded aux request to aux applied.
This cost is explicitly deferred, not eliminated or hidden in a fake complete.
Baseline default aux applied at 15.4224 s after page click. Other visible secondary
outputs remain, including an approximately 2.66 MB message; the candidate is not
claimed to have zero background work. No new queue/cache/backend architecture.

### Exact wire composition (bytes, decimal)

| Auxiliary field | Bytes | Needed for primary draw? |
|---|---:|---|
| 981,807 barcode strings / selection keys | 21,789,572 | No; stable hover/selection IDs |
| Transcript counts i32 | 3,927,228 | No; hover |
| Expressed gene counts i16 | 1,963,614 | No; hover |
| sample category codes i8 | 981,807 | No; hover |
| seurat_clusters category codes i8 | 981,807 | No; hover |
| State category codes i8 | 981,807 | No; hover |
| Pseudotime f64 | 7,854,456 | No; hover (precision unchanged) |
| Header, labels, alignment | 1,305 | Protocol |

There is no geometry, repeated x/y, edge or lineage array in this auxiliary packet.
The default primary contains 7,854,456 B x/y and 981,807 B categorical codes, plus
2,384 B header/colors/edge shapes. State sends a ~3.26 KB descriptor instead:
canonical-projection-v1, exclude_uint32 (72,772 B), 981,807 state codes, cached
projection hydration on subsequent frames. It avoids dynamic metadata grouping
and coordinate serialization; its first State switch still fetches the static
8 MB resource when default dynamic coloring has not populated that resource cache.
Both arms show a second State primary caused by existing reactivity. Not redesigned.

The existing Gene demand mechanism now also covers trajectory_projection, including
the pending-ID first-hover gate, request deduplication and stale frame cancellation.
The server still validates epoch/generation/token and resolves the same exact data.
Real State hover: CAAGAAAAGTCAAGGC-57, 4,009 transcripts, 1,974 genes, Excitatory
neuron, pseudotime 0.90. Non-State brush resolves 7,184 stable IDs; first examples
AACTTTCCATCGTCGG-1, AAGTCTGAGAGAGCTC-1, AATCCAGGTACATCCA-1. Both colors render
981,807 points. Wire, dataset-context and demand-hover targeted suites pass.
Raw evidence records header types/bytes and actual requests; candidate .first.json
and .first.rss precede hover/correctness. Browser heap and R RSS are observational,
not isolated whole-browser native allocations; full later RSS includes correctness.

## Bounded memory and shared-hint lifecycle

**ACCEPTED**: scope the existing browser shared-coordinate hint by dataset context.
The first bounded run (memory-final, 5d3d18d7) found a real pre-existing omission:
coordviews_shared_base carried fingerprints but no epoch/generation. On A -> B -> A,
the browser cleared its coordinates, while the server still trusted A's old hint
and omitted both coordinates and the static resource descriptor. The client
subsequently recovered through a 102,216,534-byte full bundle and logged unitOf
on missing coordinates. This was not caused by reference supplementation: the
failure occurred before the fresh primary could hydrate. The old hint contract
is visible in baseline 28444813 as well.

The fix adds dataset_context to the existing hint and checks it in Linked and
specialist shared-projection eligibility. Exact same-context reuse remains; a
different generation or epoch must use the normal static resource path. There
is no new cache, cleanup manager, request framework or protocol codec. Both Gene
helper call sites pass the existing request context. Targeted tests cover correct
context reuse, stale generation/epoch, wire fallback, incremental expression,
NA/numeric/RGB, demand hover and dataset context.

### Final finite sequence (memory-fixed; diagnostic, not a latency A/B)

Fresh R/session: loaded A Gene -> Gad1 -> Mean(Gad1,Slc17a7) -> Linked -> Trajectory
-> B Gene/LYZ -> A Linked -> B Gene -> A Linked. Data info is used for the selector.
Five dataset changes including initial Demo -> A; snapshots use production
dataset cleanup followed by gc(). The selector may redirect to Data info while
loading, so a page navigation is retried only after the visible state is confirmed.

| Snapshot | R RSS MiB | gc live MiB |
|---|---:|---:|
| Loaded A Gene, before gene selection | 1692.7 | 1011.2 |
| A repeated Gene | 2051.1 | 1037.4 |
| A first Linked | 2044.9 | 1049.1 |
| A Trajectory | 2161.0 | 1083.4 |
| B first Gene | 2161.0 | 1035.5 |
| A second Linked | 2161.0 | 1053.9 |
| B second Gene | 2161.0 | 1036.8 |
| A final Linked | 2173.5 | 1054.0 |

R peak 2173.5 MiB; browser observed JS heap peak 395.19 MiB (not native browser RSS).
The comparable A Linked live sizes stabilize at 1053.9 -> 1054.0 MiB. Live objects
fall on B while RSS remains high: evidence favors allocator/native high-water
behavior plus bounded caches, not monotonically accumulating live R objects.
This finite check cannot prove the absence of every leak.

Cache counts: expression rows 0 -> 2 -> 3, then stable at 3 / 16,011,952 bytes
(existing LRU limit 4); two immutable CRB prototypes; one Viewer Pack descriptor;
one projection-resource descriptor (~2.5 KB); no trajectory/spatial resource
descriptor in this default-color sequence; extra-material cache zero. Pending aux
0-2 while active and zero after switches; all recorded pending contexts match the
active generation. Old immutable prototypes and two old-dataset expression rows
are deliberately retained within those bounds, so "zero old objects" is NOT claimed.
No active old-dataset frame or selection was observed. R6/environment/closure
object.size figures are shallow and not a substitute for gc live size.

Open .bpcells FDs remain zero in all snapshots, so there is no growing open-FD
count; this does not enumerate native memory-mapped handles. The active backend
switches embedded/BPCells correctly. Previously fixed browser static-map cleanup
is not retested; these are R descriptor counts, not browser resource-map counts.
All 32 old-generation/wrong-epoch acceptance-gate probes return false; these are
read-only gate probes, supplemented by wire/context tests, not 32 injected packets.
Both A return visits draw 1M points with no wire fallback; largest message ~601 KB.

One separate Shiny 1.14 client error appeared during the second empty-selection
transition: coordviews_selected_cells_UI received a silent validation error in
state invalidated. An explicit NULL instead of req(character()) candidate passed
its empty-output Shiny unit test, but the final browser smoke reproduced the same
state error for a VALUE message. **REJECTED**: it does not fix the output lifecycle
problem and is reverted. The smoke remains separate from the memory series.
This console-level output state issue remains **INCONCLUSIVE**; it is not evidence
of stale dataset acceptance, but a fully clean lifecycle acceptance is NOT claimed.
No errors are suppressed, no Shiny internals monkey patched, no ready signal moved.

After that smoke warning, the A return still rendered 1M points without fallback.
A real brush selected 3,354 cells in all four Linked panels, and the selected-cell
plot plus table rendered all 3,354 records (first visible ID AACCATGTCTGTGCAA-1).
Thus current data/selection function recovered, but the transient console error
remains unresolved; this is not a fully clean-console sign-off.

The memory-fixed process started before the final explicit context argument was
added to Gene's preparation helper (the rendering helper and Linked guard were
already active). That call-site completion is covered by subsequent targeted Gene
tests and the final fresh-process smoke; memory-fixed is not a benchmark of that
last one-line change. memory-final and memory-fixed are not pooled.

## Reconnect: BLOCKED for resumable-session acceptance; configuration unchanged

The live client reports allowReconnect=false. Installed Shiny 1.14.0 JavaScript
initializes it to false; the repository has no explicit enabling call. This is
evidence of inherited default behavior, not evidence of an intentional product
decision. allowReconnect(TRUE) also requires transport support; forcing a browser
retry alone does not establish same-session resumption. Installed implementation
is retained in reconnect-installed.txt.

No concrete product requirement for resumable sessions was established. Enabling
it would need a deployment/session-ownership contract (routing, expiry and state
restoration), while the current epoch synchronization already protects fresh
connections. No configuration change or fake reload-as-resume claim. Resumable
reconnect remains a deployment/product limitation, not a failed application test.

## Final scope and disposition

- ACCEPTED: Gene prepared-value relay removal; Linked canonical supplement
  references; Trajectory demand auxiliary loading; generation-scoped shared hints.
- REJECTED: removing Gene one-cell prewarm; replacing empty-selection req with
  NULL as a fix for the Shiny invalidated-state error.
- INCONCLUSIVE: stable <=3 s Gene across five runs; Linked whole-page speed gain;
  clean Shiny output lifecycle during all switches. No invented performance win.
- BLOCKED: same-session resumable reconnect in the current standalone transport.

All work uses the original branch/worktree. No Ren, Spatial, HLA, Projection or
Groups benchmark was run. Existing geometry/index/precision formats, expression
storage and RGB reading remain. Results are not pooled across different harness
revisions or diagnostic sequences. FINAL_SHA256.json freezes the raw evidence,
executed harness snapshots and final source files. Candidate commits were made
after their working-tree tests; commit IDs identify retained mechanisms, not a
claim that those future commits were HEAD when their earlier runs started.
