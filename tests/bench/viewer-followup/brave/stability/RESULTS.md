# Remaining Viewer stability and latency

Baseline `602f9303`; existing `卧槽/pr6_clean_perf` checkout. Protocol and hashes
are adjacent. Raw results are retained, including failed diagnostics. No broad
Viewer audit or unaffected-page benchmark was repeated.

## Gene: INCONCLUSIVE for stable <=3 seconds; STOP further code experiments

The current production Gene implementation is unchanged. These are the five
previous candidate first-Gad1 operations, individually reanalyzed from original
JSON and R traces by analyze-existing.py (gene-five-timelines.json). They are
not new A/B measurements. All have the exact Gad1 key and 1,000,000 points.

| Run | Input -> R observed | R observed -> send end | Primary WS observed | WS observed -> ready | First usable | Complete |
|---|---:|---:|---:|---:|---:|---:|
| debounce-C | unavailable | 1426 | 1703.9 | 382.2 | 2086.1 | 2834.3 |
| pair2-C | 80.8 +/-30.6 | 1439 | 1704.5 | 522.5 | 2227.0 | 3725.8 |
| pair3-C | 98.8 +/-28.3 | 1363 | 1658.2 | 1961.9 | 3620.1 | 5682.7 |
| final4-C | 1104.9 +/-29.8 | 1360 | 2646.6 | 2005.0 | 4651.6 | 6714.0 |
| final5-C | 1476.3 +/-29.7 | 1494 | 3211.1 | 2141.3 | 5352.4 | 7384.9 |

All times ms from shiny:inputchanged except the explicitly described interval.
Primary WS listener runs after Shiny's handler, so its timestamp includes some
client processing and is not the network's first byte. The final two slow runs
overlap the existing one-cell BPCells initialization before R observes the input
(final5 warmup 1304 ms). This is queue contention, not an extra full Gad1 read.
Input-to-observer does not isolate socket-send delay from R queueing.

Detailed server phases (same run order):

| Stage, ms | debounce-C | pair2-C | pair3-C | final4-C | final5-C |
|---|---:|---:|---:|---:|---:|
| Input observed -> row start (includes 200 ms input debounce) |273|291|270|291|305|
| Row preparation / expression access |891|906|851|848|927|
| Row ready -> render (includes 50 ms prepared debounce) |85|86|95|88|98|
| Sparse f64 color preparation |142|130|124|115|137|
| Primary wire pack |16|7|8|6|9|
| Summary CPU, after primary acknowledgement |221|245|256|266|313|

The variance is dominated by (1) pre-observer first-backend work, and (2) browser
paint/ack scheduling. The stable ~0.85-0.93 s row read is the third large absolute
cost, but explains little variance. Relative to pair2, final5 adds 1395.5 ms
before observation, 55 ms inside the R primary path, and 1618.8 ms between the
primary WS observation and ready. Browser long tasks (185-250 ms for the main
update) do not explain the multi-second gap; measured frame gaps approach 1.03 s.
reportSinglePainted waits for GPU completion and two animation frames. The old
data does not separately timestamp each GPU wait/frame, so GPU versus frame
scheduling is not uniquely resolved. Foreground occlusion is NOT claimed as fact.

Physical click -> input, socket send, exact BPCells native read/index subphases,
and operation-specific decode/apply/upload are unavailable in these original
traces. Recolor inherits old page timing fields; treating its old decode/draw
numbers as current would be false precision. Actual browser point count/key and
post-draw readiness are retained. Canonical order uses the existing numeric/full
row path; no full barcode rematch, all-gene dense matrix, geometry rebuild,
bulk hover preparation or second full expression read appears. Summary/group
work starts only after the primary acknowledgement; empty selected summaries
do not prepare selected-cell data. Secondary CPU is not the first-primary cause.

Current five-run first median/range remains **3620.1 [2086.1,5352.4] ms**, only
2/5 <=3500 ms. Complete **5682.7 [2834.3,7384.9] ms**. Historical paired relay
comparison remains B 4020.3 [3650.9,4125.0] vs C 2227.0 [2086.1,3620.1]; do not
compare that three-run C median with five-run B (no such sample exists).

No new latency gain or resource change is claimed. Existing first-operation R
peaks 1932.6-1969.7 MiB (four complete RSS records), JS heap 129.6-157.5 MiB,
WS ~1.053 MB; the primary color packet is exactly 873229 bytes. The prior
no-prewarm experiment moved initialization into the first read and worsened
complete; it is not repeated. An async backend worker would add lifecycle and
memory complexity. Changing ready frames would change acceptance semantics.
Neither is justified by the current evidence. Expression/RGB/precision/hover
correctness evidence remains the unchanged prior acceptance; no claim of a new
full Gene correctness suite is made.

## Shiny selected UI: ACCEPTED

Commit `5a511f56` adds a page gate before selection/metadata access and keeps only
that gate runnable while hidden. It deliberately bypasses the global page-scoped
outputOptions wrapper for this one lightweight UI output. Child plot/table work
remains suspended/demand-driven. req() is retained; errors are not suppressed.

The baseline B2 repro established the precise server order:

1. At mono 994195326, sidebar=loadData, dataset=Demo, output hidden, selection
   context cleared. The old output's context was Mouse generation 6.
2. 994195332 recalculating -> 994195340 recalculated queues an empty-selection
   validation result while hidden.
3. 994195353 a second dependency invalidation sends binding progress. The output
   is now suspended, so it does not rerun.
4. 994195471 Shiny flushes the previously queued error along with new Data info
   values. The browser now expects a recalculation, not a value/error.

Browser confirmation at 12:52:51.964Z: the exact invalidated-state exception,
one DOM node, no remove/rebind. The old-generation empty result is an output
protocol race, not a stale cell payload accepted by the dataset gate. Shiny's
onInvalidate calls showProgress; its flushOutput retains the already queued
result. Both the req and prior NULL variants are vulnerable to that ordering.

Important evidence limitation: B2.json is the earlier saved snapshot; the last
Save was not persisted before the test server stopped. The later repro is in
the complete B2.stages.jsonl and browser tool transcript; check-output-order.py
therefore reports server_full_violations=1 even though browser_saved_violations=0.
This is not presented as a complete saved browser trace of the failure.

Candidate C: 18 recalculations, 71 recorded output wire messages, **zero invalid
state transitions**, one output DOM node, no new console exceptions (the tab's
old pre-reload errors were filtered by timestamp), no geometry fallback. Real
3354-cell brush and the 3354-row table agree; first ID AACCATGTCTGTGCAA-1. Empty
selection, hidden Linked, Linked->Gene->Linked, repeated Mouse->Demo->Mouse,
Gad1->Mean, Trajectory and return visits pass. The focused test confirms hidden
generation changes perform zero selection/metadata reads and current data is
read again on return. Invalidation during an in-flight nonempty selection is
not deterministically injected; rapid page switches and post-selection switches
are covered, so an exhaustive race proof is not claimed.

The matched first Linked observations B2/C are first 5459.5/5387.7 ms and complete
10583.4/10450.5 ms: no visible regression, **not a formal performance gain**.

## Linked performance: INCONCLUSIVE; STOP

B2 first->complete = 5123.9 ms. The first large supplement is observed at 7920.3
ms (2460.8 ms after first usable), clone at 8207.2, final painted readiness at
10583.4. Main blocks are supplement preparation/arrival (~2.46 s), post-final
message painting/ack scheduling (~2.38 s), then clone delivery/rebuild (~0.29 s
between large messages; client long tasks 334/219 ms overlap message processing).
These intervals overlap CPU/transport and must not be summed as pure CPU stages.
Prior Rprof attributes supplement cost to canonical metadata (~1.0 s sampled)
and trajectory mapping (~1.07 s sampled). Empty selection output takes only
~10-40 ms; there is no second 16 MB coordinate message or selected summary load.

The exact canonical metadata/index alternative was already rejected in three
pairs: R preparation 2183 ->343 ms but complete 10396 ->11650 ms. Repeating it
would violate the no-repeat rule. No new clearly eliminable >=0.5 s chain has
been demonstrated beyond that rejected approach and unresolved paint scheduling.
No new Linked performance patch or expensive acceptance series. Existing 74.9 KB
coordinate references and ~601 KB clone packet remain. See ../RESULTS.md and
../remaining/RESULTS.md for the rejected and retained mechanisms respectively.

## Trajectory: measurement correction and focused candidates

The old specialist-state signal is synchronous after drawAll. Its timing is
**primary draw submission**, not first usable: viewer-shell still blocks input
until the later painted=true cell-view-ready event. Parent-directory B/C/D and
historical fresh/warm/revisit figures must not be called first usable. Their raw
files remain diagnostic evidence. A separately versioned paint/ series observes
actual painted readiness plus loader removal and canvas count, using identical
conditions and hashes within that new comparison. No production ready behavior
is changed. The initial diagnostic-listener-B1 exposed event listener order;
formal B1 onward check in a microtask after all synchronous listeners finish.

Candidate changes are only indexed metadata column assignment and reuse of
Gene's existing empty-selection metadata path. The first preserves an identical
real 981807-row data frame while avoiding cbind row-name rebuilding. Offline
default column 527 ->11 ms; browser traced primary cell preparation 814 ->50 ms.
The second avoids ~0.75 s sampled full-metadata CPU before the primary path:
traced primary preparation starts at page 3207 ->1964 ms in the old diagnostic.
Module initialization 124/131 ms, packed index 26/24 ms and first-frame metadata
5/3 ms are not dominant. Canonical static frame, excluded index, f64 precision,
geometry/cache invalidation, defaults and required draws remain unchanged.

Formal painted comparison and final functional evidence are recorded below.

### Trajectory performance: ACCEPTED (`7dab9018`)

New protocol `paint/PROTOCOL.md`, baseline `5a511f56`, candidate source hashes
in each identity file. Windows native Brave, fresh R/tab each run. Actual saved
viewport is **1369x859 in every Trajectory run**, not the inherited intended
1619x950; lifecycle B2/C did use 1619x950. This is explicitly a separate series,
not pooled with lifecycle, Gene or historical measurements. Harness contains
equal tracing/Rprof overhead in each arm; these are instrumented browser times.

| Pair/run | B first usable ms | C first usable ms |
|---|---:|---:|
|1|5338.5|3465.3|
|2|5490.8|3432.0|
|3|5615.9|3336.8|
|Final C4|not run|3463.1|
|Final C5|not run|3460.6|

Three interleaved pairs: **5490.8 [5338.5,5615.9] ->3432.0
[3336.8,3465.3] ms**, 37.5% faster. Five final candidate observations:
**3460.6 [3336.8,3465.3] ms**. All eight have 981807 points, current Mouse
context, painted=true, pageReady=true, no blocking loader and aria-busy removed.
Microtask verification overhead is 0.2-0.4 ms. The <=3 s target is not claimed.

| Run | Click -> request | Server prepare | Serialize/transfer residual | Decode | Draw |
|---|---:|---:|---:|---:|---:|
|B1|802.1|3142.5|278.5|11.5|113.3|
|B2|936.6|3096.3|320.7|17.7|155.6|
|B3|850.9|3290.6|211.4|8.8|136.0|
|C1|858.9|1232.3|146.7|12.0|150.2|
|C2|825.7|1226.9|163.1|12.2|140.1|
|C3|882.8|1261.5|167.5|12.2|138.9|
|C4|859.8|1201.7|155.3|10.9|131.9|
|C5|841.3|1194.1|164.9|11.3|141.5|

These are production timing fields; transfer is a residual, not independently
measured socket serialization. Primary draw median B 4495.3 vs final C 2362.7 ms.
The remaining approximately one-second draw-to-painted scheduling wait is real
in the observed browser. No claim is made that occlusion uniquely explains it.

R peak median **1832.9 ->1780.7 MiB**. Candidate range1770.8-1781.1 MiB.
Complete browser capture B1 JS heap116.2 MiB; C five-run range116.2-131.3,
median124.0 MiB. This modest variation is reported, not declared a zero increase;
no new cache, client array or retained allocation was introduced. Native browser
RSS was not isolated. B2/B3 saved before the final ~1.98 MB secondary output, so
their partial browser payload/heap/last-message fields are excluded from resource
comparisons. Their complete first-usable records and continuously monitored R
peaks remain valid. B1/C complete-capture WS totals ~11.535 MB are unchanged;
primary binary payload is8838647 bytes throughout. No first-entry auxiliary
barcode/hover packet; the38.48 MB auxiliary remains interaction-demand only.

No formal Trajectory complete reduction is claimed: this harness does not
independently time the final secondary chart paint. Its last-message time is
explicitly not complete. Warm State in C5: primary draw865.0 ms, painted first
usable2488.4 ms. Revisit: draw209.3, painted251.4 ms. Warm State uses canonical
projection-v1 / exclude_uint32,8072772 static coordinate/subset bytes and3260-byte
control payload; it is a separate warm operation, not comparable with historical
draw-only0.8 s as if that were actual first usable.

Final functional checks observed edges/labels, 981807 default/State points,
18193 excluded cells, Marker-guided ->Monocle2 synthetic_1m1000000 points ->back,
real2006-cell selection, and Mouse->Demo1476->Mouse with selection reset.
C5.first.json/.rss preserve first-entry resources. Its later Save did not replace
the first JSON; those additional UI observations are not claimed as fully saved
event traces. A separate final correctness run below persists its full sequence.

### Interaction correctness: ACCEPTED (`756126a7`)

Final interaction checks exposed two existing defects, independent of the
performance patch: a single-level hover dictionary is auto-unboxed as a JSON
string, while the browser accepted arrays only; the selected table required
appearance inputs from unopened Settings. Fixes normalize singleton hover
dictionaries at display time (including null/NA handling) and gate the table
only on valid selection. No wire/precision/cache/ready mechanism changes.

Fresh final correctness run, unopened Settings: real3412-cell brush,3412 table
rows, first ID AAACGGGAGAGTCGGT-1 / canonical index19. Hover at
CCGGTAGCATCAGTAC-106 displays4199 transcripts,1958 genes, full sample label
"10x E18 mouse brain (1M subset)", cluster0, Neuroblast, pseudotime0.83.
Mouse->Demo->Mouse ends generation4 with981807 points and0 selected; console
error/warn list empty. paint/correctness.json persists all9 operations and final
context. This functional run is not included in performance medians.
Targeted tests pass39 R expectations including a Node fixture with10 assertions:
exact indexed values/order/factor levels/NA/zero, selection mapping, empty-summary
equivalence, lazy-control table gate, singleton/multiple hover levels and numeric
formatting. See targeted-tests.txt and trajectory-target.json.

## Finite memory/lifecycle

Candidate C R RSS start/peak/final **1623.09 / 2215.21 / 2154.30 MiB**; gc live
start/final **983.1 / 1056.6 MiB** (start is before Gene/Trajectory caches exist).
After Gene/Trajectory live 1087.5 MiB falls to 1056.6 on final Mouse Linked.
Existing bounded cache entries, obsolete-context rejection and per-snapshot
sizes are in summary.json. No new cache or retained large data is introduced by
the selected-UI gate. This finite mixed-page sequence does not establish a
long-term plateau by itself; prior same-page repeat evidence remains applicable.
Browser JS heap peak is **408.90 MiB**, not native process RSS. The final server
memory record (label 32) was persisted in memory.jsonl; the second Save raced
shutdown and the saved browser JSON includes snapshots only through label 23.
The summary uses all five server snapshots and marks the unsaved final browser
probe unavailable. The four persisted browser snapshots reject all 11 obsolete
contexts and all four wrong-epoch probes. No injected stale payload is claimed.
Final caches: two expression rows / 16,000,096 bytes, two CRB prototypes, one
pack, one projection descriptor, zero pending aux and zero trajectory/spatial
resource descriptors. Old immutable rows/prototypes are intentionally bounded;
they are not claimed to be all removed. No heap profiling.
Resumable reconnect stays allowReconnect=false; no deployment configuration is
changed or misclassified as a performance bug.

## Final disposition

- ACCEPTED: `5a511f56` Linked selected-output lifecycle, `7dab9018` Trajectory
  actual first-usable reduction, `756126a7` two interaction correctness fixes.
- INCONCLUSIVE / STOP: stable3 s Gene and further Linked complete improvement.
  No new Gene/Linked performance source experiment was retained or repeated.
- REJECTED historical approaches remain rejected: Gene no-prewarm, empty-UI
  replacement for lifecycle, and Linked metadata-only speedup without full-page
  gain. The draw-only measurement and initial listener-order probe were replaced
  as acceptance methods; their raw evidence is retained, not silently discarded.
- BLOCKED: same-session resumable reconnect under allowReconnect=false.

Remaining worthwhile investigations are the first-operation backend queue and
GPU/frame scheduling gaps. Trajectory selector/request initialization is about
0.8-0.94 s; its optimized primary R path about1.2 s. Do not spend another broad
cycle on the already-small index read/descriptor/decode steps. No Ren, Spatial,
HLA, Projection, Groups or Immune benchmark was run. No new branch/worktree.
SHA256 manifest covers archived raw profiles, all evidence and retained source.
