# Linked supplement tail: diagnostic closeout

Date: 2026-10-10. Baseline: **b04751cb**, branch `卧槽/pr6_clean_perf`, same
checkout. **No production changes and no performance candidate.** Gene and
Trajectory pages were neither modified nor exercised. This is a diagnostic
record, not a formal performance comparison or a complete functional pass.

## Environment and limits

Native Windows Brave 154.1.96.59, existing profile/process, ordinary headed tab,
natural viewport **1369 x 915**. The prior 1619 x 950 and 1369 x 859 runs are not
paired comparators. Windows 11 / i9-12900K / 128 GiB; existing WSL Ubuntu 24.04
R 4.6.1 / BPCells 0.3.1 / Shiny 1.14.0, package loaded from this checkout.
Mouse CRB SHA-256:
`cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38`.
Sidecar/Viewer Pack identity follows the existing viewer-followup protocol.
No browser flags, environment configuration, data, precision or ready rules changed.

Build the test-only renderer with `python tests/bench/viewer-followup/linked-tail/build.py`.
Launch command (from repository root):

```text
wsl --exec Rscript tests/bench/viewer-followup/linked-tail/server.R /mnt/c/Users/wangx/Projects/CerebroNexus /mnt/c/Users/wangx/Projects/shinyapps_docker/apps/cerebronexus/private-data/cerebro_mouse_brain_1m_pages_clean_v1.crb /tmp/cerebro-linked-tail.pid 8876 /mnt/c/Users/wangx/Projects/CerebroNexus/tests/bench/viewer-followup/linked-tail/diagnostic2
```

`IDENTITY.json` distinguishes production source from generated instrumentation;
`SHA256.txt` hashes the preserved evidence. `analyze.py` reads the compressed raw
record and prints the inclusive function intervals and browser event timeline.

Fresh R process, new page document, Data info -> Mouse -> first Linked visit.
No other analytical page was visited first. OS/browser static caches were not
cleared; this is fresh-process first visit, **not machine-cold**. A subsequent
revisit and Mouse -> Demo -> Mouse sequence were correctness checks only.

Test-only R trace probes record monotonic elapsed/process CPU time; Rprof uses
the platform's effective 10 ms interval with GC profiling. Test-only JS copies
add entry/exit markers and markers inside the existing two RAF callbacks.
No callback is removed, rescheduled or declared ready early. An early WebSocket
listener timestamps receipt before Shiny processing. Existing GPU counters
measure submission time, not independent hardware fence completion. Browser
heap is approximate `performance.memory`, not isolated browser RSS.

The first draft probe mistakenly wrapped a Shiny handler with function arity 0;
Shiny rejected it before a valid run. It was corrected to arity 1, the R process
restarted, and that failed run excluded. `first.json.gz` is the valid first
operation, copied before later interaction tests. `diagnostic2.json.gz` contains
the later correctness sequence. Its large recorded selection arrays and repeated
DOM reporting make later timings unsuitable for performance conclusions.

## First-operation timeline (ms from Linked click)

| Boundary | Time / duration | Interpretation |
|---|---:|---|
| Browser visible input event | 0.6 | Earliest Linked request signal |
| Primary server preparation | 303 duration | 277 resource + 26 bundle, nested in traced 307 ms |
| Primary wire packing | 25 duration | 2,604 bytes; static resources delivered separately |
| Primary binary received | 1,497.1 | Includes deferred module setup/server queue; not all transfer |
| Primary binary decode | 0.4 duration | Existing telemetry |
| Static projection fetch/decode | 342 duration | 8 MB, 14.1 ms decode, with metadata fetch in parallel |
| Static metadata fetch | 106.5 duration | 1 MB; do not add to the parallel projection duration |
| Primary apply | 1,841.0–2,045.1 | 204.1 ms, includes drawing |
| Initial paint confirmation scheduled | 2,044.9 | Token 1 |
| Resize confirmations scheduled | 2,526.4; 3,559.7 | Tokens 2 and 3 supersede earlier confirmations |
| Primary actually acknowledged usable | **5,557.6** | Painted + primaryReady + 1M points |
| Supplement request sent | 5,558.9 | Triggered by the primary acknowledgement |
| R supplement input -> prepare start | 7 duration | No seconds-long reactive queue here |
| Supplement prepare | **2,402 duration** | Inclusive trace; payload telemetry 2,398 ms |
| Supplement packing | 7 duration | 74,900 bytes |
| Supplement received | 8,060.7 | Early WS listener; full WS frame 74,922 bytes |
| Supplement binary decode | 0.7 duration | Geometry hydration separately 3.2 ms |
| Supplement apply | 8,065.3–8,377.0 | 311.7 ms inclusive, including 175.5 ms final drawAll |
| Clone preparation (worker) | 125 duration | Outside main R CPU profile; starts after supplement send |
| Clone packing | 10 duration | 601,252 bytes |
| Clone received | 8,484.0 | Full WS frame 601,274 bytes |
| Clone decode / sparse hydration | 2.7 / 3.4 duration | Separate from apply |
| Clone apply | 8,490.5–8,699.8 | 209.3 ms including drawing |
| Clone paint confirmation scheduled | 8,699.7 | Token 6 |
| Resize redraw / new confirmation | 8,741.0–8,742.2 | Token 7 supersedes token 6 |
| Final RAF 1 / RAF 2 | 9,620.8 / 10,619.3 | ~1 Hz cadence |
| Complete acknowledgement | **10,620.0** | Existing painted/complete semantics |

The clock probe round trip was 60.1 ms. Using its midpoint gives approximately
42 ms from supplement send-end to browser receipt, uncertainty at least +/-30 ms.
The primary corresponding interval is approximately 5 +/-30 ms. Do not use
wall-clock `serializeTransferMs` as a precise network measurement, or add these
intervals to inclusive server/browser totals.

First -> complete is **5,062.4 ms**. It partitions in browser time into:

- 2,503.1 ms: primary acknowledgement -> supplement receipt, mostly R preparation.
- 423.3 ms: supplement receipt -> clone receipt, including supplement application.
- 215.8 ms: clone receipt -> clone apply end.
- 1,920.2 ms: clone apply end -> final acknowledgement, including the resize redraw
  and renewed two-frame confirmation.

There is no measured long task during the final ~1.88 s after that resize.
The long tasks at supplement and clone processing are 340 and 219 ms; they do not
explain the subsequent two ~1-second frame intervals. The specific native cause
of the RAF cadence remains unresolved. Visibility and document focus were true;
these do not prove native-window occlusion/compositor state. No readiness shortcut
was attempted. Primary has the same cadence, but optimizing it is outside this task.

## Server attribution: nested durations, not additive totals

| Function/phase | Wall ms | Process CPU ms | Finding |
|---|---:|---:|---|
| cv_prepare_progressive_supplement | 2,402 | 1,753 | Outer total, once |
| cv_build_progressive_supplement | 2,317 | 1,693 | Nested in prepare, once |
| cv_canonical_metadata | 1,348 | 822 | Includes forcing the lazy getter argument |
| getter argument forced | 1,251 | 736 | First thin metadata/barcode hydration |
| metadata cv_cell_ids | 92 | 84 | Nested validation/encoding after forcing |
| cv_build_deferred_metadata | 3 | 1 | Group descriptions only; no per-cell group codes |
| cv_build_trajectories | 944 | 864 | Two requested, visible trajectory spaces |
| trajectory cv_cell_ids | 96 + 129 | 87 + 116 | Nested in trajectory preparation |
| cv_async_clone_spec | 4 | 1 | Descriptor only |
| cv_start_clone_task | 18 | 14 | Dispatch, not worker compute |
| clone worker | 125 | Not sampled | Existing separate worker telemetry |
| cv_send_progressive_supplement | 63 | 53 | Includes packing and send setup |

Rprof attributes roughly 620 ms of sampled metadata work to `readLines` of the
BPCells sidecar's **cell-name text file**, not expression access. It also samples
180 ms of metadata uniqueness checks. The 1,251 ms getter wall time must not be
called pure disk I/O: it includes parsing, allocation and validation. Wall-minus-CPU
is not independently attributable to I/O versus host scheduling.

Trajectory samples include ~360 ms `match`, ~190 ms GC, ~170 ms uniqueness checks,
and ~150 ms `unique.default`. These are samples inside the 944 ms interval, not
additional costs. No expression dense materialization or expression matrix read
occurs on this path. Coordinate vectors are allocated by the existing compatibility
builder; transport references remove them before sending. Replacing that builder
with the old canonical-index experiment is explicitly out of scope and was not done.

## Duplication/reuse audit and candidate decisions

- **ACCEPTED observation:** one primary, one supplement, one clone for initial
  Mouse; no wire fallback. Three dataset generations over the full correctness
  sequence each build once. A same-generation revisit does not build again.
- **ACCEPTED observation:** delayed metadata hydration is memoized. Later selection
  access forces the already-loaded getter in single-digit milliseconds. There are
  six metadata columns; group descriptors take 3 ms, not the historical 2.26 s.
  Some validation repeats after hydration, but its ~92 ms is below the gate.
- **REJECTED before implementation:** add another metadata/trajectory cache.
  No prior prepared supplement exists on the fresh Linked visit. Revisit already
  reuses it. Cross-generation reuse would require additional identity/lifetime
  management without evidence of a qualifying whole-page gain.
- **REJECTED before implementation:** defer trajectory/clone data until interaction.
  Both trajectories and the clone panel are displayed in this workspace; deferring
  them changes completion semantics. Deferred group values and clone detail rows
  are already demand-loaded. Clone compute alone is below the 300–500 ms gate.
- **REJECTED before implementation:** restore the old metadata/index patch.
  Its prior native-Brave A/B failed the whole-page gate. No mechanical retry.
- **REJECTED before implementation:** bypass or advance the two-frame confirmation.
  This would not repair native frame cadence. The final resize really invokes a
  redraw, so an earlier confirmation is not justified by these probes.
- No production candidate was applied; consequently none needed reverting and
  **formal A/B was not started**. Historical 2.275/7.828 s and mixed-session numbers
  are not compared against this instrumented 5.558/10.620 s result.

## Resources and correctness

Initial operation: primary **2,604 B**, separate static geometry **8,000,000 B**,
static metadata **1,000,000 B**, supplement **74,900 B**, clone **601,252 B**.
No old 16 MB supplement or geometry fallback. Existing GPU counters report
12.2 ms submit at primary and 21.7 ms across final panels; these are cumulative
per-panel CPU submit counters, not an independently measured GPU execution phase.
R main-process peak **1,705.58 MiB** (worker RSS not included); sampled JS heap peak
**226.29 MiB**. No candidate comparison exists, so these are baseline observations,
not proof of a memory improvement or regression.

**ACCEPTED:** all four Linked panels display 1M canonical points; actual box drag
selects 163,941 cells consistently across all four. The two trajectory mapped
counts are 163,680 and 163,941; receptor-bearing subset is 16,346. Selected-cell
table renders 163,941 entries, with barcode and metadata columns and correct
visible first rows. Evidence: selection.txt / selection.png.

**ACCEPTED:** Mouse generation 2 -> Demo generation 3 (1,476 cells) -> Mouse
generation 4 (1M cells) restores current fingerprint and clears selection. No old
dataset is displayed after current readiness and no fallback is requested.
Same-generation revisit is ready at 332.5 ms in this diagnostic session; do not
treat this later value as a formal performance benchmark. No artificial stale
payload was injected; this is a real switch observation plus existing guard review.

**BLOCKED functional closeout:** ordinary hover, enabled through Settings, displays
the current group (tooltip opacity 1, text `0`). Clicking to pin a tooltip raises
`Cannot read properties of null` at production `cell_views.js:3851`:
`gg.levels[gg.values[i]]` for a deferred group whose `values` is null. The function
is unmodified and not wrapped by the diagnostic probe. This is a current-baseline
Linked bug, separate from the previously known Shiny `invalidated` issue. See
console.json.gz and hover evidence. Do not claim a complete hover/detail pass or
an error-free Viewer. No fields were silently omitted to conceal it. The selected
cell table passed; pinned single-cell detail did not.

## Conclusion

**INCONCLUSIVE** whole-page improvement: no admissible candidate was identified.
The remaining server cost is real first-use hydration and trajectory mapping;
the larger browser tail is renewed paint confirmation under ~1 Hz RAF cadence.
Further broad caching/index/geometry changes are not justified by this scoped
diagnosis. Stop performance experiments here. The pinned deferred-group tooltip
bug is a separate, concrete correctness item for a subsequent scoped fix.

**Linked has entered the low-return optimization region for this scope; no further
performance optimization is pursued in this round.**
