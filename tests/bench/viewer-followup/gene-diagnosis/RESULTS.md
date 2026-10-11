# First-gene latency attribution — diagnosis only

Baseline: `4d8e96def839496126d63be8b8da617647efbba7`, existing
`卧槽/pr6_clean_perf` worktree. No production code, branch, worktree, data
protocol, precision, renderer scheduling, or readiness definition was changed.
Only Gene / Mouse 1M / first Gad1 was exercised. Three new captures, not A/B.

## Conclusions

**ACCEPTED (attribution):** the historical slow operation overlaps synchronous
one-cell backend initialization; the new probes directly isolate long waits in
the two RAF callbacks after GPU completion. These are separate costs. Full Gad1
expression preparation is not the source of the approximately 3-second spread.

**INCONCLUSIVE:** the browser/OS reason for approximately 1 Hz RAF delivery,
the exact compositor presentation time, and stable <=3-second performance.
`visible` and `hasFocus() === true` do not establish an unobscured native window
or explain scheduler policy. No claim that occlusion, GC, or power management
caused the delay. No code solution has been tested or accepted this turn.

## Historical comparison, unchanged frozen harness

Source: `../brave/remaining/gene-*.json`, corresponding `.stages.jsonl` and
`.Rprof.gz`; derived records in `../brave/stability/gene-five-timelines.json`.
Protocol/data/harness identity remains in `../brave/remaining/GENE_SHA256.json`
and the associated identity files. The five candidate operations use Mouse 1M,
Gad1, the same frozen harness and post-draw readiness. They are first operations
after the empty Gene page; the asynchronous backend warmup's completion at
input time is a measured variable, not a deliberately controlled warm start.

All entries below are milliseconds from `shiny:inputchanged`. R offsets are
approximate cross-clock estimates, with historical midpoint uncertainty about
30 ms per run; they are not physical click or network arrival timestamps.

| Milestone | Fast (pair2-C) | Typical (pair3-C) | Slow (final5-C) |
|---|---:|---:|---:|
| Input | 0 | 0 | 0 |
| R high-priority input observer | 80.8 | 98.8 | 1476.3 |
| Cached expression preparation starts | 371.8 | 368.8 | 1781.3 |
| Cached expression preparation ends | 1277.8 | 1219.8 | 2708.3 |
| Primary render starts | 1363.8 | 1314.8 | 2806.3 |
| Sparse f64 color starts / ends | 1367.8 / 1497.8 | 1317.8 / 1441.8 | 2809.3 / 2946.3 |
| Wire pack starts / ends | 1507.8 / 1514.8 | 1449.8 / 1457.8 | 2954.3 / 2963.3 |
| Primary render/send wrapper ends | 1519.8 | 1461.8 | 2970.3 |
| WS callback observed AFTER Shiny handler | 1704.5 | 1658.2 | 3211.1 |
| Primary painted/ready acknowledgement | 2227.0 | 3620.1 | 5352.4 |
| By-group summary CPU starts / ends | 2339.8 / 2584.8 | 3715.8 / 3971.8 | 5490.3 / 5803.3 |
| Complete (by-group painted) | 3725.8 | 5682.7 | 7384.9 |

The absolute fastest `gene-debounce-C` is **2086.1 ms**, complete **2834.3 ms**,
with WS observed at **1703.9 ms**, followed by **382.2 ms** until ready.
It has no successful browser/R clock calibration. Its R-observed-relative
timeline is: row 273–1164, render 1249–1426, color 1254–1396, wire 1405–1421.
Those cannot legitimately be placed on the browser's input timeline. Pair2-C
(2227.0 ms) is the closest fast sample with cross-clock alignment. The old raw
files cannot retrospectively supply physical input, actual socket receipt,
native-read, GPU-idle, or individual RAF timestamps. Recolor's inherited timing
fields describe the empty page render and must not be reused for Gad1.

### Accounting for the slow-minus-fast difference

For final5-C minus pair2-C, total **3125.4 ms**:

- Input to R observer: **+1395.5 ms**. The slow prewarm ran from approximately
  **-44.7 to +1259.3 ms**, duration 1304 ms; only its **1259.3 ms overlap**
  belongs after input. In fast pair2 it ended **2647.2 ms before** input.
  The remaining approximately **136 ms** of the extra pre-observer interval
  cannot be split into network/flush/dispatch from the old probes.
- R observer to primary wrapper end: **+55 ms**. Row preparation differs by
  only **+21 ms** (906 vs 927); input-observed to row-start differs by 14 ms.
- Wrapper end to post-Shiny WS observation: **+56.1 ms**, a combined transfer
  and client handling interval, not pure network latency.
- Post-Shiny WS observation to ready: **+1618.8 ms** (522.5 vs 2141.3).

These are non-overlapping intervals. Color and wire timings are nested inside
the primary render interval; do not add them again. The 2086.1-to-5352.4 total
is 3266.3 ms, but its backend split is unavailable, so the complete accounting
uses the calibrated 2227.0-ms fast run rather than inventing timestamps.

## Backend attribution

`inst/viewer/gene_expression/obj_projection_expression_levels.R:191–215`
observes the empty plot's `expression_projection_rendered_key` and synchronously
calls `viewerExpressionRow(data_set(), 1L, available[[1L]])`. For this dataset
that is one Xkr4 value, not a second million-cell Gad1 read. R cannot service
the pending Gad1 input while this callback executes. The debounced selected
gene value can still be empty during that callback.

The slow historical R profile contains 98 samples in this warmup stack excluding
the actual `viewerExpressionValues` path. 64 have `dims_matrix_file_cpp` as the
sampled leaf, 12 `lazyLoadDBfetch`; the stack includes
`requireNamespace -> ncol -> viewerExpressionCells -> viewerExpressionRow ->
observeEvent(expression_projection_rendered_key)`. This identifies backend
dimension/method initialization in the synchronous prewarm. Sample counts are
CPU evidence, not independent wall-clock durations.

The ordinary 200 ms gene-selection and 50 ms prepared-value debounce remain.
Historical input-observed-to-row-start is 270–305 ms, not a multi-second
invalidation problem. A diagnostic run has an additional 590 ms inside
`observeEvent(delayed())` / `.dependents$values` invalidation. That pause is not
present at comparable scale in the old five profiles and the new harness enables
reactlog. It must not be substituted for the old slow-run root cause or used to
justify a reactive framework rewrite.

## Direct browser instrumentation (new, separate diagnostic protocol)

`build.py` produces a test-only cell_views.js copy and server harness. It inserts
timestamps without changing draw, GPU wait, RAF, or ready calls. The WebSocket
receive probe is installed before Shiny. Fresh R process and tab per capture,
Demo -> Mouse -> Gene empty plot -> first Gad1, real Windows Brave, existing
WSL R/package/data environment. Browser viewport was 1369 x 859. These
diagnostics are not pooled with old measurements, nor called an A/B improvement.

`run1` has a footer installation error (zero-argument custom message handler).
Its head/renderer timestamps remain valid but no complete/input comparison is
claimed. `run2` and `run3` use the corrected identical harness. Their start is
the input event immediately preceding the actual immutable Gad1 socket packet.
Earlier input event array values can mutate through Selectize; selecting the
first event whose stored value later says Gad1 would be wrong.

| Browser event (ms after operation origin) | run1 (socket origin) | run2 (input origin) | run3 (input origin) |
|---|---:|---:|---:|
| Socket send | 0 | 68.9 | 87.0 |
| Earliest primary message receive | 1438.8 | 2301.3 | 1707.9 |
| Decode starts / ends | 1439.2 / 1441.8 | 2301.7 / 2303.8 | 1708.5 / 1710.5 |
| Sparse color hydration starts / ends | 1442.0 / 1446.3 | 2304.0 / 2305.9 | 1710.6 / 1712.6 |
| Renderer apply starts | 1447.0 | 2306.2 | 1713.0 |
| First draw submission ends | 1648.4 | 2508.7 | 1924.9 |
| GPU completion promise resolves | 1856.7 | 2588.8 | 2085.3 |
| First RAF callback | 2572.5 | 3085.4 | 2723.0 |
| Second RAF callback | 3572.0 | 4082.9 | 3723.0 |
| Primary painted event | 3572.8 | 4083.9 | 3723.6 |
| By-group complete | unavailable | 5583.4 | 5536.5 |

Thus the two RAF waits total **1715.3 / 1494.1 / 1637.7 ms**. The second RAF
alone waits **999.5 / 997.5 / 1000.0 ms**. This is directly measured in
`inst/viewer/www/cell_views.js:7934` (`reportSinglePainted`), after GPU work has
settled, before `reportSelection('primary')` and `cerebro:cell-view-ready`.
The approximately 200 ms renderer task finishes before this gap; there is no
recorded >50 ms JS long task during these post-GPU RAF intervals. Sparse decode
takes about 2 ms, not seconds. All three primary packets are **873229 bytes**,
one per Gad1 update. Geometry is reused; renderer point count is 1,000,000.

The frame cadence is observed, but its native scheduler cause is not determined.
All captured painted events report visible and focused. A foreground-window
control was requested; no new confirmation was received during these captures.
Do not assume this rules out occlusion or proves it. No browser flags were
changed. The GPU completion timestamp proves work completion, not physical
compositor presentation. The first RAF callback also runs before its frame's
paint; removing the second RAF would alter the conservative ready guarantee.
No claim that draw submission alone is actual user-visible first usable.

The page loader is already off for the empty plot, and remains off at Gad1
ready; no separate multi-second mask-removal handler was measured. Acknowledgement
and progress completion still wait for the two RAF callbacks. Secondary by-group
work begins after that acknowledgement, so it extends complete but cannot cause
the preceding RAF delay. Selected summaries have no selection to materialize.

### Clock and profiling limitations

New midpoint calibration estimates put R input arrival 35–52 ms before the
recorded socket send in runs 2/3, which is causally impossible. Run3's two clock
offsets also differ by 19.2 ms over 24.6 seconds. Absolute new cross-system R
offsets are retained only as diagnostic estimates with a warning; use the
same-clock browser timeline and R-relative durations. Do not claim sub-millisecond
cross-clock precision. This limitation does not affect the measured RAF intervals
or same-R-clock prewarm duration. Historical calibrated splits remain approximate.

Native BPCells read/index subphases are sampled by Rprof, not separately timed
at every C++ boundary. Rprof did not enable gc.profiling, so **GC is not excluded**.
LongTask observation does not identify the browser compositor or native GPU
scheduler. Reactlog and instrumentation can perturb the diagnostic R path.

## Suspicions checked

- No second full Gad1 read; one unrelated one-cell prewarm is real.
- Existing canonical numeric indices are used. No million-barcode reorder.
- No all-gene dense matrix; the requested gene vector is materialized as required.
- No rebuilt/retransmitted geometry on the Gad1 recolor; one color primary packet.
- No bulk hover preparation on this update; existing lazy hover is retained.
- No full dataset validation/descriptor preparation in this first-Gad1 path.
- No evidence of duplicate Gad1 relay or selector restoration overwriting it.
- By-group statistics/secondary chart CPU begins after primary ack. No primary
  dependency on global Shiny idle or quiet. Small UI outputs still participate
  in flushes, but do not explain the historical multi-second spread.
- One-time backend warmup is confirmed. GC-specific attribution and native
  browser frame throttling policy remain unresolved, not silently ruled out.

## Minimal proposals, no implementation

1. **First priority: explain/restore normal frame cadence under the unchanged
   readiness protocol.** Verify the native foreground/occlusion state and RAF
   delivery together before touching `reportSinglePainted`. If the environment
   is throttling frames, correcting that environment preserves all paint checks.
   The measured post-GPU wait bounds the theoretical saving at roughly
   **1.5–1.7 seconds** in the new captures, minus normal frame delay. This is an
   upper bound, not a benchmarked improvement. Merely reporting ready earlier
   is not an acceptable solution, and no safe application RAF patch is yet proven.
2. **Avoid synchronous warmup overlapping accepted input.** Consider performing
   the existing one-time BPCells initialization in the dataset preparation phase
   before users can interact, with the existing dataset identity/generation.
   This can move up to **1.26 seconds** out of the measured slow operation, but
   may add the same cost earlier; it is not an end-to-end saving and requires
   explicit page/dataset readiness accounting. Do not add workers/caches merely
   to move this cost. Lower observer priority cannot preempt a warmup already
   running. Removing prewarm was already REJECTED (initialization moved into the
   first read and complete worsened); that experiment was not repeated.

Fast historical runs demonstrate that <=3 seconds is possible with unchanged
precision and architecture. Stable <=3 seconds is not proven: normalizing the
RAF cadence and keeping first-backend initialization out of the interaction
would plausibly reach it, but doing only one leaves the other multi-second risk.
The stable expression CPU cost near 0.9 seconds is not the reasonable lower
bound of the observed 5.35-second tail. Further broad expression/geometry
optimization is unsupported by this diagnosis.

## Artifacts and status

Raw browser events, R stage traces, profiles, reactlogs, derived timelines, and
source hashes are adjacent. `build.py` regenerates the ignored instrumented
renderer from the recorded production HEAD. Run1's failed footer is disclosed;
its exact earlier footer revision is not retained, so it is corroborative only.
No latency candidate or production patch was made, accepted, reverted, or
committed. No other page or long-session benchmark ran. The diagnostic R process
and task-created Brave tab were closed after capture.
