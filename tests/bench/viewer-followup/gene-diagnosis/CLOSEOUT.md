# Final bounded Gene / Linked / Trajectory closeout

Date: 2026-10-10. Production baseline `4d8e96de`, branch `卧槽/pr6_clean_perf`,
same worktree. **No production changes or retained performance candidate.**
No expression-reader, geometry, RGB, barcode order, precision, wire, ready,
cache, or scheduler changes. This is the final bounded investigation, not a
new broad audit. No Spatial, HLA, Projection, Groups, Ren, or long-session run.

## Disposition

| Direction | Conclusion | Reason |
|---|---|---|
| Viewer-specific RAF fix | INCONCLUSIVE; stop | 1 Hz reproduced without Viewer; native scheduler cause not established. Changing ready is unacceptable. |
| Dimension-only backend prewarm | REJECTED | Only 60 ms total difference in the targeted screen; first gene itself slower. No browser A/B justified. |
| Moving prewarm earlier | REJECTED as an unproven saving | Synchronous initialization still costs time; moving it to page/data loading does not reduce total cost. No patch made. |
| Further Linked supplement optimization | INCONCLUSIVE; stop | Known metadata/index candidate already failed whole-page A/B; do not repeat it. No different minimal second-scale candidate established. |
| Trajectory mechanism retention | ACCEPTED | Current canonical/excluded-index/State path confirmed; no missing historical mechanism or unsolicited first-entry aux packet. |
| Stable Gene <=3 seconds | INCONCLUSIVE; not achieved | Real current-baseline operation remains 3.429 s with ~1.586 s post-GPU RAF wait. |

## 1. RAF isolation

The independent `frame.html` has no Viewer, Shiny, BPCells, WebGPU or application
ready logic. It requests 20 RAFs, first without canvas drawing, then with a
200x100 canvas repaint on every callback. `frame-control.json` retains all
intervals rounded to 0.1 ms as read from the visible diagnostic DOM.

- Without canvas drawing: 8/20 intervals >=1000 ms, maximum 1031.4 ms.
- With canvas drawing: 14/20 intervals >=1000 ms, maximum 1031.4 ms.
- Both report visible and focused. Initial intervals are approximately 31 ms,
  then long 1000–1031 ms intervals appear. Continuous canvas damage does not
  remove the low cadence.

This excludes Viewer computation, application auxiliary work and WebGPU
completion as **necessary conditions** for the symptom. It does not distinguish
native window occlusion, browser/profile scheduling, extensions, power policy,
or automation interaction. The native-window control from the preceding turn
was stopped by Computer Use's URL safety check. This turn requested explicit
selection of the exact RAF tab; no new user confirmation arrived before the
bounded captures ended. No native-window workaround, browser flags, extension
or system settings were changed. Do not label occlusion as the proven cause.

Current Viewer first Gad1 (ms from input):

| Milestone | ms |
|---|---:|
| Primary decode begins | 1409.9 |
| First draw submission ends | 1613.4 |
| GPU completion | 1839.5 |
| RAF 1 | 2423.2 |
| RAF 2 | 3425.2 |
| Painted event timestamp | 3429.4 |
| Complete, visible by-group painted | 5486.5 |

Post-GPU RAF waiting is **1585.7 ms**, including **1002.0 ms** for the second
callback. No RAF was removed, no ready event was advanced, and the actual
paint-boundary gate remains unchanged. The diagnosis does not equate GPU idle
or draw submission with physical display presentation.

## 2. BPCells prewarm experiment

`warm-experiment.R`, two fresh sequential WSL R processes on the same 1M fixture.
Only the test's warmup expression differs; the actual reader is untouched. Arm A
uses the current one-cell warmup; arm B initializes dimensions only. Both then
read the same complete Gad1 row. This is a targeted rejection screen, not a
formal Viewer A/B series.

| Seconds | Existing one-cell warmup | Dimension-only experiment |
|---|---:|---:|
| Dataset attach | 4.837 | 5.101 |
| Warmup | 1.038 | 0.848 |
| First complete Gad1 row | 0.899 | 1.029 |
| Warmup + first gene | **1.937** | **1.877** |
| Dataset + warmup + first gene | **6.774** | **6.978** |

One million values, 927311 zeros, no NA, identical serialized value SHA-256
`992585674d823a0115686971d60b54723bfb85f414ed85327e6265075e017e05`.
End RSS 1486311424 vs 1478553600 bytes (not peak). The 60 ms total difference
does not meet the second-scale gate. No repeat, no browser candidate, no source
patch. Moving this initialization into page loading would just transfer the
same synchronous cost; lower observer priority cannot interrupt it once running.
The previously rejected no-prewarm experiment was not repeated.

The real browser closeout retains the original warmup, measured at 1123 ms,
before the recorded Gad1 operation. Cached expression preparation is 787 ms.
This warmed operation is not evidence that overlapping-input contention was
fixed. No claim that prewarm was successfully moved out of the total user wait.

## 3. Linked tail

Current source already replaces validated duplicate coordinates with static
geometry references. The previously cited **16 MB is historical**, not the
current supplement. In this run:

- First painted: 4637.0 ms; complete: 10793.9 ms; tail **6156.9 ms**.
- Supplement observed at 8497.1 ms; clone at 8831.3 ms.
- First->supplement observed: 3860.1 ms; supplement->clone observed: 334.2 ms;
  clone observed->final painted: 1962.6 ms. These are disjoint elapsed intervals,
  not additive pure CPU timings.
- Traced canonical metadata: **1293 ms**; trajectory mapping: **2041 ms**;
  progressive supplement build: 3345 ms; preparation wrapper: 3444 ms. These
  are nested; do not add wrapper and child durations. Production transport
  prepare reports 3503 ms, a slightly broader boundary.
- Supplement transport payload 74888 bytes (74910 at the WS listener), decode
  1.1 ms, decode-to-draw 332.8 ms. Clone payload 601256 bytes (601278 at WS),
  decode 3.3 ms, decode-to-draw 90.1 ms. No repeated 16 MB packet.
- Initial static coordinates were already cached from Gene. This is a mixed
  session diagnostic, not comparable to the old 2.275/7.828 s fresh-page series.

The compact supplement intentionally falls back when trajectory modalities are
present; the old candidate expanded it using canonical metadata/index reuse.
That exact direction previously reduced R preparation 2183->343 ms but worsened
whole-page complete 10396->11650 ms in three pairs. It is not reintroduced.
Current context/token checks, single supplement per primary, async clone, and
selection lifecycle remain. No new cache or state machine was added. No further
Linked whole-page improvement is claimed.

## 4. Trajectory: keep the verified implementation

One mixed-session default entry, after Gene and Linked have settled:
**2431.1 ms actual painted first usable**. Warm State, after the default frame
settles: **2237.8 ms actual painted first usable**. Later State revisit:
**452.4 ms actual painted first usable**. These are distinct conditions, not
fresh/default repetitions or a formal improvement over existing acceptance.

Current default primary preparation in the wire header is **512.1633 ms**,
primary packet **8838647 bytes**. The initial default page does not send the
38.48 MB auxiliary; it is requested later by actual brush/hover interaction.
No old-frame auxiliary was ahead of default entry or the settled State action
in this sequence. This does not prove preemption of arbitrary in-flight R work.

State emits **3260 bytes**, `canonical-projection-v1`, `exclude_uint32`,
981807 retained points / 18193 exclusions. Canonical projection cache hit is
true; subset asset is 72772 bytes; categorical state asset 981807 bytes.
The inherited State `serverPrepareMs=29446` and old request timestamp refer to
earlier page entry and are explicitly **not** State operation durations.

Historical ~0.6 s warm and ~0.093 s revisit numbers used different readiness
and cache conditions. The preceding accepted painted protocol already corrected
this: default five-run median **3460.6 [3336.8,3465.3] ms**, State painted2488.4,
revisit painted251.4. That accepted result remains the formal baseline. No static
architecture rewrite or new Trajectory performance candidate was warranted.

## 5. Browser validation and resource scope

Environment: existing Windows native Brave, WSL R, Mouse 1M fixture with SHA-256
`cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38`, current
production baseline; inherited environment/package identities are in the
existing viewer-followup manifests. Test-only harness combines the existing
painted protocol with the timestamp-only Gene renderer copy. GC-enabled Rprof
and tracing are active. Viewport 1369x859. `closeout-identity.json` records exact
harness hashes; `closeout.json`, `.stages.jsonl`, `.Rprof.gz`, `.rss`, and
`closeout-summary.json` retain evidence. No new ready definition.

This is **one diagnostic mixed-session smoke run**, not a timing acceptance
series. There is no new candidate to justify 3 or 5 interleaved pairs. Browser
clock is authoritative for user operation intervals. Inherited timings are not
used to infer current operation timing. Trajectory complete was not separately
instrumented and is not substituted with last-message/quiet time.

- Gene Gad1: 1M points, exact expression check as above, primary packet873229 B;
  total recorded first-gene WS1052782 B.
- Linked: 1M primary, final complete true, current dataset fingerprint; current
  supplement and clone byte counts above. Revisit first633.5 / complete713.6 ms.
- Trajectory: default/State981807 points, visible State labels/graph, real326-cell
  box selection and326-cell composition. The shared cohort becomes empty after
  the Linked return; selection retention across those modules is not claimed.
- Mouse->Demo:1476 rendered Gene points, no genes,0 selected, painted ready,
  correct generation3. Screenshot confirms old ordinal tooltip text retained
  in hidden DOM is not displayed over the new dataset. No console errors/warnings.
- Existing focused correctness tests: **140 passing expectations** (39 indexed
  metadata/mapping/selection/lifecycle expectations plus101 dataset-context
  expectations). Node fixtures check exact current/old generation and epoch
  acceptance/rejection. This is not an injected browser stale-payload test.
- Complete fresh hover metadata text was not independently recaptured this turn;
  prior `756126a7` browser acceptance remains the evidence. No changed hover code.

R end/peak **2059.01/2259.33 MiB** for the whole finite run, including the later
real brush and dataset switch. Gene JS heap peak145.57 MiB; mixed-session peak
434.39 MiB after demanded auxiliary/selection work. Native Brave RSS observation
3853611008 B spans23 processes and other user tabs; **not isolated page RSS or
a measured browser peak**. No formal resource regression comparison is claimed.
No production allocations/cache/payload mechanisms changed.

## Stop and artifacts

Gene is closed without a safe stable<=3s result. Linked and Trajectory keep their
existing implementations. Remaining second-scale costs are the environmental
RAF cadence, synchronous first backend initialization and Linked supplement
preparation; the last has already failed the tested simple whole-page remedy.
They do not justify another broad optimization cycle under the current rules.

Only diagnostics/evidence are committed. Large raw browser logs, profiles,
reactlogs and derived timelines have lossless gzip copies; uncompressed local
copies and generated cell_views.js are ignored.
Run `build.py` before `build-closeout.py` to regenerate the renderer/harness from
the recorded production source. Analysis scripts also read the gzip archives.
The diagnostic R and HTTP services and task-created tabs were stopped/closed.
