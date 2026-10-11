# Focused Windows Brave follow-up

Baseline: `c108c7e0`. Results below are separate from the prior WSL Chrome series.
No expression, RGB, precision, wire, Spatial or HLA production changes.

## Linked: REJECTED

Candidate extended the existing compact supplement to trajectories with validated
canonical integer indexes, avoiding full thin metadata hydration. Legacy/missing
indexes retained the barcode fallback. The removed candidate is preserved in
`evidence/rejected-linked.patch`; per-arm source/harness hashes accompany raw data.

An exploratory screen showed a clear R preparation benefit. Three interleaved
formal pairs then failed the full-page retention gate. Do not promote these noisy
measurements to a performance improvement, or extend a failed candidate to an
expensive five-pair acceptance run.

| Metric | Baseline median [min,max] | Candidate median [min,max] |
|---|---|---|
| First usable, ms | 6487.3 [3332.1,7428.3] | 6430.0 [6336.5,6516.6] |
| Complete, ms | 10396.3 [9796.5,14554.5] | 11650.2 [11580.6,11836.6] |
| Supplement R prepare, ms | 2183 [2157,2206] | 343 [327,360] |
| R tree peak, MiB | 1566.43 [1565.80,1566.49] | 1523.26 [1523.13,1523.77] |
| All Brave processes peak, MiB | 3620.95 [3545.74,3626.83] | 3636.70 [3569.31,3681.07] |
| Supplement payload, bytes | 16002268 | 16002268 |

The browser RSS includes existing user tabs; it cannot establish an isolated
Viewer memory regression. Full websocket bytes are ~16.613 MB (47 messages),
with small nonce/header variation. The candidate never removed the supplementary
coordinates or browser reconstruction. Baseline revisit observations: 144.0,
159.2,161.2 ms; candidate:190.9,171.4,111.2 ms. B1 revisit is recorded in the tool
transcript; later revisits are in raw JSON. Primary and completion events include
painted=true, the exact cell fingerprint and 1,000,000 points.

The exact 1M comparison passed for both trajectory spaces, including row order,
NA and zero coordinates. The targeted coordinated-views suite passed, including
new invalid/missing-index checks in the rejected patch. An initial evidence writer
failed to serialize proc_time after checks passed; its corrected replay persisted
the exact comparison in linked-correctness.json. No claims use that writer error.

There are multi-second gaps between draw submission and painted readiness in this
headed Brave environment. Foreground/occlusion throttling was suspected, not
proven. A request to place the test window in the foreground was made. Raw
cross-host serializeTransferMs can be negative and is invalid for attribution;
browser-local arrival/decode/draw and R elapsed preparation remain available.
Never subtract unsynchronized Windows/WSL clocks. No ready signal was changed.

First-usable <2 s and shorter complete are **not established**. The production
candidate and its production-only tests were reverted. The old 2.275/7.828 s
Chrome results remain historical; they are not this Brave baseline.

## Cache measurement (before changes)

`cache-before.json` measures the actual fetchCachedResource function with ten
synthetic immutable 8 MB assets. Retained buffers grow 8,16,...,80 MB; a repeated
URL hits the existing cache. This is deterministic retained-buffer evidence,
not a browser RSS or a claim that ordinary A/B dataset switching leaks 8 MB each
time. The four resource maps have no dataset-invalidation clearing in baseline.

## Trajectory: ACCEPTED mechanism check; INCONCLUSIVE historical timing target

No production change. One directed Brave confirmation, not an A/B claim:

- Default seurat_clusters: first primary 3930.1 ms, 981807 points, 8838643 bytes.
  Browser binary decode 9.3 ms and first draw 106 ms. Primary is ready well before
  the auxiliary message (38481596 bytes, arrives ~10.914 s after page click).
- After that auxiliary work settles, State input -> actual primary is 809.2 ms.
  canonical-projection-v1 / exclude_uint32 are active. Projection 8000000 bytes,
  subset 72772 bytes (18193 exclusions), category 981807 bytes; primary 3256 bytes.
  A following 3260-byte update hits both geometry and subset caches. No altered
  default color, dropped cells, changed ready signal or new cache architecture.
- State page revisit: cached event at 186.1 ms, all 981807 points retained.
- State auxiliary work is still substantial (aux event +5141.6 ms). Eager auxiliary
  preparation remains a worthwhile future target for rapid cross-page operations;
  the earlier report already demonstrated queued State changes behind old aux.

Default non-State and warm State are different operations. The historical 0.6 s
cannot be compared directly to this headed Brave observation. Event timing fields
inherited from the old page request (e.g. serverPrepareMs ~37 s on the State change)
are not the State operation's server time; only the new input timestamp is used.
Evidence: trajectory-check.json and trajectory-check.diagnostic.json.

## Gene selector: ACCEPTED

The mounted Gene/RGB selectors now retain their registered server search choices
on same-dataset page activation. Dataset changes still refresh them; the old
generation/data-set retry checks remain. The previous dataset reference is
released on dataset invalidation even while Gene is hidden. Other callers keep
the default refresh behavior. Expression reads, geometry and payload are unchanged.

The targeted expression-rgb-dataset-switch suite passes (25 expectations),
including immediate return-page input, deliberate clear, hidden dataset changes,
canonical spelling, and obsolete callback rejection. In real Brave on Mouse,
Gene(Gad1) -> Linked -> Gene -> immediate input added Slc17a7 at 313.9 ms after
the return click. The final selector retained both genes and the final primary
render key contained both, with 1,000,000 points and Mean expression (2 genes).
No delayed selector clear or Gad1 restoration followed. Evidence:
selector-check.diagnostic.json and selector-check.json. These are correctness
observations, not new Gene timing benchmarks. The browser run used the same
Gene behavior before the final restriction of hidden-data tracking to the opt-in
Gene selectors; the final shared-helper version passed the targeted suite again.

## Dataset resource lifetime: ACCEPTED

The measured baseline retained ten distinct 8 MB resource buffers (80 MB).
The final implementation clears the four existing static resource maps at dataset
invalidation. Same-dataset page returns still reuse their entries. The identical
ten-dataset fixture now retains 8 MB per active dataset and zero entries after
invalidation. This bounds obsolete-dataset retention in these maps; it does not
impose a byte limit on resources belonging to the active dataset. No LRU, new
cache manager, precision change or geometry format change was introduced.

An old failed in-flight fetch can no longer delete a newer same-URL promise:
deletion is conditional on the map still containing that exact pending promise.
Existing dataset/epoch application guards remain in force. Exact helper fixtures
verify all four maps, same-dataset reuse and this late-failure race. Evidence:
cache-before.json, cache-after.json, static-resource-lifetime.js.

## Incremental Gene field memo: ACCEPTED correctness correction

Actual browser inspection exposed a stale legend/hover field memo: an incremental
update reuses the data object, so fieldOf() could keep the previous expression
field even after the current values changed. Clear that memo alongside the
existing paint-order/clip memo invalidation, and on dataset invalidation.
This does not change expression reads, precision, payload or primary geometry.

The real-function incremental fixture now primes the old memo, applies the next
expression (including NA), and asserts the new field and legend label/range.
Brave then displayed Gad1 0-10, Mean(Gad1, Slc17a7) 0-5, and Demo TNFRSF4 0-1.9.
Before/after raw lifecycle events are preserved separately. No new Gene first-load
benchmark or performance claim was made.

## Finite lifecycle acceptance

One R process, two browser sessions separated by a reload, Mouse and Demo. The
final session switched Demo -> Mouse -> Demo -> Mouse and performed seven page
transitions, repeated Gene operations and Linked return. Final Gene immediate
return input arrived 315.2 ms after the page click and retained both selected genes.
The final Mouse frame had 1,000,000 points, current generation 4, Gad1 0-10, and
zero selected cells. Demo had 1,476 cells. Actual earlier brush selected five
Mouse cells; hover resolved barcode GCACATATCGTTTAGG-61 and its metadata.

Eight recorded obsolete dataset contexts were rejected; the synthetic obsolete
epoch was rejected. No browser errors were recorded. These checks plus existing
dataset-context tests cover stale-result rejection; they are not an exhaustive
concurrency proof. Reload produced a new session/epoch. Resumable reconnect
remains **BLOCKED** by allowReconnect=false, outside this task's scope.

| Phase | R tree RSS MiB | R tree peak MiB | All Brave RSS MiB |
|---|---:|---:|---:|
| Start Demo | 1397.69 | 1397.69 | 3261.19 |
| First Mouse Gene | 2163.15 | 2163.15 | 3466.82 |
| Mouse brush | 2111.03 | 2271.68 | 3248.59 |
| Before reload | 2256.58 | 2271.68 | 3214.47 |
| After reload Mouse | 2111.46 | 2271.68 | 3396.74 |
| Mouse Linked return | 2469.39 | 2469.39 | 3578.75 |
| Demo return | 2469.39 | 2469.39 | 3306.84 |
| Second Mouse | 2469.39 | 2469.39 | 3426.12 |
| End | 2554.42 | 2562.36 | 3493.90 |

RSS has decreases and retained high-water allocations; this short run does not
prove an unbounded leak or whole-application steady-state memory bound. The
static-map retained-buffer issue is demonstrated and corrected independently.
Brave values include unrelated user tabs; 3578.75 MiB is the maximum of these
snapshots, not an isolated Viewer peak. No comparative P2 RSS/latency A/B was
performed, so a formal claim of no memory/performance regression is **INCONCLUSIVE**.
No functional regression was observed in the targeted checks. Heavy heap profiling
was not justified by this finite run.

Raw evidence: final-lifecycle.{json,diagnostic.json,memory.jsonl,rss},
lifecycle-before-legend.{json,diagnostic.json}. The raw diagnostic `state` field
used summary() without its required view ID and is invalid (ready=false/0 points).
It is excluded from conclusions: actual primary events match the final context
and contain 1M points; visible DOM independently confirmed the count and legend.
The saved identity records the harness hash actually used. The future harness
now supplies expression_projection; this correction was not retroactively applied
to raw evidence. The P1 diagnostic harness is recoverable at 3cd67fdd; selector
evidence used the version committed at 9b8524b2. Formal Linked hashes are unchanged.

### Targeted validation

- R expression-rgb-dataset-switch: passed, 25 expectations (selector commit).
- R static-resource-lifetime, viewer-dataset-context and single-expression-incremental:
  passed after resource-lifetime changes.
- Final Node single-expression-incremental: passed after field memo correction,
  including existing RGB, selection order, stale paint and fallback checks.
- Final Node static-resource-lifetime and gene-hover-demand: passed.
- git diff --check: passed.
- No Spatial, HLA, Groups, Immune, Projection performance sweep, Ren benchmark,
  or additional Gene first-load optimization was performed.

## Final disposition and remaining work

- Linked: **REJECTED** candidate; production baseline restored. Commit 33f8e631
  retains the measured evidence and rejected patch. Its 16 MB supplement and
  browser reconstruction remain the meaningful unresolved complete-time cost.
  check-linked.R requires the preserved rejected patch to replay candidate checks.
- Trajectory: existing canonical/excluded-index/static path confirmed in 3cd67fdd.
  Default non-State first-load and 38.48 MB eager auxiliary payload remain slower;
  no safe, small architectural replacement was justified in this bounded task.
- Selector: **ACCEPTED**, 9b8524b2. No unnecessary same-dataset re-registration.
- Static resource/field memo correctness: **ACCEPTED**, final cache-fix commit
  containing this report. No server data-access, expression protocol or ready
  definition change. The only rejected production experiment was Linked compact
  supplement expansion; no hidden experimental path remains enabled.
