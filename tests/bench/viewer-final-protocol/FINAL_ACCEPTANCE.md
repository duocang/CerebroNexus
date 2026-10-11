# Clean PR6 final acceptance and retirement

Date: 2026-10-10. Production source at the start of this acceptance:
`eb432a11b17ebdb06f0caa58690110d8cc43a2b9`, branch `卧槽/pr6_clean_perf`.
Historical PR6 baseline: `cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303`.
The earlier five-pair Projection/Gene-entry comparison remains in `RESULTS.md`.
This report supplements it with actual expression operations, the final dirty-work
ports, functional data preparation checks, Spatial, and session lifecycle checks.

## Evidence boundaries

The frozen v2 page harness and 1M dataset identities in `README.md` are unchanged.
Raw evidence for this acceptance is initially stored outside the checkout in
`C:\Users\wangx\Projects\CerebroNexus-backups\2026-10-10-pr6-final-acceptance`.
Performance observations run serially without concurrent functional tests.
First entry means warm filesystem, fresh R/browser, page previously unvisited.
No claim is made about a truly cold OS cache.

The WSL Chrome process uses `--use-angle=swiftshader-webgl`. WebGL2 is therefore
software rendered in these observations. This condition is compatible with this
series' existing WSL measurements; it is not a Windows Brave hardware-GPU result.
Page readiness is a scoped draw/content event, never global Shiny idle.

The first supplemental Gene probe was a pilot. It mistakenly selected the first
draw after an operation even when its label belonged to the previous input, and
reported page-scoped stage timings as operation timings. Its numbers are
**REJECTED**. Operation protocol v2 records label/panel state at the actual draw
event and matches that event. Unavailable operation-stage and complete timings
are `NA`. Instrumented profiler runs are excluded from latency comparisons.

## Data preparation audit order

| Step | Retained implementation | Acceptance scope |
| --- | --- | --- |
| Seurat conversion and split layers | Canonical cells and explicit layer partition checks | Public pipeline and split-layer tests |
| CRB write/read | Thin v2 descriptors, stable fingerprint, external BPCells, exact order recovery | IO, exported-data, Viewer Pack tests |
| Bundle creation | Validated backend plan and self-contained runtime | Synthetic production bundle and isolated-package smoke |
| Dataset activation | Cached prototype, safe session clone, deferred backend, generation snapshot | Clone/context tests and repeated browser dataset switches |
| Immutable resources | Cached Pack validation and HTTP projection/category resources | Pack and projection tests; frozen 1M page screen |
| Projection metadata/hover | Canonical integer indices; distinct-value formatting preserves text | Existing exact-value evidence plus projection checks |
| Gene | Bounded row cache, one materialization for RGB, exact f64 colors, lazy secondary work | Single/Mean/Separate/RGB operations and exact-value tests |
| Groups | Deferred summary/page outputs | Demo functionality and 1M screen |
| Linked | Progressive primary and supplement, palette deduplication | Demo background/palette checks and targeted 1M comparison |
| Trajectory | Static canonical frame and excluded-cell subset | Cell-coordinate/selection tests and 1M screen |
| Immune/HLA | Receptor-focused data, selective parser, early graph guard and hidden output gates | Repertoire browser tests, HLA contract/parser tests, existing Ren stress evidence |
| Spatial | Mode-lazy controls, hidden Moran guard, persistent controls | Spatial tests and independent canonical grid |

## Branch retirement

The backup bundle and dirty-content archives in
`C:\Users\wangx\Projects\CerebroNexus-backups\2026-10-10-pr6-clean` were verified
before deleting the four old branch refs: `pr1_new_crb`, `pr2_viewer_ui_fixes`,
`pr4_viewer_lifecycle`, and `pr5_viewer_requests` under `卧槽/`.
No `卧槽/pr3*` ref was present. Only `卧槽/pr6_clean_perf` is retained.
Eighteen already-invalid worktree registrations were pruned. Files in unrelated
directories were not deleted. After acceptance, the two temporary checkouts
`C:\Users\wangx\Projects\worktrees\pr6-clean-perf` and
`C:\Users\wangx\Projects\worktrees\pr6-reference` were removed with
`git worktree remove` after verifying clean status and archiving the four ignored
test images/PDF. The sole registered Git worktree is now
`C:\Users\wangx\Projects\CerebroNexus`, on `卧槽/pr6_clean_perf`.
The unrelated existing `worktrees/pr7` directory was preserved.

Effective mechanisms and rejected historical experiments are itemized in
`OLD_BRANCH_DISPOSITION.md` and `AUDIT.md`; complete old branches are recoverable
from the bundle, and old uncommitted content from the patch/tar archives.

## Production source and final optimization

The final production source is `bad37fcad9d48b31eaac8c98fe1aa2ce553c5bca`.
Its parent `eb432a11b17ebdb06f0caa58690110d8cc43a2b9` contains the last useful
old dirty-work ports. The subsequent acceptance commit changes tests, harnesses
and reports only.

The only new production optimization in this acceptance is a five-line Gene
hover-demand gate in `cell_views.js`. Selection-core/cell identity delivery stays
eager; full hover metadata is requested on an actual nearest-point hover and
deduplicated. The 1M browser check recovered the exact source barcode, transcript
count, expressed-gene count, sample and cluster. The first hover may wait for its
metadata; full tooltip contents and numeric precision are preserved.

The three-pair Level-2 comparison uses baseline `eb432a11`, candidate `bad37fca`,
the frozen 1M CRB, and operation-v2 harness SHA-256
`c52c450781b59a1ffd61f43d0c441c9f125577f45202c05fd749b257b864a8df`.
Each arm has a fresh R/browser. Each arm then performs six successive operations.
Times below are seconds, median [min–max]; they exclude page entry.

| Operation | Baseline first usable | Candidate first usable | Conclusion |
| --- | ---: | ---: | --- |
| First single gene | 7.124 [6.796–7.158] | 7.138 [6.981–7.264] | INCONCLUSIVE |
| Second gene | 1.863 [1.795–2.051] | 1.818 [1.804–1.919] | INCONCLUSIVE |
| Cached gene return | 1.057 [1.054–1.131] | 0.981 [0.943–1.041] | INCONCLUSIVE |
| Mean of three | 1.804 [1.779–1.858] | 1.823 [1.745–1.835] | INCONCLUSIVE |
| Separate three | 1.183 [1.156–1.337] | 1.152 [1.116–1.183] | INCONCLUSIVE |

First-gene observed websocket receive decreased from about 31.26 MB to 23.26 MB
in all three pairs, approximately 25.6%. **ACCEPTED** for avoiding unused hover
metadata delivery; **INCONCLUSIVE** for first-gene latency improvement. The
aspirational first-gene ≤3 s target is not met. These are observed operation
windows, not a guarantee that all delayed secondary work has completed.
Complete and uninstrumented operation stages are `NA`.

The v2 RGB boundary was insufficient: a mode transition can draw an intermediate
RGB frame before its debounced three-channel specification arrives. V2 RGB
latency and completed-operation traffic conclusions are **REJECTED**. Protocol
v3 additionally requires the exact three-gene/three-channel render key. Its
single-pair screen below is a separate series, not an extra repeat of v2.

## Current 1M page screen

Source `eb432a11`, frozen page harness and CRB, one first/revisit observation per
page. All fourteen rows have `status=ok` and `correctness_pass=TRUE`. These are
single observations, not medians. The later Gene-only hover gate is checked by
the operation and exact-hover tests. No unrelated page was rerun for that gate.

| Page | First usable s | Complete s | Revisit usable / complete s | First websocket receive B |
| --- | ---: | ---: | ---: | ---: |
| Projection | 2.006 | 2.006 | 0.067 / 0.067 | 32,398 |
| Groups | 1.701 | 1.701 | 0.011 / 0.011 | 621,147 |
| Gene entry, before gene operation | 2.126 | 2.126 | 0.566 / 0.566 | 22,224,872 |
| Linked | 1.792 | 6.531 | 0.034 / 0.034 | 16,612,330 |
| Trajectory | 2.706 | 2.706 | 0.087 / 0.087 | 8,846,229 |
| Immune | 2.424 | 2.424 | 0.048 / 0.049 | 1,623,518 |
| HLA | 1.929 | 1.929 | 0.032 / 0.032 | 441,464 |

Four observations fail the existing strict budgets: Projection first, Gene
first/revisit, and Immune first. Budgets were not changed. **ACCEPTED** for the
screened content correctness; **INCONCLUSIVE** for a speed improvement. The
prior five-pair Projection/Gene-entry gate and separate per-page memory screen
remain in `RESULTS.md` with their original source identities. Timing-mode RSS is
`NA`; older memory observations are not presented as new measurements.

### Linked targeted comparison

One interleaved B/C pair per first/revisit state compares historical PR6
`cbd65fe0` with `bad37fca`, using the same frozen 1M page harness and fixture.
All four content checks pass. Historical PR6 first completion exceeds the
existing budget; the candidate rows pass. No budget was changed.

| Visit | PR6 usable / complete s | Clean usable / complete s | PR6 / clean websocket B |
| --- | ---: | ---: | ---: |
| First | 2.194 / 8.838 | 1.815 / 6.814 | 16,610,968 / 16,612,330 |
| Revisit | 0.024 / 1.990 | 0.033 / 0.033 | 16,610,120 / 3,651 |

The observed revisit no longer repeats the large supplement. This supports
retaining the visible-only palette/deduplication mechanism; it is **ACCEPTED**
for the tested correctness. A stable latency improvement is **INCONCLUSIVE**
from one pair. No speedup factor is claimed and unaffected pages were not rerun.

## Functional acceptance and test repairs

The separate Spatial screen is below; it was required by the earlier Spatial
ports, not by the later Gene hover gate.

| Scope | Initial cases | Result and follow-up | Conclusion |
| --- | ---: | --- | --- |
| Data IO, Pack, clone, Seurat/export pipeline | 96 | All pass; no skips | ACCEPTED |
| Viewer/context/selection/expression/Spatial/HLA contracts | 305 | Stale positional mock, whitespace assertion and pre-context request expectations repaired; affected files rerun successfully | ACCEPTED |
| Production bundle | 10 | Two assertions expected paths where selectors now expose stable keys; corrected and complete bundle smoke rerun successfully | ACCEPTED |
| Browser app, new modules, trajectory, immune | 41 | Four stale selector/id tests repaired; three Spatial/Linked cases and the Gene case pass focused reruns | ACCEPTED |
| Real Gene hover on 1M | One focused run | No eager metadata request; exact tooltip and repeated-hover deduplication pass | ACCEPTED |
| HLA Data & QC actual click/return | One focused run | Stored typing, four samples, all three tables and stable table nodes; no visible errors | ACCEPTED |
| Dataset lifecycle | Eight switches plus fresh recovery | Counts, fingerprints, generations, actual Projection draws and stale-epoch rejection pass | ACCEPTED |

These are 452 initial cases plus focused reruns, not 452 cases rerun after every
test repair. Repairs update mocks/selectors to the production contract; startup
timeouts were extended without changing performance budgets. Test logs retain
the original failures and successful reruns. No functional pass is inferred from
global Shiny idle; browser tests verify the relevant content/draw/selection state.

The lifecycle run alternates 1,476 and 12,000 cells through generations 1–9.
After-action R RSS changes from 1,223.7 to 1,232.6 MiB; browser RSS from 1,782.7
to 1,928.3 MiB. After disconnect and document reload, browser RSS is 1,682.9 MiB.
This establishes functioning cleanup on fresh document recovery, not a proof of
zero retained memory. Long-session retained-memory attribution is **INCONCLUSIVE**.

The installed standalone Shiny 1.14 transport advertises `$allowReconnect=false`.
Forcing its private `reconnect()` method produced client output-state errors and
a new-session pending generation. An attempted server `onFlushed` sync patch
did not fix this unsupported flow and was reversed exactly; the rejected patch
and failing traces remain in the external evidence directory. Normal websocket
disconnect followed by browser reload passed, with a new epoch and rejection of
the previous epoch. Same-session resumable reconnect is **BLOCKED** by the local
deployment capability; simulated context-sync contracts do not replace that gate.

## Strict RGB screening and remaining work

The separate operation-v3 resource pair samples recursive process RSS after
AppDriver startup. Baseline → candidate R peak is 2,049.9 → 2,145.3 MiB (+4.7%);
browser peak is 2,220.0 → 2,241.9 MiB (+1.0%). These are six-operation session
peaks, not per-page peaks or retained heap. The single pair shows no large
browser-RSS regression, but it does not establish a memory saving or equivalence.
Memory improvement is **INCONCLUSIVE**. Its latency rows are excluded from all
timing summaries. Delayed traffic also changes its operation-window byte counts;
those must not be treated as complete-operation payloads.

Operation-v3 harness SHA-256 is
`ea8b67058bf9a6a6069b6a247d81e4bef86f18d79d9096f06b4a02d18d54bb96`.
Baseline/candidate source and CRB identities match the v2 comparison, but this is
a separate single pair. Exact RGB render-key readiness was 2.063 / 2.073 s;
observed websocket receive was 26,507,395 / 27,035,118 B. **INCONCLUSIVE** for
speed or traffic improvement. No additional pairs were warranted. The baseline's
first-gene observation was 10.058 s and the candidate's 6.889 s in this pair;
that noisy single difference does not supersede the three-pair no-gain result.

| Remaining area | Evidence and next bounded experiment | Conclusion |
| --- | --- | --- |
| First real Gene operation | Three-pair candidate median 7.138 s; page entry is not gene readiness. Selection-core/cell identity and secondary work still dominate traffic. Profile canonical ID delivery before attempting a static identity asset or a smaller core. | INCONCLUSIVE |
| Linked first completion | Primary is usable near 1.8 s; visible supplements finish near 6.5 s and total websocket receive is about 16.6 MB. Profile the supplement build/transfer separately. | INCONCLUSIVE |
| RGB mode transition | Strict-key screen catches the correct final channels; an earlier interim frame exists. Do not infer a completed operation from mode text alone. | INCONCLUSIVE |
| Long session browser retention | About +146 MiB over eight small dataset switches, falling on document reload. Attribution requires a focused retained-object/cache analysis before any leak claim or cache rewrite. | INCONCLUSIVE |
| Brave hardware GPU / true cold OS / session resume | Not available in this WSL software-WebGL2 acceptance environment. Do not extrapolate absolute timings. | BLOCKED |
| Restoring old codecs, chunk readers or broad request framework | Rejected history or duplicate functionality; no new evidence justifies another experiment. | REJECTED |
| Rewriting static geometry, integer index, RGB batching or trajectory core | Mature mechanisms already present and covered. No demonstrated missing mechanism or gain warrants a rewrite. | REJECTED |

Ren was not rerun as a second full page benchmark: no Ren expression backend or
HLA parser production code changed in this acceptance. Its prior exact-value
1.46M expression stress check and three-pair parser A/B remain applicable to
those unchanged mechanisms, with their original source/harness identities in
`RESULTS.md`. They do not establish current full-page Ren latency.

There is no comparable frozen-protocol A/B against the retired slow
`卧槽/pr5_viewer_requests` checkout. A speedup factor versus that branch is
**INCONCLUSIVE**. Against historical PR6, the retained five-pair gate shows no
meaningful Projection/Gene-entry speed change; the clean baseline adds exact
f64 expression transport, the better HLA fallback, narrow lifecycle/UI fixes and
the accepted hover-demand reduction. These are scoped improvements, not a claim
that every page became faster.

## Integration inventory

**Retained from mature PR6:** the complete PR0–PR5 ancestry, thin CRB v2,
BPCells storage-order access and canonical integer indices, stable fingerprints,
safe session clones, validation reuse, Viewer Pack/HTTP resources, static geometry
and GPU reuse, RGB one-materialization access, progressive specialist frames,
trajectory canonical geometry and PR6 UI/long-legend fixes. Many old `卧槽`
changes already had equivalent or better implementations here; they were not
duplicated.

**Ported selectively:** the better HLA parser/early guard/hidden gates, optional
hover-field handling, pending-choice/nonce and dataset lifecycle fixes, stable
DataTables/HLA QC hosts, Selectize listener cleanup, HLA pending-state retirement,
Spatial hidden Moran and persistent controls, and visible-only Linked palette
deduplication. The exact dirty-work disposition is in `OLD_BRANCH_DISPOSITION.md`.

**Added or corrected:** exact f64 expression-color transport, distinct-value
Projection hover formatting with identical text, and the Gene hover-demand gate.
No precision-reducing old `sparse-f32` color path was restored.

**Excluded:** broad resource monkey patches/observer rewrites, the old Plotly
request framework, no-benefit generic reader/chunking, the memory-regressing HLA
parser variant, no-stable-gain graph formatter, quantization, and already-reverted
shared-codec/component experiments. Old generated benchmark output is archived
outside the clean source tree. The ineffective reconnect patch from this
acceptance was also rejected and reversed.

### Commits since historical PR6

| Commit | Purpose |
| --- | --- |
| `73f746ac` | Establish clean PR6 baseline and integration audit |
| `000bf411` | HLA hot paths and lossless expression transport |
| `67735831` | Dataset selection races and table widgets |
| `8c06ae19` | Frozen measured protocol and exact color headers |
| `0e2ec540` | Selector lifecycle and Ren HLA verification |
| `87c616c5` | Results and stable HLA QC hosts |
| `1450bb38` | Five-pair key-page comparison |
| `eb432a11` | Last valuable old dirty lifecycle/Spatial/Linked fixes |
| `bad37fca` | On-demand Gene hover metadata |
| This report's acceptance commit | Test contract repairs, reproducible focused probes and final evidence; no additional production change |

The earlier integration history is preserved. This retirement/acceptance did not
rewrite it or replay whole old branches.

## Independent Spatial acceptance

The new canonical fixture is versioned separately in `README.md` and
`final-acceptance/spatial-fixture.identity.json`: exactly 1,000,000 cells,
1000×1000 canonical grid, fixed alignment image, fixed CRB hash and cell
fingerprint. Its Viewer Pack manifest SHA-256 is
`a9972d8cf05168701032f862577be02f77fc67982fc241f37fd44b22b2e1516a`.
Historical PR6 `cbd65fe0` and clean production `bad37fca` use the same fixture,
frozen page harness, renderer and first/revisit definition. One pair per state;
all four observations pass content and existing budget checks.

| Visit | PR6 usable / complete s | Clean usable / complete s | PR6 / clean websocket B |
| --- | ---: | ---: | ---: |
| First | 1.636 / 1.636 | 0.594 / 0.594 | 40,821 / 34,693 |
| Revisit | 0.017 / 0.017 | 0.059 / 0.059 | 30,210,128 / 3,870 |

**ACCEPTED** for the screened Spatial correctness and no observed first-entry
regression. A stable timing gain is **INCONCLUSIVE** from one pair. The primary
event is the harness completion boundary here; large baseline revisit traffic
can arrive after the already-visible primary frame. It is not evidence that
30 MB transferred in 17 ms. This new series is not pooled with historical ~1.7 s
measurements. Alignment/background switching, bounds and hidden-Moran behavior
also have passing focused demo/contract tests. No Spatial expression benchmark
is inferred from a fixture whose spatial expression matrix has zero gene rows.

## Reproducible evidence

`final-acceptance/MANIFEST.json` hashes the compact raw TSV/JSON evidence copied
into the repository. Full logs, failed diagnostics, R profiling output, the
rejected reconnect patch and runner sources remain in the external acceptance
backup directory. Timing and memory runs are distinct. Page source identities
are in each manifest; any `git_dirty=TRUE` on `bad37fca` comes from acceptance
tests/reports, with no production-source diff. The final report commit adds no
production changes beyond `bad37fca`.

Backup-before-delete identities are in
`C:\Users\wangx\Projects\CerebroNexus-backups\2026-10-10-pr6-clean\BACKUP_MANIFEST.json`.
That bundle retains all four deleted branch tips; the exact dirty patch and
untracked archive retain the old uncommitted work. The optional Codex app
attachment at `.codex/worktrees/pr6-clean-perf/CerebroNexus` is stale and absent
on disk; app archival refused it as owned by another task. This ownership check
was not bypassed and is separate from actual Git worktree cleanup.
