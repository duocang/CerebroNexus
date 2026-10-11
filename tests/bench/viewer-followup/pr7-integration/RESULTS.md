# PR7 Viewer selective integration results

2026-10-10. Target branch `卧槽/pr6_clean_perf`, existing checkout
`C:/Users/wangx/Projects/CerebroNexus`. Starting HEAD
`2dbd6668744f5eb6c6898771a1c2cc566b0aa6c3`, clean. Frozen source
`cb770fe9328fb7037a0008a1bb54fc2744cf86ae`, merge base
`cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303`. Local tracking and read-only remote
query both returned `75401c75c9926bf6948ec7736ce5483fe0e4d086` at audit start.
The local PR7 ref was checked again after implementation and remained unchanged.
No fetch or source-baseline drift. Mechanism/dependency matrix: [AUDIT.md](AUDIT.md).

**Implementation and focused offline validation complete; native Brave
acceptance BLOCKED.** These are correctness fixes, not a new performance series.

| PR7 item | Target gap / disposition | Source | Production commit | Evidence |
|---|---|---|---|---|
| Canonical Gene selection summary/table | Compact metadata ordinals did not match stable barcode selection; fixed only the two gated outputs. | `cb770fe9` | `9b2edc60` | Old bodies fail 6 assertions; new bodies pass 10. Real Mouse/Demo output-body check passes. |
| Early row-key fallback | Already covered by pending-ID gate and republishing the current selection after aux. Numeric-string guessing rejected. | `4d6d14d9` | None | Existing specialist pending/ready/latest/clear tests, canonical numeric-barcode test. |
| Hidden/already-painted progress | Close off-page and skip duplicate painted keys; isolate visibility/request reads to avoid new expression dependencies. | `21025249`, `e9a28c9a` | `023b7324` | Real Shiny testServer progress handles and expression-read counters. |
| Obsolete group summary | Reject absent or wrong-length group column; normal validation clears old output. | `21025249` | `023b7324` | Production render-body test; valid replacement group works; no cancelOutput retention. |
| Dataset context/async state | Existing context/epoch/generation, token, invalidation and pending aux guards already cover the referenced mechanisms. | `5c5ca28f` | None | Current source audit + 101 context and 36 shared-selection assertions; no second framework. |
| ROI dataset/rotation | New ROI/content/coordinate feature closure, absent from target; DEFERRED. | `91e87d77`, `6c936a53` | None | Dependency closure in AUDIT.md; no isolated current bug demonstrated. |
| Background removal | Target has explicit No Background choice; PR7 patch repairs its own changed selector. | `dd3313e6` | None | Current selectInput/normalization source; no Spatial edits or Spatial regression run. |

## Validation conditions and evidence

- WSL R **4.6.1**, package loaded from this checkout with `pkgload::load_all`.
  This was not Windows-native R. Tests are offline R/Shiny/Node tests.
- `targeted-tests.txt`: **198 passing expectations**, no failures/warnings/skips.
  Covers canonical selection, pending/ready identities, context/generation/epoch,
  hover demand and progress/group lifecycle. `compatibility-tests.txt` adds
  passing expression f64/NA/zero, no-primary-metadata, RGB selector/dataset,
  retained geometry/order, stale-paint and incremental renderer tests.
- `baseline-tests.txt`: corrected negative-control replay, **6 selection and
  6 lifecycle/group failures**. Baseline files are read into a temporary
  directory; the checkout is never overwritten. The earlier testServer harness
  mistakenly read copied mutable scalar bindings; corrected before this saved
  baseline. See `lifecycle-tests.md`.
- `fixture-check.json`: actual 1,000,000-cell Mouse CRB and 1,476-cell Demo,
  Mouse -> Demo -> Mouse (generations 1/2/3), production render bodies, canonical
  indices `[1000000,1,500000]` / `[1476,1,738]`. Summary/table and repeated table
  output agree; previous generation selection is rejected. Expression values
  are explicit test sentinels `[0,NA,9]`, not a new expression-backend test.
  Zero `getMetaData` calls **from selected outputs before selection**; the
  fixture setup itself reads compact metadata (and ordinary Demo metadata).
- Mouse SHA-256 remains
  `cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38`.
  Demo SHA-256 and selected barcode samples are in `fixture-check.json`.
- `git diff --check` passes. Source/evidence hashes are in `IDENTITY.json`.
  Source/harness identity is separate from commit timestamps. No benchmark,
  global-idle readiness substitution, new ready definition or A/B claim.

Replay from repository root in WSL:

```text
Rscript tests/bench/viewer-followup/pr7-integration/reproduce.R
Rscript tests/bench/viewer-followup/pr7-integration/run-tests.R
Rscript tests/bench/viewer-followup/pr7-integration/fixture-check.R <repo-root> <Mouse-crb> <output-json>
```

`reproduce.R` intentionally reports baseline test failures; the candidate runner
fails on a regression. The saved raw outputs distinguish these two purposes.

## Native Brave boundary

The existing Windows Brave window titled CerebroNexus was selected from returned
app/window inventory. Computer Use stopped during window/state acquisition:
“could not determine the current browser URL on Windows with enough confidence
to enforce policy.” No browser input was issued; no fallback automation or safety
bypass was attempted. No new R browser service, test tab, viewport, screenshot,
browser timing or console pass exists for this candidate.

Consequently **native Demo/1M hover, real box drag before/after deferred IDs,
visible summary/table consistency, page revisit, progress transitions and
Mouse -> Demo -> Mouse remain unverified in Brave at this candidate**. The
offline fixtures establish the narrow identity/lifecycle contracts, not those
physical interactions. Earlier baseline Brave acceptance remains historical.
No new functional regression appears in completed tests, but a full native
functional pass is not claimed. This is an external validation stop, not RAF
investigation; RAF cadence was neither measured nor changed.

## Cost and semantic boundaries

Only four Gene R files changed in production. No Builder, Spatial, JS renderer,
expression reader, RGB, geometry, barcode order, precision, wire, cache or
dataset framework changes. Canonical barcode access stays behind the existing
nonempty/generation-validated selection gate and after demanded aux identity.
It adds the correct barcode slice to selected output preparation; no new eager
million-row materialization, primary payload, fetch or retained cache is added.
The table's corrected barcode contents are an intentional output correction.
Resource reasoning is architectural; no quantitative heap/RSS equivalence test
was performed. Static resources are not conflated with WebSocket payloads.

Progress remains a UI handle, separate from painted readiness. Unpainted work
still opens progress, stale acknowledgements cannot close a different key, and
navigation/request snapshots do not reread expression. Existing paint gates,
selection semantics and context validation remain in force. Default `req`
validation clears obsolete group output; it does not keep old results visible.

## Closed performance directions and retained limits

Stable Gene <=3 seconds remains unachieved and closed to further optimization.
Brave ~1 Hz RAF root cause remains unresolved; no occlusion claim. Backend
dimension-only prewarm and failed Linked metadata/index/cache experiments stay
rejected. Linked functionality retains the accepted `2dbd6668` pinned-tooltip
fix; no Linked performance work. Trajectory keeps canonical/excluded-index/State
and demand auxiliary paths; historical 0.6-second conditions remain incomparable.
No Ren or unaffected-page runs. Resumable reconnect (`allowReconnect=false`),
long-term whole-application heap bounds, independently timed Trajectory complete,
and a unified old-PR5/PR6/current Brave comparison remain outside this result.

The two implementation commits preserve independent rollback boundaries. The
following documentation commit adds this report and the offline fixture evidence;
it does not change production. Nothing was pushed and PR7 was not modified.

## Final Viewer closeout attempt — 2026-10-10

Starting HEAD `33c5d71c3aaa7b07a6617260a0644673811c6c40`, branch
`卧槽/pr6_clean_perf`, existing checkout, clean working tree. No branch or
worktree was created. This attempt was limited to the requested native Windows
Brave Gene smoke test; no benchmark, profiling, optimization or repeat audit.

The fresh app inventory returned the existing `CerebroNexus - Brave` window.
Window/state acquisition again stopped with the exact tool response:

> Computer Use has been stopped for this turn because it could not determine
> the current browser URL on Windows with enough confidence to enforce policy.

**Real Brave acceptance remains incomplete due to the tool restriction.** No
browser input was issued and no restriction was bypassed. Gene display, hover,
box selection, visible summary/table consistency, return-page progress,
Mouse -> Demo -> Mouse isolation, stale-generation behavior, and browser/R
error checks were not newly exercised or certified. No new code-level failure
was observed; this is not a claim that an unexecuted smoke test passed.

Production code and the existing 198 targeted assertions, compatibility tests
and offline Mouse/Demo results are unchanged; no tests were rerun. Only this
closeout record and its report hash were updated. No service or test tab was
started. No push was performed.

The user-defined external-tool stop condition has been reached. **Viewer work
is closed with the explicit native-UI validation exception above.** No further
Viewer optimization or investigation is scheduled. Spatial ROI/background and
their Builder/content/coordinate contract dependencies remain deferred to the
PR7 Builder stage or a separately scoped task. Builder implementation was not
started during this closeout.
