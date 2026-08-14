# Spatial live-preview review fixes design

## Status

Approved direction: fix the four correctness gaps confirmed after the Spatial
live-preview split, keep coordinate editing rotation-only, and require exact
browser and persistence regressions before the branch is considered ready.

## Goal

Preserve next-frame client-side Spatial rendering while making server Plotly
replacement, legacy coordinate transforms, dataset switching, and
matching-label apply-to-all deterministic and lossless.

## Confirmed root causes

### Server-render restore can cache a mixed Plotly source

`shiny:value` arrives before Plotly has necessarily finished consuming the new
widget value. The current fixed 80 ms timer can therefore rebuild the draft
source from an old `layout.meta.builder_alignment_rotation` and already-restyled
`plot.data`. A subsequent slider change rotates the traces from the wrong
baseline.

### Rotation-only normalization stops at the preview boundary

The Spatial editor forces coordinate scale to one for preview and Save, but the
original non-one scale can survive in dataset settings, frozen plans, and the
`exportFromSeurat()` call. A legacy entry can therefore display one geometry and
export another.

### Dataset selection bypasses the unsaved-draft gate

The dataset rail mutates `current_dataset` before the Spatial editor can decide
whether to Save, Discard, or Cancel. The `current()` observer then clears the
cache-only draft.

### Apply-to-all does not own target coverage or the transaction

Target records are finalized without a target preview, so they inherit stale
`outside` and `total` diagnostics and are marked saved. The source is committed
before target image decoding and encoding finish, so a later target failure
leaves a partial operation.

## Considered approaches

### Plotly restoration

1. Increase the fixed timeout. Rejected because render time depends on device,
   graph size, and browser scheduling; no timeout proves completion.
2. Restore on every `plotly_afterplot`. Rejected because the client draft
   renderer's own `restyle` and `relayout` operations also emit that event.
3. Add a server render token and consume a pending restore only after an
   `afterplot` carrying a different token. Chosen because it identifies the
   structural render rather than guessing its duration.

### Legacy coordinate scale

1. Preserve a hidden non-one scale. Rejected because the approved Builder
   product contract is rotation-only and the current preview already presents
   scale one.
2. Force scale one inside the public `exportFromSeurat()` API. Rejected because
   non-Builder callers still legitimately use the general scale contract.
3. Canonicalize scale one at Builder state and plan boundaries. Chosen because
   Builder preview, frozen plan, and export then agree without narrowing the
   public API.

### Dataset switching

1. Revert the selection after it occurs. Rejected because reactive observers
   can already clear state before the revert.
2. Let the rail delegate selection through a pre-commit callback. Chosen because
   the Spatial editor can either commit immediately or hold the callback behind
   the existing Save/Discard/Cancel dialog.

### Apply-to-all coverage

1. Retain stale coverage. Rejected as false saved evidence.
2. Mark copied targets unsaved. Safe but rejected because it degrades
   apply-to-all into a parameter-copy shortcut.
3. Encode all records in memory, request exact per-section coverage from the
   worker, and commit once only after every result passes. Chosen because it
   preserves the feature's saved semantics and makes the operation atomic.

## Architecture

### Render-token handshake

Every server execution of the Spatial `renderPlotly` output increments a local
monotonic render token and writes it into `layout.meta`. The token is not a data
revision and is not persisted; it identifies one delivered Plotly structure.

On `shiny:value`, JavaScript:

1. records the current plot element and its current render token;
2. invalidates queued and cached client draft work;
3. marks one structural restore as pending;
4. does not schedule a timer.

The shared `plotly_afterplot` handler, and the initial `enhancePlot` attachment
path for a newly created element, check the pending restore. They consume it
only when the current plot's render token differs from the recorded token. They
then clear the cached source and schedule one draft on the next animation frame.
An old client restyle can emit `afterplot`, but it still carries the recorded
token and therefore cannot consume the pending structural restore.

Native and jQuery `shiny:value` delivery are coalesced by retaining the first
pending record until it is consumed. A newer server value replaces a pending
record only after capturing the last rendered token, never by using elapsed
time.

### Builder-only coordinate canonicalization

The Builder state normalizer and frozen-plan coordinate normalizer both emit:

```r
list(
  schema_version = 1L,
  rotation_degrees = normalized$rotation_degrees,
  scale = 1
)
```

The editor's existing read-time guard remains as defense in depth. The generic
coordinate-transform helpers and `exportFromSeurat()` retain support for
non-one scale outside Builder. Loading a legacy Builder entry therefore migrates
its in-memory state to scale one, and freezing an older or fixture-provided
entry repeats the same canonicalization before build.

### Pre-commit dataset selection

`builder_dataset_rail_server()` gains an injected selection gate with an
immediate default so existing callers preserve their behavior. The gate receives
the requested dataset id and a one-shot commit closure. The closure re-reads the
current store, validates that the requested id still exists, performs the state
selection, and invokes `on_select`.

The Spatial alignment server exposes a dataset-switch request method. With no
unsaved draft it calls the closure immediately. Otherwise it stores the source
dataset, target dataset, and closure and opens the same Save/Discard/Cancel
dialog used for section and image changes:

- Save finalizes the current record, then calls the selection closure.
- Discard restores the baseline, then calls the selection closure.
- Cancel clears the pending request and leaves the current dataset unchanged.

Only one pending navigation target is allowed. A dataset change from another
source invalidates the pending request rather than executing a stale closure.

### Atomic apply-to-all coverage

Apply-to-all becomes an asynchronous transaction with an explicit pending
contract:

1. Capture dataset id, snapshot identity, dataset revision, source section,
   image label, current draft parameters, and rotation-only coordinate
   transforms.
2. Finalize the source and every matching target in memory. Decode and encode
   every image before changing the store.
3. Build a named map of final record bounds and enqueue a new worker request for
   full-coordinate coverage of every affected spatial section.
4. In the worker, load each section's full coordinates, apply that section's
   canonical coordinate transform, and compute `builder_bounds_cover()` against
   its final record bounds.
5. On response, verify the request token, dataset/snapshot/revision, active
   source section and label, and unchanged draft parameters. Missing sections,
   malformed coverage, worker errors, or any `outside > 0` abort the operation
   without committing source or targets.
6. Attach the returned `outside` and `total`, mark all affected records saved,
   and call `commit_images()` once.

While a transaction is pending, a second apply-to-all request is ignored and
the confirmation action reports that coverage is still being checked. Ordinary
slider edits may continue, but they make the captured contract stale and cause
the response to abort without a commit.

`builder_alignment_apply_transform_to_matching_label()` also computes its
intermediate bounds from `builder_alignment_oriented_bounds()` before applying
translation and scale, matching the sibling helper and the final record
contract.

## Error handling

- A Plotly value that never produces a new render token leaves the restore
  pending; the next server render can satisfy it, and no mixed source is cached.
- Invalid legacy coordinate transforms still fail through existing validation;
  only a valid positive scale is canonicalized to one.
- A stale or missing dataset selection closure is discarded with no state
  mutation.
- Apply-to-all reports image read/encode errors, worker errors, stale requests,
  malformed coverage, and uncovered sections separately. None of these paths
  writes a partial image collection.

## Test design

### Render race

The real Builder browser test performs a 45-degree draft, saves it while the
server Plotly render is delayed beyond the former 80 ms window, then moves to 50
degrees. The final point coordinates must equal one canonical 50-degree
rotation from the server baseline, not 45 plus 50. The drag phase must still
produce zero server Plotly values.

### Legacy scale

Unit tests feed `scale = 1.7` through Builder state normalization and plan
freezing and expect scale one in both. A separate coordinate-contract test keeps
proving that the generic transform/export layer accepts non-one scale.

### Dataset navigation

Server tests cover immediate switching without a draft and Save, Discard, and
Cancel with a dirty draft. They assert both the selected dataset and the source
record state, including that Cancel never transiently changes `current()`.

### Apply-to-all

Worker/unit tests use multiple sections with different coordinates and rotated
image extents. They assert per-section coverage, oriented target bounds, and one
final commit. Image failure, worker failure, missing coverage, outside cells,
and an edited-during-request contract each assert zero commits.

### Final regression

Run the focused Builder Spatial/state/plan/worker tests, the existing real
Chrome Spatial live-preview test, and the repository's normal final check once
the logical commits and unrelated user changes remain stable.

## Scope and safety

- Do not modify or stage the existing unrelated local changes in
  `inst/builder/build.R`, `inst/builder/plan/freeze.R`,
  `tests/testthat/test-builder-coordinator.R`, or
  `tests/testthat/test-builder-plan-core.R`.
- Do not force-push the rewritten branch during implementation.
- Do not redesign Spatial controls, restore coordinate scale UI, or change the
  generic coordinate-transform API.
- Keep live slider rendering client-only; worker traffic is permitted only for
  structural preview requests, Save, and the explicit apply-to-all coverage
  transaction.
