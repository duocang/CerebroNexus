# Builder dataset-switch feedback and Spatial preview reuse

## Problem

Loaded datasets remain in the background Worker, but selecting another dataset still feels stalled because the client waits for a Shiny round trip before changing the rail, the Configure workbench is regenerated, and Spatial alignment state is deliberately cleared before a fresh `spatial_preview` request. Spatial previews are not cached, so revisiting a dataset repeats Worker computation, transfer, and canvas rendering.

The UI provides no dataset-switch-specific feedback during this interval. The result looks like a dead click even when the request is progressing normally.

## Goals

- A dataset click must produce visible feedback immediately, before the server responds.
- The selected rail row must change optimistically and later reconcile with the authoritative server patch.
- The right workbench must retain its shape under a light loading veil instead of disappearing or showing a blank panel.
- Copy must describe the actual phase: `Switching dataset…` followed by `Preparing Spatial preview…` when Spatial work is required.
- A previously completed Spatial preview must be reused while its dataset/section/settings contract is unchanged.
- Rapid repeated selections and stale Worker responses must never clear the newest loading state or render the wrong dataset.
- Failure must release the loading state and leave an actionable error rather than an indefinite spinner.

## Non-goals

- Do not invent a percentage progress value; the Worker does not expose meaningful fractional progress for preview computation.
- Do not keep a permanently mounted Configure DOM for every dataset.
- Do not redesign the dataset rail or the Spatial controls.
- Do not include broad Configure-state memoization in this change. It can be measured separately after the largest repeated Spatial work is removed.

## Chosen interaction

The approved design is option A: a light workbench veil.

1. Clicking a ready dataset immediately marks that row selected in the browser and starts a new client switch generation.
2. The existing right workbench remains visible but becomes non-interactive under a translucent veil with a small indeterminate spinner.
3. Initial copy is `Switching dataset…`.
4. Once the server has accepted the target and Spatial preview work is pending, copy becomes `Preparing Spatial preview…`.
5. The veil fades out only when the target workbench is usable. For a Spatial dataset, that means the matching Spatial canvas scene is ready; for a dataset without Spatial content, it means the matching Configure workbench has flushed.
6. Server rejection or Worker failure removes the veil, reconciles the rail to server truth, and exposes the existing error notification.

Reduced-motion users receive the same state changes without animated rotation or fading. The veil is limited to `#workbench`; the dataset rail stays visible. The overlay uses `role="status"`, `aria-live="polite"`, and `aria-busy` on the workbench without moving keyboard focus.

## Client state and reconciliation

Add one small client-owned dataset-switch state with:

- target dataset id;
- monotonically increasing generation;
- phase (`switching`, `spatial`, or idle);
- start time, used only for timeout/error recovery rather than a displayed estimate.

Every click supersedes the previous generation. Custom messages and DOM readiness signals carry or resolve to a dataset id, and may complete the overlay only if they still match the latest target. The normal `builder_dataset_rail_patch` remains authoritative: optimistic classes and `aria-current` are temporary and are overwritten by reconciliation if the server chooses another state.

Clicking the already-current dataset is a no-op and must not show the veil. While a switch is pending, another dataset row remains selectable so the user can change their mind; mutation controls inside the workbench are inert.

## Spatial preview cache

Introduce a session-local cache parallel to the existing projection and trajectory preview caches. A record is keyed by dataset id and guarded by the existing Spatial preview contract:

- snapshot identity;
- section;
- default projection;
- default group;
- assay;
- layer.

The cache stores `pending` or `ready` state and the completed preview payload. On a ready hit, switching restores `alignment_preview` and `spatial_coords` immediately and skips the Worker request. On a miss, the request is enqueued once and the completed payload is stored before it is applied to the active dataset.

Changing any contract field naturally misses the cache. Replaced snapshots therefore cannot reuse old coordinates. A stale result may populate only the exact contract it was requested for and may update the visible preview only when its dataset and section still match the current selection.

## Server-to-client state boundary

The browser owns the immediate `switching` state. The server reports only authoritative milestones:

- Spatial preview requested or restored for a target dataset/section;
- matching Spatial canvas scene emitted and usable;
- matching non-Spatial Configure workbench flushed;
- request failure/recovery.

Messages include the dataset id and, where needed, the server request generation. The client ignores any milestone that does not match its latest pending target. This keeps the loader honest without delaying initial feedback on a server round trip.

## Error and timeout behavior

Worker and protocol errors clear the matching pending state and use the existing Builder error surface. A defensive client timeout must not pretend the operation succeeded: it changes the overlay to `This is taking longer than expected…` and keeps the latest server state authoritative. A subsequent matching ready or error message still settles it. The timeout is diagnostic UX, not cancellation.

## Test contract

- Client contract: clicking a different ready row immediately updates `aria-current`, sets `#workbench[aria-busy=true]`, and shows the switching veil before a server rail patch.
- Reconciliation contract: a server rail patch can accept or roll back the optimistic selection.
- Race contract: A → B → A ignores stale readiness for the first A/B generations.
- Spatial cache contract: first visit enqueues one preview; a matching revisit restores the cached payload without a second enqueue.
- Invalidation contract: changing any Spatial preview contract field enqueues a new request.
- Completion contract: a matching Spatial scene or non-Spatial flush clears the veil; an error releases it with existing error feedback.
- Accessibility contract: status semantics and reduced-motion CSS are present, while focus remains on the clicked dataset control.

## Expected result

The rail responds within the click frame instead of waiting for Shiny. A first visit still shows honest progress while the Worker calculates the Spatial preview. Revisited unchanged datasets avoid repeated Spatial calculation and should require only Configure/canvas reconciliation. At no point does the page appear unresponsive or display a fake percentage.
