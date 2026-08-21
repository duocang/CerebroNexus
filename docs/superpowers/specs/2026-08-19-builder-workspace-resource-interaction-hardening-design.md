# Builder workspace resource and interaction hardening

## Goal

Make project restore, save, build retry, dataset switching, and Spatial preview remain responsive and internally consistent for large projects and long-lived sessions.

## Approved scope

- Preserve the current Spatial sidebar wheel-to-page-scroll behavior. It is intentional product behavior.
- Prevent global shortcuts from escaping an open modal.
- Roll back failed optimistic dataset switches and make the same target retryable.
- Make a new Build attempt authoritative over a stale terminal result and give the blocking overlay a complete focus lifecycle.
- Prevent stale delayed Spatial-section messages from overwriting newer state.
- Remove avoidable idle DOM work, stale observer targets, and per-pointer full-sample computation.
- Avoid repeated restore-time artifact hashing and image hydration.
- Close the lifecycle of managed sources, checkpoints, generated artifacts, content-addressed generations, and preview caches.
- Keep Spatial preview and uploaded-image memory bounded in the Shiny main process.

## Interaction design

An open modal owns keyboard input. Page-level shortcuts do nothing until the modal closes. A dataset switch records the last server-authoritative selection; success commits the optimistic target, while error or timeout restores that selection and permits a new request. During Build, active flow states (`preparing`, `queued`, `building`) take precedence over an older result. Focus moves to a focusable operation-status element before the shell becomes inert, then moves to the terminal result or back to the initiating action when the operation ends.

Delayed client reconciliation uses generation tokens. A callback may only write state if its generation is still current.

## Resource design

Restore computes one lightweight status snapshot per dataset. Status checks verify paths, members, sizes, and fingerprints without turning image files into data URIs; hydration happens only for a selected dataset that needs it. The snapshot is passed through confirmation, artifact restoration, and check-mark creation instead of recomputing hashes.

After source synchronization commits, the live dataset entry is rebased to the managed source blob and the owned session-source copy is removed when it is no longer needed. Artifact registration uses a staged bundle and reuses fingerprints; when source and destination share a filesystem, it promotes the checkpoint by atomic rename rather than copying and rehashing the same bytes. Successfully committed manifests trigger conservative reachability cleanup that retains current, backup, and active-operation references.

Spatial preview obtains layer cell membership without joining expression values. Only sampled points, bounds, and aggregate coverage facts cross the worker boundary. Uploaded raster files are rejected before unbounded decode when their declared dimensions or decoded pixel count exceeds the budget; retained editable pixels are bounded to the same preview envelope as the encoded image.

## Browser performance design

Timers exist only while they have active work. DOM writes are skipped when text is unchanged. Observer target sets are pruned when Shiny removes nodes. Spatial hover processing is attached to the canvas, coalesced to one animation frame, and reuses a precomputed rotation transform. Existing reduced-motion behavior remains unchanged.

## Error handling and safety

- Failed restore verification remains visible and never silently downgrades to a reusable artifact.
- Cleanup runs only on normalized, owned managed paths and never follows an unmanaged symlink.
- Garbage collection is post-commit and best-effort; a cleanup failure cannot invalidate a successful save.
- Existing project `.bak` recovery and concurrent-window revision protection remain authoritative.

## Testing contract

Add focused unit/contract/browser tests for each state transition and resource boundary before changing production behavior. Per the explicit task constraint, the tests are added but are not executed locally in this implementation run.
