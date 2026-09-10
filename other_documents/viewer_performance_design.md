# Viewer interaction performance design

## Goal

Reduce Viewer startup and interaction latency on large datasets without changing UI, features, public APIs or asynchronous execution semantics.

## Constraints

- Do not add dependencies, background workers, asynchronous runtimes or cross-session caches.
- Preserve current errors, progress reporting and per-session isolation.
- Keep generated Viewer runtime code under `inst/`.
- Reuse the existing Canvas renderer, Shiny lifecycle and expression backend interfaces.

## Design

1. Deliver the first reactive event immediately and debounce only later changes.
2. Keep hidden outputs suspended until their sidebar page is first visited.
3. Build specialist pages from specialist payloads without forcing Linked Views to construct its full bundle.
4. Build projection hover data only for displayed cells and send columnar fields instead of complete HTML strings.
5. Use a lazily built screen-space grid for pointer hit testing instead of scanning every cell on every event.
6. Draw transient hover and selection marks on the overlay Canvas without repainting the base layer.
7. Coalesce pointer-driven pan, orbit and lasso redraws to one browser animation frame while rendering final mouse-up state synchronously.
8. Cache stable Spatial image encoding and hull geometry within the current session and reactive scope.
9. Keep per-field paint-order and clipping caches so one panel does not evict another panel's calculation.

## Non-goals

- No cross-session data sharing.
- No worker lifecycle or cancellation framework.
- No speculative preload of hidden Linked Views.
- No WebGL renderer, binary transport or level-of-detail system.
- No new public API.

## Error handling

Existing validation and `tryCatch()` behavior remains authoritative. Optimized payloads and caches must preserve missing-cell handling, cell order and dataset identity checks.

## Verification

- Run focused Viewer synchronization and coordinated-view tests.
- Compare backend, Viewer-interaction and cumulative revisions on the same 1M-cell CRB.
- Alternate browser execution order and report medians rather than fastest runs.
- Reject runs with an incorrect cell count, an empty Canvas or browser error logs.
- Record time, R allocation, Shiny RSS and wire-payload size where applicable.
