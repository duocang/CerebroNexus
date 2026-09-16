# Instant dataset information and lazy Viewer loading

## Context

The Viewer currently renders the three Data Info cards through `data_set()`. Selecting a 1.46-million-cell CRB therefore blocks the cards behind CRB deserialization, BPCells attachment, cache cloning, and unrelated reactive invalidations. A cold Ren switch takes about 19.5 seconds and a process-cache hit still takes about 12.3 seconds even though the cards only need cell count, organism, and export date.

## Goals

- Update the selected dataset label and Data Info cards without loading the runtime Cerebro object.
- Keep BPCells expression attachment lazy until a data-dependent page actually requests it.
- Preserve generated-app portability, uploads, legacy CRBs, multiple sessions, and the existing process cache.
- Add no dependency and no new sidecar file.

## Non-goals

- Do not move R6 or BPCells objects between R processes.
- Do not add `future`, `mirai`, or a second Shiny worker architecture.
- Do not precompute page-specific Projection, Linked Views, or immune-repertoire payloads in the catalog.

## Design

### Dataset catalog

Each configured dataset receives one compact catalog entry containing its configured label, exact runtime path, cell count, organism, and export date. `createShinyApp()` already reads every CRB during preflight, so it extracts the catalog entry from that existing object and stores the complete catalog in `cerebro_config.rds` under the internal `.dataset_catalog` option. `launchCerebro()` builds the same in-memory catalog once from configured file-backed CRBs before returning the Shiny app. Variable-backed datasets and uploads remain uncatalogued and use the existing loaded-object fallback.

The existing configuration object is the single catalog artifact. A separate QS2 file would duplicate lifecycle and path bookkeeping for only three scalar values.

### Runtime selection and loading

The selected dataset path remains authoritative. A `current_dataset_info()` reactive resolves the matching catalog entry without calling `data_set()`. The Data Info value boxes read only this reactive when an entry exists.

`data_set()` gains a request gate. While the active page is Data Info and the selected path has a catalog entry, observers that accidentally touch `data_set()` suspend before CRB loading. Selecting a different dataset on Data Info resets the gate. Opening any data-dependent sidebar page opens the gate and loads the selected dataset through the existing cache. Switching datasets while already on a data-dependent page loads the replacement immediately. An uncatalogued upload or variable-backed dataset opens the gate and retains current behavior.

This is demand-driven loading, not simulated background concurrency. It keeps Data Info responsive and avoids work the user may never request. A later event-loop callback would only defer blocking work, while a subprocess would require unsafe or expensive R6/BPCells serialization.

### Lazy process-cache clones

The process cache stores the deserialized CRB prototype before an external expression handle is attached. Each session shallow-clones that inert prototype first, then installs its own lazy external-expression binding and runtime sidecar roots. This preserves session isolation without forcing the delayed BPCells binding during `R6::clone()`.

### Compatibility and failure behavior

Catalog entries are validated before use. Missing or malformed entries fall back to the existing loaded-object path rather than showing fabricated values. Generated apps retain relative runtime paths; direct launches retain normalized configured paths. Uploaded datasets continue to load before their cards render. Backend-plan and sidecar validation remain unchanged.

## Verification

- Unit tests prove preflight and direct launch produce identical catalog shapes.
- Runtime tests prove cached session clones remain distinct and their external expression binding remains lazy.
- Viewer contract tests prove catalog-backed Data Info helpers do not reference `data_set()` and the request gate opens for non-Data-Info pages.
- The existing Viewer and generated-app test files remain green.
- A two-dataset browser benchmark confirms the Ren Data Info cards update within 500 ms of selection and no BPCells attach is logged until a data-dependent page is opened.

