# Shareable Linked View Configuration

Date: 2026-08-20

## Context

Linked views already coordinates every visible lens through one browser-owned
selection keyed by cell index. The browser reports selected cell barcodes to the
Shiny server for the selected-cell plot and table, but the rest of the state is
ephemeral: projection and Spatial choices, filters, colour mode, point display,
panel focus, viewports, image alignment, and Trekker controls disappear when the
session ends.

Builder Project persistence solves a different problem. It is a directory
transaction containing source data, immutable configuration sidecars, assets,
artifacts, and a revisioned manifest. A generated or directly launched Viewer
does not depend on that directory and may not be able to write to it. Linked
view state therefore needs its own portable contract instead of becoming part
of `builder-project.json`.

## Goal

Let a user select scatterplot points in Linked views, save the coordinated state
as a versioned JSON file, copy the canonical JSON for sharing, and restore the
same state in another compatible session.

Success means that restoring a configuration reproduces the active cohort and
the context needed to interpret it across all linked lenses, while refusing to
apply the configuration to the wrong cell population.

## Options Considered

### 1. Browser-only export and import

The existing client can serialize its private variables directly and use a Blob
and `FileReader` for download and upload. This is the smallest implementation,
but it makes the browser the only schema authority and gives the server no
opportunity to bound, canonicalize, or reject hostile uploads.

### 2. Dedicated Viewer contract with client capture and server validation

The client captures and applies state because it owns the coordination engine.
The server owns the versioned plain-JSON schema, size limits, semantic
normalization, dataset compatibility check, canonical encoding, download, and
sanitized error messages. This keeps the trust boundary in R without moving
high-frequency interaction onto the server.

This is the selected option.

### 3. Builder Project sidecar or project archive

A project sidecar could store named Viewer states beside a Builder dataset, but
it would couple an independently deployable Viewer to Builder storage and write
permissions. A complete Builder exchange archive is a separate, substantially
larger feature involving sources, assets, artifacts, archive extraction, and
project forking. It is not required to share a Linked views configuration.

## Product Experience

Linked views adds an always-available **Save / share** button to the top control
bar. It is disabled until a compatible data bundle is visible. The button opens
a compact native dialog with three actions:

- **Download JSON** asks the server to validate and canonicalize the current
  state, then downloads `linked-views-<timestamp>.json`.
- **Copy JSON** performs the same validation and copies the canonical text to
  the clipboard.
- **Open JSON...** uploads one `.json` file to the current Shiny session. The
  server validates it against the current bundle before the client applies it.

The dialog includes a short privacy statement and one `aria-live` status line.
Success reports the number of restored cells. Failure keeps the current state
untouched and explains whether the file is malformed, unsupported, oversized,
or belongs to a different cell population.

The controls are available before a cohort exists so an imported configuration
can create one. Download and copy remain useful with an empty selection because
the surrounding linked-view context is itself reproducible state.

## JSON Contract

The exchange format is ordinary JSON, not `jsonlite::serializeJSON()`. Version 1
uses this envelope:

```json
{
  "schema": "cerebronexus-linked-view",
  "version": 1,
  "created_at": "2026-08-20T14:32:00Z",
  "dataset": {
    "cell_count": 12345,
    "cell_fingerprint": "md5-cell-set-v1:0123456789abcdef0123456789abcdef"
  },
  "selection": {
    "cells": ["AAAC...", "AAAG..."],
    "source": "UMAP"
  },
  "view": {
    "colour": {
      "mode": "cell_type",
      "gene": null,
      "rgb_genes": [],
      "clip": 0.01
    },
    "projections": ["umap"],
    "spatial_sections": ["donorA"],
    "active_spatial": "donorA",
    "filters": {
      "sample": ["donorA", "donorB"]
    },
    "hidden_levels": [],
    "display": {
      "percentage_cells": 100,
      "point_size": 3.0,
      "point_opacity": 0.8,
      "group_labels": true,
      "selection_mode": "lasso",
      "clone_layout": "stack"
    },
    "focus_space": null,
    "lenses": [
      {
        "space": "projection::umap",
        "viewport": {"cx": 0.5, "cy": 0.5, "span": 1.0},
        "rotation": null
      }
    ],
    "spatial_backgrounds": [],
    "trekker": {
      "dissolve_percentage": 0,
      "evidence": false,
      "niche_radius": 250
    }
  }
}
```

The cell fingerprint is an identity guard, not an authenticity signature. It is
the MD5 of a length-prefixed, sorted UTF-8 cell-barcode stream, prefixed with the
algorithm name. Sorting permits the same population to be shared between files
whose internal cell order differs; the selection itself remains barcode-based.
No dataset path or local filename enters the JSON.

Schema version 1 is strict. Unknown top-level fields, unknown fields inside
versioned records, future versions, non-finite numbers, duplicate selected
barcodes, duplicate lens identities, and values outside documented ranges are
rejected. Optional capability-specific fields may be absent and default to the
current dataset's normal state.

## State Boundary

Version 1 includes state that changes the scientific interpretation or visible
reproduction of the linked workspace:

- active cohort barcodes and its source label;
- selected projection and Spatial lenses, active Spatial section, and focused
  lens;
- colour mode, selected gene/RGB genes, continuous-range clipping, legend-hidden
  levels, and group-filter level names;
- percentage of cells, point size, point opacity, labels, selection mode, and
  clonal layout;
- per-lens viewport and 3-D rotation;
- Spatial background mode, image identity, and numeric alignment state, without
  embedding the image itself;
- Trekker dissolve, evidence, and niche-radius controls.

Version 1 deliberately excludes hover state, pinned tooltips, an open cell card,
open dialogs/popovers, animation progress, cached expression vectors, metadata,
coordinates, expression matrices, image data URIs, filesystem paths, Builder
project identifiers, authentication data, and tokens.

## Architecture

### R contract module

`inst/viewer/coordinated_views/config.R` is a pure, bundle-safe module. It:

1. computes the path-free cell-population fingerprint;
2. decodes bounded JSON with `simplifyVector = FALSE`;
3. validates and normalizes the complete version-1 envelope;
4. verifies the current cell count/fingerprint and selected barcode membership;
5. emits canonical pretty JSON with a server-authored UTC timestamp.

The module uses only base R, `tools`, and the already imported `jsonlite`; it
must work in a self-contained generated App without the package installed.

### Server transport

`inst/viewer/coordinated_views/server.R` sources the contract module and adds:

- a request observer for copy/download snapshots sent by the client;
- a session-local canonical JSON value consumed by a `downloadHandler`;
- an upload observer that reads at most 5 MiB and validates before applying;
- `coordviews_config_result` custom messages containing only normalized state,
  canonical JSON for clipboard use, or safe user-facing errors.

Every request carries a client nonce. Late replies are ignored by the browser.
The current `coordviews_bundle()` is the dataset authority for validation.

### Browser state adapter

The existing `coordviews.js` closure remains the authority for interactive
state. It exposes a narrow adapter that can:

- capture a plain version-1 snapshot without exposing the data bundle;
- apply an already normalized snapshot transactionally;
- report readiness and a sanitized summary.

Application validates capabilities again against the current bundle, builds all
new state in temporary values, and commits only after every reference resolves.
It restores panel and control context first, then calls the existing
`setSelection()` once so every current consumer updates through the established
selection path.

`inst/viewer/www/coordviews-config.js` owns dialog events, Shiny request/reply
transport, file-input affordance, hidden download activation, clipboard access,
status messaging, and stale-nonce rejection. It knows nothing about canvas
internals beyond the adapter.

## Validation and Limits

- Maximum uploaded or canonical JSON size: 5 MiB.
- Maximum nesting depth: 10.
- Maximum selected cells: the current dataset's cell count, additionally bounded
  by what fits in the byte limit.
- Strings are UTF-8, scalar where required, and bounded to 256 bytes except cell
  barcodes, which are bounded to 1,024 bytes.
- Arrays that represent sets must contain unique values.
- Numeric controls are finite and constrained to the UI's supported ranges.
- Dataset fingerprint and cell count must both match before any state is sent to
  the browser.
- Every selected barcode must exist in the current bundle; partial restoration
  is rejected rather than silently changing the cohort.
- Unsupported projections, sections, groups, fields, genes, images, or lenses
  fail without changing current state. There is no best-effort partial apply in
  version 1.

## Error Handling

The server logs the internal condition and returns one of a small set of safe
messages. The client never displays raw parser errors or local paths. Import is
transactional: validation or capability failure leaves the current selection,
filters, viewports, and controls untouched. Copy failure offers download as a
fallback only after the canonical state exists.

## Testing

### Pure contract tests

- stable cell fingerprint independent of cell order;
- canonical version-1 round trip;
- selection and every supported view-state field normalization;
- malformed JSON, unsupported schema/version, size/depth limits, duplicate and
  missing cells, dataset mismatch, non-finite/out-of-range values, and unknown
  fields;
- canonical output contains no dataset path, image URI, metadata, expression,
  credentials, or token values.

### Browser tests

- box/lasso selection -> capture -> clear -> apply restores the same cohort in
  every panel and reports the same barcodes to Shiny;
- projection/Spatial choices, filters, colour mode, display controls, focus,
  viewports, rotation, image state, and Trekker controls round-trip;
- a capability mismatch changes nothing and yields an actionable status;
- Save / share is keyboard reachable, the dialog traps no hidden state, Escape
  closes it, and status uses `aria-live`;
- data-set replacement invalidates an old configuration;
- server-validated copy/download/upload transport ignores stale replies.

### Final verification

Run JavaScript syntax checking, the focused contract and browser suites, the
generated-App smoke affected by the new bundled module, `scripts/precheck.sh
fast`, and the repository's full `scripts/precheck.sh` once the commit history
is stable.

## Non-goals

- sharing source data, CRBs, images, or a complete Builder Project;
- cloud-hosted links, accounts, permissions, or collaborative editing;
- named server-side configuration storage;
- migration from unversioned historical files, because none exist;
- restoring transient hover, tooltip, dialog, or animation state.
