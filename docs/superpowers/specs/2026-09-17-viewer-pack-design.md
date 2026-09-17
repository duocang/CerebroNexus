# Viewer Pack design

## Goal

Large CRBs may carry a rebuildable sibling Viewer Pack containing stable, dataset-derived acceleration data. The 500,000-cell threshold is evaluated once for the whole dataset; module assets are then selected only by module availability, never by module row count.

## Contract

The CRB remains canonical. `<dataset>.viewer/manifest.json` is optional, deletable, and never required to open a CRB. A pack is used only when its schema version, CRB fingerprint, ordered-cell fingerprint, asset size, and checksum all match. Missing, partial, corrupt, incompatible, or misaligned packs return `NULL` and the Viewer follows the existing CRB path.

`buildViewerPack()` accepts an existing CRB, `viewer_binary = "auto" | "always" | "never"`, `viewer_binary_threshold = 500000L`, and explicit overwrite semantics. It builds in a private sibling directory, validates the completed pack, then publishes it by rename. The manifest is written last.

## Initial vertical slice

The common layer records the canonical cell count and ordered-cell fingerprint. Projection assets store one shared Float32-compatible matrix per available embedding. Metadata assets dictionary-encode categorical columns and preserve numeric columns as Float32-compatible vectors with explicit missingness. HLA/TCR assets store chain-normalized segments for every available TCR chain after the stable IR-to-metadata join. These HLA assets remove the measured Ren cold work of loading/parsing the complete repertoire for the first motif view while leaving cohort selection, chain choice, thresholds, graph parameters, active selection, and final motif graph construction at runtime.

The Viewer discovers the sibling pack from the selected CRB path. Assets are lazy-loaded and checksum-verified on first use. HLA uses a valid precomputed chain asset and otherwise executes the existing `getImmuneRepertoire() -> hla_annotate_ir_metadata() -> hla_parse_ir_segments()` path unchanged.

## Deferred work

Immune clonal projection indexes and abundance aggregates follow only after the common/HLA slice proves correctness and measurable benefit. Spatial, trajectory, Trekker, trees, and expression acceleration are added only where profiling shows repeated stable work; the expression matrix remains in BPCells/H5.

## Verification

Tests cover `auto` below/equal/above threshold, `always`, `never`, atomic overwrite, manifest fields, module presence, checksums, schema and fingerprint mismatch, missing/corrupt assets, fallback, Float32 projection tolerance, metadata groups/counts, and HLA normalized-data parity. Existing million-cell, first-frame, coordinated-view, sync, IR, and HLA suites remain unchanged and must pass.
