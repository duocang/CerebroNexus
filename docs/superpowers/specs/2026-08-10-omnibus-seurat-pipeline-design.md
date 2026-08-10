# Omnibus Seurat-to-Cerebro Pipeline Design

## Summary

CerebroNexus will ship one small, deterministic, synthetic Omnibus Seurat
object as the canonical end-to-end demonstration input. The object will be
converted by `convertSeuratToCerebro()`, loaded as the first bundled Viewer
dataset, and used to verify that `createShinyApp()` produces a standalone app.

The Omnibus fixture replaces hand-assembled CRB data as the primary integration
proof. Existing PBMC data remains as a focused regression fixture and is not
removed by this change.

## Goals

- Commit a reproducible Seurat `.rds` that exercises the supported conversion
  surface instead of testing only prebuilt `.crb` files.
- Generate the bundled Omnibus `.crb` exclusively through
  `convertSeuratToCerebro()` and `exportFromSeurat()`.
- Preserve a synthetic FOV tissue image and coordinate-space bounds during the
  conversion itself.
- Cover expression, metadata groups, cell cycle, projections, gene lists,
  marker genes, most-expressed genes, mean expression, enriched pathways,
  trajectories, immune repertoire, HLA typing, spatial data, Trekker, trees,
  and extra material in one compact object.
- Verify the complete Seurat -> CRB -> standalone Shiny app path offline.
- Put Omnibus first in the source app dataset selector and make it the default.

## Non-goals

- The synthetic fixture is not a biological benchmark and must not be described
  as real measured data.
- It does not replace the technology-specific public spatial examples.
- It does not change the `Cerebro` R6 field layout. The existing `spatial` field
  already supports `histology_image` and `histology_image_bounds`.
- It does not delete PBMC scripts, documentation, or fixtures in this change.
- It does not commit a generated standalone Shiny app.

## Generated artifacts

`data-raw/build_omnibus_demo.R` will deterministically generate:

- `inst/extdata/examples/demo_omnibus_seurat.rds`: the canonical source object;
- `inst/extdata/examples/demo_omnibus.crb`: its converted Cerebro object.

The script will use a fixed seed, require no network access, write through
temporary files, reread both outputs, validate their contracts, and replace the
committed artifacts only after validation succeeds. The fixture will contain
120 cells and 80 genes so it remains suitable for package installation and
routine tests.

## Seurat fixture contract

The source object contains one RNA assay with counts and normalized data,
stable cell and gene names, multiple samples, clusters and cell types, QC and
cell-cycle columns, UMAP/PCA reductions, and one synthetic `FOV` with centroid
coordinates.

The existing flat `object@misc` conventions remain authoritative for optional
payloads:

- `experiment`, `parameters`, `technical_info`, and `gene_lists`;
- `marker_genes`, `most_expressed_genes`, `mean_expression`, and
  `enriched_pathways`;
- `trajectories`, `trees`, and `extra_material`;
- `immune_repertoire`, `hla_typing`, and `hla_typing_source_type`;
- `trekker`.

Repertoire rows will include both TCR and BCR examples and use cell barcodes
that exist in the expression matrix. Synthetic HLA data will include both class
I and class II loci so conditional Viewer surfaces can be exercised.

## Synthetic FOV image contract

Seurat `FOV` objects provide geometry but no portable general-purpose tissue
raster slot. A dedicated conversion payload will therefore be introduced at
`object@misc$cerebro_spatial_images`:

```r
list(
  omnibus_fov = list(
    histology_image = "data:image/png;base64,...",
    histology_image_bounds = c(
      xmin = 0,
      xmax = 1000,
      ymin = 0,
      ymax = 800
    )
  )
)
```

The key must match a name in `Seurat::Images(object)`. The exporter will validate
that the image is a single `data:image/...` URI, the bounds contain exactly the
four finite named values above, `xmin < xmax`, `ymin < ymax`, and all exported
coordinates lie within the bounds. Invalid payloads fail conversion with the
image name in the error. Missing payloads continue to mean coordinates-only,
preserving existing FOV, Slide-seq, and legacy behavior.

After `.getSpatialData()` resolves coordinates and expression, the exporter
will merge the validated payload into that image's spatial entry before calling
`addSpatialData()`. No post-export mutation is allowed in the Omnibus builder.

## Other missing conversion coverage

`exportFromSeurat()` already consumes most optional `misc` payloads. It will add
the missing `object@misc$trekker` pass-through, validating through the existing
`Cerebro$addTrekker()` method. This keeps the Omnibus CRB wholly derived from the
Seurat source rather than patched after conversion.

## Application behavior

`inst/app.R` will list `demo_omnibus.crb` first under the label `Omnibus`.
Because `crb_pick_smallest_file` remains false, Omnibus becomes the initial
dataset. PBMC and the technology-specific demonstrations remain selectable.
The app comments and dataset documentation will state clearly that Omnibus is
synthetic and intended to demonstrate feature coverage.

## Tests

Tests will be layered so failures identify the broken boundary:

1. Unit tests validate accepted and rejected `cerebro_spatial_images` payloads,
   including FOV-name mismatches, malformed data URIs, invalid bounds, and
   coordinates outside the bounds.
2. Export tests construct a minimal in-memory Seurat/FOV fixture and assert that
   the resulting `Cerebro` spatial entry contains coordinates, expression, the
   image URI, and bounds. A Trekker pass-through assertion covers the second new
   conversion path.
3. Artifact tests load `demo_omnibus_seurat.rds` and `demo_omnibus.crb`, verify
   the expected classes and feature matrix identity, and assert every declared
   Omnibus capability is present and non-empty.
4. An integration test runs `convertSeuratToCerebro()` on the committed RDS in a
   temporary directory, compares the resulting semantic payload to the bundled
   CRB, then calls `createShinyApp()` and verifies the private CRB and Viewer
   sources are present and readable without loading CerebroNexus at runtime.
5. A source-app contract test asserts that Omnibus is the first configured
   dataset and no external spatial-image option is required for it.

Tests compare semantic fields rather than raw RDS bytes because R6 and Seurat
serialization can include environment details. Expensive artifact regeneration
is not performed automatically during ordinary unit tests.

## Documentation

`data-raw/omnibus.md` will document the synthetic data model, every populated
feature, the reproducible build command, expected outputs, and the distinction
between synthetic Omnibus and real public spatial demos. `data-raw/DATASETS.md`
will register both artifacts and their provenance.

## Error handling and compatibility

- Existing Seurat objects without `cerebro_spatial_images` export unchanged.
- Existing CRBs and FOV coordinate extraction remain compatible.
- Malformed optional image or Trekker payloads fail before the destination CRB
  is replaced.
- The builder leaves the previous committed artifacts intact on failure.
- All assets are synthetic, contain no personal data, and have no external
  redistribution restrictions.

## Acceptance criteria

- `demo_omnibus_seurat.rds` is a valid Seurat object and is sufficient by itself
  to regenerate `demo_omnibus.crb` offline.
- The regenerated object inherits `Cerebro`, contains every declared feature,
  and its FOV entry includes the embedded synthetic image and valid bounds.
- `createShinyApp()` successfully bundles the regenerated CRB into a standalone
  application.
- Omnibus is first and default in `inst/app.R`.
- Focused tests, the complete package test suite, installation, and package
  checks pass with no new warnings attributable to this work.
