# Multi-spatial image public API design

## Summary

CerebroNexus will model spatial backgrounds with three stable identities:
the application data set, the Seurat/Cerebro spatial entry, and the
user-supplied image label. The public conversion and application-building
functions will accept this hierarchy, the `Cerebro` class will store multiple
named backgrounds per spatial entry, and the Viewer will expose only the
backgrounds belonging to the selected spatial entry.

The term `spatial entry` means a name returned by
`SeuratObject::Images(object)` before conversion and by
`Cerebro$availableSpatial()` afterward. It can represent a Visium slice, a
Xenium or MERFISH FOV, a Slide-seq puck, or another `SpatialImage` subclass. It
does not imply that the name is a donor or sample identifier.

## Public API

### `convertSeuratToCerebro()` and `exportFromSeurat()`

Both functions gain an optional `spatial_images` argument. Since one Seurat
object is being converted, the structure begins at the spatial-entry level:

```r
spatial_images = list(
  "donorA tissue" = c(
    "H&E" = "images/donorA_he.png",
    "DAPI" = "images/donorA_dapi.jpg"
  ),
  "donorB tissue" = list(
    "H&E" = list(
      path = "images/donorB_he.png",
      bounds = c(xmin = 0, xmax = 1200, ymin = 0, ymax = 900)
    )
  )
)
```

The first-level keys must match `SeuratObject::Images(object)`. Image labels are
arbitrary user-facing strings; they must be non-empty and unique only within
their spatial entry. PNG, JPEG/JPG, and SVG are accepted. A named character
vector is shorthand for descriptors containing only `path`. A descriptor can
also provide explicit coordinate-space `bounds`; otherwise bounds are derived
from the exported coordinate range.

Image files supplied during conversion are encoded into the CRB. The input
Seurat object is not modified. `exportFromSeurat()` receives the normalized
manifest from `convertSeuratToCerebro()` and also supports direct callers.

`object@misc$cerebro_spatial_images` remains supported for programmatically
constructed Seurat objects, but its canonical shape changes to:

```r
list(
  "donorA tissue" = list(
    "H&E" = list(
      histology_image = "data:image/png;base64,...",
      histology_image_bounds = c(
        xmin = 0, xmax = 1000, ymin = 0, ymax = 800
      )
    )
  )
)
```

The former one-payload-per-spatial shape is accepted as a legacy shorthand and
normalized to one image named `Tissue background`. If the function argument and
`object@misc` both declare the same spatial/image pair, conversion fails rather
than choosing one silently.

### `createShinyApp()`

Because this function can bundle multiple CRBs, its `spatial_images` argument
uses the complete hierarchy:

```r
spatial_images = list(
  Atlas = list(
    "donorA tissue" = c(
      "Additional H&E" = "images/donorA_second_scan.png"
    ),
    "donorB tissue" = c(
      "IF panel" = "images/donorB_if.svg"
    )
  )
)
```

`Atlas` must match a `cerebro_data` label. The second level must match
`availableSpatial()` in that CRB. The third-level labels are displayed in the
Viewer. External files are copied under `spatial-assets/` and stored in the
generated configuration as relative paths. They are merged at runtime with
embedded CRB backgrounds without modifying the CRB.

The old dataset-to-path form remains compatible only when that CRB has exactly
one spatial entry. With zero or multiple entries it fails with an error that
shows the available spatial names. This prevents an image from being silently
attached to the wrong FOV.

The existing `spatial_images_flip_*`, `spatial_images_scale_*`, and
`spatial_images_offset_*` arguments remain accepted for legacy single-spatial
apps. A new `spatial_image_settings` argument carries per-image settings using
the same dataset/spatial/image hierarchy. Each leaf may contain `flip_x`,
`flip_y`, `scale_x`, `scale_y`, `offset_x`, `offset_y`, and `rotation`. Legacy
settings are normalized only when their target is unambiguous.

## Canonical `Cerebro` storage

Each `Cerebro$spatial[[spatial_name]]` entry continues to contain coordinates
and expression. New objects store backgrounds under:

```r
histology_images = list(
  "H&E" = list(
    histology_image = "data:image/png;base64,...",
    histology_image_bounds = c(
      xmin = 0, xmax = 1000, ymin = 0, ymax = 800
    )
  )
)
```

`Cerebro$addSpatialData()` validates and canonicalizes this structure.
`Cerebro$getSpatialData()` returns the canonical structure. Viewer-side readers
also normalize legacy entries containing singular `histology_image` and
`histology_image_bounds`, because serialized historical CRBs may carry older
embedded methods and fields.

Coordinates-only entries remain valid and use an empty `histology_images`
list. Image bounds contain exactly finite `xmin`, `xmax`, `ymin`, and `ymax`,
with ordered ranges containing all coordinates.

## Viewer behavior

Background choices are resolved from the current dataset and the currently
selected spatial entry. The selector contains:

- `No Background`;
- embedded image labels from that spatial entry;
- external image labels configured for that dataset and spatial entry.

Changing from `donorA tissue` to `donorB tissue` resets an invalid background
selection and exposes only donor B choices. Image settings are resolved by the
exact dataset/spatial/image identity. Selecting or transforming a background
does not change coordinate axes or the cells included in coordinated views.

The standard Spatial tab implements this contract in this branch. The same
pure resolver helpers form the integration boundary for the separate
Coordinated Views branch, where each lens supplies its own spatial name.

## Validation and errors

Validation occurs before output publication:

- all list levels require unique non-empty names;
- dataset names must match `cerebro_data`;
- spatial names must match `Images(object)` or `availableSpatial()`;
- paths must be existing regular files with supported extensions;
- image labels must not collide between embedded and external sources;
- bounds and settings must be scalar, finite, correctly named, and ordered;
- multiple input files cannot claim the same bundled target path;
- malformed optional image data is a hard error, while an omitted image means
  coordinates-only spatial data.

Errors identify the complete dataset/spatial/image path. Failed conversion or
application creation leaves an existing CRB, sidecar, or app directory intact.

## Expanded Omnibus fixture

The synthetic Omnibus Seurat object remains 80 genes and 120 cells but gains
three 40-cell spatial entries:

- `donorA tissue`: two named embedded backgrounds;
- `donorB tissue`: one named embedded background, plus an external background
  used by the application integration test;
- `donorC tissue`: coordinates only.

The entries deliberately have different coordinate layouts and bounds. This
tests multiple spatial identities and image isolation without claiming that
the synthetic FOV classes reproduce every platform's native object class. The
existing real Visium, Slide-seq, MERFISH, and Xenium fixtures continue to cover
technology-specific extraction.

The fixture retains expression, groups, PCA/UMAP, marker and most-expressed
genes, enrichment, trajectories, TCR/BCR, HLA, Trekker, trees, and extra
material. The builder also writes a marker CSV and the external image used by
the public-API integration example.

## End-to-end public API proof

An executable integration test starts only from committed user-facing inputs:
the Omnibus Seurat RDS, marker CSV, and external spatial images. It calls
`convertSeuratToCerebro()` with group renaming, marker import, and
`expression_matrix_mode = "h5"`, verifies the CRB and sibling H5 backend, then
calls `createShinyApp()` with multiple named CRBs and the nested external image
manifest.

The generated app is reread and checked for private CRB/H5 placement, nested
image configuration, spatial/image isolation, embedded-image preservation,
coordinates-only behavior, Viewer sources, and package-free runtime loading.
The test uses a temporary output directory and does not commit a generated app.

## Documentation and compatibility

Roxygen and Rd documentation will show the full calls for both public
functions. The spatial and Omnibus data documentation will explain platform
differences and distinguish a Seurat spatial entry from a donor. NEWS will
record the new canonical hierarchy and the constrained legacy shorthand.

Existing coordinate-only CRBs and legacy single-image CRBs remain readable.
Existing applications using dataset-to-path image configuration remain valid
when each target CRB contains exactly one spatial entry. Ambiguous legacy calls
fail explicitly instead of guessing.

## Acceptance criteria

- A single spatial entry can expose multiple arbitrary user-named backgrounds.
- Multiple spatial entries in one CRB cannot see each other's images.
- Coordinates-only entries remain usable.
- Conversion accepts either an in-memory Seurat object or an RDS path and can
  embed named external PNG/JPEG/SVG files.
- App creation bundles nested external images without exposing CRB or H5 files
  over HTTP.
- The expanded Omnibus assets are generated only through the public converter,
  with no post-conversion CRB mutation.
- The literal `convertSeuratToCerebro()` then `createShinyApp()` workflow passes
  with H5 expression storage and marker-file import.
- Focused tests, complete package tests, installation, and static package checks
  introduce no new failures or warnings.
