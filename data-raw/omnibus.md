# Synthetic Omnibus demo

`demo_omnibus_seurat.rds` is the canonical end-to-end input fixture for
CerebroNexus. It is a real Seurat object, but all values are synthetic and are
generated locally with fixed seed `20260810`. It contains no human-derived or
third-party data.

`demo_omnibus.crb` is produced only by `convertSeuratToCerebro()`. The builder
does not reopen or mutate the converted CRB, so the pair tests the public
conversion pipeline instead of demonstrating manual class assembly.

## Contents

The source Seurat object contains exactly 80 genes and 120 cells. Four cell
types and four clusters span three 40-cell donors. `donorA` and `donorC` are
Control; `donorB` is Treatment. It carries normalized RNA expression, QC
columns, cell-cycle assignments, PCA and UMAP embeddings, and three named
centroid `FOV` entries that together cover every cell:

- `donorA tissue`: a circular coordinate layout with embedded `H&E` and `DAPI`
  backgrounds;
- `donorB tissue`: an 8 x 5 grid with embedded `H&E`; the public conversion
  example adds an external `IF panel` PNG;
- `donorC tissue`: a triangular coordinate layout with no embedded image; the
  app-building example supplies a `Pathology review` PNG without changing the
  CRB.

Those labels are fixture names, not required protocol vocabulary. In a real
object the keys are exactly the names returned by `SeuratObject::Images()`.
Image labels are likewise chosen by the caller, and one spatial entry may have
several named alternatives for Viewer switching.

The following optional conversion surfaces are populated:

- experiment, parameter, technical, and gene-list metadata;
- marker genes, most-expressed genes, mean expression, and enriched pathways;
- a cell-type tree and a list-based monocle2-style trajectory;
- unified TCR and BCR rows for both samples;
- synthetic class I and class II HLA genotypes for both samples;
- an extra table and ggplot;
- a compact Trekker payload aligned to the expression cells;
- three FOV coordinate/expression entries, multiple embedded backgrounds, and
  a coordinates-only entry.

These structures are intentionally small but non-empty. They exist to exercise
Viewer behavior and conversion contracts, not to support biological claims.

## Spatial image transport

Seurat `FOV` objects carry geometry but no portable general-purpose tissue
raster. The source object therefore declares embedded backgrounds at:

```r
object@misc$cerebro_spatial_images[["donorA tissue"]][["H&E"]]
object@misc$cerebro_spatial_images[["donorA tissue"]][["DAPI"]]
object@misc$cerebro_spatial_images[["donorB tissue"]][["H&E"]]
```

Each leaf contains `histology_image`, a base64 PNG data URI, and
`histology_image_bounds`, its named coordinate-space extent. During export,
CerebroNexus validates every URI, the four finite ordered bounds, the spatial
entry name, and containment of the exported `x`/`y` coordinates. The validated
images are stored as a named list in the matching CRB spatial entry. Omitting
this declaration, as for `donorC tissue`, deliberately leaves coordinates only.

External PNG/JPEG/SVG files can instead be supplied to
`convertSeuratToCerebro(spatial_images = ...)`, which embeds them in the CRB,
or to `createShinyApp(spatial_images = ...)`, which bundles them alongside the
app. Conversion uses `spatial entry -> image label`; app creation adds the
dataset level: `dataset -> spatial entry -> image label`. Explicit descriptor
`bounds` are in the same coordinate space as the points; when omitted during
conversion, they are derived from the coordinate range.

## Complete public API workflow

The following two calls use only package-shipped inputs. They are also
executable as `Rscript data-raw/verify_omnibus_public_api.R` from a source
checkout.

```r
library(CerebroNexus)

input_dir <- system.file("extdata/examples", package = "CerebroNexus")

convertSeuratToCerebro(
  seurat_file = file.path(input_dir, "demo_omnibus_seurat.rds"),
  result_dir = "output",
  assay = "RNA",
  slot = "data",
  experiment_name = "Synthetic Omnibus",
  organism = "Human",
  groups = c("orig.ident", "condition", "cell_type"),
  groups_naming = list(
    "orig.ident" = "sample",
    "cell_type" = "cluster"
  ),
  marker_file = file.path(input_dir, "demo_omnibus_markers.csv"),
  marker_method = "Synthetic markers",
  expression_matrix_mode = "h5",
  spatial_images = list(
    "donorB tissue" = c(
      "IF panel" = file.path(input_dir, "demo_omnibus_donorB_if.png")
    )
  )
)

createShinyApp(
  cerebro_data = c(
    Omnibus = "output/cerebro_demo_omnibus_seurat.crb"
  ),
  result_dir = "my_app",
  welcome_message = "<h2>Synthetic Omnibus Atlas</h2>",
  port = 8080,
  host = "127.0.0.1",
  max_request_size = 8000,
  overwrite = TRUE,
  spatial_images = list(
    Omnibus = list(
      "donorC tissue" = list(
        "Pathology review" = list(
          path = file.path(input_dir, "demo_omnibus_donorC_review.png"),
          bounds = c(xmin = 100, xmax = 900, ymin = 100, ymax = 700)
        )
      )
    )
  ),
  spatial_image_settings = list(
    Omnibus = list(
      "donorC tissue" = list(
        "Pathology review" = list(
          flip_x = FALSE, flip_y = FALSE,
          scale_x = 1, scale_y = 1,
          offset_x = 0, offset_y = 0,
          rotation = 0
        )
      )
    )
  )
)

# Run locally or deploy the directory to Shiny Server.
shiny::runApp("my_app")
```

`seurat_file` may also be an in-memory Seurat object. H5 mode creates the CRB
and a sibling `.h5`; `createShinyApp()` copies both into private app storage.

## Rebuild

From the repository root, run:

```bash
Rscript data-raw/build_omnibus_demo.R
```

The script requires only local package dependencies and performs no downloads.
It generates the Seurat RDS, marker CSV, two external PNG inputs, and converted
CRB in a temporary directory below `inst/extdata/examples`, rereads them,
checks every declared data surface, and replaces the committed outputs only
after validation succeeds.

To verify the committed artifacts and their integration contract, run:

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline")'
```
