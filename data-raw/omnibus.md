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
types are split over two samples and four clusters. It carries normalized RNA
expression, QC columns, cell-cycle assignments, PCA and UMAP embeddings, and a
centroid-based `FOV` covering every cell.

The following optional conversion surfaces are populated:

- experiment, parameter, technical, and gene-list metadata;
- marker genes, most-expressed genes, mean expression, and enriched pathways;
- a cell-type tree and a list-based monocle2-style trajectory;
- unified TCR and BCR rows for both samples;
- synthetic class I and class II HLA genotypes for both samples;
- an extra table and ggplot;
- a compact Trekker payload aligned to the expression cells;
- FOV coordinates, expression, and a synthetic embedded tissue image.

These structures are intentionally small but non-empty. They exist to exercise
Viewer behavior and conversion contracts, not to support biological claims.

## FOV image transport

Seurat `FOV` objects carry geometry but no portable general-purpose tissue
raster. The source object therefore declares the image at:

```r
object@misc$cerebro_spatial_images$omnibus_fov
```

The entry contains `histology_image`, a base64 PNG data URI, and
`histology_image_bounds`, the named coordinate-space extent. During export,
CerebroNexus validates the URI, the four finite ordered bounds, the FOV name,
and containment of all exported `x`/`y` coordinates. The validated values are
then stored in the CRB spatial entry. Objects without this optional declaration
remain coordinates-only.

## Rebuild

From the repository root, run:

```bash
Rscript data-raw/build_omnibus_demo.R
```

The script requires only local package dependencies and performs no downloads.
It generates both artifacts in a temporary directory below
`inst/extdata/examples`, rereads them, checks every declared data surface, and
replaces the committed outputs only after validation succeeds.

To verify the committed artifacts and their integration contract, run:

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline")'
```
