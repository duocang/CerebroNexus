# Builder fixtures

The Builder gallery ships two synthetic inputs through one action:

- `inst/builder/fixtures/complete_viewer/complete_viewer_data.rds`
- three precomputed Marker gene attachments (`.csv`, `.tsv`, and `.xlsx`)
- two supplementary-table attachments (`.csv` and `.xlsx`)
- seven deterministic histology attachments (`.png` and `.jpg`)
- `inst/builder/fixtures/trekker_spatial/trekker_4_tissues.qs2`
- four Trekker tissue H&E images (`.jpg`)

These are Builder inputs, not demo `.crb` outputs. Regenerate the complete set
from the repository root with:

```sh
Rscript data-raw/build_builder_fixtures.R
```

The generator is offline and deterministic. It uses one explicit object seed;
all tabular values and image pixels come from fixed data or mathematical
functions. Passing one optional directory argument writes the same fixture set
there for reproducibility tests. Synthetic construction remains in
`data-raw/build_builder_fixtures.R`; the installed runtime only reads the
committed artifacts.

The Trekker fixture is copied from the project demo source. It contains one
SlideSeq section named `slice1`; its four H&E files are loaded as distinct
named images for that section.

## Complete Viewer data Seurat

`complete_viewer_data.rds` follows the same inspection and build path as a
user-uploaded RDS. It contains:

- sparse RNA counts and normalized expression;
- patient, section, FOV, sample, condition, cell-type, cluster, region,
  cell-cycle, and QC metadata;
- PCA, UMAP, and t-SNE reductions;
- gene lists, Marker genes, Most expressed genes, mean expression, enrichment,
  a valid Monocle 2 trajectory, and embedded supplementary tables and plot;
- a six-sample immune repertoire with TRA, TRB, and IGH records plus canonical
  synthetic HLA typing;
- three patients represented by six measured Spatial FOVs with micron
  coordinate systems;
- a valid Trekker payload aligned to a subset of Seurat cell barcodes.

HLA and Trekker coverage here verifies detection, preservation, BuildPlan and
Viewer behavior for embedded payloads. It does not imply a dedicated HLA or
Trekker upload editor.

## Explicit attachments

The marker attachments exercise single-level CSV/TSV mapping and multi-sheet
XLSX mapping. The supplementary attachments exercise CSV plus multi-sheet XLSX
materialization. They are intentionally not embedded in the RDS, so tests must
pass through the same explicit Enhance workflow used for local uploads.

The catalog maps histology files to explicit `section_id` and `fov_ids`; file
names are not the authority for that relationship:

- `section_a_1_he.png` and `section_a_1_dapi.png`
- `section_a_2_he.png` and `section_a_2_dapi.png`
- `section_b_1_he.png`, `section_b_1_if.jpg`, and `section_b_1_pas.png`

The remaining B sections and C section are coordinates-only. A photograph is
never a new FOV. Gallery examples do not attach these images automatically;
they are files for exercising the same manual Add image workflow as user data.

`inst/builder/io.R` is the source of truth for the
`complete_viewer_data` and `trekker_spatial` records, their expected manifests,
conditional Viewer pages, and sidecar inventories.
