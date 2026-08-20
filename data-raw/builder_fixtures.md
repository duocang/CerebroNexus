# Builder fixtures

The Builder gallery ships one compact input fixture:

- `inst/builder/fixtures/all_content.rds`
- seven deterministic histology PNG sidecars mapped explicitly to sections and
  FOVs

These are Builder inputs, not demo `.crb` outputs. Regenerate them from the
repository root with:

```sh
Rscript data-raw/build_builder_fixtures.R
```

The generator is offline and deterministic, with one explicit object seed and
images derived entirely from fixed dimensions and mathematical functions. It
overwrites only the committed files in `inst/builder/fixtures/`.

All synthetic-data construction lives in `data-raw/build_builder_fixtures.R`,
which is excluded from the installed package. The installed Builder runtime
only locates and reads the committed RDS and sidecars; it does not contain a
fixture factory. Passing one optional directory argument writes the same
fixture set to that directory for reproducibility tests.

## All content Seurat

`all_content.rds` is a serialized synthetic Seurat object designed to follow
the same inspection and build path as a user-uploaded RDS. It contains:

- a sparse RNA counts layer and normalized data;
- marker-driven synthetic expression and explicit `patient_id`, `section_id`,
  `fov_id`, `sample_id`, `condition`, cell-type, cluster, region, and QC
  metadata;
- PCA, UMAP, and t-SNE reductions;
- three Xenium patients represented by six measured sections/FOVs:
  `patient_a` has two sections, `patient_b` has three, and `patient_c`
  has one; each FOV has a declared micron coordinate system;
- a valid Trekker payload aligned to a subset of Seurat cell barcodes.

The fixture intentionally does not precompute Marker genes, Most expressed
genes, mean expression, enrichment, trajectories, supplementary tables,
immune repertoire, or HLA typing. Those families must enter through Builder's
Enhance workflow or later dedicated upload scenarios.

## Histology sidecars

The catalog in `inst/builder/io.R` maps these files to explicit `section_id`
and `fov_ids`; the file name is never the authority for that relationship. The
fixture directory contains:

- `section_a_1_he.png` and `section_a_1_dapi.png`
- `section_a_2_he.png` and `section_a_2_dapi.png`
- `section_b_1_he.png`, `section_b_1_if.png`, and `section_b_1_pas.png`

The A sections each have two images, B's first section has three images, and
the remaining B sections plus C are coordinates-only. A photo is never a new
FOV. The PNG files are standalone alignment inputs; selecting the built-in
example does not silently attach them to the generated Cerebro object. This
preserves the same explicit image-alignment step used for a local Seurat
upload.

The catalog in `inst/builder/io.R` is the source of truth for the single
`all_content` gallery record, its expected manifest, visible pages, and
supporting files. Capability-specific immune, HLA, analysis, and legacy data
remain test-local fixtures rather than public gallery examples.
