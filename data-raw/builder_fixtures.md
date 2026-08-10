# Builder fixtures

The Builder gallery ships one compact input fixture:

- `inst/builder/fixtures/all_content.rds`
- five deterministic tissue PNG sidecars for patient A and patient B

These are Builder inputs, not demo `.crb` outputs. Regenerate them from the
repository root with:

```sh
Rscript data-raw/build_builder_fixtures.R
```

The generator is offline, deterministic, and preserves the caller's random
number state. It overwrites only the committed files in
`inst/builder/fixtures/`.

## All content Seurat

`all_content.rds` is a serialized synthetic Seurat object designed to follow
the same inspection and build path as a user-uploaded RDS. It contains:

- a sparse RNA counts layer and normalized data;
- marker-driven synthetic expression and patient, section, cell-type, cluster,
  region, and QC metadata;
- PCA, UMAP, and t-SNE reductions;
- three Xenium patients represented by six native FOVs:
  `patient_a` has two sections, `patient_b` has three, and `patient_c`
  has one;
- a valid Trekker payload aligned to a subset of Seurat cell barcodes.

The fixture intentionally does not precompute Marker genes, Most expressed
genes, mean expression, enrichment, trajectories, supplementary tables,
immune repertoire, or HLA typing. Those families must enter through Builder's
Enhance workflow or later dedicated upload scenarios.

## Histology sidecars

The fixture directory contains:

- `patient_a_section_1.png`
- `patient_a_section_2.png`
- `patient_b_section_1.png`
- `patient_b_section_2.png`
- `patient_b_section_3.png`

Patient C has spatial coordinates but no image. The PNG files are standalone
alignment inputs; selecting the built-in example does not silently attach them
to the generated Cerebro object. This preserves the same explicit image
alignment step used for a local Seurat upload.

The catalog in `inst/builder/io.R` is the source of truth for the single
`all_content` gallery record, its expected manifest, visible pages, and
supporting files. Capability-specific immune, HLA, analysis, and legacy data
remain test-local fixtures rather than public gallery examples.
