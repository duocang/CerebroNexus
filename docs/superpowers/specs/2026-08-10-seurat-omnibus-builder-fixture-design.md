# Seurat Omnibus Builder Fixture Design

## Goal

Replace the Builder gallery's Cerebro-shaped synthetic examples with one
serialized synthetic Seurat object that behaves like a user's uploaded Xenium
dataset. The first phase deliberately excludes immune repertoire, HLA, and
precomputed analysis results so the fixture remains small and the regression
surface stays focused.

## Gallery contract

The Builder gallery exposes exactly one example:

- id: `all_content`
- label: `All content`
- source: a serialized `.rds` containing a Seurat object
- provenance: synthetic

All current gallery examples and their generated fixture RDS files are removed.
They may return later as independently designed upload scenarios.

## Seurat object contract

The fixture is constructed with public Seurat/SeuratObject APIs and is saved as
an RDS. It must survive a save/read round trip before it is accepted.

The object contains:

- a sparse RNA counts layer and normalized data layer;
- synthetic, biologically recognizable marker-driven expression;
- metadata for `patient`, `section`, `cell_type`, `cluster`, `region`,
  `nCount_RNA`, and `nFeature_RNA`;
- three patients: `patient_a`, `patient_b`, and `patient_c`;
- PCA, UMAP, and t-SNE reductions aligned to every cell;
- six native Seurat FOVs representing Xenium sections:
  - `patient_a_section_1`
  - `patient_a_section_2`
  - `patient_b_section_1`
  - `patient_b_section_2`
  - `patient_b_section_3`
  - `patient_c_section_1`
- one Trekker payload aligned to a subset of object cell barcodes.

The fixture does not contain precomputed Marker genes, Most expressed genes,
mean expression, enrichment, trajectories, supplementary tables, immune
repertoire, or HLA typing. Those features must be exercised through Builder's
Enhance workflow in later tests.

## Histology sidecars

Five deterministic synthetic tissue PNGs ship beside the RDS:

- two for patient A;
- three for patient B;
- none for patient C.

The images are standalone fixture inputs, not embedded in a Cerebro-shaped
`@misc` structure. Patient C therefore exercises the coordinates-only spatial
path. Image dimensions and cell-coordinate bounds are deterministic and differ
between sections so alignment bugs cannot be hidden by reusing one image.

## Construction architecture

The fixture builder is split by responsibility:

1. expression and metadata construction;
2. reduction construction;
3. native FOV construction;
4. deterministic tissue image construction;
5. Trekker construction and alignment;
6. round-trip validation and serialization.

The coordinated-views `build_omnibus_demo.R` is used as a biological and visual
reference, but the output is a Seurat object rather than a `Cerebro_v1.3`
instance. No `Cerebro_v1.3$new()` or direct Cerebro slot assembly is allowed in
the fixture path.

## Builder data flow

Selecting the example uses the same Seurat adapter and inspection path as a
user-uploaded RDS. The example may bypass the browser file picker, but it must
not bypass adapter inspection, snapshotting, recommendation, review, analysis,
or build execution.

The five image files are declared as fixture sidecars for tests and future image
alignment scenarios. They are not silently converted into built spatial
content when the Seurat example is selected.

## Test strategy

The first phase replaces broad per-example assertions with focused contracts:

- the gallery and catalog each expose only `all_content`;
- the serialized fixture reopens as a Seurat object;
- expression layers, metadata, and reductions are aligned;
- the patient/section distribution is exactly 2/3/1;
- all six FOVs contain valid centroids;
- exactly five deterministic PNG sidecars exist and patient C has none;
- Trekker barcodes are unique and belong to the Seurat object;
- forbidden precomputed `@misc` analysis families are absent;
- the object passes Builder adapter inspection and can freeze/build through the
  existing Seurat pipeline.

Tests for removed immune, HLA, spatial-only, and legacy examples are deleted or
rewritten only where they specifically assert the gallery/catalog fixture
matrix. Unit tests for those production capabilities remain intact.

## Scope boundaries

This phase does not add immune repertoire or HLA data, generate precomputed
analysis results, redesign the spatial image upload UI, or restore additional
gallery examples. It also does not make the fixture large enough for performance
benchmarking; the object stays compact enough for routine package tests.
