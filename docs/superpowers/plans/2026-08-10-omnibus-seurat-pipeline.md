# Omnibus Seurat-to-Cerebro Pipeline Implementation Plan

> **For AI agent workers:** Required sub-skill: use `superpowers-zh:subagent-driven-development` (recommended) or `superpowers-zh:executing-plans` to implement this plan task by task. Track progress with the checkboxes below.

**Goal:** Ship a deterministic synthetic Omnibus Seurat object that converts into the default bundled Cerebro dataset, preserves an embedded FOV image, and verifies the complete Seurat-to-CRB-to-Shiny-app path.

**Architecture:** A focused internal validator reads optional image payloads from `object@misc$cerebro_spatial_images` and merges them into spatial entries during `exportFromSeurat()`. The same exporter passes through `object@misc$trekker`. A reproducible data-raw builder creates one 120-cell/80-gene Seurat object, converts it without post-export mutation, validates both artifacts, and the package tests each boundary independently.

**Tech stack:** R, R6, Seurat/SeuratObject, Matrix, testthat, png, base64enc, Shiny.

---

## File structure

- Create `R/spatial_image_payload.R`: pure validation and merge helpers for Seurat-carried spatial images.
- Modify `R/exportFromSeurat.R`: validate top-level spatial-image names, merge one payload per FOV, and pass through Trekker data.
- Create `tests/testthat/test-spatial-image-payload.R`: unit and exporter tests for valid and invalid payloads.
- Modify `tests/testthat/test-trekker.R`: cover Seurat `misc$trekker` conversion.
- Create `data-raw/build_omnibus_demo.R`: deterministic fixture construction, conversion, semantic validation, and staged replacement.
- Create `data-raw/omnibus.md`: fixture contract, rebuild instructions, and feature inventory.
- Modify `data-raw/DATASETS.md`: register the Seurat and CRB artifacts.
- Create `tests/testthat/test-omnibus-pipeline.R`: committed-artifact and end-to-end conversion/bundling contracts.
- Modify `inst/app.R`: make Omnibus the first/default dataset.
- Create `inst/extdata/examples/demo_omnibus_seurat.rds`: generated source fixture.
- Create `inst/extdata/examples/demo_omnibus.crb`: generated conversion result.
- Modify `NEWS.md`: announce the canonical Omnibus conversion fixture.

### Task 1: Validate and export embedded FOV image payloads

**Files:**
- Create: `R/spatial_image_payload.R`
- Modify: `R/exportFromSeurat.R:1663-1765`
- Create: `tests/testthat/test-spatial-image-payload.R`

- [ ] **Step 1: Write failing validator tests**

Create table-driven tests around the pure helper:

```r
valid_payload <- list(
  histology_image = paste0("data:image/png;base64,", base64enc::base64encode(charToRaw("png"))),
  histology_image_bounds = c(xmin = 0, xmax = 100, ymin = 0, ymax = 80)
)
coords <- data.frame(x = c(10, 90), y = c(5, 75))

expect_identical(
  .validateCerebroSpatialImage(valid_payload, "omnibus_fov", coords),
  valid_payload
)
expect_error(
  .validateCerebroSpatialImage(
    within(valid_payload, histology_image <- "not-a-data-uri"),
    "omnibus_fov",
    coords
  ),
  "omnibus_fov.*data:image"
)
expect_error(
  .validateCerebroSpatialImage(valid_payload, "omnibus_fov", data.frame(x = 101, y = 10)),
  "outside.*bounds"
)
```

Also assert duplicate/empty top-level names, unknown FOV names, missing bound names, non-finite bounds, reversed ranges, and missing numeric `x`/`y` columns fail with actionable messages.

- [ ] **Step 2: Run the tests and observe the missing helper failure**

Run:

```bash
R -q -e 'devtools::test(filter = "spatial-image-payload")'
```

Expected: FAIL because `.validateCerebroSpatialImage()` and `.validateCerebroSpatialImages()` do not exist.

- [ ] **Step 3: Implement the pure validators**

Create internal functions with these interfaces:

```r
.validateCerebroSpatialImage <- function(payload, image_name, coordinates) {
  required <- c("histology_image", "histology_image_bounds")
  # Require an exact named list payload, a data:image URI, finite ordered
  # xmin/xmax/ymin/ymax bounds, numeric finite x/y coordinates, and containment.
  payload[required]
}

.validateCerebroSpatialImages <- function(payloads, available_images) {
  # NULL returns NULL. Otherwise require a uniquely named list whose names are
  # all present in available_images. Return payloads unchanged.
}
```

Use base R only so the helper remains available in package checks without new runtime dependencies.

- [ ] **Step 4: Add an exporter-level failing test**

Construct a minimal Seurat object with a `CreateFOV(CreateCentroids(...))`, put a valid payload under `object@misc$cerebro_spatial_images$omnibus_fov`, call `exportFromSeurat()`, and assert:

```r
spatial <- readRDS(output)$getSpatialData("omnibus_fov")
expect_identical(spatial$histology_image, payload$histology_image)
expect_identical(spatial$histology_image_bounds, payload$histology_image_bounds)
```

Expected before implementation: FAIL because the image fields are absent.

- [ ] **Step 5: Wire validation and merge into `exportFromSeurat()`**

Immediately after resolving `has_images`, validate top-level payload names against `names(object@images)`. Refactor the image loop so `tryCatch()` wraps only `.getSpatialData()` and coordinate normalization; it returns `NULL` after issuing the existing warning when coordinate extraction fails. After that catch boundary, skip `NULL` entries and validate/merge the declared payload before `addSpatialData()`:

```r
if (is.null(spatial_data)) {
  next
}
if (!is.null(spatial_images[[image_name]])) {
  image_payload <- .validateCerebroSpatialImage(
    spatial_images[[image_name]],
    image_name,
    spatial_data$coordinates
  )
  spatial_data[names(image_payload)] <- image_payload
}
```

Validation errors and `addSpatialData()` errors must propagate. Only coordinate-extraction errors retain the existing warning behavior; malformed declared image payloads must not be swallowed by the spatial `tryCatch()`.

- [ ] **Step 6: Run focused tests**

Run:

```bash
R -q -e 'devtools::test(filter = "spatial-image-payload|spatial")'
```

Expected: PASS with no new warnings.

- [ ] **Step 7: Commit**

```bash
git add R/spatial_image_payload.R R/exportFromSeurat.R tests/testthat/test-spatial-image-payload.R
git commit -m "feat: preserve Seurat FOV images during export"
```

### Task 2: Pass Trekker data through Seurat conversion

**Files:**
- Modify: `R/exportFromSeurat.R:1600-1665`
- Modify: `tests/testthat/test-trekker.R`

- [ ] **Step 1: Write a failing pass-through test**

Build a minimal Seurat object, attach a representative list, export it, and compare the value:

```r
trekker <- list(
  coordinates = data.frame(cell = colnames(object), x = seq_along(colnames(object)), y = 1),
  metadata = data.frame(cell = colnames(object), cell_type = "synthetic"),
  positioning_qc = list(pct_positioned = 100),
  morans_i = data.frame(gene = "Gene001", morans_i = 0.5),
  evidence = list()
)
object@misc$trekker <- trekker
exportFromSeurat(object, file = output, groups = "cluster", slot = "data", verbose = FALSE)
expect_equal(readRDS(output)$getTrekker(), trekker)
```

- [ ] **Step 2: Run the test and observe `NULL`**

Run:

```bash
R -q -e 'devtools::test(filter = "trekker")'
```

Expected: FAIL because `getTrekker()` returns `NULL`.

- [ ] **Step 3: Implement the pass-through**

Add this before spatial export:

```r
if (!is.null(object@misc$trekker)) {
  export$addTrekker(object@misc$trekker)
}
```

The existing `addTrekker()` contract rejects non-list values.

- [ ] **Step 4: Run focused tests and commit**

Run:

```bash
R -q -e 'devtools::test(filter = "trekker|exportFromSeurat")'
```

Expected: PASS.

```bash
git add R/exportFromSeurat.R tests/testthat/test-trekker.R
git commit -m "feat: export Trekker payloads from Seurat"
```

### Task 3: Build the deterministic Omnibus Seurat fixture

**Files:**
- Create: `data-raw/build_omnibus_demo.R`
- Create: `data-raw/omnibus.md`
- Modify: `data-raw/DATASETS.md`

- [ ] **Step 1: Write the artifact contract test first**

Create `tests/testthat/test-omnibus-pipeline.R` with an initial test that loads both expected paths and asserts they exist. It must then assert the Seurat dimensions are exactly 80 genes by 120 cells, the CRB inherits `Cerebro`, and both share cell and gene identifiers.

- [ ] **Step 2: Run the test and observe missing artifacts**

Run:

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline")'
```

Expected: FAIL because `demo_omnibus_seurat.rds` and `demo_omnibus.crb` do not exist.

- [ ] **Step 3: Implement deterministic expression and metadata construction**

In `build_omnibus_demo.R`, use `set.seed(20260810)`, 80 stable gene names, 120 stable cell names, a sparse count matrix, and deterministic metadata columns:

```r
metadata <- data.frame(
  orig.ident = rep(c("sample_A", "sample_B"), each = 60),
  seurat_clusters = rep(c("0", "1", "2", "3"), each = 30),
  cell_type = rep(c("T cell", "B cell", "Myeloid", "Stromal"), each = 30),
  phase = rep(c("G1", "S", "G2M"), length.out = 120),
  row.names = cell_names
)
object <- Seurat::CreateSeuratObject(counts = counts, meta.data = metadata)
object <- Seurat::NormalizeData(object, verbose = FALSE)
```

Add deterministic PCA/UMAP embeddings directly with `CreateDimReducObject()` so the build does not depend on stochastic neighbor algorithms.

- [ ] **Step 4: Add the synthetic FOV and image payload**

Create 120 centroid coordinates inside `[0, 1000] x [0, 800]`, attach them under `object[["omnibus_fov"]]`, generate a small RGB PNG with tissue-like gradients and cluster-shaped regions, encode it with `base64enc`, and attach it using the exact `cerebro_spatial_images` contract.

- [ ] **Step 5: Populate every optional payload**

Add deterministic, non-empty structures under the existing `misc` contracts for gene lists, marker genes, most/mean expression, enriched pathways, a simple `phylo` tree, a list-based trajectory with metadata and edges, unified TCR+BCR repertoire rows tied to valid barcodes, canonical class I/class II HLA typing, extra table/ggplot, and a minimal valid Trekker payload using the same cells.

- [ ] **Step 6: Convert without CRB mutation and validate**

Save a staged Seurat RDS, call:

```r
convertSeuratToCerebro(
  seurat_file = staged_seurat,
  result_dir = stage_dir,
  assay = "RNA",
  slot = "data",
  experiment_name = "Synthetic Omnibus",
  organism = "Human",
  groups = c("seurat_clusters", "orig.ident", "cell_type", "phase"),
  cell_cycle = "phase",
  add_most_expressed_genes = FALSE,
  verbose = FALSE
)
```

Rename only the staged converter output to `demo_omnibus.crb`; do not open and mutate the CRB. Reread both staged files and validate every declared feature, spatial image containment, `Cerebro` class, and 80-by-120 expression dimensions before replacing destination files.

- [ ] **Step 7: Document and register the fixture**

In `data-raw/omnibus.md`, record the fixed seed, dimensions, synthetic status, every populated Viewer surface, image payload contract, exact rebuild command, and generated outputs. Add matching source/provenance/build/output entries to `data-raw/DATASETS.md`.

- [ ] **Step 8: Generate artifacts and rerun the contract test**

Run:

```bash
Rscript data-raw/build_omnibus_demo.R
R -q -e 'devtools::test(filter = "omnibus-pipeline")'
```

Expected: PASS and both artifact files are present.

- [ ] **Step 9: Commit**

```bash
git add data-raw/build_omnibus_demo.R data-raw/omnibus.md data-raw/DATASETS.md tests/testthat/test-omnibus-pipeline.R inst/extdata/examples/demo_omnibus_seurat.rds inst/extdata/examples/demo_omnibus.crb
git commit -m "feat: add synthetic Omnibus Seurat pipeline"
```

### Task 4: Make Omnibus the default Viewer dataset

**Files:**
- Modify: `inst/app.R:20-85`
- Modify: `tests/testthat/test-omnibus-pipeline.R`
- Modify: `NEWS.md:1-15`

- [ ] **Step 1: Add a failing source-app contract test**

Parse the `crb_file_to_load` block and assert that its first named entry is exactly `"Omnibus" = "extdata/examples/demo_omnibus.crb"`, `crb_pick_smallest_file` is false, and Omnibus has no entry in `spatial_images`.

- [ ] **Step 2: Run and observe the PBMC-first failure**

Run:

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline")'
```

Expected: FAIL because PBMC is currently first.

- [ ] **Step 3: Update app configuration and release notes**

Insert Omnibus first, rewrite the surrounding comment to identify it as a synthetic feature-coverage fixture, retain all current datasets, and add a CerebroNexus 4.2 NEWS bullet describing the reproducible Seurat source and end-to-end conversion proof.

- [ ] **Step 4: Run focused app tests and commit**

Run:

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline|app-inst|spatial")'
```

Expected: PASS.

```bash
git add inst/app.R NEWS.md tests/testthat/test-omnibus-pipeline.R
git commit -m "feat: make Omnibus the default demo"
```

### Task 5: Verify conversion and standalone app creation

**Files:**
- Modify: `tests/testthat/test-omnibus-pipeline.R`

- [ ] **Step 1: Add the end-to-end integration test**

Copy the committed Seurat RDS to a temporary input path, call `convertSeuratToCerebro()` with the builder's documented arguments, then assert semantic equality for expression dimensions/names, groups, spatial image/bounds, trajectory methods, repertoire, HLA, Trekker, and extra material. Call:

```r
createShinyApp(
  cerebro_data = c(Omnibus = generated_crb),
  result_dir = app_dir,
  launch_browser = FALSE,
  verbose = FALSE
)
```

Assert `app.R`, `cerebro_config.rds`, Viewer source directories, and one private CRB exist; reread the private CRB and verify it remains `Cerebro` with an embedded image. Source `utility_functions.R` into a clean environment and assert it contains no `CerebroNexus::` dependency.

- [ ] **Step 2: Run the complete Omnibus integration test**

Run:

```bash
R -q -e 'devtools::test(filter = "omnibus-pipeline")'
```

Expected: PASS.

- [ ] **Step 3: Run final package verification**

Run:

```bash
git diff --check
R -q -e 'devtools::test()'
R CMD INSTALL .
R -q -e 'devtools::check(document = FALSE, manual = FALSE, cran = FALSE)'
```

Expected: focused and full tests pass, installation succeeds, and package check reports no new errors or warnings attributable to this branch.

- [ ] **Step 4: Perform final specification and quality review**

Compare every design acceptance criterion against code and test evidence. Check `rg -n "demo_omnibus|cerebro_spatial_images"` for undocumented or dead paths, verify all nine pre-existing CRBs still load, and confirm neither `/Users/nuioi/projects/shiny/_wt_builder_auth` nor `/Users/nuioi/projects/shiny/_wt_coord_views` changed.

- [ ] **Step 5: Commit final integration coverage**

```bash
git add tests/testthat/test-omnibus-pipeline.R
git commit -m "test: verify Omnibus app generation"
```
