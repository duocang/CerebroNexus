# Builder performance implementation plan

> **For implementers:** Execute the checked steps in order and keep the two benchmark rounds independently measurable.

**Goal:** Optimize million-cell Builder import first, then integrate the PR5 Thin CRB/qs2 persistence contract.

**Architecture:** Round 1 reuses existing Builder source-integrity and profile structures instead of adding caches or workers. Round 2 ports only the CRB codec and wires Builder verification and generated-app loading to it.

**Technology:** R, Shiny, callr, Seurat, BPCells, qs2, testthat benchmark helpers.

---

### Task 1: Reuse local serialized snapshots

**Files:**
- Modify: `inst/builder/adapters.R`
- Exercise: `tests/testthat/test-builder-adapters.R`

- [ ] Make `builder_seurat_file_adapter()` opt into the existing reusable-source path.
- [ ] Preserve pre-read, post-read, and content-integrity checks.
- [ ] Exercise RDS and qs2 source reuse through the existing snapshot contract.

### Task 2: Reuse metadata analysis

**Files:**
- Modify: `inst/builder/profile.R`
- Modify: `inst/builder/inspect.R`
- Modify: `inst/builder/adapters.R`
- Exercise: `tests/testthat/test-builder-profile.R`

- [ ] Compute distinct values, missing values, and level counts once per metadata column.
- [ ] Build the legacy group choices and counts from the modern metadata profile without rescanning million-cell columns.
- [ ] Keep the legacy profile output contract unchanged.

### Task 3: Measure Round 1

**Files:**
- Create: `tests/bench/benchmark_builder_1m.R`

- [ ] Run native import, inspection, and snapshot in a fresh process against `mouse_brain_1m.rds`.
- [ ] Record wall time, snapshot bytes, and phase timing in a machine-readable result.

### Task 4: Port Thin CRB/qs2

**Files:**
- Create: `R/cerebro_io.R`
- Modify: `R/class-Cerebro.R`
- Modify: `R/exportFromSeurat.R`
- Modify: `R/exportFromSCE.R`
- Modify: `R/convertSeuratToCerebro.R`
- Modify: `R/createShinyApp.R`
- Modify: `inst/viewer/utility_functions.R`
- Modify: `inst/builder/build.R`
- Modify: `NAMESPACE`
- Exercise: `tests/testthat/test-cerebro-io.R`

- [ ] Port PR5 payload detection, Thin schema, atomic save, read, and conversion functions.
- [ ] Publish Builder CRBs with the codec-aware writer and verify them with the codec-aware reader.
- [ ] Bundle the matching generated-app reader without changing Viewer page logic.
- [ ] Preserve legacy RDS CRB compatibility and sidecar path validation.

### Task 5: Measure Round 2

**Files:**
- Modify: `tests/bench/benchmark_builder_1m.R`

- [ ] Measure legacy-compatible RDS and Thin qs2 writes on the same one-million-cell object and sidecar.
- [ ] Measure payload decode and hydrated read times.
- [ ] Compare output size, wall time, and peak RSS, then record the final commit and artifact fingerprints.
