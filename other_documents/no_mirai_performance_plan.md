# No-mirai Viewer performance phase 2 implementation plan

> **For the implementing agent:** use subagent-driven development and TDD for
> every production-code change in this plan.

**Goal:** Remove avoidable specialist-view startup work and large-data redraw,
hover, sampling, and gene-set aggregation costs without adding dependencies,
asynchronous runtimes, or cross-session caches.

**Architecture:** Keep the existing Shiny-to-Canvas protocol and expression
backend interfaces. Correct the visibility boundary, reuse per-field browser
calculations, coalesce pointer frames, subset hover work at its consumer, and
route gene-set aggregation through the existing block API.

**Technology:** R, R6, Shiny, base JavaScript Canvas 2D, testthat.

---

### Task 1: Specialist visibility, per-field caches, and pointer frames

**Files:**
- Modify: `inst/viewer/www/cell_views.js`
- Modify: `tests/testthat/test-coordinated-views.R`

- [ ] Add source-contract tests proving that dedicated pages report only
  `linkedVis`, build a dataset-fingerprinted specialist base, cache paint order
  and clip limits by field, and schedule pointer redraws through one pending
  `requestAnimationFrame`.
- [ ] Run `NOT_CRAN=true Rscript -e 'devtools::test(filter="coordinated-views", reporter="summary")'`
  and confirm the new assertions fail for the missing behaviour.
- [ ] Add the minimum JavaScript needed for those assertions. Reuse the existing
  dataset fingerprint and renderer state; do not add a second renderer or a
  generic scheduler abstraction.
- [ ] Re-run the same command and `node --check inst/viewer/www/cell_views.js`;
  both must exit zero.

### Task 2: Subset hover construction and index-only sampling

**Files:**
- Modify: `inst/viewer/shiny_server.R`
- Modify: `inst/viewer/overview/obj_projection_cells_to_show.R`
- Modify: `inst/viewer/gene_expression/obj_projection_cells_to_show.R`
- Modify: `inst/viewer/spatial/obj_projection_cells_to_show.R`
- Modify: specialist hover consumers under `inst/viewer/overview/`,
  `inst/viewer/gene_expression/`, and `inst/viewer/spatial/` only if required
  to preserve row ordering.
- Modify: `tests/testthat/test-viewer-sync-performance.R`

- [ ] Add tests proving hover formatting receives only requested indices and
  Overview/Gene/Spatial sampling returns valid, unique integer row positions
  without copying or sampling a metadata frame. Cover the scalar-index case so
  base R's `sample(x)` shorthand cannot sample `1:x` by mistake.
- [ ] Run `NOT_CRAN=true Rscript -e 'devtools::test(filter="viewer-sync-performance|viewer-review-feedback", reporter="summary")'`
  and confirm the new assertions fail for the missing behaviour.
- [ ] Move hover construction behind an index-aware session helper and replace
  metadata-frame samples and Spatial's two-step sample with a single integer
  sample. Preserve the existing percentage, group filtering, ordering, and
  empty-set semantics.
- [ ] Re-run the focused command and confirm zero failures.

### Task 3: Sparse gene-set aggregation

**Files:**
- Modify: `R/class-Cerebro.R`
- Modify: `tests/testthat/test-r-functions.R`

- [ ] Add tests comparing `getMeanExpressionForCells()` with known dense and
  sparse matrices, including cell subsets, missing cells, and empty genes.
- [ ] Run `NOT_CRAN=true Rscript -e 'devtools::test(filter="r-functions", reporter="summary")'`
  and confirm the new assertions fail because the method still calls the dense
  extraction path.
- [ ] Implement the minimum fix with `getExpressionBlock()` and backend-native
  `colMeans`, preserving names, ordering, validation, and existing errors.
- [ ] Re-run the focused command and confirm zero failures.

### Task 4: Isolated measurements and repository verification

**Files:**
- Modify or add only benchmark scripts/results already used by this branch if
  reproducible evidence needs to be retained.

- [ ] Measure before/after medians for specialist first-open latency, 100k-cell
  hover formatting, 8-gene redraw preparation at 100k and 500k cells, pointer
  redraw count, and sparse gene-set time/memory using the same fixtures and
  commands on both revisions.
- [ ] Run `node --check inst/viewer/www/cell_views.js` and Air on changed R files.
- [ ] Run `NOT_CRAN=true Rscript -e 'devtools::test(reporter="summary")'` once on
  the final tree and record pass/fail/warn/skip counts.
- [ ] Confirm `DESCRIPTION`, `NAMESPACE`, lockfiles, CI files, hooks, and
  environment files have no diff; inspect the complete branch diff for scope.
- [ ] Commit with English Conventional Commit messages, push only
  `origin/perf/viewer-no-mirai`, manually dispatch the two existing CI
  workflows for the exact pushed SHA, and wait for both conclusions.
