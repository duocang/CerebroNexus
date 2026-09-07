# HLA/TCR Publication Evidence Implementation Plan

> **For the implementing agent:** Required sub-skill: use
> `superpowers:executing-plans` and complete tasks in order. This side
> conversation must execute inline; subagents are unavailable.

**Goal:** Make the real HLA/TCR Cerebro case regenerate every numerical result,
table, figure, configuration, and Viewer screenshot used by the publication.

**Architecture:** Keep the tracked 12,000-cell CRB as the offline CI input and
retain the raw 10x rebuild as a release-only gate. A single publisher derives
both the strict TRB biological case and secondary CTgene Viewer case into a
staging directory, verifies the complete artifact set, and only then publishes.
Tests independently recompute biological results from the CRB.

**Technical stack:** R, CerebroNexus HLA/TCR core, `jsonlite`, `igraph`,
`ggplot2`, `digest`, `testthat`, `shinytest2`.

---

## File Structure

- Create `data-raw/hla_tcr_dextramer_sources.csv`: pinned official raw inputs.
- Create `data-raw/build_hla_tcr_publication.R`: sole publication entry point,
  derivation, staging, verification, tables, and scientific figures.
- Modify `data-raw/build_hla_tcr_dextramer_demo.R`: verify raw inputs and remove
  volatile build state.
- Delete `data-raw/prepare_hla_tcr_end_to_end_case.R` and
  `data-raw/build_hla_tcr_main_case.R`: superseded divergent publishers.
- Create `inst/extdata/examples/demo_hla_tcr_publication.manifest.json` and
  three publication CSV tables.
- Replace the three branch-specific tests with
  `test-hla-tcr-publication-data.R`,
  `test-hla-tcr-publication-artifacts.R`, and
  `test-hla-tcr-publication-browser.R`.
- Modify both HLA/TCR vignettes and the short guide to consume generated
  evidence and document one command.
- Modify `data-raw/README.md` and `data-raw/hla.md` for the two-tier workflow.
- Regenerate the existing HLA/TCR figures and screenshots in `vignettes/img/`.

### Task 1: Pin raw provenance and deterministic CRB construction

**Files:**
- Create: `data-raw/hla_tcr_dextramer_sources.csv`
- Modify: `data-raw/build_hla_tcr_dextramer_demo.R`
- Test: `tests/testthat/test-hla-tcr-publication-artifacts.R`

- [ ] **Step 1: Write the failing source-registry contract**

Add a test which reads the CSV, expects 12 unique donor/modality rows, validates
64-character SHA-256 values and positive byte sizes, and confirms the raw
builder references the registry and no longer uses `Sys.Date()`.

```r
expect_equal(nrow(sources), 12L)
expect_true(all(grepl("^[0-9a-f]{64}$", sources$sha256)))
expect_true(all(sources$bytes > 0))
expect_match(builder, "hla_tcr_dextramer_sources.csv", fixed = TRUE)
expect_no_match(builder, "Sys.Date()", fixed = TRUE)
```

- [ ] **Step 2: Run the test and verify the expected failure**

```bash
Rscript -e 'devtools::test(filter="hla-tcr-publication-artifacts")'
```

Expected: failure because the registry and test file do not yet exist.

- [ ] **Step 3: Compute and record actual cached input metadata**

Use the existing ignored `data-raw/vdj_10x_dextramer/` cache. Record exact
sizes and `shasum -a 256` results for four contig CSVs, four binarized matrices,
and four expression archives. Do not record the downloaded supplementary PDF;
the build uses the transcribed published HLA table.

- [ ] **Step 4: Enforce source bytes before parsing**

Load the registry once, match every donor/modality path, and stop before parsing
when size or SHA differs. Keep `.part` downloads and atomic rename. Make the
Seurat pipeline deterministic with `set.seed(42)`, explicit `seed.use = 42`,
and one UMAP worker. Store a fixed artifact-release date in the CRB.

- [ ] **Step 5: Run focused tests and commit**

```bash
Rscript -e 'devtools::test(filter="hla-tcr-publication-artifacts")'
git add data-raw/hla_tcr_dextramer_sources.csv \
  data-raw/build_hla_tcr_dextramer_demo.R \
  tests/testthat/test-hla-tcr-publication-artifacts.R
git commit -m "test(data): pin HLA/TCR source bytes"
```

### Task 2: Build one atomic publication generator

**Files:**
- Create: `data-raw/build_hla_tcr_publication.R`
- Delete: `data-raw/prepare_hla_tcr_end_to_end_case.R`
- Delete: `data-raw/build_hla_tcr_main_case.R`
- Create/modify: `inst/extdata/examples/demo_hla_tcr_*.json`
- Create: `inst/extdata/examples/demo_hla_tcr_publication.*.csv`
- Test: `tests/testthat/test-hla-tcr-publication-data.R`
- Test: `tests/testthat/test-hla-tcr-publication-artifacts.R`

- [ ] **Step 1: Write independent failing biological tests**

Load the CRB directly and independently derive:

```r
strict <- trb[
  trb$v_gene == "TRBV19" &
    trb$j_gene == "TRBJ1-5" &
    trb$cdr3 == "CASSIYSNQPQHF",
]
expect_equal(nrow(strict), 10L)
expect_equal(table(strict$sample), c(donor1 = 2L, donor2 = 8L))
expect_equal(strict_motif_nodes, 15L)
expect_equal(strict_motif_cells, 30L)

expect_equal(length(ctgene_cells), 293L)
expect_equal(table(ctgene_meta$sample), c(donor1 = 142L, donor2 = 151L))
expect_equal(ctgene_motif_nodes, 34L)
expect_equal(ctgene_motif_cells, 627L)
```

Also assert sparse 2,000 x 12,000 expression, aligned UMAP, four balanced
donors, paired TRA/TRB, and the exact independently genotyped HLA table.

- [ ] **Step 2: Write failing artifact tests**

Require the unified manifest and three CSVs. Compare their cell sets and counts
to the independently derived test values, not only to one another. Require all
manifest-declared hashes to match bytes on disk.

- [ ] **Step 3: Run both test files and verify expected failures**

```bash
Rscript -e 'devtools::test(filter="hla-tcr-publication-(data|artifacts)")'
```

Expected: biological data assertions pass; unified publication artifacts fail
because they do not exist.

- [ ] **Step 4: Implement the single publisher**

Support exactly these modes:

```r
mode <- match.arg(mode, c("from-crb", "from-raw"))
verify_only <- "--verify" %in% commandArgs(trailingOnly = TRUE)
screenshots <- "--screenshots" %in% commandArgs(trailingOnly = TRUE)
```

`from-raw` invokes the raw builder before the same derivation path. Derive both
cases once, sort every public collection deterministically, write all outputs
under an adjacent staging directory, validate them, then either compare
(`--verify`) or atomically replace final files.

- [ ] **Step 5: Regenerate machine-readable artifacts**

Run the non-verify offline publisher once and inspect the manifest and CSVs:

```bash
Rscript data-raw/build_hla_tcr_publication.R --from-crb
```

- [ ] **Step 6: Remove superseded publishers and update command references**

Delete the two older case-publisher scripts only after the unified command has
reproduced their existing JSON, barcode, and Linked views semantics.

- [ ] **Step 7: Run focused tests and commit**

```bash
Rscript -e 'devtools::test(filter="hla-tcr-publication-(data|artifacts)")'
git add data-raw inst/extdata/examples tests/testthat
git commit -m "feat(data): unify HLA/TCR evidence build"
```

### Task 3: Generate paper tables and scientific figures

**Files:**
- Modify: `data-raw/build_hla_tcr_publication.R`
- Modify: `vignettes/img/hla_tcr_*.svg`
- Modify: selected `vignettes/img/hla_tcr_*.png`
- Test: `tests/testthat/test-hla-tcr-publication-artifacts.R`

- [ ] **Step 1: Add failing figure/table contracts**

Require manifest entries for every publication table and scientific figure.
Validate CSV schemas, stable row ordering, non-empty SVG view boxes, declared
PNG dimensions, and matching hashes for non-raster artifacts.

- [ ] **Step 2: Verify the new contracts fail**

Run the artifact test and confirm it fails on missing generated figure metadata.

- [ ] **Step 3: Generate figures from derived evidence**

Generate the strict-clonotype UMAP, strict motif network, secondary workflow
context, and HLA context from the same in-memory derivation used for tables.
Use fixed dimensions, levels, colours, labels, and seeds. Preserve current
public filenames referenced by pkgdown.

- [ ] **Step 4: Regenerate and verify**

```bash
Rscript data-raw/build_hla_tcr_publication.R --from-crb
Rscript data-raw/build_hla_tcr_publication.R --from-crb --verify
Rscript -e 'devtools::test(filter="hla-tcr-publication-artifacts")'
```

- [ ] **Step 5: Commit**

```bash
git add data-raw/build_hla_tcr_publication.R \
  inst/extdata/examples vignettes/img \
  tests/testthat/test-hla-tcr-publication-artifacts.R
git commit -m "feat(data): generate HLA/TCR paper evidence"
```

### Task 4: Make both articles consume generated evidence

**Files:**
- Modify: `vignettes/hla_tcr_antigen_selected.Rmd`
- Modify: `vignettes/hla_tcr_main_case.Rmd`
- Modify: `docs/hla-tcr-end-to-end-case.md`
- Modify: `data-raw/README.md`
- Modify: `data-raw/hla.md`
- Test: `tests/testthat/test-hla-tcr-publication-artifacts.R`

- [ ] **Step 1: Add failing article-source contracts**

Require both Rmd files to load the generated manifest/CSV evidence and reject
duplicated hard-coded headline numbers such as literal selected-cell and motif
counts in prose.

- [ ] **Step 2: Verify the contracts fail**

Run the artifact test and confirm the existing literal prose is detected.

- [ ] **Step 3: Replace result literals with inline evidence**

Load generated evidence in a hidden setup chunk and use inline R for all result
counts, sequence identifiers, donor distributions, HLA context, and figure
captions. Keep methods constants explicit where they are scientific definitions.

- [ ] **Step 4: Reduce the short guide to navigation and commands**

Remove its third copy of the results narrative. Document the offline and raw
release commands and link to the two articles.

- [ ] **Step 5: Render and commit**

```bash
Rscript -e 'devtools::build_vignettes()'
Rscript -e 'pkgdown::build_site(new_process=TRUE, preview=FALSE)'
Rscript -e 'devtools::test(filter="hla-tcr-publication-artifacts")'
git add vignettes docs data-raw tests/testthat
git commit -m "docs: bind HLA/TCR results to evidence"
```

### Task 5: Capture and test the real Viewer journey

**Files:**
- Modify: `data-raw/build_hla_tcr_publication.R`
- Create: `tests/testthat/test-hla-tcr-publication-browser.R`
- Delete: `tests/testthat/test-hla-tcr-main-case-browser.R`
- Modify: `vignettes/img/hla_tcr_*.png`

- [ ] **Step 1: Write the failing full-selection browser assertion**

Load the generated configuration, restore it in the Viewer, return the complete
selected cell vector, and compare it with the expected set:

```r
expect_setequal(restored$cells, expected$cells)
expect_equal(length(restored$cells), length(expected$cells))
```

Navigate to HLA/TCR Motifs and require the real TRB motif state to become ready.

- [ ] **Step 2: Verify the browser test fails on the old journey**

```bash
Rscript -e 'devtools::test(filter="hla-tcr-publication-browser")'
```

Expected: failure because the replacement browser test and publication
screenshot mode are not complete.

- [ ] **Step 3: Implement screenshot mode**

Reuse the same readiness conditions as the browser test. Capture the six named
states at a fixed 1440 x 900 viewport and record dataset/selection fingerprints,
state names, dimensions, filenames, and source commit in the manifest.

- [ ] **Step 4: Capture, verify, and commit**

```bash
Rscript data-raw/build_hla_tcr_publication.R \
  --from-crb --screenshots
Rscript -e 'devtools::test(filter="hla-tcr-publication-browser")'
git add data-raw/build_hla_tcr_publication.R tests/testthat vignettes/img \
  inst/extdata/examples/demo_hla_tcr_publication.manifest.json
git commit -m "test(viewer): verify real HLA/TCR journey"
```

### Task 6: Release-level verification

**Files:** all changed files.

- [ ] **Step 1: Run offline reproducibility twice**

```bash
Rscript data-raw/build_hla_tcr_publication.R --from-crb --verify
Rscript data-raw/build_hla_tcr_publication.R --from-crb --verify
git status --short
```

Expected: both runs pass and the second run leaves no changes.

- [ ] **Step 2: Install and run focused tests**

```bash
R CMD INSTALL .
Rscript -e 'devtools::test(filter="hla-tcr-publication")'
```

- [ ] **Step 3: Run repository checks available on the PR #150 base**

The base has no `scripts/precheck.sh`, so run its constituent checks directly:

```bash
Rscript -e 'devtools::test(stop_on_failure=TRUE)'
R CMD build .
R CMD check --no-manual --as-cran CerebroNexus_*.tar.gz
Rscript -e 'pkgdown::build_site(new_process=TRUE, preview=FALSE)'
git diff --check
```

- [ ] **Step 4: Inspect the final publication diff**

Confirm no raw files, Seurat object, cache, temporary staging directory,
absolute host path, or volatile timestamp is tracked. Confirm the two cases are
described with distinct scientific roles.

- [ ] **Step 5: Commit verification-only corrections if needed**

Use a focused Conventional Commit only when verification required a source or
artifact correction. Do not create an empty verification commit.
