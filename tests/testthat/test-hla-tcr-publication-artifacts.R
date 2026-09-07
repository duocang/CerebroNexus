# test-hla-tcr-publication-artifacts.R — publication evidence contracts.

publication_root_candidates <- c(
  normalizePath(".", mustWork = FALSE),
  normalizePath("../..", mustWork = FALSE),
  normalizePath(testthat::test_path("../.."), mustWork = FALSE)
)
publication_root <- publication_root_candidates[file.exists(file.path(
  publication_root_candidates,
  "DESCRIPTION"
))][1]

publication_sha256 <- function(path) {
  if (nzchar(Sys.which("sha256sum"))) {
    output <- system2("sha256sum", path, stdout = TRUE)
    return(tolower(sub("[[:space:]].*$", "", output[[1L]])))
  }
  output <- system2("shasum", c("-a", "256", path), stdout = TRUE)
  tolower(sub("[[:space:]].*$", "", output[[1L]]))
}

test_that("HLA/TCR raw inputs are pinned and verified", {
  registry_file <- file.path(
    publication_root,
    "data-raw/hla_tcr_dextramer_sources.csv"
  )
  builder_file <- file.path(
    publication_root,
    "data-raw/build_hla_tcr_dextramer_demo.R"
  )

  expect_true(file.exists(registry_file))
  if (!file.exists(registry_file)) {
    return(invisible(NULL))
  }

  sources <- read.csv(registry_file, stringsAsFactors = FALSE)
  expect_identical(
    colnames(sources),
    c("donor", "modality", "filename", "url", "bytes", "sha256")
  )
  expect_equal(nrow(sources), 12L)
  expect_equal(nrow(unique(sources[c("donor", "modality")])), 12L)
  expect_true(all(sources$donor %in% paste0("donor", 1:4)))
  expect_true(all(sources$modality %in% c("contigs", "binarized", "gex")))
  expect_true(all(grepl("^https://cf[.]10xgenomics[.]com/", sources$url)))
  expect_true(all(sources$bytes > 0))
  expect_true(all(grepl("^[0-9a-f]{64}$", sources$sha256)))

  builder <- paste(readLines(builder_file, warn = FALSE), collapse = "\n")
  expect_match(builder, "hla_tcr_dextramer_sources.csv", fixed = TRUE)
  expect_false(grepl("Sys.Date()", builder, fixed = TRUE))
})

test_that("the unified publication artifact set is complete", {
  artifact_dir <- file.path(publication_root, "inst/extdata/examples")
  required <- c(
    "demo_hla_tcr_publication.manifest.json",
    "demo_hla_tcr_publication.cohort.csv",
    "demo_hla_tcr_publication.hla.csv",
    "demo_hla_tcr_publication.sequences.csv"
  )
  paths <- file.path(artifact_dir, required)
  expect_true(all(file.exists(paths)))
  if (!all(file.exists(paths))) {
    return(invisible(NULL))
  }

  manifest <- jsonlite::fromJSON(paths[[1L]], simplifyVector = FALSE)
  expect_identical(manifest$schema, "cerebronexus-hla-tcr-publication")
  expect_identical(as.integer(manifest$version), 1L)
  expect_identical(as.integer(manifest$dataset$cell_count), 12000L)
  expect_identical(as.integer(manifest$strict_case$cell_count), 10L)
  expect_identical(as.integer(manifest$strict_case$motif$node_count), 15L)
  expect_identical(as.integer(manifest$strict_case$motif$cell_count), 30L)
  expect_identical(as.integer(manifest$viewer_case$cell_count), 293L)
  expect_identical(as.integer(manifest$viewer_case$motif$node_count), 34L)
  expect_identical(as.integer(manifest$viewer_case$motif$cell_count), 627L)

  cohort <- read.csv(paths[[2L]], stringsAsFactors = FALSE)
  hla <- read.csv(paths[[3L]], stringsAsFactors = FALSE)
  sequences <- read.csv(paths[[4L]], stringsAsFactors = FALSE)
  expect_identical(cohort$donor, paste0("donor", 1:4))
  expect_identical(cohort$total_cells, rep(3000L, 4))
  expect_equal(nrow(hla), 14L)
  expect_equal(sum(sequences$case_id == "strict-clonotype"), 1L)
  expect_equal(sum(sequences$case_id == "viewer-expanded-clone"), 10L)

  for (artifact in manifest$artifacts) {
    artifact_path <- file.path(artifact_dir, artifact$file)
    expect_true(file.exists(artifact_path), info = artifact$file)
    expect_identical(
      unname(file.info(artifact_path)$size),
      as.numeric(artifact$bytes),
      info = artifact$file
    )
    expect_identical(
      publication_sha256(artifact_path),
      artifact$sha256,
      info = artifact$file
    )
  }

  figure_files <- c(
    "vignettes/img/hla_tcr_publication_umap.svg",
    "vignettes/img/hla_tcr_publication_motifs.svg",
    "vignettes/img/hla_tcr_publication_restriction.svg"
  )
  expect_identical(
    unname(vapply(manifest$figures, `[[`, character(1), "file")),
    figure_files
  )
  for (figure in manifest$figures) {
    figure_path <- file.path(publication_root, figure$file)
    expect_true(file.exists(figure_path), info = figure$file)
    if (!file.exists(figure_path)) {
      next
    }
    svg <- paste(readLines(figure_path, warn = FALSE), collapse = "\n")
    expect_match(svg, "<svg", fixed = TRUE)
    expect_match(svg, "viewBox=", fixed = TRUE)
    expect_identical(publication_sha256(figure_path), figure$sha256)
  }

  screenshot_metadata <- file.path(
    artifact_dir,
    "demo_hla_tcr_publication.screenshots.json"
  )
  expect_true(file.exists(screenshot_metadata))
  expect_length(manifest$screenshots, 8L)
  if (file.exists(screenshot_metadata) && length(manifest$screenshots) == 8L) {
    metadata <- jsonlite::fromJSON(screenshot_metadata, simplifyVector = FALSE)
    expect_identical(
      metadata$dataset_fingerprint,
      manifest$dataset$cell_fingerprint
    )
    expect_identical(metadata$source_commit, manifest$screenshot_source_commit)
    for (screenshot in manifest$screenshots) {
      screenshot_path <- file.path(publication_root, screenshot$file)
      expect_true(file.exists(screenshot_path), info = screenshot$file)
      expect_identical(as.integer(screenshot$width), 1440L)
      expect_identical(as.integer(screenshot$height), 900L)
      expect_identical(
        publication_sha256(screenshot_path),
        screenshot$sha256,
        info = screenshot$file
      )
    }
  }
})

test_that("HLA/TCR articles consume generated evidence", {
  strict_file <- file.path(
    publication_root,
    "vignettes/hla_tcr_antigen_selected.Rmd"
  )
  viewer_file <- file.path(publication_root, "vignettes/hla_tcr_main_case.Rmd")
  guide_file <- file.path(publication_root, "docs/hla-tcr-end-to-end-case.md")
  strict <- paste(readLines(strict_file, warn = FALSE), collapse = "\n")
  viewer <- paste(readLines(viewer_file, warn = FALSE), collapse = "\n")
  guide <- paste(readLines(guide_file, warn = FALSE), collapse = "\n")

  expect_match(strict, "demo_hla_tcr_publication.manifest.json", fixed = TRUE)
  expect_match(strict, "publication$strict_case", fixed = TRUE)
  expect_match(viewer, "demo_hla_tcr_publication.manifest.json", fixed = TRUE)
  expect_match(viewer, "publication$viewer_case", fixed = TRUE)
  expect_match(strict, "hla_tcr_publication_umap.svg", fixed = TRUE)
  expect_match(viewer, "hla_tcr_publication_motifs.svg", fixed = TRUE)

  browser_test <- paste(readLines(file.path(
    publication_root,
    "tests/testthat/test-hla-tcr-publication-browser.R"
  ), warn = FALSE), collapse = "\n")
  expect_match(browser_test, "expect_setequal", fixed = TRUE)
  expect_match(browser_test, "recalculating", fixed = TRUE)

  combined <- paste(strict, viewer, guide, sep = "\n")
  expect_false(grepl("prepare_hla_tcr_end_to_end_case.R", combined, fixed = TRUE))
  expect_false(grepl("build_hla_tcr_main_case.R", combined, fixed = TRUE))
  expect_match(
    guide,
    "build_hla_tcr_publication.R --from-crb --verify",
    fixed = TRUE
  )
})
