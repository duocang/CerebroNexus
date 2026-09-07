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
