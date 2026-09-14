builder_repo_source("stats.R")

builder_stats_fixture <- function() {
  list(
    n_cells = 120L,
    n_genes = 230L,
    group_counts = list(
      cluster = stats::setNames(rep(10L, 14L), LETTERS[1:14])
    ),
    reductions = list(
      umap = list(
        dimensions = 2L,
        cells = list(count = 120L),
        structurally_valid = TRUE
      ),
      broken = list(
        dimensions = 1L,
        cells = list(count = 80L),
        structurally_valid = FALSE
      )
    ),
    spatial = list(section_count = 2L, sections = c("a", "b")),
    qc_values = list(nCount_RNA = seq_len(120L), nFeature_RNA = seq_len(120L))
  )
}

test_that("statistic frames fold long group tails and retain verified counts", {
  frame <- builder_stats_frame(
    builder_stats_fixture(),
    list(
      groups = "cluster",
      reductions = "umap",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA"
    )
  )

  expect_identical(frame$cells, 120L)
  expect_identical(frame$genes, 230L)
  expect_identical(nrow(frame$group_distribution), 13L)
  expect_identical(frame$group_distribution$bucket[[13L]], "Other")
  expect_identical(sum(frame$group_distribution$count), 140L)
  expect_identical(frame$projections$valid, TRUE)
  expect_identical(frame$spatial$sections, 2L)
})

test_that("QC samples are deterministic and capped below 1001 values", {
  profile <- builder_stats_fixture()
  profile$qc_values$nCount_RNA <- seq_len(5000L)
  frame <- builder_stats_frame(profile, list(nUMI = "nCount_RNA"))

  expect_lte(frame$qc_samples, 1000L)
  expect_lte(nrow(frame$qc), 1000L)
  expect_identical(frame$qc$value[[1L]], 1)
})

test_that("legacy inspected profiles stay renderable without raw objects", {
  profile <- list(
    n_cells = 80L,
    n_genes = 230L,
    reductions = c("umap", "pca"),
    images = "section-a",
    group_counts = list(cluster = c(alpha = 50L, beta = 30L))
  )
  frame <- builder_stats_frame(
    profile,
    list(groups = "cluster", reductions = "umap")
  )

  expect_identical(frame$group_distribution$bucket, c("alpha", "beta"))
  expect_identical(frame$projections$cells, 80L)
  expect_true(frame$projections$valid)
  expect_identical(frame$spatial$sections, 1L)
})

test_that("lightweight CRB profiles tolerate configured QC fields without samples", {
  profile <- list(
    n_cells = 80L,
    n_genes = 230L,
    reductions = "umap",
    group_counts = list()
  )

  frame <- builder_stats_frame(
    profile,
    list(
      reductions = "umap",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA"
    )
  )

  expect_identical(nrow(frame$qc), 0L)
  expect_identical(frame$qc_samples, 0L)
})
