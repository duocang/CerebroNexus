overview_helpers <- new.env(parent = globalenv())
sys.source(viewer_test_path("core", "dataset_overview.R"), overview_helpers)
sys.source(viewer_test_path("load_data", "overview_ui.R"), overview_helpers)

overview_fixture <- function() {
  list(
    getExperiment = function() list(description = "  ", source = NA_character_),
    getMetaData = function() {
      data.frame(sample = c("a", "a", "b", NA), cluster = c("T", "T", "B", "B"))
    },
    getGroups = function() c("sample", "cluster"),
    getGeneNames = function() c("A", "B", "C"),
    availableProjections = function() "umap",
    availableSpatial = function() character(),
    getExpressionMatrix = function(...) {
      stop("Must never read expression values")
    }
  )
}

test_that("overview uses known metadata and omits blank information", {
  result <- overview_helpers$viewerDatasetOverview(overview_fixture())
  expect_null(result$description)
  expect_null(result$source)
  expect_identical(result$genes, 3L)
  expect_identical(result$samples, 2L)
  expect_equal(result$sample$counts, c(a = 2, b = 1))
  expect_equal(result$sample$missing, 1)
  expect_named(result$annotations, "cluster")
  html <- as.character(overview_helpers$viewerOverviewUI(list(
    label = "<script>dataset</script>",
    cells = 4,
    overview = result
  )))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("Data source|Exported|not available|Spatial views", html))
})

test_that("bundle creation embeds the same compact overview", {
  object <- Cerebro$new()
  object$setMetaData(data.frame(sample = c("a", "b")))
  object$addGroup("sample", c("a", "b"))
  info <- CerebroNexus:::.readBundleDatasetInfo(object, "Dataset", "data.crb")
  expect_identical(info$overview$samples, 2L)
  expect_equal(info$overview$sample$counts, c(a = 1, b = 1))
})

test_that("missing metadata produces no invented sample or gene counts", {
  object <- overview_fixture()
  object$getMetaData <- function() data.frame(condition = c("a", "b"))
  object$getGroups <- function() "condition"
  object$getGeneNames <- function() NULL
  result <- overview_helpers$viewerDatasetOverview(object)
  expect_null(result$samples)
  expect_null(result$sample)
  expect_null(result$genes)
  expect_named(result$annotations, "condition")
})

test_that("all categories remain separately visible in large summaries", {
  object <- overview_fixture()
  object$getMetaData <- function() data.frame(sample = rep(seq_len(200), 5))
  object$getGroups <- function() "sample"
  result <- overview_helpers$viewerDatasetOverview(object)
  expect_length(result$sample$counts, 200)
  expect_equal(result$sample$total, 1000)
  expect_equal(sum(result$sample$counts) + result$sample$remaining, 1000)
  expect_equal(result$samples, 200)
  expect_false(overview_helpers$viewerOverviewTruncated(result))
  html <- as.character(overview_helpers$viewerOverviewBars(result$sample))
  expect_equal(
    lengths(regmatches(
      html,
      gregexpr('class="dataset-bar-row"', html, fixed = TRUE)
    )),
    200
  )
  expect_false(grepl("Top 10|Other categories", html))
  legacy <- result
  legacy$sample$counts <- head(result$sample$counts, 10)
  legacy$sample$remaining <- 950
  expect_true(overview_helpers$viewerOverviewTruncated(legacy))
  expect_false(overview_helpers$viewerOverviewTruncated(NULL))
})

test_that("explicit sample column works without name heuristics", {
  object <- overview_fixture()
  object$getExperiment <- function() {
    list(sample_column = "donor", description = "Study description")
  }
  object$getMetaData <- function() data.frame(donor = c("a", "b", "a"))
  object$getGroups <- function() "donor"
  result <- overview_helpers$viewerDatasetOverview(object)
  expect_equal(result$samples, 2)
  expect_length(result$annotations, 0)
  expect_identical(result$description, "Study description")
})

test_that("thin CRB summaries obtain gene counts from matrix headers", {
  skip_if_not_installed("HDF5Array")
  fixture <- viewer_h5_fixture(
    withr::local_tempdir(),
    values = matrix(
      0,
      3,
      2,
      dimnames = list(c("A", "B", "C"), c("x", "y"))
    ),
    stem = "thin"
  )
  info <- CerebroNexus:::.readBundleDatasetInfo(
    fixture$object,
    "Thin",
    fixture$crb
  )
  expect_identical(info$overview$genes, 3L)
  expect_null(fixture$object$expression)
})

test_that("counts match for factors, blanks, whitespace and ties", {
  object <- overview_fixture()
  object$getMetaData <- function() {
    data.frame(
      sample = factor(c("b", "a", "a", " ", NA)),
      cluster = c("T", "B", "B", "T", ""),
      stringsAsFactors = FALSE
    )
  }
  result <- overview_helpers$viewerDatasetOverview(object)
  expect_equal(result$sample$counts, c(a = 2, b = 1))
  expect_identical(result$sample$levels, 2L)
  expect_equal(result$sample$total, 3)
  expect_equal(result$sample$missing, 2)
  expect_equal(result$annotations$cluster$counts, c(B = 2, T = 2))
  expect_equal(result$annotations$cluster$missing, 1)
})

test_that("a supplied gene count replaces reading the gene names", {
  object <- overview_fixture()
  object$getGeneNames <- function() stop("Must not attach the matrix")
  expect_identical(
    overview_helpers$viewerDatasetOverview(object, gene_count = 27943)$genes,
    27943L
  )
  expect_null(overview_helpers$viewerDatasetOverview(object)$genes)
})

test_that("the overview of a Cerebro object does not hydrate cell names", {
  object <- new.env()
  makeActiveBinding(
    "meta_data",
    function() stop("Must not read cell names"),
    object
  )
  attr(object, "cerebro_projection_first_frame") <- list(
    meta_data = data.frame(sample = c("a", "a", "b"))
  )
  object$getMetaData <- function() stop("Must not read cell names")
  object$getExperiment <- function() list()
  object$getGroups <- function() "sample"
  object$getGeneNames <- function() c("A", "B")
  object$availableProjections <- function() "umap"
  object$availableSpatial <- function() character()
  result <- overview_helpers$viewerDatasetOverview(object)
  expect_equal(result$sample$counts, c(a = 2, b = 1))
  expect_identical(result$genes, 2L)
})

test_that("supplied projection names replace reading them from the object", {
  object <- overview_fixture()
  object$availableProjections <- function() stop("Must not hydrate")
  result <- overview_helpers$viewerDatasetOverview(
    object,
    projections = c("UMAP", "PCA")
  )
  expect_identical(result$projections, c("UMAP", "PCA"))
})

test_that("category bars escape labels and format counts", {
  html <- as.character(overview_helpers$viewerOverviewBars(list(
    counts = c("<b>T</b>" = 1234, B = 766),
    total = 2000,
    missing = 0
  )))
  expect_match(html, 'title="&lt;b&gt;T&lt;/b&gt;"', fixed = TRUE)
  expect_match(html, ">&lt;b&gt;T&lt;/b&gt;</span>", fixed = TRUE)
  expect_match(html, "1,234", fixed = TRUE)
  expect_match(html, 'style="width:61.7%"', fixed = TRUE)
  expect_match(html, "<small>38.3%</small>", fixed = TRUE)
  expect_false(grepl("dataset-note", html, fixed = TRUE))
})
