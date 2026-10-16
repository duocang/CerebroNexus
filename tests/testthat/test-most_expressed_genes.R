# test-most_expressed_genes.R — Tests for most expressed genes module

shiny_root <- system.file("viewer", package = "CerebroNexus")
example_crb <- system.file(
  "extdata/examples/example.crb",
  package = "CerebroNexus"
)

test_that("most_expressed_genes module files parse without errors", {
  mod_files <- c("UI.R", "server.R", "table.R", "select_group.R")
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "most_expressed_genes", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("most_expressed_genes UI defines correct tabName", {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("most_expressed_genes", "UI.R"), scope)
  page <- htmltools::tagQuery(scope$tab_most_expressed_genes)
  expect_equal(page$filter("#shiny-tab-mostExpressedGenes")$length(), 1L)
})

test_that("example.crb most expressed genes class methods work", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  groups <- crb$getGroupsWithMostExpressedGenes()
  expect_true(is.character(groups))
  expect_true(length(groups) > 0)
  result <- crb$getMostExpressedGenes(groups[1])
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("gene" %in% colnames(result))
})

test_that("precomputed expression getters validate their own keys", {
  crb <- Cerebro$new()
  pct <- data.frame(sample = "A", gene = "CD3D", pct = 1)
  mean <- data.frame(sample = "A", gene = "CD3D", mean_expr = 2)
  crb$groups <- list(sample = c("A", "B"))
  crb$meta_data <- data.frame(unrelated = 1)
  crb$most_expressed_genes <- list(sample = pct)
  crb$mean_expression <- list(sample = mean)

  expect_identical(crb$getMostExpressedGenes("sample"), pct)
  expect_identical(crb$getMeanExpression("sample"), mean)
  expect_error(crb$getMostExpressedGenes("missing"), "not available")
  expect_error(crb$getMeanExpression("missing"), "not available")
})

test_that("utility wrappers in inst/viewer parse without error", {
  util_file <- file.path(shiny_root, "utility_functions.R")
  skip_if_not(file.exists(util_file))
  expect_no_error(parse(file = util_file))
})
