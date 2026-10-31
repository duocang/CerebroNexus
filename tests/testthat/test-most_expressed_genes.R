# test-most_expressed_genes.R — Tests for most expressed genes module

example_crb <- system.file(
  "extdata/examples/example.crb",
  package = "CerebroNexus"
)

## UI.R, table.R and select_group.R are sourced or parsed by the page tests.
test_that("most_expressed_genes server parses without errors", {
  expect_no_error(parse(
    file = viewer_test_path("most_expressed_genes", "server.R")
  ))
})

test_that("example.crb most expressed genes class methods work", {
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
