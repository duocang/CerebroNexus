test_that("new viewer defaults match unsorted, unlabelled Seurat plots", {
  sys.source(
    viewer_test_path("core", "viewer_content_contract.R"),
    environment()
  )
  defaults <- builderViewerDisplayDefaults()
  expect_identical(defaults$gene_plotting_order, "Input order")
  expect_true(all(!unlist(defaults[grepl("group_labels$", names(defaults))])))
  expect_identical(
    builderViewerDisplayDefaults(list(
      gene_plotting_order = "Highest expression on top"
    ))$gene_plotting_order,
    "Highest expression on top"
  )
  # "Random" never shuffled cells; it is rejected rather than reinterpreted.
  expect_error(
    builderViewerDisplayDefaults(list(gene_plotting_order = "Random")),
    "Input order"
  )
  expect_identical(
    builderViewerDisplayDefaults(list(group_labels = TRUE))$group_labels,
    TRUE
  )
})

test_that("expression paint order preserves cells when disabled", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_identical(
    system2(
      "node",
      c(
        shQuote(test_path("fixtures", "expression-paint-order.js")),
        shQuote(viewer_test_path("www", "cell_views.js"))
      )
    ),
    0L
  )
})
