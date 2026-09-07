async_workload_source_root <- testthat::test_path(
  "..",
  "..",
  "inst",
  "viewer"
)
async_workload_viewer_root <- if (
  file.exists(file.path(
    async_workload_source_root,
    "async_runtime.R"
  ))
) {
  async_workload_source_root
} else {
  system.file("viewer", package = "CerebroNexus")
}

read_viewer <- function(...) {
  paste(
    readLines(file.path(async_workload_viewer_root, ...)),
    collapse = "\n"
  )
}

test_that("motif graph construction is dispatched with latest-wins caching", {
  source <- read_viewer("hla_tcr_motifs", "data.R")
  runtime <- read_viewer("async_runtime.R")

  expect_match(source, "cerebro_async_latest_value", fixed = TRUE)
  expect_match(runtime, "worker = cerebro_async_source_call", fixed = TRUE)
  expect_match(
    source,
    'function_name = "hla_build_motif_graph_raw"',
    fixed = TRUE
  )
  expect_match(source, "hla_scoped_graph_job$invoke", fixed = TRUE)
  expect_match(source, "hla_global_graph_job$invoke", fixed = TRUE)
})

test_that("Moran's I is dispatched without transferring a matrix backend", {
  source <- read_viewer("spatial", "out_morans_i.R")

  expect_match(source, "cerebro_async_latest_value", fixed = TRUE)
  expect_match(source, 'function_name = "morans_i"', fixed = TRUE)
  expect_match(source, "x = coords[[1]][idx]", fixed = TRUE)
  expect_match(source, "values = expr[idx]", fixed = TRUE)
  expect_false(grepl("data_set = data_set", source, fixed = TRUE))
})

test_that("expensive scRepertoire plots run through daemon jobs", {
  source <- read_viewer("immune_repertoire", "visualizations.R")
  server <- read_viewer("immune_repertoire", "server.R")

  for (name in c(
    "abundance",
    "diversity",
    "homeostasis",
    "compare",
    "overlap",
    "rarefaction",
    "size_distribution"
  )) {
    expect_match(source, paste0("ir_", name, "_job"), fixed = TRUE)
  }
  expect_match(source, "cerebro_async_namespace_call", fixed = TRUE)
  expect_match(server, "req_scRepertoire <- function()", fixed = TRUE)
  expect_false(grepl('loadNamespace("scRepertoire")', server, fixed = TRUE))
})

test_that("trajectory projection preparation runs on immutable inputs", {
  projection <- read_viewer("trajectory", "projection_plot.R")
  worker <- file.path(
    async_workload_viewer_root,
    "trajectory",
    "async_workers.R"
  )

  expect_true(file.exists(worker))
  expect_match(projection, "trajectory_projection_job", fixed = TRUE)
  expect_match(
    projection,
    'function_name = "trajectory_prepare_projection"',
    fixed = TRUE
  )
  expect_match(projection, "cells_df = cells_df", fixed = TRUE)
  expect_false(grepl("data_set = data_set", projection, fixed = TRUE))

  density <- read_viewer("trajectory", "distribution_along_pseudotime.R")
  expect_match(density, "trajectory_density_job", fixed = TRUE)
  expect_match(
    density,
    'function_name = "trajectory_density_series"',
    fixed = TRUE
  )
})

test_that("gene expression uses one backend slice and daemon post-processing", {
  source <- read_viewer("gene_expression", "obj_projection_expression_levels.R")
  worker <- file.path(
    async_workload_viewer_root,
    "gene_expression",
    "async_workers.R"
  )

  expect_true(file.exists(worker))
  expect_match(source, "expression_projection_job", fixed = TRUE)
  expect_match(
    source,
    'function_name = "expression_prepare_levels"',
    fixed = TRUE
  )
  expect_match(
    source,
    "expression_matrix <- data_set()$getExpressionMatrix",
    fixed = TRUE
  )
  expect_false(grepl("getMeanExpressionForCells", source, fixed = TRUE))
  expect_false(grepl("data_set = data_set", source, fixed = TRUE))
})

test_that("linked views share bundles and batch gene reads", {
  source <- read_viewer("coordinated_views", "server.R")
  app <- read_viewer("shiny_server.R")

  expect_match(app, ".coordviews_process_cache", fixed = TRUE)
  expect_match(source, "cv_gene_job", fixed = TRUE)
  expect_match(source, 'function_name = "cv_prepare_gene_values"', fixed = TRUE)
  expect_match(source, "genes = genes", fixed = TRUE)
  expect_match(source, ".coordviews_process_cache", fixed = TRUE)
})

test_that("CRBs and external tables are shared or loaded off process", {
  utility <- read_viewer("utility_functions.R")
  app <- read_viewer("shiny_server.R")
  extra <- read_viewer("extra_material", "content.R")

  expect_match(app, ".crb_process_cache", fixed = TRUE)
  expect_match(utility, 'path %in% configured_paths', fixed = TRUE)
  expect_match(
    utility,
    'get(".crb_process_cache", inherits = TRUE)',
    fixed = TRUE
  )
  expect_match(extra, "extra_material_table_job", fixed = TRUE)
  expect_match(
    extra,
    'function_name = "extra_material_read_table"',
    fixed = TRUE
  )
})

test_that("plot and HLA archive writes are asynchronous", {
  hla <- read_viewer("hla_tcr_motifs", "visualizations.R")
  utility <- read_viewer("utility_functions.R")

  expect_match(hla, "hla_export_job", fixed = TRUE)
  expect_match(hla, 'function_name = "hla_build_export_archive"', fixed = TRUE)
  expect_match(utility, "saveViewerPlotAsync", fixed = TRUE)
  expect_match(utility, "cerebro_async_ggsave", fixed = TRUE)
  for (path in list(
    c("overview", "event_projection_export_plot.R"),
    c("gene_expression", "event_projection_export_plot.R"),
    c("spatial", "event_projection_export_plot.R"),
    c("trajectory", "projection_export.R")
  )) {
    expect_match(
      do.call(read_viewer, as.list(path)),
      "saveViewerPlotAsync",
      fixed = TRUE
    )
  }
})

test_that("pure async workers preserve scientific and export results", {
  source(
    file.path(
      async_workload_viewer_root,
      "gene_expression",
      "async_workers.R"
    ),
    local = TRUE
  )
  source(
    file.path(
      async_workload_viewer_root,
      "coordinated_views",
      "async_workers.R"
    ),
    local = TRUE
  )
  source(
    file.path(
      async_workload_viewer_root,
      "extra_material",
      "async_workers.R"
    ),
    local = TRUE
  )
  source(
    file.path(
      async_workload_viewer_root,
      "hla_tcr_motifs",
      "async_workers.R"
    ),
    local = TRUE
  )

  matrix <- matrix(
    1:6,
    nrow = 2,
    dimnames = list(c("A", "B"), c("c1", "c2", "c3"))
  )
  expect_equal(
    expression_prepare_levels(matrix, "aggregate", c("A", "B"), NULL, 3L),
    unname(colMeans(matrix))
  )
  expect_equal(
    cv_prepare_gene_values(matrix, c("B", "A"), c("c3", "c1")),
    list(B = c(6, 2), A = c(5, 1))
  )

  root <- withr::local_tempdir()
  unsafe <- data.frame("=formula" = "@cmd", check.names = FALSE)
  saveRDS(unsafe, file.path(root, "table.rds"))
  safe <- extra_material_read_table(root, "table.rds", "group", "sheet")
  expect_identical(names(safe), "'=formula")
  expect_identical(safe[[1L]], "'@cmd")

  withr::local_envvar(R_ZIPCMD = "")
  archive <- hla_build_export_archive(list(manifest = data.frame(x = 1L)))
  expect_type(archive, "raw")
  expect_gt(length(archive), 0L)
})
