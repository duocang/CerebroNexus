builder_plan_contract_source_runtime(environment())
builder_repo_source("marker_import.R")

builder_marker_import_xlsx <- function(sheets) {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path), envir = parent.frame())
  writexl::write_xlsx(sheets, path)
  path
}

test_that("marker import inventories delimited files and XLSX sheets", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(cluster = "B", gene = "MS4A1"),
    csv,
    row.names = FALSE
  )
  xlsx <- builder_marker_import_xlsx(list(
    all_cells = data.frame(
      cluster = c("B", "T"),
      gene = c("MS4A1", "CD3D")
    ),
    NK = data.frame(gene = "NKG7")
  ))

  got <- builder_marker_import_inventory(c(csv, xlsx))

  expect_identical(
    vapply(got, `[[`, character(1), "source_name"),
    c(basename(csv), "all_cells", "NK")
  )
  expect_true(all(vapply(got, `[[`, logical(1), "valid")))
})

test_that("single and multi cluster imports normalize to the Viewer contract", {
  levels <- c("B", "T", "NK")
  single <- builder_marker_import_map_single(
    builder_marker_import_source(
      "",
      "NK.csv",
      NULL,
      data.frame(gene = "NKG7")
    ),
    group = "cell_type",
    level = "NK",
    known_levels = levels
  )
  multiple <- builder_marker_import_map_multiple(
    builder_marker_import_source(
      "",
      "markers.csv",
      NULL,
      data.frame(
        label = c("B", "T"),
        gene = c("MS4A1", "CD3D")
      )
    ),
    group = "cell_type",
    column = "label",
    known_levels = levels
  )

  expect_identical(names(single$table)[[1L]], "cell_type")
  expect_identical(single$table$cell_type, "NK")
  expect_identical(names(multiple$table)[[1L]], "cell_type")
  expect_identical(
    builder_marker_import_coverage(list(single, multiple), levels)$missing,
    character()
  )
})

test_that("unknown and overlapping cluster assignments are rejected", {
  source <- builder_marker_import_source(
    "",
    "markers.csv",
    NULL,
    data.frame(gene = "NKG7")
  )
  unknown <- builder_marker_import_map_single(
    source,
    group = "cell_type",
    level = "Unknown",
    known_levels = "NK"
  )
  first <- builder_marker_import_map_single(
    source,
    group = "cell_type",
    level = "NK",
    known_levels = "NK"
  )
  duplicate <- builder_marker_import_validate_sources(
    list(first, first),
    known_levels = "NK"
  )

  expect_identical(unknown$error, "unknown_cluster")
  expect_identical(duplicate$error, "duplicate_cluster")
})

test_that("marker import merge preserves existing methods", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  object@misc$marker_genes <- list(
    cerebro_seurat = list(cluster = data.frame())
  )
  imported <- list(
    method = "Scanpy Wilcoxon",
    group = "cluster",
    sources = list(builder_marker_import_map_single(
      builder_marker_import_source(
        "",
        "B.csv",
        NULL,
        data.frame(gene = "MS4A1")
      ),
      group = "cluster",
      level = "B",
      known_levels = c("A", "B", "C")
    ))
  )

  got <- builder_attach_marker_imports(object, list(imported))

  expect_setequal(
    names(got@misc$marker_genes),
    c("cerebro_seurat", "Scanpy Wilcoxon")
  )
  expect_identical(
    got@misc$marker_genes[["Scanpy Wilcoxon"]][["cluster"]][[1L]],
    "B"
  )
})

test_that("marker import merge refuses an existing method name", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  object@misc$marker_genes <- list(cerebro_seurat = list())

  expect_error(
    builder_attach_marker_imports(
      object,
      list(list(
        method = "cerebro_seurat",
        group = "cluster",
        sources = list()
      ))
    ),
    "already exists"
  )
})
