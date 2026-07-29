skip_if_no_layered_seurat <- function() {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  skip_if_not(
    utils::compareVersion(
      as.character(utils::packageVersion("Seurat")),
      "5.0.0"
    ) >=
      0,
    "requires Seurat v5"
  )
}

make_public_layered_object <- function() {
  skip_if_no_layered_seurat()

  set.seed(7301)
  genes <- c(
    "MT-ND1",
    "RPL3",
    "RPS4X",
    paste0("Gene", seq_len(37))
  )
  cells <- paste0("Cell", seq_len(16))
  counts <- matrix(
    rpois(length(genes) * length(cells), lambda = 4) + 1,
    nrow = length(genes),
    dimnames = list(genes, cells)
  )
  object <- suppressWarnings(
    Seurat::CreateSeuratObject(counts = counts)
  )
  object$sample <- rep(c("sample_a", "sample_b"), each = 8)
  object$cluster <- rep(c("cluster_1", "cluster_2"), times = 8)
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object[["RNA"]] <- split(object[["RNA"]], f = object$sample)
  object
}

test_that("calculatePercentGenes handles split counts layers", {
  object <- make_public_layered_object()
  before <- SeuratObject::Layers(object[["RNA"]])

  result <- calculatePercentGenes(
    object = object,
    assay = "RNA",
    genes = list(marker = c("MT-ND1", "Gene1"))
  )

  expect_type(result, "list")
  expect_length(result$marker, ncol(object))
  expect_setequal(names(result$marker), colnames(object))
  expect_identical(SeuratObject::Layers(object[["RNA"]]), before)
})

test_that("getMostExpressedGenes handles split counts layers", {
  object <- make_public_layered_object()
  before <- SeuratObject::Layers(object[["RNA"]])

  expect_no_error(
    result <- suppressMessages(
      getMostExpressedGenes(
        object = object,
        assay = "RNA",
        groups = "sample"
      )
    )
  )

  expect_s4_class(result, "Seurat")
  expect_s3_class(result@misc$most_expressed_genes$sample, "data.frame")
  expect_setequal(
    unique(
      as.character(result@misc$most_expressed_genes$sample$sample)
    ),
    unique(as.character(object$sample))
  )
  expect_identical(SeuratObject::Layers(object[["RNA"]]), before)
})

test_that("addPercentMtRibo handles split counts layers", {
  object <- make_public_layered_object()
  before <- SeuratObject::Layers(object[["RNA"]])

  expect_no_error(
    result <- suppressMessages(
      addPercentMtRibo(
        object = object,
        assay = "RNA",
        organism = "hg",
        gene_nomenclature = "name"
      )
    )
  )

  expect_true(all(c("percent_mt", "percent_ribo") %in% colnames(result[[]])))
  expect_equal(nrow(result[[]]), ncol(object))
  expect_identical(SeuratObject::Layers(object[["RNA"]]), before)
})

test_that("gene-set enrichment gets past split data resolution", {
  skip_if_not_installed("GSVA")
  skip_if_not_installed("qvalue")
  object <- make_public_layered_object()
  missing_gmt <- file.path(tempdir(), "layered-does-not-exist.gmt")

  expect_error(
    performGeneSetEnrichmentAnalysis(
      object = object,
      assay = "RNA",
      GMT_file = missing_gmt,
      groups = "sample"
    ),
    "Specified GMT file with gene sets cannot be found",
    fixed = TRUE
  )
})

test_that("spatial extraction can reuse an already resolved matrix", {
  object <- make_public_layered_object()
  object$x <- seq_len(ncol(object))
  object$y <- rev(seq_len(ncol(object)))
  resolution <- .getExpressionMatrix(
    seurat = object,
    assay = "RNA",
    slot = "data",
    join_samples = TRUE,
    allow_cross_semantic_fallback = FALSE,
    return_resolution = TRUE
  )

  expect_no_error(
    spatial <- .getSpatialData(
      object = object,
      layer = "data",
      assay = "RNA",
      coord_source = "metadata",
      coord_cols = c("x", "y"),
      expression_data = resolution$data,
      expression_layer = resolution$resolved
    )
  )

  expect_equal(ncol(spatial$expression), ncol(object))
  expect_setequal(colnames(spatial$expression), colnames(object))
  expect_identical(rownames(spatial$coordinates), colnames(object))
  expect_identical(spatial$requested_layer, "data")
  expect_identical(spatial$layer, resolution$resolved)
})

test_that("spatial payload records a cross-semantic fallback honestly", {
  skip_if_no_layered_seurat()
  counts <- matrix(
    seq_len(24),
    nrow = 4,
    dimnames = list(
      paste0("g", seq_len(4)),
      paste0("c", seq_len(6))
    )
  )
  object <- suppressWarnings(
    Seurat::CreateSeuratObject(counts = counts)
  )
  object$x <- seq_len(ncol(object))
  object$y <- rev(seq_len(ncol(object)))

  expect_warning(
    resolution <- .getExpressionMatrix(
      seurat = object,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE,
      return_resolution = TRUE
    ),
    "falling back to `counts`",
    fixed = TRUE
  )

  spatial <- .getSpatialData(
    object = object,
    layer = resolution$requested,
    assay = "RNA",
    coord_source = "metadata",
    coord_cols = c("x", "y"),
    expression_data = resolution$data,
    expression_layer = resolution$resolved
  )

  expect_identical(spatial$requested_layer, "data")
  expect_identical(spatial$layer, "counts")
})

test_that("public consumers do not cross semantic classes", {
  object <- make_public_layered_object()
  for (layer in grep(
    "^counts\\.",
    SeuratObject::Layers(object[["RNA"]]),
    value = TRUE
  )) {
    suppressWarnings(
      SeuratObject::LayerData(object[["RNA"]], layer = layer) <- NULL
    )
  }

  expect_error(
    calculatePercentGenes(
      object = object,
      assay = "RNA",
      genes = list(marker = "Gene1")
    ),
    "no same-semantic fallback",
    fixed = TRUE
  )
})

test_that("resolution records distinguish requested and physical layers", {
  skip_if_no_layered_seurat()
  counts <- matrix(
    seq_len(24),
    nrow = 4,
    dimnames = list(
      paste0("g", seq_len(4)),
      paste0("c", seq_len(6))
    )
  )
  object <- suppressWarnings(
    Seurat::CreateSeuratObject(counts = counts)
  )

  expect_warning(
    resolution <- .getExpressionMatrix(
      seurat = object,
      assay = "RNA",
      slot = "data",
      join_samples = FALSE,
      allow_cross_semantic_fallback = TRUE,
      return_resolution = TRUE
    ),
    "falling back to `counts`",
    fixed = TRUE
  )

  expect_type(resolution, "list")
  expect_identical(resolution$requested, "data")
  expect_identical(resolution$resolved, "counts")
  expect_equal(
    resolution$data,
    SeuratObject::LayerData(
      object[["RNA"]],
      layer = "counts"
    )
  )
})

test_that("expression-cell validation preserves or restores object order", {
  cells <- paste0("c", seq_len(6))
  matrix_in_order <- matrix(
    seq_len(18),
    nrow = 3,
    dimnames = list(paste0("g", seq_len(3)), cells)
  )

  unchanged <- .validate_expression_cells(
    expression_data = matrix_in_order,
    object_cells = cells,
    assay = "RNA",
    requested_layer = "data",
    resolved_layer = "data"
  )
  expect_identical(unchanged, matrix_in_order)

  shuffled <- matrix_in_order[, rev(cells), drop = FALSE]
  reordered <- .validate_expression_cells(
    expression_data = shuffled,
    object_cells = cells,
    assay = "RNA",
    requested_layer = "data",
    resolved_layer = "data"
  )
  expect_identical(colnames(reordered), cells)
  expect_identical(reordered, matrix_in_order)
})

test_that("expression-cell validation reports both sides of a mismatch", {
  expression <- matrix(
    seq_len(12),
    nrow = 3,
    dimnames = list(
      paste0("g", seq_len(3)),
      c("c1", "c2", "unexpected", "c4")
    )
  )

  err <- tryCatch(
    {
      .validate_expression_cells(
        expression_data = expression,
        object_cells = c("c1", "c2", "c3", "c4"),
        assay = "RNA",
        requested_layer = "data",
        resolved_layer = "counts"
      )
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )

  expect_false(is.na(err))
  expect_true(grepl("Missing object cells (1): c3", err, fixed = TRUE))
  expect_true(grepl(
    "Unexpected matrix cells (1): unexpected",
    err,
    fixed = TRUE
  ))
  expect_true(grepl("requested `data`", err, fixed = TRUE))
  expect_true(grepl("resolved `counts`", err, fixed = TRUE))
})

test_that("expression-cell validation requires usable column names", {
  expression <- matrix(seq_len(12), nrow = 3)

  expect_error(
    .validate_expression_cells(
      expression_data = expression,
      object_cells = paste0("c", seq_len(4)),
      assay = "RNA",
      requested_layer = "data",
      resolved_layer = "data"
    ),
    "cell names",
    fixed = TRUE
  )
})
