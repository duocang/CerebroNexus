## Tests for Seurat v5 split (layered) objects.
##
## `split(assay, f = ...)` produces one layer per group, named `<root>.<suffix>`
## where the suffix is whatever the splitting factor's levels are -- sample names
## far more often than integers. Layer resolution used to recognise only the
## numeric form, so an object split by sample name was never joined and the
## export silently kept the first sample's layer alone.
##
## All tests require Seurat; skipped gracefully when it is not installed.

skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")

## `split()` on an assay, and layers as a concept, arrived with Seurat 5. The
## package still declares Seurat (>= 3.0.0), so an older environment has to skip
## rather than fail on a fixture it cannot build.
skip_if_not(
  utils::compareVersion(
    as.character(utils::packageVersion("Seurat")),
    "5.0.0"
  ) >=
    0,
  "split layers require Seurat >= 5"
)

## ---------------------------------------------------------------------------
## Fixtures
## ---------------------------------------------------------------------------

## A minimal normalised object with a UMAP, split by the factor given in `by`.
## `by` is a plain vector: pass characters for `counts.s1`-style layers and
## integers for the legacy `counts.1` form.
make_split_object <- function(by, n_genes = 60, n_cells = 20) {
  set.seed(42)
  counts <- matrix(
    rpois(n_genes * n_cells, 3),
    nrow = n_genes,
    ncol = n_cells,
    dimnames = list(
      paste0("g", seq_len(n_genes)),
      paste0("c", seq_len(n_cells))
    )
  )
  obj <- Seurat::CreateSeuratObject(
    counts = methods::as(counts, "CsparseMatrix")
  )
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj$sample <- rep(by, length.out = n_cells)
  obj$cluster <- factor(rep(c("c1", "c2"), length.out = n_cells))
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      rnorm(n_cells * 2),
      ncol = 2,
      dimnames = list(colnames(obj), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )
  obj[["RNA"]] <- split(obj[["RNA"]], f = obj$sample)
  obj
}

export_args <- function(object, file, ...) {
  c(
    list(
      object = object,
      assay = "RNA",
      experiment_name = "split layers",
      organism = "mm",
      groups = c("sample", "cluster"),
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      file = file,
      verbose = FALSE
    ),
    list(...)
  )
}

## ---------------------------------------------------------------------------
## Semantic-root matching
## ---------------------------------------------------------------------------

test_that(".filter_same_semantic_layers strips non-numeric split suffixes", {
  available <- c("counts.s1", "counts.s2", "data.s1", "data.s2")

  expect_setequal(
    .filter_same_semantic_layers("data", available),
    c("data.s1", "data.s2")
  )
  expect_setequal(
    .filter_same_semantic_layers("counts", available),
    c("counts.s1", "counts.s2")
  )
})

test_that(".filter_same_semantic_layers keeps `scale.data` off the `data` root", {
  ## `scale.data` contains a dot itself, so a naive `sub("\\.[^.]+$", "", x)`
  ## roots it as `scale` and matches nothing -- while `scale.data.s1` would root
  ## as `scale.data` and appear to be a valid fallback for a `data` request.
  ## Both directions have to stay separate.
  available <- c("counts.s1", "data.s1", "scale.data.s1")

  expect_setequal(
    .filter_same_semantic_layers("scale.data", available),
    "scale.data.s1"
  )
  expect_setequal(
    .filter_same_semantic_layers("data", available),
    "data.s1"
  )
  expect_false(
    "scale.data.s1" %in% .filter_same_semantic_layers("data", available)
  )
})

## ---------------------------------------------------------------------------
## Layer resolution on split objects
## ---------------------------------------------------------------------------

test_that("a sample-name-split object resolves to the joined `data` layer", {
  obj <- make_split_object(c("s1", "s2"))
  expect_setequal(
    SeuratObject::Layers(obj[["RNA"]]),
    c("counts.s1", "counts.s2", "data.s1", "data.s2")
  )

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )

  ## every cell, not just the first sample's half
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))

  ## normalised values, not counts standing in for them
  expect_true(any(matrix_out@x %% 1 != 0))
})

test_that("the legacy numeric split form keeps resolving as before", {
  obj <- make_split_object(c(1L, 2L))
  expect_setequal(
    SeuratObject::Layers(obj[["RNA"]]),
    c("counts.1", "counts.2", "data.1", "data.2")
  )

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_true(any(matrix_out@x %% 1 != 0))
})

test_that("a full-cell dotted custom layer is not treated as split", {
  obj <- make_split_object(c("s1", "s2"))
  obj[["RNA"]] <- SeuratObject::JoinLayers(obj[["RNA"]])
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "data.imputed"
  ) <- SeuratObject::LayerData(obj[["RNA"]], layer = "data")

  expect_no_message(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "counts",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE,
      verbose = TRUE
    )
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))
})

test_that("a full-cell custom layer is excluded from a real split join", {
  obj <- make_split_object(c("s1", "s2"))
  obj[["RNA"]] <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(obj[["RNA"]], layer = "data")
  imputed_data <- joined_data
  imputed_data@x <- imputed_data@x + 100
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "data.imputed"
  ) <- imputed_data
  obj[["RNA"]] <- split(obj[["RNA"]], f = obj$sample)

  expect_no_error(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))
  expect_equal(matrix_out, joined_data)
})

test_that("Seurat-prefix custom layers are excluded from a real split join", {
  for (custom_name in c("data_imputed", "dataBackup")) {
    obj <- make_split_object(c("s1", "s2"))
    obj[["RNA"]] <- SeuratObject::JoinLayers(obj[["RNA"]])
    joined_data <- SeuratObject::LayerData(obj[["RNA"]], layer = "data")
    custom_data <- joined_data
    custom_data@x <- custom_data@x + 100
    SeuratObject::LayerData(
      obj[["RNA"]],
      layer = custom_name
    ) <- custom_data
    obj[["RNA"]] <- split(obj[["RNA"]], f = obj$sample)

    expect_no_error(
      matrix_out <- .getExpressionMatrix(
        seurat = obj,
        assay = "RNA",
        slot = "data",
        join_samples = TRUE,
        allow_cross_semantic_fallback = TRUE
      )
    )
    expect_equal(matrix_out, joined_data, info = custom_name)
  }
})

test_that("an unrelated partial layer does not hide a real split partition", {
  obj <- make_split_object(c("s1", "s2"))
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")
  custom_cells <- c(
    colnames(obj)[obj$sample == "s1"][1:3],
    colnames(obj)[obj$sample == "s2"][1:3]
  )
  custom_data <- joined_data[, custom_cells, drop = FALSE]
  custom_data@x <- custom_data@x + 100
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "data.imputed"
  ) <- custom_data

  expect_no_error(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))
  expect_equal(matrix_out, joined_data)
})

test_that("a requested custom root resolves its complete split partition", {
  obj <- make_split_object(c("s1", "s2"))
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")

  for (sample in c("s1", "s2")) {
    sample_cells <- colnames(obj)[obj$sample == sample]
    SeuratObject::LayerData(
      obj[["RNA"]],
      layer = paste0("ambient.", sample)
    ) <- joined_data[, sample_cells, drop = FALSE]
  }

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "ambient",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    )
  )
  expect_equal(matrix_out, joined_data)
  expect_setequal(colnames(matrix_out), colnames(obj))
})

test_that("a requested custom root may itself contain a dot", {
  obj <- make_split_object(c("s1", "s2"))
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")

  for (sample in c("s1", "s2")) {
    sample_cells <- colnames(obj)[obj$sample == sample]
    SeuratObject::LayerData(
      obj[["RNA"]],
      layer = paste0("ambient.corrected.", sample)
    ) <- joined_data[, sample_cells, drop = FALSE]
  }

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "ambient.corrected",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    )
  )
  expect_equal(matrix_out, joined_data)
})

test_that("a full custom prefix is not substituted for an absent exact root", {
  obj <- make_split_object(c("s1", "s2"))
  obj[["RNA"]] <- SeuratObject::JoinLayers(obj[["RNA"]])
  imputed <- SeuratObject::LayerData(obj[["RNA"]], layer = "data")
  imputed@x <- imputed@x + 100
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "data.imputed"
  ) <- imputed
  suppressWarnings(
    SeuratObject::LayerData(obj[["RNA"]], layer = "data") <- NULL
  )

  expect_error(
    .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    ),
    "Exact layer `data` is absent",
    fixed = TRUE
  )
})

test_that("resolving a partition leaves every caller layer unchanged", {
  obj <- make_split_object(c("s1", "s2"))
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")
  custom_data <- joined_data
  custom_data@x <- custom_data@x + 100
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "dataBackup"
  ) <- custom_data

  before_names <- SeuratObject::Layers(obj[["RNA"]])
  before_custom <- SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "dataBackup"
  )

  expect_no_error(
    .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    )
  )

  expect_identical(SeuratObject::Layers(obj[["RNA"]]), before_names)
  expect_equal(
    SeuratObject::LayerData(obj[["RNA"]], layer = "dataBackup"),
    before_custom
  )
})

## ---------------------------------------------------------------------------
## Export entry point
## ---------------------------------------------------------------------------

test_that("a sample-split object exports every cell in embedded mode", {
  obj <- make_split_object(c("s1", "s2"))
  out_dir <- withr::local_tempdir()

  crb <- file.path(out_dir, "embedded.crb")
  expect_no_error(do.call(
    exportFromSeurat,
    export_args(obj, crb, expression_matrix_mode = "embedded")
  ))

  cerebro <- readRDS(crb)
  expect_equal(ncol(cerebro$expression), ncol(obj))
  expect_equal(nrow(cerebro$getMetaData()), ncol(obj))
  ## column order must match the meta data the export builds from Cells()
  expect_identical(colnames(cerebro$expression), SeuratObject::Cells(obj))
})

test_that("a sample-split object exports every cell in h5 mode", {
  ## The external backends are where a truncated matrix used to slip through
  ## unnoticed: they leave `self$expression` NULL, so nothing compared the
  ## matrix to the meta data and the .crb described more cells than it had.
  skip_if_not_installed("HDF5Array")

  obj <- make_split_object(c("s1", "s2"))
  out_dir <- withr::local_tempdir()

  crb <- file.path(out_dir, "external.crb")
  expect_no_error(do.call(
    exportFromSeurat,
    export_args(obj, crb, expression_matrix_mode = "h5")
  ))

  ## the sibling is stored cells x genes
  on_disk <- HDF5Array::TENxMatrix(
    file.path(out_dir, "external.h5"),
    group = "expression"
  )
  expect_equal(nrow(on_disk), ncol(obj))
  expect_equal(nrow(readRDS(crb)$getMetaData()), ncol(obj))
})

test_that("a sample-split object exports every cell in bpcells mode", {
  skip_if_not_installed("BPCells")

  obj <- make_split_object(c("s1", "s2"))
  out_dir <- withr::local_tempdir()

  crb <- file.path(out_dir, "external.crb")
  expect_no_error(do.call(
    exportFromSeurat,
    export_args(obj, crb, expression_matrix_mode = "bpcells")
  ))

  matrix_dir <- file.path(out_dir, "external.bpcells")
  expect_true(dir.exists(matrix_dir))
  on_disk <- BPCells::open_matrix_dir(dir = matrix_dir)
  expect_equal(ncol(on_disk), ncol(obj))
  expect_equal(nrow(readRDS(crb)$getMetaData()), ncol(obj))
})

test_that("a layer asked for by name is not joined away underneath the caller", {
  ## Joining consumes the split layers, so a request for one of them by name
  ## and a join cannot both happen. The substitution would be invisible: the
  ## joined `data` matches the request's own semantic root, so not even the
  ## cross-semantic warning fires, and a caller asking for one sample would
  ## silently receive every sample.
  obj <- make_split_object(c("s1", "s2"))
  cells_in_s2 <- colnames(obj)[obj$sample == "s2"]

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data.s2",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )

  expect_equal(ncol(matrix_out), length(cells_in_s2))
  expect_setequal(colnames(matrix_out), cells_in_s2)
})

test_that("a disk-backed assay is refused with the reason, not just a class", {
  ## Reading a BPCells- or DelayedArray-backed assay is not supported. That is
  ## unchanged here, but the refusal used to read `Received: RenameDims` and
  ## nothing else, which says neither what happened nor what to do -- and
  ## invites confusion with `expression_matrix_mode = "bpcells"`, which is
  ## about how the .crb stores its matrix, not how the source object holds it.
  skip_if_not_installed("BPCells")

  matrix_dir <- file.path(withr::local_tempdir(), "counts")
  dense <- matrix(
    rpois(30 * 12, 3),
    nrow = 30,
    dimnames = list(paste0("g", 1:30), paste0("c", 1:12))
  )
  BPCells::write_matrix_dir(
    methods::as(methods::as(dense, "CsparseMatrix"), "IterableMatrix"),
    dir = matrix_dir
  )
  obj <- Seurat::CreateSeuratObject(
    counts = BPCells::open_matrix_dir(dir = matrix_dir)
  )
  obj$sample <- rep(c("s1", "s2"), length.out = 12)
  obj[["RNA"]] <- split(obj[["RNA"]], f = obj$sample)

  err <- tryCatch(
    .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "counts",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("lives on disk", err, fixed = TRUE))
  expect_true(grepl("dgCMatrix", err, fixed = TRUE))
  expect_true(grepl("expression_matrix_mode", err, fixed = TRUE))

  ## The advice has to be safe on the object it is given. Converting only the
  ## requested layer would settle for one `counts.*` layer and leave the object
  ## holding a single sample -- advice that reproduces the bug this file is
  ## about is worse than no advice.
  advice_lines <- strsplit(err, "\n", fixed = TRUE)[[1]]
  code_start <- grep("^  (for \\(|layer <- )", advice_lines)[1]
  prose_start <- grep(
    "^(Convert every layer|Note that)",
    advice_lines
  )
  code_end <- min(prose_start[prose_start > code_start]) - 1L
  advice_code <- paste(
    sub("^  ", "", advice_lines[code_start:code_end]),
    collapse = "\n"
  )

  advice_env <- new.env(parent = globalenv())
  advice_env$seurat <- obj
  expect_no_warning(
    eval(parse(text = advice_code), envir = advice_env)
  )

  materialised <- .getExpressionMatrix(
    seurat = advice_env$seurat,
    assay = "RNA",
    slot = "counts",
    join_samples = TRUE,
    allow_cross_semantic_fallback = TRUE
  )
  expect_equal(ncol(materialised), ncol(dense))
  expect_setequal(colnames(materialised), colnames(dense))
})

test_that("exporting one named split layer fails on the cell count, clearly", {
  ## Honouring the request above means the export then holds a matrix covering
  ## one sample, which is exactly what the cell-count check exists to refuse --
  ## and it names the cause instead of blaming the meta data.
  obj <- make_split_object(c("s1", "s2"))

  err <- tryCatch(
    do.call(
      exportFromSeurat,
      export_args(
        obj,
        file.path(withr::local_tempdir(), "partial.crb"),
        slot = "data.s2"
      )
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("JoinLayers", err, fixed = TRUE))
  expect_true(grepl(as.character(ncol(obj)), err, fixed = TRUE))
})

test_that("a partial expression matrix is rejected identically in every mode", {
  ## An assay can hold a layer that covers only part of the object -- here by
  ## dropping one sample's counts before joining. Joining then yields a `counts`
  ## layer for half the cells, which no amount of layer-name matching can fix.
  ## The export has to refuse it rather than pair it with a full meta data table.
  obj <- make_split_object(c("s1", "s2"))
  suppressWarnings(
    SeuratObject::LayerData(obj[["RNA"]], layer = "counts.s2") <- NULL
  )

  out_dir <- file.path(tempdir(), "split_layers_partial")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  modes <- c("embedded", "h5")
  if (!requireNamespace("HDF5Array", quietly = TRUE)) {
    modes <- "embedded"
  }

  messages <- vapply(
    modes,
    function(mode) {
      args <- export_args(
        obj,
        file.path(out_dir, paste0(mode, ".crb")),
        slot = "counts",
        expression_matrix_mode = mode
      )
      tryCatch(
        {
          suppressWarnings(do.call(exportFromSeurat, args))
          NA_character_
        },
        error = function(e) conditionMessage(e)
      )
    },
    character(1)
  )

  expect_false(any(is.na(messages)))
  expect_true(all(grepl("JoinLayers", messages, fixed = TRUE)))
  expect_true(all(grepl(as.character(ncol(obj)), messages, fixed = TRUE)))
  ## the same defect must not report itself differently depending on where the
  ## matrix was headed
  expect_equal(length(unique(messages)), 1L)
})
