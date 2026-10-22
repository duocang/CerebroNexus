## Small H5-backed thin CRB for Viewer tests. `values` is genes x cells with
## dimnames; the matrix goes to `<stem>.h5` next to the CRB the way
## exportFromSeurat() writes it (cells x genes TENx layout, uncompressed).
viewer_h5_fixture <- function(
  root,
  values = NULL,
  stem = "fixture",
  projections = c("umap"),
  metadata = NULL,
  save = TRUE
) {
  if (is.null(values)) {
    values <- matrix(
      c(0, 1, 4, 2, 0, 5, 3, 6, 0, 7, 8, 9),
      nrow = 3L,
      dimnames = list(
        c("g1", "g2", "g3"),
        c("c1", "c2", "c3", "c4")
      )
    )
  }
  cells <- colnames(values)
  genes <- rownames(values)
  h5 <- file.path(root, paste0(stem, ".h5"))
  crb <- file.path(root, paste0(stem, ".crb"))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  sparse <- methods::as(Matrix::Matrix(values, sparse = TRUE), "CsparseMatrix")
  HDF5Array::writeTENxMatrix(
    methods::as(Matrix::t(sparse), "CsparseMatrix"),
    h5,
    group = "expression",
    level = 0L
  )

  object <- Cerebro$new()
  object$setExpressionBackend("h5", basename(h5))
  if (is.null(metadata)) {
    metadata <- data.frame(
      cell_barcode = cells,
      sample = factor(rep(c("a", "b"), length.out = length(cells))),
      row.names = cells
    )
  }
  object$setMetaData(metadata)
  object$addGroup("sample", levels(metadata$sample))
  ## A character vector names canonical projections; a named list of data
  ## frames is stored as given (for example with shuffled row names).
  if (is.character(projections)) {
    projections <- stats::setNames(
      lapply(projections, function(name) {
        data.frame(
          x = seq_along(cells),
          y = rev(seq_along(cells)),
          row.names = cells
        )
      }),
      projections
    )
  }
  for (name in names(projections)) {
    object$addProjection(name, projections[[name]])
  }
  if (save) {
    saveCerebro(object, crb, viewer_binary = "never")
  }
  list(
    crb = crb,
    h5 = h5,
    cells = cells,
    genes = genes,
    values = values,
    object = object
  )
}

## Runtime scope with the Viewer helpers loaded, as a generated app has them.
viewer_runtime_scope <- function() {
  scope <- viewer_runtime_env()
  scope
}
