## Native h5ad (AnnData) writer ---------------------------------------------------
##
## The reverse of readH5adAsSCE(): a Cerebro object is written to the AnnData
## on-disk format (encoding-version 0.1/0.2) with rhdf5, so no Python is
## involved and the file opens with anndata/scanpy as it is.
##
## rhdf5 notes that shape this file (checked against h5py):
##   * a logical vector is written as int8, but anndata needs an HDF5 enum
##     named FALSE/TRUE (what h5py calls bool), so booleans are written through
##     the low-level API with an enum type and a raw (1 byte/element) buffer;
##   * strings must be variable-length UTF-8, attributes scalar where AnnData
##     reads a scalar;
##   * an R matrix of dim (r, c) lands as an HDF5 dataset of shape (c, r), so
##     cells x dims embeddings are written transposed.

#' @keywords internal
#' @noRd
.h5adPutAttrs <- function(file, path, scalars = list(), arrays = list()) {
  fid <- rhdf5::H5Fopen(file)
  on.exit(rhdf5::H5Fclose(fid), add = TRUE)
  target <- rhdf5::H5Oopen(fid, path)
  on.exit(rhdf5::H5Oclose(target), add = TRUE, after = FALSE)
  for (name in names(scalars)) {
    value <- scalars[[name]]
    if (is.character(value)) {
      rhdf5::h5writeAttribute(
        enc2utf8(value),
        target,
        name,
        encoding = "UTF-8",
        variableLengthString = TRUE,
        asScalar = TRUE
      )
    } else {
      rhdf5::h5writeAttribute(as.integer(value), target, name, asScalar = TRUE)
    }
  }
  for (name in names(arrays)) {
    value <- arrays[[name]]
    if (is.character(value)) {
      rhdf5::h5writeAttribute(
        enc2utf8(value),
        target,
        name,
        encoding = "UTF-8",
        variableLengthString = TRUE
      )
    } else {
      rhdf5::h5writeAttribute(as.integer(value), target, name)
    }
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.h5adPutGroup <- function(file, path, type, version, ...) {
  rhdf5::h5createGroup(file, path)
  .h5adPutAttrs(
    file,
    path,
    scalars = c(list(`encoding-type` = type, `encoding-version` = version), list(...))
  )
}

#' @keywords internal
#' @noRd
.h5adPutStrings <- function(file, path, x, type = "string-array") {
  rhdf5::h5write(
    enc2utf8(as.character(x)),
    file,
    path,
    variableLengthString = TRUE,
    encoding = "UTF-8"
  )
  .h5adPutAttrs(
    file,
    path,
    scalars = list(`encoding-type` = type, `encoding-version` = "0.2.0")
  )
}

#' @keywords internal
#' @noRd
.h5adPutArray <- function(file, path, x) {
  rhdf5::h5write(x, file, path)
  .h5adPutAttrs(
    file,
    path,
    scalars = list(`encoding-type` = "array", `encoding-version` = "0.2.0")
  )
}

#' Chunked, compressed 1-d dataset: avoids one chunk the size of the whole
#' array, so the matrix can be read in slices.
#' @keywords internal
#' @noRd
.h5adPutVector <- function(file, path, x, storage.mode) {
  rhdf5::h5createDataset(
    file,
    path,
    dims = length(x),
    storage.mode = storage.mode,
    chunk = max(1L, min(length(x), 65536L)),
    level = 4L
  )
  rhdf5::h5write(x, file, path)
}

#' AnnData reads a bool only from an HDF5 enum with the members FALSE / TRUE.
#' @keywords internal
#' @noRd
.h5adPutBool <- function(file, path, x, encode = TRUE) {
  fid <- rhdf5::H5Fopen(file)
  on.exit(rhdf5::H5Fclose(fid), add = TRUE)
  tid <- rhdf5::H5Tenum_create("H5T_STD_I8LE")
  rhdf5::H5Tenum_insert(tid, "FALSE", 0L)
  rhdf5::H5Tenum_insert(tid, "TRUE", 1L)
  space <- rhdf5::H5Screate_simple(length(x))
  on.exit(rhdf5::H5Sclose(space), add = TRUE, after = FALSE)
  dataset <- rhdf5::H5Dcreate(fid, path, tid, space)
  on.exit(rhdf5::H5Dclose(dataset), add = TRUE, after = FALSE)
  rhdf5::H5Dwrite(
    dataset,
    as.raw(as.integer(x)),
    h5type = tid,
    h5spaceMem = space
  )
  if (encode) {
    .h5adPutAttrs(
      file,
      path,
      scalars = list(`encoding-type` = "array", `encoding-version` = "0.2.0")
    )
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.h5adPutCategorical <- function(file, path, x) {
  x <- if (is.factor(x)) x else factor(as.character(x))
  .h5adPutGroup(
    file,
    path,
    "categorical",
    "0.2.0",
    ordered = is.ordered(x)
  )
  codes <- as.integer(x) - 1L
  codes[is.na(codes)] <- -1L
  rhdf5::h5write(codes, file, paste0(path, "/codes"))
  rhdf5::h5write(
    enc2utf8(levels(x)),
    file,
    paste0(path, "/categories"),
    variableLengthString = TRUE,
    encoding = "UTF-8"
  )
  .h5adPutAttrs(
    file,
    paste0(path, "/categories"),
    scalars = list(`encoding-type` = "string-array", `encoding-version` = "0.2.0")
  )
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.h5adPutNullable <- function(file, path, values, mask, type) {
  .h5adPutGroup(file, path, type, "0.1.0")
  if (identical(type, "nullable-boolean")) {
    .h5adPutBool(file, paste0(path, "/values"), values, encode = FALSE)
  } else {
    rhdf5::h5write(values, file, paste0(path, "/values"))
  }
  .h5adPutBool(file, paste0(path, "/mask"), mask, encode = FALSE)
  invisible(TRUE)
}

#' Write one obs/uns table column in the matching AnnData encoding. Returns
#' FALSE when the column type has no AnnData counterpart (it is skipped).
#' @keywords internal
#' @noRd
.h5adPutColumn <- function(file, path, x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    x <- as.character(x)
  }
  if (is.factor(x)) {
    .h5adPutCategorical(file, path, x)
  } else if (is.character(x)) {
    if (anyNA(x) || length(unique(x)) <= length(x) / 2) {
      .h5adPutCategorical(file, path, x)
    } else {
      .h5adPutStrings(file, path, x)
    }
  } else if (is.logical(x)) {
    if (anyNA(x)) {
      .h5adPutNullable(
        file,
        path,
        ifelse(is.na(x), FALSE, x),
        is.na(x),
        "nullable-boolean"
      )
    } else {
      .h5adPutBool(file, path, x)
    }
  } else if (is.integer(x)) {
    if (anyNA(x)) {
      .h5adPutNullable(
        file,
        path,
        ifelse(is.na(x), 0L, x),
        is.na(x),
        "nullable-integer"
      )
    } else {
      .h5adPutArray(file, path, x)
    }
  } else if (is.numeric(x)) {
    .h5adPutArray(file, path, as.double(x))
  } else {
    return(FALSE)
  }
  TRUE
}

#' Write a data.frame as an AnnData dataframe group. Returns the names of the
#' columns that were written.
#' @keywords internal
#' @noRd
.h5adPutDataFrame <- function(file, path, index, columns) {
  names(columns) <- gsub("/", "_", names(columns), fixed = TRUE)
  names(columns) <- make.unique(names(columns))
  written <- character()
  .h5adPutGroup(file, path, "dataframe", "0.2.0")
  .h5adPutStrings(file, paste0(path, "/_index"), index)
  for (name in names(columns)) {
    ok <- tryCatch(
      .h5adPutColumn(file, paste0(path, "/", name), columns[[name]]),
      error = function(e) FALSE
    )
    if (isTRUE(ok)) {
      written <- c(written, name)
    }
  }
  .h5adPutAttrs(
    file,
    path,
    scalars = list(`_index` = "_index"),
    arrays = list(`column-order` = written)
  )
  written
}

#' The expression matrix as a genes x cells dgCMatrix, whatever backend holds it.
#' @keywords internal
#' @noRd
.crbExpressionAsCsparse <- function(x) {
  if (inherits(x, "dgCMatrix")) {
    return(x)
  }
  if (is.matrix(x)) {
    storage.mode(x) <- "double"
  }
  ## BPCells matrices only know the direct coercion; other Matrix classes
  ## (triplet, row-compressed, delayed) go through CsparseMatrix.
  tryCatch(
    methods::as(x, "dgCMatrix"),
    error = function(e) {
      methods::as(methods::as(x, "CsparseMatrix"), "dgCMatrix")
    }
  )
}

#' @keywords internal
#' @noRd
.h5adProjectionName <- function(name) {
  name <- gsub("[^A-Za-z0-9._-]", "_", name)
  if (tolower(name) %in% c("pca", "umap", "tsne")) {
    paste0("X_", tolower(name))
  } else {
    paste0("X_", name)
  }
}

#' Export a Cerebro file to AnnData (h5ad)
#'
#' @description
#' Writes the contents of a \code{.crb} file (or a Cerebro object) as an
#' \code{.h5ad} file that anndata and scanpy open directly, closing the loop
#' with \code{\link{convertH5adToCerebro}}. It is written natively with
#' \pkg{rhdf5}, without Python. Together with \code{\link{readH5adAsSCE}} this
#' makes \code{.crb} the hub between Seurat, SingleCellExperiment and AnnData:
#' \code{exportFromSeurat()} / \code{exportFromSCE()} /
#' \code{convertH5adToCerebro()} go in, \code{exportToH5ad()} comes out.
#'
#' @details
#' The mapping is:
#' \itemize{
#'   \item the expression matrix becomes \code{X}, stored as a CSR matrix of
#'     cells x genes. Cerebro keeps genes x cells, and the CSC of genes x cells
#'     is the CSR of cells x genes as it is, so no data are rearranged. It is
#'     whatever matrix the \code{.crb} was exported with (typically
#'     log-normalised), under any backend (embedded, h5, BPCells), and is
#'     loaded into memory as a sparse matrix.
#'   \item cell meta data becomes \code{obs}: factors and repeated strings as
#'     categoricals, logical, integer and numeric columns in their own types
#'     (missing values as nullable arrays or \code{NaN}).
#'   \item projections become \code{obsm}: \code{UMAP} is \code{X_umap},
#'     \code{TSNE} is \code{X_tsne}, \code{PCA} is \code{X_pca}, others
#'     \code{X_<name>}.
#'   \item marker gene tables are stored as data frames under
#'     \code{uns["cerebro"]["marker_genes"]}, which \code{\link{readH5adAsSCE}}
#'     reads back.
#' }
#' Missing values in a grouping variable are the explicit \code{"N/A"} level
#' that Cerebro gives them on export, so they arrive in \code{obs} as that
#' category rather than as \code{NaN}.
#' Immune repertoire, HLA typing, spatial data, trees, trajectories, enriched
#' pathways and extra material have no AnnData counterpart and are not
#' exported; a message names what was left out. The file is written to a
#' private temporary file and moved into place only when complete.
#'
#' @param input Path to a \code{.crb} file, or a \code{Cerebro} object.
#' @param file Output \code{.h5ad} path.
#' @param overwrite Replace an existing \code{file}; default \code{FALSE}.
#' @param verbose Report what was skipped.
#'
#' @return \code{file}, invisibly.
#'
#' @examples
#' \dontrun{
#' exportToH5ad("cerebro_pbmc.crb", "pbmc.h5ad")
#' # then, in Python: adata = anndata.read_h5ad("pbmc.h5ad")
#' }
#'
#' @seealso \code{\link{convertH5adToCerebro}}, \code{\link{readH5adAsSCE}},
#'   \code{\link{readCerebro}}
#'
#' @export
exportToH5ad <- function(input, file, overwrite = FALSE, verbose = FALSE) {
  .h5adRequire()
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(file)
  ) {
    stop("`file` must be one non-empty output path.", call. = FALSE)
  }
  if (dir.exists(file)) {
    stop("`file` points to a directory, not a .h5ad path: ", file, call. = FALSE)
  }
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("`file` already exists; use overwrite = TRUE: ", file, call. = FALSE)
  }
  object <- if (is.character(input)) {
    if (length(input) != 1L || !file.exists(input)) {
      stop("Cerebro file not found: ", paste(input, collapse = ", "), call. = FALSE)
    }
    readCerebro(input)
  } else {
    input
  }
  if (!inherits(object, "Cerebro")) {
    stop("`input` must be a .crb path or a Cerebro object.", call. = FALSE)
  }

  expression <- object$expression
  if (is.null(expression)) {
    stop("The Cerebro object has no expression matrix to export.", call. = FALSE)
  }
  cells <- colnames(expression)
  genes <- rownames(expression)
  if (is.null(cells) || is.null(genes)) {
    stop("The expression matrix needs cell and gene names.", call. = FALSE)
  }
  matrix <- .crbExpressionAsCsparse(expression)
  n_cells <- ncol(matrix)
  n_genes <- nrow(matrix)

  meta <- object$getMetaData()
  if (!is.null(meta$cell_barcode) && !identical(as.character(meta$cell_barcode), cells)) {
    meta <- meta[match(cells, meta$cell_barcode), , drop = FALSE]
  }
  columns <- as.list(meta[setdiff(names(meta), "cell_barcode")])

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(pattern = ".h5ad-stage-", tmpdir = dirname(file), fileext = ".h5ad")
  on.exit(unlink(stage, force = TRUE), add = TRUE)
  rhdf5::h5createFile(stage)
  .h5adPutAttrs(
    stage,
    "/",
    scalars = list(`encoding-type` = "anndata", `encoding-version` = "0.1.0")
  )

  ## X: the CSC of genes x cells is the CSR of cells x genes, slot for slot
  .h5adPutGroup(stage, "X", "csr_matrix", "0.1.0")
  .h5adPutAttrs(stage, "X", arrays = list(shape = c(n_cells, n_genes)))
  if (length(matrix@x)) {
    .h5adPutVector(stage, "X/data", as.double(matrix@x), "double")
    .h5adPutVector(stage, "X/indices", as.integer(matrix@i), "integer")
  } else {
    stop("The expression matrix has no non-zero values.", call. = FALSE)
  }
  rhdf5::h5write(as.integer(matrix@p), stage, "X/indptr")

  skipped <- character()
  obs_written <- .h5adPutDataFrame(stage, "obs", cells, columns)
  dropped <- setdiff(gsub("/", "_", names(columns), fixed = TRUE), obs_written)
  if (length(dropped)) {
    skipped <- c(skipped, paste0("obs columns without an AnnData type: ", paste(dropped, collapse = ", ")))
  }

  ## AnnData requires a column-order attribute and rhdf5 cannot write an empty
  ## one, so var carries scanpy's conventional gene_symbols column.
  .h5adPutDataFrame(stage, "var", genes, list(gene_symbols = genes))

  ## obsm
  .h5adPutGroup(stage, "obsm", "dict", "0.1.0")
  for (projection in object$availableProjections()) {
    embedding <- object$getProjection(projection)
    if (!is.null(rownames(embedding)) && all(cells %in% rownames(embedding))) {
      embedding <- embedding[cells, , drop = FALSE]
    }
    embedding <- as.matrix(embedding)
    if (nrow(embedding) != n_cells || !is.numeric(embedding)) {
      skipped <- c(skipped, paste0("projection ", projection, " (does not match the cells)"))
      next
    }
    .h5adPutArray(stage, paste0("obsm/", .h5adProjectionName(projection)), t(unname(embedding)))
  }

  ## uns: marker tables, readable by readH5adAsSCE()
  .h5adPutGroup(stage, "uns", "dict", "0.1.0")
  methods_with_markers <- object$getMethodsForMarkerGenes()
  if (length(methods_with_markers)) {
    .h5adPutGroup(stage, "uns/cerebro", "dict", "0.1.0")
    .h5adPutGroup(stage, "uns/cerebro/marker_genes", "dict", "0.1.0")
    for (method in methods_with_markers) {
      method_path <- paste0("uns/cerebro/marker_genes/", gsub("/", "_", method, fixed = TRUE))
      .h5adPutGroup(stage, method_path, "dict", "0.1.0")
      for (group in object$getGroupsWithMarkerGenes(method)) {
        table <- object$getMarkerGenes(method, group)
        if (is.data.frame(table) && nrow(table)) {
          .h5adPutDataFrame(
            stage,
            paste0(method_path, "/", gsub("/", "_", group, fixed = TRUE)),
            as.character(seq_len(nrow(table)) - 1L),
            as.list(table)
          )
        }
      }
    }
  }

  ## what AnnData has no place for
  has <- function(value) !is.null(value) && length(value) > 0L
  left_out <- c(
    if (has(tryCatch(object$getImmuneRepertoire(), error = function(e) NULL))) "immune repertoire",
    if (has(object$hla_typing)) "HLA typing",
    if (has(object$spatial)) "spatial data",
    if (length(object$trees)) "trees",
    if (length(object$trajectories)) "trajectories",
    if (length(object$enriched_pathways)) "enriched pathways",
    if (length(object$extra_material)) "extra material"
  )
  left_out <- c(left_out, skipped)
  if (length(left_out)) {
    message("[INFO] Not exported to h5ad: ", paste(left_out, collapse = "; "), ".")
  }

  ## publish: replace only once the new file is complete
  backup <- NULL
  if (file.exists(file)) {
    backup <- paste0(file, ".bak-", Sys.getpid())
    if (!file.rename(file, backup)) {
      stop("Could not replace the existing file: ", file, call. = FALSE)
    }
  }
  if (!file.rename(stage, file)) {
    if (!is.null(backup)) {
      file.rename(backup, file)
    }
    stop("Could not move the finished file into place: ", file, call. = FALSE)
  }
  if (!is.null(backup)) {
    unlink(backup, force = TRUE)
  }
  invisible(file)
}
