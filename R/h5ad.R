## Native h5ad (AnnData) reader --------------------------------------------------
##
## Reads the AnnData on-disk format (anndata >= 0.8, encoding-version 0.1/0.2)
## straight from HDF5 with rhdf5, so no Python, reticulate or zellkonverter is
## involved. The result is a SingleCellExperiment, the hub every other route
## already converts from.
##
## AnnData keeps cells x genes (obs x var); Cerebro and SCE keep genes x cells.
## A CSR matrix of cells x genes is byte-for-byte the CSC matrix of genes x
## cells, so the common sparse case needs no transpose and no copy of the data.

#' @keywords internal
#' @noRd
.h5adRequire <- function() {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop(
      "Reading h5ad files requires the 'rhdf5' package. Install it via ",
      "BiocManager::install(\"rhdf5\").",
      call. = FALSE
    )
  }
}

#' Catalogue of the file's objects, so existence and object type are looked up
#' once instead of reopening the file for every question.
#' @keywords internal
#' @noRd
.h5adContext <- function(file) {
  listing <- rhdf5::h5ls(file, recursive = TRUE, datasetinfo = FALSE)
  parent <- ifelse(listing$group == "/", "", listing$group)
  list(
    file = file,
    path = paste0(parent, "/", listing$name),
    otype = listing$otype
  )
}

#' @keywords internal
#' @noRd
.h5adHas <- function(ctx, path) {
  paste0("/", sub("^/", "", path)) %in% ctx$path
}

#' @keywords internal
#' @noRd
.h5adIsGroup <- function(ctx, path) {
  idx <- match(paste0("/", sub("^/", "", path)), ctx$path)
  !is.na(idx) && identical(as.character(ctx$otype[[idx]]), "H5I_GROUP")
}

#' @keywords internal
#' @noRd
.h5adChildren <- function(ctx, path) {
  prefix <- paste0("/", sub("^/", "", path), "/")
  paths <- ctx$path[startsWith(ctx$path, prefix)]
  rest <- substring(paths, nchar(prefix) + 1L)
  unique(sub("/.*$", "", rest))
}

#' @keywords internal
#' @noRd
.h5adAttrs <- function(ctx, path) {
  tryCatch(rhdf5::h5readAttributes(ctx$file, path), error = function(e) list())
}

#' @keywords internal
#' @noRd
.h5adEncoding <- function(ctx, path) {
  type <- .h5adAttrs(ctx, path)[["encoding-type"]]
  if (is.null(type)) NA_character_ else as.character(type)[[1L]]
}

#' Drop HDF5's array wrapper and turn bool enums into logicals.
#' @keywords internal
#' @noRd
.h5adVector <- function(x) {
  if (is.factor(x)) {
    lv <- levels(x)
    if (length(lv) && all(lv %in% c("FALSE", "TRUE"))) {
      return(as.character(x) == "TRUE")
    }
    return(as.character(x))
  }
  as.vector(x)
}

#' Read one obs/var column (or index) in any of the AnnData encodings.
#' @keywords internal
#' @noRd
.h5adReadElement <- function(ctx, path) {
  type <- .h5adEncoding(ctx, path)
  if (is.na(type)) {
    type <- if (.h5adIsGroup(ctx, path)) "unknown-group" else "array"
  }
  switch(
    type,
    array = ,
    "string-array" = .h5adVector(rhdf5::h5read(ctx$file, path)),
    categorical = {
      codes <- as.integer(.h5adVector(rhdf5::h5read(ctx$file, paste0(path, "/codes"))))
      cats <- .h5adReadElement(ctx, paste0(path, "/categories"))
      ordered <- isTRUE(as.logical(.h5adAttrs(ctx, path)[["ordered"]]))
      codes[codes < 0L] <- NA_integer_
      factor(cats[codes + 1L], levels = as.character(cats), ordered = ordered)
    },
    "nullable-integer" = ,
    "nullable-boolean" = ,
    "nullable-string-array" = {
      values <- .h5adVector(rhdf5::h5read(ctx$file, paste0(path, "/values")))
      mask <- .h5adVector(rhdf5::h5read(ctx$file, paste0(path, "/mask")))
      if (identical(type, "nullable-boolean")) {
        values <- as.logical(values)
      }
      values[as.logical(mask)] <- NA
      values
    },
    stop(
      "Unsupported h5ad column encoding `",
      type,
      "` at `",
      path,
      "`.",
      call. = FALSE
    )
  )
}

#' Read an AnnData dataframe group (obs, var, raw/var) as an index plus columns.
#' @keywords internal
#' @noRd
.h5adReadDataFrame <- function(ctx, path, verbose = FALSE) {
  if (!.h5adHas(ctx, path)) {
    stop("h5ad file has no `", path, "` table.", call. = FALSE)
  }
  if (!.h5adIsGroup(ctx, path)) {
    stop(
      "`",
      path,
      "` is stored in the legacy (anndata < 0.7) compound-dataset layout. ",
      "Re-save the file with a current anndata, e.g. ",
      "`anndata.read_h5ad(f).write_h5ad(f)`.",
      call. = FALSE
    )
  }
  attrs <- .h5adAttrs(ctx, path)
  children <- .h5adChildren(ctx, path)
  index_name <- attrs[["_index"]]
  if (is.null(index_name) || !index_name %in% children) {
    index_name <- intersect(c("_index", "index"), children)[1L]
  }
  if (is.na(index_name)) {
    stop("h5ad table `", path, "` has no index.", call. = FALSE)
  }
  index <- as.character(.h5adReadElement(ctx, paste0(path, "/", index_name)))

  columns <- attrs[["column-order"]]
  columns <- if (is.null(columns)) {
    setdiff(children, c(index_name, "__categories"))
  } else {
    as.character(columns)
  }
  columns <- columns[nzchar(columns) & columns %in% children]
  values <- lapply(columns, function(column) {
    tryCatch(
      .h5adReadElement(ctx, paste0(path, "/", column)),
      error = function(e) {
        if (verbose) {
          message("[INFO] Skipping h5ad column `", column, "`: ", conditionMessage(e))
        }
        NULL
      }
    )
  })
  names(values) <- columns
  values <- values[!vapply(values, is.null, logical(1))]
  values <- values[vapply(values, length, integer(1)) == length(index)]
  list(index = index, columns = values)
}

#' @keywords internal
#' @noRd
.h5adAsFrame <- function(table) {
  frame <- data.frame(row.names = table$index, check.names = FALSE)
  for (column in names(table$columns)) {
    frame[[column]] <- table$columns[[column]]
  }
  frame
}

#' Read a cells x genes AnnData matrix as genes x cells.
#' @keywords internal
#' @noRd
.h5adReadMatrix <- function(ctx, path, n_obs, n_vars) {
  type <- .h5adEncoding(ctx, path)
  attrs <- .h5adAttrs(ctx, path)
  if (is.na(type) && !is.null(attrs[["h5sparse_format"]])) {
    type <- paste0(attrs[["h5sparse_format"]], "_matrix")
    attrs[["shape"]] <- attrs[["h5sparse_shape"]]
  }
  if (is.na(type) && !.h5adIsGroup(ctx, path)) {
    type <- "array"
  }

  if (type %in% c("csr_matrix", "csc_matrix")) {
    shape <- as.integer(attrs[["shape"]])
    if (!identical(shape, c(as.integer(n_obs), as.integer(n_vars)))) {
      stop(
        "`",
        path,
        "` has shape ",
        paste(shape, collapse = " x "),
        " but obs/var describe ",
        n_obs,
        " x ",
        n_vars,
        ".",
        call. = FALSE
      )
    }
    data <- as.numeric(.h5adVector(rhdf5::h5read(ctx$file, paste0(path, "/data"))))
    indices <- .h5adVector(rhdf5::h5read(ctx$file, paste0(path, "/indices")))
    indptr <- .h5adVector(rhdf5::h5read(ctx$file, paste0(path, "/indptr")))
    if (length(data) > .Machine$integer.max) {
      stop(
        "`",
        path,
        "` has more than 2^31 - 1 non-zero values, beyond a dgCMatrix.",
        call. = FALSE
      )
    }
    indices <- as.integer(indices)
    indptr <- as.integer(indptr)
    csr <- identical(type, "csr_matrix")
    ## genes x cells: a CSR (cells x genes) is the CSC of genes x cells as is
    dims <- if (csr) c(n_vars, n_obs) else c(n_obs, n_vars)
    major <- if (csr) n_obs else n_vars
    if (length(indptr) != major + 1L || length(indices) != length(data)) {
      stop("`", path, "` has inconsistent indptr/indices/data lengths.", call. = FALSE)
    }
    built <- tryCatch(
      methods::new("dgCMatrix", i = indices, p = indptr, x = data, Dim = dims),
      error = function(e) NULL
    )
    if (is.null(built)) {
      ## unsorted or duplicated indices within a slice: rebuild from triplets
      slice <- rep.int(seq_len(major), diff(indptr))
      built <- if (csr) {
        Matrix::sparseMatrix(i = indices + 1L, j = slice, x = data, dims = dims)
      } else {
        Matrix::sparseMatrix(i = slice, j = indices + 1L, x = data, dims = dims)
      }
      built <- methods::as(built, "CsparseMatrix")
    }
    return(if (csr) built else methods::as(Matrix::t(built), "CsparseMatrix"))
  }

  if (identical(type, "array")) {
    ## C-order (cells x genes) is read by rhdf5 as an R (genes x cells) array
    dense <- rhdf5::h5read(ctx$file, path)
    if (!identical(as.integer(dim(dense)), c(as.integer(n_vars), as.integer(n_obs)))) {
      stop("Dense matrix `", path, "` does not match obs/var.", call. = FALSE)
    }
    storage.mode(dense) <- "double"
    return(methods::as(dense, "CsparseMatrix"))
  }
  stop("Unsupported matrix encoding `", type, "` at `", path, "`.", call. = FALSE)
}

#' @keywords internal
#' @noRd
.h5adReductionName <- function(name) {
  name <- sub("^X_", "", name)
  if (tolower(name) %in% c("pca", "umap", "tsne")) toupper(name) else name
}

#' obsm entries with >= 2 numeric columns, as cells x dims matrices.
#' @keywords internal
#' @noRd
.h5adReadObsm <- function(ctx, cells, verbose = FALSE) {
  if (!.h5adHas(ctx, "obsm")) {
    return(list())
  }
  result <- list()
  for (entry in .h5adChildren(ctx, "obsm")) {
    path <- paste0("obsm/", entry)
    embedding <- tryCatch(
      {
        if (.h5adIsGroup(ctx, path)) {
          if (!identical(.h5adEncoding(ctx, path), "dataframe")) {
            stop("unsupported encoding")
          }
          table <- .h5adReadDataFrame(ctx, path)
          numeric <- table$columns[vapply(table$columns, is.numeric, logical(1))]
          do.call(cbind, numeric)
        } else {
          raw <- rhdf5::h5read(ctx$file, path)
          if (is.matrix(raw)) t(raw) else NULL
        }
      },
      error = function(e) NULL
    )
    if (is.null(embedding) || !is.matrix(embedding) || nrow(embedding) != length(cells) || ncol(embedding) < 2L) {
      if (verbose) {
        message("[INFO] Skipping obsm entry `", entry, "` (not a cells x >=2 numeric array).")
      }
      next
    }
    storage.mode(embedding) <- "double"
    if (is.null(colnames(embedding))) {
      colnames(embedding) <- paste0(.h5adReductionName(entry), "_", seq_len(ncol(embedding)))
    }
    rownames(embedding) <- cells
    result[[.h5adReductionName(entry)]] <- embedding
  }
  result
}

#' scanpy's rank_genes_groups results as Cerebro marker tables: one table per
#' `groupby` variable whose first column holds that variable's levels, which is
#' what the Viewer needs to offer its per-group filter.
#' @keywords internal
#' @noRd
.h5adReadRankGenes <- function(ctx) {
  if (!.h5adHas(ctx, "uns")) {
    return(list())
  }
  markers <- list()
  for (entry in .h5adChildren(ctx, "uns")) {
    base <- paste0("uns/", entry)
    needed <- paste0(base, c("/names", "/scores"))
    if (!.h5adIsGroup(ctx, base) || !all(vapply(needed, function(p) .h5adHas(ctx, p), logical(1)))) {
      next
    }
    read_field <- function(field) {
      path <- paste0(base, "/", field)
      if (.h5adHas(ctx, path)) tryCatch(rhdf5::h5read(ctx$file, path), error = function(e) NULL)
    }
    gene <- read_field("names")
    if (!is.data.frame(gene)) {
      next
    }
    params <- function(field) {
      path <- paste0(base, "/params/", field)
      if (.h5adHas(ctx, path)) as.character(.h5adVector(rhdf5::h5read(ctx$file, path)))[[1L]] else NA_character_
    }
    groupby <- params("groupby")
    method <- params("method")
    fields <- list(
      score = read_field("scores"),
      avg_log2FC = read_field("logfoldchanges"),
      p_val = read_field("pvals"),
      p_val_adj = read_field("pvals_adj")
    )
    tables <- lapply(names(gene), function(level) {
      table <- data.frame(
        level = level,
        gene = as.character(gene[[level]]),
        stringsAsFactors = FALSE
      )
      for (field in names(fields)) {
        if (is.data.frame(fields[[field]]) && level %in% names(fields[[field]])) {
          table[[field]] <- as.numeric(fields[[field]][[level]])
        }
      }
      table[!is.na(table$gene) & nzchar(table$gene), , drop = FALSE]
    })
    table <- do.call(rbind, tables)
    if (is.null(table) || !nrow(table)) {
      next
    }
    rownames(table) <- NULL
    group_name <- if (is.na(groupby)) entry else groupby
    names(table)[[1L]] <- group_name
    label <- paste0("scanpy: ", if (is.na(method)) entry else method)
    markers[[label]][[group_name]] <- table
  }
  markers
}

#' Marker tables that exportToH5ad() stores under uns["cerebro"]["marker_genes"],
#' so a Cerebro -> h5ad -> Cerebro trip keeps them.
#' @keywords internal
#' @noRd
.h5adReadCerebroMarkers <- function(ctx) {
  root <- "uns/cerebro/marker_genes"
  if (!.h5adHas(ctx, root)) {
    return(list())
  }
  markers <- list()
  for (method in .h5adChildren(ctx, root)) {
    method_path <- paste0(root, "/", method)
    for (table in .h5adChildren(ctx, method_path)) {
      frame <- tryCatch(
        {
          raw <- .h5adReadDataFrame(ctx, paste0(method_path, "/", table))
          frame <- .h5adAsFrame(raw)
          rownames(frame) <- NULL
          frame
        },
        error = function(e) NULL
      )
      if (is.data.frame(frame) && nrow(frame)) {
        markers[[method]][[table]] <- frame
      }
    }
  }
  markers
}

#' Read an h5ad file as a SingleCellExperiment
#'
#' @description
#' Reads the AnnData on-disk format (anndata 0.8 or later) directly from HDF5
#' with \pkg{rhdf5}, without Python or \pkg{zellkonverter}, and returns a
#' \code{SingleCellExperiment}. From there the object can go to
#' \code{\link{convertSCEToCerebro}}, or to any other tool that reads SCE.
#'
#' @details
#' The mapping is:
#' \itemize{
#'   \item \code{X} becomes the assay \code{"X"}; each entry of \code{layers}
#'     becomes an assay of the same name (so \code{layers["counts"]} is the
#'     assay \code{"counts"}).
#'   \item \code{obs} / \code{var} become \code{colData} / \code{rowData}.
#'     Categorical, string, boolean and nullable integer/boolean/string columns
#'     are all decoded; missing values stay \code{NA}.
#'   \item \code{obsm} entries with at least two numeric columns become
#'     \code{reducedDims}; a leading \code{X_} is dropped and \code{pca},
#'     \code{umap} and \code{tsne} are upper-cased (\code{X_umap} is
#'     \code{"UMAP"}).
#'   \item scanpy's \code{uns["rank_genes_groups"]} results become
#'     \code{metadata(sce)$marker_genes}, one table per \code{groupby}
#'     variable; marker tables written by \code{\link{exportToH5ad}} under
#'     \code{uns["cerebro"]["marker_genes"]} are read back as they were.
#' }
#' Sparse \code{X} (CSR or CSC) and dense \code{X} are supported. A CSR matrix
#' is used as it is, because the CSR of cells x genes is the CSC of genes x
#' cells. Other \code{uns}, \code{obsp}, \code{varm} and \code{varp} content is
#' not read.
#'
#' @param file Path to a \code{.h5ad} file.
#' @param use_raw Use \code{raw/X} (and \code{raw/var}) as the expression
#'   matrix, as scanpy workflows often keep the full, unfiltered gene set
#'   there. \code{layers} are then not read, because their genes differ.
#' @param verbose Print what was skipped.
#'
#' @return A \code{SingleCellExperiment} with genes in rows and cells in
#'   columns. Duplicate gene names are made unique with a message; duplicate
#'   cell names are an error.
#'
#' @examples
#' \dontrun{
#' sce <- readH5adAsSCE("pbmc.h5ad")
#' convertSCEToCerebro(sce, "output", groups = "cell_type")
#' }
#'
#' @seealso \code{\link{convertH5adToCerebro}}, \code{\link{convertSCEToCerebro}}
#'
#' @export
readH5adAsSCE <- function(file, use_raw = FALSE, verbose = FALSE) {
  .h5adRequire()
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    stop(
      "The 'SingleCellExperiment' package is needed to read h5ad files.",
      call. = FALSE
    )
  }
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    stop("h5ad file not found: ", paste(file, collapse = ", "), call. = FALSE)
  }
  ctx <- tryCatch(
    .h5adContext(file),
    error = function(e) {
      stop("`", file, "` is not a readable HDF5/h5ad file: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!.h5adHas(ctx, "X") && !.h5adHas(ctx, "layers")) {
    stop("`", file, "` has no `X` matrix or layers; is it an h5ad file?", call. = FALSE)
  }

  obs <- .h5adReadDataFrame(ctx, "obs", verbose)
  cells <- obs$index
  if (anyNA(cells) || any(!nzchar(cells)) || anyDuplicated(cells)) {
    stop("h5ad obs index (cell names) must be unique and non-empty.", call. = FALSE)
  }

  var_path <- "var"
  x_path <- "X"
  if (isTRUE(use_raw)) {
    if (!.h5adHas(ctx, "raw/X")) {
      stop("`use_raw = TRUE` but the file has no `raw/X`.", call. = FALSE)
    }
    var_path <- "raw/var"
    x_path <- "raw/X"
  }
  var <- .h5adReadDataFrame(ctx, var_path, verbose)
  genes <- var$index
  if (anyNA(genes) || any(!nzchar(genes))) {
    stop("h5ad var index (gene names) must be non-empty.", call. = FALSE)
  }
  if (anyDuplicated(genes)) {
    message(
      "[INFO] ",
      sum(duplicated(genes)),
      " duplicated gene name(s) were made unique with make.unique()."
    )
    genes <- make.unique(genes)
  }

  assays <- list()
  with_dimnames <- function(m) {
    dimnames(m) <- list(genes, cells)
    m
  }
  if (.h5adHas(ctx, x_path)) {
    assays[["X"]] <- with_dimnames(.h5adReadMatrix(ctx, x_path, length(cells), length(genes)))
  }
  if (!isTRUE(use_raw) && .h5adHas(ctx, "layers")) {
    for (layer in .h5adChildren(ctx, "layers")) {
      assays[[layer]] <- tryCatch(
        with_dimnames(.h5adReadMatrix(ctx, paste0("layers/", layer), length(cells), length(genes))),
        error = function(e) {
          message("[INFO] Skipping layer `", layer, "`: ", conditionMessage(e))
          NULL
        }
      )
    }
    assays <- assays[!vapply(assays, is.null, logical(1))]
  }
  if (!length(assays)) {
    stop("No readable expression matrix found in `", file, "`.", call. = FALSE)
  }

  row_data <- .h5adAsFrame(list(index = genes, columns = var$columns))
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = assays,
    colData = .h5adAsFrame(obs),
    rowData = row_data,
    reducedDims = .h5adReadObsm(ctx, cells, verbose)
  )
  markers <- c(.h5adReadRankGenes(ctx), .h5adReadCerebroMarkers(ctx))
  if (length(markers)) {
    sce@metadata$marker_genes <- markers
  }
  sce
}
