#' @title
#' Export Seurat object to Cerebro.
#'
#' @description
#' This function allows to export a Seurat object to visualize in Cerebro.
#'
#' @param object Seurat object.
#' @param assay Assay to pull expression values from; defaults to \code{RNA}.
#' @param slot Slot to pull expression values from; defaults to \code{data}. It
#' is recommended to use sparse data (such as log-transformed or raw counts)
#' instead of dense data (such as the \code{scaled} slot) to avoid performance
#' bottlenecks in the Cerebro interface.
#' @param file Where to save the output.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. \code{hg} (human), \code{mm} (mouse), etc.
#' @param groups Names of grouping variables in meta data
#' (\code{object@meta.data}), e.g. \code{c("sample","cluster")}; at least one
#' must be provided; defaults to \code{NULL}.
#' @param main_group The primary grouping variable to use for display in Cerebro;
#' must be one of the grouping variables specified in \code{groups}; defaults to
#' \code{NULL}.
#' @param cell_cycle Names of columns in meta data
#' (\code{object@meta.data}) that contain cell cycle information, e.g.
#' \code{c("Phase")}; defaults to \code{NULL}.
#' @param nUMI Column in \code{object@meta.data} that contains information about
#' number of transcripts per cell; defaults to \code{nUMI}.
#' @param nGene Column in \code{object@meta.data} that contains information
#' about number of expressed genes per cell; defaults to \code{nGene}.
#' @param add_all_meta_data If set to \code{TRUE}, all further meta data columns
#' will be extracted as well.
#' @param use_delayed_array When set to \code{TRUE}, the expression matrix will
#' be stored as an \code{RleMatrix} (see \code{DelayedArray} package). This can
#' be useful for very large data sets, as the matrix won't be loaded into memory
#' and instead values will be read from the disk directly, at the cost of
#' performance. Note that it is necessary to install the \code{DelayedArray}
#' package. If set to \code{FALSE} (default), the expression matrix will be
#' copied from the input object as is. It is recommended to use a sparse format,
#' such as \code{dgCMatrix} from the \code{Matrix} package. Ignored when
#' \code{expression_matrix_mode} is set to an external backend.
#' @param expression_matrix_mode How to persist the expression matrix. One of
#' \code{"embedded"} (default), \code{"bpcells"}, or \code{"h5"}.
#' \itemize{
#'   \item \code{"embedded"} stores the matrix inside the \code{.crb} file, as
#'   before. Compatible with all existing \code{.crb} readers.
#'   \item \code{"bpcells"} writes the matrix to a BPCells on-disk directory
#'   next to the \code{.crb} and keeps only a lightweight handle in the
#'   serialised object. Recommended for large sparse matrices. The resulting
#'   \code{.crb} is portable as long as the sibling \code{.bpcells/} directory
#'   travels with it; the Shiny runtime re-resolves paths via
#'   \code{getExpressionBackend()$location} relative to the \code{.crb}'s
#'   parent directory (step 7.3 runtime attach).
#'   \item \code{"h5"} writes the matrix via \code{HDF5Array::writeTENxMatrix()}
#'   to a TENx-format sparse HDF5 file next to the \code{.crb} (sibling
#'   \code{<stem>.h5}) and tags the backend with that relative location. The
#'   on-disk layout matches \code{inst/extdata/v1.4/example.h5}: a single
#'   \code{/expression} group with \code{data}, \code{indices}, \code{indptr},
#'   \code{shape}, \code{genes}, and \code{barcodes} datasets. The matrix is
#'   stored cells x genes (TENx column-favoured, optimised for per-gene
#'   reads); the Shiny runtime attach reads it back as a lazy
#'   \code{HDF5Array::TENxMatrix} seed and transposes it lazily to Cerebro's
#'   internal genes x cells layout via \code{DelayedArray::t()} (free). The
#'   in-memory \code{dgCMatrix} is never materialised on attach, so RAM stays
#'   close to the \code{.crb} metadata size. Requires the \pkg{HDF5Array}
#'   package.
#' }
#' @param verbose Set this to \code{TRUE} if you want additional log messages;
#' defaults to \code{FALSE}.
#'
#' @section Immune Repertoire:
#' If \code{object@misc$immune_repertoire} contains a named list of
#' data.frames (one per sample, with scRepertoire columns such as CTgene,
#' CTnt, CTaa, CTstrict), it will be automatically exported into the Cerebro
#' object via \code{addImmuneRepertoire()}.  Legacy \code{bcr_data} /
#' \code{tcr_data} slots are also supported as a fallback.
#'
#' @section HLA typing:
#' If \code{object@misc$hla_typing} holds an HLA genotype table -- a canonical
#' long \code{data.frame}, a wide \code{sample} + \code{HLA-*_1/_2}
#' \code{data.frame}, or a named list (sample -> allele vector) -- it is
#' exported via \code{addHLATyping()}, parallel to the immune repertoire. The
#' provenance in \code{object@misc$hla_typing_source_type} (one of
#' \code{"genotyped"}, \code{"imputed"}, \code{"synthetic"}, \code{"unknown"};
#' default \code{"unknown"}) is carried through, so a predicted or fabricated
#' genotype is never mistaken for a directly typed one.
#'
#' @return
#' No data returned.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/v1.4/pbmc_seurat.rds",
#'   package = "CerebroNexus"))
#' exportFromSeurat(
#'   object = pbmc,
#'   file = file.path(tempdir(), 'pbmc_Seurat.crb'),
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   groups = c('sample','seurat_clusters'),
#'   nUMI = 'nCount_RNA',
#'   nGene = 'nFeature_RNA',
#'   use_delayed_array = FALSE,
#'   verbose = TRUE
#' )
#'
#' @import dplyr
#' @importFrom methods as
#' @importFrom rlang .data
#'
#' @export
#'
exportFromSeurat <- function(
  object,
  assay = 'RNA',
  slot = 'data',
  file,
  experiment_name,
  organism,
  groups,
  main_group = NULL,
  cell_cycle = NULL,
  nUMI = 'nUMI',
  nGene = 'nGene',
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "bpcells", "h5"),
  verbose = FALSE
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  expression_matrix_mode <- match.arg(expression_matrix_mode)
  if (
    expression_matrix_mode == "h5" &&
      !requireNamespace("HDF5Array", quietly = TRUE)
  ) {
    stop(
      "expression_matrix_mode = \"h5\" requires the HDF5Array package. ",
      "Install it via BiocManager::install(\"HDF5Array\") and re-run, or ",
      "switch to expression_matrix_mode = \"bpcells\" / \"embedded\".",
      call. = FALSE
    )
  }
  if (
    expression_matrix_mode == "bpcells" &&
      !requireNamespace("BPCells", quietly = TRUE)
  ) {
    stop(
      "expression_matrix_mode = \"bpcells\" requires the BPCells package. ",
      "Install it and re-run, or switch to expression_matrix_mode = \"embedded\".",
      call. = FALSE
    )
  }
  if (expression_matrix_mode != "embedded" && use_delayed_array) {
    if (verbose) {
      message(
        "expression_matrix_mode = \"",
        expression_matrix_mode,
        "\" supersedes use_delayed_array; the RleArray conversion is skipped."
      )
    }
  }

  ## check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop(
      "The 'Seurat' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## Check Seurat package version using compareVersion
  seurat_version <- as.character(utils::packageVersion("Seurat"))
  if (utils::compareVersion(seurat_version, "3.0.0") < 0) {
    stop(
      paste0(
        "The installed Seurat package is of version `",
        seurat_version,
        "`, but at least v3.0 is required."
      ),
      call. = FALSE
    )
  }

  ## check if provided object is of class "Seurat"
  if (!inherits(object, "Seurat")) {
    stop(
      paste0(
        "Provided object is of class `",
        paste(class(object), collapse = ", "),
        "` but must be of class 'Seurat'."
      ),
      call. = FALSE
    )
  }

  ## check version of Seurat object and stop if it is lower than 3
  obj_version <- as.character(object@version)
  if (utils::compareVersion(obj_version, "3.0.0") < 0) {
    stop(
      paste0(
        "Provided Seurat object has version `",
        obj_version,
        "` but must be at least 3.0."
      ),
      call. = FALSE
    )
  }

  ## `groups`
  if (any(groups %in% names(object@meta.data) == FALSE)) {
    stop(
      paste0(
        'Some group columns could not be found in meta data: ',
        paste0(
          groups[which(groups %in% names(object@meta.data) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }

  ## `main_group`
  if (!is.null(main_group) && !(main_group %in% groups)) {
    stop(
      paste0(
        'Specified main_group `',
        main_group,
        '` is not in the list of groups. ',
        'Valid options are: ',
        paste(groups, collapse = ', ')
      ),
      call. = FALSE
    )
  }

  ## `nUMI`
  if ((nUMI %in% names(object@meta.data) == FALSE)) {
    stop(
      paste0(
        'Column with number of transcripts per cell (`',
        nUMI,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }

  ## `nGene`
  if ((nGene %in% names(object@meta.data) == FALSE)) {
    stop(
      paste0(
        'Column with number of expressed genes per cell (`',
        nGene,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }

  ## `cell_cycle`
  if (any(cell_cycle %in% names(object@meta.data) == FALSE)) {
    stop(
      paste0(
        'Some cell cycle columns could not be found in meta data: ',
        paste0(
          cell_cycle[which(cell_cycle %in% names(object@meta.data) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }

  ## check if provided assay exists
  if ((assay %in% names(object@assays) == FALSE)) {
    stop(
      paste0(
        'Specified assay `',
        assay,
        '` could not be found in provided Seurat ',
        'object.'
      ),
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ## prepare the artifact transaction
  ##--------------------------------------------------------------------------##

  ## Everything this run writes goes to a hidden staging directory next to the
  ## target first. Whatever is already on disk is moved aside only once the
  ## ordinary export work has succeeded, and is put back if publishing fails.
  ## Before this, the sibling matrix was deleted and rewritten early, so a
  ## failure further down left the previous .crb sitting next to a matrix from
  ## the run that failed -- a pair that still looked complete and described
  ## two different data sets.
  ##
  ## Building the layout here also forces `file`, which has no default: a
  ## missing or unwritable path used to surface only at the very end, after
  ## the whole export had been computed.
  artifact_layout <- .newExportArtifactLayout(file, expression_matrix_mode)
  artifact_rollback_needed <- FALSE
  on.exit(
    {
      if (artifact_rollback_needed) {
        .restoreExportArtifacts(artifact_layout)
      }
      .cleanupExportArtifactLayout(artifact_layout)
    },
    add = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## initialize Cerebro object
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Initializing Cerebro object...'
      )
    )
  } else {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Start collecting data...'
      )
    )
  }

  ## create new Cerebro object
  export <- Cerebro_v1.3$new()

  ## add experiment name
  export$addExperiment('experiment_name', experiment_name)

  ## add organism
  export$addExperiment('organism', organism)

  ## add cerebroApp version
  export$setVersion(utils::packageVersion('CerebroNexus'))

  ##--------------------------------------------------------------------------##
  ## add transcript counts
  ##--------------------------------------------------------------------------##

  ## get expression data using shared utility function. This is a top-level,
  ## user-facing export entry point whose job is to get the object out to a
  ## .crb, so it opts into legacy cross-semantic layer fallback (e.g. a
  ## Seurat v5 RNA assay with only a `counts` layer, requested at the default
  ## slot = "data"). Without this the export would hard-stop here — before even
  ## reaching the spatial block — on objects that exported fine on master. The
  ## fallback itself warns, so the substitution is never silent.
  ## Split (layered) Seurat v5 objects are joined first, matching
  ## `convertSeuratToCerebro()`. Without it only the first sample's layer is
  ## read: the meta data still describes every cell while the matrix describes
  ## one sample, and the two are never compared again in the `h5` and `bpcells`
  ## modes. `JoinLayers()` materialises the merged matrix, so a very large split
  ## object pays one memory peak here -- the price of exporting all of it.
  expression_data <- .getExpressionMatrix(
    seurat = object,
    assay = assay,
    slot = slot,
    join_samples = TRUE,
    allow_cross_semantic_fallback = TRUE,
    verbose = verbose
  )

  ## Backend-independent guard. `embedded` used to catch a short matrix by
  ## accident, in `setMetaData()`, and reported it as a meta data problem;
  ## `h5` and `bpcells` never populate `self$expression` and so caught nothing
  ## at all, writing a .crb whose meta data and matrix disagree. Check here,
  ## once, before the modes diverge, so every mode fails the same way.
  object_cells <- Seurat::Cells(object)
  if (!setequal(colnames(expression_data), object_cells)) {
    stop(
      "Expression matrix covers ",
      ncol(expression_data),
      " of the object's ",
      length(object_cells),
      " cells.\n",
      "This usually means the object is a split (layered) Seurat v5 object ",
      "whose layers could not be merged.\n",
      "Suggestions:\n",
      "  1. Join the layers before exporting: object <- ",
      "SeuratObject::JoinLayers(object, assay = \"",
      assay,
      "\")\n",
      "  2. Inspect the layers with: Layers(object[[\"",
      assay,
      "\"]])\n",
      "  3. Check that the requested slot (`",
      slot,
      "`) exists for every cell, not just some samples",
      call. = FALSE
    )
  }
  ## Meta data, projections and every other table are built from `Cells(object)`
  ## further down; line the matrix up with them rather than trusting the layer
  ## order, which `JoinLayers()` groups by sample. Guarded because reordering
  ## copies the whole matrix, and an unsplit object already arrives in order.
  if (!identical(colnames(expression_data), object_cells)) {
    expression_data <- expression_data[, object_cells, drop = FALSE]
  }

  if (expression_matrix_mode == "embedded") {
    ## convert expression data to "RleArray" if requested, if it is "dgCMatrix" or
    ## "matrix" format, and if the "DelayedArray" package is available
    if (
      use_delayed_array == TRUE &&
        inherits(expression_data, c('matrix', 'dgCMatrix')) &&
        requireNamespace("DelayedArray", quietly = TRUE)
    ) {
      if (verbose) {
        message(
          paste0(
            '[',
            format(Sys.time(), '%H:%M:%S'),
            '] Storing expression data as ',
            'DelayedArray...'
          )
        )
      }
      requireNamespace("DelayedArray", quietly = TRUE)
      expression_data <- methods::as(expression_data, "RleArray")
    }

    ## add expression data
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Adding expression data (embedded)...'
      )
    )
    export$setExpression(expression_data)
  } else if (expression_matrix_mode == "bpcells") {
    ## Write the expression matrix to a BPCells on-disk directory sitting next
    ## to the target .crb. Keep a BPCells IterableMatrix handle on the object
    ## so that the in-place session (crb + sibling .bpcells dir on the same
    ## machine, same paths) can use it immediately. Step 7.3's runtime attach
    ## will additionally re-resolve the relative location when the crb has
    ## been moved to a different machine or layout.
    ## Write into staging. The previous directory of the same name, if any, is
    ## left untouched until the commit window at the end of the export.
    bpc_dirname <- artifact_layout$sibling_name
    bpc_abs <- artifact_layout$staged_sibling

    ## Sparse dgCMatrix is BPCells' native input; dense matrices have to be
    ## coerced once. Everything else (RleMatrix, DelayedMatrix) is rare enough
    ## here that we cover it defensively.
    if (!inherits(expression_data, "dgCMatrix")) {
      if (inherits(expression_data, "matrix")) {
        expression_data <- methods::as(expression_data, "CsparseMatrix")
      } else if (inherits(expression_data, c("RleMatrix", "DelayedMatrix"))) {
        expression_data <- methods::as(
          as.matrix(expression_data),
          "CsparseMatrix"
        )
      }
    }

    ## BPCells's on-disk format only bit-packs when the matrix's storage
    ## type is integer. dgCMatrix always stores values as double, even when
    ## every nonzero is an integer count (the typical scRNA-seq case), so
    ## we explicitly convert to "uint32_t" when the values are losslessly
    ## representable as non-negative integers. This shrinks the BPCells
    ## sibling ~5x on integer counts (e.g. 50k cells x 20k genes: 440 MB
    ## raw double -> 78 MB bit-packed). Normalised data (slot = "data" or
    ## "scale.data") stays as double — bit-packing would silently truncate.
    nnz_int_ok <- length(expression_data@x) > 0L &&
      all(expression_data@x >= 0) &&
      all(expression_data@x == as.integer(expression_data@x)) &&
      all(expression_data@x <= .Machine$integer.max)
    bpc_iter <- methods::as(expression_data, "IterableMatrix")
    if (nnz_int_ok) {
      bpc_iter <- BPCells::convert_matrix_type(bpc_iter, type = "uint32_t")
      bpc_storage_msg <- "uint32_t (bit-packed)"
    } else {
      bpc_storage_msg <- "double (raw, non-integer values detected)"
    }

    if (verbose) {
      message(sprintf(
        "[%s] Writing expression matrix to BPCells directory: %s [%s]",
        format(Sys.time(), "%H:%M:%S"),
        bpc_abs,
        bpc_storage_msg
      ))
    }
    BPCells::write_matrix_dir(mat = bpc_iter, dir = bpc_abs)
    mat_handle <- BPCells::open_matrix_dir(dir = bpc_abs)

    ## The staged handle is attached now so that the cell-count checks in
    ## setMetaData() and addProjection() still have a matrix to compare
    ## against. It points into the staging directory, which will not exist
    ## after this call returns, so the commit window below reopens the matrix
    ## at its published path before anything is serialised.
    export$setExpression(mat_handle, backend = "external")
    export$setExpressionBackend(type = "bpcells", location = bpc_dirname)
  } else if (expression_matrix_mode == "h5") {
    ## Write the expression matrix to a TENxMatrix-format sparse HDF5 file
    ## sitting next to the target .crb. The on-disk orientation is cells x
    ## genes — TENx CSC stores columns contiguously, so the per-gene reads
    ## that Cerebro does at runtime become single-column lookups. Cerebro's
    ## internal layout is genes x cells, so the runtime attach lazily
    ## transposes the TENxMatrix seed back via DelayedArray::t() (free).
    ## Write into staging; the previous sibling stays where it is until the
    ## commit window at the end of the export.
    h5_filename <- artifact_layout$sibling_name
    h5_abs <- artifact_layout$staged_sibling

    if (!inherits(expression_data, "dgCMatrix")) {
      if (inherits(expression_data, "matrix")) {
        expression_data <- methods::as(expression_data, "CsparseMatrix")
      } else if (inherits(expression_data, c("RleMatrix", "DelayedMatrix"))) {
        expression_data <- methods::as(
          as.matrix(expression_data),
          "CsparseMatrix"
        )
      }
    }

    ## transpose genes x cells -> cells x genes for storage
    m_disk <- methods::as(Matrix::t(expression_data), "CsparseMatrix")

    if (verbose) {
      message(sprintf(
        "[%s] Writing expression matrix to TENx HDF5 file: %s",
        format(Sys.time(), "%H:%M:%S"),
        h5_abs
      ))
    }

    HDF5Array::writeTENxMatrix(m_disk, h5_abs, group = "expression")

    ## self$expression stays NULL — saveRDS therefore does not embed the
    ## matrix inside the .crb. The runtime attach reads the sibling back
    ## as a lazy TENxMatrix seed (no in-memory dgCMatrix materialisation).
    export$setExpressionBackend(type = "h5", location = h5_filename)
  }

  ##--------------------------------------------------------------------------##
  ## collect some more data if present
  ##--------------------------------------------------------------------------##

  ## date of analysis
  if (!is.null(object@misc$experiment$date_of_analysis)) {
    export$addExperiment(
      'date_of_analysis',
      object@misc$experiment$date_of_analysis
    )
  }

  ## date of export
  export$addExperiment('date_of_export', Sys.Date())

  ## `parameters`
  if (!is.null(object@misc$parameters)) {
    for (i in seq_along(object@misc$parameters)) {
      name <- names(object@misc$parameters)[i]
      export$addParameters(
        name,
        object@misc$parameters[[name]]
      )
    }
  }

  ## `technical_info`
  if (!is.null(object@misc$technical_info)) {
    for (i in seq_along(object@misc$technical_info)) {
      export$addTechnicalInfo(
        names(object@misc$technical_info)[i],
        object@misc$technical_info[[i]]
      )
    }
  }

  ## `gene_lists`
  if (!is.null(object@misc$gene_lists)) {
    for (i in seq_along(object@misc$gene_lists)) {
      export$addGeneList(
        names(object@misc$gene_lists)[i],
        object@misc$gene_lists[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## prepare meta data
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Collecting available meta data...'
      )
    )
  }

  ## cell barcodes
  temp_meta_data <- data.frame(
    "cell_barcode" = Seurat::Cells(object),
    stringsAsFactors = FALSE
  )

  ##--------------------------------------------------------------------------##
  ## add grouping variables, factorize if necessary
  ##--------------------------------------------------------------------------##

  ## go through grouping variables
  for (i in groups) {
    ## check content of column in meta data
    ## ... content not factorized
    if (
      !is.factor(object@meta.data[[i]]) &&
        is.character(object@meta.data[[i]])
    ) {
      ## get all values and unique values (sorted, which removes NA)
      values <- object@meta.data[[i]]
      levels <- sort(unique(values), na.last = NA)

      ## check if there are NA values; if so, change NA values to 'N/A' and add
      ## 'N/A' to levels
      if (any(is.na(values))) {
        values[is.na(values)] <- 'N/A'
        levels <- c(levels, 'N/A')
      }

      ## factorize values
      temp_meta_data[[i]] <- factor(values, levels = levels)

      ## ... content is factorized but there are NA values and NA is not among the
      ##     factor levels
    } else if (
      is.factor(object@meta.data[[i]]) &&
        any(is.na(object@meta.data[[i]])) &&
        'NA' %in% levels(object@meta.data[[i]]) == FALSE
    ) {
      ## print log message
      if (verbose) {
        message(
          glue::glue(
            '[{format(Sys.time(), "%H:%M:%S")}] Adding `NA` to factor levels ',
            'of group `{i}`...'
          )
        )
      }

      ## add 'N/A' to factor levels for NA values
      levels <- levels(object@meta.data[[i]])
      values <- as.character(object@meta.data[[i]])
      values[is.na(values)] <- 'N/A'
      values <- factor(values, levels = c(levels, 'N/A'))
      temp_meta_data[[i]] <- values

      ## ... none of the above
    } else {
      ## copy content to meta data
      temp_meta_data[[i]] <- object@meta.data[[i]]
    }
  }

  ## number of transcripts and expressed genes
  temp_meta_data[["nUMI"]] <- object@meta.data[[nUMI]]
  temp_meta_data[["nGene"]] <- object@meta.data[[nGene]]

  ## rest of meta data
  meta_data_columns <- names(object@meta.data)
  meta_data_columns <- meta_data_columns[-which(meta_data_columns %in% groups)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == nUMI)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == nGene)]

  ##--------------------------------------------------------------------------##
  ## cell cycle
  ##--------------------------------------------------------------------------##
  if (
    !is.null(cell_cycle) &&
      length(cell_cycle) > 0
  ) {
    for (i in cell_cycle) {
      if (is.factor(object@meta.data[[i]])) {
        tmp_names <- levels(object@meta.data[[i]])
      } else {
        tmp_names <- unique(object@meta.data[[i]])
      }
      # colData(export$expression)[[i]] <- factor(object@meta.data[[i]], levels = tmp_names)
      temp_meta_data[[i]] <- factor(object@meta.data[[i]], levels = tmp_names)
    }
    meta_data_columns <- meta_data_columns[
      -which(meta_data_columns %in% cell_cycle)
    ]
  }

  ##--------------------------------------------------------------------------##
  ## add all other meta data if specified
  ##--------------------------------------------------------------------------##
  if (add_all_meta_data == TRUE) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting all meta data columns...'
        )
      )
    }
    for (i in meta_data_columns) {
      # colData(export$expression)[[i]] <- object@meta.data[[i]]
      temp_meta_data[[i]] <- object@meta.data[[i]]
    }
  }

  ## make column names in meta data unique (if necessary)
  # colnames(colData(export$expression)) <- make.unique(colnames(colData(export$expression)))
  colnames(temp_meta_data) <- make.unique(colnames(temp_meta_data))

  ##--------------------------------------------------------------------------##
  ## add meta data
  ##--------------------------------------------------------------------------##
  export$setMetaData(temp_meta_data)

  ##--------------------------------------------------------------------------##
  ## add grouping variables and cell cycle columns
  ##--------------------------------------------------------------------------##
  for (i in groups) {
    export$addGroup(i, levels(temp_meta_data[[i]]))
  }

  ## set main group if specified
  if (!is.null(main_group)) {
    export$addParameters('main_group', main_group)
  }

  if (
    !is.null(cell_cycle) &&
      length(cell_cycle) > 0
  ) {
    export$setCellCycle(cell_cycle)
  }

  ##--------------------------------------------------------------------------##
  ## projections
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Extracting dimensional reductions...'
      )
    )
  }
  projections <- list()
  projections_available <- names(object@reductions)
  projections_available_pca <- projections_available[grep(
    projections_available,
    pattern = 'pca',
    ignore.case = TRUE,
    invert = FALSE
  )]
  projections_available_non_pca <- projections_available[grep(
    projections_available,
    pattern = 'pca',
    ignore.case = TRUE,
    invert = TRUE
  )]
  if (length(projections_available) == 0) {
    stop('No dimensional reductions available.', call. = FALSE)
  } else if (
    length(projections_available) == 1 &&
      length(projections_available_pca) == 1
  ) {
    # SingleCellExperiment::reducedDims(export$expression)[[projections_available]] <- as.data.frame(
    #   object@reductions[[projections_available]]@cell.embeddings
    # )
    export$addProjection(
      projections_available,
      as.data.frame(object@reductions[[projections_available]]@cell.embeddings)
    )
    warning(
      paste0(
        'Warning: Only PCA as dimensional reduction found, will export ',
        'first and second principal components. Consider using tSNE and/or ',
        'UMAP instead.'
      )
    )
  } else if (length(projections_available_non_pca) >= 1) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] ',
          'Will export the following dimensional reductions: ',
          paste(projections_available_non_pca, collapse = ', ')
        )
      )
    }
    for (projection in projections_available_non_pca) {
      # SingleCellExperiment::reducedDims(export$expression)[[projection]] <- as.data.frame(
      #   object@reductions[[projection]]@cell.embeddings
      # )
      export$addProjection(
        projection,
        as.data.frame(object@reductions[[projection]]@cell.embeddings)
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## group trees
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$trees)) {
    ## check if it's a list
    if (!is.list(object@misc$trees)) {
      stop(
        '`object@misc$trees` is not a list.',
        call. = FALSE
      )
    }
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting trees...'
        )
      )
    }
    for (i in seq_along(object@misc$trees)) {
      export$addTree(
        names(object@misc$trees)[i],
        object@misc$trees[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$most_expressed_genes)) {
    ## check if it's a list
    if (!is.list(object@misc$most_expressed_genes)) {
      stop(
        '`object@misc$most_expressed_genes` is not a list.',
        call. = FALSE
      )
    }
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of most expressed genes...'
        )
      )
    }

    for (i in seq_along(object@misc$most_expressed_genes)) {
      group <- names(object@misc$most_expressed_genes)[i]
      if (group %in% groups) {
        export$addMostExpressedGenes(
          group,
          object@misc$most_expressed_genes[[i]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## mean expression
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$mean_expression)) {
    ## check if it's a list
    if (!is.list(object@misc$mean_expression)) {
      stop(
        '`object@misc$mean_expression` is not a list.',
        call. = FALSE
      )
    }
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of mean expression...'
        )
      )
    }

    for (i in seq_along(object@misc$mean_expression)) {
      group <- names(object@misc$mean_expression)[i]
      if (group %in% groups) {
        export$addMeanExpression(
          group,
          object@misc$mean_expression[[i]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## Immune repertoire data (unified)
  ##--------------------------------------------------------------------------##
  ## `is.list()` was the whole guard, and a data.frame passes it -- its columns
  ## became the sample names and the wrong shape reached the .crb intact.
  has_unified_repertoire <-
    !is.null(object@misc$immune_repertoire) &&
    length(object@misc$immune_repertoire) > 0
  if (has_unified_repertoire) {
    .validateImmuneRepertoire(
      object@misc$immune_repertoire,
      cell_barcodes = Seurat::Cells(object),
      source_label = "`@misc$immune_repertoire`"
    )
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting immune repertoire data (',
          length(object@misc$immune_repertoire),
          ' samples)...'
        )
      )
    }
    export$addImmuneRepertoire(object@misc$immune_repertoire)
  }

  ##--------------------------------------------------------------------------##
  ## HLA typing (optional; parallel to immune_repertoire)
  ##--------------------------------------------------------------------------##
  ## Accepts a canonical long data.frame, a wide sample x locus table, or a
  ## named list (sample -> allele vector). Provenance is read from an optional
  ## `object@misc$hla_typing_source_type` (default "unknown") so an uploaded or
  ## imputed genotype is never silently treated as directly typed.
  if (
    !is.null(object@misc$hla_typing) &&
      (is.data.frame(object@misc$hla_typing) ||
        (is.list(object@misc$hla_typing) &&
          length(object@misc$hla_typing) > 0))
  ) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting HLA typing...'
        )
      )
    }
    st <- object@misc$hla_typing_source_type
    if (is.null(st) || !nzchar(st)) {
      st <- 'unknown'
    }
    export$addHLATyping(object@misc$hla_typing, source_type = st)
  }

  ##--------------------------------------------------------------------------##
  ## BCR data (legacy)
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$bcr_data)) {
    ## Same check as the unified slot: `getImmuneRepertoire()` falls back to
    ## merging the legacy slots, so leaving them unchecked would just move the
    ## way around it rather than close it.
    .validateImmuneRepertoire(
      object@misc$bcr_data,
      cell_barcodes = Seurat::Cells(object),
      source_label = "`@misc$bcr_data`"
    )
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of BCR data...'
        )
      )
    }
    export$addBCRData(object@misc$bcr_data)
  }

  ##--------------------------------------------------------------------------##
  ## TCR data (legacy)
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$tcr_data)) {
    .validateImmuneRepertoire(
      object@misc$tcr_data,
      cell_barcodes = Seurat::Cells(object),
      source_label = "`@misc$tcr_data`"
    )
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of TCR data...'
        )
      )
    }
    export$addTCRData(object@misc$tcr_data)
  }

  ## R6 methods are serialized into the .crb, so changing the class getter does
  ## not repair files that were already written. For new exports that only use
  ## the two legacy slots, populate the unified field that the serialized getter
  ## already prefers. Keep the legacy fields above for getBCR()/getTCR().
  if (!has_unified_repertoire) {
    legacy_repertoire <- .mergeRepertoiresBySample(
      object@misc$tcr_data,
      object@misc$bcr_data
    )
    if (!is.null(legacy_repertoire) && length(legacy_repertoire) > 0) {
      export$addImmuneRepertoire(legacy_repertoire)
    }
  }

  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$marker_genes)) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting marker genes table...'
        )
      )
    }
    ## marker_genes is a nested list: list(method = list(group = data.frame))
    ## (existing shiny consumers depend on the nested layout; the
    ## flat-data.frame simplification is deferred until H6 lands).
    if (!is.list(object@misc$marker_genes)) {
      stop('`object@misc$marker_genes` is not a list.', call. = FALSE)
    }
    for (i in seq_along(object@misc$marker_genes)) {
      method <- names(object@misc$marker_genes)[i]
      for (j in seq_along(object@misc$marker_genes[[method]])) {
        if (is.list(object@misc$marker_genes[[method]][j])) {
          group <- names(object@misc$marker_genes[[method]])[j]
          export$addMarkerGenes(
            method,
            group,
            object@misc$marker_genes[[method]][[group]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$enriched_pathways)) {
    ## check if it's a list
    if (!is.list(object@misc$enriched_pathways)) {
      stop(
        '`object@misc$enriched_pathways` is not a list.',
        call. = FALSE
      )
    }
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting pathway enrichment results...'
        )
      )
    }
    ## for each method
    for (i in seq_along(object@misc$enriched_pathways)) {
      method <- names(object@misc$enriched_pathways)[i]
      ## for each group
      for (j in seq_along(object@misc$enriched_pathways[[method]])) {
        if (is.list(object@misc$enriched_pathways[[method]][j])) {
          group <- names(object@misc$enriched_pathways[[method]])[j]

          ## only add enriched pathways if group is present in `groups`
          if (group %in% groups) {
            export$addEnrichedPathways(
              method,
              group,
              object@misc$enriched_pathways[[method]][[group]]
            )
          }
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if (length(object@misc$trajectories) == 0) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] No trajectories to extract...'
        )
      )
    }
  } else {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] ',
          # 'Extracting trajectories...'
          'Will export the following trajectories: ',
          paste(names(object@misc$trajectories$monocle2), collapse = ', ')
        )
      )
    }
    ## for each method
    for (i in seq_along(object@misc$trajectories)) {
      method <- names(object@misc$trajectories)[i]
      if (method == 'monocle2') {
        ## for each trajectory
        for (j in seq_along(object@misc$trajectories[[i]])) {
          export$addTrajectory(
            method,
            names(object@misc$trajectories[[i]])[j],
            object@misc$trajectories[[i]][[j]]
          )
        }
      } else {
        warning(
          paste0(
            'Warning: Skipping trajectories of method `',
            method,
            '`. At the ',
            'moment, only trajectories generated with Monocle 2 (`monocle2`) ',
            'are supported.'
          )
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## extra material
  ##
  ## currently, only tables can be exported
  ##--------------------------------------------------------------------------##

  ## define valid categories
  valid_categories <- c('tables')

  ## check of extra material exists, that it is in list format, and that the
  ## list is not empty
  if (
    !is.null(object@misc$extra_material) &&
      is.list(object@misc$extra_material) &&
      length(object@misc$extra_material) > 0
  ) {
    if (verbose) {
      message(
        glue::glue(
          '[{format(Sys.time(), "%H:%M:%S")}] Found extra material to export...'
        )
      )
    }

    ## go through categories in `extra_material` slot
    for (category in names(object@misc$extra_material)) {
      ## do this if category is `tables`
      if (category == 'tables') {
        ## go through tables
        for (i in seq_along(object@misc$extra_material$tables)) {
          ## export table
          export$addExtraMaterial(
            category = 'tables',
            name = names(object@misc$extra_material$tables)[i],
            content = object@misc$extra_material$tables[[i]]
          )
        }

        ## do this if category is `plots`
      } else if (category == 'plots') {
        ## go through tables
        for (i in seq_along(object@misc$extra_material$plots)) {
          ## export table
          export$addExtraMaterial(
            category = 'plots',
            name = names(object@misc$extra_material$plots)[i],
            content = object@misc$extra_material$plots[[i]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## spatial data
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Checking for spatial data...'
      )
    )
  }

  ## Spatial images live in the `@images` slot on Seurat v3+ objects, so this
  ## path handles both Seurat v4 (VisiumV1, SlideSeq) and v5 (VisiumV2, FOV)
  ## objects. `.getSpatialData()` resolves coordinates version-agnostically via
  ## GetTissueCoordinates + S4 slot access, so no version branch is needed here.
  has_images <- .spx_has_slot(object, "images") &&
    !is.null(object@images) &&
    length(object@images) > 0

  if (has_images) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] ',
          'Spatial data found. Extracting spatial coordinates...'
        )
      )
    }

    for (image_name in names(object@images)) {
      tryCatch(
        {
          # Extract spatial data (coordinates + expression)
          # Using .getSpatialData helper which handles Visium, FOV/Xenium, etc.
          spatial_data <- .getSpatialData(
            object,
            image = image_name,
            layer = slot,
            assay = assay
          )

          # Also add coordinates as a projection for compatibility with existing visualization functions
          coords_df <- spatial_data$coordinates

          # Identify coordinate columns to use for projection (2D)
          proj_cols <- character(0)

          # Standard Visium
          if (all(c("imagerow", "imagecol") %in% colnames(coords_df))) {
            proj_cols <- c("imagerow", "imagecol")
          } else if (all(c("x", "y") %in% colnames(coords_df))) {
            # Standard FOV/Xenium/Other
            proj_cols <- c("x", "y")
          } else if (ncol(coords_df) >= 2) {
            # Fallback: use first two columns
            proj_cols <- colnames(coords_df)[1:2]
          }

          if (length(proj_cols) == 2) {
            coords_df <- coords_df[, proj_cols, drop = FALSE]
            if (verbose) {
              message(paste0(
                '[',
                format(Sys.time(), '%H:%M:%S'),
                '] ',
                'Added spatial projection: ',
                image_name
              ))
            }
          }
          spatial_data$coordinates <- coords_df

          # Add to Cerebro object
          export$addSpatialData(image_name, spatial_data)

          if (verbose) {
            message(
              paste0(
                '[',
                format(Sys.time(), '%H:%M:%S'),
                '] ',
                'Added spatial data: ',
                image_name,
                ' (',
                nrow(spatial_data$coordinates),
                ' cells)'
              )
            )
          }
        },
        error = function(e) {
          ## Never drop a spatial image silently: an object that clearly has
          ## `@images` but whose extraction fails (e.g. requested layer=slot is
          ## absent) would otherwise export "successfully" with no Spatial tab
          ## and no clue why. Always warn, regardless of `verbose`.
          warning(
            'Could not extract spatial data for image `',
            image_name,
            '` (layer = "',
            slot,
            '"): ',
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## show overview of Cerebro object
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[',
      format(Sys.time(), '%H:%M:%S'),
      '] ',
      'Overview of Cerebro object:\n'
    )
  )

  ## print object
  export$print()

  ##--------------------------------------------------------------------------##
  ## save Cerebro object to disk
  ##--------------------------------------------------------------------------##

  ## The output directory was created when the artifact layout was built, so
  ## by this point it exists even for a multi-level path.

  ## log message
  message(
    paste0(
      '[',
      format(Sys.time(), '%H:%M:%S'),
      '] Saving Cerebro object to: ',
      file
    )
  )

  ##--------------------------------------------------------------------------##
  ## commit window
  ##
  ## Everything above this point can fail without touching what is already on
  ## disk. From here the previous artifacts are moved aside and the staged
  ## ones take their place; any error or interrupt in between puts the
  ## previous set back. See R/utils-export-artifacts.R for the exact
  ## guarantee, which stops at process death.
  ##--------------------------------------------------------------------------##

  if (expression_matrix_mode == "bpcells") {
    ## BPCells stores an absolute path inside the handle, so the object may
    ## only be serialised once the matrix sits at its published location.
    ## Publishing therefore has to happen before saveRDS(), not after.
    .backupExportArtifacts(artifact_layout)
    artifact_rollback_needed <- TRUE
    .publishStagedArtifact(
      artifact_layout$staged_sibling,
      artifact_layout$target_sibling
    )
    export$setExpression(
      BPCells::open_matrix_dir(dir = artifact_layout$target_sibling),
      backend = "external"
    )
    export$setExpressionBackend(
      type = "bpcells",
      location = artifact_layout$sibling_name
    )
    saveRDS(export, artifact_layout$staged_crb)
    .publishStagedArtifact(
      artifact_layout$staged_crb,
      artifact_layout$target_crb
    )
  } else {
    ## `embedded` carries the matrix inside the object and `h5` carries only a
    ## relative tag, so both can be serialised before anything on disk moves.
    ## That keeps the window in which the two entries disagree down to two
    ## renames.
    saveRDS(export, artifact_layout$staged_crb)
    .backupExportArtifacts(artifact_layout)
    artifact_rollback_needed <- TRUE
    if (!is.null(artifact_layout$staged_sibling)) {
      .publishStagedArtifact(
        artifact_layout$staged_sibling,
        artifact_layout$target_sibling
      )
    }
    .publishStagedArtifact(
      artifact_layout$staged_crb,
      artifact_layout$target_crb
    )
  }

  artifact_rollback_needed <- FALSE
  .cleanupExportArtifactLayout(artifact_layout)

  ## log message
  ## ... writing to file was successful
  if (file.exists(file)) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Done!'
      )
    )
    ## ... target file doesn't exist
  } else {
    stop(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Something went wrong while ',
        'saving the file.'
      ),
      .call = FALSE
    )
  }
}
