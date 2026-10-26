## Format-independent export core -------------------------------------------
##
## Every exportFromXxx() adapter reduces its native object to plain R data (a
## cell meta data.frame, a `misc` list, cell barcodes, a genes x cells matrix,
## named embeddings) and hands it to these helpers. The helpers know nothing
## about Seurat, SingleCellExperiment or AnnData, so a new input format only
## needs a thin adapter and inherits the external expression backends, the
## transactional publish and the Viewer Pack from here.

#' @keywords internal
#' @noRd
.exportCheckOutputArgs <- function(
  file,
  expression_matrix_mode,
  use_delayed_array,
  verbose
) {
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(file)
  ) {
    stop("`file` must be one non-empty output path.", call. = FALSE)
  }
  if (dir.exists(file)) {
    stop("`file` points to a directory, not a .crb path: ", file, call. = FALSE)
  }
  if (!identical(expression_matrix_mode, "embedded")) {
    if (!identical(tolower(tools::file_ext(file)), "crb")) {
      stop(
        "External expression backends require `file` to end in .crb so the ",
        "Cerebro object and its sidecar have distinct portable names. Received: ",
        file,
        call. = FALSE
      )
    }
    .validatePortableExportBasename(file)
  }
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
}

#' @keywords internal
#' @noRd
.exportCheckGroups <- function(
  meta_names,
  groups,
  main_group
) {
  ## `groups`
  if (any(groups %in% meta_names == FALSE)) {
    stop(
      paste0(
        'Some group columns could not be found in meta data: ',
        paste0(
          groups[which(groups %in% meta_names == FALSE)],
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
}

#' @keywords internal
#' @noRd
.exportCheckMetaColumns <- function(
  meta_names,
  nUMI,
  nGene,
  cell_cycle
) {
  ## `nUMI`
  if ((nUMI %in% meta_names == FALSE)) {
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
  if ((nGene %in% meta_names == FALSE)) {
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
  if (any(cell_cycle %in% meta_names == FALSE)) {
    stop(
      paste0(
        'Some cell cycle columns could not be found in meta data: ',
        paste0(
          cell_cycle[which(cell_cycle %in% meta_names == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }
}

#' Create the output directory and the private stage; returns the stage dir.
#' The caller owns cleanup (`on.exit(unlink(stage))`).
#' @keywords internal
#' @noRd
.exportPrepareOutput <- function(
  final_file
) {
  final_dir <- dirname(final_file)
  if (!dir.exists(final_dir)) {
    dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(final_dir)) {
    stop("Failed to create output directory: ", final_dir, call. = FALSE)
  }
  .createPrivateExportStage(final_file)
}

#' @keywords internal
#' @noRd
.exportInit <- function(
  experiment_name,
  organism,
  misc,
  verbose
) {
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
  export <- Cerebro$new()

  ## add experiment name
  export$addExperiment('experiment_name', experiment_name)
  # Preserve optional overview prose exactly when supplied, never manufacture it.
  for (field in c("description", "source", "sample_column")) {
    value <- misc$experiment[[field]]
    if (
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(value))
    ) {
      export$addExperiment(field, value)
    }
  }

  ## add organism
  export$addExperiment('organism', organism)

  ## add cerebroApp version
  export$setVersion(package_version(.cerebroRuntimeVersion()))

  export
}

#' Persist the genes x cells matrix (embedded / bpcells / h5). Returns the
#' matrix as it stands afterwards, because the spatial block reuses it.
#' @keywords internal
#' @noRd
.exportWriteExpression <- function(
  export,
  expression_data,
  expression_matrix_mode,
  use_delayed_array,
  export_stage_dir,
  final_file,
  verbose
) {
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
    crb_dir <- export_stage_dir
    bpc_dirname <- .exportSidecarName(final_file, expression_matrix_mode)
    bpc_abs <- file.path(crb_dir, bpc_dirname)

    ## BPCells writes an error if the directory already exists; clean first
    ## so the exporter is idempotent.
    if (dir.exists(bpc_abs)) {
      unlink(bpc_abs, recursive = TRUE)
    }

    ## Delayed inputs go through a block-written disk bridge. Never realize
    ## the complete delayed matrix just to convert it back to sparse storage.
    delayed_bridge <- NULL
    if (!inherits(expression_data, "dgCMatrix")) {
      if (inherits(expression_data, "matrix")) {
        expression_data <- methods::as(expression_data, "CsparseMatrix")
      } else if (inherits(expression_data, c("RleMatrix", "DelayedMatrix"))) {
        if (!requireNamespace("HDF5Array", quietly = TRUE)) {
          stop(
            "Exporting delayed expression to BPCells requires HDF5Array.",
            call. = FALSE
          )
        }
        integer_counts <- all(unlist(
          DelayedArray::blockApply(expression_data, function(block) {
            all(is.finite(block)) &&
              all(block >= 0) &&
              all(block <= .Machine$integer.max) &&
              all(block == floor(block))
          }),
          use.names = FALSE
        ))
        delayed_bridge <- tempfile(
          "delayed-expression-",
          tmpdir = crb_dir,
          fileext = ".h5"
        )
        HDF5Array::writeTENxMatrix(
          DelayedArray::t(expression_data),
          delayed_bridge,
          group = "matrix"
        )
        expression_data <- t(BPCells::open_matrix_10x_hdf5(delayed_bridge))
        if (integer_counts) {
          expression_data <- BPCells::convert_matrix_type(
            expression_data,
            type = "uint32_t"
          )
        }
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
    if (inherits(expression_data, "IterableMatrix")) {
      bpc_iter <- expression_data
      bpc_storage_msg <- paste0(BPCells::matrix_type(bpc_iter), " (source)")
    } else {
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
    }

    if (verbose) {
      message(sprintf(
        "[%s] Writing expression matrix to BPCells directory: %s [%s]",
        format(Sys.time(), "%H:%M:%S"),
        bpc_abs,
        bpc_storage_msg
      ))
    }
    ## Keep Cerebro's genes x cells dimensions, but physically store rows
    ## contiguously so per-gene and small gene-set queries avoid a full scan.
    mat_handle <- .writeBpcellsGeneMajor(bpc_iter, bpc_abs)
    if (!is.null(delayed_bridge)) {
      unlink(delayed_bridge)
    }

    ## Carry the live handle (absolute path inside @dir -- BPCells normalises
    ## it on open_matrix_dir()) AND the portable relative location tag. Step
    ## 7.3's attach reads the tag, not @dir, so the crb stays portable.
    export$setExpression(mat_handle, backend = "external")
    ## The publisher must be able to drop the final R reference before moving
    ## the directory on Windows; do not retain this local alias until return.
    mat_handle <- NULL
    export$setExpressionBackend(type = "bpcells", location = bpc_dirname)
  } else if (expression_matrix_mode == "h5") {
    ## Write the expression matrix to a TENxMatrix-format sparse HDF5 file
    ## sitting next to the target .crb. The on-disk orientation is cells x
    ## genes — TENx CSC stores columns contiguously, so the per-gene reads
    ## that Cerebro does at runtime become single-column lookups. Cerebro's
    ## internal layout is genes x cells, so the runtime attach lazily
    ## transposes the TENxMatrix seed back via DelayedArray::t() (free).
    crb_dir <- export_stage_dir
    h5_filename <- .exportSidecarName(final_file, expression_matrix_mode)
    h5_abs <- file.path(crb_dir, h5_filename)

    if (file.exists(h5_abs)) {
      file.remove(h5_abs)
    }

    ## a sidecar written straight from an h5ad only moves here
    temp_sidecar <- .tempSidecarOf(expression_data)
    if (!is.null(temp_sidecar)) {
      if (verbose) {
        message(sprintf(
          "[%s] Moving expression matrix written from the h5ad to: %s",
          format(Sys.time(), "%H:%M:%S"),
          h5_abs
        ))
      }
      if (!file.rename(temp_sidecar, h5_abs)) {
        if (!file.copy(temp_sidecar, h5_abs)) {
          stop(
            "Could not move the expression sidecar to: ",
            h5_abs,
            call. = FALSE
          )
        }
        unlink(temp_sidecar)
      }
      .releaseTempSidecar(temp_sidecar)
      expression_data <- DelayedArray::t(HDF5Array::TENxMatrix(
        h5_abs,
        group = "expression"
      ))
      export$setExpressionBackend(type = "h5", location = h5_filename)
      return(expression_data)
    }

    if (!inherits(expression_data, "dgCMatrix")) {
      if (inherits(expression_data, "matrix")) {
        expression_data <- methods::as(expression_data, "CsparseMatrix")
      }
    }

    if (verbose) {
      message(sprintf(
        "[%s] Writing expression matrix to TENx HDF5 file: %s",
        format(Sys.time(), "%H:%M:%S"),
        h5_abs
      ))
    }

    ## A dgCMatrix is transposed and written by compiled code; a matrix held
    ## cells x genes already is written as it is; anything else is transposed
    ## genes x cells -> cells x genes here first.
    if (!.writeSidecarFromGenesByCells(expression_data, h5_abs)) {
      m_disk <- .cellsByGenesSeed(expression_data)
      if (is.null(m_disk)) {
        m_disk <- if (
          inherits(expression_data, c("RleMatrix", "DelayedMatrix"))
        ) {
          DelayedArray::t(expression_data)
        } else {
          methods::as(Matrix::t(expression_data), "CsparseMatrix")
        }
      }
      .writeTENxH5(m_disk, h5_abs, group = "expression")
    }

    ## self$expression stays NULL — saveRDS therefore does not embed the
    ## matrix inside the .crb. The runtime attach reads the sibling back
    ## as a lazy TENxMatrix seed (no in-memory dgCMatrix materialisation).
    export$setExpressionBackend(type = "h5", location = h5_filename)
  }

  expression_data
}

#' @keywords internal
#' @noRd
.exportAddExperimentMisc <- function(
  export,
  misc
) {
  ## date of analysis
  if (!is.null(misc$experiment$date_of_analysis)) {
    export$addExperiment(
      'date_of_analysis',
      misc$experiment$date_of_analysis
    )
  }

  ## date of export
  export$addExperiment('date_of_export', Sys.Date())

  ## `parameters`
  if (!is.null(misc$parameters)) {
    for (i in seq_along(misc$parameters)) {
      name <- names(misc$parameters)[i]
      export$addParameters(
        name,
        misc$parameters[[name]]
      )
    }
  }

  ## `technical_info`
  if (!is.null(misc$technical_info)) {
    for (i in seq_along(misc$technical_info)) {
      export$addTechnicalInfo(
        names(misc$technical_info)[i],
        misc$technical_info[[i]]
      )
    }
  }

  ## `gene_lists`
  if (!is.null(misc$gene_lists)) {
    for (i in seq_along(misc$gene_lists)) {
      export$addGeneList(
        names(misc$gene_lists)[i],
        misc$gene_lists[[i]]
      )
    }
  }
}

#' Trajectory methods `.exportAddMiscResults()` passes on: Monocle 2, and what
#' `readH5adAsSCE()` makes from scanpy/scVelo results.
#' @keywords internal
#' @noRd
.exportTrajectoryMethods <- c('monocle2', 'paga', 'scvelo', 'pseudotime')

#' @keywords internal
#' @noRd
.exportAddMetaData <- function(
  export,
  meta,
  cells,
  groups,
  nUMI,
  nGene,
  cell_cycle,
  add_all_meta_data,
  main_group,
  verbose
) {
  ## cell barcodes
  temp_meta_data <- data.frame(
    "cell_barcode" = cells,
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
      !is.factor(meta[[i]]) &&
        is.character(meta[[i]])
    ) {
      ## get all values and unique values (sorted, which removes NA)
      values <- meta[[i]]
      levels <- sort(unique(values), na.last = NA)

      ## check if there are NA values; if so, change NA values to 'N/A' and add
      ## 'N/A' to levels
      if (any(is.na(values))) {
        values[is.na(values)] <- 'N/A'
        levels <- unique(c(levels, 'N/A'))
      }

      ## factorize values
      temp_meta_data[[i]] <- factor(values, levels = levels)

      ## ... content is factorized but there are NA values and NA is not among the
      ##     factor levels
    } else if (
      is.factor(meta[[i]]) &&
        any(is.na(meta[[i]]))
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
      levels <- levels(meta[[i]])
      values <- as.character(meta[[i]])
      values[is.na(values)] <- 'N/A'
      values <- factor(values, levels = unique(c(levels, 'N/A')))
      temp_meta_data[[i]] <- values

      ## ... none of the above
    } else {
      ## copy content to meta data
      temp_meta_data[[i]] <- meta[[i]]
    }
  }

  ## number of transcripts and expressed genes
  temp_meta_data[["nUMI"]] <- meta[[nUMI]]
  temp_meta_data[["nGene"]] <- meta[[nGene]]

  ## rest of meta data
  meta_data_columns <- names(meta)
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
      if (is.factor(meta[[i]])) {
        tmp_names <- levels(meta[[i]])
      } else {
        tmp_names <- unique(meta[[i]])
      }
      # colData(export$expression)[[i]] <- factor(meta[[i]], levels = tmp_names)
      temp_meta_data[[i]] <- factor(meta[[i]], levels = tmp_names)
    }
    meta_data_columns <- setdiff(meta_data_columns, cell_cycle)
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
      # colData(export$expression)[[i]] <- meta[[i]]
      temp_meta_data[[i]] <- meta[[i]]
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

  invisible(export)
}

#' @param projection_names Candidate reductions (all of them, or the explicit
#'   subset when `explicit_projections`).
#' @param get_embedding `function(name)` returning a cells x dims data.frame.
#' @keywords internal
#' @noRd
.exportAddProjections <- function(
  export,
  projection_names,
  get_embedding,
  explicit_projections,
  verbose
) {
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Extracting dimensional reductions...'
      )
    )
  }
  projections_available <- projection_names
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
    stop(
      "No dimensional reductions available. Add at least one first, e.g. ",
      "scater::runPCA() / scater::runUMAP() on a SingleCellExperiment, ",
      "scanpy.tl.umap() on an AnnData, or RunUMAP() on a Seurat object.",
      call. = FALSE
    )
  } else if (explicit_projections) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Will export the following dimensional reductions: ',
          paste(projections_available, collapse = ', ')
        )
      )
    }
    for (projection in projections_available) {
      export$addProjection(
        projection,
        get_embedding(projection)
      )
    }
  } else if (
    length(projections_available) == 1 &&
      length(projections_available_pca) == 1
  ) {
    # SingleCellExperiment::reducedDims(export$expression)[[projections_available]] <- as.data.frame(
    #   object@reductions[[projections_available]]@cell.embeddings
    # )
    export$addProjection(
      projections_available,
      get_embedding(projections_available)
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
        get_embedding(projection)
      )
    }
  }

  invisible(export)
}

#' Trees, most expressed genes, mean expression, immune repertoire, HLA,
#' markers, pathways, trajectories, extra material and Trekker, all read from
#' a plain `misc` list.
#' @keywords internal
#' @noRd
.exportAddMiscResults <- function(
  export,
  misc,
  groups,
  cells,
  verbose
) {
  ##--------------------------------------------------------------------------##
  ## group trees
  ##--------------------------------------------------------------------------##
  if (!is.null(misc$trees)) {
    ## check if it's a list
    if (!is.list(misc$trees)) {
      stop(
        '`misc$trees` is not a list.',
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
    for (i in seq_along(misc$trees)) {
      export$addTree(
        names(misc$trees)[i],
        misc$trees[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if (!is.null(misc$most_expressed_genes)) {
    ## check if it's a list
    if (!is.list(misc$most_expressed_genes)) {
      stop(
        '`misc$most_expressed_genes` is not a list.',
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

    for (i in seq_along(misc$most_expressed_genes)) {
      group <- names(misc$most_expressed_genes)[i]
      if (group %in% groups) {
        export$addMostExpressedGenes(
          group,
          misc$most_expressed_genes[[i]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## mean expression
  ##--------------------------------------------------------------------------##
  if (!is.null(misc$mean_expression)) {
    ## check if it's a list
    if (!is.list(misc$mean_expression)) {
      stop(
        '`misc$mean_expression` is not a list.',
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

    for (i in seq_along(misc$mean_expression)) {
      group <- names(misc$mean_expression)[i]
      if (group %in% groups) {
        export$addMeanExpression(
          group,
          misc$mean_expression[[i]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## Immune repertoire data (unified)
  ##--------------------------------------------------------------------------##
  ## `is.list()` was the whole guard, and a data.frame passes it -- its columns
  ## became the sample names and the wrong shape reached the .crb intact.
  unified_repertoire <- .dropEmptyRepertoireSamples(
    misc$immune_repertoire
  )
  has_unified_repertoire <-
    !is.null(unified_repertoire) &&
    length(unified_repertoire) > 0
  if (has_unified_repertoire) {
    unified_repertoire <- .normalizeImmuneRepertoire(
      unified_repertoire,
      cell_barcodes = cells,
      source_label = "`@misc$immune_repertoire`"
    )
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting immune repertoire data (',
          length(unified_repertoire),
          ' samples)...'
        )
      )
    }
    export$addImmuneRepertoire(unified_repertoire)
  }

  ##--------------------------------------------------------------------------##
  ## HLA typing (optional; parallel to immune_repertoire)
  ##--------------------------------------------------------------------------##
  ## Accepts a canonical long data.frame, a wide sample x locus table, or a
  ## named list (sample -> allele vector). Provenance is read from an optional
  ## `misc$hla_typing_source_type` (default "unknown") so an uploaded or
  ## imputed genotype is never silently treated as directly typed.
  if (
    !is.null(misc$hla_typing) &&
      (is.data.frame(misc$hla_typing) ||
        (is.list(misc$hla_typing) &&
          length(misc$hla_typing) > 0))
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
    st <- misc$hla_typing_source_type
    if (is.null(st) || !nzchar(st)) {
      st <- 'unknown'
    }
    export$addHLATyping(misc$hla_typing, source_type = st)
  }

  ##--------------------------------------------------------------------------##
  ## BCR data (legacy)
  ##--------------------------------------------------------------------------##
  if (!has_unified_repertoire && !is.null(misc$bcr_data)) {
    bcr_data <- .dropEmptyRepertoireSamples(misc$bcr_data)
    ## Same check as the unified slot: `getImmuneRepertoire()` falls back to
    ## merging the legacy slots, so leaving them unchecked would just move the
    ## way around it rather than close it.
    bcr_data <- .normalizeImmuneRepertoire(
      bcr_data,
      cell_barcodes = cells,
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
    if (length(bcr_data) > 0L) {
      export$addBCRData(bcr_data)
    }
  }

  ##--------------------------------------------------------------------------##
  ## TCR data (legacy)
  ##--------------------------------------------------------------------------##
  if (!has_unified_repertoire && !is.null(misc$tcr_data)) {
    tcr_data <- .dropEmptyRepertoireSamples(misc$tcr_data)
    tcr_data <- .normalizeImmuneRepertoire(
      tcr_data,
      cell_barcodes = cells,
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
    if (length(tcr_data) > 0L) {
      export$addTCRData(tcr_data)
    }
  }

  ## R6 methods are serialized into the .crb, so changing the class getter does
  ## not repair files that were already written. For new exports that only use
  ## the two legacy slots, populate the unified field that the serialized getter
  ## already prefers. Keep the legacy fields above for getBCR()/getTCR().
  if (!has_unified_repertoire) {
    legacy_repertoire <- .mergeRepertoiresBySample(
      if (exists("tcr_data", inherits = FALSE)) tcr_data else NULL,
      if (exists("bcr_data", inherits = FALSE)) bcr_data else NULL
    )
    legacy_repertoire <- .dropEmptyRepertoireSamples(legacy_repertoire)
    if (!is.null(legacy_repertoire) && length(legacy_repertoire) > 0) {
      legacy_repertoire <- .normalizeImmuneRepertoire(
        legacy_repertoire,
        cell_barcodes = cells,
        source_label = "the merged legacy immune repertoire"
      )
      export$addImmuneRepertoire(legacy_repertoire)
    }
  }

  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if (!is.null(misc$marker_genes)) {
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
    if (!is.list(misc$marker_genes)) {
      stop('`misc$marker_genes` is not a list.', call. = FALSE)
    }
    for (i in seq_along(misc$marker_genes)) {
      method <- names(misc$marker_genes)[i]
      for (j in seq_along(misc$marker_genes[[method]])) {
        if (is.list(misc$marker_genes[[method]][j])) {
          group <- names(misc$marker_genes[[method]])[j]
          export$addMarkerGenes(
            method,
            group,
            misc$marker_genes[[method]][[group]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if (!is.null(misc$enriched_pathways)) {
    ## check if it's a list
    if (!is.list(misc$enriched_pathways)) {
      stop(
        '`misc$enriched_pathways` is not a list.',
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
    for (i in seq_along(misc$enriched_pathways)) {
      method <- names(misc$enriched_pathways)[i]
      ## for each group
      for (j in seq_along(misc$enriched_pathways[[method]])) {
        if (is.list(misc$enriched_pathways[[method]][j])) {
          group <- names(misc$enriched_pathways[[method]])[j]

          ## only add enriched pathways if group is present in `groups`
          if (group %in% groups) {
            export$addEnrichedPathways(
              method,
              group,
              misc$enriched_pathways[[method]][[group]]
            )
          }
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if (length(misc$trajectories) == 0) {
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
          paste(
            unlist(
              lapply(
                intersect(names(misc$trajectories), .exportTrajectoryMethods),
                function(method) names(misc$trajectories[[method]])
              )
            ),
            collapse = ', '
          )
        )
      )
    }
    ## for each method
    for (i in seq_along(misc$trajectories)) {
      method <- names(misc$trajectories)[i]
      if (method %in% .exportTrajectoryMethods) {
        ## for each trajectory
        for (j in seq_along(misc$trajectories[[i]])) {
          export$addTrajectory(
            method,
            names(misc$trajectories[[i]])[j],
            misc$trajectories[[i]][[j]]
          )
        }
      } else {
        warning(
          paste0(
            'Warning: Skipping trajectories of method `',
            method,
            '`. At the ',
            'moment, only trajectories of the methods ',
            paste0('`', .exportTrajectoryMethods, '`', collapse = ', '),
            ' are supported.'
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
    !is.null(misc$extra_material) &&
      is.list(misc$extra_material) &&
      length(misc$extra_material) > 0
  ) {
    if (verbose) {
      message(
        glue::glue(
          '[{format(Sys.time(), "%H:%M:%S")}] Found extra material to export...'
        )
      )
    }

    ## go through categories in `extra_material` slot
    for (category in names(misc$extra_material)) {
      ## do this if category is `tables`
      if (category == 'tables') {
        ## go through tables
        for (i in seq_along(misc$extra_material$tables)) {
          ## export table
          export$addExtraMaterial(
            category = 'tables',
            name = names(misc$extra_material$tables)[i],
            content = misc$extra_material$tables[[i]]
          )
        }

        ## do this if category is `plots`
      } else if (category == 'plots') {
        ## go through tables
        for (i in seq_along(misc$extra_material$plots)) {
          ## export table
          export$addExtraMaterial(
            category = 'plots',
            name = names(misc$extra_material$plots)[i],
            content = misc$extra_material$plots[[i]]
          )
        }
      }
    }
  }

  # Preserve workbook/sheet display names without adding a fake content category.
  table_index <- viewerExtraMaterialTableIndex(
    misc$extra_material$table_index,
    as.character(names(export$getExtraMaterial()$tables))
  )
  if (length(table_index)) {
    export$addTechnicalInfo("extra_material_table_index", table_index)
  }

  ##--------------------------------------------------------------------------##
  ## Trekker single-cell spatial mapping
  ##--------------------------------------------------------------------------##
  if (!is.null(misc$trekker)) {
    export$addTrekker(misc$trekker)
  }

  ## Default colours for groups, e.g. scanpy's uns[<column>_colors]: a list named
  ## by grouping variable, each a named vector (level -> "#RRGGBB"). The Viewer
  ## uses them as the starting colours; only exported groups are kept.
  if (is.list(misc$group_colors) && length(misc$group_colors)) {
    kept <- Filter(
      function(colors) {
        is.character(colors) &&
          !is.null(names(colors)) &&
          length(colors) > 0L &&
          !anyNA(colors) &&
          all(grepl("^#[0-9A-Fa-f]{6}$", colors))
      },
      misc$group_colors[intersect(names(misc$group_colors), groups)]
    )
    if (length(kept)) {
      export$addTechnicalInfo("group_colors", kept)
    }
  }

  invisible(export)
}

#' @keywords internal
#' @noRd
.exportFinalize <- function(
  export,
  final_file,
  export_stage_dir,
  expression_matrix_mode,
  codec,
  viewer_options
) {
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

  ## log message
  message(
    paste0(
      '[',
      format(Sys.time(), '%H:%M:%S'),
      '] Saving Cerebro object to: ',
      final_file
    )
  )

  ## Replace the sidecar and .crb with best-effort rollback on ordinary errors.
  .publishCerebroExport(
    export = export,
    final_file = final_file,
    stage_dir = export_stage_dir,
    expression_matrix_mode = expression_matrix_mode,
    codec = codec
  )
  if (
    .viewerPackEnabled(
      viewer_options$mode,
      viewer_options$threshold,
      length(.viewerPackCells(export))
    )
  ) {
    buildViewerPack(
      final_file,
      viewer_binary = "always",
      viewer_binary_threshold = viewer_options$threshold,
      overwrite = TRUE
    )
  }

  ## log message
  ## ... writing to file was successful
  if (file.exists(final_file)) {
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

#' Spatial tab from point coordinates held in embeddings (MERFISH, Slide-seq,
#' AnnData obsm["spatial"]): each embedding with at least two numeric columns
#' becomes a spatial entry, paired with the expression of the same cells. With
#' `split` (a vector named by cell) the cells are divided into one entry per
#' level, which is how several tissue sections kept in one coordinate frame are
#' shown as separate sections. Same record the Seurat route builds for a spatial
#' reduction. `images` holds tissue images per library (see
#' .h5adReadSpatialImages): the single library's images go to an unsplit entry,
#' and with `split` each section takes the images of the library of that name.
#' @keywords internal
#' @noRd
.exportAddSpatialFromEmbeddings <- function(
  export,
  embeddings,
  expression_data,
  assay,
  layer,
  split = NULL,
  images = NULL,
  verbose = FALSE
) {
  ## a matrix held cells x genes is cut there and handed on as a dgCMatrix
  cells_by_genes <- .cellsByGenesSeed(expression_data)
  ## a sidecar file moves with the .crb, so its cells are read now
  from_sidecar <- !is.null(.tenxSidecarOf(expression_data))
  add_entry <- function(
    entry_name,
    source_name,
    embedding,
    entry_images = NULL
  ) {
    spatial_data <- tryCatch(
      {
        cells <- intersect(rownames(embedding), colnames(expression_data))
        coordinates <- data.frame(
          .cell = cells,
          .image = entry_name,
          .coordinate_source = paste0("reduction.", source_name),
          x = as.numeric(embedding[cells, 1L]),
          y = as.numeric(embedding[cells, 2L]),
          row.names = cells,
          stringsAsFactors = FALSE
        )
        if (
          !length(cells) ||
            anyDuplicated(rownames(embedding)) ||
            any(!is.finite(coordinates$x)) ||
            any(!is.finite(coordinates$y))
        ) {
          stop("The spatial reduction has no safe paired coordinates.")
        }
        projection <- .spx_export_projection_coordinates(coordinates)
        list(
          coordinates = if (is.null(projection)) coordinates else projection,
          expression = if (from_sidecar) {
            methods::as(expression_data[, cells, drop = FALSE], "dgCMatrix")
          } else if (is.null(cells_by_genes)) {
            expression_data[, cells, drop = FALSE]
          } else {
            Matrix::t(cells_by_genes[cells, , drop = FALSE])
          },
          assay = assay,
          requested_layer = layer,
          layer = layer,
          image = entry_name,
          coordinate_source = paste0("reduction.", source_name)
        )
      },
      error = function(e) {
        ## never drop spatial data silently
        warning(
          "Could not extract spatial data for `",
          entry_name,
          "`: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
    if (is.null(spatial_data)) {
      return(invisible(NULL))
    }
    if (length(entry_images)) {
      spatial_data$histology_images <- entry_images
    }
    export$addSpatialData(entry_name, spatial_data)
    if (verbose) {
      message(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Added spatial data: ',
        entry_name,
        ' (',
        nrow(spatial_data$coordinates),
        ' cells)'
      )
    }
  }

  for (image_name in names(embeddings)) {
    embedding <- embeddings[[image_name]]
    if (is.null(split)) {
      ## one library's images belong to the one entry; several libraries need
      ## `split` to tell which section each belongs to
      if (length(images) > 1L) {
        message(
          "[INFO] The file has tissue images for ",
          length(images),
          " libraries; they are not attached to the single `",
          image_name,
          "` entry. Split the sections (spatial_split_by = \"library_id\") ",
          "to show each on its own image."
        )
      }
      add_entry(
        image_name,
        image_name,
        embedding,
        if (length(images) == 1L) images[[1L]]
      )
      next
    }
    section <- as.character(split[rownames(embedding)])
    levels_present <- unique(section[!is.na(section)])
    ## numeric-looking sections (e.g. Bregma -29, -24, ...) sort numerically
    numeric_levels <- suppressWarnings(as.numeric(levels_present))
    if (!anyNA(numeric_levels)) {
      levels_present <- levels_present[order(numeric_levels)]
    } else {
      levels_present <- sort(levels_present)
    }
    for (level in levels_present) {
      add_entry(
        paste0(image_name, " [", level, "]"),
        image_name,
        embedding[!is.na(section) & section == level, , drop = FALSE],
        images[[level]]
      )
    }
  }
  invisible(export)
}
