#' @keywords internal
#' @noRd
.loadImmuneRepertoireData <- function(file_path, data_type, verbose = TRUE) {
  data_type_upper <- toupper(data_type)

  if (is.null(file_path) || !nzchar(file_path)) {
    return(NULL)
  }

  if (!file.exists(file_path)) {
    stop(
      data_type_upper,
      " file not found: ",
      file_path,
      "\n",
      "Suggestions:\n",
      "  1. Check if the file path is correct\n",
      "  2. Verify the file extension is .rds\n",
      "  3. Ensure you have read permissions for the file"
    )
  }

  data <- tryCatch(
    {
      readRDS(file_path)
    },
    error = function(e) {
      stop(
        "Failed to read ",
        data_type_upper,
        " data from: ",
        file_path,
        "\n",
        "  Error: ",
        e$message,
        "\n",
        "Suggestions:\n",
        "  1. Verify the file is a valid .rds file\n",
        "  2. Check if the file was created using saveRDS()\n",
        "  3. Try reading the file directly: readRDS('",
        file_path,
        "')"
      )
    }
  )

  if (is.null(data)) {
    stop(
      data_type_upper,
      " data is NULL after reading from: ",
      file_path,
      "\n",
      "Suggestions:\n",
      "  1. Check if the source file contains valid data\n",
      "  2. Verify the file was not corrupted\n",
      "  3. Try recreating the .rds file"
    )
  }

  if (!is.list(data) || length(data) == 0) {
    stop(
      data_type_upper,
      " data is not a valid list or is empty.\n",
      "  Expected: A list of contig annotations\n",
      "  Received: ",
      class(data)[1],
      " with length ",
      length(data),
      "\n",
      "Suggestions:\n",
      "  1. Verify the data structure matches scRepertoire format\n",
      "  2. Check if the data was properly saved using saveRDS()\n",
      "  3. Ensure the data contains contig annotations"
    )
  }

  if (verbose) {
    message("[INFO] Loaded ", data_type_upper, " data from: ", file_path)
    message(
      "[INFO] ",
      data_type_upper,
      " data contains ",
      length(data),
      " samples"
    )
  }

  return(data)
}

#' Extract immune repertoire data from Seurat metadata
#'
#' When scRepertoire's \code{combineExpression()} has been used, the Seurat
#' metadata contains columns like CTgene, CTnt, CTaa, CTstrict, etc.
#' This function extracts those columns and splits by sample into the
#' list-of-data.frames format expected by scRepertoire visualization functions.
#' TCR and BCR data are kept together; scRepertoire's \code{chain} parameter
#' handles filtering at plot time.
#'
#' @param seurat A Seurat object with scRepertoire columns in meta.data.
#' @param groups Character vector of group column names to include in output.
#' @param sample_col Column name to split samples by; defaults to "orig.ident".
#' @param verbose Logical; print progress messages.
#' @return A named list of data.frames (one per sample), or NULL if no
#'   repertoire data is found.
#' @keywords internal
#' @noRd
.extractRepertoireFromMetadata <- function(
  seurat,
  groups = NULL,
  sample_col = "orig.ident",
  verbose = TRUE
) {
  core_cols <- c("CTgene", "CTnt", "CTaa", "CTstrict")
  meta_names <- names(seurat@meta.data)
  present_core <- core_cols[core_cols %in% meta_names]

  if (length(present_core) == 0) {
    if (verbose) {
      message(
        "[INFO] No scRepertoire columns found in metadata, ",
        "skipping repertoire extraction."
      )
    }
    return(NULL)
  }

  if (verbose) {
    message(paste0(
      "[",
      format(Sys.time(), "%H:%M:%S"),
      "] Found scRepertoire columns in metadata: ",
      paste(present_core, collapse = ", ")
    ))
  }

  # Additional scRepertoire columns to preserve
  optional_cols <- c(
    "clonalProportion",
    "clonalFrequency",
    "cloneSize",
    "Frequency",
    "frequency",
    "cloneType"
  )
  present_optional <- optional_cols[optional_cols %in% meta_names]

  # Identify cells with non-NA repertoire data
  primary_col <- if ("CTgene" %in% present_core) "CTgene" else present_core[1]
  has_data <- !is.na(seurat@meta.data[[primary_col]]) &
    nzchar(as.character(seurat@meta.data[[primary_col]]))

  if (sum(has_data) == 0) {
    if (verbose) {
      message("[INFO] No cells with non-NA repertoire data found.")
    }
    return(NULL)
  }

  # Columns to keep
  cols_to_keep <- unique(c(present_core, present_optional))
  if (!is.null(groups)) {
    cols_to_keep <- unique(c(cols_to_keep, groups[groups %in% meta_names]))
  }

  rep_df <- seurat@meta.data[has_data, cols_to_keep, drop = FALSE]
  rep_df$barcode <- rownames(seurat@meta.data)[has_data]

  # Determine sample column for splitting
  actual_sample_col <- NULL
  for (col in c(sample_col, "orig.ident", "sample", "Sample")) {
    if (col %in% meta_names) {
      actual_sample_col <- col
      break
    }
  }

  if (!is.null(actual_sample_col)) {
    rep_df$.sample_id <- as.character(
      seurat@meta.data[[actual_sample_col]][has_data]
    )
  } else {
    rep_df$.sample_id <- "Sample_1"
  }

  # Split by sample into list-of-data.frames
  result <- split(rep_df, rep_df$.sample_id)
  result <- lapply(result, function(x) {
    x$.sample_id <- NULL
    x
  })

  if (verbose) {
    # Detect data types present
    types <- character(0)
    if ("CTgene" %in% names(rep_df)) {
      ct <- as.character(rep_df$CTgene)
      if (any(grepl("TR[ABDG]", ct))) {
        types <- c(types, "TCR")
      }
      if (any(grepl("IG[HKL]", ct))) types <- c(types, "BCR")
    }
    message(paste0(
      "[INFO] Extracted immune repertoire: ",
      sum(has_data),
      " cells in ",
      length(result),
      " sample(s)",
      if (length(types) > 0) {
        paste0(" [", paste(types, collapse = "+"), "]")
      } else {
        ""
      }
    ))
  }

  return(result)
}

#' Assemble Cell Ranger contig annotations into clones
#'
#' Turning per-contig rows into the per-cell clone calls the app reads is
#' scRepertoire's job, and it is a job with real subtleties (chain pairing,
#' multi-chain cells, clone definitions). Delegate rather than reimplement.
#'
#' Name a sample after the directory holding its contig file
#'
#' Cell Ranger gives every sample's file the same name, so that name never
#' identifies a sample -- not even when there is only one file. The containing
#' directory does. This is decided per path rather than only when names
#' collide: a lone `filtered_contig_annotations.csv` used to become a sample
#' literally called "filtered_contig_annotations", and scRepertoire then
#' prefixed every barcode with it, which is a reliable way to end up with a
#' repertoire that matches no cell.
#'
#' A file the user renamed keeps its own stem, since that is the only signal
#' they gave.
#'
#' @param paths Contig annotation file paths.
#'
#' @return One sample name per path.
#'
#' @keywords internal
#' @noRd
.deriveContigSampleNames <- function(paths) {
  standard_names <- c(
    "filtered_contig_annotations.csv",
    "all_contig_annotations.csv"
  )
  sample_names <- tools::file_path_sans_ext(basename(paths))
  from_cellranger <- basename(paths) %in% standard_names
  sample_names[from_cellranger] <- basename(dirname(paths[from_cellranger]))

  ## Renamed files can still collide with each other; the directory is the
  ## next best guess for all of them.
  if (anyDuplicated(sample_names)) {
    sample_names <- basename(dirname(paths))
  }
  if (anyDuplicated(sample_names)) {
    stop(
      "Could not derive distinct sample names from the contig file paths. ",
      "Pass `sample_names` explicitly.",
      call. = FALSE
    )
  }
  sample_names
}

#' @keywords internal
#' @noRd
.combineContigAnnotations <- function(
  paths,
  receptor,
  sample_names = NULL,
  verbose = TRUE
) {
  if (!requireNamespace("scRepertoire", quietly = TRUE)) {
    stop(
      "Reading Cell Ranger contig annotations needs the scRepertoire package, ",
      "which assembles contigs into clones. Install it, or pass the output of ",
      "scRepertoire::combineTCR() / combineBCR() directly.",
      call. = FALSE
    )
  }

  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths) > 0) {
    stop(
      "Contig annotation file(s) not found: ",
      paste(missing_paths, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(sample_names)) {
    sample_names <- .deriveContigSampleNames(paths)
  }
  if (length(sample_names) != length(paths)) {
    stop(
      "`sample_names` has ",
      length(sample_names),
      " entries for ",
      length(paths),
      " contig annotation file(s); they have to line up.",
      call. = FALSE
    )
  }

  if (verbose) {
    message(
      "[INFO] Reading ",
      length(paths),
      " ",
      toupper(receptor),
      " contig annotation file(s)"
    )
  }

  contigs <- lapply(paths, function(path) {
    utils::read.csv(path, stringsAsFactors = FALSE)
  })

  combined <- if (identical(toupper(receptor), "BCR")) {
    scRepertoire::combineBCR(contigs, samples = sample_names)
  } else {
    scRepertoire::combineTCR(contigs, samples = sample_names)
  }

  ## `samples =` prefixes every barcode with the sample name. The cell names
  ## have to carry the same prefix or nothing joins; the caller's shape check
  ## reports it when they do not.
  combined
}

#' Merge repertoires that describe the same samples
#'
#' A sample is one biological sample, not one receptor type, so `donorA`'s T
#' cells and `donorA`'s B cells belong in one table. Concatenating the two
#' lists instead produces two entries called `donorA`, and `x[["donorA"]]`
#' returns only the first -- so one receptor type silently disappears, while
#' the app and the HLA page read the list's names as sample identifiers.
#'
#' Cells are mutually exclusive between the two (a cell carries a TCR or a
#' BCR), which is why row-binding them is the right merge and is what the
#' vignettes have always told people to do by hand.
#'
#' @keywords internal
#' @noRd
.mergeRepertoiresBySample <- function(...) {
  parts <- Filter(
    function(part) !is.null(part) && length(part) > 0,
    list(...)
  )
  if (length(parts) == 0) {
    return(NULL)
  }
  if (length(parts) == 1) {
    return(parts[[1]])
  }

  sample_names <- unique(unlist(lapply(parts, names), use.names = FALSE))
  merged <- lapply(sample_names, function(sample_name) {
    frames <- Filter(
      Negate(is.null),
      lapply(parts, function(part) part[[sample_name]])
    )
    if (length(frames) == 1) {
      return(frames[[1]])
    }
    ## TCR and BCR tables carry the same scRepertoire columns in the ordinary
    ## case, but not necessarily -- fill rather than fail on a missing one.
    all_columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
    filled <- lapply(frames, function(frame) {
      for (column in setdiff(all_columns, names(frame))) {
        ## Length has to follow the frame: a sample can legitimately have no
        ## rows -- every receptor filtered out -- and assigning a scalar to a
        ## zero-row data.frame is an error ("replacement has 1 row, data has 0").
        frame[[column]] <- rep(NA, nrow(frame))
      }
      frame[, all_columns, drop = FALSE]
    })
    bound <- do.call(rbind, filled)
    rownames(bound) <- NULL
    bound
  })
  names(merged) <- sample_names
  merged
}

#' Resolve one repertoire argument into a named list of data.frames
#'
#' @keywords internal
#' @noRd
.resolveRepertoireInput <- function(
  input,
  receptor,
  sample_names = NULL,
  verbose = TRUE
) {
  if (is.null(input)) {
    return(NULL)
  }

  argument_label <- paste0("the `", tolower(receptor), "` argument")

  if (is.character(input)) {
    ## `convertSeuratToCerebro()` passes its `bcr_file` / `tcr_file` through
    ## unchanged, and those default to NULL but are often "" in scripts.
    input <- input[nzchar(input)]
    if (length(input) == 0) {
      return(NULL)
    }
    extensions <- tolower(tools::file_ext(input))

    if (all(extensions == "rds")) {
      if (length(input) != 1) {
        stop(
          argument_label,
          " holds several .rds paths. Pass one .rds holding the whole named ",
          "list, or pass the list itself.",
          call. = FALSE
        )
      }
      loaded <- .loadImmuneRepertoireData(input, receptor, verbose)
      .validateImmuneRepertoire(loaded, source_label = argument_label)
      return(loaded)
    }

    if (all(extensions == "csv")) {
      return(.combineContigAnnotations(
        input,
        receptor = receptor,
        sample_names = sample_names,
        verbose = verbose
      ))
    }

    stop(
      argument_label,
      " holds file paths that are neither all .rds nor all .csv: ",
      paste(unique(extensions), collapse = ", "),
      ". Pass a .rds holding a named list, Cell Ranger ",
      "filtered_contig_annotations.csv files, or the list itself.",
      call. = FALSE
    )
  }

  .validateImmuneRepertoire(input, source_label = argument_label)
  input
}

#' Add immune repertoire data to a Seurat object
#'
#' Puts TCR and/or BCR data into \code{object@misc$immune_repertoire} in the
#' shape CerebroNexus reads: a list named by sample, each element a data.frame
#' whose \code{barcode} column matches the object's cell names.
#'
#' Before this function existed the slot had to be assigned by hand, following
#' a convention written down in a vignette with no function behind it and
#' nothing checking the result. The shape is checked here, so a mistake is
#' reported where it was made rather than surfacing as an empty page.
#'
#' @param object A \code{Seurat} object.
#' @param tcr TCR data, as either the named list \code{scRepertoire::combineTCR()}
#'   returns, the path to an \code{.rds} holding such a list, or a vector of
#'   Cell Ranger \code{filtered_contig_annotations.csv} paths (one per sample,
#'   assembled with scRepertoire).
#' @param bcr BCR data, in any of the forms accepted for \code{tcr}.
#' @param sample_col Metadata column identifying the sample, used when reading
#'   the repertoire out of \code{meta.data}; defaults to \code{"orig.ident"}.
#' @param sample_names Sample names for the \code{.csv} form, in the order the
#'   paths are given. Derived from the paths when omitted.
#' @param groups Metadata columns to carry into the extracted data.frames when
#'   reading out of \code{meta.data}.
#' @param from_metadata When neither \code{tcr} nor \code{bcr} is given, read
#'   the repertoire out of scRepertoire's \code{meta.data} columns (\code{CTgene},
#'   \code{CTnt}, \code{CTaa}, \code{CTstrict}) if they are there. Defaults to
#'   \code{TRUE}.
#' @param verbose Print progress messages; defaults to \code{TRUE}.
#'
#' @return The \code{Seurat} object, with the repertoire in
#'   \code{@misc$immune_repertoire}. Returned unchanged when there is no
#'   repertoire to add.
#'
#' @examples
#' \dontrun{
#' ## from scRepertoire's output
#' combined <- scRepertoire::combineTCR(contig_list, samples = donor_ids)
#' seurat <- addImmuneRepertoire(seurat, tcr = combined)
#'
#' ## or straight from Cell Ranger
#' seurat <- addImmuneRepertoire(
#'   seurat,
#'   tcr = file.path(sample_dirs, "filtered_contig_annotations.csv"),
#'   sample_names = donor_ids
#' )
#'
#' ## or from meta.data, after scRepertoire::combineExpression()
#' seurat <- addImmuneRepertoire(seurat)
#'
#' exportFromSeurat(seurat, file = "my_data.crb", experiment_name = "mine",
#'                  organism = "hg", groups = c("sample", "seurat_clusters"))
#' }
#'
#' @export
addImmuneRepertoire <- function(
  object,
  tcr = NULL,
  bcr = NULL,
  sample_col = "orig.ident",
  sample_names = NULL,
  groups = NULL,
  from_metadata = TRUE,
  verbose = TRUE
) {
  if (!methods::is(object, "Seurat")) {
    stop(
      "`object` must be of class 'Seurat', not '",
      class(object)[1],
      "'.",
      call. = FALSE
    )
  }

  tcr_data <- .resolveRepertoireInput(tcr, "TCR", sample_names, verbose)
  bcr_data <- .resolveRepertoireInput(bcr, "BCR", sample_names, verbose)

  repertoire <- .mergeRepertoiresBySample(tcr_data, bcr_data)

  if (length(repertoire) == 0 && isTRUE(from_metadata)) {
    repertoire <- .extractRepertoireFromMetadata(
      object,
      groups = groups,
      sample_col = sample_col,
      verbose = verbose
    )
  }

  if (is.null(repertoire) || length(repertoire) == 0) {
    if (verbose) {
      message(
        "[INFO] No immune repertoire data to add; object returned unchanged."
      )
    }
    return(object)
  }

  .validateImmuneRepertoire(
    repertoire,
    cell_barcodes = colnames(object),
    source_label = "the immune repertoire data",
    ## Handed in on purpose: data that matches no cell is a mistake worth
    ## refusing here, where the caller can still fix it.
    zero_overlap = "error"
  )

  object@misc$immune_repertoire <- repertoire
  object
}
