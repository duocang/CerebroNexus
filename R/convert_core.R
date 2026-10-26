## Format-independent conversion steps -----------------------------------------
##
## The pre-export preparation shared by convertSeuratToCerebro() and
## convertSCEToCerebro(). They work on the cell meta data (`meta`: anything with
## names(), [[, [[<- and rownames(), i.e. a data.frame or a DataFrame) and on
## the free-form `misc` list (Seurat @misc, SCE metadata()), and hand the edited
## copies back; they never see the container object.

#' @keywords internal
#' @noRd
.convertValidateGroups <- function(meta, groups, max_group_levels, verbose) {
  missing_groups <- groups[!groups %in% names(meta)]

  if (length(missing_groups) > 0) {
    if (length(missing_groups) == length(groups)) {
      # All groups are missing - stop execution
      stop(
        paste0(
          "All specified groups are missing from metadata: ",
          paste(missing_groups, collapse = ", "),
          "\nAvailable columns: ",
          paste(names(meta), collapse = ", ")
        ),
        call. = FALSE
      )
    } else {
      # Some groups are missing - warn user
      warning(
        paste0(
          "Some specified groups are missing from metadata and will be skipped: ",
          paste(missing_groups, collapse = ", ")
        ),
        call. = FALSE
      )
      # Remove missing groups from the groups vector
      groups <- groups[groups %in% names(meta)]
    }
  }

  # Convert group values to character and check level counts ----------------##
  groups_to_remove <- character(0)

  for (group_name in groups) {
    # Convert to character
    meta[[group_name]] <- as.character(meta[[
      group_name
    ]])

    # Count unique levels (excluding NA)
    unique_levels <- unique(meta[[group_name]][
      !is.na(meta[[group_name]])
    ])
    n_levels <- length(unique_levels)

    # Check if exceeds maximum
    if (n_levels > max_group_levels) {
      if (verbose) {
        message(paste0(
          "[WARNING] Group '",
          group_name,
          "' has ",
          n_levels,
          " unique levels (> ",
          max_group_levels,
          "), will be excluded."
        ))
      }
      groups_to_remove <- c(groups_to_remove, group_name)
    }
  }

  # Remove groups with too many levels
  if (length(groups_to_remove) > 0) {
    groups <- groups[!groups %in% groups_to_remove]

    # Check if any groups remain
    if (length(groups) == 0) {
      stop(
        paste0(
          "All groups have been excluded due to exceeding max_group_levels (",
          max_group_levels,
          "). Consider increasing max_group_levels or ",
          "using different grouping variables."
        ),
        call. = FALSE
      )
    }

    if (verbose) {
      message(paste0(
        "[INFO] Remaining groups: ",
        paste(groups, collapse = ", ")
      ))
    }
  }

  list(meta = meta, groups = groups)
}

#' @keywords internal
#' @noRd
.convertRenameGroups <- function(
  meta,
  misc,
  groups,
  groups_naming,
  repertoire_sample_col = "orig.ident"
) {
  # Rename group columns according to groups_naming
  if (!is.null(groups_naming) && length(groups_naming) > 0) {
    # Validate groups_naming structure
    if (is.null(names(groups_naming)) || length(names(groups_naming)) == 0) {
      stop(
        "groups_naming must be a named list/vector with names corresponding to existing group names.",
        call. = FALSE
      )
    }
    source_names <- names(groups_naming)
    if (
      anyNA(source_names) ||
        any(!nzchar(source_names)) ||
        anyDuplicated(source_names)
    ) {
      stop(
        "groups_naming source names must be non-empty and unique.",
        call. = FALSE
      )
    }
    # Check if at least one name in groups_naming exists in groups
    valid_names <- names(groups_naming)[names(groups_naming) %in% groups]

    if (length(valid_names) < length(names(groups_naming))) {
      warning(
        paste0(
          "Some names in groups_naming do not exist in the specified groups and will be ignored: ",
          paste(setdiff(names(groups_naming), valid_names), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (length(valid_names) == 0) {
      stop(
        paste0(
          "None of the names in groups_naming exist in the specified groups.\n",
          "groups_naming names: ",
          paste(names(groups_naming), collapse = ", "),
          "\n",
          "Available groups: ",
          paste(groups, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    effective_mappings <- groups_naming[valid_names]
    valid_targets <- vapply(
      effective_mappings,
      function(target) {
        is.character(target) &&
          length(target) == 1L &&
          !is.na(target) &&
          nzchar(target)
      },
      logical(1)
    )
    if (!all(valid_targets)) {
      stop(
        "groups_naming targets must each be a non-empty scalar character name.",
        call. = FALSE
      )
    }
    target_names <- unname(vapply(
      effective_mappings,
      `[[`,
      character(1),
      1L
    ))
    duplicate_target <- duplicated(target_names) |
      duplicated(
        target_names,
        fromLast = TRUE
      )
    if (any(duplicate_target)) {
      stop(
        "Multiple groups map to the same groups_naming target '",
        target_names[which(duplicate_target)[[1L]]],
        "'.",
        call. = FALSE
      )
    }

    # Preflight every effective rename before changing metadata or misc. This
    # keeps the in-memory Seurat object intact if any later mapping conflicts.
    keyed_misc <- c(
      "trees",
      "most_expressed_genes",
      "mean_expression",
      "group_colors"
    )
    for (old_name in valid_names) {
      new_name <- groups_naming[[old_name]]
      if (identical(old_name, new_name)) {
        next
      }
      if (new_name %in% names(meta)) {
        stop(
          "Group rename metadata conflict: '",
          old_name,
          "' -> '",
          new_name,
          "'; the target column already exists.",
          call. = FALSE
        )
      }
      for (collection in keyed_misc) {
        payload <- misc[[collection]]
        if (
          is.list(payload) &&
            old_name %in% names(payload) &&
            new_name %in% names(payload)
        ) {
          stop(
            "Group rename conflict in misc payload '",
            collection,
            "': '",
            old_name,
            "' -> '",
            new_name,
            "'; both keys already exist.",
            call. = FALSE
          )
        }
      }
    }

    for (old_name in valid_names) {
      new_name <- groups_naming[[old_name]]
      if (identical(old_name, new_name)) {
        next
      }
      # Rename column in metadata
      meta[[new_name]] <- meta[[old_name]]
      meta[[old_name]] <- NULL

      # Keep group-indexed analysis payloads aligned with the renamed metadata.
      # exportFromSeurat() registers the new group names before importing these
      # collections, so a stale key would either be dropped (summary tables) or
      # rejected (trees) even though the public rename itself is valid.
      for (collection in keyed_misc) {
        payload <- misc[[collection]]
        if (
          is.list(payload) &&
            old_name %in% names(payload) &&
            !new_name %in% names(payload)
        ) {
          names(payload)[names(payload) == old_name] <- new_name
          misc[[collection]] <- payload
        }
      }

      # Update groups vector with new name
      idx <- which(groups == old_name)
      if (length(idx) > 0) {
        groups[idx] <- new_name
      }
      if (identical(old_name, repertoire_sample_col)) {
        repertoire_sample_col <- new_name
      }
    }
  }

  list(
    meta = meta,
    misc = misc,
    groups = groups,
    repertoire_sample_col = repertoire_sample_col
  )
}

#' @keywords internal
#' @noRd
#' @param group_values Optional named list (grouping variable -> its values). When
#'   the marker table's cluster column holds values of exactly one of them, the
#'   table is stored as ONE table named after that grouping variable, with the
#'   cluster column first and renamed to it. That is the shape the Viewer's
#'   per-group filter expects (and what scanpy's results arrive as); without it,
#'   or when no grouping variable matches, the table is split per cluster value
#'   as the Seurat route has always done.
.convertAttachMarkers <- function(
  misc,
  marker_file,
  marker_method,
  verbose,
  group_values = NULL
) {
  if (!is.null(marker_file) && nzchar(marker_file)) {
    if (!file.exists(marker_file)) {
      stop("marker_file not found: ", marker_file, call. = FALSE)
    }

    markers_df <- .readMarkerFile(marker_file, verbose)

    # Attach to Seurat object under misc$marker_genes.
    # exportFromSeurat expects list(method = list(group = df)).
    # Split markers_df by 'cluster' / 'group' column when available,
    # otherwise dump as a single 'all' group under the chosen method.
    if (!is.null(markers_df) && nrow(markers_df) > 0) {
      group_col <- intersect(c("cluster", "group"), colnames(markers_df))[1]
      matched <- character()
      if (!is.na(group_col) && length(group_values)) {
        cluster_values <- unique(as.character(markers_df[[group_col]]))
        matched <- names(group_values)[vapply(
          group_values,
          function(values) all(cluster_values %in% as.character(values)),
          logical(1)
        )]
      }
      if (length(matched) == 1L) {
        table <- markers_df[
          c(group_col, setdiff(names(markers_df), group_col))
        ]
        names(table)[[1L]] <- matched
        per_group <- stats::setNames(list(table), matched)
      } else if (!is.na(group_col)) {
        per_group <- split(markers_df, markers_df[[group_col]], drop = TRUE)
      } else {
        per_group <- list(all = markers_df)
      }
      misc$marker_genes <- list()
      misc$marker_genes[[marker_method]] <- per_group
    }
  }

  misc
}

#' @keywords internal
#' @noRd
.convertAddProvidedMostExpressed <- function(
  misc,
  most_expressed_genes,
  verbose
) {
  provided <- FALSE
  if (!is.null(most_expressed_genes)) {
    # User provided most_expressed_genes
    if (is.data.frame(most_expressed_genes)) {
      # Convert data.frame to list
      most_expressed_genes <- list(unknown = most_expressed_genes)
      if (verbose) {
        message(
          "[INFO] Converted most_expressed_genes from data.frame to list(unknown = ...)"
        )
      }
    } else if (is.list(most_expressed_genes)) {
      # Check if list elements are data.frames and handle unnamed elements
      unnamed_count <- 0
      for (i in seq_along(most_expressed_genes)) {
        # Check if element is a data.frame
        if (!is.data.frame(most_expressed_genes[[i]])) {
          warning(paste0(
            "Element ",
            i,
            " in most_expressed_genes is not a data.frame and will be skipped."
          ))
          most_expressed_genes[[i]] <- NULL
          next
        }

        # Handle unnamed elements
        if (
          is.null(names(most_expressed_genes)[i]) ||
            names(most_expressed_genes)[i] == ""
        ) {
          unnamed_count <- unnamed_count + 1
          names(most_expressed_genes)[i] <- paste0("unknown", unnamed_count)
          if (verbose) {
            message(paste0(
              "[INFO] Assigned name 'unknown",
              unnamed_count,
              "' to unnamed element ",
              i
            ))
          }
        }
      }
      # Remove NULL elements (those that weren't data.frames)
      most_expressed_genes <- most_expressed_genes[
        !sapply(most_expressed_genes, is.null)
      ]
    } else {
      stop(
        "most_expressed_genes must be either a data.frame or a list of data.frames.",
        call. = FALSE
      )
    }

    # Assign to seurat object
    if (length(most_expressed_genes) > 0) {
      if (
        is.null(misc$most_expressed_genes) ||
          !is.list(misc$most_expressed_genes)
      ) {
        misc$most_expressed_genes <- list()
      }
      misc$most_expressed_genes <- c(
        misc$most_expressed_genes,
        most_expressed_genes
      )
      if (verbose) {
        message(paste0(
          "[INFO] Added ",
          length(most_expressed_genes),
          " most_expressed_genes group(s): ",
          paste(names(most_expressed_genes), collapse = ", ")
        ))
      }
    }

    # Don't calculate if user provided data
    provided <- TRUE
  }

  list(misc = misc, provided = provided)
}

#' Per gene and group value: cells, sum of values and cells with a value
#' above zero, for a sparse genes x cells matrix (or the lazy transpose of a
#' cells x genes one). `codes` gives each cell's value index or NA, in matrix
#' order. Both are one sparse product with a cells x groups indicator, which
#' adds each gene's cells in matrix order as Matrix::rowSums() does, instead
#' of a subset per group value. A matrix read from an H5 sidecar is summed
#' from the file in the same order. NULL for other matrix classes.
#' @keywords internal
#' @noRd
.groupGeneStats <- function(expr_matrix, codes, n_groups) {
  keep <- which(!is.na(codes))
  sidecar <- .tenxSidecarOf(expr_matrix)
  if (!is.null(sidecar)) {
    ## an H5 sidecar: summed while it was written from the h5ad, or read gene
    ## by gene by compiled code
    stats <- .tempSidecarGroupStats(sidecar$file, codes, n_groups)
    if (is.null(stats)) {
      stats <- sidecarGroupStatsCpp(
        sidecar$file,
        sidecar$group,
        ifelse(is.na(codes), -1L, as.integer(codes) - 1L),
        as.integer(n_groups),
        .sidecarThreads()
      )
    }
    names <- list(rownames(expr_matrix), NULL)
    dimnames(stats$total) <- names
    dimnames(stats$positive) <- names
    return(list(
      n = tabulate(codes[keep], n_groups),
      total = stats$total,
      positive = stats$positive
    ))
  }
  cells_by_genes <- .cellsByGenesSeed(expr_matrix)
  if (is.null(cells_by_genes) && !inherits(expr_matrix, "dgCMatrix")) {
    return(NULL)
  }
  membership <- Matrix::sparseMatrix(
    i = keep,
    j = codes[keep],
    x = 1,
    dims = c(length(codes), n_groups)
  )
  genes <- rownames(expr_matrix)
  m <- if (is.null(cells_by_genes)) expr_matrix else cells_by_genes
  per_group <- function(values) {
    out <- if (is.null(cells_by_genes)) {
      as.matrix(values %*% membership)
    } else {
      t(as.matrix(Matrix::t(membership) %*% values))
    }
    dimnames(out) <- list(genes, NULL)
    out
  }
  total <- per_group(m)
  ## the same pattern with 1 for every value above zero
  m@x <- if (isTRUE(length(m@x) == 0L || min(m@x) > 0)) {
    rep(1, length(m@x))
  } else {
    as.numeric(m@x > 0)
  }
  positive <- per_group(m)
  list(n = tabulate(codes[keep], n_groups), total = total, positive = positive)
}

#' @keywords internal
#' @noRd
.convertComputeGroupSummaries <- function(
  misc,
  meta,
  expr_matrix,
  groups,
  verbose
) {
  # Initialize list structures
  if (
    is.null(misc$most_expressed_genes) ||
      !is.list(misc$most_expressed_genes)
  ) {
    misc$most_expressed_genes <- list()
  }
  if (
    is.null(misc$mean_expression) ||
      !is.list(misc$mean_expression)
  ) {
    misc$mean_expression <- list()
  }

  # Calculate for each group
  for (group_name in groups) {
    if (verbose) {
      message(paste0(
        "[",
        format(Sys.time(), "%H:%M:%S"),
        "] Processing group: ",
        group_name
      ))
    }

    group_values <- unique(meta[[group_name]])
    group_values <- group_values[!is.na(group_values)]

    pct_results <- list()
    expr_results <- list()

    ## all values of the group in one pass over the matrix, when it is sparse
    group_stats <- .groupGeneStats(
      expr_matrix,
      match(
        as.character(meta[[group_name]])[match(
          colnames(expr_matrix),
          rownames(meta)
        )],
        as.character(group_values)
      ),
      length(group_values)
    )

    for (value_index in seq_along(group_values)) {
      group_value <- group_values[[value_index]]
      if (!is.null(group_stats)) {
        n_cells <- group_stats$n[[value_index]]
        if (n_cells == 0) {
          next
        }
        gene_pct <- group_stats$positive[, value_index] / n_cells * 100
        gene_mean <- group_stats$total[, value_index] / n_cells
      } else {
        # Get cells belonging to this group value using cell names (more robust)
        cells_in_group <- rownames(meta)[
          meta[[group_name]] == group_value
        ]
        # Filter to cells that exist in the expression matrix
        cells_in_group <- cells_in_group[
          cells_in_group %in% colnames(expr_matrix)
        ]

        if (length(cells_in_group) == 0) {
          next
        }

        # Get expression subset for this group
        expr_subset <- expr_matrix[, cells_in_group, drop = FALSE]

        # Calculate percentage of cells expressing each gene
        gene_pct <- Matrix::rowSums(expr_subset > 0) /
          length(cells_in_group) *
          100

        # Calculate mean expression per gene
        gene_mean <- Matrix::rowMeans(expr_subset)
      }

      # Create data frame for percentage and sort
      gene_pct_df <- data.frame(
        gene = names(gene_pct),
        pct = as.numeric(gene_pct),
        stringsAsFactors = FALSE
      )
      gene_pct_df[["cluster"]] <- as.character(group_value)
      gene_pct_df <- gene_pct_df[order(-gene_pct_df$pct), ]
      rownames(gene_pct_df) <- NULL

      # Create data frame for mean expression and sort
      gene_expr_df <- data.frame(
        gene = names(gene_mean),
        mean_expr = as.numeric(gene_mean),
        stringsAsFactors = FALSE
      )
      gene_expr_df[["cluster"]] <- as.character(group_value)
      gene_expr_df <- gene_expr_df[order(-gene_expr_df$mean_expr), ]
      rownames(gene_expr_df) <- NULL

      pct_results[[as.character(group_value)]] <- gene_pct_df
      expr_results[[as.character(group_value)]] <- gene_expr_df
    }

    # Merge all data frames into one and reorder columns with cluster first
    if (length(pct_results) > 0) {
      pct_results_df <- do.call(rbind, pct_results)
      rownames(pct_results_df) <- NULL
      pct_results_df <- pct_results_df[, c(
        "cluster",
        setdiff(names(pct_results_df), "cluster")
      )]
      misc$most_expressed_genes[[group_name]] <- pct_results_df
    } else {
      misc$most_expressed_genes[[group_name]] <- data.frame()
    }

    if (length(expr_results) > 0) {
      expr_results_df <- do.call(rbind, expr_results)
      rownames(expr_results_df) <- NULL
      expr_results_df <- expr_results_df[, c(
        "cluster",
        setdiff(names(expr_results_df), "cluster")
      )]
      misc$mean_expression[[group_name]] <- expr_results_df
    } else {
      misc$mean_expression[[group_name]] <- data.frame()
    }
  }

  if (verbose) {
    message(paste0(
      "[",
      format(Sys.time(), "%H:%M:%S"),
      "] Most expressed genes and mean expression calculation completed."
    ))
  }

  misc
}
