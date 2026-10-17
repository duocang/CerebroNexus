## Trajectory information from AnnData ----------------------------------------
##
## Cerebro's Trajectory page is built around one structure per trajectory,
## list(meta, edges): `meta` has one row per cell with its position in a 2-D
## space (DR_1, DR_2), a numeric `pseudotime` and a factor `state`; `edges` are
## straight segments drawn under the cells. What scanpy and scVelo leave in an
## AnnData file is mapped onto that, per pseudotime column found in `obs`:
##
##   method "paga"        uns["paga"]: groups as nodes (at the centroid of their
##                        cells in the embedding), connectivities as edges
##   method "scvelo"      obsm["velocity_<basis>"]: the velocity field averaged
##                        on a grid, each arrow drawn as a shaft and two barbs
##   method "pseudotime"  only the pseudotime column and the embedding (no
##                        edges), when the file has neither of the above
##
## The Trajectory page needs a pseudotime, so graphs without one are skipped.

#' @keywords internal
#' @noRd
.h5adTrajectoryMethods <- c("paga", "scvelo", "pseudotime")

#' Numeric obs columns that hold a pseudotime (scanpy dpt, scVelo velocity
#' pseudotime and latent time, and the usual `<tool>_pseudotime` names).
#' @keywords internal
#' @noRd
.h5adPseudotimeColumns <- function(columns) {
  numeric <- names(columns)[vapply(columns, is.numeric, logical(1))]
  known <- c("dpt_pseudotime", "velocity_pseudotime", "latent_time")
  others <- setdiff(
    numeric[grepl("pseudotime$", numeric, ignore.case = TRUE)],
    known
  )
  c(known[known %in% numeric], others)
}

#' The embedding the trajectories are drawn in: UMAP, else t-SNE, else the first
#' embedding that is not a PCA (a PCA is never a useful backdrop).
#' @keywords internal
#' @noRd
.h5adTrajectoryEmbedding <- function(embeddings) {
  if (!length(embeddings)) {
    return(NULL)
  }
  names_upper <- toupper(names(embeddings))
  pick <- match(c("UMAP", "TSNE"), names_upper)
  pick <- pick[!is.na(pick)]
  if (!length(pick)) {
    pick <- which(
      names_upper != "PCA" & !grepl("^VELOCITY", names_upper)
    )
  }
  if (!length(pick)) {
    return(NULL)
  }
  list(name = names(embeddings)[[pick[[1L]]]], xy = embeddings[[pick[[1L]]]][, 1:2, drop = FALSE])
}

#' The `state` of every cell: the PAGA grouping if there is one, otherwise the
#' first usual cluster column, otherwise a single level.
#' @keywords internal
#' @noRd
.h5adTrajectoryState <- function(columns, groupby, n_cells) {
  candidates <- c(groupby, "leiden", "louvain", "clusters", "cell_type", "celltype")
  candidates <- candidates[!is.na(candidates) & candidates %in% names(columns)]
  for (column in candidates) {
    value <- columns[[column]]
    if (is.factor(value) || is.character(value)) {
      return(droplevels(factor(value)))
    }
  }
  factor(rep("cells", n_cells))
}

#' @keywords internal
#' @noRd
.h5adTrajectoryMeta <- function(xy, pseudotime, state, cells) {
  pseudotime <- as.numeric(pseudotime)
  pseudotime[!is.finite(pseudotime)] <- NA_real_
  data.frame(
    DR_1 = as.numeric(xy[, 1L]),
    DR_2 = as.numeric(xy[, 2L]),
    pseudotime = pseudotime,
    state = state,
    row.names = cells,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.h5adTrajectoryEdges <- function(
  source = character(),
  target = character(),
  weight = numeric(),
  from = matrix(numeric(), 0L, 2L),
  to = matrix(numeric(), 0L, 2L)
) {
  data.frame(
    source = as.character(source),
    target = as.character(target),
    weight = as.numeric(weight),
    source_dim_1 = as.numeric(from[, 1L]),
    source_dim_2 = as.numeric(from[, 2L]),
    target_dim_1 = as.numeric(to[, 1L]),
    target_dim_2 = as.numeric(to[, 2L]),
    stringsAsFactors = FALSE
  )
}

#' PAGA graph (uns["paga"]) as edges between group centroids.
#'
#' @return NULL when the file has no usable PAGA, else list(groupby, labels,
#'   edges).
#' @keywords internal
#' @noRd
.h5adReadPaga <- function(
  ctx,
  columns,
  xy,
  threshold = 0.01,
  verbose = FALSE
) {
  if (!.h5adIsGroup(ctx, "uns/paga")) {
    return(NULL)
  }
  skip <- function(why) {
    if (verbose) {
      message("[INFO] Skipping uns[\"paga\"]: ", why)
    }
    NULL
  }
  if (!all(.h5adHas(ctx, c("uns/paga/groups", "uns/paga/connectivities")))) {
    return(skip("it has no `groups` and `connectivities`."))
  }
  groupby <- as.character(.h5adVector(.h5adRead(ctx, "uns/paga/groups")))[[1L]]
  if (!groupby %in% names(columns)) {
    return(skip(paste0("its grouping `", groupby, "` is not an obs column.")))
  }
  groups <- columns[[groupby]]
  labels <- if (is.factor(groups)) levels(groups) else sort(unique(as.character(groups)))
  n <- length(labels)
  connectivities <- tryCatch(
    as.matrix(.h5adReadMatrix(ctx, "uns/paga/connectivities", n, n)),
    error = function(e) NULL
  )
  if (is.null(connectivities)) {
    return(skip(paste0("`connectivities` is not ", n, " x ", n, ".")))
  }
  ## symmetric in scanpy; a stored transpose makes no difference
  connectivities <- pmax(connectivities, t(connectivities))
  centroids <- do.call(
    rbind,
    lapply(labels, function(label) {
      colMeans(xy[as.character(groups) %in% label, , drop = FALSE], na.rm = TRUE)
    })
  )
  hit <- which(
    upper.tri(connectivities) & connectivities >= threshold,
    arr.ind = TRUE
  )
  hit <- hit[order(hit[, 1L], hit[, 2L]), , drop = FALSE]
  list(
    groupby = groupby,
    labels = labels,
    edges = .h5adTrajectoryEdges(
      labels[hit[, 1L]],
      labels[hit[, 2L]],
      connectivities[hit],
      centroids[hit[, 1L], , drop = FALSE],
      centroids[hit[, 2L], , drop = FALSE]
    )
  )
}

#' scVelo's velocity field (obsm["velocity_<basis>"]) as arrows on a grid.
#'
#' The cells are binned on an `n_grid` x `n_grid` grid (25 x 25 for large data
#' sets, coarser for small ones so that a bin holds about three cells), the
#' velocity averaged per bin that holds at least `min_cells` cells, and every
#' average drawn from the mean cell position as a shaft plus two barbs. Lengths are scaled so that
#' a typical (95th percentile) arrow spans one and a half bins, long enough to
#' show between the cells the Viewer draws over the trajectory.
#'
#' @keywords internal
#' @noRd
.h5adVelocityArrows <- function(
  xy,
  velocity,
  n_grid = max(5L, min(25L, as.integer(round(sqrt(nrow(xy) / 3))))),
  min_cells = 3L
) {
  ok <- is.finite(xy[, 1L]) &
    is.finite(xy[, 2L]) &
    is.finite(velocity[, 1L]) &
    is.finite(velocity[, 2L])
  if (sum(ok) < min_cells) {
    return(.h5adTrajectoryEdges())
  }
  xy <- xy[ok, , drop = FALSE]
  velocity <- velocity[ok, , drop = FALSE]
  bin <- function(x) {
    span <- diff(range(x))
    if (span == 0) {
      return(rep(1L, length(x)))
    }
    pmin(n_grid, 1L + as.integer(floor((x - min(x)) / span * n_grid)))
  }
  key <- bin(xy[, 1L]) + (bin(xy[, 2L]) - 1L) * n_grid
  mean_by <- function(x) as.numeric(tapply(x, key, mean))
  counts <- as.integer(table(key))
  tail <- cbind(mean_by(xy[, 1L]), mean_by(xy[, 2L]))
  field <- cbind(mean_by(velocity[, 1L]), mean_by(velocity[, 2L]))
  keep <- counts >= min_cells
  tail <- tail[keep, , drop = FALSE]
  field <- field[keep, , drop = FALSE]
  speed <- sqrt(rowSums(field^2))
  keep <- speed > 0
  if (!any(keep)) {
    return(.h5adTrajectoryEdges())
  }
  tail <- tail[keep, , drop = FALSE]
  field <- field[keep, , drop = FALSE]
  speed <- speed[keep]

  cell_size <- min(diff(range(xy[, 1L])), diff(range(xy[, 2L]))) / n_grid
  scale <- 1.5 * cell_size / unname(stats::quantile(speed, 0.95))
  length_out <- pmin(speed * scale, 2.5 * cell_size)
  direction <- field / speed
  tip <- tail + direction * length_out
  head_length <- 0.3 * length_out
  rotate <- function(d, angle) {
    cbind(
      d[, 1L] * cos(angle) - d[, 2L] * sin(angle),
      d[, 1L] * sin(angle) + d[, 2L] * cos(angle)
    )
  }
  ## barbs point back from the tip, +-25 degrees off the shaft
  barb_1 <- tip - rotate(direction, pi / 7) * head_length
  barb_2 <- tip - rotate(direction, -pi / 7) * head_length

  id <- sprintf("v%03d", seq_len(nrow(tail)))
  .h5adTrajectoryEdges(
    c(id, paste0(id, "_tip"), paste0(id, "_tip")),
    c(paste0(id, "_tip"), paste0(id, "_barb1"), paste0(id, "_barb2")),
    c(speed, speed, speed),
    rbind(tail, tip, tip),
    rbind(tip, barb_1, barb_2)
  )
}

#' Gather the trajectories an AnnData file carries (see the header above).
#'
#' @param ctx h5ad context.
#' @param obs result of `.h5adReadDataFrame(ctx, "obs")`.
#' @param embeddings named list of cells x dims matrices (the reducedDims).
#' @return Named list by method, each a named list of `list(meta, edges)`;
#'   empty when there is nothing to show.
#' @keywords internal
#' @noRd
.h5adReadTrajectories <- function(ctx, obs, embeddings, verbose = FALSE) {
  columns <- obs$columns
  cells <- obs$index
  pseudotimes <- .h5adPseudotimeColumns(columns)
  has_velocity <- any(startsWith(.h5adChildren(ctx, "obsm"), "velocity_"))
  has_paga <- .h5adIsGroup(ctx, "uns/paga")
  if (!length(pseudotimes)) {
    if (verbose && (has_paga || has_velocity)) {
      message(
        "[INFO] Not importing uns[\"paga\"] / velocity: the Trajectory page ",
        "needs a pseudotime obs column (e.g. dpt_pseudotime, latent_time)."
      )
    }
    return(list())
  }
  base <- .h5adTrajectoryEmbedding(embeddings)
  if (is.null(base)) {
    if (verbose) {
      message("[INFO] Not importing trajectories: the file has no UMAP/t-SNE embedding to draw them on.")
    }
    return(list())
  }

  trajectories <- list()
  add <- function(method, name, state, edges, pseudotime, xy = base$xy) {
    trajectories[[method]][[name]] <<- list(
      meta = .h5adTrajectoryMeta(xy, columns[[pseudotime]], state, cells),
      edges = edges
    )
  }

  paga <- if (has_paga) {
    .h5adReadPaga(ctx, columns, base$xy, verbose = verbose)
  }
  if (!is.null(paga)) {
    state <- .h5adTrajectoryState(columns, paga$groupby, length(cells))
    for (pseudotime in pseudotimes) {
      add("paga", paste0(paga$groupby, " (", pseudotime, ")"), state, paga$edges, pseudotime)
    }
  }

  if (has_velocity) {
    state <- .h5adTrajectoryState(columns, if (!is.null(paga)) paga$groupby, length(cells))
    for (entry in grep("^velocity_", .h5adChildren(ctx, "obsm"), value = TRUE)) {
      basis <- .h5adReductionName(sub("^velocity_", "", entry))
      path <- paste0("obsm/", entry)
      embedding <- embeddings[[basis]]
      raw <- tryCatch(
        if (.h5adIsGroup(ctx, path)) NULL else t(.h5adRead(ctx, path)),
        error = function(e) NULL
      )
      if (
        is.null(embedding) ||
          !is.matrix(raw) ||
          !identical(dim(raw), c(length(cells), ncol(raw))) ||
          ncol(raw) < 2L
      ) {
        if (verbose) {
          message("[INFO] Skipping obsm[\"", entry, "\"]: no cells x 2 array, or no `", basis, "` embedding to match.")
        }
        next
      }
      arrows <- .h5adVelocityArrows(embedding[, 1:2, drop = FALSE], raw[, 1:2, drop = FALSE])
      for (pseudotime in pseudotimes) {
        add("scvelo", paste0(entry, " (", pseudotime, ")"), state, arrows, pseudotime, embedding[, 1:2, drop = FALSE])
      }
    }
  }

  if (!length(trajectories)) {
    state <- .h5adTrajectoryState(columns, NULL, length(cells))
    for (pseudotime in pseudotimes) {
      add("pseudotime", pseudotime, state, .h5adTrajectoryEdges(), pseudotime)
    }
  }
  trajectories
}
