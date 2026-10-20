# Keeps a random subset of the cells of the shipped example data sets, in place,
# without rebuilding them from their (large) sources: `targets` below gives the
# number of cells each file keeps. Everything that is indexed by cell is subset
# consistently: expression, metadata, projections, the spatial coordinates and
# expression of every entry, the Trekker per-nucleus vectors and the immune
# repertoire. Group levels that lose all their cells are dropped. Analyses and
# images are kept. A file that already has no more cells than its target is left
# untouched, so the script can be run again and again with the same result. Run
# from the package root:
#   Rscript data-raw/subsample_demos.R
devtools::load_all(".", quiet = TRUE)
source("data-raw/demo_gene_panel.R")

## A trajectory lists the cells it covers in the row names of `meta`; only the
## cells that are kept stay in it (the graph edges describe states, not cells).
demo_subset_trajectories <- function(trajectories, kept) {
  lapply(trajectories, function(method) {
    lapply(method, function(trajectory) {
      if (is.list(trajectory) && is.data.frame(trajectory$meta)) {
        trajectory$meta <- trajectory$meta[
          rownames(trajectory$meta) %in% kept, , drop = FALSE
        ]
      }
      trajectory
    })
  })
}

demo_subsample_cells <- function(object, n_keep, seed) {
  cells <- colnames(object$expression)
  stopifnot(
    identical(cells, as.character(object$meta_data$cell_barcode)),
    !anyDuplicated(cells)
  )
  set.seed(seed)
  keep <- sort(sample.int(length(cells), n_keep))
  kept <- cells[keep]

  result <- object$clone(deep = TRUE)
  result$expression <- object$expression[, keep, drop = FALSE]
  metadata <- object$meta_data[keep, , drop = FALSE]
  for (group in names(object$groups)) {
    if (is.factor(metadata[[group]])) {
      metadata[[group]] <- droplevels(metadata[[group]])
      result$groups[[group]] <- levels(metadata[[group]])
    } else {
      result$groups[[group]] <- intersect(object$groups[[group]], unique(metadata[[group]]))
    }
  }
  result$meta_data <- metadata
  result$projections <- lapply(
    object$projections,
    function(projection) projection[rownames(projection) %in% kept, , drop = FALSE]
  )
  for (name in names(object$spatial)) {
    entry <- object$spatial[[name]]
    entry$coordinates <- entry$coordinates[rownames(entry$coordinates) %in% kept, , drop = FALSE]
    if (!is.null(entry$expression)) {
      entry$expression <- entry$expression[, colnames(entry$expression) %in% kept, drop = FALSE]
    }
    result$spatial[[name]] <- entry
  }
  result$trajectories <- demo_subset_trajectories(object$trajectories, kept)
  if (length(object$immune_repertoire)) {
    repertoire <- lapply(
      object$immune_repertoire,
      function(table) table[table$barcode %in% kept, , drop = FALSE]
    )
    result$immune_repertoire <- repertoire[vapply(repertoire, nrow, integer(1)) > 0L]
  }
  if (is.list(object$trekker)) {
    n <- length(cells)
    subset_nuclei <- function(x) {
      if (is.data.frame(x) && nrow(x) == n) {
        x[keep, , drop = FALSE]
      } else if (is.atomic(x) && length(x) == n) {
        x[keep]
      } else if (is.list(x) && !is.null(names(x)) && all(cells %in% names(x))) {
        x[kept]
      } else if (is.list(x)) {
        lapply(x, subset_nuclei)
      } else {
        x
      }
    }
    trekker <- object$trekker
    for (field in setdiff(names(trekker), c("meta", "qc", "moran", "celltype"))) {
      trekker[[field]] <- subset_nuclei(trekker[[field]])
    }
    trekker$meta$n_cells <- length(kept)
    result$trekker <- trekker
  }
  result$parameters$demo_cell_subsample <- list(
    n_keep = n_keep,
    seed = seed,
    input_cells = length(cells),
    retained_cells = length(kept),
    scope = "Random cells; analyses, images and full-input QC retained"
  )
  result
}

targets <- c(
  demo_hla_tcr_dextramer = 2000L,
  demo_spatial_merfish = 1000L,
  demo_trekker = 1000L,
  demo_spatial_xenium = 2000L,
  demo_spatial_visium = 1000L,
  example = 1000L,
  demo_full_tcr_bcr = 1000L,
  demo_spatial_slideseq = 3000L
)
for (i in seq_along(targets)) {
  name <- names(targets)[[i]]
  file <- file.path("inst/extdata/examples", paste0(name, ".crb"))
  object <- readCerebro(file)
  before <- ncol(object$expression)
  if (before <= targets[[i]]) {
    message(name, ": ", before, " cells, unchanged")
    next
  }
  save_demo_crb(demo_subsample_cells(object, targets[[i]], seed = 20261020L + i), file)
  message(name, ": ", before, " -> ", ncol(readCerebro(file)$expression), " cells, ", file.size(file), " bytes")
}
