.viewer1mCacheDir <- function(path = NULL) {
  if (!is.null(path)) {
    if (
      !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
    ) {
      stop("CEREBRO_LARGE_CACHE must be one non-empty path.", call. = FALSE)
    }
    return(path)
  }
  file.path(tools::R_user_dir("CerebroNexus", "cache"), "large-examples")
}

.viewer1mPaths <- function(cache_dir = NULL) {
  root <- file.path(.viewer1mCacheDir(cache_dir), "1m")
  list(
    source = file.path(
      root,
      "source",
      "1M_neurons_filtered_gene_bc_matrices_h5.h5"
    ),
    seurat = file.path(root, "seurat", "mouse_brain_1m.rds"),
    seurat_sidecar = file.path(root, "seurat", "mouse_brain_1m.bpcells"),
    crb = file.path(root, "cerebro", "cerebro_mouse_brain_1m.crb"),
    crb_sidecar = file.path(root, "cerebro", "cerebro_mouse_brain_1m.bpcells")
  )
}

.viewer1mComplete <- function(file, sidecar) {
  file.exists(file) &&
    !dir.exists(file) &&
    dir.exists(sidecar) &&
    length(list.files(sidecar, all.files = TRUE, no.. = TRUE)) > 0L
}

.viewer1mRequire <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "The 1M benchmark requires: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

.viewer1mDownload <- function(dest) {
  if (file.exists(dest) && !dir.exists(dest)) {
    return(dest)
  }
  url <- paste0(
    "https://cf.10xgenomics.com/samples/cell-exp/1.3.0/1M_neurons/",
    "1M_neurons_filtered_gene_bc_matrices_h5.h5"
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  partial <- tempfile(paste0(basename(dest), ".part-"), dirname(dest))
  on.exit(unlink(partial, force = TRUE), add = TRUE)
  old_options <- options(timeout = max(getOption("timeout"), 86400))
  on.exit(options(old_options), add = TRUE)
  utils::download.file(url, partial, mode = "wb", method = "libcurl")
  if (!file.exists(partial) || !file.rename(partial, dest)) {
    stop("Could not publish the downloaded 1M source matrix.", call. = FALSE)
  }
  dest
}

.viewer1mUniqueGeneSymbols <- function(symbols, n_genes) {
  symbols <- as.character(symbols)
  if (
    length(symbols) != n_genes ||
      anyNA(symbols) ||
      any(!nzchar(symbols))
  ) {
    stop(
      "The 10x source must contain one non-empty gene symbol per row.",
      call. = FALSE
    )
  }
  make.unique(symbols)
}

.viewer1mIllustrativeTrajectory <- function(metadata, projection) {
  required_metadata <- c("cell_barcode", "seurat_clusters")
  if (
    !is.data.frame(metadata) ||
      !all(required_metadata %in% colnames(metadata)) ||
      is.null(dim(projection)) ||
      ncol(projection) < 2L
  ) {
    stop(
      "The 1M demo trajectory requires cell barcodes, clusters, and a 2-D projection.",
      call. = FALSE
    )
  }

  cells <- as.character(metadata$cell_barcode)
  projection <- as.matrix(projection[, seq_len(2L), drop = FALSE])
  projection_cells <- rownames(projection)
  if (!is.null(projection_cells)) {
    projection <- projection[match(cells, projection_cells), , drop = FALSE]
  } else if (nrow(projection) != length(cells)) {
    stop("The 1M demo projection does not match its metadata.", call. = FALSE)
  }

  clusters <- as.character(metadata$seurat_clusters)
  coordinates <- data.frame(
    DR_1 = as.numeric(projection[, 1L]),
    DR_2 = as.numeric(projection[, 2L])
  )
  keep <- !is.na(cells) &
    nzchar(cells) &
    !is.na(clusters) &
    nzchar(clusters) &
    stats::complete.cases(coordinates)
  if (sum(keep) < 2L) {
    stop(
      "The 1M demo trajectory needs at least two valid cells.",
      call. = FALSE
    )
  }

  coordinates <- coordinates[keep, , drop = FALSE]
  cells <- cells[keep]
  clusters <- clusters[keep]
  axis_range <- range(coordinates$DR_1)
  pseudotime <- if (diff(axis_range) > 0) {
    (coordinates$DR_1 - axis_range[[1L]]) / diff(axis_range)
  } else {
    rep(0, nrow(coordinates))
  }
  state_levels <- sort(unique(clusters), method = "radix")
  trajectory_meta <- data.frame(
    DR_1 = coordinates$DR_1,
    DR_2 = coordinates$DR_2,
    pseudotime = pseudotime,
    state = factor(clusters, levels = state_levels),
    row.names = cells
  )

  centers <- stats::aggregate(
    trajectory_meta[c("DR_1", "DR_2")],
    list(state = trajectory_meta$state),
    mean
  )
  centers <- centers[order(centers$DR_1, centers$DR_2, centers$state), ]
  trajectory_edges <- data.frame(
    source_dim_1 = utils::head(centers$DR_1, -1L),
    source_dim_2 = utils::head(centers$DR_2, -1L),
    target_dim_1 = utils::tail(centers$DR_1, -1L),
    target_dim_2 = utils::tail(centers$DR_2, -1L)
  )

  list(meta = trajectory_meta, edges = trajectory_edges)
}

.viewer1mEnrichDemoObject <- function(object) {
  changed <- FALSE
  parameters <- tryCatch(object$getParameters(), error = function(e) list())
  if (!identical(parameters[["main_group"]], "seurat_clusters")) {
    object$addParameters("main_group", "seurat_clusters")
    changed <- TRUE
  }

  method <- "illustrative"
  trajectory_name <- "UMAP_cluster_path"
  methods <- tryCatch(
    object$getMethodsForTrajectories(),
    error = function(e) character()
  )
  names_for_method <- if (method %in% methods) {
    tryCatch(
      object$getNamesOfTrajectories(method),
      error = function(e) character()
    )
  } else {
    character()
  }
  if (!trajectory_name %in% names_for_method) {
    projection_names <- object$availableProjections()
    if (!length(projection_names)) {
      stop("The 1M demo needs a projection for Linked Views.", call. = FALSE)
    }
    object$addTrajectory(
      method,
      trajectory_name,
      .viewer1mIllustrativeTrajectory(
        object$getMetaData(),
        object$getProjection(projection_names[[1L]])
      )
    )
    changed <- TRUE
  }
  changed
}

.viewer1mEnrichDemoCrb <- function(file) {
  object <- readCerebro(file)
  if (.viewer1mEnrichDemoObject(object)) {
    message(
      "Adding the 1M Linked Views trajectory and seurat_clusters default."
    )
    saveCerebro(object, file)
  }
  file
}

.viewer1mGeneSymbols <- function(source, n_genes) {
  listing <- rhdf5::h5ls(source, recursive = TRUE)
  hit <- which(listing$name == "gene_names" & listing$otype == "H5I_DATASET")
  if (length(hit) != 1L) {
    stop(
      "The 10x source must contain exactly one gene_names dataset.",
      call. = FALSE
    )
  }
  dataset <- file.path(listing$group[hit], listing$name[hit])
  .viewer1mUniqueGeneSymbols(rhdf5::h5read(source, dataset), n_genes)
}

.viewer1mAnalyze <- function(object) {
  previous_future_size <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = max(previous_future_size, 4e9))
  on.exit(options(future.globals.maxSize = previous_future_size), add = TRUE)

  object <- Seurat::NormalizeData(object, verbose = TRUE)
  object <- Seurat::FindVariableFeatures(object, verbose = TRUE)
  object <- Seurat::SketchData(
    object,
    ncells = 50000L,
    method = "LeverageScore",
    sketched.assay = "sketch",
    seed = 123L,
    verbose = TRUE
  )
  SeuratObject::DefaultAssay(object) <- "sketch"
  object <- Seurat::FindVariableFeatures(object, verbose = TRUE)
  object <- Seurat::ScaleData(object, verbose = TRUE)
  object <- Seurat::RunPCA(object, npcs = 30L, seed.use = 123L, verbose = TRUE)
  object <- Seurat::FindNeighbors(object, dims = seq_len(30L), verbose = TRUE)
  object <- Seurat::FindClusters(
    object,
    resolution = 1,
    random.seed = 123L,
    verbose = TRUE
  )
  object <- Seurat::RunUMAP(
    object,
    dims = seq_len(30L),
    return.model = TRUE,
    seed.use = 123L,
    verbose = TRUE
  )
  Seurat::ProjectData(
    object,
    assay = "RNA",
    full.reduction = "pca.full",
    sketched.assay = "sketch",
    sketched.reduction = "pca",
    umap.model = "umap",
    dims = seq_len(30L),
    refdata = list(cluster_full = "seurat_clusters"),
    verbose = TRUE
  )
}

.viewer1mBuildSeurat <- function(paths) {
  matrix <- BPCells::open_matrix_10x_hdf5(paths$source)
  matrix_cells <- colnames(matrix)
  if (anyDuplicated(matrix_cells) || length(matrix_cells) < 1000000L) {
    stop("The 10x source must contain at least 1M unique cells.", call. = FALSE)
  }
  gene_symbols <- .viewer1mGeneSymbols(paths$source, nrow(matrix))
  selected_cells <- matrix_cells[
    floor(seq(0, length(matrix_cells) - 1, length.out = 1000000L)) + 1L
  ]
  selected_matrix <- matrix[, selected_cells]
  rownames(selected_matrix) <- gene_symbols
  if (!inherits(selected_matrix, "IterableMatrix")) {
    selected_matrix <- methods::as(selected_matrix, "IterableMatrix")
  }
  selected_matrix <- BPCells::convert_matrix_type(
    selected_matrix,
    type = "uint32_t"
  )

  dir.create(dirname(paths$seurat), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(paths$seurat_sidecar)) {
    unlink(paths$seurat_sidecar, recursive = TRUE, force = TRUE)
  }
  BPCells::write_matrix_dir(selected_matrix, dir = paths$seurat_sidecar)
  counts <- BPCells::open_matrix_dir(paths$seurat_sidecar)
  object <- SeuratObject::CreateSeuratObject(
    counts = counts,
    meta.data = data.frame(
      sample = rep("10x E18 mouse brain (1M subset)", 1000000L),
      row.names = selected_cells
    ),
    assay = "RNA",
    project = "mouse_brain_1m"
  )
  analyzed <- .viewer1mAnalyze(object)
  embedding <- SeuratObject::Embeddings(analyzed, "full.umap")
  metadata <- analyzed[[]]
  metadata <- data.frame(
    sample = metadata[selected_cells, "sample"],
    seurat_clusters = as.character(metadata[selected_cells, "cluster_full"]),
    row.names = selected_cells,
    check.names = FALSE
  )
  object <- SeuratObject::CreateSeuratObject(
    counts = BPCells::open_matrix_dir(paths$seurat_sidecar),
    meta.data = metadata,
    assay = "RNA",
    project = "mouse_brain_1m"
  )
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embedding[selected_cells, , drop = FALSE],
    key = "UMAP_",
    assay = "RNA"
  )
  saveRDS(object, paths$seurat, version = 3)
}

prepareViewer1mBenchmarkData <- function(cache_dir = NULL) {
  paths <- .viewer1mPaths(cache_dir)
  .viewer1mRequire("BPCells")
  if (.viewer1mComplete(paths$crb, paths$crb_sidecar)) {
    return(.viewer1mEnrichDemoCrb(paths$crb))
  }

  .viewer1mRequire(c("Seurat", "SeuratObject", "rhdf5"))
  if (!.viewer1mComplete(paths$seurat, paths$seurat_sidecar)) {
    message("Preparing the 1M benchmark data from official 10x data.")
    .viewer1mDownload(paths$source)
    .viewer1mBuildSeurat(paths)
  }

  dir.create(dirname(paths$crb), recursive = TRUE, showWarnings = FALSE)
  convertSeuratToCerebro(
    seurat_file = paths$seurat,
    result_dir = dirname(paths$crb),
    assay = "RNA",
    slot = "counts",
    experiment_name = "10x E18 mouse brain (1M subset)",
    organism = "Mouse",
    groups = c("sample", "seurat_clusters"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    expression_matrix_mode = "bpcells",
    add_most_expressed_genes = FALSE,
    verbose = TRUE
  )
  if (!.viewer1mComplete(paths$crb, paths$crb_sidecar)) {
    stop("The 1M benchmark CRB is incomplete.", call. = FALSE)
  }
  .viewer1mEnrichDemoCrb(paths$crb)
}
