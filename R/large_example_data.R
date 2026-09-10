.largeExampleSpec <- function(size) {
  if (is.null(size)) {
    return(NULL)
  }
  if (
    !is.character(size) ||
      length(size) != 1L ||
      is.na(size) ||
      !tolower(size) %in% c("50k", "1m")
  ) {
    stop(
      "'example_data_size' must be one of NULL, '50k', or '1m'.",
      call. = FALSE
    )
  }

  size <- tolower(size)
  base <- "https://cf.10xgenomics.com/samples/cell-exp"
  if (identical(size, "50k")) {
    stem <- "fresh_68k_pbmc_donor_a"
    root <- paste(base, "1.1.0", stem, sep = "/")
    return(list(
      size = size,
      slug = "pbmc_50k",
      n_cells = 50000L,
      label = "50K PBMC example",
      experiment = "10x Fresh 68k PBMC Donor A (50K subset)",
      organism = "Human",
      matrix = list(
        format = "mex",
        url = paste0(root, "/", stem, "_filtered_gene_bc_matrices.tar.gz")
      )
    ))
  }

  stem <- "1M_neurons"
  root <- paste(base, "1.3.0", stem, sep = "/")
  list(
    size = size,
    slug = "mouse_brain_1m",
    n_cells = 1000000L,
    label = "1M mouse brain example",
    experiment = "10x E18 mouse brain (1M subset)",
    organism = "Mouse",
    matrix = list(
      format = "h5",
      url = paste0(root, "/", stem, "_filtered_gene_bc_matrices_h5.h5")
    )
  )
}

.largeExampleCacheDir <- function(path) {
  if (!is.null(path)) {
    if (
      !is.character(path) ||
        length(path) != 1L ||
        is.na(path) ||
        !nzchar(path)
    ) {
      stop("'example_data_dir' must be one non-empty path.", call. = FALSE)
    }
    return(path)
  }

  file.path(tools::R_user_dir("CerebroNexus", "cache"), "large-examples")
}

.requireLargeExamplePackages <- function(
  packages = c("Seurat", "SeuratObject", "BPCells"),
  available = requireNamespace
) {
  missing <- packages[
    !vapply(
      packages,
      function(package) available(package, quietly = TRUE),
      logical(1)
    )
  ]
  if (length(missing)) {
    stop(
      "Large examples require these packages before data are downloaded: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.downloadLargeExample <- function(
  url,
  dest,
  downloader = function(source, target) {
    utils::download.file(
      source,
      destfile = target,
      mode = "wb",
      quiet = FALSE,
      method = "libcurl"
    )
  }
) {
  if (file.exists(dest) && !dir.exists(dest)) {
    return(dest)
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  partial <- tempfile(
    pattern = paste0(basename(dest), ".part-"),
    tmpdir = dirname(dest)
  )
  on.exit(unlink(partial, recursive = TRUE, force = TRUE), add = TRUE)

  old_options <- options(timeout = max(getOption("timeout"), 86400))
  on.exit(options(old_options), add = TRUE)
  downloader(url, partial)
  if (!file.exists(partial) || dir.exists(partial)) {
    stop("Large example download did not create a file.", call. = FALSE)
  }
  if (!file.rename(partial, dest)) {
    stop("Could not publish the downloaded large example file.", call. = FALSE)
  }
  dest
}

.largeExamplePaths <- function(spec, cache_dir) {
  root <- file.path(.largeExampleCacheDir(cache_dir), spec$size)
  list(
    root = root,
    matrix = file.path(root, "source", basename(spec$matrix$url)),
    matrix_extract = file.path(root, "source", "matrix"),
    seurat = file.path(root, "seurat", paste0(spec$slug, ".rds")),
    seurat_sidecar = file.path(
      root,
      "seurat",
      paste0(spec$slug, ".bpcells")
    ),
    crb = file.path(root, "cerebro", paste0("cerebro_", spec$slug, ".crb")),
    crb_sidecar = file.path(
      root,
      "cerebro",
      paste0("cerebro_", spec$slug, ".bpcells")
    )
  )
}

.completeLargeExampleArtifact <- function(file, sidecar) {
  file.exists(file) && !dir.exists(file) && dir.exists(sidecar)
}

.extractLargeExampleArchive <- function(archive, exdir) {
  if (
    dir.exists(exdir) &&
      length(list.files(exdir, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  ) {
    return(exdir)
  }
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::untar(archive, exdir = exdir)
  exdir
}

.acquireLargeExample <- function(
  spec,
  paths,
  download = .downloadLargeExample,
  extract = .extractLargeExampleArchive
) {
  message(
    "Preparing the ",
    spec$label,
    " from official 10x Genomics data."
  )
  download(spec$matrix$url, paths$matrix)
  if (identical(spec$matrix$format, "mex")) {
    extract(paths$matrix, paths$matrix_extract)
  }
  invisible(paths)
}

.findLargeExampleFile <- function(root, pattern, label) {
  matches <- list.files(
    root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(matches) != 1L) {
    stop(
      "Expected exactly one ",
      label,
      " in the extracted large example data; found ",
      length(matches),
      ".",
      call. = FALSE
    )
  }
  matches
}

.readLargeExampleMatrix <- function(spec, paths) {
  if (identical(spec$matrix$format, "h5")) {
    return(BPCells::open_matrix_10x_hdf5(paths$matrix))
  }
  matrix_file <- .findLargeExampleFile(
    paths$matrix_extract,
    "^matrix[.]mtx([.]gz)?$",
    "10x matrix.mtx"
  )
  matrix <- Seurat::Read10X(dirname(matrix_file))
  if (is.list(matrix)) {
    if (length(matrix) != 1L) {
      stop(
        "The 50K source contains more than one expression matrix.",
        call. = FALSE
      )
    }
    matrix <- matrix[[1L]]
  }
  matrix
}

.selectLargeExampleCells <- function(matrix_cells, n_cells) {
  if (anyDuplicated(matrix_cells)) {
    stop("Large example cell barcodes must be unique.", call. = FALSE)
  }
  if (length(matrix_cells) < n_cells) {
    stop(
      "The source matrix has ",
      length(matrix_cells),
      " cells; ",
      n_cells,
      " are required.",
      call. = FALSE
    )
  }
  indices <- floor(seq(0, length(matrix_cells) - 1, length.out = n_cells)) + 1L
  matrix_cells[indices]
}

.analyzeLargeExample <- function(object) {
  previous_future_size <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = max(previous_future_size, 4e9))
  on.exit(
    options(future.globals.maxSize = previous_future_size),
    add = TRUE
  )

  sketch_cells <- min(50000L, max(5000L, ceiling(ncol(object) * 0.1)))
  object <- Seurat::NormalizeData(object, verbose = TRUE)
  object <- Seurat::FindVariableFeatures(object, verbose = TRUE)
  object <- Seurat::SketchData(
    object,
    ncells = sketch_cells,
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

.buildLargeExampleSeurat <- function(
  spec,
  paths,
  read_matrix = .readLargeExampleMatrix,
  analyze = .analyzeLargeExample
) {
  matrix <- read_matrix(spec, paths)
  selected_cells <- .selectLargeExampleCells(colnames(matrix), spec$n_cells)
  selected_matrix <- matrix[, selected_cells]

  dir.create(dirname(paths$seurat), recursive = TRUE, showWarnings = FALSE)
  if (!inherits(selected_matrix, "IterableMatrix")) {
    selected_matrix <- methods::as(selected_matrix, "IterableMatrix")
  }
  selected_matrix <- BPCells::convert_matrix_type(
    selected_matrix,
    type = "uint32_t"
  )
  if (dir.exists(paths$seurat_sidecar)) {
    unlink(paths$seurat_sidecar, recursive = TRUE, force = TRUE)
  }
  BPCells::write_matrix_dir(mat = selected_matrix, dir = paths$seurat_sidecar)

  counts <- BPCells::open_matrix_dir(paths$seurat_sidecar)
  object <- SeuratObject::CreateSeuratObject(
    counts = counts,
    meta.data = data.frame(
      sample = rep(spec$experiment, spec$n_cells),
      row.names = selected_cells
    ),
    assay = "RNA",
    project = spec$slug
  )
  analyzed <- analyze(object)
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
    project = spec$slug
  )
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embedding[selected_cells, , drop = FALSE],
    key = "UMAP_",
    assay = "RNA"
  )

  saveRDS(object, paths$seurat, version = 3)
  paths$seurat
}

.convertLargeExample <- function(
  spec,
  paths,
  converter = convertSeuratToCerebro
) {
  dir.create(dirname(paths$crb), recursive = TRUE, showWarnings = FALSE)
  converter(
    seurat_file = paths$seurat,
    result_dir = dirname(paths$crb),
    assay = "RNA",
    slot = "counts",
    experiment_name = spec$experiment,
    organism = spec$organism,
    groups = c("sample", "seurat_clusters"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    expression_matrix_mode = "bpcells",
    add_most_expressed_genes = FALSE,
    verbose = TRUE
  )
  paths$crb
}

.prepareLargeExample <- function(
  size,
  cache_dir = NULL,
  acquire = .acquireLargeExample,
  build_seurat = .buildLargeExampleSeurat,
  convert = .convertLargeExample
) {
  spec <- .largeExampleSpec(size)
  if (is.null(spec)) {
    return(NULL)
  }
  paths <- .largeExamplePaths(spec, cache_dir)
  if (.completeLargeExampleArtifact(paths$crb, paths$crb_sidecar)) {
    .requireLargeExamplePackages("BPCells")
    return(stats::setNames(paths$crb, spec$label))
  }

  .requireLargeExamplePackages()
  if (!.completeLargeExampleArtifact(paths$seurat, paths$seurat_sidecar)) {
    acquire(spec, paths)
    build_seurat(spec, paths)
  }
  convert(spec, paths)
  if (!.completeLargeExampleArtifact(paths$crb, paths$crb_sidecar)) {
    stop(
      "Large example conversion did not create a complete CRB.",
      call. = FALSE
    )
  }
  stats::setNames(paths$crb, spec$label)
}
