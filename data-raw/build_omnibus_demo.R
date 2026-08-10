#!/usr/bin/env Rscript

## Build the deterministic synthetic Omnibus Seurat and Cerebro fixtures.
## Run from any directory with:
##   Rscript data-raw/build_omnibus_demo.R

required_packages <- c(
  "base64enc",
  "devtools",
  "ggplot2",
  "Matrix",
  "png",
  "Seurat",
  "SeuratObject"
)
missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]
if (length(missing_packages) > 0L) {
  stop(
    "Install the required build packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_argument) == 1L) {
  sub("^--file=", "", script_argument)
} else {
  "data-raw/build_omnibus_demo.R"
}
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  mustWork = TRUE
)
output_dir <- file.path(project_root, "inst", "extdata", "examples")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
devtools::load_all(project_root, quiet = TRUE)

set.seed(20260810)
n_genes <- 80L
n_cells <- 120L
gene_names <- sprintf("Gene%03d", seq_len(n_genes))
cell_names <- sprintf("Cell%03d", seq_len(n_cells))
cluster <- rep(as.character(0:3), each = 30L)
cell_type <- rep(c("T cell", "B cell", "Myeloid", "Stromal"), each = 30L)
sample_id <- rep(c("donorA", "donorB", "donorC"), each = 40L)
condition <- ifelse(sample_id == "donorB", "Treatment", "Control")

counts <- matrix(
  stats::rpois(n_genes * n_cells, lambda = 1.5),
  nrow = n_genes,
  dimnames = list(gene_names, cell_names)
)
for (cluster_id in 0:3) {
  marker_rows <- seq.int(cluster_id * 10L + 1L, cluster_id * 10L + 10L)
  marker_cells <- which(cluster == as.character(cluster_id))
  counts[marker_rows, marker_cells] <- counts[marker_rows, marker_cells] +
    matrix(
      stats::rpois(length(marker_rows) * length(marker_cells), lambda = 5),
      nrow = length(marker_rows)
    )
}
counts <- methods::as(counts, "CsparseMatrix")

metadata <- data.frame(
  orig.ident = sample_id,
  seurat_clusters = cluster,
  cell_type = cell_type,
  phase = rep(c("G1", "S", "G2M"), length.out = n_cells),
  treatment = rep(c("Control", "Stimulated"), times = 60L),
  condition = condition,
  stringsAsFactors = FALSE,
  row.names = cell_names
)
object <- SeuratObject::CreateSeuratObject(
  counts = counts,
  assay = "RNA",
  meta.data = metadata,
  project = "SyntheticOmnibus"
)
object <- Seurat::NormalizeData(object, verbose = FALSE)

centres <- matrix(
  c(-4, 2, -1, -3, 3, -2, 4, 3),
  nrow = 4L,
  byrow = TRUE
)
umap <- centres[as.integer(cluster) + 1L, , drop = FALSE] +
  matrix(stats::rnorm(n_cells * 2L, sd = 0.45), ncol = 2L)
rownames(umap) <- cell_names
colnames(umap) <- c("UMAP_1", "UMAP_2")
pca <- matrix(
  stats::rnorm(n_cells * 4L),
  nrow = n_cells,
  dimnames = list(cell_names, paste0("PC_", 1:4))
)
object[["umap"]] <- SeuratObject::CreateDimReducObject(
  embeddings = umap,
  key = "UMAP_",
  assay = "RNA"
)
object[["pca"]] <- SeuratObject::CreateDimReducObject(
  embeddings = pca,
  key = "PC_",
  assay = "RNA"
)

segment_points <- function(from, to, n) {
  fraction <- seq(0, 1, length.out = n + 1L)[seq_len(n)]
  cbind(
    x = from[[1L]] + fraction * (to[[1L]] - from[[1L]]),
    y = from[[2L]] + fraction * (to[[2L]] - from[[2L]])
  )
}

angle <- seq(0, 2 * pi, length.out = 41L)[seq_len(40L)]
donor_a_coordinates <- data.frame(
  x = 400 + 220 * cos(angle),
  y = 300 + 180 * sin(angle),
  cell = cell_names[1:40]
)
donor_b_grid <- expand.grid(
  x = seq(50, 750, length.out = 8L),
  y = seq(100, 500, length.out = 5L)
)
donor_b_coordinates <- data.frame(
  x = donor_b_grid$x,
  y = donor_b_grid$y,
  cell = cell_names[41:80]
)
donor_c_xy <- rbind(
  segment_points(c(100, 100), c(900, 100), 14L),
  segment_points(c(900, 100), c(450, 700), 13L),
  segment_points(c(450, 700), c(100, 100), 13L)
)
donor_c_coordinates <- data.frame(
  x = donor_c_xy[, "x"],
  y = donor_c_xy[, "y"],
  cell = cell_names[81:120]
)
spatial_coordinates <- list(
  `donorA tissue` = donor_a_coordinates,
  `donorB tissue` = donor_b_coordinates,
  `donorC tissue` = donor_c_coordinates
)
for (spatial_index in seq_along(spatial_coordinates)) {
  spatial_name <- names(spatial_coordinates)[[spatial_index]]
  centroids <- SeuratObject::CreateCentroids(
    spatial_coordinates[[spatial_name]]
  )
  object[[spatial_name]] <- SeuratObject::CreateFOV(
    coords = list(centroids = centroids),
    type = "centroids",
    assay = "RNA",
    key = paste0("donor", LETTERS[[spatial_index]], "_")
  )
}
## Seurat's public replacement method applies make.names() to FOV names. The
## stored list itself supports display labels, so restore the fixture's exact
## human-readable spatial-entry names after all entries have been inserted.
names(object@images) <- names(spatial_coordinates)

image_array <- function(height, width, kind) {
  x <- matrix(
    rep(seq(0, 1, length.out = width), each = height),
    nrow = height
  )
  y <- matrix(
    rep(seq(0, 1, length.out = height), width),
    nrow = height
  )
  tissue <- exp(-((x - 0.48)^2 / 0.15 + (y - 0.52)^2 / 0.24))
  image <- array(0, dim = c(height, width, 3L))
  if (identical(kind, "he")) {
    image[,, 1L] <- 0.18 + 0.72 * tissue
    image[,, 2L] <- 0.12 + 0.48 * tissue + 0.12 * x
    image[,, 3L] <- 0.28 + 0.58 * tissue + 0.08 * y
  } else if (identical(kind, "dapi")) {
    nuclei <- pmax(0, sin(x * 20) * cos(y * 24)) * tissue
    image[,, 1L] <- 0.03 + 0.08 * tissue
    image[,, 2L] <- 0.05 + 0.18 * nuclei
    image[,, 3L] <- 0.18 + 0.78 * nuclei
  } else if (identical(kind, "if")) {
    signal <- pmax(0, sin((x + y) * 18)) * tissue
    image[,, 1L] <- 0.05 + 0.78 * signal
    image[,, 2L] <- 0.08 + 0.62 * tissue
    image[,, 3L] <- 0.10 + 0.35 * (1 - signal) * tissue
  } else {
    boundary <- as.numeric(abs(y - (0.18 + 0.58 * x)) < 0.035)
    image[,, 1L] <- 0.72 + 0.20 * tissue
    image[,, 2L] <- 0.68 + 0.16 * tissue - 0.45 * boundary
    image[,, 3L] <- 0.58 + 0.18 * tissue - 0.35 * boundary
  }
  pmin(pmax(image, 0), 1)
}

image_data_uri <- function(image) {
  image_file <- tempfile(fileext = ".png")
  on.exit(unlink(image_file), add = TRUE)
  png::writePNG(image, image_file)
  paste0(
    "data:image/png;base64,",
    base64enc::base64encode(image_file)
  )
}

donor_a_he <- image_array(80L, 100L, "he")
donor_a_dapi <- image_array(80L, 100L, "dapi")
donor_b_he <- image_array(72L, 96L, "he")
donor_b_if <- image_array(72L, 96L, "if")
donor_c_review <- image_array(90L, 110L, "review")
object@misc$cerebro_spatial_images <- list(
  `donorA tissue` = list(
    `H&E` = list(
      histology_image = image_data_uri(donor_a_he),
      histology_image_bounds = c(
        xmin = 100,
        xmax = 700,
        ymin = 50,
        ymax = 550
      )
    ),
    DAPI = list(
      histology_image = image_data_uri(donor_a_dapi),
      histology_image_bounds = c(
        xmin = 100,
        xmax = 700,
        ymin = 50,
        ymax = 550
      )
    )
  ),
  `donorB tissue` = list(
    `H&E` = list(
      histology_image = image_data_uri(donor_b_he),
      histology_image_bounds = c(
        xmin = 0,
        xmax = 800,
        ymin = 50,
        ymax = 550
      )
    )
  ),
  `donorC tissue` = list()
)

object@misc$experiment <- list(date_of_analysis = as.Date("2026-08-10"))
object@misc$parameters <- list(
  fixture_seed = 20260810L,
  fixture_type = "synthetic_omnibus",
  main_group = "cell_type"
)
object@misc$technical_info <- list(
  generator = "data-raw/build_omnibus_demo.R",
  dimensions = sprintf("%d genes x %d cells", n_genes, n_cells)
)
object@misc$gene_lists <- list(
  synthetic_signature_A = gene_names[1:8],
  synthetic_signature_B = gene_names[21:28],
  mitochondrial = gene_names[77:80]
)

marker_table <- do.call(
  rbind,
  lapply(seq_along(unique(cell_type)), function(i) {
    type <- unique(cell_type)[[i]]
    genes <- gene_names[((i - 1L) * 10L + 1L):(i * 10L)]
    data.frame(
      cluster = type,
      gene = genes,
      avg_log2FC = seq(2.5, 1.2, length.out = length(genes)),
      pct.1 = seq(0.95, 0.70, length.out = length(genes)),
      pct.2 = seq(0.25, 0.45, length.out = length(genes)),
      p_val_adj = seq(1e-8, 1e-4, length.out = length(genes)),
      on_cell_surface = rep(c(TRUE, FALSE), length.out = length(genes)),
      stringsAsFactors = FALSE
    )
  })
)
object@misc$marker_genes <- list(
  "Synthetic differential expression" = list(cell_type = marker_table)
)
object@misc$most_expressed_genes <- list(
  cell_type = marker_table[, c("cluster", "gene", "pct.1")]
)
colnames(object@misc$most_expressed_genes$cell_type)[3L] <- "pct"
object@misc$mean_expression <- list(
  cell_type = data.frame(
    cluster = marker_table$cluster,
    gene = marker_table$gene,
    mean_expr = seq(4, 1, length.out = nrow(marker_table)),
    stringsAsFactors = FALSE
  )
)
object@misc$enriched_pathways <- list(
  fgsea = list(
    cell_type = data.frame(
      cluster = unique(cell_type),
      pathway = paste("Synthetic pathway", seq_along(unique(cell_type))),
      NES = c(2.1, 1.8, -1.7, 2.4),
      p_val_adj = c(0.001, 0.004, 0.008, 0.002),
      stringsAsFactors = FALSE
    )
  )
)
object@misc$trees <- list(
  cell_type = structure(
    list(
      edge = matrix(
        c(7, 5, 7, 6, 5, 1, 5, 2, 6, 3, 6, 4),
        byrow = TRUE,
        ncol = 2L
      ),
      edge.length = rep(1, 6L),
      tip.label = unique(cell_type),
      Nnode = 3L
    ),
    class = "phylo"
  )
)

trajectory_meta <- data.frame(
  DR_1 = umap[, 1L],
  DR_2 = umap[, 2L],
  pseudotime = rank(umap[, 1L], ties.method = "first") / n_cells,
  state = factor(cluster),
  row.names = cell_names
)
trajectory_edges <- data.frame(
  source = c("state_0", "state_1", "state_2"),
  target = c("state_1", "state_2", "state_3"),
  weight = 1,
  source_dim_1 = centres[1:3, 1L],
  source_dim_2 = centres[1:3, 2L],
  target_dim_1 = centres[2:4, 1L],
  target_dim_2 = centres[2:4, 2L],
  stringsAsFactors = FALSE
)
object@misc$trajectories <- list(
  monocle2 = list(
    synthetic_progression = list(
      meta = trajectory_meta,
      edges = trajectory_edges
    )
  )
)

repertoire_cells <- c(cell_names[1:12], cell_names[61:72])
receptor <- rep(rep(c("TCR", "BCR"), each = 6L), 2L)
repertoire <- data.frame(
  barcode = repertoire_cells,
  CTgene = paste0(
    ifelse(receptor == "TCR", "TRB", "IGH"),
    "_synthetic_",
    seq_along(repertoire_cells)
  ),
  CTnt = paste0("NT", sprintf("%03d", seq_along(repertoire_cells))),
  CTaa = paste0("AA", sprintf("%03d", seq_along(repertoire_cells))),
  CTstrict = paste0("Clone", rep(1:8, each = 3L)),
  receptor = receptor,
  stringsAsFactors = FALSE
)
object@misc$immune_repertoire <- split(
  repertoire,
  object@meta.data[repertoire$barcode, "orig.ident"]
)
object@misc$hla_typing <- list(
  donorA = c(
    "HLA-A*02:01",
    "HLA-B*07:02",
    "HLA-C*07:02",
    "HLA-DRB1*15:01",
    "HLA-DQB1*06:02"
  ),
  donorB = c(
    "HLA-A*01:01",
    "HLA-B*08:01",
    "HLA-C*07:01",
    "HLA-DRB1*03:01",
    "HLA-DQB1*02:01"
  ),
  donorC = c(
    "HLA-A*03:01",
    "HLA-B*15:01",
    "HLA-C*03:04",
    "HLA-DRB1*04:01",
    "HLA-DQB1*03:02"
  )
)
object@misc$hla_typing_source_type <- "synthetic"

summary_table <- as.data.frame(table(cell_type, sample_id))
colnames(summary_table) <- c("cell_type", "sample", "cells")
summary_plot <- ggplot2::ggplot(
  summary_table,
  ggplot2::aes(x = cell_type, y = cells, fill = sample)
) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Synthetic Omnibus composition")
object@misc$extra_material <- list(
  tables = list(composition = summary_table),
  plots = list(composition = summary_plot)
)

object@misc$trekker <- list(
  meta = list(
    n_cells_full = n_cells,
    n_cells = n_cells,
    n_genes_obj = n_genes,
    unit = "synthetic coordinate units",
    coord_source = "deterministic Omnibus spatial entries"
  ),
  qc = list(pct_positioned = 100, status = "synthetic"),
  barcodes = unname(cell_names),
  x = unname(round(unlist(lapply(spatial_coordinates, `[[`, "x")), 2)),
  y = unname(round(unlist(lapply(spatial_coordinates, `[[`, "y")), 2)),
  ux = unname(round(umap[, 1L], 3)),
  uy = unname(round(umap[, 2L], 3)),
  clusters = unname(cluster),
  celltype = stats::setNames(unique(cell_type), as.character(0:3)),
  fields = list(
    synthetic_gradient = list(
      v = as.integer(round(seq(0, 255, length.out = n_cells))),
      min = 0,
      max = 1,
      label = "Synthetic gradient",
      desc = "Deterministic field for Viewer coverage."
    )
  ),
  moran = list(list(gene = gene_names[[1L]], morans_i = 0.5)),
  evidence = list()
)

build_artifacts <- function() {
  stage_dir <- tempfile("omnibus-stage-", tmpdir = output_dir)
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)

  artifact_names <- c(
    "demo_omnibus_seurat.rds",
    "demo_omnibus.crb",
    "demo_omnibus_markers.csv",
    "demo_omnibus_donorB_if.png",
    "demo_omnibus_donorC_review.png"
  )
  staged <- stats::setNames(
    file.path(stage_dir, artifact_names),
    artifact_names
  )
  destinations <- stats::setNames(
    file.path(output_dir, artifact_names),
    artifact_names
  )

  saveRDS(
    object,
    staged[["demo_omnibus_seurat.rds"]],
    compress = "xz",
    version = 3
  )
  utils::write.csv(
    marker_table,
    staged[["demo_omnibus_markers.csv"]],
    row.names = FALSE
  )
  png::writePNG(donor_b_if, staged[["demo_omnibus_donorB_if.png"]])
  png::writePNG(
    donor_c_review,
    staged[["demo_omnibus_donorC_review.png"]]
  )

  convertSeuratToCerebro(
    seurat_file = staged[["demo_omnibus_seurat.rds"]],
    result_dir = stage_dir,
    assay = "RNA",
    slot = "data",
    experiment_name = "Synthetic Omnibus",
    organism = "Human",
    groups = c(
      "seurat_clusters",
      "orig.ident",
      "cell_type",
      "phase",
      "condition"
    ),
    cell_cycle = "phase",
    add_most_expressed_genes = FALSE,
    verbose = FALSE
  )
  converted_crb <- file.path(stage_dir, "cerebro_demo_omnibus_seurat.crb")
  if (!file.rename(converted_crb, staged[["demo_omnibus.crb"]])) {
    stop("Could not stage the converted Omnibus CRB.", call. = FALSE)
  }

  if (!all(file.exists(staged)) || any(file.info(staged)$size <= 0L)) {
    stop("The staged Omnibus artifact set is incomplete.", call. = FALSE)
  }

  source_object <- readRDS(staged[["demo_omnibus_seurat.rds"]])
  cerebro_object <- readRDS(staged[["demo_omnibus.crb"]])
  staged_markers <- utils::read.csv(
    staged[["demo_omnibus_markers.csv"]],
    stringsAsFactors = FALSE
  )
  donor_b_png <- png::readPNG(staged[["demo_omnibus_donorB_if.png"]])
  donor_c_png <- png::readPNG(staged[["demo_omnibus_donorC_review.png"]])
  spatial_names <- names(spatial_coordinates)
  source_spatial_cells <- lapply(spatial_names, function(spatial_name) {
    Seurat::Cells(source_object[[spatial_name]])
  })
  names(source_spatial_cells) <- spatial_names
  expected_image_labels <- list(
    `donorA tissue` = c("H&E", "DAPI"),
    `donorB tissue` = "H&E",
    `donorC tissue` = NULL
  )

  stopifnot(
    inherits(source_object, "Seurat"),
    all(unname(dim(source_object)) == c(n_genes, n_cells)),
    identical(Seurat::Images(source_object), spatial_names),
    all(lengths(source_spatial_cells) == 40L),
    length(unique(unlist(source_spatial_cells))) == n_cells,
    setequal(unlist(source_spatial_cells), cell_names),
    setequal(unique(source_object$orig.ident), c("donorA", "donorB", "donorC")),
    all(unname(table(source_object$orig.ident)) == rep(40L, 3L)),
    setequal(unique(source_object$condition), c("Control", "Treatment")),
    identical(
      lapply(source_object@misc$cerebro_spatial_images, names),
      expected_image_labels
    ),
    inherits(cerebro_object, "Cerebro"),
    setequal(cerebro_object$getCellNames(), cell_names),
    setequal(cerebro_object$getGeneNames(), gene_names),
    setequal(
      cerebro_object$getGroups(),
      c("seurat_clusters", "orig.ident", "cell_type", "phase", "condition")
    ),
    identical(cerebro_object$getCellCycle(), "phase"),
    length(cerebro_object$getGeneLists()) > 0L,
    identical(cerebro_object$availableProjections(), "umap"),
    !is.null(cerebro_object$getTree("cell_type")),
    identical(cerebro_object$getGroupsWithMostExpressedGenes(), "cell_type"),
    identical(cerebro_object$getGroupsWithMeanExpression(), "cell_type"),
    length(cerebro_object$getMethodsForMarkerGenes()) > 0L,
    length(cerebro_object$getMethodsForEnrichedPathways()) > 0L,
    identical(cerebro_object$getMethodsForTrajectories(), "monocle2"),
    length(cerebro_object$getImmuneRepertoire()) > 0L,
    nrow(cerebro_object$getHLATyping()) > 0L,
    length(cerebro_object$getExtraMaterialCategories()) > 0L,
    !is.null(cerebro_object$getTrekker()),
    identical(cerebro_object$availableSpatial(), spatial_names),
    setequal(unique(staged_markers$cluster), unique(cell_type)),
    all(table(staged_markers$cluster) > 0L),
    identical(dim(donor_b_png), c(72L, 96L, 3L)),
    identical(dim(donor_c_png), c(90L, 110L, 3L))
  )

  for (spatial_name in spatial_names) {
    spatial <- cerebro_object$getSpatialData(spatial_name)
    stopifnot(
      nrow(spatial$coordinates) == 40L,
      setequal(
        rownames(spatial$coordinates),
        source_spatial_cells[[spatial_name]]
      ),
      identical(
        names(spatial$histology_images),
        expected_image_labels[[spatial_name]]
      )
    )
    for (image in spatial$histology_images) {
      bounds <- image$histology_image_bounds
      stopifnot(
        startsWith(image$histology_image, "data:image/png;base64,"),
        identical(names(bounds), c("xmin", "xmax", "ymin", "ymax")),
        all(spatial$coordinates$x >= bounds[["xmin"]]),
        all(spatial$coordinates$x <= bounds[["xmax"]]),
        all(spatial$coordinates$y >= bounds[["ymin"]]),
        all(spatial$coordinates$y <= bounds[["ymax"]])
      )
    }
  }

  backup_dir <- file.path(stage_dir, "backups")
  dir.create(backup_dir)
  existed <- file.exists(destinations)
  if (any(existed)) {
    copied <- file.copy(
      destinations[existed],
      file.path(backup_dir, names(destinations)[existed]),
      overwrite = TRUE
    )
    if (!all(copied)) {
      stop("Could not back up existing Omnibus artifacts.", call. = FALSE)
    }
  }

  replaced <- character(0)
  replacement_error <- NULL
  for (artifact_name in artifact_names) {
    copied <- file.copy(
      staged[[artifact_name]],
      destinations[[artifact_name]],
      overwrite = TRUE
    )
    if (!copied) {
      replacement_error <- paste0(
        "Could not replace generated artifact: ",
        destinations[[artifact_name]]
      )
      break
    }
    replaced <- c(replaced, artifact_name)
  }
  if (!is.null(replacement_error)) {
    for (artifact_name in replaced) {
      if (existed[[artifact_name]]) {
        file.copy(
          file.path(backup_dir, artifact_name),
          destinations[[artifact_name]],
          overwrite = TRUE
        )
      } else {
        unlink(destinations[[artifact_name]], force = TRUE)
      }
    }
    stop(replacement_error, call. = FALSE)
  }
}

build_artifacts()
message(
  "Built demo_omnibus_seurat.rds, demo_omnibus.crb, marker CSV, and two ",
  "external PNGs in ",
  output_dir
)
