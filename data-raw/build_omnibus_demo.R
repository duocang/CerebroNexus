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
sample_id <- rep(c("sample_A", "sample_B"), each = 60L)

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

grid <- expand.grid(
  x = seq(60, 940, length.out = 12L),
  y = seq(50, 750, length.out = 10L)
)
spatial_coordinates <- data.frame(
  x = grid$x,
  y = grid$y,
  cell = cell_names
)
centroids <- SeuratObject::CreateCentroids(spatial_coordinates)
object[["omnibus_fov"]] <- SeuratObject::CreateFOV(
  coords = list(centroids = centroids),
  type = "centroids",
  assay = "RNA"
)

image_height <- 80L
image_width <- 100L
image_x <- matrix(
  rep(seq(0, 1, length.out = image_width), each = image_height),
  nrow = image_height
)
image_y <- matrix(
  rep(seq(0, 1, length.out = image_height), image_width),
  nrow = image_height
)
tissue <- exp(-((image_x - 0.48)^2 / 0.15 + (image_y - 0.52)^2 / 0.24))
histology <- array(0, dim = c(image_height, image_width, 3L))
histology[,, 1L] <- 0.18 + 0.72 * tissue
histology[,, 2L] <- 0.12 + 0.48 * tissue + 0.12 * image_x
histology[,, 3L] <- 0.28 + 0.58 * tissue + 0.08 * image_y
image_file <- tempfile(fileext = ".png")
png::writePNG(pmin(histology, 1), image_file)
image_uri <- paste0(
  "data:image/png;base64,",
  base64enc::base64encode(image_file)
)
unlink(image_file)
object@misc$cerebro_spatial_images <- list(
  omnibus_fov = list(
    histology_image = image_uri,
    histology_image_bounds = c(
      xmin = 0,
      xmax = 1000,
      ymin = 0,
      ymax = 800
    )
  )
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
  sample_A = c(
    "HLA-A*02:01",
    "HLA-B*07:02",
    "HLA-C*07:02",
    "HLA-DRB1*15:01",
    "HLA-DQB1*06:02"
  ),
  sample_B = c(
    "HLA-A*01:01",
    "HLA-B*08:01",
    "HLA-C*07:01",
    "HLA-DRB1*03:01",
    "HLA-DQB1*02:01"
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
    coord_source = "deterministic Omnibus FOV"
  ),
  qc = list(pct_positioned = 100, status = "synthetic"),
  barcodes = unname(cell_names),
  x = unname(round(spatial_coordinates$x, 2)),
  y = unname(round(spatial_coordinates$y, 2)),
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

stage_dir <- tempfile("omnibus-stage-", tmpdir = output_dir)
dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
previous_error_handler <- getOption("error")
options(error = function() {
  unlink(stage_dir, recursive = TRUE, force = TRUE)
  options(error = previous_error_handler)
})
staged_seurat <- file.path(stage_dir, "demo_omnibus_seurat.rds")
saveRDS(object, staged_seurat, compress = "xz", version = 3)

convertSeuratToCerebro(
  seurat_file = staged_seurat,
  result_dir = stage_dir,
  assay = "RNA",
  slot = "data",
  experiment_name = "Synthetic Omnibus",
  organism = "Human",
  groups = c("seurat_clusters", "orig.ident", "cell_type", "phase"),
  cell_cycle = "phase",
  add_most_expressed_genes = FALSE,
  verbose = FALSE
)
converted_crb <- file.path(stage_dir, "cerebro_demo_omnibus_seurat.crb")
staged_crb <- file.path(stage_dir, "demo_omnibus.crb")
if (!file.rename(converted_crb, staged_crb)) {
  stop("Could not stage the converted Omnibus CRB.", call. = FALSE)
}

source_object <- readRDS(staged_seurat)
cerebro_object <- readRDS(staged_crb)
stopifnot(
  inherits(source_object, "Seurat"),
  all(unname(dim(source_object)) == c(n_genes, n_cells)),
  inherits(cerebro_object, "Cerebro"),
  setequal(cerebro_object$getCellNames(), cell_names),
  setequal(cerebro_object$getGeneNames(), gene_names),
  length(cerebro_object$getGroups()) == 4L,
  length(cerebro_object$getGeneLists()) > 0L,
  length(cerebro_object$getMethodsForMarkerGenes()) > 0L,
  length(cerebro_object$getMethodsForEnrichedPathways()) > 0L,
  length(cerebro_object$getMethodsForTrajectories()) > 0L,
  length(cerebro_object$getImmuneRepertoire()) > 0L,
  nrow(cerebro_object$getHLATyping()) > 0L,
  length(cerebro_object$getExtraMaterialCategories()) > 0L,
  !is.null(cerebro_object$getTrekker()),
  identical(cerebro_object$availableSpatial(), "omnibus_fov")
)
spatial <- cerebro_object$getSpatialData("omnibus_fov")
stopifnot(
  is.character(spatial$histology_image),
  startsWith(spatial$histology_image, "data:image/png;base64,"),
  identical(
    names(spatial$histology_image_bounds),
    c("xmin", "xmax", "ymin", "ymax")
  )
)

replace_staged_file <- function(staged, destination) {
  if (!file.rename(staged, destination)) {
    stop(
      "Could not replace generated artifact: ",
      destination,
      call. = FALSE
    )
  }
}
replace_staged_file(
  staged_seurat,
  file.path(output_dir, "demo_omnibus_seurat.rds")
)
replace_staged_file(staged_crb, file.path(output_dir, "demo_omnibus.crb"))
unlink(stage_dir, recursive = TRUE, force = TRUE)
options(error = previous_error_handler)

message("Built demo_omnibus_seurat.rds and demo_omnibus.crb in ", output_dir)
