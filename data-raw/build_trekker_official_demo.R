## Build the small Trekker demo from the official Mouse Brain TrekkerU_C data.
##
## Usage:
##   CEREBRONEXUS_TREKKER_DATA_DIR=/path/to/trekker-official \
##     Rscript data-raw/build_trekker_official_demo.R

suppressPackageStartupMessages({
  library(Matrix)
  library(Seurat)
})

pkgload::load_all(".", quiet = TRUE)

official_root <- Sys.getenv("CEREBRONEXUS_TREKKER_DATA_DIR")
if (!nzchar(official_root)) {
  stop(
    "Set CEREBRONEXUS_TREKKER_DATA_DIR to the official Trekker data directory."
  )
}

dataset_dir <- if (basename(official_root) == "mouse-brain-trekkeru-c") {
  official_root
} else {
  file.path(official_root, "mouse-brain-trekkeru-c")
}
if (!dir.exists(dataset_dir)) {
  stop("Official Mouse Brain TrekkerU_C directory not found: ", dataset_dir)
}

find_one <- function(pattern) {
  matches <- list.files(dataset_dir, pattern = pattern, full.names = TRUE)
  if (length(matches) != 1L) {
    stop("Expected exactly one official file matching: ", pattern)
  }
  matches[[1L]]
}

rds_path <- find_one("ConfPositioned_seurat_spatial.*\\.rds$")
location_path <- find_one("Location_ConfPositionedNuclei.*\\.csv$")
metrics_path <- find_one("summary_metrics.*\\.csv$")
markers_path <- find_one("variable_features_clusters.*\\.csv$")
moran_path <- find_one("variable_features_spatial_moransi.*\\.txt$")

source_object <- readRDS(rds_path)
markers <- utils::read.csv(markers_path, row.names = 1L, check.names = FALSE)
moran <- utils::read.delim(moran_path, row.names = 1L, check.names = FALSE)

set.seed(42)
cells_by_cluster <- split(
  Seurat::Cells(source_object),
  as.character(source_object$seurat_clusters)
)
cells <- unlist(
  lapply(cells_by_cluster, function(cluster_cells) {
    sample(cluster_cells, min(40L, length(cluster_cells)))
  }),
  use.names = FALSE
)

marker_order <- order(
  as.character(markers$cluster),
  markers$p_val_adj,
  -markers$avg_log2FC
)
ordered_markers <- markers[marker_order, , drop = FALSE]
marker_genes <- unlist(
  lapply(split(ordered_markers$gene, ordered_markers$cluster), head, 3L),
  use.names = FALSE
)
moran_rank <- suppressWarnings(as.numeric(
  moran$moransi.spatially.variable.rank
))
moran_genes <- rownames(moran)[order(moran_rank, na.last = TRUE)]

counts <- SeuratObject::LayerData(
  source_object,
  assay = "RNA",
  layer = "counts"
)[, cells, drop = FALSE]
top_expressed <- rownames(counts)[order(
  Matrix::rowSums(counts),
  decreasing = TRUE
)]
genes <- head(
  intersect(
    unique(c(marker_genes, head(moran_genes, 30L), top_expressed)),
    rownames(counts)
  ),
  100L
)
counts <- counts[genes, , drop = FALSE]
metadata <- source_object[[]][cells, , drop = FALSE]
object <- Seurat::CreateSeuratObject(
  counts = counts,
  assay = "RNA",
  meta.data = metadata
)
object[["SPATIAL"]] <- SeuratObject::CreateDimReducObject(
  embeddings = Seurat::Embeddings(source_object, "SPATIAL")[
    cells,
    ,
    drop = FALSE
  ],
  key = "SPATIAL_",
  assay = "RNA"
)
object[["umap"]] <- SeuratObject::CreateDimReducObject(
  embeddings = Seurat::Embeddings(source_object, "umap")[cells, , drop = FALSE],
  key = "UMAP_",
  assay = "RNA"
)
Seurat::Idents(object) <- "seurat_clusters"
rm(source_object, counts)

sample_dir <- tempfile("trekker-demo-")
dir.create(sample_dir)
on.exit(unlink(sample_dir, recursive = TRUE, force = TRUE), add = TRUE)

location <- utils::read.csv(location_path, row.names = 1L, check.names = FALSE)
location <- location[cells, , drop = FALSE]
sample_location <- file.path(
  sample_dir,
  "Mouse_Brain_TrekkerU_C_demo_Location_ConfPositionedNuclei.csv"
)
utils::write.csv(location, sample_location, quote = FALSE)

sample_metrics <- file.path(
  sample_dir,
  "Mouse_Brain_TrekkerU_C_demo_summary_metrics.csv"
)
if (!file.copy(metrics_path, sample_metrics)) {
  stop("Could not copy the official summary metrics.")
}

sample_markers <- file.path(
  sample_dir,
  "Mouse_Brain_TrekkerU_C_demo_variable_features_clusters.csv"
)
markers <- markers[
  as.character(markers$cluster) %in%
    names(cells_by_cluster) &
    markers$gene %in% genes,
  ,
  drop = FALSE
]
utils::write.csv(markers, sample_markers, quote = FALSE)

sample_moran <- file.path(
  sample_dir,
  "Mouse_Brain_TrekkerU_C_demo_variable_features_spatial_moransi.txt"
)
moran <- moran[intersect(rownames(moran), genes), , drop = FALSE]
utils::write.table(
  moran,
  sample_moran,
  sep = "\t",
  quote = FALSE,
  col.names = NA
)

section_by_cell <- stats::setNames(rep(NA_character_, length(cells)), cells)
sampled_clusters <- split(
  cells,
  as.character(metadata[cells, "seurat_clusters"])
)
for (cluster_cells in sampled_clusters) {
  spatial_order <- order(
    location[cluster_cells, "SPATIAL_1"],
    location[cluster_cells, "SPATIAL_2"],
    cluster_cells
  )
  ordered_cells <- cluster_cells[spatial_order]
  section_by_cell[ordered_cells] <- rep(
    c("slice1", "slice2"),
    length.out = length(ordered_cells)
  )
}

write_overlay <- function(name, body) {
  path <- file.path(sample_dir, name)
  writeLines(
    c(
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1000 700">',
      '<rect width="1000" height="700" fill="#f8fafc"/>',
      body,
      '</svg>'
    ),
    path,
    useBytes = TRUE
  )
  path
}

slice1_ellipses <- write_overlay(
  "slice1-synthetic-ellipses.svg",
  c(
    '<ellipse cx="320" cy="350" rx="260" ry="210" fill="#7c3aed" fill-opacity=".55"/>',
    '<ellipse cx="700" cy="310" rx="210" ry="250" fill="#c4b5fd" fill-opacity=".75"/>',
    '<path d="M70 590 Q500 360 930 570" fill="none" stroke="#5b21b6" stroke-width="34" stroke-linecap="round"/>'
  )
)
slice1_triangles <- write_overlay(
  "slice1-synthetic-triangles.svg",
  c(
    '<path d="M80 610 L300 100 L500 610 Z" fill="#06b6d4" fill-opacity=".5"/>',
    '<path d="M430 590 L700 70 L930 590 Z" fill="#67e8f9" fill-opacity=".65"/>',
    '<path d="M90 180 H910 M90 350 H910 M90 520 H910" stroke="#0e7490" stroke-width="18" stroke-dasharray="36 24"/>'
  )
)
slice2_polygons <- write_overlay(
  "slice2-synthetic-polygons.svg",
  c(
    '<path d="M80 240 L260 70 L470 150 L520 390 L300 610 L90 520 Z" fill="#f97316" fill-opacity=".58"/>',
    '<path d="M560 120 L900 90 L940 420 L700 620 L520 390 Z" fill="#fdba74" fill-opacity=".72"/>',
    '<path d="M150 350 L850 350" stroke="#c2410c" stroke-width="30" stroke-linecap="round"/>'
  )
)
slice2_circles <- write_overlay(
  "slice2-synthetic-circles.svg",
  c(
    '<circle cx="230" cy="220" r="145" fill="#22c55e" fill-opacity=".55"/>',
    '<circle cx="520" cy="430" r="190" fill="#86efac" fill-opacity=".62"/>',
    '<circle cx="810" cy="210" r="120" fill="#15803d" fill-opacity=".48"/>',
    '<path d="M80 610 C260 450 360 650 520 510 S760 330 930 500" fill="none" stroke="#166534" stroke-width="26"/>'
  )
)

out_file <- "inst/extdata/examples/demo_trekker_mouse_brain_u_c.crb"
exportFromSeurat(
  object = object,
  assay = "RNA",
  slot = "counts",
  file = out_file,
  trekker_data = list(
    location = sample_location,
    metrics = sample_metrics,
    cluster_markers = sample_markers,
    moran = sample_moran
  ),
  trekker_sections = section_by_cell,
  trekker_images = list(
    slice1 = list(
      `Purple ellipses` = list(
        path = slice1_ellipses,
        provenance = "Demo image layer; not measured tissue imagery."
      ),
      `Cyan triangles` = list(
        path = slice1_triangles,
        provenance = "Demo image layer; not measured tissue imagery."
      )
    ),
    slice2 = list(
      `Orange polygons` = list(
        path = slice2_polygons,
        provenance = "Demo image layer; not measured tissue imagery."
      ),
      `Green circles` = list(
        path = slice2_circles,
        provenance = "Demo image layer; not measured tissue imagery."
      )
    )
  ),
  trekker_image_settings = list(
    slice1 = list(
      `Purple ellipses` = list(image_opacity = 0.42),
      `Cyan triangles` = list(image_opacity = 0.26, rotation = 4)
    ),
    slice2 = list(
      `Orange polygons` = list(image_opacity = 0.38),
      `Green circles` = list(image_opacity = 0.28, flip_x = TRUE)
    )
  ),
  experiment_name = "Mouse brain (TrekkerU C, official subset)",
  organism = "mm",
  groups = "seurat_clusters",
  main_group = "seurat_clusters",
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA",
  add_all_meta_data = TRUE,
  expression_matrix_mode = "embedded",
  verbose = TRUE
)

sidecar_dir <- sub("\\.crb$", ".trekker", out_file)
artifacts <- c(out_file, list.files(sidecar_dir, full.names = TRUE))
total_bytes <- sum(file.info(artifacts)$size)
if (total_bytes >= 1024^2) {
  stop("Trekker demo exceeds 1 MiB: ", total_bytes, " bytes")
}
message("Built ", out_file, " (", total_bytes, " bytes including sidecar)")
