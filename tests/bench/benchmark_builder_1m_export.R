#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "usage: benchmark_builder_1m_export.R <source.rds> <output.crb> <result.csv>",
    call. = FALSE
  )
}

source_rds <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
output_crb <- path.expand(args[[2L]])
result_csv <- path.expand(args[[3L]])
if (tolower(tools::file_ext(output_crb)) != "crb") {
  stop("The output path must end in .crb.", call. = FALSE)
}
output_sidecar <- file.path(
  dirname(output_crb),
  paste0(tools::file_path_sans_ext(basename(output_crb)), ".bpcells")
)
for (path in c(output_crb, output_sidecar, result_csv)) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path) || dir.exists(path)) {
    stop("Refusing to replace benchmark output: ", path, call. = FALSE)
  }
}

timed <- function(code) {
  before <- proc.time()
  value <- force(code)
  elapsed <- proc.time() - before
  list(
    value = value,
    wall_seconds = unname(elapsed[["elapsed"]]),
    cpu_seconds = unname(elapsed[["user.self"]] + elapsed[["sys.self"]])
  )
}

directory_bytes <- function(path) {
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE
  )
  sum(file.info(files)$size, na.rm = TRUE)
}

suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
for (file in c(
  "profile.R",
  "inspect.R",
  "extras.R",
  "build.R"
)) {
  sys.source(file.path("inst", "builder", file), envir = globalenv())
}

read_phase <- timed(readRDS(source_rds))
object <- read_phase$value
if (!methods::is(object, "Seurat") || ncol(object) != 1000000L) {
  stop(
    "The benchmark source must be a one-million-cell Seurat object.",
    call. = FALSE
  )
}
assay <- SeuratObject::DefaultAssay(object)
layer <- SeuratObject::Layers(object[[assay]])[[1L]]
matrix <- suppressWarnings(SeuratObject::LayerData(
  object[[assay]],
  layer = layer
))
if (!inherits(matrix, "IterableMatrix")) {
  stop(
    "The one-million-cell source must retain its BPCells layer.",
    call. = FALSE
  )
}

groups <- "seurat_clusters"
projections <- "umap"
n_umi <- "nCount_RNA"
n_gene <- "nFeature_RNA"
missing <- c(
  setdiff(c(groups, n_umi, n_gene), colnames(object@meta.data)),
  setdiff(projections, names(object@reductions))
)
if (length(missing)) {
  stop(
    "The benchmark source is missing: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}
group_values <- object@meta.data[[groups]]
group_levels <- if (is.factor(group_values)) {
  levels(group_values)
} else {
  sort(unique(as.character(group_values)), na.last = NA)
}
if (anyNA(group_values)) {
  group_levels <- c(group_levels, "N/A")
}
source_metadata <- colnames(object@meta.data)
expected_metadata <- make.unique(c(
  "cell_barcode",
  groups,
  "nUMI",
  "nGene",
  setdiff(source_metadata, c(groups, n_umi, n_gene))
))
sidecar <- basename(output_sidecar)
item <- list(
  name = "mouse_brain_1m",
  assay = assay,
  layer = layer,
  organism = "mm",
  included_groups = groups,
  default_group = groups,
  nUMI = n_umi,
  nGene = n_gene,
  included_projections = projections,
  metadata_policy = list(retained = source_metadata),
  expression_backend = "bpcells",
  sidecars = sidecar,
  spatial_image_storage = "external",
  images = list(),
  spatial_coordinate_transforms = list(),
  trekker_alignment = NULL,
  artifact_identity = list(
    schema_version = 2L,
    cells = builder_axis_identity(SeuratObject::Cells(object)),
    features = builder_axis_identity(rownames(object[[assay]])),
    group_levels = stats::setNames(list(group_levels), groups),
    projections = projections,
    metadata = expected_metadata,
    source_metadata = source_metadata,
    spatial_sections = character()
  ),
  viewer_page_expectations = list(visible_conditional = character())
)

gc(FALSE)
export_phase <- timed(.builder_build_export(object, item, output_crb))
if (!file.exists(output_crb)) {
  stop("Builder export did not produce a CRB.", call. = FALSE)
}
sidecar_path <- file.path(dirname(output_crb), sidecar)
if (!dir.exists(sidecar_path)) {
  stop("Builder export did not produce the BPCells sidecar.", call. = FALSE)
}
rm(matrix)
gc(FALSE)
verify_phase <- timed(builder_verify_crb(output_crb, item))
if (!isTRUE(verify_phase$value$valid)) {
  stop("Builder verification did not return valid evidence.", call. = FALSE)
}
digest_phase <- timed(unname(tools::md5sum(output_crb)))

revision <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE)[[1L]],
  error = function(error) NA_character_
)
result <- data.frame(
  recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  revision = revision,
  cells = ncol(object),
  features = nrow(object),
  source_storage = "bpcells",
  source_storage_order = BPCells::storage_order(
    suppressWarnings(SeuratObject::LayerData(object[[assay]], layer = layer))
  ),
  backend = verify_phase$value$backend$type,
  read_seconds = read_phase$wall_seconds,
  export_seconds = export_phase$wall_seconds,
  verify_seconds = verify_phase$wall_seconds,
  digest_seconds = digest_phase$wall_seconds,
  measured_seconds = sum(c(
    read_phase$wall_seconds,
    export_phase$wall_seconds,
    verify_phase$wall_seconds,
    digest_phase$wall_seconds
  )),
  crb_bytes = unname(file.info(output_crb)$size),
  sidecar_bytes = directory_bytes(sidecar_path),
  md5 = digest_phase$value,
  stringsAsFactors = FALSE
)
utils::write.csv(result, result_csv, row.names = FALSE)
print(result, row.names = FALSE)
