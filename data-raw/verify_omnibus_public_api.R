#!/usr/bin/env Rscript

command_args <- commandArgs(trailingOnly = TRUE)
full_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", full_args, value = TRUE)
if (length(script_arg) != 1L) {
  stop("Could not resolve the verifier script path.", call. = FALSE)
}

script_path <- normalizePath(
  sub("^--file=", "", script_arg),
  winslash = "/",
  mustWork = TRUE
)
repo_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  winslash = "/",
  mustWork = TRUE
)
output_root <- if (length(command_args) > 0L) {
  normalizePath(
    command_args[[1L]],
    winslash = "/",
    mustWork = FALSE
  )
} else {
  tempfile("omnibus-public-api-")
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop(
    "The devtools package is required to verify a source checkout.",
    call. = FALSE
  )
}
if (!requireNamespace("HDF5Array", quietly = TRUE)) {
  stop(
    "The HDF5Array package is required for this H5 verification.",
    call. = FALSE
  )
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
input_dir <- file.path(output_root, "inputs")
dir.create(input_dir, showWarnings = FALSE)
fixture_names <- c(
  "demo_omnibus_seurat.rds",
  "demo_omnibus_markers.csv",
  "demo_omnibus_donorB_if.png",
  "demo_omnibus_donorC_review.png"
)
fixture_source <- file.path(
  repo_root,
  "inst",
  "extdata",
  "examples",
  fixture_names
)
fixture_target <- file.path(input_dir, fixture_names)
if (!all(file.exists(fixture_source))) {
  stop("The committed Omnibus input fixtures are incomplete.", call. = FALSE)
}
if (!all(file.copy(fixture_source, fixture_target, overwrite = TRUE))) {
  stop("Could not copy the committed Omnibus inputs.", call. = FALSE)
}

devtools::load_all(repo_root, quiet = TRUE)
old_directory <- setwd(output_root)
on.exit(setwd(old_directory), add = TRUE)
dir.create("output", showWarnings = FALSE)

convertSeuratToCerebro(
  seurat_file = "inputs/demo_omnibus_seurat.rds",
  result_dir = "output",
  assay = "RNA",
  slot = "data",
  experiment_name = "Synthetic Omnibus",
  organism = "Human",
  groups = c("orig.ident", "condition", "cell_type"),
  groups_naming = list("orig.ident" = "sample", "cell_type" = "cluster"),
  marker_file = "inputs/demo_omnibus_markers.csv",
  marker_method = "Synthetic markers",
  spatial_images = list(
    "donorB tissue" = c("IF panel" = "inputs/demo_omnibus_donorB_if.png")
  ),
  expression_matrix_mode = "h5",
  verbose = FALSE
)

createShinyApp(
  cerebro_data = c(Omnibus = "output/cerebro_demo_omnibus_seurat.crb"),
  spatial_images = list(
    Omnibus = list(
      "donorC tissue" = c(
        "Pathology review" = "inputs/demo_omnibus_donorC_review.png"
      )
    )
  ),
  result_dir = "my_app",
  welcome_message = "<h2>Synthetic Omnibus Atlas</h2>",
  port = 8080,
  host = "127.0.0.1",
  max_request_size = 8000,
  overwrite = TRUE,
  launch_browser = FALSE,
  verbose = FALSE
)

assert_public <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

crb_path <- file.path("output", "cerebro_demo_omnibus_seurat.crb")
assert_public(
  file.exists(crb_path),
  "The public converter did not create the CRB."
)
crb <- readRDS(crb_path)
assert_public(
  inherits(crb, "Cerebro"),
  "The converted object is not a Cerebro object."
)

backend <- crb$getExpressionBackend()
assert_public(
  identical(backend$type, "h5") &&
    is.character(backend$location) &&
    length(backend$location) == 1L,
  "The converted CRB does not declare the requested H5 backend."
)
h5_path <- file.path(dirname(crb_path), backend$location)
assert_public(
  file.exists(h5_path),
  "The converted CRB's H5 sidecar is missing."
)
h5_matrix <- HDF5Array::TENxMatrix(h5_path, group = "expression")
assert_public(
  identical(dim(h5_matrix), c(120L, 80L)),
  "The H5 sidecar does not contain the complete 120-cell by 80-gene matrix."
)

assert_public(
  setequal(crb$getGroups(), c("sample", "condition", "cluster")) &&
    !any(c("orig.ident", "cell_type") %in% crb$getGroups()),
  "The requested public group renames were not applied."
)
metadata <- crb$getMetaData()
assert_public(
  setequal(unique(metadata$sample), c("donorA", "donorB", "donorC")) &&
    setequal(unique(metadata$condition), c("Control", "Treatment")) &&
    setequal(
      unique(metadata$cluster),
      c("T cell", "B cell", "Myeloid", "Stromal")
    ),
  "The renamed groups do not retain the complete Omnibus metadata."
)

assert_public(
  identical(crb$getMethodsForMarkerGenes(), "Synthetic markers"),
  "The public marker-file import did not retain its method name."
)
marker_groups <- crb$getGroupsWithMarkerGenes("Synthetic markers")
imported_markers <- do.call(
  rbind,
  lapply(marker_groups, function(group) {
    crb$getMarkerGenes("Synthetic markers", group)
  })
)
expected_markers <- utils::read.csv(
  file.path("inputs", "demo_omnibus_markers.csv"),
  stringsAsFactors = FALSE
)
assert_public(
  nrow(imported_markers) == nrow(expected_markers) &&
    setequal(imported_markers$gene, expected_markers$gene),
  "The public marker-file import is incomplete."
)

expected_spatial_images <- list(
  "donorA tissue" = c("H&E", "DAPI"),
  "donorB tissue" = c("H&E", "IF panel"),
  "donorC tissue" = character()
)
assert_public(
  identical(crb$availableSpatial(), names(expected_spatial_images)),
  "The converted CRB does not contain the three isolated spatial entries."
)
for (spatial_name in names(expected_spatial_images)) {
  spatial <- crb$getSpatialData(spatial_name)
  actual_labels <- names(spatial$histology_images)
  if (is.null(actual_labels)) {
    actual_labels <- character()
  }
  assert_public(
    setequal(actual_labels, expected_spatial_images[[spatial_name]]),
    paste0("Unexpected image labels for spatial entry: ", spatial_name)
  )
  for (image_label in actual_labels) {
    descriptor <- spatial$histology_images[[image_label]]
    assert_public(
      grepl("^data:image/png;base64,", descriptor$histology_image) &&
        identical(
          names(descriptor$histology_image_bounds),
          c("xmin", "xmax", "ymin", "ymax")
        ),
      paste0(
        "Invalid embedded image descriptor: ",
        spatial_name,
        "/",
        image_label
      )
    )
  }
}

app_dir <- "my_app"
required_app_sources <- c(
  file.path(app_dir, "app.R"),
  file.path(app_dir, "cerebro_config.rds"),
  file.path(app_dir, "viewer"),
  file.path(app_dir, "extdata")
)
assert_public(
  all(file.exists(required_app_sources)),
  "createShinyApp did not bundle all required app sources."
)

config <- readRDS(file.path(app_dir, "cerebro_config.rds"))
assert_public(
  identical(config$welcome_message, "<h2>Synthetic Omnibus Atlas</h2>") &&
    identical(config$.bundle_run_options$shiny_app_options$port, 8080L) &&
    identical(config$.bundle_run_options$shiny_app_options$host, "127.0.0.1") &&
    identical(
      config$.bundle_run_options$max_request_size_bytes,
      as.double(8000 * 1024^2)
    ),
  "The generated app config does not preserve the public run options."
)

configured_crb <- unname(config$crb_file_to_load[["Omnibus"]])
private_crb <- file.path(app_dir, configured_crb)
private_h5 <- file.path(app_dir, "private-data", backend$location)
assert_public(
  file.exists(private_crb) && file.exists(private_h5),
  "The generated app does not contain its private CRB and H5 sidecar."
)
assert_public(
  identical(
    config$.bundle_backend_plan$entries[[configured_crb]],
    list(type = "h5", mode = "bundled", location = backend$location)
  ),
  "The app backend plan does not bind the private CRB to its H5 sidecar."
)

external <- config$spatial_images$Omnibus[["donorC tissue"]][[
  "Pathology review"
]]
external_path <- if (is.list(external)) external$path else external
assert_public(
  is.character(external_path) &&
    length(external_path) == 1L &&
    !grepl("^(/|[A-Za-z]:)", external_path) &&
    file.exists(file.path(app_dir, external_path)),
  "The donorC external image asset is missing or non-portable."
)

private_object <- readRDS(private_crb)
assert_public(
  identical(private_object$getExpressionBackend(), backend) &&
    setequal(
      names(private_object$getSpatialData("donorA tissue")$histology_images),
      c("H&E", "DAPI")
    ) &&
    setequal(
      names(private_object$getSpatialData("donorB tissue")$histology_images),
      c("H&E", "IF panel")
    ) &&
    identical(
      private_object$getSpatialData("donorC tissue")$histology_images,
      list()
    ),
  "The app's private CRB changed the converted backend or spatial isolation."
)

cat("OMNIBUS_OUTPUT_ROOT=", output_root, "\n", sep = "")
