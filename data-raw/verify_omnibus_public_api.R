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

cat("OMNIBUS_OUTPUT_ROOT=", output_root, "\n", sep = "")
