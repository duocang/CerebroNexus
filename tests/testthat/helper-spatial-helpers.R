spatial_helpers_file <- file.path(
  "..",
  "..",
  "inst",
  "viewer",
  "spatial",
  "func_spatial_helpers.R"
)

if (!file.exists(spatial_helpers_file)) {
  spatial_helpers_file <- system.file(
    "viewer/spatial/func_spatial_helpers.R",
    package = "CerebroNexus"
  )
}

sys.source(spatial_helpers_file, envir = environment())

expected_spatial_image_target <- function(
  dataset,
  spatial_name,
  image_label,
  filename
) {
  CerebroNexus:::.spatialImageBundleTarget(
    dataset,
    spatial_name,
    image_label,
    filename
  )
}
