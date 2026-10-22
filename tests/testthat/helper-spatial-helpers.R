sys.source(
  viewer_test_path("core", "viewer_content_contract.R"),
  envir = environment()
)
viewer_runtime_env(environment())
sys.source(
  viewer_test_path("spatial", "func_spatial_helpers.R"),
  envir = environment()
)

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
