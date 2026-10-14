# Temporary Node runners live outside tests/, so resolve the shared loader here.
viewer_js_loader <- function() {
  path <- normalizePath(testthat::test_path("fixtures", "js-source.cjs"))
  sprintf("const js = require(%s);", jsonlite::toJSON(path, auto_unbox = TRUE))
}
