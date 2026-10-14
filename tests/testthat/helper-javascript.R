skip_if_no_viewer_js_parser <- function() {
  node <- Sys.which("node")
  testthat::skip_if(!nzchar(node), "Node unavailable")
  fixture <- normalizePath(testthat::test_path("fixtures", "js-source.cjs"))
  # Resolve from the actual fixture, just as Node will when the test runs.
  # Acorn is optional test tooling, not part of the installed R package.
  status <- system2(
    node,
    c("-e", shQuote(sprintf(
      paste0(
        "const load = require('module').createRequire(%s);",
        "try { load.resolve('acorn'); } catch (e) {",
        "if (e.code === 'MODULE_NOT_FOUND') process.exit(42); throw e; }"
      ),
      jsonlite::toJSON(fixture, auto_unbox = TRUE)
    ))),
    stdout = FALSE,
    stderr = FALSE
  )
  testthat::skip_if(status == 42L, "Optional Acorn parser unavailable; run npm ci --prefix tests")
  if (status != 0L) stop("Could not check availability of the JavaScript parser")
}

# Temporary Node runners live outside tests/, so resolve the shared loader here.
viewer_js_loader <- function() {
  skip_if_no_viewer_js_parser()
  path <- normalizePath(testthat::test_path("fixtures", "js-source.cjs"))
  sprintf("const js = require(%s);", jsonlite::toJSON(path, auto_unbox = TRUE))
}
