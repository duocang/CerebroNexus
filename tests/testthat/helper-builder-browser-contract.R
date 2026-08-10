builder_browser_old_contract_app <- function(
  app_dir,
  .local_envir = parent.frame()
) {
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  fixture_root <- tempfile("builder-old-contract-")
  fixture_dir <- file.path(fixture_root, "builder")
  viewer_dir <- file.path(dirname(app_dir), "viewer")
  if (!dir.exists(viewer_dir)) {
    stop("Builder Viewer support is unavailable.", call. = FALSE)
  }
  dir.create(fixture_root, recursive = TRUE)
  fs::dir_copy(app_dir, fixture_dir)
  fs::dir_copy(viewer_dir, file.path(fixture_root, "viewer"))
  withr::defer(
    unlink(fixture_root, recursive = TRUE, force = TRUE),
    envir = .local_envir
  )

  app_file <- file.path(fixture_dir, "app.R")
  source_file <- normalizePath(
    file.path(app_dir, "app.R"),
    winslash = "/",
    mustWork = TRUE
  )
  marker <- ".cerebro_bundle_privacy_contract_version"
  namespace <- asNamespace("CerebroNexus")
  original_value <- get(marker, envir = namespace, inherits = FALSE)
  original_locked <- bindingIsLocked(marker, namespace)
  restore_marker <- function() {
    if (bindingIsLocked(marker, namespace)) {
      unlockBinding(marker, namespace)
    }
    assign(marker, original_value, envir = namespace)
    if (original_locked) {
      lockBinding(marker, namespace)
    }
  }
  withr::defer(restore_marker(), envir = .local_envir)
  writeLines(
    c(
      "namespace <- asNamespace(\"CerebroNexus\")",
      sprintf("marker <- %s", deparse(marker)),
      "if (!exists(marker, namespace, inherits = FALSE)) {",
      "  stop(\"Privacy contract marker is unavailable.\", call. = FALSE)",
      "}",
      "if (bindingIsLocked(marker, namespace)) {",
      "  unlockBinding(marker, namespace)",
      "}",
      "assign(marker, 0L, envir = namespace)",
      "lockBinding(marker, namespace)",
      sprintf("app <- source(%s, local = TRUE)$value", deparse(source_file)),
      "if (!shiny::is.shiny.appobj(app)) {",
      "  stop(\"Builder fixture did not return a Shiny app.\", call. = FALSE)",
      "}",
      "app"
    ),
    app_file
  )
  shiny::shinyAppDir(fixture_dir)
}
