builder_browser_old_contract_app <- function(
  app_file,
  .local_envir = parent.frame()
) {
  namespace <- asNamespace("CerebroNexus")
  marker <- ".cerebro_bundle_privacy_contract_version"
  if (!exists(marker, namespace, inherits = FALSE)) {
    stop("Privacy contract marker is unavailable.", call. = FALSE)
  }
  original_value <- get(marker, envir = namespace, inherits = FALSE)
  original_locked <- bindingIsLocked(marker, namespace)
  restore <- function() {
    if (bindingIsLocked(marker, namespace)) {
      unlockBinding(marker, namespace)
    }
    assign(marker, original_value, envir = namespace)
    if (original_locked) {
      lockBinding(marker, namespace)
    }
  }
  withr::defer(restore(), envir = .local_envir)

  if (original_locked) {
    unlockBinding(marker, namespace)
  }
  assign(marker, 0L, envir = namespace)
  if (!bindingIsLocked(marker, namespace)) {
    lockBinding(marker, namespace)
  }
  shiny::shinyAppFile(app_file)
}
