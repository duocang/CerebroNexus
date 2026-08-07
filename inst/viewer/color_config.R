##----------------------------------------------------------------------------##
## Resolving the palette configured at bundle creation.
##
## `createShinyApp(colors = ...)` has always accepted a per-dataset palette and
## written it into the generated app's configuration. Nothing ever read it:
## `reactive_colors()` started from an empty list and filled it from the default
## colorset or the Color management pickers, so a configured palette was
## silently ignored and a deployment could not fix its own colours.
##
## Pure functions only -- no reactive, input, output or session references --
## so this file is sourced once at process startup and can be exercised in
## tests without a running app.
##----------------------------------------------------------------------------##

#' Find the palette configured for the data set currently loaded.
#'
#' The configuration is keyed by the label the user chose in
#' `createShinyApp(cerebro_data = c("PBMC example" = path))`, while the running
#' app knows only the path it loaded. Map one to the other.
#'
#' Returns an empty list when the data set was uploaded, is not in the
#' configuration, or when no palette was configured at all -- in every one of
#' those cases the defaults apply.
#'
#' @param color_config `Cerebro.options[["colors"]]`.
#' @param selected_path The `.crb` path currently loaded.
#' @param configured_files `Cerebro.options[["crb_file_to_load"]]`, a vector of
#'   paths named by data set label.
#'
#' @return A named list of palettes, one entry per grouping variable.
resolve_configured_colors <- function(
  color_config,
  selected_path,
  configured_files
) {
  if (!is.list(color_config) || length(color_config) == 0) {
    return(list())
  }
  if (
    is.null(selected_path) ||
      length(selected_path) != 1L ||
      is.na(selected_path) ||
      !nzchar(selected_path)
  ) {
    return(list())
  }
  labels <- names(configured_files)
  if (is.null(configured_files) || is.null(labels)) {
    return(list())
  }

  match_at <- which(unname(configured_files) == selected_path)
  if (length(match_at) == 0) {
    return(list())
  }
  palette <- color_config[[labels[match_at[1]]]]
  if (!is.list(palette) || length(palette) == 0) {
    return(list())
  }
  palette
}

#' Lay a configured palette over the defaults.
#'
#' Only names that are levels of the current data set are taken; a configured
#' colour for a level that no longer exists is ignored rather than added, and a
#' level the configuration says nothing about keeps its default. The result
#' therefore always has exactly one colour per current level.
#'
#' @param defaults Named character vector covering every current level.
#' @param configured Named character vector, possibly partial.
#'
#' @return `defaults` with the configured entries substituted.
apply_configured_colors <- function(defaults, configured) {
  if (is.null(configured) || length(configured) == 0) {
    return(defaults)
  }
  if (is.null(names(configured))) {
    return(defaults)
  }
  shared <- intersect(names(defaults), names(configured))
  if (length(shared) == 0) {
    return(defaults)
  }
  defaults[shared] <- as.character(configured[shared])
  defaults
}
