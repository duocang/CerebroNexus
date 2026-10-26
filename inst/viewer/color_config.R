## Resolve palettes configured when a self-contained Viewer bundle was built.

resolve_configured_colors <- function(
  color_config,
  selected_path,
  configured_files
) {
  if (!is.list(color_config) || !length(color_config)) {
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
  dataset_id <- if (selected_path %in% labels) {
    selected_path
  } else {
    match_at <- which(unname(configured_files) == selected_path)
    if (!length(match_at)) {
      return(list())
    }
    labels[[match_at[[1L]]]]
  }
  palette <- color_config[[dataset_id]]
  if (!is.list(palette) || !length(palette)) {
    return(list())
  }
  palette
}

apply_configured_colors <- function(defaults, configured) {
  if (!length(configured) || is.null(names(configured))) {
    return(defaults)
  }
  shared <- intersect(names(defaults), names(configured))
  if (!length(shared)) {
    return(defaults)
  }
  defaults[shared] <- as.character(configured[shared])
  defaults
}

## Default colours a data set carries for the levels of one group
## (technical_info$group_colors, e.g. from scanpy's uns[<column>_colors]).
## Only valid "#RRGGBB" entries for levels the group actually has are returned;
## NULL when there is nothing usable. Palettes configured when the app was built
## and colours picked in the Viewer are applied on top of these.
stored_group_colors <- function(technical_info, group_name, levels) {
  stored <- tryCatch(
    technical_info[["group_colors"]][[group_name]],
    error = function(error) NULL
  )
  if (!is.character(stored) || is.null(names(stored))) {
    return(NULL)
  }
  stored <- stored[intersect(levels, names(stored))]
  stored <- stored[grepl("^#[0-9A-Fa-f]{6}$", stored)]
  if (!length(stored)) {
    return(NULL)
  }
  stored
}
