trekker_gene_suggest <- function(tk, gene_names) {
  supplied <- unique(c(
    as.character(tk$cluster_markers$gene %||% character()),
    as.character(tk$moran$gene %||% character())
  ))
  supplied[supplied %in% gene_names]
}

trekker_file_path <- function(descriptor, crb_path = NULL, app_root = ".") {
  relative <- descriptor$path %||% ""
  if (
    length(relative) != 1L ||
      is.na(relative) ||
      !nzchar(relative) ||
      grepl("^([A-Za-z]:|/|\\\\)", relative) ||
      any(
        strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]] %in%
          c("", ".", "..")
      )
  ) {
    return(NULL)
  }
  candidates <- c(
    file.path(app_root, relative),
    if (!is.null(crb_path)) file.path(dirname(crb_path), relative)
  )
  candidates <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (!length(candidates)) NULL else candidates[[1L]]
}

trekker_file_size <- function(bytes) {
  if (is.null(bytes) || !is.finite(bytes)) {
    return("unknown")
  }
  units <- c("B", "KB", "MB", "GB")
  index <- min(floor(log(max(bytes, 1), 1024)) + 1L, length(units))
  paste0(
    format(round(bytes / 1024^(index - 1L), 1L), trim = TRUE),
    " ",
    units[[index]]
  )
}

trekker_image_uri <- function(descriptor, crb_path = NULL, app_root = ".") {
  path <- trekker_file_path(
    descriptor,
    crb_path = crb_path,
    app_root = app_root
  )
  if (is.null(path) || !requireNamespace("base64enc", quietly = TRUE)) {
    return(NULL)
  }
  paste0("data:", descriptor$mime, ";base64,", base64enc::base64encode(path))
}

trekker_empty <- function(message) {
  div(class = "tk-empty", message)
}
