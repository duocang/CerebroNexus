## Plain-language help used by inline prompts and the glossary.

.builder_help_entries <- list(
  assay = c("Assay", "A named set of measurements for the same cells."),
  layer = c(
    "Layer",
    "The measurement values to include, such as counts or normalized values."
  ),
  embedded = c(
    "Embedded storage",
    "Keep expression values inside the dataset file. Simple, but uses more memory."
  ),
  h5 = c(
    "H5 storage",
    "Keep expression values in a companion file to reduce memory use."
  ),
  bpcells = c(
    "BPCells storage",
    "Keep expression values in a companion folder for large datasets."
  ),
  `default group` = c(
    "Default group",
    "The labels used to colour cells when the Viewer opens."
  ),
  `default projection` = c(
    "Default projection",
    "The two-dimensional cell map shown when the Viewer opens."
  ),
  cell_barcode = c(
    "Cell identity",
    "A stable unique name that keeps each cell matched across all content."
  ),
  show_upload_ui = c(
    "Allow uploads",
    "Let Viewer users temporarily open their own compatible dataset."
  ),
  `optional analyses` = c(
    "Optional analyses",
    "Extra results you may compute now. They can take time and are never selected automatically."
  ),
  `initial dataset` = c(
    "Initial dataset",
    "The dataset shown first when the generated App opens."
  ),
  palettes = c("Palettes", "The saved colours used for each group of cells.")
)

builder_help_resolve <- function(term) {
  key <- tolower(trimws(as.character(term)[1L]))
  entry <- .builder_help_entries[[key]]
  if (is.null(entry)) {
    return(list(
      term = key,
      title = "Help",
      plain = "No explanation is available yet."
    ))
  }
  list(term = key, title = entry[[1L]], plain = entry[[2L]])
}
