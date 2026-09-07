## Read and sanitize a generated Extra material table outside the Shiny process.

extra_material_read_table <- function(root, path, group_label, sheet_label) {
  table <- tryCatch(
    readRDS(file.path(root, path)),
    error = function(error) {
      stop(
        "Unable to read table `",
        sheet_label,
        "` from `",
        group_label,
        "`.",
        call. = FALSE
      )
    }
  )
  if (!is.data.frame(table)) {
    stop("The selected Extra material table is invalid.", call. = FALSE)
  }
  neutralize <- function(values) {
    values <- as.character(values)
    unsafe <- !is.na(values) & grepl("^[[:space:]]*[=+@-]", values)
    values[unsafe] <- paste0("'", values[unsafe])
    values
  }
  names(table) <- neutralize(names(table))
  text_columns <- which(vapply(
    table,
    function(column) is.character(column) || is.factor(column),
    logical(1)
  ))
  for (column_index in text_columns) {
    table[[column_index]] <- neutralize(table[[column_index]])
  }
  table
}
