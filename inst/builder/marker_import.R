##----------------------------------------------------------------------------##
## Inert import records for precomputed Marker genes.
##----------------------------------------------------------------------------##

builder_marker_import_error <- function(filename, sheet, error) {
  list(
    source_name = if (is.null(sheet)) basename(filename) else sheet,
    file_name = basename(filename),
    sheet = sheet,
    table = NULL,
    columns = character(),
    rows = 0L,
    valid = FALSE,
    error = error
  )
}

builder_marker_import_read_delimited <- function(path, extension) {
  separator <- switch(extension, csv = ",", tsv = "\t", NULL)
  if (is.null(separator)) {
    return(NULL)
  }
  result <- try(
    utils::read.delim(
      path,
      sep = separator,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    silent = TRUE
  )
  if (inherits(result, "try-error")) NULL else result
}

builder_marker_import_source <- function(path, filename, sheet, table) {
  source_name <- if (is.null(sheet)) basename(filename) else as.character(sheet)
  valid <- is.data.frame(table) && nrow(table) > 0L && ncol(table) > 0L
  list(
    source_name = source_name,
    file_name = basename(filename),
    sheet = sheet,
    table = if (valid) table else NULL,
    columns = if (valid) names(table) else character(),
    rows = if (valid) as.integer(nrow(table)) else 0L,
    valid = valid,
    error = if (valid) NULL else "unusable_table"
  )
}

builder_marker_import_file_inventory <- function(path, filename) {
  extension <- tolower(tools::file_ext(filename))
  if (!file.exists(path)) {
    return(list(builder_marker_import_error(filename, NULL, "file_not_found")))
  }
  if (extension %in% c("csv", "tsv")) {
    return(list(builder_marker_import_source(
      path,
      filename,
      NULL,
      builder_marker_import_read_delimited(path, extension)
    )))
  }
  if (identical(extension, "xlsx")) {
    sheets <- try(readxl::excel_sheets(path), silent = TRUE)
    if (inherits(sheets, "try-error") || !length(sheets)) {
      return(list(builder_marker_import_error(
        filename,
        NULL,
        "unreadable_workbook"
      )))
    }
    return(lapply(sheets, function(sheet) {
      table <- try(
        as.data.frame(readxl::read_excel(path, sheet)),
        silent = TRUE
      )
      if (inherits(table, "try-error")) {
        table <- NULL
      }
      builder_marker_import_source(path, filename, sheet, table)
    }))
  }
  list(builder_marker_import_error(filename, NULL, "unsupported_format"))
}

builder_marker_import_inventory <- function(
  paths,
  filenames = basename(paths)
) {
  paths <- as.character(paths)
  filenames <- as.character(filenames)
  if (length(paths) != length(filenames)) {
    stop(
      "Marker import paths and filenames must have the same length.",
      call. = FALSE
    )
  }
  unlist(
    Map(builder_marker_import_file_inventory, paths, filenames),
    recursive = FALSE,
    use.names = FALSE
  )
}
