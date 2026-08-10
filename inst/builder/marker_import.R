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

builder_marker_import_infer_level <- function(
  source_name,
  sheet,
  known_levels
) {
  candidates <- unique(c(
    tools::file_path_sans_ext(basename(source_name)),
    as.character(sheet %||% "")
  ))
  hits <- known_levels[tolower(known_levels) %in% tolower(candidates)]
  if (length(hits) == 1L) hits else NULL
}

builder_marker_import_normalize <- function(table, group, values) {
  table[[group]] <- as.character(values)
  table <- table[c(group, setdiff(names(table), group))]
  rownames(table) <- NULL
  table
}

builder_marker_import_mapping_error <- function(source, error) {
  source$table <- NULL
  source$valid <- FALSE
  source$error <- error
  source$levels <- character()
  source
}

builder_marker_import_map_single <- function(
  source,
  group,
  level,
  known_levels
) {
  if (!isTRUE(source$valid) || !is.data.frame(source$table)) {
    return(builder_marker_import_mapping_error(source, "unusable_table"))
  }
  level <- trimws(as.character(level %||% ""))
  if (length(level) != 1L || is.na(level) || !nzchar(level)) {
    return(builder_marker_import_mapping_error(source, "missing_cluster"))
  }
  if (!level %in% known_levels) {
    return(builder_marker_import_mapping_error(source, "unknown_cluster"))
  }
  source$table <- builder_marker_import_normalize(source$table, group, level)
  source$columns <- names(source$table)
  source$group <- group
  source$levels <- level
  source$mapping <- "single"
  source$error <- NULL
  source
}

builder_marker_import_map_multiple <- function(
  source,
  group,
  column,
  known_levels
) {
  if (!isTRUE(source$valid) || !is.data.frame(source$table)) {
    return(builder_marker_import_mapping_error(source, "unusable_table"))
  }
  column <- as.character(column %||% "")
  if (
    length(column) != 1L || is.na(column) || !column %in% names(source$table)
  ) {
    return(builder_marker_import_mapping_error(
      source,
      "missing_cluster_column"
    ))
  }
  values <- as.character(source$table[[column]])
  if (anyNA(values) || any(!nzchar(trimws(values)))) {
    return(builder_marker_import_mapping_error(source, "missing_cluster"))
  }
  levels <- unique(values)
  if (any(!levels %in% known_levels)) {
    return(builder_marker_import_mapping_error(source, "unknown_cluster"))
  }
  source$table <- builder_marker_import_normalize(source$table, group, values)
  source$columns <- names(source$table)
  source$group <- group
  source$levels <- levels
  source$mapping <- "multiple"
  source$error <- NULL
  source
}

builder_marker_import_validate_sources <- function(sources, known_levels) {
  if (!length(sources)) {
    return(list(valid = FALSE, error = "empty_import"))
  }
  valid <- vapply(sources, function(source) isTRUE(source$valid), logical(1))
  if (!all(valid)) {
    return(list(valid = FALSE, error = "unresolved_source"))
  }
  levels <- unlist(lapply(sources, `[[`, "levels"), use.names = FALSE)
  if (any(!levels %in% known_levels)) {
    return(list(valid = FALSE, error = "unknown_cluster"))
  }
  if (anyDuplicated(levels)) {
    return(list(valid = FALSE, error = "duplicate_cluster"))
  }
  list(valid = TRUE, error = NULL, sources = sources)
}

builder_marker_import_coverage <- function(sources, known_levels) {
  present <- unique(unlist(
    lapply(sources, function(source) {
      as.character(source$levels %||% character())
    }),
    use.names = FALSE
  ))
  list(
    known = as.character(known_levels),
    present = present,
    missing = setdiff(as.character(known_levels), present)
  )
}

builder_marker_import_bind_tables <- function(sources) {
  tables <- lapply(sources, `[[`, "table")
  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  aligned <- lapply(tables, function(table) {
    missing <- setdiff(columns, names(table))
    for (column in missing) {
      table[[column]] <- NA
    }
    table[columns]
  })
  result <- do.call(rbind, aligned)
  rownames(result) <- NULL
  result
}

builder_attach_marker_imports <- function(object, imports) {
  if (!methods::is(object, "Seurat") || !length(imports)) {
    return(object)
  }
  existing <- object@misc$marker_genes %||% list()
  for (imported in imports) {
    method <- imported$method %||% ""
    group <- imported$group %||% ""
    if (!is.character(method) || length(method) != 1L || !nzchar(method)) {
      stop("Imported Marker genes require one method name.", call. = FALSE)
    }
    if (!is.character(group) || length(group) != 1L || !nzchar(group)) {
      stop("Imported Marker genes require one group.", call. = FALSE)
    }
    if (method %in% names(existing)) {
      stop(
        "An imported Marker genes method already exists: ",
        method,
        call. = FALSE
      )
    }
    checked <- builder_marker_import_validate_sources(
      imported$sources %||% list(),
      unique(unlist(lapply(imported$sources %||% list(), `[[`, "levels")))
    )
    if (!isTRUE(checked$valid)) {
      stop("Imported Marker genes sources are invalid.", call. = FALSE)
    }
    existing[[method]] <- list()
    existing[[method]][[group]] <- builder_marker_import_bind_tables(
      checked$sources
    )
  }
  object@misc$marker_genes <- existing
  object
}
