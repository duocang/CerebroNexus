#' Prepare external tables for a generated Viewer
#'
#' @keywords internal
#' @noRd
.bundleExtraTables <- function(
  extra_tables = NULL,
  extra_tables_sheets = NULL
) {
  if (is.null(extra_tables)) {
    if (!is.null(extra_tables_sheets)) {
      stop("`extra_tables_sheets` requires `extra_tables`.", call. = FALSE)
    }
    return(NULL)
  }

  paths <- if (is.list(extra_tables)) {
    vapply(
      extra_tables,
      function(path) {
        if (
          !is.character(path) ||
            length(path) != 1L ||
            is.na(path) ||
            !nzchar(path)
        ) {
          stop(
            "Every extra_tables entry must be one non-empty file path.",
            call. = FALSE
          )
        }
        path
      },
      character(1)
    )
  } else {
    extra_tables
  }
  labels <- names(paths)
  if (
    !is.character(paths) ||
      length(paths) == 0L ||
      is.null(labels) ||
      anyNA(labels) ||
      any(!nzchar(labels)) ||
      anyDuplicated(labels) ||
      anyNA(paths) ||
      any(!nzchar(paths))
  ) {
    stop(
      "`extra_tables` must be a named collection of one non-empty file paths.",
      call. = FALSE
    )
  }

  extensions <- tolower(tools::file_ext(paths))
  if (any(!extensions %in% c("csv", "tsv", "txt", "xls", "xlsx", "xlsm"))) {
    stop("extra_tables contains an unsupported file extension.", call. = FALSE)
  }
  if (any(!file.exists(paths))) {
    stop("Extra table file(s) not found.", call. = FALSE)
  }
  if (any(!utils::file_test("-f", paths))) {
    stop(
      "Each extra_tables entry must be an existing regular file.",
      call. = FALSE
    )
  }

  sheet_maps <- .extraTableSheetMaps(extra_tables_sheets, labels, extensions)
  files <- lapply(seq_along(paths), function(file_index) {
    path <- paths[[file_index]]
    extension <- extensions[[file_index]]
    tables <- if (extension %in% c("csv", "tsv", "txt")) {
      stats::setNames(
        list(.readExtraDelimited(path, extension)),
        labels[[file_index]]
      )
    } else {
      .readExtraWorkbook(path, labels[[file_index]])
    }
    sheet_indices <- attr(tables, "sheet_indices")
    if (is.null(sheet_indices)) {
      sheet_indices <- seq_along(tables)
    }
    mapping <- sheet_maps[[labels[[file_index]]]]
    if (!is.null(mapping)) {
      missing <- setdiff(unname(mapping), names(tables))
      if (length(missing) > 0L) {
        stop(
          "Mapped source sheet `",
          missing[[1L]],
          "` was not found or is empty.",
          call. = FALSE
        )
      }
      collisions <- intersect(
        names(mapping),
        setdiff(names(tables), unname(mapping))
      )
      if (length(collisions) > 0L) {
        stop(
          "Mapped final label `",
          collisions[[1L]],
          "` collides with an unmapped source sheet.",
          call. = FALSE
        )
      }
      final_labels <- names(tables)
      final_labels[match(unname(mapping), names(tables))] <- names(mapping)
    } else {
      final_labels <- names(tables)
    }
    list(
      key = paste0("external-file:", file_index),
      sheets = Map(
        function(table, label, sheet_index) {
          list(
            key = paste("external", file_index, sheet_index, sep = ":"),
            label = label,
            table = table
          )
        },
        tables,
        final_labels,
        sheet_indices
      )
    )
  })
  names(files) <- labels
  list(files = files)
}

.extraTableSheetMaps <- function(maps, labels, extensions) {
  result <- stats::setNames(vector("list", length(labels)), labels)
  if (is.null(maps)) {
    return(result)
  }
  if (
    !is.list(maps) ||
      is.null(names(maps)) ||
      anyNA(names(maps)) ||
      any(!nzchar(names(maps))) ||
      anyDuplicated(names(maps))
  ) {
    stop("`extra_tables_sheets` must be a named list.", call. = FALSE)
  }
  unknown <- setdiff(names(maps), labels)
  if (length(unknown) > 0L) {
    stop(
      "extra_tables_sheets file label `",
      unknown[[1L]],
      "` is not present in extra_tables.",
      call. = FALSE
    )
  }
  for (file_label in names(maps)) {
    if (
      !extensions[[match(file_label, labels)]] %in% c("xls", "xlsx", "xlsm")
    ) {
      stop("extra_tables_sheets can only map Excel files.", call. = FALSE)
    }
    mapping <- maps[[file_label]]
    mapping_names <- names(mapping)
    if (
      !is.list(mapping) ||
        is.null(mapping_names) ||
        anyNA(mapping_names) ||
        any(!nzchar(mapping_names))
    ) {
      stop(
        "Each extra_tables_sheets file mapping must be a named list.",
        call. = FALSE
      )
    }
    if (anyDuplicated(mapping_names)) {
      stop(
        "Each file mapping must not contain a duplicate final label.",
        call. = FALSE
      )
    }
    sources <- vapply(
      mapping,
      function(source) {
        if (
          !is.character(source) ||
            length(source) != 1L ||
            is.na(source) ||
            !nzchar(source)
        ) {
          stop(
            "Each mapped source sheet must be one non-empty name.",
            call. = FALSE
          )
        }
        source
      },
      character(1)
    )
    if (anyDuplicated(sources)) {
      stop(
        "Each file mapping must not contain a duplicate source sheet.",
        call. = FALSE
      )
    }
    result[[file_label]] <- stats::setNames(sources, mapping_names)
  }
  result
}

.readExtraDelimited <- function(path, extension) {
  utils::read.table(
    path,
    header = TRUE,
    sep = if (extension == "csv") "," else "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    comment.char = ""
  )
}

.readExtraWorkbook <- function(path, file_label) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Reading Excel extra tables requires the readxl package.",
      call. = FALSE
    )
  }
  workbook <- tryCatch(
    {
      sheets <- readxl::excel_sheets(path)
      list(
        sheets = sheets,
        tables = lapply(sheets, function(sheet) {
          readxl::read_excel(path, sheet = sheet)
        })
      )
    },
    error = function(error) {
      stop(
        "Unable to read Excel extra table `",
        file_label,
        "`.",
        call. = FALSE
      )
    }
  )
  non_empty <- vapply(workbook$tables, nrow, integer(1)) > 0L
  tables <- lapply(workbook$tables[non_empty], as.data.frame)
  names(tables) <- workbook$sheets[non_empty]
  attr(tables, "sheet_indices") <- which(non_empty)
  tables
}
