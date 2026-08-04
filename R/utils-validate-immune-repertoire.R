#' Check immune repertoire data against the shape the app consumes
#'
#' The app reads a repertoire as a list named by sample, each element a
#' data.frame carrying `barcode` -- the only key joining a receptor to a cell --
#' plus the four standard scRepertoire clone-call columns `CTgene`, `CTnt`,
#' `CTaa`, and `CTstrict`.
#'
#' Nothing enforced that. A flat data.frame satisfies `is.list()`, and its
#' `length()` is its column count, so the old guard waved one through and its
#' column names became the sample names. The file exported intact and came
#' apart only in the running app, where the sample dropdown lists `barcode`,
#' `CTgene`, `CTaa` and every panel is empty.
#'
#' The check belongs at the export boundary rather than on the R6 setter:
#' `immune_repertoire` is a public field, so a setter is trivially bypassed by
#' assigning to it, and most scripts in `data-raw/` do exactly that.
#'
#' Structural problems stop the export -- there is no reading the data without
#' them. Problems that merely degrade the page warn, because a partly usable
#' repertoire is still worth exporting.
#'
#' @param data The object found in the slot.
#' @param cell_barcodes The object's cell names, when known, for the join check.
#' @param source_label How to refer to `data` in messages, e.g.
#'   "`@misc$immune_repertoire`" or "the `tcr` argument".
#'
#' @return `data`, invisibly.
#'
#' @keywords internal
#' @noRd
.validateImmuneRepertoire <- function(
  data,
  cell_barcodes = NULL,
  source_label = "`@misc$immune_repertoire`"
) {
  shape_advice <- paste0(
    "Immune repertoire data has to be a named list of data.frames, one per ",
    "sample -- the names become the sample labels in the app. ",
    "combineTCR() and combineBCR() return exactly that. ",
    "For a single sample, write list(sample1 = your_data_frame)."
  )

  if (is.data.frame(data)) {
    stop(
      source_label,
      " is a single data.frame. ",
      shape_advice,
      call. = FALSE
    )
  }

  if (!is.list(data)) {
    stop(
      source_label,
      " is a ",
      class(data)[1],
      ". ",
      shape_advice,
      call. = FALSE
    )
  }

  if (length(data) == 0) {
    return(invisible(data))
  }

  sample_names <- names(data)
  if (
    is.null(sample_names) ||
      any(is.na(sample_names)) ||
      !all(nzchar(sample_names))
  ) {
    stop(
      source_label,
      " has entries without a name. Each entry is one sample and its name is ",
      "the label the app shows in the sample selector, so an unnamed entry ",
      "cannot be offered. ",
      shape_advice,
      call. = FALSE
    )
  }

  ## `x[["donorA"]]` returns the first match and nothing says the others exist,
  ## so a duplicated name means an entry is unreachable -- the shape most often
  ## produced by concatenating a TCR list and a BCR list that describe the same
  ## samples instead of row-binding them per sample.
  if (anyDuplicated(sample_names) > 0) {
    repeated <- unique(sample_names[duplicated(sample_names)])
    stop(
      source_label,
      " has more than one entry named: ",
      paste(repeated, collapse = ", "),
      ". A name identifies one biological sample, and only the first entry ",
      "with a given name is ever read, so the others would be lost. If this ",
      "is TCR and BCR data for the same samples, row-bind them per sample ",
      "into one data.frame -- a cell carries one or the other, never both.",
      call. = FALSE
    )
  }

  required_columns <- c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict")
  for (index in seq_along(data)) {
    element <- data[[index]]
    label <- sample_names[index]

    if (!is.data.frame(element)) {
      stop(
        source_label,
        " entry `",
        label,
        "` is a ",
        class(element)[1],
        ", not a data.frame. ",
        shape_advice,
        call. = FALSE
      )
    }

    missing_columns <- setdiff(required_columns, names(element))
    if (length(missing_columns) > 0L) {
      stop(
        source_label,
        " entry `",
        label,
        "` is missing required scRepertoire column(s): ",
        paste(missing_columns, collapse = ", "),
        ". `barcode` joins receptors to cells; CTgene/CTnt/CTaa/CTstrict ",
        "back the clone-call choices and sequence analyses in the app. ",
        "Columns found: ",
        paste(names(element), collapse = ", "),
        call. = FALSE
      )
    }

    for (column_name in required_columns) {
      column <- element[[column_name]]
      if (
        !is.atomic(column) ||
          !is.null(dim(column)) ||
          length(column) != nrow(element)
      ) {
        stop(
          source_label,
          " entry `",
          label,
          "` column `",
          column_name,
          "` must be a one-dimensional atomic vector with one value per row; ",
          "received class ",
          paste(class(column), collapse = "/"),
          ", length ",
          length(column),
          ", for ",
          nrow(element),
          " row(s).",
          call. = FALSE
        )
      }
    }

    barcodes <- as.character(element[["barcode"]])
    invalid_barcodes <- is.na(barcodes) | !nzchar(barcodes)
    if (any(invalid_barcodes)) {
      stop(
        source_label,
        " entry `",
        label,
        "` has ",
        sum(invalid_barcodes),
        " missing or empty barcode value(s). Every repertoire row must name ",
        "the cell it belongs to.",
        call. = FALSE
      )
    }
    if (anyDuplicated(barcodes) > 0L) {
      repeated <- unique(barcodes[duplicated(barcodes)])
      stop(
        source_label,
        " entry `",
        label,
        "` has more than one row for barcode(s): ",
        paste(utils::head(repeated, 5L), collapse = ", "),
        ". The app and scRepertoire treat one row as one cell, so duplicate ",
        "rows would inflate clone sizes.",
        call. = FALSE
      )
    }
  }

  all_barcodes <- unlist(
    lapply(data, function(element) as.character(element[["barcode"]])),
    use.names = FALSE
  )
  if (anyDuplicated(all_barcodes) > 0L) {
    repeated <- unique(all_barcodes[duplicated(all_barcodes)])
    stop(
      source_label,
      " assigns barcode(s) to more than one sample: ",
      paste(utils::head(repeated, 5L), collapse = ", "),
      ". Cell barcodes must be globally unique across the repertoire list.",
      call. = FALSE
    )
  }

  if (!is.null(cell_barcodes) && length(cell_barcodes) > 0) {
    per_sample_overlap <- vapply(
      data,
      function(df) {
        length(intersect(unique(as.character(df[["barcode"]])), cell_barcodes))
      },
      integer(1)
    )
    repertoire_barcodes <- unique(all_barcodes)
    total_overlap <- sum(per_sample_overlap)

    ## No overlap at all is not a degraded page, it is no page: every receptor
    ## is orphaned, and the documented contract -- barcodes are the object's
    ## cell names -- is simply not met. The usual cause is `combineTCR(samples =)`
    ## prefixing barcodes without `RenameCells()` on the object to match.
    if (total_overlap == 0) {
      stop(
        source_label,
        " shares no barcode with the object's cells: 0 of ",
        length(repertoire_barcodes),
        " repertoire barcodes match any of ",
        length(cell_barcodes),
        " cell names, so no receptor could ever be tied to a cell. ",
        "combineTCR(samples = ) and combineBCR(samples = ) prefix barcodes ",
        "with the sample name -- if they did, the same prefix has to be on ",
        "the cell names, via SeuratObject::RenameCells(). ",
        "Repertoire barcodes look like: ",
        paste(utils::head(repertoire_barcodes, 2), collapse = ", "),
        "; cell names look like: ",
        paste(utils::head(cell_barcodes, 2), collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(data)
}

.normalizeImmuneRepertoire <- function(
  data,
  cell_barcodes = NULL,
  source_label = "`@misc$immune_repertoire`"
) {
  .validateImmuneRepertoire(
    data,
    cell_barcodes = cell_barcodes,
    source_label = source_label
  )
  if (is.null(data) || length(data) == 0L) {
    return(data)
  }

  required_columns <- c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict")
  normalized <- lapply(data, function(element) {
    for (column_name in required_columns) {
      element[[column_name]] <- as.character(element[[column_name]])
    }
    element
  })

  if (!is.null(cell_barcodes) && length(cell_barcodes) > 0L) {
    original_rows <- vapply(normalized, nrow, integer(1))
    normalized <- lapply(normalized, function(element) {
      element[element$barcode %in% cell_barcodes, , drop = FALSE]
    })
    retained_rows <- vapply(normalized, nrow, integer(1))
    removed_rows <- original_rows - retained_rows
    if (any(removed_rows > 0L)) {
      removed_samples <- names(normalized)[retained_rows == 0L]
      sample_note <- if (length(removed_samples) > 0L) {
        paste0(
          " Sample(s) matching no cell were removed: ",
          paste(removed_samples, collapse = ", "),
          "."
        )
      } else {
        ""
      }
      warning(
        source_label,
        " removed ",
        sum(removed_rows),
        " repertoire row(s) whose barcodes are absent from the object.",
        sample_note,
        call. = FALSE
      )
    }
  }

  .dropEmptyRepertoireSamples(normalized)
}

.dropEmptyRepertoireSamples <- function(data) {
  if (is.null(data) || !is.list(data) || is.data.frame(data)) {
    return(data)
  }
  data[vapply(
    data,
    function(element) !is.data.frame(element) || nrow(element) > 0L,
    logical(1)
  )]
}
