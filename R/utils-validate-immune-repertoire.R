#' Check immune repertoire data against the shape the app consumes
#'
#' The app reads a repertoire as a list named by sample, each element a
#' data.frame carrying at least `barcode` -- the only key joining a receptor to
#' a cell -- and `CTgene`, which chain detection scans for TRA/TRB/TRG/TRD and
#' IGH/IGK/IGL.
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
#' @param zero_overlap What to do when not one barcode matches a cell.
#'   `"error"` when the caller supplied the data on purpose, `"warning"` when
#'   the export merely discovered it on the object.
#'
#' @return `data`, invisibly.
#'
#' @keywords internal
#' @noRd
.validateImmuneRepertoire <- function(
  data,
  cell_barcodes = NULL,
  source_label = "`@misc$immune_repertoire`",
  zero_overlap = c("error", "warning")
) {
  zero_overlap <- match.arg(zero_overlap)
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

    if (!("barcode" %in% names(element))) {
      stop(
        source_label,
        " entry `",
        label,
        "` has no `barcode` column. The barcode is the only key joining a ",
        "receptor to a cell; without it the repertoire and the cells can ",
        "never be matched up. Columns found: ",
        paste(names(element), collapse = ", "),
        call. = FALSE
      )
    }
  }

  ## Shape checking ends here. What follows compares the repertoire against
  ## the object's cells: a missing column only makes the page poorer, while
  ## the severity of "no barcode matches any cell" is the caller's to choose
  ## -- see `zero_overlap`.
  missing_ctgene <- sample_names[
    !vapply(data, function(df) "CTgene" %in% names(df), logical(1))
  ]
  if (length(missing_ctgene) > 0) {
    warning(
      source_label,
      " entries without a `CTgene` column: ",
      paste(missing_ctgene, collapse = ", "),
      ". Chain detection scans that column for TRA/TRB/TRG/TRD and ",
      "IGH/IGK/IGL, so no chain will be recognised for them.",
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
    repertoire_barcodes <- unique(unlist(
      lapply(data, function(df) as.character(df[["barcode"]])),
      use.names = FALSE
    ))
    total_overlap <- sum(per_sample_overlap)

    ## No overlap at all means no receptor can ever be tied to a cell. How bad
    ## that is depends on who is asking. Someone handing repertoire data in
    ## deliberately wants it refused. An export that merely found a slot on the
    ## object cannot assume the same: subsetting a Seurat object keeps `@misc`,
    ## so a perfectly ordinary "run TCR, then keep one compartment" workflow
    ## arrives here with a stale slot, and refusing it would block an export
    ## that used to succeed with an empty repertoire page.
    ##
    ## The message stays neutral about the cause, because both explanations are
    ## common and the data cannot tell them apart.
    if (total_overlap == 0) {
      overlap_message <- paste0(
        source_label,
        " shares no barcode with the object's cells: 0 of ",
        length(repertoire_barcodes),
        " repertoire barcodes match any of ",
        length(cell_barcodes),
        " cell names, so no receptor can be tied to a cell. ",
        "Either the barcodes are prefixed differently -- combineTCR(samples = ) ",
        "and combineBCR(samples = ) prefix them with the sample name, which ",
        "then has to be on the cell names too via SeuratObject::RenameCells() ",
        "-- or the object was subset after the repertoire was built, leaving a ",
        "repertoire for cells that are no longer present. ",
        "Repertoire barcodes look like: ",
        paste(utils::head(repertoire_barcodes, 2), collapse = ", "),
        "; cell names look like: ",
        paste(utils::head(cell_barcodes, 2), collapse = ", "),
        "."
      )
      if (identical(zero_overlap, "error")) {
        stop(overlap_message, call. = FALSE)
      }
      warning(overlap_message, call. = FALSE)
    }

    ## Some receptors not matching a cell is ordinary -- cells get filtered
    ## after the repertoire is combined. A whole sample matching nothing while
    ## its neighbours match is not: that is a naming problem confined to one
    ## sample, and it would leave that sample silently empty.
    empty_samples <- sample_names[per_sample_overlap == 0]
    if (length(empty_samples) > 0) {
      warning(
        source_label,
        " has sample(s) whose barcodes match no cell: ",
        paste(empty_samples, collapse = ", "),
        ". Their receptors will be orphaned while the other samples' are not, ",
        "which usually means those entries are named or prefixed differently.",
        call. = FALSE
      )
    }
  }

  invisible(data)
}
