# Compact, expression-value-free summaries shared by bundle creation and Viewer.
viewerOverviewText <- function(value) {
  if (
    length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))
  ) {
    return(NULL)
  }
  trimws(as.character(value))
}

viewerDatasetOverview <- function(object, gene_count = NULL) {
  safely <- function(expr, fallback = NULL) {
    tryCatch(expr, error = function(e) fallback)
  }
  experiment <- safely(object$getExperiment(), list())
  metadata <- safely(object$getMetaData(), data.frame())
  groups <- intersect(safely(object$getGroups(), character()), names(metadata))
  # Only explicit sample metadata or the canonical sample column is used.
  sample_column <- viewerOverviewText(experiment$sample_column)
  if (is.null(sample_column) && "sample" %in% names(metadata)) {
    sample_column <- "sample"
  }
  if (!is.null(sample_column) && !sample_column %in% names(metadata)) {
    sample_column <- NULL
  }
  summarize <- function(column) {
    # Count on integer codes and clean only the distinct values: a million-cell
    # column has a handful of levels, so trimming every cell is wasted work.
    raw <- metadata[[column]]
    if (is.factor(raw)) {
      distinct <- levels(raw)
      codes <- as.integer(raw)
    } else {
      distinct <- unique(raw)
      codes <- match(raw, distinct)
    }
    distinct <- as.character(distinct)
    usable <- !is.na(distinct) & nzchar(trimws(distinct))
    tally <- tabulate(codes, nbins = length(distinct))
    missing <- length(codes) - sum(tally[usable])
    keep <- usable & tally > 0L
    if (!any(keep)) {
      return(NULL)
    }
    distinct <- distinct[keep]
    tally <- tally[keep]
    by_name <- order(distinct)
    distinct <- distinct[by_name]
    tally <- tally[by_name]
    by_size <- order(tally, decreasing = TRUE)
    list(
      column = column,
      levels = length(distinct),
      total = sum(tally),
      missing = missing,
      counts = stats::setNames(as.numeric(tally[by_size]), distinct[by_size]),
      remaining = 0
    )
  }
  sample <- if (!is.null(sample_column)) summarize(sample_column) else NULL
  annotations <- lapply(setdiff(groups, sample_column), summarize)
  names(annotations) <- setdiff(groups, sample_column)
  annotations <- Filter(Negate(is.null), annotations)
  # A caller that already knows the gene count (the Viewer reads it from the
  # H5 file) avoids attaching the expression matrix just to count its rows.
  genes <- if (is.null(gene_count)) {
    safely(length(object$getGeneNames()))
  } else {
    as.integer(gene_count)
  }
  if (is.null(genes) || genes == 0L) {
    genes <- NULL
  }
  spatial <- safely(as.character(object$availableSpatial()), character())
  projections <- safely(
    as.character(object$availableProjections()),
    character()
  )
  list(
    version = 2L,
    description = viewerOverviewText(experiment$description),
    source = viewerOverviewText(experiment$source),
    experiment = viewerOverviewText(experiment$experiment_name),
    genes = genes,
    samples = if (!is.null(sample)) sample$levels else NULL,
    spatial = spatial,
    projections = projections,
    sample = sample,
    annotations = annotations
  )
}

viewerOverviewTruncated <- function(overview) {
  summaries <- c(list(overview$sample), overview$annotations)
  any(vapply(
    summaries,
    function(summary) {
      !is.null(summary) &&
        (isTRUE(summary$remaining > 0) ||
          isTRUE(length(summary$counts) < summary$levels))
    },
    logical(1)
  ))
}
