# Compact, expression-value-free summaries shared by bundle creation and Viewer.
viewerOverviewText <- function(value) {
  if (
    length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))
  ) {
    return(NULL)
  }
  trimws(as.character(value))
}

viewerDatasetOverview <- function(
  object,
  gene_count = NULL,
  projections = NULL
) {
  safely <- function(expr, fallback = NULL) {
    tryCatch(expr, error = function(e) fallback)
  }
  experiment <- safely(object$getExperiment(), list())
  # Summaries never need cell names. A thin CRB that the Viewer has not yet
  # hydrated keeps its stored table in the first-frame cache; reading
  # `meta_data` itself would first read every cell name from the H5 file.
  stored <- if (is.environment(object)) {
    frame <- attr(object, "cerebro_projection_first_frame", exact = TRUE)
    if (is.list(frame) && is.data.frame(frame$meta_data)) {
      frame$meta_data
    } else {
      safely(object$meta_data)
    }
  }
  metadata <- if (is.data.frame(stored)) {
    stored
  } else {
    safely(object$getMetaData(), data.frame())
  }
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
    # tabulate() reads a factor's codes in place; as.integer() would copy
    # every column, and those copies trigger a full garbage collection that
    # costs about a second in a Viewer process.
    raw <- metadata[[column]]
    if (is.factor(raw)) {
      distinct <- levels(raw)
      codes <- raw
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
      counts = stats::setNames(as.numeric(tally[by_size]), distinct[by_size])
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
  # The accessor of a thin CRB would hydrate it; a caller that already holds
  # the names (the Viewer's first-frame cache) passes them in.
  projections <- if (is.null(projections)) {
    safely(as.character(object$availableProjections()), character())
  } else {
    as.character(projections)
  }
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
