## Small, inert statistic frames derived only from verified profile/plan state.

BUILDER_STATS_QC_MAX <- 1000L
BUILDER_STATS_GROUP_MAX <- 12L

.builder_stats_count <- function(profile, legacy, typed) {
  value <- profile[[legacy]]
  if (is.null(value)) {
    value <- profile$identity[[typed]]$count
  }
  as.integer(value %||% 0L)
}

.builder_stats_distribution <- function(
  counts,
  limit = BUILDER_STATS_GROUP_MAX
) {
  source_counts <- counts %||% integer()
  count_names <- names(source_counts) %||%
    as.character(seq_along(source_counts))
  counts <- as.integer(source_counts)
  names(counts) <- count_names
  counts <- counts[!is.na(counts) & counts >= 0L]
  counts <- sort(counts, decreasing = TRUE, method = "radix")
  if (!length(counts)) {
    return(data.frame(bucket = character(), count = integer()))
  }
  if (length(counts) > limit) {
    counts <- c(counts[seq_len(limit)], Other = sum(counts[-seq_len(limit)]))
  }
  data.frame(
    bucket = names(counts),
    count = unname(counts),
    stringsAsFactors = FALSE
  )
}

builder_stats_frame <- function(profile, plan = list()) {
  stopifnot(is.list(profile), is.list(plan))
  group <- (plan$groups %||% plan$default_group %||% character())[1L]
  counts <- if (length(group)) profile$group_counts[[group]] else integer()

  selected_reductions <- plan$reductions %||% names(profile$reductions)
  projection_rows <- lapply(selected_reductions, function(name) {
    typed_reductions <- is.list(profile$reductions)
    item <- if (typed_reductions) {
      profile$reductions[[name]] %||% list()
    } else {
      list()
    }
    data.frame(
      projection = name,
      dimensions = as.integer(item$dimensions %||% NA_integer_),
      cells = as.integer(
        item$cells$count %||% .builder_stats_count(profile, "n_cells", "cells")
      ),
      valid = if (typed_reductions) {
        isTRUE(item$structurally_valid %||% item$exportable)
      } else {
        name %in% as.character(profile$reductions %||% character())
      },
      stringsAsFactors = FALSE
    )
  })
  projections <- if (length(projection_rows)) {
    do.call(rbind, projection_rows)
  } else {
    data.frame(
      projection = character(),
      dimensions = integer(),
      cells = integer(),
      valid = logical()
    )
  }

  qc_fields <- unique(c(plan$nUMI, plan$nGene))
  qc_fields <- qc_fields[!is.na(qc_fields) & nzchar(qc_fields)]
  qc_rows <- lapply(qc_fields, function(field) {
    values <- as.numeric(profile$qc_values[[field]] %||% numeric())
    values <- values[is.finite(values)]
    if (length(values) > BUILDER_STATS_QC_MAX) {
      index <- unique(round(seq(
        1,
        length(values),
        length.out = BUILDER_STATS_QC_MAX
      )))
      values <- values[index]
    }
    data.frame(field = field, value = values, stringsAsFactors = FALSE)
  })
  qc <- if (length(qc_rows)) {
    do.call(rbind, qc_rows)
  } else {
    data.frame(field = character(), value = numeric())
  }

  spatial <- profile$spatial %||%
    list(
      section_count = length(profile$images %||% character())
    )
  structure(
    list(
      cells = .builder_stats_count(profile, "n_cells", "cells"),
      genes = .builder_stats_count(profile, "n_genes", "features"),
      groups = as.integer(length(counts)),
      group_distribution = .builder_stats_distribution(counts),
      projections = projections,
      spatial = data.frame(
        sections = as.integer(
          spatial$section_count %||% length(spatial$sections %||% character())
        ),
        stringsAsFactors = FALSE
      ),
      qc = qc,
      qc_samples = as.integer(if (nrow(qc)) max(table(qc$field)) else 0L)
    ),
    class = c("builder_stats_frame", "list")
  )
}
