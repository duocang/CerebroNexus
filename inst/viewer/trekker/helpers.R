trekker_gene_suggest <- function(tk, gene_names) {
  supplied <- unique(c(
    as.character(tk$cluster_markers$gene %||% character()),
    as.character(tk$moran$gene %||% character())
  ))
  supplied[supplied %in% gene_names]
}

trekker_entity_label <- function(tk, plural = FALSE) {
  label <- as.character(tk$entity_type %||% "observation")
  if (length(label) != 1L || is.na(label) || !nzchar(label)) {
    label <- "observation"
  }
  if (!plural) {
    return(label)
  }
  if (identical(label, "nucleus")) "nuclei" else paste0(label, "s")
}

trekker_recovery_status <- function(value) {
  value <- suppressWarnings(as.integer(value))
  labels <- c(
    `0` = "Original positioning",
    `1` = "Bead-removal recovery",
    `2` = "Second-pass recovery"
  )
  result <- unname(labels[as.character(value)])
  unknown <- is.na(result) & !is.na(value)
  result[unknown] <- paste("Status", value[unknown])
  result
}

trekker_positioning_labels <- function() {
  c(
    number_clusters_o = "Candidate locations (initial)",
    number_clusters = "Candidate locations (final)",
    SB_total = "Spatial barcodes",
    SB_UMI_total = "Spatial barcode UMIs",
    SB_noise = "Noise spatial barcodes",
    SB_UMI_noise = "Noise spatial-barcode UMIs",
    SB_top_cluster = "Top-location spatial barcodes",
    SB_UMI_top_cluster = "Top-location spatial-barcode UMIs",
    umimax = "Maximum spatial-barcode UMI",
    proportion_SB_noise = "Noise barcode fraction",
    proportion_SB_UMI_noise = "Noise UMI fraction",
    proportion_SB_top_cluster = "Top-location barcode fraction",
    proportion_SB_UMI_top_cluster = "Top-location UMI fraction",
    rep_status = "Recovery status"
  )
}

trekker_positioning_metadata <- function(positioning) {
  if (
    is.null(positioning) ||
      !is.data.frame(positioning) ||
      !all(c("barcode", "source_barcode") %in% names(positioning))
  ) {
    return(NULL)
  }
  labels <- trekker_positioning_labels()
  fields <- intersect(names(labels), names(positioning))
  if (!length(fields)) {
    return(NULL)
  }
  result <- positioning[, fields, drop = FALSE]
  if ("rep_status" %in% fields) {
    result$rep_status <- trekker_recovery_status(result$rep_status)
  }
  names(result) <- paste0("Trekker: ", unname(labels[fields]))
  rownames(result) <- as.character(positioning$barcode)
  result
}

trekker_positioning_record <- function(positioning, barcode) {
  if (
    is.null(positioning) ||
      !is.data.frame(positioning) ||
      length(barcode) != 1L ||
      !"barcode" %in% names(positioning)
  ) {
    return(data.frame())
  }
  at <- match(as.character(barcode), as.character(positioning$barcode))
  if (is.na(at)) {
    return(data.frame())
  }
  labels <- c(source_barcode = "Source barcode", trekker_positioning_labels())
  fields <- intersect(names(labels), names(positioning))
  values <- positioning[at, fields, drop = FALSE]
  if ("rep_status" %in% fields) {
    values$rep_status <- trekker_recovery_status(values$rep_status)
  }
  data.frame(
    Field = unname(labels[fields]),
    Value = as.character(unlist(values, use.names = FALSE)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

trekker_selection_ids <- function(selection) {
  if (is.null(selection)) {
    return(character())
  }
  ids <- if (is.list(selection) && !is.null(selection$ids)) {
    selection$ids
  } else {
    selection
  }
  as.character(ids)
}

trekker_metric_value <- function(metrics, aliases) {
  if (
    is.null(metrics) ||
      !all(c("Metrics", "Value") %in% names(metrics)) ||
      !nrow(metrics)
  ) {
    return(NA_real_)
  }
  at <- match(tolower(aliases), tolower(as.character(metrics$Metrics)))
  at <- at[!is.na(at)]
  if (!length(at)) {
    return(NA_real_)
  }
  value <- gsub("[,%[:space:]]", "", as.character(metrics$Value[[at[[1L]]]]))
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 1L && is.finite(value)) value else NA_real_
}

trekker_positioning_summary <- function(metrics) {
  values <- function(aliases) trekker_metric_value(metrics, aliases)
  compact <- function(frame) frame[is.finite(frame$value), , drop = FALSE]
  funnel <- compact(data.frame(
    stage = c(
      "Input nuclei",
      "Found in Trekker",
      "Valid spatial barcodes",
      "Positioned",
      "Confidently positioned"
    ),
    value = c(
      values("Total_nuclei_from_single-nuclei_sequencing_library"),
      values(
        "Nuclei_from_single-nuclei_sequencing_library_found_in_Trekker_library"
      ),
      values(paste0(
        "Nuclei_from_single-nuclei_sequencing_library_found_in_",
        "Trekker_library_with_valid_spatial_barcodes"
      )),
      values(c("Nuclei_positioned", "Total_nuclei_positioned")),
      values(c(
        "Nuclei_confidently_positioned",
        "Total_nuclei_positioned_with_1_spatial_location"
      ))
    ),
    stringsAsFactors = FALSE
  ))
  distribution <- compact(data.frame(
    bucket = c("Invalid", "0", "1", "2", "3", "4+"),
    value = c(
      values("Nuclei_with_invalid_spatial_barcodes"),
      values("Nuclei_0"),
      values("Nuclei_1"),
      values("Nuclei_2"),
      values("Nuclei_3"),
      values(c("Nuclei_>=4", "Nuclei_o_>=4"))
    ),
    stringsAsFactors = FALSE
  ))
  qc <- compact(data.frame(
    metric = c(
      "proper_read_pairs",
      "matched_valid_barcodes",
      "positioned",
      "median_useful_reads"
    ),
    label = c(
      "Proper read pairs",
      "Matched valid barcodes",
      "Nuclei positioned",
      "Median useful reads per nucleus"
    ),
    value = c(
      values("Pct_readpairs_with_proper_structure"),
      values(paste0(
        "Pct_readpairs_matched_to_single_nuclei_barcodes_with_",
        "valid_spatial_barcodes"
      )),
      values("Pct_nuclei_positioned"),
      values("Median_useful_reads_per_nuclei")
    ),
    unit = c("%", "%", "%", ""),
    stringsAsFactors = FALSE
  ))
  list(funnel = funnel, distribution = distribution, qc = qc)
}

trekker_run_qc_details <- function(metrics) {
  values <- function(aliases) trekker_metric_value(metrics, aliases)
  compact <- function(frame) {
    frame <- frame[is.finite(frame$value), , drop = FALSE]
    rownames(frame) <- NULL
    frame
  }
  reads <- compact(data.frame(
    stage = c(
      "Total read pairs",
      "Proper structure",
      "Used for nucleus matching",
      "Matched nucleus barcodes",
      "Matched + valid spatial barcodes"
    ),
    value = c(
      values("Total_readpairs_in_Trekker_library"),
      values("Readpairs_with_proper_structure"),
      values("Readpairs_used_for_matching_to_single_nuclei_barcodes"),
      values("Readpairs_matched_single_nuclei_barcodes"),
      values(paste0(
        "Readpairs_matched_to_single_nuclei_barcodes_with_",
        "valid_spatial_barcodes"
      ))
    ),
    stringsAsFactors = FALSE
  ))
  rates <- compact(data.frame(
    metric = c(
      "Proper structure",
      "Matched nucleus barcodes",
      "Matched + valid spatial barcodes",
      "Valid spatial barcodes",
      "Useful reads"
    ),
    value = c(
      values("Pct_readpairs_with_proper_structure"),
      values("Pct_readpairs_matched_to_single_nuclei_barcodes"),
      values(paste0(
        "Pct_readpairs_matched_to_single_nuclei_barcodes_with_",
        "valid_spatial_barcodes"
      )),
      values("Pct_valid_spatial_barcodes"),
      values("Pct_useful_reads")
    ),
    stringsAsFactors = FALSE
  ))
  depth <- compact(data.frame(
    statistic = rep(c("Mean", "Median"), 4L),
    measure = rep(
      c("Reads", "Useful reads", "Spatial barcodes", "Spatial barcode UMIs"),
      each = 2L
    ),
    value = c(
      values("Mean_reads_per_nuclei"),
      values("Median_reads_per_nuclei"),
      values("Mean_useful_reads_per_nuclei"),
      values("Median_useful_reads_per_nuclei"),
      values("Mean_spatial_barcodes_per_nuclei"),
      values("Median_spatial_barcodes_per_nuclei"),
      values("Mean_spatial_barcodes_UMI_per_nuclei"),
      values("Median_spatial_barcodes_UMI_per_nuclei")
    ),
    stringsAsFactors = FALSE
  ))
  positioning <- compact(data.frame(
    stage = c(
      "Initially 1",
      "Initially 2",
      "Initially 3",
      "Initially 4+",
      "Salvaged from 2",
      "Salvaged from 3",
      "Salvaged from 4+",
      "Replicate: 1 location",
      "Replicate: 2+ locations",
      "Round 2: 1 location",
      "Round 2: 2+ locations"
    ),
    value = c(
      values("Nuclei_o_1"),
      values("Nuclei_o_2"),
      values("Nuclei_o_3"),
      values(c("Nuclei_o_>=4", "Nuclei_o_4")),
      values("Nuclei_salvaged_2"),
      values("Nuclei_salvaged_3"),
      values("Nuclei_salvaged_>=4"),
      values("nuclei_1_rep"),
      values("nuclei_2plus_rep"),
      values("nuclei_1_rep_round2"),
      values("nuclei_2plus_rep_round2")
    ),
    stringsAsFactors = FALSE
  ))
  list(reads = reads, rates = rates, depth = depth, positioning = positioning)
}

trekker_spatial_qc <- function(metadata, coordinates) {
  required <- c("barcode", "x", "y")
  if (
    is.null(metadata) ||
      is.null(rownames(metadata)) ||
      is.null(coordinates) ||
      !all(required %in% names(coordinates))
  ) {
    return(data.frame())
  }
  at <- match(as.character(coordinates$barcode), rownames(metadata))
  field <- function(candidates) {
    candidate <- candidates[candidates %in% names(metadata)]
    if (!length(candidate)) {
      return(rep(NA_real_, nrow(coordinates)))
    }
    suppressWarnings(as.numeric(metadata[[candidate[[1L]]]][at]))
  }
  count <- field(c("nCount_RNA", "nUMI"))
  feature <- field(c("nFeature_RNA", "nGene"))
  metrics <- list(
    "Mitochondrial UMI (%)" = field(c("percent.mt", "percent_mt")),
    "log10 RNA UMI" = if ("log10_nCount_RNA" %in% names(metadata)) {
      field("log10_nCount_RNA")
    } else {
      log10(pmax(count, 0) + 1)
    },
    "log10 detected genes" = if ("log10_nFeature_RNA" %in% names(metadata)) {
      field("log10_nFeature_RNA")
    } else {
      log10(pmax(feature, 0) + 1)
    }
  )
  rows <- lapply(names(metrics), function(metric) {
    data.frame(
      barcode = as.character(coordinates$barcode),
      x = suppressWarnings(as.numeric(coordinates$x)),
      y = suppressWarnings(as.numeric(coordinates$y)),
      metric = metric,
      value = metrics[[metric]],
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result <- result[
    is.finite(result$x) & is.finite(result$y) & is.finite(result$value),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}

trekker_top_markers <- function(markers, cluster, n = 20L) {
  if (is.null(markers) || !nrow(markers)) {
    return(data.frame())
  }
  rows <- markers[
    as.character(markers$cluster) == as.character(cluster),
    ,
    drop = FALSE
  ]
  if (!nrow(rows)) {
    return(rows)
  }
  rows$p_val_adj <- suppressWarnings(as.numeric(rows$p_val_adj))
  rows$avg_log2FC <- suppressWarnings(as.numeric(rows$avg_log2FC))
  rows$pct.1 <- suppressWarnings(as.numeric(rows$pct.1))
  rows$pct.2 <- suppressWarnings(as.numeric(rows$pct.2))
  order_index <- order(rows$p_val_adj, -rows$avg_log2FC, na.last = TRUE)
  if (!is.null(n)) {
    order_index <- utils::head(order_index, n)
  }
  rows[order_index, , drop = FALSE]
}

trekker_marker_dot <- function(
  markers,
  expression,
  groups,
  n_per_cluster = 3L
) {
  if (
    is.null(markers) ||
      !nrow(markers) ||
      is.null(expression) ||
      is.null(rownames(expression)) ||
      length(groups) != ncol(expression)
  ) {
    return(data.frame())
  }
  groups <- as.character(groups)
  clusters <- trekker_natural_levels(markers$cluster)
  selected <- do.call(
    rbind,
    lapply(clusters, function(cluster) {
      utils::head(
        trekker_top_markers(markers, cluster, n = NULL),
        n_per_cluster
      )
    })
  )
  genes <- unique(as.character(selected$gene))
  genes <- genes[genes %in% rownames(expression)]
  clusters <- clusters[clusters %in% groups]
  if (!length(genes) || !length(clusters)) {
    return(data.frame())
  }
  expression <- expression[genes, , drop = FALSE]
  rows <- lapply(clusters, function(cluster) {
    keep <- !is.na(groups) & groups == cluster
    values <- expression[, keep, drop = FALSE]
    data.frame(
      gene = genes,
      cluster = cluster,
      average = as.numeric(rowMeans(values)),
      percent = as.numeric(rowMeans(values > 0) * 100),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result$scaled_average <- 0
  for (gene in genes) {
    at <- result$gene == gene
    values <- result$average[at]
    spread <- stats::sd(values)
    result$scaled_average[at] <- if (is.finite(spread) && spread > 0) {
      pmax(-2.5, pmin(2.5, (values - mean(values)) / spread))
    } else {
      0
    }
  }
  rownames(result) <- NULL
  result
}

trekker_top_spatial_genes <- function(moran, available_genes, n = 6L) {
  if (
    is.null(moran) ||
      !nrow(moran) ||
      !all(c("gene", "moransi.spatially.variable.rank") %in% names(moran))
  ) {
    return(character())
  }
  rank <- suppressWarnings(as.numeric(moran$moransi.spatially.variable.rank))
  genes <- unique(as.character(moran$gene[order(rank, na.last = TRUE)]))
  utils::head(genes[genes %in% available_genes], n)
}

trekker_ranked_moran <- function(moran) {
  if (is.null(moran) || !nrow(moran)) {
    return(data.frame())
  }
  moran$MoransI_observed <- suppressWarnings(as.numeric(moran$MoransI_observed))
  moran$MoransI_p.value <- suppressWarnings(as.numeric(moran$MoransI_p.value))
  moran$moransi.spatially.variable.rank <- suppressWarnings(as.numeric(
    moran$moransi.spatially.variable.rank
  ))
  moran[
    order(moran$moransi.spatially.variable.rank, na.last = TRUE),
    ,
    drop = FALSE
  ]
}

trekker_marker_moran <- function(markers, moran) {
  if (is.null(markers) || is.null(moran) || !nrow(markers) || !nrow(moran)) {
    return(data.frame())
  }
  at <- match(as.character(markers$gene), as.character(moran$gene))
  keep <- !is.na(at)
  result <- markers[keep, , drop = FALSE]
  matched <- moran[at[keep], , drop = FALSE]
  for (column in setdiff(names(matched), "gene")) {
    result[[column]] <- matched[[column]]
  }
  for (column in c(
    "avg_log2FC",
    "pct.1",
    "pct.2",
    "p_val_adj",
    "MoransI_observed",
    "MoransI_p.value",
    "moransi.spatially.variable.rank"
  )) {
    result[[column]] <- suppressWarnings(as.numeric(result[[column]]))
  }
  result$pct_delta <- result$pct.1 - result$pct.2
  rownames(result) <- NULL
  result
}

trekker_cohort_qc <- function(metadata, selected) {
  fields <- intersect(
    c("nCount_RNA", "nFeature_RNA", "percent.mt"),
    names(metadata %||% data.frame())
  )
  if (!length(fields) || is.null(rownames(metadata))) {
    return(data.frame())
  }
  selected <- intersect(as.character(selected), rownames(metadata))
  rows <- lapply(fields, function(field) {
    all_values <- suppressWarnings(as.numeric(metadata[[field]]))
    all_values <- all_values[is.finite(all_values)]
    displayed <- data.frame(
      field = rep(field, length(all_values)),
      cohort = rep("Displayed", length(all_values)),
      value = all_values,
      stringsAsFactors = FALSE
    )
    active_values <- suppressWarnings(as.numeric(metadata[selected, field]))
    active_values <- active_values[is.finite(active_values)]
    active <- data.frame(
      field = rep(field, length(active_values)),
      cohort = rep("Active cohort", length(active_values)),
      value = active_values,
      stringsAsFactors = FALSE
    )
    rbind(displayed, active)
  })
  do.call(rbind, rows)
}

trekker_qc_range <- function(metadata, field, range, outside = FALSE) {
  if (
    is.null(metadata) ||
      is.null(rownames(metadata)) ||
      length(field) != 1L ||
      !field %in% names(metadata) ||
      length(range) != 2L
  ) {
    return(character())
  }
  values <- suppressWarnings(as.numeric(metadata[[field]]))
  limits <- sort(suppressWarnings(as.numeric(range)))
  keep <- is.finite(values) &
    is.finite(limits[[1L]]) &
    is.finite(limits[[2L]]) &
    values >= limits[[1L]] &
    values <= limits[[2L]]
  if (isTRUE(outside)) {
    keep <- is.finite(values) & !keep
  }
  as.character(rownames(metadata)[keep])
}

trekker_natural_levels <- function(values) {
  levels <- unique(as.character(values[!is.na(values) & nzchar(values)]))
  numeric_levels <- suppressWarnings(as.numeric(levels))
  if (length(levels) && all(is.finite(numeric_levels))) {
    levels[order(numeric_levels)]
  } else {
    sort(levels)
  }
}

trekker_gene_evidence <- function(gene, expression, groups, markers, moran) {
  empty <- list(
    distribution = data.frame(),
    markers = data.frame(),
    moran = data.frame()
  )
  if (
    length(gene) != 1L ||
      is.na(gene) ||
      is.null(expression) ||
      !gene %in% rownames(expression) ||
      length(groups) != ncol(expression)
  ) {
    return(empty)
  }
  values <- suppressWarnings(as.numeric(expression[gene, ]))
  groups <- as.character(groups)
  levels <- trekker_natural_levels(groups)
  distribution <- do.call(
    rbind,
    lapply(levels, function(group) {
      selected <- values[!is.na(groups) & groups == group]
      selected <- selected[is.finite(selected)]
      data.frame(
        cluster = group,
        average = if (length(selected)) mean(selected) else NA_real_,
        median = if (length(selected)) stats::median(selected) else NA_real_,
        percent = if (length(selected)) mean(selected > 0) * 100 else NA_real_,
        nuclei = length(selected),
        stringsAsFactors = FALSE
      )
    })
  )
  marker_rows <- if (
    !is.null(markers) &&
      nrow(markers) &&
      all(c("gene", "cluster") %in% names(markers))
  ) {
    markers[as.character(markers$gene) == gene, , drop = FALSE]
  } else {
    data.frame()
  }
  moran_rows <- if (
    !is.null(moran) && nrow(moran) && "gene" %in% names(moran)
  ) {
    moran[as.character(moran$gene) == gene, , drop = FALSE]
  } else {
    data.frame()
  }
  list(
    distribution = distribution,
    markers = marker_rows,
    moran = moran_rows
  )
}

trekker_polygon_area <- function(x, y) {
  points <- unique(data.frame(x = x, y = y))
  if (nrow(points) < 3L) {
    return(0)
  }
  at <- grDevices::chull(points$x, points$y)
  at <- c(at, at[[1L]])
  abs(sum(
    points$x[at[-length(at)]] *
      points$y[at[-1L]] -
      points$y[at[-length(at)]] * points$x[at[-1L]]
  )) /
    2
}

trekker_knn_indices <- function(x, y, k) {
  n <- length(x)
  if (n < 2L) {
    return(matrix(integer(), nrow = n, ncol = 0L))
  }
  k <- min(max(1L, as.integer(k)), n - 1L)
  distances <- as.matrix(stats::dist(cbind(x, y)))
  diag(distances) <- Inf
  nearest <- vapply(
    seq_len(n),
    function(index) {
      order(distances[index, ], seq_len(n))[seq_len(k)]
    },
    integer(k)
  )
  matrix(as.integer(nearest), nrow = n, ncol = k, byrow = TRUE)
}

trekker_spatial_profiles <- function(coordinates, groups, maximum = 1000L) {
  if (
    is.null(coordinates) ||
      !all(c("x", "y") %in% names(coordinates)) ||
      length(groups) != nrow(coordinates)
  ) {
    return(data.frame())
  }
  x <- suppressWarnings(as.numeric(coordinates$x))
  y <- suppressWarnings(as.numeric(coordinates$y))
  groups <- as.character(groups)
  keep <- is.finite(x) & is.finite(y) & !is.na(groups) & nzchar(groups)
  x <- x[keep]
  y <- y[keep]
  groups <- groups[keep]
  if (!length(x)) {
    return(data.frame())
  }
  total <- length(x)
  sample_at <- trekker_sample_indices(total, maximum)
  x <- x[sample_at]
  y <- y[sample_at]
  groups <- groups[sample_at]
  neighbours <- trekker_knn_indices(x, y, 1L)
  nearest_same <- if (ncol(neighbours)) {
    groups[neighbours[, 1L]] == groups
  } else {
    rep(NA, length(groups))
  }
  levels <- trekker_natural_levels(groups)
  result <- do.call(
    rbind,
    lapply(levels, function(group) {
      at <- which(groups == group)
      centre_x <- mean(x[at])
      centre_y <- mean(y[at])
      data.frame(
        group = group,
        nuclei = length(at),
        share = length(at) / length(groups),
        centroid_x = centre_x,
        centroid_y = centre_y,
        hull_area = trekker_polygon_area(x[at], y[at]),
        median_radius = stats::median(sqrt(
          (x[at] - centre_x)^2 + (y[at] - centre_y)^2
        )),
        nearest_same_group = if (all(is.na(nearest_same[at]))) {
          NA_real_
        } else {
          mean(nearest_same[at], na.rm = TRUE)
        },
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(result) <- NULL
  attr(result, "n_sample") <- length(sample_at)
  attr(result, "sampled") <- length(sample_at) < total
  result
}

trekker_local_neighbourhood <- function(
  coordinates,
  groups,
  target,
  k = 6L,
  maximum = 1000L
) {
  if (
    is.null(coordinates) ||
      !all(c("x", "y") %in% names(coordinates)) ||
      length(groups) != nrow(coordinates) ||
      length(target) != 1L
  ) {
    return(data.frame())
  }
  x <- suppressWarnings(as.numeric(coordinates$x))
  y <- suppressWarnings(as.numeric(coordinates$y))
  groups <- as.character(groups)
  barcodes <- if ("barcode" %in% names(coordinates)) {
    as.character(coordinates$barcode)
  } else {
    as.character(seq_len(nrow(coordinates)))
  }
  keep <- is.finite(x) & is.finite(y) & !is.na(groups) & nzchar(groups)
  x <- x[keep]
  y <- y[keep]
  groups <- groups[keep]
  barcodes <- barcodes[keep]
  if (length(x) < 2L || !target %in% groups) {
    return(data.frame())
  }
  at <- trekker_sample_indices(length(x), maximum)
  x <- x[at]
  y <- y[at]
  groups <- groups[at]
  barcodes <- barcodes[at]
  neighbours <- trekker_knn_indices(x, y, k)
  is_target <- matrix(
    groups[neighbours] == as.character(target),
    nrow = nrow(neighbours)
  )
  count <- rowSums(is_target)
  global <- mean(groups == as.character(target))
  data.frame(
    barcode = barcodes,
    x = x,
    y = y,
    group = groups,
    target_share = count / ncol(neighbours),
    enrichment = log2((count + 0.5) / (ncol(neighbours) * global + 0.5)),
    stringsAsFactors = FALSE
  )
}

trekker_section_summary <- function(coordinates, metadata, groups) {
  empty <- list(composition = data.frame(), qc = data.frame())
  if (
    is.null(coordinates) ||
      !all(c("barcode", "section") %in% names(coordinates)) ||
      length(groups) != nrow(coordinates)
  ) {
    return(empty)
  }
  sections <- trekker_natural_levels(as.character(coordinates$section))
  levels <- trekker_natural_levels(as.character(groups))
  counts <- table(
    factor(as.character(coordinates$section), levels = sections),
    factor(as.character(groups), levels = levels)
  )
  composition <- expand.grid(
    section = sections,
    group = levels,
    stringsAsFactors = FALSE
  )
  composition$count <- as.integer(counts[cbind(
    match(composition$section, sections),
    match(composition$group, levels)
  )])
  totals <- rowSums(counts)
  composition$share <- composition$count /
    totals[match(composition$section, sections)]
  if (is.null(metadata) || is.null(rownames(metadata))) {
    return(list(composition = composition, qc = data.frame()))
  }
  fields <- unlist(
    lapply(
      list(
        c("nCount_RNA", "nUMI"),
        c("nFeature_RNA", "nGene"),
        c("percent.mt", "percent_mt")
      ),
      function(candidates) {
        candidates[candidates %in% names(metadata)][1L]
      }
    ),
    use.names = FALSE
  )
  fields <- fields[!is.na(fields)]
  metadata_at <- match(as.character(coordinates$barcode), rownames(metadata))
  qc <- do.call(
    rbind,
    lapply(sections, function(section) {
      section_at <- which(as.character(coordinates$section) == section)
      do.call(
        rbind,
        lapply(fields, function(field) {
          values <- suppressWarnings(as.numeric(
            metadata[[field]][metadata_at[section_at]]
          ))
          values <- values[is.finite(values)]
          data.frame(
            section = section,
            field = field,
            value = if (length(values)) stats::median(values) else NA_real_,
            nuclei = length(values),
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
  list(composition = composition, qc = qc)
}

trekker_coordinate_audit <- function(
  coordinates,
  metadata_keys,
  spatial = NULL
) {
  row <- function(check, status, detail) {
    data.frame(
      check = check,
      status = status,
      detail = detail,
      stringsAsFactors = FALSE
    )
  }
  required <- c("barcode", "section", "x", "y")
  if (is.null(coordinates) || !all(required %in% names(coordinates))) {
    return(row("Location schema", "Fail", "Required columns are missing."))
  }
  barcodes <- as.character(coordinates$barcode)
  valid_barcodes <- !is.na(barcodes) & nzchar(barcodes)
  duplicate_count <- sum(duplicated(barcodes[valid_barcodes]))
  rows <- list(row(
    "Location barcode uniqueness",
    if (all(valid_barcodes) && duplicate_count == 0L) "Pass" else "Fail",
    sprintf(
      "%s rows; %s duplicate barcodes",
      nrow(coordinates),
      duplicate_count
    )
  ))
  metadata_keys <- unique(as.character(metadata_keys))
  missing_metadata <- sum(!barcodes[valid_barcodes] %in% metadata_keys)
  missing_location <- sum(!metadata_keys %in% barcodes[valid_barcodes])
  rows[[length(rows) + 1L]] <- row(
    "CRB metadata coverage",
    if (!missing_metadata && !missing_location) "Pass" else "Warning",
    sprintf(
      "%s Location-only; %s metadata-only barcodes",
      missing_metadata,
      missing_location
    )
  )
  finite <- is.finite(suppressWarnings(as.numeric(coordinates$x))) &
    is.finite(suppressWarnings(as.numeric(coordinates$y)))
  rows[[length(rows) + 1L]] <- row(
    "Finite Location coordinates",
    if (all(finite)) "Pass" else "Fail",
    sprintf("%s of %s rows are finite", sum(finite), length(finite))
  )
  sections <- as.character(coordinates$section)
  valid_sections <- !is.na(sections) & nzchar(sections)
  rows[[length(rows) + 1L]] <- row(
    "Section membership",
    if (all(valid_sections)) "Pass" else "Fail",
    sprintf("%s sections", length(unique(sections[valid_sections])))
  )
  if (!is.null(spatial) && ncol(spatial) >= 2L && !is.null(rownames(spatial))) {
    at <- match(barcodes, rownames(spatial))
    shared <- !is.na(at) & finite
    location_x <- suppressWarnings(as.numeric(coordinates$x[shared]))
    location_y <- suppressWarnings(as.numeric(coordinates$y[shared]))
    spatial_x <- suppressWarnings(as.numeric(spatial[at[shared], 1L]))
    spatial_y <- suppressWarnings(as.numeric(spatial[at[shared], 2L]))
    same <- function(left, right) {
      length(left) && isTRUE(all.equal(left, right, tolerance = 1e-8))
    }
    x_same <- same(location_x, spatial_x)
    y_same <- same(location_y, spatial_y)
    y_inverted <- same(location_y, -spatial_y)
    rows[[length(rows) + 1L]] <- row(
      "SPATIAL coordinate agreement",
      if (x_same && (y_same || y_inverted)) "Pass" else "Warning",
      if (x_same && y_inverted) {
        "X matched; Y inverted by the Seurat plotting convention."
      } else if (x_same && y_same) {
        "X and Y matched."
      } else {
        sprintf(
          "%s of %s Location barcodes were comparable.",
          sum(shared),
          length(barcodes)
        )
      }
    )
  }
  do.call(rbind, rows)
}

trekker_knn_overlap <- function(
  first,
  second,
  groups = NULL,
  k = 6L,
  maximum = 1000L
) {
  empty <- list(per_nucleus = data.frame(), summary = data.frame())
  if (
    is.null(first) ||
      is.null(second) ||
      ncol(first) < 2L ||
      ncol(second) < 2L ||
      is.null(rownames(first)) ||
      is.null(rownames(second))
  ) {
    return(empty)
  }
  keys <- intersect(rownames(first), rownames(second))
  if (length(keys) < 2L) {
    return(empty)
  }
  keys <- keys[trekker_sample_indices(length(keys), maximum)]
  first <- first[keys, seq_len(2L), drop = FALSE]
  second <- second[keys, seq_len(2L), drop = FALSE]
  valid <- apply(first, 1L, function(value) {
    all(is.finite(suppressWarnings(as.numeric(value))))
  }) &
    apply(second, 1L, function(value) {
      all(is.finite(suppressWarnings(as.numeric(value))))
    })
  keys <- keys[valid]
  first <- first[valid, , drop = FALSE]
  second <- second[valid, , drop = FALSE]
  if (length(keys) < 2L) {
    return(empty)
  }
  k <- min(max(1L, as.integer(k)), length(keys) - 1L)
  first_knn <- trekker_knn_indices(first[, 1L], first[, 2L], k)
  second_knn <- trekker_knn_indices(second[, 1L], second[, 2L], k)
  overlap <- vapply(
    seq_along(keys),
    function(index) {
      length(intersect(first_knn[index, ], second_knn[index, ])) /
        length(union(first_knn[index, ], second_knn[index, ]))
    },
    numeric(1)
  )
  group_values <- if (is.null(groups)) {
    rep("All nuclei", length(keys))
  } else if (!is.null(names(groups))) {
    as.character(groups[keys])
  } else if (length(groups) == length(keys)) {
    as.character(groups)
  } else {
    rep("All nuclei", length(keys))
  }
  group_values[is.na(group_values) | !nzchar(group_values)] <- "Unknown"
  per_nucleus <- data.frame(
    barcode = keys,
    group = group_values,
    overlap = overlap,
    stringsAsFactors = FALSE
  )
  levels <- trekker_natural_levels(group_values)
  summary <- do.call(
    rbind,
    lapply(levels, function(group) {
      values <- overlap[group_values == group]
      data.frame(
        group = group,
        mean_overlap = mean(values),
        median_overlap = stats::median(values),
        nuclei = length(values),
        stringsAsFactors = FALSE
      )
    })
  )
  list(per_nucleus = per_nucleus, summary = summary)
}

trekker_categorical_values <- function(
  metadata,
  keys,
  mode,
  registered_groups = character()
) {
  if (is.null(metadata) || is.null(mode) || !startsWith(mode, "meta:")) {
    return(NULL)
  }
  column <- sub("^meta:", "", mode)
  values <- metadata[[column]]
  if (
    is.null(values) || (is.numeric(values) && !column %in% registered_groups)
  ) {
    return(NULL)
  }
  as.character(values[match(as.character(keys), rownames(metadata))])
}

trekker_density_bins <- function(coordinates, bins = 36L) {
  if (is.null(coordinates) || !all(c("x", "y") %in% names(coordinates))) {
    return(data.frame())
  }
  x <- suppressWarnings(as.numeric(coordinates$x))
  y <- suppressWarnings(as.numeric(coordinates$y))
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (!length(x)) {
    return(data.frame())
  }
  bins <- max(1L, as.integer(bins))
  limits <- function(values) {
    extent <- range(values)
    if (extent[[1L]] == extent[[2L]]) extent + c(-0.5, 0.5) else extent
  }
  x_breaks <- seq(limits(x)[[1L]], limits(x)[[2L]], length.out = bins + 1L)
  y_breaks <- seq(limits(y)[[1L]], limits(y)[[2L]], length.out = bins + 1L)
  x_bin <- cut(x, x_breaks, include.lowest = TRUE, labels = FALSE)
  y_bin <- cut(y, y_breaks, include.lowest = TRUE, labels = FALSE)
  counts <- stats::aggregate(
    rep.int(1L, length(x_bin)),
    list(x_bin = x_bin, y_bin = y_bin),
    sum
  )
  names(counts)[[3L]] <- "count"
  data.frame(
    x0 = x_breaks[counts$x_bin],
    x1 = x_breaks[counts$x_bin + 1L],
    y0 = y_breaks[counts$y_bin],
    y1 = y_breaks[counts$y_bin + 1L],
    count = counts$count
  )
}

trekker_sample_indices <- function(n, maximum = 1000L) {
  n <- max(0L, as.integer(n))
  maximum <- max(1L, as.integer(maximum))
  if (!n) {
    return(integer())
  }
  if (n <= maximum) {
    return(seq_len(n))
  }
  unique(as.integer(round(seq(1, n, length.out = maximum))))
}

trekker_neighbourhood <- function(
  coordinates,
  groups,
  k = 6L,
  maximum = 1000L,
  permutations = 199L,
  seed = 1L
) {
  empty <- list(
    adjacency = data.frame(),
    composition = data.frame(),
    n_sample = 0L,
    sampled = FALSE,
    message = "At least seven positioned observations in two groups are required."
  )
  if (is.null(coordinates) || !all(c("x", "y") %in% names(coordinates))) {
    return(empty)
  }
  x <- suppressWarnings(as.numeric(coordinates$x))
  y <- suppressWarnings(as.numeric(coordinates$y))
  groups <- as.character(groups)
  keep <- is.finite(x) & is.finite(y) & !is.na(groups) & nzchar(groups)
  x <- x[keep]
  y <- y[keep]
  groups <- groups[keep]
  if (length(x) < 7L || length(unique(groups)) < 2L) {
    return(empty)
  }
  sample_at <- trekker_sample_indices(length(x), maximum)
  x <- x[sample_at]
  y <- y[sample_at]
  groups <- groups[sample_at]
  n <- length(x)
  k <- min(max(1L, as.integer(k)), n - 1L)
  neighbours <- trekker_knn_indices(x, y, k)
  edges <- unique(t(apply(
    cbind(
      rep(seq_len(n), each = k),
      as.vector(t(neighbours))
    ),
    1L,
    sort
  )))
  levels <- trekker_natural_levels(groups)
  observed <- matrix(
    0,
    length(levels),
    length(levels),
    dimnames = list(
      levels,
      levels
    )
  )
  composition <- observed
  for (edge in seq_len(nrow(edges))) {
    left <- groups[[edges[edge, 1L]]]
    right <- groups[[edges[edge, 2L]]]
    observed[left, right] <- observed[left, right] + 1
    if (left != right) {
      observed[right, left] <- observed[right, left] + 1
    }
    composition[left, right] <- composition[left, right] + 1
    composition[right, left] <- composition[right, left] + 1
  }
  sizes <- table(factor(groups, levels = levels))
  edge_count <- nrow(edges)
  adjacency <- do.call(
    rbind,
    lapply(levels, function(left) {
      do.call(
        rbind,
        lapply(levels, function(right) {
          expected <- if (left == right) {
            edge_count * sizes[[left]] * (sizes[[left]] - 1) / (n * (n - 1))
          } else {
            edge_count * 2 * sizes[[left]] * sizes[[right]] / (n * (n - 1))
          }
          value <- observed[left, right]
          data.frame(
            group_a = left,
            group_b = right,
            observed = value,
            expected = expected,
            enrichment = if (expected > 0) log2(value / expected) else NA_real_,
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
  permutations <- max(0L, as.integer(permutations))
  adjacency$p_value <- NA_real_
  adjacency$p_adj <- NA_real_
  if (permutations > 0L) {
    level_count <- length(levels)
    edge_counts <- function(labels) {
      left <- match(labels[edges[, 1L]], levels)
      right <- match(labels[edges[, 2L]], levels)
      counts <- matrix(
        tabulate(left + (right - 1L) * level_count, nbins = level_count^2),
        nrow = level_count,
        dimnames = list(levels, levels)
      )
      diagonal <- diag(counts)
      counts <- counts + t(counts)
      diag(counts) <- diagonal
      counts
    }
    expected <- matrix(
      adjacency$expected,
      nrow = level_count,
      byrow = TRUE,
      dimnames = list(levels, levels)
    )
    extreme <- matrix(0L, level_count, level_count)
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit(
      {
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (
          exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        ) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )
    set.seed(as.integer(seed))
    for (iteration in seq_len(permutations)) {
      permuted <- edge_counts(sample(groups, length(groups), replace = FALSE))
      extreme <- extreme +
        ifelse(
          observed >= expected,
          permuted >= observed,
          permuted <= observed
        )
    }
    p_value <- (extreme + 1) / (permutations + 1)
    selected <- upper.tri(p_value, diag = TRUE)
    adjusted <- matrix(NA_real_, level_count, level_count)
    adjusted[selected] <- stats::p.adjust(p_value[selected], method = "BH")
    adjusted[lower.tri(adjusted)] <- t(adjusted)[lower.tri(adjusted)]
    row_index <- match(adjacency$group_a, levels)
    column_index <- match(adjacency$group_b, levels)
    adjacency$p_value <- p_value[cbind(row_index, column_index)]
    adjacency$p_adj <- adjusted[cbind(row_index, column_index)]
  }
  composition_rows <- do.call(
    rbind,
    lapply(levels, function(left) {
      total <- sum(composition[left, ])
      data.frame(
        selected_group = left,
        group = levels,
        observed_share = if (total) composition[left, ] / total else NA_real_,
        global_share = as.numeric(sizes) / n,
        count = as.numeric(composition[left, ]),
        stringsAsFactors = FALSE
      )
    })
  )
  list(
    adjacency = adjacency,
    composition = composition_rows,
    n_sample = n,
    sampled = length(sample_at) < sum(keep),
    message = NULL
  )
}

trekker_metric_label <- function(metric) {
  labels <- c(
    sample_id = "Sample",
    single_cell_assay = "Single-cell assay",
    tile_id = "Tile",
    eps = "Positioning radius (ε)",
    numi = "RNA UMI",
    ncount_rna = "RNA UMI",
    ngene = "Detected genes",
    nfeature_rna = "Detected genes",
    percent.mt = "Mitochondrial UMI (%)",
    percent_mt = "Mitochondrial UMI (%)",
    min_spatial_barcodes_used_to_locate_a_nucleus_centroid = "Minimum spatial barcodes",
    maximum_umi_cutoff = "Maximum UMI cutoff"
  )
  key <- tolower(as.character(metric))
  readable <- unname(labels[key])
  readable[is.na(readable)] <- gsub(
    "_",
    " ",
    as.character(metric)[is.na(readable)],
    fixed = TRUE
  )
  readable
}

trekker_file_path <- function(descriptor, crb_path = NULL, app_root = ".") {
  relative <- descriptor$path %||% ""
  if (
    length(relative) != 1L ||
      is.na(relative) ||
      !nzchar(relative) ||
      grepl("^([A-Za-z]:|/|\\\\)", relative) ||
      any(
        strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]] %in%
          c("", ".", "..")
      )
  ) {
    return(NULL)
  }
  candidates <- c(
    file.path(app_root, relative),
    if (!is.null(crb_path)) file.path(dirname(crb_path), relative)
  )
  candidates <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (!length(candidates)) NULL else candidates[[1L]]
}

trekker_file_size <- function(bytes) {
  if (is.null(bytes) || !is.finite(bytes)) {
    return("unknown")
  }
  units <- c("B", "KB", "MB", "GB")
  index <- min(floor(log(max(bytes, 1), 1024)) + 1L, length(units))
  paste0(
    format(round(bytes / 1024^(index - 1L), 1L), trim = TRUE),
    " ",
    units[[index]]
  )
}

trekker_image_uri <- function(descriptor, crb_path = NULL, app_root = ".") {
  path <- trekker_file_path(
    descriptor,
    crb_path = crb_path,
    app_root = app_root
  )
  if (is.null(path) || !requireNamespace("base64enc", quietly = TRUE)) {
    return(NULL)
  }
  paste0("data:", descriptor$mime, ";base64,", base64enc::base64encode(path))
}

trekker_empty <- function(message) {
  div(class = "tk-empty", message)
}
