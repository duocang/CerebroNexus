expressionSummaryMode <- function(display_mode, gene_count, dimensions = 2) {
  if (identical(display_mode, "rgb")) {
    return("rgb")
  }
  if (
    identical(display_mode, "separate") &&
      identical(dimensions, 2L) &&
      gene_count >= 2 &&
      gene_count <= 9
  ) {
    return("separate")
  }
  "mean"
}

expressionSummarySpec <- function(
  display_mode,
  genes,
  rgb_genes = list(),
  dimensions = 2
) {
  genes <- unique(as.character(genes))
  genes <- genes[!is.na(genes) & nzchar(genes)]

  if (length(genes) == 0) {
    return(list(kind = "empty", series = list()))
  }

  display_mode <- expressionSummaryMode(
    display_mode,
    length(genes),
    as.integer(dimensions)
  )

  if (identical(display_mode, "rgb")) {
    channel_colors <- c(r = "#dc2626", g = "#16a34a", b = "#2563eb")
    series <- lapply(names(channel_colors), function(channel) {
      gene <- rgb_genes[[channel]]
      if (is.null(gene) || length(gene) != 1 || !gene %in% genes) {
        return(NULL)
      }
      list(
        label = paste0(toupper(channel), " · ", gene),
        key = channel,
        genes = gene,
        color = unname(channel_colors[[channel]])
      )
    })
    return(list(kind = "rgb", series = Filter(Negate(is.null), series)))
  }

  if (identical(display_mode, "separate") && length(genes) > 1) {
    return(list(
      kind = "separate",
      series = lapply(genes, function(gene) {
        list(label = gene, key = gene, genes = gene, color = NULL)
      })
    ))
  }

  list(
    kind = "mean",
    series = list(list(
      label = if (length(genes) == 1) {
        genes[[1]]
      } else {
        paste0("Mean expression (", length(genes), " genes)")
      },
      key = NULL,
      genes = genes,
      color = NULL
    ))
  )
}

## One violin panel per series (three per row, one value axis); `colors` per
## group, or the series' own colour for every group (RGB channels).
plotExpressionSummary <- function(series, groups, colors) {
  if (!length(series)) {
    return(list(type = "empty"))
  }
  # Bound the total density payload across panels, rather than sending 4096
  # points for every patient/sample in a large atlas. Exact summaries below
  # still use every finite observation, independently of the density budget.
  group_count <- max(1L, length(unique(groups[!is.na(groups)])))
  density_points <- max(
    32L,
    min(4096L, floor(65536 / (group_count * length(series))))
  )
  panels <- lapply(series, function(item) {
    cerebroViolinData(
      data.frame(group = groups, level = item$values, stringsAsFactors = FALSE),
      "level",
      "group",
      colors,
      "Expression level",
      "number",
      max_points = density_points,
      title = if (length(series) > 1) item$label,
      fill = item$color
    )
  })
  if (length(panels) == 1) {
    return(panels[[1]])
  }
  cerebroViolinPanels(panels, "Expression level", "number")
}
