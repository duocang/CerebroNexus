## Pure trajectory workers. Inputs are ordinary data frames/lists so no Shiny
## reactive or Cerebro object crosses the daemon boundary.

trajectory_prepare_projection <- function(
  cells_df,
  trajectory_edges,
  group_filters,
  percentage,
  display,
  line_color,
  hover_groups,
  seed = 42L
) {
  keep <- !is.na(cells_df$pseudotime)
  for (group in names(group_filters)) {
    if (!group %in% colnames(cells_df)) {
      next
    }
    selected <- group_filters[[group]]
    if (!length(selected)) {
      keep[] <- FALSE
      break
    }
    keep <- keep & cells_df[[group]] %in% selected
  }
  cells_df <- cells_df[keep, , drop = FALSE]

  if (percentage < 100 && nrow(cells_df) > 0L) {
    set.seed(seed)
    size <- ceiling(percentage / 100 * nrow(cells_df))
    cells_df <- cells_df[sample(seq_len(nrow(cells_df)), size), , drop = FALSE]
  }
  if (nrow(cells_df) > 0L) {
    set.seed(seed + 1L)
    cells_df <- cells_df[sample(seq_len(nrow(cells_df))), , drop = FALSE]
  }

  trajectory_lines <- lapply(seq_len(nrow(trajectory_edges)), function(i) {
    list(
      type = "line",
      line = list(color = line_color, width = 1),
      xref = "x",
      yref = "y",
      x0 = trajectory_edges$source_dim_1[i],
      y0 = trajectory_edges$source_dim_2[i],
      x1 = trajectory_edges$target_dim_1[i],
      y1 = trajectory_edges$target_dim_2[i]
    )
  })

  hover_info <- if (nrow(cells_df) == 0L) {
    character(0)
  } else {
    paste0(
      "<b>Cell</b>: ",
      cells_df$cell_barcode,
      "<br><b>Transcripts</b>: ",
      formatC(cells_df$nUMI, format = "f", big.mark = ",", digits = 0),
      "<br><b>Expressed genes</b>: ",
      formatC(cells_df$nGene, format = "f", big.mark = ",", digits = 0)
    )
  }
  for (group in intersect(hover_groups, colnames(cells_df))) {
    hover_info <- paste0(
      hover_info,
      "<br><b>",
      group,
      "</b>: ",
      cells_df[[group]]
    )
  }
  if (nrow(cells_df) > 0L) {
    hover_info <- paste0(
      hover_info,
      "<br><b>State</b>: ",
      cells_df$state,
      "<br><b>Pseudotime</b>: ",
      formatC(cells_df$pseudotime, format = "f", digits = 2)
    )
  }

  c(
    list(
      cells_df = cells_df,
      trajectory_lines = trajectory_lines,
      hover_info = hover_info
    ),
    display
  )
}

trajectory_density_series <- function(pseudotime, group, group_levels) {
  stats::setNames(
    lapply(group_levels, function(level) {
      values <- pseudotime[group == level & !is.na(group)]
      if (length(values) < 2L) {
        return(NULL)
      }
      estimate <- stats::density(values, kernel = "gaussian")
      list(x = estimate$x, y = estimate$y)
    }),
    group_levels
  )
}
