##----------------------------------------------------------------------------##
## Spatial autocorrelation (Moran's I) of the displayed ImageFeaturePlot gene.
##
## Reports how spatially clustered the selected gene's expression is: ~+1 when
## high/low cells segregate into patches, ~0 for a random spatial pattern. Only
## computed in ImageFeaturePlot mode. The O(n^2) neighbour weighting is capped by
## down-sampling to a fixed number of cells, so the score stays responsive on
## large slides (it's an estimate of the same statistic on a random subset).
##----------------------------------------------------------------------------##
output[["spatial_projection_morans_i"]] <- renderText({
  plot_parameters <- spatial_projection_parameters_plot()
  req(identical(plot_parameters[["plot_type"]], "ImageFeaturePlot"))
  gene <- plot_parameters[["feature_to_display"]]
  req(gene, gene %in% getGeneNames())

  spatial_data <- getSpatialData(plot_parameters[["projection"]])
  coords <- spatial_data$coordinates
  req(nrow(coords) >= 2)
  cell_index <- spatial_projection_cell_index()
  req(length(cell_index) == nrow(coords))
  expr <- viewerExpressionRow(data_set(), cell_index, gene)
  req(!is.null(expr))

  ## Down-sample for the O(n^2) neighbour search so large slides stay responsive.
  max_cells <- 2000
  n <- length(cell_index)
  idx <- seq_len(n)
  if (n > max_cells) {
    set.seed(42) # stable score across re-renders
    idx <- sort(sample(idx, max_cells))
  }

  score <- morans_i(
    coords[[1]][idx],
    coords[[2]][idx],
    expr[idx],
    k = 6
  )
  if (is.na(score)) {
    return("not enough cells")
  }
  paste0(
    formatC(score, format = "f", digits = 3),
    if (n > max_cells) paste0(" (", max_cells, "-cell subsample)") else ""
  )
}) %>%
  cachePlot(
    spatial_projection_parameters_plot()[["projection"]],
    spatial_projection_parameters_plot()[["feature_to_display"]],
    available_crb_files$selected
  )

## Keep it computed even when the title-bar span is momentarily hidden (e.g.
## while switching plot type), so the value is ready as soon as it reappears.
outputOptions(
  output,
  "spatial_projection_morans_i",
  suspendWhenHidden = FALSE
)

## Info box explaining the score, shown when pressing the "info" button next to
## the Moran's I value in the projection title bar.
observeEvent(input[["spatial_projection_morans_i_info"]], {
  showModal(
    modalDialog(
      spatial_projection_morans_i_info[["text"]],
      title = spatial_projection_morans_i_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

spatial_projection_morans_i_info <- list(
  title = "Spatial autocorrelation (Moran's I)",
  text = HTML(
    "
    <p>Moran's I measures how spatially clustered the displayed gene's
    expression is across the tissue — whether cells with similar expression
    tend to sit next to each other.</p>
    <p>The score runs from about <b>-1</b> to <b>+1</b>:</p>
    <ul>
      <li><b>Near +1</b>: strong spatial structure — high-expressing cells are
      grouped together (e.g. the gene marks a region or layer).</li>
      <li><b>Near 0</b>: expression is spread out with no spatial pattern.</li>
      <li><b>Negative</b>: neighbouring cells tend to differ (a checkerboard-like
      pattern; uncommon).</li>
    </ul>
    <p>It is computed from each cell's six nearest spatial neighbours. Large
    slides are down-sampled to a fixed 2,000-cell subset for a responsive,
    stable score; when that happens the count is shown next to the value.</p>
    "
  )
)
