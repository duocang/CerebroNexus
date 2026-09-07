source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/trekker/helpers.R"),
  local = TRUE
)

trekker_slot <- reactive({
  req(data_set())
  tk <- tryCatch(data_set()$getTrekker(), error = function(error) NULL)
  if (identical(tk$schema_version, 1L)) tk else NULL
})

trekker_gene_names <- reactive({
  tryCatch(data_set()$getGeneNames(), error = function(error) character())
})

trekker_metadata <- reactive({
  metadata <- tryCatch(data_set()$getMetaData(), error = function(error) NULL)
  positioning <- trekker_positioning_metadata(trekker_slot()$positioning)
  if (is.null(metadata)) {
    return(positioning)
  }
  keys <- if ("cell_barcode" %in% names(metadata)) {
    metadata$cell_barcode
  } else {
    rownames(metadata)
  }
  rownames(metadata) <- as.character(keys)
  if (!is.null(positioning)) {
    positioning <- positioning[rownames(metadata), , drop = FALSE]
    metadata <- cbind(metadata, positioning)
  }
  metadata
})

trekker_first_text <- function(...) {
  values <- as.character(unlist(list(...), use.names = FALSE))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values)) values[[1L]] else NULL
}

trekker_section_coordinates <- reactive({
  tk <- req(trekker_slot())
  section <- input[["trekker_section"]] %||%
    unique(as.character(tk$coordinates$section))[[1L]]
  tk$coordinates[tk$coordinates$section == section, , drop = FALSE]
})

trekker_positioning_cohort <- reactiveVal(list(
  active = FALSE,
  section = NULL,
  ids = character()
))

trekker_analysis_coordinates <- reactive({
  coordinates <- trekker_section_coordinates()
  cohort <- trekker_positioning_cohort()
  section <- input[["trekker_section"]] %||%
    unique(as.character(coordinates$section))[[1L]]
  if (!isTRUE(cohort$active) || !identical(cohort$section, section)) {
    return(coordinates)
  }
  coordinates[
    as.character(coordinates$barcode) %in% as.character(cohort$ids),
    ,
    drop = FALSE
  ]
})

trekker_spatial_preview <- reactive({
  coordinates <- trekker_section_coordinates()
  sample_at <- trekker_sample_indices(nrow(coordinates), maximum = 8000L)
  list(
    coordinates = coordinates[sample_at, , drop = FALSE],
    total = nrow(coordinates),
    sampled = length(sample_at) < nrow(coordinates)
  )
})

trekker_selected_nuclei <- reactive({
  trekker_selection_ids(input[["trekker_projection_persistent_selection"]])
})

trekker_plotly <- function(
  plot,
  margin = list(l = 55, r = 20, b = 55, t = 10)
) {
  plot <- plotly::layout(
    plot,
    margin = margin,
    font = list(family = "Inter, sans-serif", color = "#202124")
  )
  plot <- plotly::event_register(plot, "plotly_click")
  plotly::config(plot, displaylogo = FALSE, responsive = TRUE)
}

trekker_subplot_titles <- function(labels, columns) {
  rows <- ceiling(length(labels) / columns)
  lapply(seq_along(labels), function(index) {
    column <- (index - 1L) %% columns + 1L
    row <- ceiling(index / columns)
    list(
      text = labels[[index]],
      x = (column - 0.5) / columns,
      y = 1 - (row - 1) / rows + 0.02,
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = 13)
    )
  })
}

trekker_select_gene <- function(gene, cluster = NULL) {
  if (length(gene) != 1L || is.na(gene) || !nzchar(gene)) {
    return(invisible(NULL))
  }
  updateSelectInput(session, "trekker_colour", selected = "gene")
  updateSelectizeInput(session, "trekker_gene", selected = as.character(gene))
  updateSelectizeInput(
    session,
    "trekker_evidence_gene",
    selected = as.character(gene)
  )
  if (
    length(cluster) == 1L &&
      !is.na(cluster) &&
      nzchar(cluster)
  ) {
    updateSelectInput(
      session,
      "trekker_marker_cluster",
      selected = as.character(cluster)
    )
  }
  invisible(gene)
}

trekker_marker_group_values <- function(metadata, keys, clusters) {
  if (is.null(metadata) || is.null(rownames(metadata))) {
    return(NULL)
  }
  candidates <- unique(c(
    "seurat_clusters",
    grep("cluster", names(metadata), value = TRUE, ignore.case = TRUE)
  ))
  candidates <- candidates[candidates %in% names(metadata)]
  for (candidate in candidates) {
    values <- as.character(metadata[[candidate]][match(
      keys,
      rownames(metadata)
    )])
    if (all(clusters %in% unique(values[!is.na(values)]))) {
      return(values)
    }
  }
  NULL
}

trekker_default_groups <- reactive({
  coordinates <- trekker_slot()$coordinates
  metadata <- trekker_metadata()
  if (is.null(metadata) || is.null(rownames(metadata))) {
    return(list(name = NULL, values = NULL))
  }
  registered <- tryCatch(
    data_set()$getGroups(),
    error = function(error) character()
  )
  candidates <- unique(c(
    "seurat_clusters",
    registered,
    grep("cluster", names(metadata), value = TRUE, ignore.case = TRUE)
  ))
  candidates <- candidates[candidates %in% names(metadata)]
  keys <- as.character(coordinates$barcode)
  for (candidate in candidates) {
    values <- as.character(metadata[[candidate]][match(
      keys,
      rownames(metadata)
    )])
    if (length(unique(values[!is.na(values) & nzchar(values)])) > 1L) {
      names(values) <- keys
      return(list(name = candidate, values = values))
    }
  }
  list(name = NULL, values = NULL)
})

output[["trekker_summary"]] <- renderUI({
  tk <- req(trekker_slot())
  labels <- c(
    location = "Location",
    positioning = "Positioning evidence",
    metrics = "Run QC",
    cluster_markers = "Cluster markers",
    moran = "Spatial genes",
    report = "Official report"
  )
  available <- names(labels)[names(labels) %in% names(tk$files)]
  section_count <- length(unique(tk$coordinates$section))
  image_count <- sum(vapply(tk$images %||% list(), length, integer(1)))
  if (!length(available) && !image_count) {
    return(NULL)
  }
  available_count <- 1L + as.integer(image_count > 0L) + length(available)
  badges <- lapply(names(labels), function(category) {
    present <- category %in% available
    span(
      class = paste("tk-badge", if (!present) "tk-badge-gray"),
      paste(labels[[category]], if (present) "available" else "not provided")
    )
  })
  badges <- c(
    list(span(
      class = "tk-badge",
      paste(section_count, if (section_count == 1L) "section" else "sections")
    )),
    list(span(
      class = paste("tk-badge", if (!image_count) "tk-badge-gray"),
      paste(image_count, "image layers")
    )),
    badges
  )
  tags$details(
    class = "tk-summary-disclosure",
    onmouseleave = paste0(
      "if (!this.open || this.classList.contains('tk-summary-closing')) return;",
      "this.classList.add('tk-summary-closing');",
      "setTimeout(() => {",
      "this.open = false;",
      "this.classList.remove('tk-summary-closing');",
      "}, 220);"
    ),
    tags$summary(
      class = "tk-summary-toggle",
      icon("database"),
      sprintf("Data (%d)", available_count),
      icon("chevron-down", class = "tk-summary-chevron")
    ),
    div(
      class = "tk-summary",
      div(class = "tk-meta", badges),
      downloadButton(
        "trekker_download_all",
        "Official data",
        class = "btn-primary"
      )
    )
  )
})
outputOptions(output, "trekker_summary", suspendWhenHidden = FALSE)

output[["trekker_run_context"]] <- renderUI({
  tk <- req(trekker_slot())
  metadata <- tk$report_metadata %||% list()
  metrics <- tk$metrics
  semantic <- trekker_positioning_summary(metrics)
  source_index <- match("Confidently positioned", semantic$funnel$stage)
  source_count <- if (is.na(source_index)) {
    NA_real_
  } else {
    semantic$funnel$value[[source_index]]
  }
  displayed <- nrow(tk$coordinates)
  items <- list(
    "Sample" = trekker_first_text(
      metadata$sample,
      if (!is.null(metrics)) {
        metrics$Value[match("Sample_ID", metrics$Metrics)]
      }
    ),
    "Platform" = trekker_first_text(
      metadata$assay_platform,
      if (!is.null(metrics)) {
        metrics$Value[match("Single_cell_assay", metrics$Metrics)]
      }
    ),
    "Tile" = trekker_first_text(
      metadata$tile_id,
      if (!is.null(metrics)) {
        metrics$Value[match("Tile_ID", metrics$Metrics)]
      }
    ),
    "Pipeline" = trekker_first_text(metadata$pipeline_version),
    "Analysis date" = trekker_first_text(metadata$analysis_date),
    "Displayed nuclei" = format(displayed, big.mark = ","),
    "Source nuclei" = if (
      is.finite(source_count) && source_count != displayed
    ) {
      format(source_count, big.mark = ",", scientific = FALSE, trim = TRUE)
    }
  )
  items <- items[vapply(
    items,
    function(value) {
      length(value) == 1L && !is.na(value) && nzchar(as.character(value))
    },
    logical(1)
  )]
  if (!length(items)) {
    return(NULL)
  }
  div(
    class = "tk-run-context",
    lapply(names(items), function(label) {
      div(
        class = "tk-context-item",
        span(class = "tk-context-label", label),
        span(class = "tk-context-value", as.character(items[[label]]))
      )
    })
  )
})

output[["trekker_main_parameters_ui"]] <- renderUI({
  tk <- req(trekker_slot())
  metadata <- trekker_metadata()
  columns <- if (is.null(metadata)) {
    character()
  } else {
    setdiff(names(metadata), "cell_barcode")
  }
  choices <- stats::setNames(paste0("meta:", columns), columns)
  choices <- c(choices, "Gene expression" = "gene")
  projections <- tryCatch(
    data_set()$availableProjections(),
    error = function(error) character()
  )
  has_umap <- any(tolower(projections) == "umap")
  sections <- unique(as.character(tk$coordinates$section))
  section <- input[["trekker_section"]] %||% sections[[1L]]
  layers <- tk$images[[section]] %||% list()
  layer_labels <- vapply(layers, function(layer) layer$label, character(1))
  visible_layers <- vapply(
    layers,
    function(layer) isTRUE(layer$settings$visible),
    logical(1)
  )
  tagList(
    selectInput(
      "trekker_section",
      "Section / FOV",
      choices = sections,
      selected = section
    ),
    selectInput(
      "trekker_view",
      "View",
      choices = c(
        "Physical map" = "physical",
        if (has_umap) {
          c("Physical + UMAP" = "pair", "Transition" = "morph")
        }
      )
    ),
    selectInput("trekker_colour", "Colour by", choices = choices),
    if (length(layers)) {
      conditionalPanel(
        condition = "input.trekker_view != 'morph'",
        selectizeInput(
          "trekker_layers",
          "Layer image",
          choices = stats::setNames(layer_labels, layer_labels),
          selected = layer_labels[visible_layers],
          multiple = TRUE,
          options = list(
            closeAfterSelect = FALSE,
            plugins = list("remove_button"),
            placeholder = "No image layer",
            render = I(
              "{
                item: function(item, escape) {
                  var first = String(item.label).trim().split(/\\s+/)[0];
                  return '<div class=\"item\" title=\"' + escape(item.label) + '\">' +
                    escape(first) + '&hellip;</div>';
                }
              }"
            )
          )
        )
      )
    },
    conditionalPanel(
      condition = "input.trekker_view == 'morph'",
      class = "trekker-transition-control",
      sliderInput(
        "trekker_morph",
        "Transition (UMAP → Physical)",
        min = 0,
        max = 1,
        value = 0,
        step = 0.01,
        ticks = FALSE
      )
    ),
    conditionalPanel(
      condition = "input.trekker_colour == 'gene'",
      selectizeInput(
        "trekker_gene",
        "Gene",
        choices = trekker_gene_suggest(tk, trekker_gene_names()),
        options = list(create = TRUE, placeholder = "search or type a gene...")
      )
    )
  )
})
outputOptions(output, "trekker_main_parameters_ui", suspendWhenHidden = FALSE)

observe({
  input[["trekker_projection_render_request"]]
  tk <- req(trekker_slot())
  section <- input[["trekker_section"]] %||%
    unique(tk$coordinates$section)[[1L]]
  view <- input[["trekker_view"]] %||% "physical"
  coordinates <- tk$coordinates[
    tk$coordinates$section == section,
    ,
    drop = FALSE
  ]
  keys <- as.character(coordinates$barcode)
  metadata <- trekker_metadata()
  groups <- tryCatch(data_set()$getGroups(), error = function(error) {
    character()
  })
  default_mode <- if (length(groups)) paste0("meta:", groups[[1L]]) else "gene"
  mode <- input[["trekker_colour"]] %||% default_mode
  categorical <- FALSE
  group <- rep("All cells", length(keys))
  color <- NULL
  label <- "All cells"
  if (startsWith(mode, "meta:") && !is.null(metadata)) {
    column <- sub("^meta:", "", mode)
    values <- metadata[[column]][match(keys, rownames(metadata))]
    categorical_values <- trekker_categorical_values(
      metadata,
      keys,
      mode,
      registered_groups = groups
    )
    categorical <- !is.null(categorical_values)
    if (categorical) {
      group <- categorical_values
    } else {
      color <- as.numeric(values)
    }
    label <- column
  } else if (identical(mode, "gene")) {
    gene <- input[["trekker_gene"]]
    if (!is.null(gene) && gene %in% trekker_gene_names()) {
      color <- as.numeric(tryCatch(
        data_set()$getExpressionMatrix(cells = keys, genes = gene),
        error = function(error) rep(NA_real_, length(keys))
      ))
      label <- gene
    }
  }
  if (!categorical && is.null(color)) {
    color <- rep(0, length(keys))
  }
  physical <- list(
    id = "trekker",
    label = paste0(section, " · Trekker physical coordinates"),
    selection_key = keys,
    x = as.numeric(coordinates$x),
    y = as.numeric(coordinates$y),
    spatial = TRUE
  )
  if (isTRUE(input[["cv-trekker-density"]])) {
    density <- trekker_density_bins(coordinates)
    if (nrow(density)) {
      physical$density <- lapply(density, unname)
    }
  }
  if (!identical(view, "morph")) {
    layers <- tk$images[[section]] %||% list()
    selected_layers <- input[["trekker_layers"]]
    layers <- Filter(function(layer) layer$label %in% selected_layers, layers)
  } else {
    layers <- list()
  }
  if (length(layers)) {
    physical$images <- Filter(
      Negate(is.null),
      lapply(seq_along(layers), function(index) {
        descriptor <- layers[[index]]
        uri <- trekker_image_uri(
          descriptor,
          crb_path = trekker_current_crb(),
          app_root = Cerebro.options[["cerebro_root"]]
        )
        if (is.null(uri)) {
          return(NULL)
        }
        bounds <- unlist(descriptor$bounds)
        list(
          id = paste(section, descriptor$label, sep = "::"),
          label = descriptor$label,
          uri = uri,
          bounds = as.list(bounds),
          coord_span = c(
            diff(bounds[c("xmin", "xmax")]),
            diff(bounds[c("ymin", "ymax")])
          ),
          preset = list(
            show = TRUE,
            opacity = descriptor$settings$image_opacity,
            rotation = descriptor$settings$rotation,
            scaleX = descriptor$settings$scale_x,
            scaleY = descriptor$settings$scale_y,
            offsetX = descriptor$settings$offset_x,
            offsetY = descriptor$settings$offset_y,
            flipX = descriptor$settings$flip_x,
            flipY = descriptor$settings$flip_y
          )
        )
      })
    )
    physical$multi_image_layers <- TRUE
  }
  panels <- list(physical)
  projections <- tryCatch(
    data_set()$availableProjections(),
    error = function(error) character()
  )
  umap_name <- projections[tolower(projections) == "umap"]
  if (view %in% c("pair", "morph") && length(umap_name)) {
    umap <- data_set()$getProjection(umap_name[[1L]])
    at <- match(keys, rownames(umap))
    if (identical(view, "morph")) {
      panels[[1L]]$label <- paste0("UMAP → ", section, " physical coordinates")
      panels[[1L]]$from_x <- as.numeric(umap[at, 1L])
      panels[[1L]]$from_y <- as.numeric(umap[at, 2L])
      panels[[1L]]$to_x <- as.numeric(coordinates$x)
      panels[[1L]]$to_y <- as.numeric(coordinates$y)
      panels[[1L]]$transition <- input[["trekker_morph"]] %||% 0
    } else {
      panels[[2L]] <- list(
        id = "umap",
        label = "UMAP",
        selection_key = keys,
        x = as.numeric(umap[at, 1L]),
        y = as.numeric(umap[at, 2L]),
        projection = TRUE
      )
    }
  }
  levels <- if (categorical) sort(unique(group[!is.na(group)])) else character()
  appearance <- current_scatter_defaults()
  cerebroCellViewRender(
    "trekker_projection",
    meta = list(
      color_type = if (categorical) "categorical" else "continuous",
      color_variable = label,
      traces = levels,
      group_colors = cerebro_group_colors(length(levels)),
      shared_filters = categorical
    ),
    data = list(
      selection_key = keys,
      panels = panels,
      group = group,
      color = color,
      point_size = appearance$point_size,
      point_opacity = appearance$point_opacity,
      percentage_cells_to_show = appearance$percentage_cells_to_show
    )
  )
})

output[["trekker_number_of_selected_cells"]] <- renderUI({
  selected <- trekker_selected_nuclei()
  entity <- trekker_entity_label(
    trekker_slot(),
    plural = length(selected) != 1L
  )
  tags$span(
    format(length(selected), big.mark = ","),
    paste(entity, "selected")
  )
})

output[["trekker_selection_export"]] <- renderUI({
  req(length(trekker_selected_nuclei()))
  div(
    class = "tk-selection-export",
    downloadButton(
      "trekker_download_selection",
      "Export selected nuclei",
      icon = icon("download")
    )
  )
})

output[["trekker_download_selection"]] <- downloadHandler(
  filename = function() {
    section <- input[["trekker_section"]] %||% "section"
    paste0("trekker-", section, "-selected-nuclei.csv")
  },
  content = function(file) {
    selected <- trekker_selected_nuclei()
    req(length(selected))
    coordinates <- trekker_slot()$coordinates
    at <- match(selected, as.character(coordinates$barcode))
    at <- at[!is.na(at)]
    selected <- as.character(coordinates$barcode[at])
    location <- data.frame(
      cell_barcode = selected,
      trekker_section = as.character(coordinates$section[at]),
      trekker_x = as.numeric(coordinates$x[at]),
      trekker_y = as.numeric(coordinates$y[at]),
      stringsAsFactors = FALSE
    )
    metadata <- trekker_metadata()
    if (!is.null(metadata)) {
      values <- metadata[
        selected,
        setdiff(names(metadata), "cell_barcode"),
        drop = FALSE
      ]
      rownames(values) <- NULL
      location <- cbind(location, values)
    }
    utils::write.csv(location, file, row.names = FALSE, na = "")
  }
)

output[["trekker_cell_inspector"]] <- renderUI({
  selected <- trekker_selected_nuclei()
  if (length(selected) != 1L) {
    entity <- trekker_entity_label(trekker_slot())
    return(trekker_empty(
      paste0(
        "Select one ",
        entity,
        " to inspect its barcode, coordinates, and CRB metadata."
      )
    ))
  }
  evidence <- trekker_positioning_record(trekker_slot()$positioning, selected)
  tagList(
    tags$h4(paste(trekker_entity_label(trekker_slot()), "inspector")),
    if (nrow(evidence)) {
      div(
        class = "tk-cell-evidence",
        tags$h5("Positioning evidence"),
        tags$p(
          class = "tk-note",
          "Vendor fields from coords_*; no composite confidence score is inferred."
        ),
        DT::DTOutput("trekker_cell_positioning_table")
      )
    },
    tags$h5("Coordinates and CRB metadata"),
    DT::DTOutput("trekker_cell_table")
  )
})

output[["trekker_cell_positioning_table"]] <- DT::renderDT({
  selected <- trekker_selected_nuclei()
  req(length(selected) == 1L)
  table <- trekker_positioning_record(trekker_slot()$positioning, selected)
  req(nrow(table))
  DT::datatable(
    table,
    rownames = FALSE,
    options = list(dom = "t", paging = FALSE),
    class = "compact stripe"
  )
})

output[["trekker_cell_table"]] <- DT::renderDT({
  selected <- trekker_selected_nuclei()
  req(length(selected) == 1L)
  tk <- trekker_slot()
  at <- match(selected, tk$coordinates$barcode)
  metadata <- trekker_metadata()
  values <- if (is.null(metadata)) NULL else metadata[selected, , drop = FALSE]
  projections <- tryCatch(
    data_set()$availableProjections(),
    error = function(error) character()
  )
  spatial_name <- projections[tolower(projections) == "spatial"]
  spatial <- if (length(spatial_name)) {
    projection <- data_set()$getProjection(spatial_name[[1L]])
    projection[
      match(selected, rownames(projection)),
      seq_len(min(2L, ncol(projection))),
      drop = TRUE
    ]
  } else {
    numeric()
  }
  gene <- if (identical(input[["trekker_colour"]], "gene")) {
    input[["trekker_gene"]]
  } else {
    NULL
  }
  expression <- if (!is.null(gene) && gene %in% trekker_gene_names()) {
    as.numeric(data_set()$getExpressionMatrix(cells = selected, genes = gene))
  } else {
    numeric()
  }
  fields <- c(
    "Barcode",
    "Location X",
    "Location Y",
    "Section / FOV",
    if (length(spatial) == 2L) c("SPATIAL X", "SPATIAL Y"),
    if (length(expression)) paste0(gene, " expression"),
    setdiff(
      names(values),
      c("cell_barcode", grep("^Trekker: ", names(values), value = TRUE))
    )
  )
  metadata_fields <- setdiff(
    names(values),
    c("cell_barcode", grep("^Trekker: ", names(values), value = TRUE))
  )
  DT::datatable(
    data.frame(
      Field = fields,
      Value = as.character(c(
        selected,
        tk$coordinates$x[[at]],
        tk$coordinates$y[[at]],
        tk$coordinates$section[[at]],
        spatial,
        expression,
        if (!is.null(values)) {
          unlist(
            values[metadata_fields],
            use.names = FALSE
          )
        }
      )),
      check.names = FALSE
    ),
    rownames = FALSE,
    options = list(dom = "t", paging = FALSE)
  )
})

trekker_spatial_qc_results <- reactive({
  preview <- trekker_spatial_preview()
  trekker_spatial_qc(trekker_metadata(), preview$coordinates)
})

trekker_qc_fields <- reactive({
  metadata <- trekker_metadata()
  if (is.null(metadata)) {
    return(character())
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
  fields[!is.na(fields)]
})

trekker_positioning_fields <- reactive({
  metadata <- trekker_metadata()
  if (is.null(metadata)) {
    return(character())
  }
  fields <- names(metadata)[startsWith(names(metadata), "Trekker: ")]
  fields[vapply(metadata[fields], is.numeric, logical(1))]
})

output[["trekker_positioning_filter_controls"]] <- renderUI({
  fields <- trekker_positioning_fields()
  if (!length(fields)) {
    return(NULL)
  }
  field <- input[["trekker_positioning_field"]] %||% fields[[1L]]
  if (!field %in% fields) {
    field <- fields[[1L]]
  }
  values <- suppressWarnings(as.numeric(trekker_metadata()[[field]]))
  values <- values[is.finite(values)]
  req(length(values))
  limits <- range(values)
  if (limits[[1L]] == limits[[2L]]) {
    limits <- limits + c(-0.5, 0.5)
  }
  div(
    class = "tk-chart-panel tk-wide-chart tk-positioning-filter",
    tags$h4("Positioning evidence"),
    tags$p(
      class = "tk-note",
      paste(
        "Filter the map with measured Trekker positioning evidence",
        "and compare the resulting active cohort."
      )
    ),
    div(
      class = "tk-inline-controls tk-qc-filter",
      selectInput(
        "trekker_positioning_field",
        "Evidence field",
        choices = stats::setNames(
          fields,
          sub("^Trekker: ", "", fields)
        ),
        selected = field
      ),
      sliderInput(
        "trekker_positioning_range",
        "Range",
        min = limits[[1L]],
        max = limits[[2L]],
        value = limits,
        step = diff(limits) / 100,
        ticks = FALSE
      ),
      div(
        class = "tk-qc-actions",
        actionButton(
          "trekker_select_positioning_range",
          "Select range"
        ),
        actionButton(
          "trekker_select_positioning_outliers",
          "Select outliers"
        ),
        actionButton(
          "trekker_clear_positioning_filter",
          "Clear filter"
        )
      )
    )
  )
})

trekker_send_positioning_selection <- function(outside) {
  field <- input[["trekker_positioning_field"]]
  range <- input[["trekker_positioning_range"]]
  req(field, length(range) == 2L)
  ids <- trekker_qc_range(trekker_metadata(), field, range, outside = outside)
  coordinates <- trekker_section_coordinates()
  visible <- as.character(coordinates$barcode)
  ids <- intersect(visible, ids)
  trekker_positioning_cohort(list(
    active = TRUE,
    section = input[["trekker_section"]] %||%
      unique(as.character(coordinates$section))[[1L]],
    ids = ids
  ))
  session$sendCustomMessage(
    "cell_view_set_selection",
    list(id = "trekker_projection", ids = ids)
  )
}

observeEvent(input[["trekker_select_positioning_range"]], {
  trekker_send_positioning_selection(FALSE)
})

observeEvent(input[["trekker_select_positioning_outliers"]], {
  trekker_send_positioning_selection(TRUE)
})

observeEvent(input[["trekker_clear_positioning_filter"]], {
  trekker_positioning_cohort(list(
    active = FALSE,
    section = NULL,
    ids = character()
  ))
  session$sendCustomMessage(
    "cell_view_set_selection",
    list(id = "trekker_projection", ids = character())
  )
})

output[["trekker_qc_filter_controls"]] <- renderUI({
  fields <- trekker_qc_fields()
  if (!length(fields)) {
    return(NULL)
  }
  field <- input[["trekker_qc_field"]] %||% fields[[1L]]
  if (!field %in% fields) {
    field <- fields[[1L]]
  }
  values <- suppressWarnings(as.numeric(trekker_metadata()[[field]]))
  values <- values[is.finite(values)]
  req(length(values))
  limits <- range(values)
  if (limits[[1L]] == limits[[2L]]) {
    limits <- limits + c(-0.5, 0.5)
  }
  div(
    class = "tk-inline-controls tk-qc-filter",
    selectInput(
      "trekker_qc_field",
      "QC field",
      choices = stats::setNames(fields, trekker_metric_label(fields)),
      selected = field
    ),
    sliderInput(
      "trekker_qc_range",
      "Range",
      min = limits[[1L]],
      max = limits[[2L]],
      value = limits,
      step = diff(limits) / 100,
      ticks = FALSE
    ),
    div(
      class = "tk-qc-actions",
      actionButton("trekker_select_qc_range", "Select range"),
      actionButton("trekker_select_qc_outliers", "Select outliers")
    )
  )
})

trekker_send_qc_selection <- function(outside) {
  field <- input[["trekker_qc_field"]]
  range <- input[["trekker_qc_range"]]
  req(field, length(range) == 2L)
  ids <- trekker_qc_range(trekker_metadata(), field, range, outside = outside)
  visible <- as.character(trekker_section_coordinates()$barcode)
  session$sendCustomMessage(
    "cell_view_set_selection",
    list(id = "trekker_projection", ids = intersect(visible, ids))
  )
}

observeEvent(input[["trekker_select_qc_range"]], {
  trekker_send_qc_selection(FALSE)
})

observeEvent(input[["trekker_select_qc_outliers"]], {
  trekker_send_qc_selection(TRUE)
})

output[["trekker_spatial_qc_empty"]] <- renderUI({
  if (!nrow(trekker_spatial_qc_results())) {
    trekker_empty("Nucleus-level RNA quality metadata was not provided.")
  }
})

output[["trekker_spatial_preview_note"]] <- renderUI({
  preview <- trekker_spatial_preview()
  if (preview$sampled) {
    tags$p(
      class = "tk-note",
      paste(
        "Showing a deterministic spatial preview of",
        format(nrow(preview$coordinates), big.mark = ","),
        "of",
        format(preview$total, big.mark = ","),
        "nuclei."
      )
    )
  }
})

output[["trekker_spatial_qc_plot"]] <- plotly::renderPlotly({
  table <- trekker_spatial_qc_results()
  req(nrow(table))
  metrics <- unique(table$metric)
  point_size <- if (length(unique(table$barcode)) > 5000L) 2 else 4
  plots <- lapply(metrics, function(metric) {
    current <- table[table$metric == metric, , drop = FALSE]
    current$tooltip <- paste0(
      "Barcode: ",
      current$barcode,
      "<br>",
      metric,
      ": ",
      signif(current$value, 4L)
    )
    plotly::layout(
      plotly::plot_ly(
        current,
        x = ~x,
        y = ~y,
        key = ~barcode,
        text = ~tooltip,
        hoverinfo = "text",
        type = "scattergl",
        mode = "markers",
        showlegend = FALSE,
        marker = list(
          size = point_size,
          color = current$value,
          colorscale = "Viridis",
          showscale = FALSE,
          opacity = 0.85
        ),
        source = "trekker_spatial_qc"
      ),
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE)
    )
  })
  plot <- do.call(
    plotly::subplot,
    c(
      plots,
      list(nrows = 1L, shareX = TRUE, shareY = TRUE, margin = 0.035)
    )
  )
  plot <- plotly::layout(
    plot,
    annotations = trekker_subplot_titles(metrics, length(metrics))
  )
  trekker_plotly(plot, margin = list(l = 10, r = 10, b = 10, t = 35))
})

output[["trekker_metrics_cards"]] <- renderUI({
  metrics <- trekker_slot()$metrics
  if (is.null(metrics) || !nrow(metrics)) {
    return(trekker_empty("Run QC was not provided."))
  }
  qc <- trekker_positioning_summary(metrics)$qc
  if (!nrow(qc)) {
    return(NULL)
  }
  div(
    class = "tk-grid",
    lapply(seq_len(nrow(qc)), function(index) {
      value <- format(
        qc$value[[index]],
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
      div(
        class = "tk-stat",
        div(class = "tk-k", qc$label[[index]]),
        div(class = "tk-v", paste0(value, qc$unit[[index]]))
      )
    })
  )
})

trekker_run_qc_detail_results <- reactive({
  trekker_run_qc_details(trekker_slot()$metrics)
})

output[["trekker_run_qc_details_empty"]] <- renderUI({
  details <- trekker_run_qc_detail_results()
  if (!any(vapply(details, nrow, integer(1)))) {
    trekker_empty("Detailed run-level QC metrics were not provided.")
  }
})

output[["trekker_run_qc_details_plot"]] <- plotly::renderPlotly({
  details <- trekker_run_qc_detail_results()
  plots <- list()
  if (nrow(details$reads)) {
    table <- details$reads
    table$stage <- factor(table$stage, levels = rev(table$stage))
    plots[[length(plots) + 1L]] <- plotly::layout(
      plotly::plot_ly(
        table,
        x = ~value,
        y = ~stage,
        type = "bar",
        orientation = "h",
        showlegend = FALSE,
        marker = list(color = "#ff6d16"),
        text = ~ format(value, big.mark = ",", scientific = FALSE),
        hovertemplate = "%{y}: %{text}<extra></extra>"
      ),
      xaxis = list(title = "", rangemode = "tozero"),
      yaxis = list(title = "")
    )
  }
  if (nrow(details$rates)) {
    table <- details$rates
    table$metric <- factor(table$metric, levels = rev(table$metric))
    plots[[length(plots) + 1L]] <- plotly::layout(
      plotly::plot_ly(
        table,
        x = ~value,
        y = ~metric,
        type = "bar",
        orientation = "h",
        showlegend = FALSE,
        marker = list(color = "#5b8def"),
        text = ~ paste0(signif(value, 4L), "%"),
        hovertemplate = "%{y}: %{text}<extra></extra>"
      ),
      xaxis = list(title = "", range = c(0, 100)),
      yaxis = list(title = "")
    )
  }
  if (nrow(details$depth)) {
    table <- details$depth
    plots[[length(plots) + 1L]] <- plotly::layout(
      plotly::plot_ly(
        table,
        x = ~measure,
        y = ~value,
        color = ~statistic,
        colors = c("#ff6d16", "#9aa0a6"),
        type = "bar",
        showlegend = TRUE,
        text = ~ format(value, big.mark = ",", scientific = FALSE),
        hovertemplate = "%{x} · %{fullData.name}: %{text}<extra></extra>"
      ),
      barmode = "group",
      xaxis = list(title = "", tickangle = -20),
      yaxis = list(title = "Count", rangemode = "tozero"),
      legend = list(orientation = "h")
    )
  }
  if (nrow(details$positioning)) {
    table <- details$positioning
    table$stage <- factor(table$stage, levels = rev(table$stage))
    plots[[length(plots) + 1L]] <- plotly::layout(
      plotly::plot_ly(
        table,
        x = ~value,
        y = ~stage,
        type = "bar",
        orientation = "h",
        showlegend = FALSE,
        marker = list(color = "#34a853"),
        text = ~ format(value, big.mark = ",", scientific = FALSE),
        hovertemplate = "%{y}: %{text}<extra></extra>"
      ),
      xaxis = list(title = "Nuclei", rangemode = "tozero"),
      yaxis = list(title = "")
    )
  }
  req(length(plots))
  titles <- c(
    if (nrow(details$reads)) "Read retention",
    if (nrow(details$rates)) "Read and barcode rates",
    if (nrow(details$depth)) "Per-nucleus depth",
    if (nrow(details$positioning)) "Ambiguity and recovery"
  )
  columns <- min(2L, length(plots))
  plot <- do.call(
    plotly::subplot,
    c(
      plots,
      list(
        nrows = ceiling(length(plots) / columns),
        shareX = FALSE,
        shareY = FALSE,
        margin = 0.13,
        titleX = TRUE,
        titleY = TRUE
      )
    )
  )
  plot <- plotly::layout(
    plot,
    annotations = trekker_subplot_titles(titles, columns)
  )
  trekker_plotly(plot, margin = list(l = 120, r = 20, b = 80, t = 35))
})

trekker_positioning <- reactive({
  trekker_positioning_summary(trekker_slot()$metrics)
})

output[["trekker_positioning_funnel_empty"]] <- renderUI({
  if (!nrow(trekker_positioning()$funnel)) {
    trekker_empty("Positioning stages were not provided.")
  }
})

output[["trekker_positioning_funnel"]] <- plotly::renderPlotly({
  table <- trekker_positioning()$funnel
  req(nrow(table))
  table$stage <- factor(table$stage, levels = rev(table$stage))
  plot <- plotly::plot_ly(
    table,
    x = ~value,
    y = ~stage,
    type = "bar",
    orientation = "h",
    marker = list(color = "#ff6d16"),
    text = ~ format(value, big.mark = ",", scientific = FALSE),
    hovertemplate = "%{y}: %{text}<extra></extra>",
    source = "trekker_funnel"
  )
  trekker_plotly(plotly::layout(
    plot,
    xaxis = list(title = "Nuclei", rangemode = "tozero"),
    yaxis = list(title = "")
  ))
})

output[["trekker_position_distribution_empty"]] <- renderUI({
  if (!nrow(trekker_positioning()$distribution)) {
    trekker_empty("Assigned-location counts were not provided.")
  }
})

output[["trekker_position_distribution"]] <- plotly::renderPlotly({
  table <- trekker_positioning()$distribution
  req(nrow(table))
  plot <- plotly::plot_ly(
    table,
    x = ~bucket,
    y = ~value,
    type = "bar",
    marker = list(color = "#5b8def"),
    text = ~ format(value, big.mark = ",", scientific = FALSE),
    hovertemplate = "%{x} locations: %{text}<extra></extra>",
    source = "trekker_positions"
  )
  trekker_plotly(plotly::layout(
    plot,
    xaxis = list(title = "Assigned spatial locations"),
    yaxis = list(title = "Nuclei", rangemode = "tozero")
  ))
})

output[["trekker_cohort_qc_empty"]] <- renderUI({
  selected <- trekker_selected_nuclei()
  data <- trekker_cohort_qc(trekker_metadata(), selected)
  if (!length(selected)) {
    trekker_empty(
      "Brush nuclei on the primary plot to compare an active cohort."
    )
  } else if (!nrow(data)) {
    trekker_empty("Nucleus-level RNA QC metadata was not provided.")
  }
})

output[["trekker_cohort_qc_plot"]] <- plotly::renderPlotly({
  selected <- trekker_selected_nuclei()
  req(length(selected))
  coordinates <- trekker_section_coordinates()
  metadata <- trekker_metadata()
  req(!is.null(metadata))
  metadata <- metadata[
    intersect(as.character(coordinates$barcode), rownames(metadata)),
    ,
    drop = FALSE
  ]
  table <- trekker_cohort_qc(metadata, selected)
  req(nrow(table))
  plot <- plotly::plot_ly(
    table,
    x = ~cohort,
    y = ~value,
    color = ~cohort,
    colors = c("#9aa0a6", "#ff6d16"),
    type = "box",
    boxpoints = FALSE,
    split = ~field,
    hoverinfo = "y+name",
    source = "trekker_cohort_qc"
  )
  trekker_plotly(plotly::layout(
    plot,
    boxmode = "group",
    xaxis = list(title = ""),
    yaxis = list(title = "Value")
  ))
})

output[["trekker_metrics_table"]] <- DT::renderDT({
  metrics <- trekker_slot()$metrics
  req(!is.null(metrics))
  DT::datatable(
    metrics,
    rownames = FALSE,
    filter = "top",
    options = list(pageLength = 25L)
  )
})

output[["trekker_markers_empty"]] <- renderUI({
  if (is.null(trekker_slot()$cluster_markers)) {
    trekker_empty("Cluster marker results were not provided.")
  }
})

trekker_marker_dot_results <- reactive({
  markers <- trekker_slot()$cluster_markers
  if (is.null(markers) || !nrow(markers)) {
    return(data.frame())
  }
  clusters <- trekker_natural_levels(markers$cluster)
  top <- do.call(
    rbind,
    lapply(clusters, function(cluster) {
      utils::head(trekker_top_markers(markers, cluster, n = NULL), 3L)
    })
  )
  genes <- unique(as.character(top$gene))
  genes <- genes[genes %in% trekker_gene_names()]
  keys <- as.character(trekker_slot()$coordinates$barcode)
  groups <- trekker_marker_group_values(trekker_metadata(), keys, clusters)
  if (!length(genes) || is.null(groups)) {
    return(data.frame())
  }
  expression <- tryCatch(
    data_set()$getExpressionMatrix(cells = keys, genes = genes),
    error = function(error) NULL
  )
  if (is.null(expression)) {
    return(data.frame())
  }
  if (!is.null(colnames(expression))) {
    at <- match(keys, colnames(expression))
    if (anyNA(at)) {
      return(data.frame())
    }
    expression <- expression[, at, drop = FALSE]
  }
  trekker_marker_dot(markers, expression, groups, n_per_cluster = 3L)
})

output[["trekker_marker_dot_empty"]] <- renderUI({
  if (!nrow(trekker_marker_dot_results())) {
    trekker_empty(
      "Marker profiles require matching cluster metadata and expression data."
    )
  }
})

output[["trekker_marker_dot_plot"]] <- plotly::renderPlotly({
  table <- trekker_marker_dot_results()
  req(nrow(table))
  table$gene_key <- table$gene
  table$cluster_key <- table$cluster
  gene_levels <- unique(table$gene)
  cluster_levels <- rev(unique(table$cluster))
  table$tooltip <- paste0(
    "Gene: ",
    table$gene_key,
    "<br>Cluster: ",
    table$cluster_key,
    "<br>Average expression: ",
    signif(table$average, 4L),
    "<br>Expressed nuclei: ",
    signif(table$percent, 4L),
    "%",
    "<br>Scaled average: ",
    signif(table$scaled_average, 4L)
  )
  plot <- plotly::plot_ly(
    table,
    x = ~gene,
    y = ~cluster,
    key = ~gene_key,
    customdata = ~cluster_key,
    size = ~percent,
    sizes = c(3, 18),
    color = ~scaled_average,
    colors = c("#2166ac", "#f7f7f7", "#b2182b"),
    text = ~tooltip,
    hoverinfo = "text",
    type = "scatter",
    mode = "markers",
    showlegend = FALSE,
    marker = list(opacity = 0.9),
    source = "trekker_marker_dot"
  )
  trekker_plotly(
    plotly::layout(
      plot,
      xaxis = list(
        title = "Top official markers",
        tickangle = -55,
        categoryorder = "array",
        categoryarray = gene_levels
      ),
      yaxis = list(
        title = "Cluster",
        categoryorder = "array",
        categoryarray = cluster_levels
      )
    ),
    margin = list(l = 65, r = 35, b = 150, t = 15)
  )
})

observeEvent(
  plotly::event_data("plotly_click", source = "trekker_marker_dot"),
  {
    click <- plotly::event_data("plotly_click", source = "trekker_marker_dot")
    trekker_select_gene(click$key, click$customdata)
  },
  ignoreInit = TRUE
)

output[["trekker_marker_controls"]] <- renderUI({
  table <- trekker_slot()$cluster_markers
  if (is.null(table) || !nrow(table)) {
    return(NULL)
  }
  clusters <- trekker_natural_levels(table$cluster)
  selected <- input[["trekker_marker_cluster"]]
  if (is.null(selected) || !selected %in% clusters) {
    selected <- clusters[[1L]]
  }
  div(
    class = "tk-inline-controls",
    selectInput(
      "trekker_marker_cluster",
      "Cluster",
      choices = clusters,
      selected = selected
    )
  )
})

trekker_selected_markers <- reactive({
  table <- trekker_slot()$cluster_markers
  req(!is.null(table), nrow(table))
  cluster <- input[["trekker_marker_cluster"]] %||%
    trekker_natural_levels(table$cluster)[[1L]]
  trekker_top_markers(table, cluster, n = NULL)
})

output[["trekker_marker_plot"]] <- plotly::renderPlotly({
  table <- utils::head(trekker_selected_markers(), 25L)
  req(nrow(table))
  table$gene_axis <- factor(table$gene, levels = rev(unique(table$gene)))
  table$tooltip <- paste0(
    "Gene: ",
    table$gene,
    "<br>Cluster: ",
    table$cluster,
    "<br>avg_log2FC: ",
    signif(table$avg_log2FC, 4L),
    "<br>pct.1: ",
    signif(table$pct.1, 4L),
    "<br>pct.2: ",
    signif(table$pct.2, 4L),
    "<br>adjusted P: ",
    format(table$p_val_adj, scientific = TRUE)
  )
  plot <- plotly::plot_ly(
    table,
    x = ~avg_log2FC,
    y = ~gene_axis,
    key = ~gene,
    text = ~tooltip,
    hoverinfo = "text",
    type = "bar",
    orientation = "h",
    marker = list(
      color = ~pct.1,
      colorscale = "Blues",
      colorbar = list(title = "pct.1")
    ),
    source = "trekker_markers"
  )
  trekker_plotly(
    plotly::layout(
      plot,
      xaxis = list(title = "Average log2 fold change"),
      yaxis = list(title = "")
    ),
    margin = list(l = 90, r = 55, b = 55, t = 10)
  )
})

output[["trekker_markers_table"]] <- DT::renderDT({
  table <- trekker_selected_markers()
  DT::datatable(
    table,
    rownames = FALSE,
    filter = "top",
    selection = "single",
    options = list(pageLength = 25L)
  )
})
observeEvent(input[["trekker_markers_table_rows_selected"]], {
  row <- input[["trekker_markers_table_rows_selected"]]
  table <- trekker_selected_markers()
  req(length(row) == 1L, !is.null(table$gene[[row]]))
  trekker_select_gene(table$gene[[row]], table$cluster[[row]])
})

observeEvent(
  plotly::event_data("plotly_click", source = "trekker_markers"),
  {
    click <- plotly::event_data("plotly_click", source = "trekker_markers")
    trekker_select_gene(click$key, input[["trekker_marker_cluster"]])
  },
  ignoreInit = TRUE
)

trekker_evidence_gene <- reactive({
  available <- trekker_gene_names()
  requested <- c(input[["trekker_evidence_gene"]], input[["trekker_gene"]])
  requested <- requested[
    !is.na(requested) & nzchar(requested) & requested %in% available
  ]
  if (length(requested)) {
    return(requested[[1L]])
  }
  suggested <- trekker_gene_suggest(trekker_slot(), available)
  if (length(suggested)) suggested[[1L]] else NULL
})

output[["trekker_gene_evidence_controls"]] <- renderUI({
  suggested <- trekker_gene_suggest(trekker_slot(), trekker_gene_names())
  if (!length(suggested)) {
    return(NULL)
  }
  div(
    class = "tk-inline-controls",
    selectizeInput(
      "trekker_evidence_gene",
      "Gene",
      choices = suggested,
      selected = trekker_evidence_gene(),
      options = list(create = TRUE, placeholder = "Search or type a gene...")
    )
  )
})

observeEvent(input[["trekker_evidence_gene"]], {
  gene <- input[["trekker_evidence_gene"]]
  req(gene %in% trekker_gene_names())
  if (!identical(gene, input[["trekker_gene"]])) trekker_select_gene(gene)
})

trekker_gene_evidence_results <- reactive({
  gene <- trekker_evidence_gene()
  preview <- trekker_spatial_preview()
  empty <- list(
    gene = gene,
    coordinates = preview$coordinates,
    expression = NULL,
    umap = NULL,
    evidence = list(
      distribution = data.frame(),
      markers = data.frame(),
      moran = data.frame()
    )
  )
  if (is.null(gene) || !nrow(preview$coordinates)) {
    return(empty)
  }
  keys <- as.character(preview$coordinates$barcode)
  expression <- tryCatch(
    data_set()$getExpressionMatrix(cells = keys, genes = gene),
    error = function(error) NULL
  )
  if (is.null(expression)) {
    return(empty)
  }
  if (is.null(dim(expression))) {
    expression <- matrix(
      expression,
      nrow = 1L,
      dimnames = list(gene, keys)
    )
  }
  if (!is.null(colnames(expression))) {
    at <- match(keys, colnames(expression))
    if (anyNA(at)) {
      return(empty)
    }
    expression <- expression[, at, drop = FALSE]
  }
  groups <- trekker_default_groups()$values
  groups <- if (is.null(groups)) {
    rep("All nuclei", length(keys))
  } else {
    as.character(groups[keys])
  }
  projections <- tryCatch(
    data_set()$availableProjections(),
    error = function(error) character()
  )
  umap_name <- projections[tolower(projections) == "umap"]
  umap <- if (length(umap_name)) {
    values <- data_set()$getProjection(umap_name[[1L]])
    values[match(keys, rownames(values)), seq_len(2L), drop = FALSE]
  } else {
    NULL
  }
  empty$expression <- expression
  empty$umap <- umap
  empty$evidence <- trekker_gene_evidence(
    gene,
    expression,
    groups,
    trekker_slot()$cluster_markers,
    trekker_slot()$moran
  )
  empty
})

output[["trekker_gene_evidence_empty"]] <- renderUI({
  result <- trekker_gene_evidence_results()
  if (is.null(result$expression)) {
    trekker_empty(
      "Gene evidence requires expression data for the selected gene."
    )
  }
})

output[["trekker_gene_evidence_maps"]] <- plotly::renderPlotly({
  result <- trekker_gene_evidence_results()
  req(!is.null(result$expression))
  coordinates <- result$coordinates
  values <- as.numeric(result$expression[result$gene, ])
  limits <- stats::quantile(
    values[is.finite(values)],
    c(0.05, 0.95),
    na.rm = TRUE,
    names = FALSE
  )
  colour <- pmax(limits[[1L]], pmin(limits[[2L]], values))
  point_size <- if (length(values) > 5000L) 2 else 4
  map <- function(x, y, scale) {
    table <- data.frame(
      barcode = as.character(coordinates$barcode),
      x = as.numeric(x),
      y = as.numeric(y),
      value = values,
      colour = colour,
      stringsAsFactors = FALSE
    )
    table <- table[order(table$colour, na.last = TRUE), , drop = FALSE]
    table$tooltip <- paste0(
      "Barcode: ",
      table$barcode,
      "<br>",
      result$gene,
      ": ",
      signif(table$value, 4L)
    )
    plotly::layout(
      plotly::plot_ly(
        table,
        x = ~x,
        y = ~y,
        key = ~barcode,
        text = ~tooltip,
        hoverinfo = "text",
        type = "scattergl",
        mode = "markers",
        showlegend = FALSE,
        marker = list(
          size = point_size,
          color = table$colour,
          colorscale = "Viridis",
          showscale = scale,
          colorbar = if (scale) list(title = result$gene) else NULL,
          opacity = 0.85
        ),
        source = "trekker_gene_evidence"
      ),
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE)
    )
  }
  has_umap <- !is.null(result$umap)
  plots <- list(map(coordinates$x, coordinates$y, !has_umap))
  titles <- "Physical"
  if (has_umap) {
    plots[[2L]] <- map(
      result$umap[, 1L],
      result$umap[, 2L],
      TRUE
    )
    titles <- c(titles, "UMAP")
  }
  plot <- do.call(
    plotly::subplot,
    c(
      plots,
      list(nrows = 1L, shareX = FALSE, shareY = FALSE, margin = 0.06)
    )
  )
  plot <- plotly::layout(
    plot,
    annotations = trekker_subplot_titles(titles, length(titles))
  )
  trekker_plotly(plot, margin = list(l = 10, r = 70, b = 15, t = 45))
})

output[["trekker_gene_evidence_distribution"]] <- plotly::renderPlotly({
  result <- trekker_gene_evidence_results()
  table <- result$evidence$distribution
  req(nrow(table))
  table$tooltip <- paste0(
    "Cluster: ",
    table$cluster,
    "<br>Average: ",
    signif(table$average, 4L),
    "<br>Median: ",
    signif(table$median, 4L),
    "<br>Expressed nuclei: ",
    signif(table$percent, 4L),
    "%",
    "<br>Nuclei: ",
    table$nuclei,
    "<br>Interpretation: exploratory layout agreement; not transcriptomic agreement"
  )
  plot <- plotly::plot_ly(
    table,
    x = ~cluster,
    y = ~average,
    size = ~percent,
    sizes = c(6, 24),
    text = ~tooltip,
    hoverinfo = "text",
    type = "scatter",
    mode = "markers",
    marker = list(color = "#ff6d16", opacity = 0.85),
    source = "trekker_gene_distribution"
  )
  trekker_plotly(plotly::layout(
    plot,
    xaxis = list(title = "Cluster", type = "category"),
    yaxis = list(title = paste("Average", result$gene, "expression"))
  ))
})

output[["trekker_gene_evidence_table"]] <- DT::renderDT({
  result <- trekker_gene_evidence_results()
  marker_rows <- if (nrow(result$evidence$markers)) {
    table <- result$evidence$markers
    data.frame(
      Source = "Cluster marker",
      Cluster = as.character(table$cluster),
      Statistic = "avg_log2FC / pct.1 / pct.2 / adjusted P",
      Value = paste(
        signif(as.numeric(table$avg_log2FC), 4L),
        signif(as.numeric(table$pct.1), 4L),
        signif(as.numeric(table$pct.2), 4L),
        format(as.numeric(table$p_val_adj), scientific = TRUE),
        sep = " / "
      ),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame()
  }
  moran_rows <- if (nrow(result$evidence$moran)) {
    table <- result$evidence$moran
    data.frame(
      Source = "Spatial gene",
      Cluster = "—",
      Statistic = "Moran's I / P / rank / official flag",
      Value = paste(
        signif(as.numeric(table$MoransI_observed), 4L),
        format(as.numeric(table$MoransI_p.value), scientific = TRUE),
        table$moransi.spatially.variable.rank,
        table$moransi.spatially.variable,
        sep = " / "
      ),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame()
  }
  table <- rbind(marker_rows, moran_rows)
  req(nrow(table))
  DT::datatable(
    table,
    rownames = FALSE,
    options = list(dom = "t", paging = FALSE)
  )
})

output[["trekker_multigene_controls"]] <- renderUI({
  suggested <- trekker_gene_suggest(trekker_slot(), trekker_gene_names())
  if (!length(suggested)) {
    return(NULL)
  }
  div(
    class = "tk-inline-controls",
    selectizeInput(
      "trekker_score_genes",
      "Genes (up to 6)",
      choices = suggested,
      selected = utils::head(suggested, 2L),
      multiple = TRUE,
      options = list(
        create = TRUE,
        maxItems = 6,
        plugins = list("remove_button")
      )
    ),
    selectInput(
      "trekker_multigene_mode",
      "Display mode",
      choices = c(
        "Mean expression" = "mean",
        "Separate panels" = "separate",
        "RGB co-expression" = "rgb"
      ),
      selected = "mean"
    )
  )
})

trekker_multigene_results <- reactive({
  mode <- input[["trekker_multigene_mode"]]
  if (is.null(mode)) {
    mode <- "mean"
  }
  genes <- unique(as.character(input[["trekker_score_genes"]]))
  genes <- genes[genes %in% trekker_gene_names()]
  if (identical(mode, "rgb")) {
    genes <- utils::head(genes, 3L)
  }
  preview <- trekker_spatial_preview()
  if (!length(genes) || !nrow(preview$coordinates)) {
    return(list(
      mode = mode,
      genes = genes,
      coordinates = preview$coordinates,
      expression = NULL
    ))
  }
  keys <- as.character(preview$coordinates$barcode)
  expression <- tryCatch(
    data_set()$getExpressionMatrix(cells = keys, genes = genes),
    error = function(error) NULL
  )
  if (is.null(expression)) {
    return(list(
      mode = mode,
      genes = genes,
      coordinates = preview$coordinates,
      expression = NULL
    ))
  }
  if (!is.null(colnames(expression))) {
    at <- match(keys, colnames(expression))
    if (anyNA(at)) {
      return(list(
        mode = mode,
        genes = genes,
        coordinates = preview$coordinates,
        expression = NULL
      ))
    }
    expression <- expression[, at, drop = FALSE]
  }
  list(
    mode = mode,
    genes = genes,
    coordinates = preview$coordinates,
    expression = expression
  )
})

output[["trekker_multigene_empty"]] <- renderUI({
  result <- trekker_multigene_results()
  if (!length(result$genes)) {
    trekker_empty("Select one to six genes to map their spatial expression.")
  } else if (identical(result$mode, "rgb") && length(result$genes) < 2L) {
    trekker_empty("Select two or three genes for RGB co-expression.")
  } else if (is.null(result$expression)) {
    trekker_empty("Expression data was unavailable for the selected genes.")
  }
})

output[["trekker_multigene_plot"]] <- plotly::renderPlotly({
  result <- trekker_multigene_results()
  req(
    length(result$genes),
    !is.null(result$expression),
    nrow(result$coordinates)
  )
  if (identical(result$mode, "rgb")) {
    req(length(result$genes) >= 2L)
  }
  point_size <- if (nrow(result$coordinates) > 5000L) 2 else 4
  expression_plot <- function(gene) {
    table <- result$coordinates
    table$value <- as.numeric(result$expression[gene, ])
    table <- table[order(table$value, na.last = TRUE), , drop = FALSE]
    table$tooltip <- paste0(
      "Gene: ",
      gene,
      "<br>Barcode: ",
      table$barcode,
      "<br>Expression: ",
      signif(table$value, 4L)
    )
    plotly::layout(
      plotly::plot_ly(
        table,
        x = ~x,
        y = ~y,
        key = ~barcode,
        text = ~tooltip,
        hoverinfo = "text",
        type = "scattergl",
        mode = "markers",
        showlegend = FALSE,
        marker = list(
          size = point_size,
          color = table$value,
          colorscale = "Viridis",
          showscale = FALSE,
          opacity = 0.85
        ),
        source = "trekker_multigene"
      ),
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE)
    )
  }

  if (identical(result$mode, "separate")) {
    plots <- lapply(result$genes, expression_plot)
    columns <- min(3L, length(plots))
    plot <- do.call(
      plotly::subplot,
      c(
        plots,
        list(
          nrows = ceiling(length(plots) / columns),
          shareX = TRUE,
          shareY = TRUE,
          margin = 0.035
        )
      )
    )
    plot <- plotly::layout(
      plot,
      annotations = trekker_subplot_titles(result$genes, columns)
    )
    return(trekker_plotly(
      plot,
      margin = list(l = 10, r = 10, b = 10, t = 35)
    ))
  }

  if (identical(result$mode, "rgb")) {
    channels <- c("R", "G", "B")[seq_along(result$genes)]
    values <- lapply(seq_along(result$genes), function(i) {
      as.numeric(result$expression[result$genes[[i]], ])
    })
    table <- result$coordinates
    table$colour <- blend_genes_to_rgb(
      r = values[[1L]],
      g = values[[2L]],
      b = if (length(values) >= 3L) values[[3L]] else NULL
    )
    table$tooltip <- paste0("Barcode: ", table$barcode)
    for (i in seq_along(result$genes)) {
      table$tooltip <- paste0(
        table$tooltip,
        "<br>",
        channels[[i]],
        ": ",
        result$genes[[i]],
        " = ",
        signif(values[[i]], 4L)
      )
    }
    plot <- plotly::plot_ly(
      table,
      x = ~x,
      y = ~y,
      key = ~barcode,
      text = ~tooltip,
      hoverinfo = "text",
      type = "scattergl",
      mode = "markers",
      showlegend = FALSE,
      marker = list(
        size = point_size,
        color = table$colour,
        opacity = 0.85
      ),
      source = "trekker_multigene"
    )
    plot <- plotly::layout(
      plot,
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      annotations = list(list(
        x = 0,
        y = 1.04,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        showarrow = FALSE,
        text = paste0(channels, ": ", result$genes, collapse = " · ")
      ))
    )
    return(trekker_plotly(
      plot,
      margin = list(l = 10, r = 10, b = 10, t = 35)
    ))
  }

  table <- result$coordinates
  table$value <- as.numeric(colMeans(result$expression, na.rm = TRUE))
  table <- table[order(table$value, na.last = TRUE), , drop = FALSE]
  table$tooltip <- paste0(
    "Barcode: ",
    table$barcode,
    "<br>Mean expression: ",
    signif(table$value, 4L),
    "<br>Genes: ",
    paste(result$genes, collapse = ", ")
  )
  plot <- plotly::plot_ly(
    table,
    x = ~x,
    y = ~y,
    key = ~barcode,
    text = ~tooltip,
    hoverinfo = "text",
    type = "scattergl",
    mode = "markers",
    marker = list(
      size = point_size,
      color = table$value,
      colorscale = "Viridis",
      colorbar = list(title = "Mean expression"),
      opacity = 0.85
    ),
    source = "trekker_multigene"
  )
  trekker_plotly(
    plotly::layout(
      plot,
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE)
    ),
    margin = list(l = 10, r = 100, b = 10, t = 10)
  )
})

trekker_spatial_gallery_results <- reactive({
  genes <- trekker_top_spatial_genes(
    trekker_slot()$moran,
    available_genes = trekker_gene_names(),
    n = 6L
  )
  preview <- trekker_spatial_preview()
  if (!length(genes) || !nrow(preview$coordinates)) {
    return(list(
      genes = character(),
      coordinates = preview$coordinates,
      expression = NULL,
      total = preview$total,
      sampled = preview$sampled
    ))
  }
  keys <- as.character(preview$coordinates$barcode)
  expression <- tryCatch(
    data_set()$getExpressionMatrix(cells = keys, genes = genes),
    error = function(error) NULL
  )
  if (!is.null(expression) && !is.null(colnames(expression))) {
    at <- match(keys, colnames(expression))
    if (anyNA(at)) {
      expression <- NULL
    } else {
      expression <- expression[, at, drop = FALSE]
    }
  }
  list(
    genes = genes,
    coordinates = preview$coordinates,
    expression = expression,
    total = preview$total,
    sampled = preview$sampled
  )
})

output[["trekker_spatial_gallery_empty"]] <- renderUI({
  result <- trekker_spatial_gallery_results()
  if (!length(result$genes) || is.null(result$expression)) {
    trekker_empty(
      "Spatial-gene maps require Moran results and expression data."
    )
  }
})

output[["trekker_spatial_gallery_note"]] <- renderUI({
  result <- trekker_spatial_gallery_results()
  if (result$sampled && !is.null(result$expression)) {
    tags$p(
      class = "tk-note",
      paste(
        "Showing the same deterministic",
        format(nrow(result$coordinates), big.mark = ","),
        "of",
        format(result$total, big.mark = ","),
        "nuclei in every panel. Click a panel to colour the primary view."
      )
    )
  } else if (!is.null(result$expression)) {
    tags$p(class = "tk-note", "Click a panel to colour the primary view.")
  }
})

output[["trekker_spatial_gene_gallery"]] <- plotly::renderPlotly({
  result <- trekker_spatial_gallery_results()
  req(length(result$genes), !is.null(result$expression))
  coordinates <- result$coordinates
  point_size <- if (nrow(coordinates) > 5000L) 2 else 4
  plots <- lapply(result$genes, function(gene) {
    value <- as.numeric(result$expression[gene, ])
    limits <- stats::quantile(
      value[is.finite(value)],
      c(0.05, 0.95),
      na.rm = TRUE,
      names = FALSE
    )
    clipped <- pmax(limits[[1L]], pmin(limits[[2L]], value))
    order_at <- order(clipped, na.last = TRUE)
    current <- coordinates[order_at, , drop = FALSE]
    current$value <- value[order_at]
    current$colour <- clipped[order_at]
    current$tooltip <- paste0(
      "Gene: ",
      gene,
      "<br>Barcode: ",
      current$barcode,
      "<br>Expression: ",
      signif(current$value, 4L)
    )
    plotly::layout(
      plotly::plot_ly(
        current,
        x = ~x,
        y = ~y,
        key = ~barcode,
        customdata = rep(gene, nrow(current)),
        text = ~tooltip,
        hoverinfo = "text",
        type = "scattergl",
        mode = "markers",
        showlegend = FALSE,
        marker = list(
          size = point_size,
          color = current$colour,
          colorscale = "Viridis",
          showscale = FALSE,
          opacity = 0.85
        ),
        source = "trekker_spatial_gallery"
      ),
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE)
    )
  })
  plot <- do.call(
    plotly::subplot,
    c(
      plots,
      list(
        nrows = if (length(plots) > 3L) 2L else 1L,
        shareX = TRUE,
        shareY = TRUE,
        margin = 0.035
      )
    )
  )
  plot <- plotly::layout(
    plot,
    annotations = trekker_subplot_titles(result$genes, min(3L, length(plots)))
  )
  trekker_plotly(plot, margin = list(l = 10, r = 10, b = 10, t = 35))
})

observeEvent(
  plotly::event_data("plotly_click", source = "trekker_spatial_gallery"),
  {
    click <- plotly::event_data(
      "plotly_click",
      source = "trekker_spatial_gallery"
    )
    trekker_select_gene(click$customdata)
  },
  ignoreInit = TRUE
)

trekker_moran_results <- reactive({
  trekker_ranked_moran(trekker_slot()$moran)
})

output[["trekker_moran_empty"]] <- renderUI({
  if (is.null(trekker_slot()$moran)) {
    trekker_empty("Spatial gene results were not provided.")
  }
})

output[["trekker_moran_plot"]] <- plotly::renderPlotly({
  table <- trekker_moran_results()
  req(nrow(table))
  flag <- as.character(table$moransi.spatially.variable)
  table$tooltip <- paste0(
    "Gene: ",
    table$gene,
    "<br>Rank: ",
    table$moransi.spatially.variable.rank,
    "<br>Moran's I: ",
    signif(table$MoransI_observed, 4L),
    "<br>P value: ",
    format(table$MoransI_p.value, scientific = TRUE),
    "<br>Official flag: ",
    flag
  )
  plot <- plotly::plot_ly(
    table,
    x = ~moransi.spatially.variable.rank,
    y = ~MoransI_observed,
    key = ~gene,
    text = ~tooltip,
    hoverinfo = "text",
    type = "scatter",
    mode = "markers+lines",
    color = ~flag,
    colors = c("FALSE" = "#9aa0a6", "TRUE" = "#ff6d16"),
    marker = list(size = 7),
    source = "trekker_moran"
  )
  trekker_plotly(plotly::layout(
    plot,
    xaxis = list(title = "Official Moran rank"),
    yaxis = list(title = "Observed Moran's I")
  ))
})

observeEvent(
  plotly::event_data("plotly_click", source = "trekker_moran"),
  {
    click <- plotly::event_data("plotly_click", source = "trekker_moran")
    trekker_select_gene(click$key)
  },
  ignoreInit = TRUE
)

trekker_marker_moran_results <- reactive({
  trekker_marker_moran(
    trekker_slot()$cluster_markers,
    trekker_slot()$moran
  )
})

output[["trekker_marker_moran_plot"]] <- plotly::renderPlotly({
  table <- trekker_marker_moran_results()
  req(nrow(table))
  table$cluster <- as.character(table$cluster)
  table$point_size <- 7 +
    14 * abs(table$pct_delta) / max(abs(table$pct_delta), na.rm = TRUE)
  table$point_size[!is.finite(table$point_size)] <- 8
  table$tooltip <- paste0(
    "Gene: ",
    table$gene,
    "<br>Cluster: ",
    table$cluster,
    "<br>avg_log2FC: ",
    signif(table$avg_log2FC, 4L),
    "<br>pct.1 / pct.2: ",
    signif(table$pct.1, 3L),
    " / ",
    signif(table$pct.2, 3L),
    "<br>adjusted P: ",
    format(table$p_val_adj, scientific = TRUE),
    "<br>Moran's I: ",
    signif(table$MoransI_observed, 4L),
    "<br>Moran rank: ",
    table$moransi.spatially.variable.rank,
    "<br>Moran P: ",
    format(table$MoransI_p.value, scientific = TRUE)
  )
  plot <- plotly::plot_ly(
    table,
    x = ~avg_log2FC,
    y = ~MoransI_observed,
    color = ~cluster,
    size = ~point_size,
    sizes = c(7, 21),
    key = ~gene,
    customdata = ~cluster,
    text = ~tooltip,
    hoverinfo = "text",
    type = "scatter",
    mode = "markers",
    source = "trekker_evidence"
  )
  trekker_plotly(
    plotly::layout(
      plot,
      xaxis = list(title = "Marker average log2 fold change"),
      yaxis = list(title = "Observed Moran's I"),
      legend = list(
        title = list(text = "Cluster"),
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.22,
        yanchor = "top"
      )
    ),
    margin = list(l = 55, r = 20, b = 115, t = 10)
  )
})

observeEvent(
  plotly::event_data("plotly_click", source = "trekker_evidence"),
  {
    click <- plotly::event_data("plotly_click", source = "trekker_evidence")
    trekker_select_gene(click$key, click$customdata)
  },
  ignoreInit = TRUE
)

output[["trekker_moran_table"]] <- DT::renderDT({
  table <- trekker_moran_results()
  req(nrow(table))
  DT::datatable(
    table,
    rownames = FALSE,
    filter = "top",
    selection = "single",
    options = list(pageLength = 25L)
  )
})
observeEvent(input[["trekker_moran_table_rows_selected"]], {
  row <- input[["trekker_moran_table_rows_selected"]]
  table <- trekker_moran_results()
  req(length(row) == 1L, !is.null(table$gene[[row]]))
  trekker_select_gene(table$gene[[row]])
})

output[["trekker_marker_moran_empty"]] <- renderUI({
  if (!nrow(trekker_marker_moran_results())) {
    trekker_empty("Marker and Moran results have no genes in common.")
  }
})

trekker_neighbourhood_results <- reactive({
  coordinates <- trekker_analysis_coordinates()
  group_info <- trekker_default_groups()
  groups <- if (is.null(group_info$values)) {
    NULL
  } else {
    as.character(group_info$values[as.character(coordinates$barcode)])
  }
  if (is.null(groups)) {
    result <- trekker_neighbourhood(
      coordinates,
      rep(NA_character_, nrow(coordinates))
    )
    result$message <- "Categorical cluster metadata is required for neighbourhoods."
    return(result)
  }
  trekker_neighbourhood(
    coordinates,
    groups,
    permutations = 199L,
    seed = 1L
  )
})

output[["trekker_neighbourhood_controls"]] <- renderUI({
  result <- trekker_neighbourhood_results()
  if (!nrow(result$composition)) {
    return(NULL)
  }
  clusters <- unique(as.character(result$composition$selected_group))
  selected <- input[["trekker_neighbour_cluster"]]
  if (is.null(selected) || !selected %in% clusters) {
    selected <- clusters[[1L]]
  }
  div(
    class = "tk-inline-controls",
    selectInput(
      "trekker_neighbour_cluster",
      "Selected cluster",
      choices = clusters,
      selected = selected
    )
  )
})

output[["trekker_neighbourhood_note"]] <- renderUI({
  result <- trekker_neighbourhood_results()
  if (!is.null(result$message)) {
    return(trekker_empty(result$message))
  }
  tags$p(
    class = "tk-note",
    paste0(
      if (
        isTRUE(trekker_positioning_cohort()$active) &&
          identical(
            trekker_positioning_cohort()$section,
            input[["trekker_section"]]
          )
      ) {
        "Evidence-filtered cohort · "
      } else {
        ""
      },
      "Six nearest neighbours · ",
      format(result$n_sample, big.mark = ","),
      if (result$sampled) " deterministically sampled nuclei" else " nuclei",
      ". Empirical P values use 199 deterministic label permutations;",
      "BH adjustment is applied across unique cluster pairs. These are ",
      "exploratory summaries, not formal spatial inference."
    )
  )
})

output[["trekker_adjacency_plot"]] <- plotly::renderPlotly({
  result <- trekker_neighbourhood_results()
  table <- result$adjacency
  req(nrow(table) > 0L)
  groups <- unique(as.character(table$group_a))
  index <- cbind(match(table$group_a, groups), match(table$group_b, groups))
  z <- matrix(NA_real_, length(groups), length(groups))
  z[index] <- table$enrichment
  finite <- z[is.finite(z)]
  floor_value <- if (length(finite)) min(finite) - 1 else -1
  display_z <- z
  display_z[is.infinite(display_z) & display_z < 0] <- floor_value
  hover <- matrix("", length(groups), length(groups))
  hover[index] <- paste0(
    table$group_a,
    " × ",
    table$group_b,
    "<br>Observed edges: ",
    table$observed,
    "<br>Expected edges: ",
    signif(table$expected, 4L),
    "<br>log2 enrichment: ",
    signif(table$enrichment, 4L),
    "<br>Empirical P: ",
    format(table$p_value, scientific = TRUE),
    "<br>BH-adjusted P: ",
    format(table$p_adj, scientific = TRUE),
    "<br>Nuclei: ",
    result$n_sample
  )
  plot <- plotly::plot_ly(
    x = groups,
    y = groups,
    z = display_z,
    text = hover,
    type = "heatmap",
    colorscale = "RdBu",
    reversescale = TRUE,
    zmid = 0,
    hoverinfo = "text",
    source = "trekker_neighbourhood"
  )
  trekker_plotly(
    plotly::layout(
      plot,
      xaxis = list(
        title = "Neighbour cluster",
        type = "category",
        categoryorder = "array",
        categoryarray = groups
      ),
      yaxis = list(
        title = "Cluster",
        type = "category",
        categoryorder = "array",
        categoryarray = groups,
        autorange = "reversed"
      )
    ),
    margin = list(l = 80, r = 20, b = 80, t = 10)
  )
})

observeEvent(
  plotly::event_data("plotly_click", source = "trekker_neighbourhood"),
  {
    click <- plotly::event_data(
      "plotly_click",
      source = "trekker_neighbourhood"
    )
    groups <- unique(c(as.character(click$x), as.character(click$y)))
    groups <- groups[!is.na(groups) & nzchar(groups)]
    req(length(groups))
    session$sendCustomMessage(
      "cell_view_focus_groups",
      list(id = "trekker_projection", groups = groups)
    )
  },
  ignoreInit = TRUE
)

output[["trekker_neighbour_composition"]] <- plotly::renderPlotly({
  result <- trekker_neighbourhood_results()
  table <- result$composition
  req(nrow(table) > 0L)
  selected <- input[["trekker_neighbour_cluster"]] %||%
    unique(table$selected_group)[[1L]]
  table <- table[table$selected_group == selected, , drop = FALSE]
  groups <- unique(as.character(table$group))
  long <- rbind(
    data.frame(
      group = table$group,
      series = "Neighbours",
      share = table$observed_share,
      count = table$count
    ),
    data.frame(
      group = table$group,
      series = "Global",
      share = table$global_share,
      count = NA_real_
    )
  )
  long$tooltip <- paste0(
    long$series,
    " · ",
    long$group,
    "<br>Share: ",
    scales::percent(long$share, accuracy = 0.1),
    ifelse(is.na(long$count), "", paste0("<br>Edges: ", long$count))
  )
  plot <- plotly::plot_ly(
    long,
    x = ~group,
    y = ~share,
    color = ~series,
    colors = c("Neighbours" = "#ff6d16", "Global" = "#9aa0a6"),
    customdata = ~group,
    text = ~tooltip,
    hoverinfo = "text",
    type = "bar",
    source = "trekker_neighbour_composition"
  )
  trekker_plotly(
    plotly::layout(
      plot,
      barmode = "group",
      xaxis = list(
        title = "Neighbour cluster",
        type = "category",
        categoryorder = "array",
        categoryarray = groups
      ),
      yaxis = list(title = "Share", tickformat = ".0%")
    ),
    margin = list(l = 55, r = 20, b = 80, t = 10)
  )
})

observeEvent(
  plotly::event_data(
    "plotly_click",
    source = "trekker_neighbour_composition"
  ),
  {
    click <- plotly::event_data(
      "plotly_click",
      source = "trekker_neighbour_composition"
    )
    group <- as.character(click$customdata)
    req(length(group) == 1L, !is.na(group), nzchar(group))
    session$sendCustomMessage(
      "cell_view_focus_groups",
      list(id = "trekker_projection", groups = group)
    )
  },
  ignoreInit = TRUE
)

trekker_spatial_profile_results <- reactive({
  coordinates <- trekker_analysis_coordinates()
  groups <- trekker_default_groups()$values
  if (is.null(groups)) {
    return(data.frame())
  }
  trekker_spatial_profiles(
    coordinates,
    as.character(groups[as.character(coordinates$barcode)])
  )
})

output[["trekker_spatial_profile_note"]] <- renderUI({
  table <- trekker_spatial_profile_results()
  if (!nrow(table) || !isTRUE(attr(table, "sampled"))) {
    return(NULL)
  }
  tags$p(
    class = "tk-note",
    paste(
      "Nearest-neighbour profile calculated on a deterministic preview of",
      format(attr(table, "n_sample"), big.mark = ","),
      "nuclei to bound memory use."
    )
  )
})

output[["trekker_spatial_profile_plot"]] <- plotly::renderPlotly({
  table <- trekker_spatial_profile_results()
  req(nrow(table))
  definitions <- list(
    list(field = "nuclei", title = "Nuclei", color = "#ff6d16"),
    list(field = "hull_area", title = "Convex-hull area", color = "#5b8def"),
    list(
      field = "median_radius",
      title = "Median centroid distance",
      color = "#34a853"
    ),
    list(
      field = "nearest_same_group",
      title = "Nearest neighbour in same cluster",
      color = "#a142f4"
    )
  )
  plots <- lapply(definitions, function(definition) {
    values <- table[[definition$field]]
    tooltip <- paste0(
      "Cluster: ",
      table$group,
      "<br>",
      definition$title,
      ": ",
      signif(values, 4L)
    )
    plotly::layout(
      plotly::plot_ly(
        x = table$group,
        y = values,
        customdata = table$group,
        text = tooltip,
        hoverinfo = "text",
        type = "bar",
        textposition = "none",
        marker = list(color = definition$color),
        showlegend = FALSE,
        source = "trekker_spatial_profiles"
      ),
      xaxis = list(title = "", type = "category"),
      yaxis = list(
        title = "",
        rangemode = "tozero",
        tickformat = if (definition$field == "nearest_same_group") {
          ".0%"
        } else {
          NULL
        }
      )
    )
  })
  plot <- do.call(
    plotly::subplot,
    c(
      plots,
      list(nrows = 2L, shareX = FALSE, shareY = FALSE, margin = 0.12)
    )
  )
  plot <- plotly::layout(
    plot,
    annotations = trekker_subplot_titles(
      vapply(definitions, `[[`, character(1), "title"),
      2L
    )
  )
  trekker_plotly(plot, margin = list(l = 55, r = 20, b = 55, t = 35))
})

output[["trekker_spatial_profile_table"]] <- DT::renderDT({
  table <- trekker_spatial_profile_results()
  req(nrow(table))
  names(table) <- c(
    "Cluster",
    "Nuclei",
    "Share",
    "Centroid X",
    "Centroid Y",
    "Hull area",
    "Median radius",
    "Nearest same cluster"
  )
  DT::datatable(
    table,
    rownames = FALSE,
    options = list(pageLength = 25L, scrollX = TRUE)
  )
})

observeEvent(
  plotly::event_data("plotly_click", source = "trekker_spatial_profiles"),
  {
    click <- plotly::event_data(
      "plotly_click",
      source = "trekker_spatial_profiles"
    )
    group <- as.character(click$customdata)
    req(length(group) == 1L, nzchar(group))
    session$sendCustomMessage(
      "cell_view_focus_groups",
      list(id = "trekker_projection", groups = group)
    )
  },
  ignoreInit = TRUE
)

trekker_local_neighbourhood_results <- reactive({
  coordinates <- trekker_analysis_coordinates()
  groups <- trekker_default_groups()$values
  target <- input[["trekker_neighbour_cluster"]]
  if (is.null(groups) || is.null(target)) {
    return(data.frame())
  }
  trekker_local_neighbourhood(
    coordinates,
    as.character(groups[as.character(coordinates$barcode)]),
    target = target,
    k = 6L,
    maximum = 1000L
  )
})

output[["trekker_local_neighbourhood_plot"]] <- plotly::renderPlotly({
  table <- trekker_local_neighbourhood_results()
  req(
    nrow(table),
    all(c("barcode", "group", "target_share", "enrichment") %in% names(table))
  )
  target <- input[["trekker_neighbour_cluster"]]
  table$tooltip <- paste0(
    "Barcode: ",
    table$barcode,
    "<br>Cluster: ",
    table$group,
    "<br>",
    target,
    " neighbour share: ",
    scales::percent(table$target_share, accuracy = 0.1),
    "<br>Local log2 enrichment: ",
    signif(table$enrichment, 4L)
  )
  table <- table[order(table$enrichment, na.last = TRUE), , drop = FALSE]
  plot <- plotly::plot_ly(
    table,
    x = ~x,
    y = ~y,
    key = ~barcode,
    text = ~tooltip,
    hoverinfo = "text",
    type = "scattergl",
    mode = "markers",
    marker = list(
      size = if (nrow(table) > 5000L) 2 else 5,
      color = table$enrichment,
      colorscale = "RdBu",
      reversescale = TRUE,
      colorbar = list(title = "Local log2"),
      opacity = 0.85
    ),
    source = "trekker_local_neighbourhood"
  )
  trekker_plotly(
    plotly::layout(
      plot,
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE)
    ),
    margin = list(l = 10, r = 75, b = 10, t = 10)
  )
})

trekker_knn_overlap_results <- reactive({
  coordinates <- trekker_analysis_coordinates()
  projections <- tryCatch(
    data_set()$availableProjections(),
    error = function(error) character()
  )
  umap_name <- projections[tolower(projections) == "umap"]
  if (!length(umap_name)) {
    return(list(per_nucleus = data.frame(), summary = data.frame()))
  }
  keys <- as.character(coordinates$barcode)
  physical <- cbind(
    x = as.numeric(coordinates$x),
    y = as.numeric(coordinates$y)
  )
  rownames(physical) <- keys
  umap <- data_set()$getProjection(umap_name[[1L]])
  groups <- trekker_default_groups()$values
  if (!is.null(groups)) {
    groups <- groups[keys]
  }
  trekker_knn_overlap(
    physical,
    umap,
    groups = groups,
    k = 6L,
    maximum = 1000L
  )
})

output[["trekker_knn_overlap_plot"]] <- plotly::renderPlotly({
  table <- trekker_knn_overlap_results()$summary
  req(nrow(table))
  table$tooltip <- paste0(
    "Cluster: ",
    table$group,
    "<br>Mean overlap: ",
    scales::percent(table$mean_overlap, accuracy = 0.1),
    "<br>Median overlap: ",
    scales::percent(table$median_overlap, accuracy = 0.1),
    "<br>Nuclei: ",
    table$nuclei
  )
  plot <- plotly::plot_ly(
    table,
    x = ~group,
    y = ~mean_overlap,
    text = ~tooltip,
    hoverinfo = "text",
    type = "bar",
    marker = list(color = "#5b8def"),
    source = "trekker_knn_overlap"
  )
  trekker_plotly(plotly::layout(
    plot,
    xaxis = list(title = "Cluster", type = "category"),
    yaxis = list(
      title = "Mean KNN Jaccard overlap",
      tickformat = ".0%",
      range = c(0, 1)
    )
  ))
})

trekker_section_results <- reactive({
  coordinates <- trekker_slot()$coordinates
  groups <- trekker_default_groups()$values
  if (is.null(groups)) {
    return(list(composition = data.frame(), qc = data.frame()))
  }
  trekker_section_summary(
    coordinates,
    trekker_metadata(),
    as.character(groups[as.character(coordinates$barcode)])
  )
})

output[["trekker_sections_empty"]] <- renderUI({
  sections <- unique(as.character(trekker_slot()$coordinates$section))
  if (length(sections) < 2L) {
    trekker_empty("Section comparison requires at least two sections or FOVs.")
  } else if (!nrow(trekker_section_results()$composition)) {
    trekker_empty(
      "Categorical cluster metadata is required for section comparison."
    )
  } else {
    tags$p(
      class = "tk-note",
      "Moran results are run-level and are not presented as section-specific statistics."
    )
  }
})

output[["trekker_section_composition"]] <- plotly::renderPlotly({
  req(length(unique(trekker_slot()$coordinates$section)) > 1L)
  table <- trekker_section_results()$composition
  req(nrow(table))
  plot <- plotly::plot_ly(
    table,
    x = ~section,
    y = ~share,
    color = ~group,
    type = "bar",
    hovertemplate = "%{x} · cluster %{fullData.name}: %{y:.1%}<extra></extra>",
    source = "trekker_section_composition"
  )
  trekker_plotly(
    plotly::layout(
      plot,
      barmode = "stack",
      xaxis = list(title = "Section / FOV"),
      yaxis = list(title = "Cluster share", tickformat = ".0%"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25)
    ),
    margin = list(l = 55, r = 20, b = 120, t = 10)
  )
})

output[["trekker_section_qc"]] <- plotly::renderPlotly({
  req(length(unique(trekker_slot()$coordinates$section)) > 1L)
  table <- trekker_section_results()$qc
  req(nrow(table))
  fields <- unique(as.character(table$field))
  plots <- lapply(fields, function(field) {
    values <- table[table$field == field, , drop = FALSE]
    values$tooltip <- paste0(
      values$section,
      "<br>Median: ",
      signif(values$value, 4L),
      "<br>Nuclei: ",
      values$nuclei
    )
    plotly::layout(
      plotly::plot_ly(
        values,
        x = ~section,
        y = ~value,
        text = ~tooltip,
        hoverinfo = "text",
        type = "bar",
        textposition = "none",
        marker = list(color = "#5b8def"),
        showlegend = FALSE,
        source = "trekker_section_qc"
      ),
      xaxis = list(title = "", type = "category"),
      yaxis = list(title = "", rangemode = "tozero")
    )
  })
  plot <- do.call(
    plotly::subplot,
    c(
      plots,
      list(nrows = 1L, shareX = FALSE, shareY = FALSE, margin = 0.08)
    )
  )
  plot <- plotly::layout(
    plot,
    annotations = trekker_subplot_titles(
      trekker_metric_label(fields),
      length(fields)
    )
  )
  trekker_plotly(
    plot,
    margin = list(l = 55, r = 20, b = 55, t = 35)
  )
})

trekker_current_crb <- reactive(available_crb_files$selected)
trekker_descriptor_path <- function(descriptor, label) {
  path <- trekker_file_path(
    descriptor,
    crb_path = trekker_current_crb(),
    app_root = Cerebro.options[["cerebro_root"]]
  )
  validate(need(
    !is.null(path),
    paste("Trekker", label, "file is unavailable.")
  ))
  path
}
trekker_download_path <- function(category) {
  descriptor <- trekker_slot()$files[[category]]
  req(!is.null(descriptor))
  trekker_descriptor_path(descriptor, category)
}

trekker_image_descriptors <- reactive({
  images <- trekker_slot()$images %||% list()
  result <- list()
  for (section in names(images)) {
    for (label in names(images[[section]])) {
      result[[length(result) + 1L]] <- images[[section]][[label]]
    }
  }
  result
})

for (category in c(
  "location",
  "positioning",
  "metrics",
  "cluster_markers",
  "moran",
  "report"
)) {
  local({
    current <- category
    output[[paste0("trekker_download_", current)]] <- downloadHandler(
      filename = function() trekker_slot()$files[[current]]$name,
      content = function(file) file.copy(trekker_download_path(current), file)
    )
  })
}

observe({
  descriptors <- trekker_image_descriptors()
  for (index in seq_along(descriptors)) {
    local({
      current <- index
      output[[paste0("trekker_download_image_", current)]] <- downloadHandler(
        filename = function() trekker_image_descriptors()[[current]]$name,
        content = function(file) {
          descriptor <- trekker_image_descriptors()[[current]]
          file.copy(
            trekker_descriptor_path(
              descriptor,
              paste0(descriptor$section, "/", descriptor$label)
            ),
            file
          )
        }
      )
    })
  }
})

output[["trekker_download_all"]] <- downloadHandler(
  filename = function() "trekker-attachments.zip",
  content = function(file) {
    paths <- c(
      vapply(names(trekker_slot()$files), trekker_download_path, character(1)),
      vapply(
        trekker_image_descriptors(),
        function(descriptor) {
          trekker_descriptor_path(
            descriptor,
            paste0(descriptor$section, "/", descriptor$label)
          )
        },
        character(1)
      )
    )
    bundle <- tempfile("trekker-download-")
    dir.create(bundle)
    on.exit(unlink(bundle, recursive = TRUE, force = TRUE), add = TRUE)
    filenames <- paste0(sprintf("%02d-", seq_along(paths)), basename(paths))
    if (!all(file.copy(paths, file.path(bundle, filenames)))) {
      stop("Could not stage Trekker attachments for download.")
    }
    old <- setwd(bundle)
    on.exit(setwd(old), add = TRUE)
    utils::zip(file, files = filenames)
  }
)

output[["trekker_report"]] <- renderUI({
  tk <- trekker_slot()
  if (is.null(tk$files$report)) {
    return(trekker_empty("The official HTML report was not provided."))
  }
  metadata <- tk$report_metadata %||% list()
  div(
    class = "tk-report",
    tags$h4(trekker_first_text(metadata$title, "Official Trekker report")),
    tags$p(
      "The HTML is retained unchanged and is available only as a download attachment."
    ),
    downloadButton("trekker_download_report", "Download official report")
  )
})

output[["trekker_files"]] <- renderUI({
  tk <- trekker_slot()
  rows <- lapply(names(tk$files), function(category) {
    descriptor <- tk$files[[category]]
    tags$tr(
      tags$td(category),
      tags$td(descriptor$name),
      tags$td(trekker_file_size(descriptor$size)),
      tags$td(tags$code(descriptor$sha256)),
      tags$td(downloadLink(paste0("trekker_download_", category), "Download"))
    )
  })
  image_rows <- lapply(seq_along(trekker_image_descriptors()), function(index) {
    descriptor <- trekker_image_descriptors()[[index]]
    tags$tr(
      tags$td(paste0("image · ", descriptor$section)),
      tags$td(
        descriptor$label,
        tags$br(),
        tags$small(descriptor$provenance %||% descriptor$name)
      ),
      tags$td(trekker_file_size(descriptor$size)),
      tags$td(tags$code(descriptor$sha256)),
      tags$td(downloadLink(
        paste0("trekker_download_image_", index),
        "Download"
      ))
    )
  })
  rows <- c(rows, image_rows)
  tagList(
    tags$p(
      "Declared by ",
      tags$b(tk$source$declared_by),
      " · imported ",
      tk$source$imported_at,
      if (!is.null(tk$source$spatial_relation)) {
        paste0(" · Seurat SPATIAL: ", tk$source$spatial_relation)
      }
    ),
    tags$table(
      class = "tk-table",
      tags$thead(tags$tr(
        tags$th("Category"),
        tags$th("Attachment"),
        tags$th("Size"),
        tags$th("SHA-256"),
        tags$th("Download")
      )),
      tags$tbody(rows)
    )
  )
})

output[["trekker_audit_table"]] <- DT::renderDT({
  projections <- tryCatch(
    data_set()$availableProjections(),
    error = function(error) character()
  )
  spatial_name <- projections[tolower(projections) == "spatial"]
  spatial <- if (length(spatial_name)) {
    data_set()$getProjection(spatial_name[[1L]])
  } else {
    NULL
  }
  metadata <- trekker_metadata()
  table <- trekker_coordinate_audit(
    trekker_slot()$coordinates,
    metadata_keys = rownames(metadata %||% data.frame()),
    spatial = spatial
  )
  names(table) <- c("Check", "Status", "Detail")
  DT::datatable(
    table,
    rownames = FALSE,
    options = list(dom = "t", paging = FALSE),
    class = "compact stripe"
  )
})

observeEvent(input[["trekker_viz_info"]], {
  showModal(modalDialog(
    title = "About Trekker data",
    "This page exists only when official companion data was explicitly attached. Location is authoritative; optional positioning evidence, run QC, cluster-marker, Moran, and report outputs are shown only when supplied.",
    easyClose = TRUE,
    footer = NULL
  ))
})
