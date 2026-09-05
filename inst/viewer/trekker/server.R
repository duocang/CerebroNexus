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
  if (is.null(metadata)) {
    return(NULL)
  }
  keys <- if ("cell_barcode" %in% names(metadata)) {
    metadata$cell_barcode
  } else {
    rownames(metadata)
  }
  rownames(metadata) <- as.character(keys)
  metadata
})

trekker_first_text <- function(...) {
  values <- as.character(unlist(list(...), use.names = FALSE))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values)) values[[1L]] else NULL
}

output[["trekker_summary"]] <- renderUI({
  tk <- req(trekker_slot())
  labels <- c(
    location = "Location",
    metrics = "Run QC",
    cluster_markers = "Cluster markers",
    moran = "Spatial genes",
    report = "Official report"
  )
  badges <- lapply(names(labels), function(category) {
    present <- category %in% names(tk$files)
    span(
      class = paste("tk-badge", if (!present) "tk-badge-gray"),
      paste(labels[[category]], if (present) "available" else "not provided")
    )
  })
  section_count <- length(unique(tk$coordinates$section))
  image_count <- sum(vapply(tk$images %||% list(), length, integer(1)))
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
  div(
    class = "tk-summary",
    div(class = "tk-meta", badges),
    downloadButton(
      "trekker_download_all",
      "Download official data",
      class = "btn-primary"
    )
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
        if (has_umap) "Physical + UMAP" = "pair"
      )
    ),
    selectInput("trekker_colour", "Colour by", choices = choices),
    if (length(layers)) {
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
    },
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
    categorical <- !is.numeric(values)
    if (categorical) {
      group <- as.character(values)
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
  layers <- tk$images[[section]] %||% list()
  selected_layers <- input[["trekker_layers"]]
  layers <- Filter(function(layer) layer$label %in% selected_layers, layers)
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
  if (identical(input[["trekker_view"]], "pair") && length(umap_name)) {
    umap <- data_set()$getProjection(umap_name[[1L]])
    at <- match(keys, rownames(umap))
    panels[[2L]] <- list(
      id = "umap",
      label = "UMAP",
      selection_key = keys,
      x = as.numeric(umap[at, 1L]),
      y = as.numeric(umap[at, 2L]),
      projection = TRUE
    )
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
  selected <- input[["trekker_projection_persistent_selection"]] %||%
    character()
  tags$span(
    format(cerebroSelectionCount(selected), big.mark = ","),
    " cells selected"
  )
})

output[["trekker_cell_inspector"]] <- renderUI({
  selected <- input[["trekker_projection_persistent_selection"]] %||%
    character()
  if (length(selected) != 1L) {
    return(trekker_empty(
      "Select one nucleus to inspect its barcode, coordinates, and CRB metadata."
    ))
  }
  tagList(tags$h4("Cell inspector"), DT::DTOutput("trekker_cell_table"))
})

output[["trekker_cell_table"]] <- DT::renderDT({
  selected <- input[["trekker_projection_persistent_selection"]] %||%
    character()
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
    setdiff(names(values), "cell_barcode")
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
            values[setdiff(names(values), "cell_barcode")],
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

output[["trekker_metrics_cards"]] <- renderUI({
  metrics <- trekker_slot()$metrics
  if (is.null(metrics) || !nrow(metrics)) {
    return(trekker_empty("Run QC was not provided."))
  }
  div(
    class = "tk-grid",
    lapply(seq_len(min(6L, nrow(metrics))), function(index) {
      div(
        class = "tk-stat",
        div(class = "tk-k", metrics$Metrics[[index]]),
        div(class = "tk-v", as.character(metrics$Value[[index]]))
      )
    })
  )
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
output[["trekker_markers_table"]] <- DT::renderDT({
  table <- trekker_slot()$cluster_markers
  req(!is.null(table))
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
  table <- trekker_slot()$cluster_markers
  req(length(row) == 1L, !is.null(table$gene[[row]]))
  updateSelectInput(session, "trekker_colour", selected = "gene")
  updateSelectizeInput(
    session,
    "trekker_gene",
    selected = as.character(table$gene[[row]])
  )
})

output[["trekker_moran_empty"]] <- renderUI({
  if (is.null(trekker_slot()$moran)) {
    trekker_empty("Spatial gene results were not provided.")
  }
})
output[["trekker_moran_table"]] <- DT::renderDT({
  table <- trekker_slot()$moran
  req(!is.null(table))
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
  table <- trekker_slot()$moran
  req(length(row) == 1L, !is.null(table$gene[[row]]))
  updateSelectInput(session, "trekker_colour", selected = "gene")
  updateSelectizeInput(
    session,
    "trekker_gene",
    selected = as.character(table$gene[[row]])
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

observeEvent(input[["trekker_viz_info"]], {
  showModal(modalDialog(
    title = "About Trekker data",
    "This page exists only when official companion data was explicitly attached. Location is authoritative; optional run QC, cluster-marker, Moran, and report outputs are shown only when supplied.",
    easyClose = TRUE,
    footer = NULL
  ))
})
