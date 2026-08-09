##----------------------------------------------------------------------------##
## Multi-panel Spatial runtime.
##
## One reactive builds every visible panel payload. Outputs are registered once
## per available spatial entry, while the multi-select only decides which stable
## cards are present in the grid. This avoids accumulating observers when users
## repeatedly add and remove tissues.
##----------------------------------------------------------------------------##

spatial_projection_selected_panels <- reactive({
  spatial_names <- availableSpatial()
  selected <- normalize_spatial_panel_selection(
    input[["spatial_projection_to_display"]],
    spatial_names
  )
  descriptors <- spatial_panel_descriptors(spatial_names)
  descriptors[match(selected, spatial_names)]
})

spatial_projection_active_panel_key <- reactiveVal(NULL)

output[["spatial_projection_grid_UI"]] <- renderUI({
  panels <- spatial_projection_selected_panels()
  if (!length(panels)) {
    return(tags$div(
      class = "spatial-projection-empty",
      "Select one or more spatial data sets to display."
    ))
  }

  tags$div(
    class = "spatial-projection-grid",
    lapply(panels, function(panel) {
      tags$div(
        class = "spatial-projection-item",
        `data-spatial-panel` = panel$key,
        shiny::tagAppendAttributes(
          cerebroBox(
            title = boxTitle(panel$name),
            tagList(
              conditionalPanel(
                condition = paste0(
                  "input.spatial_projection_plot_type === ",
                  "'ImageFeaturePlot'"
                ),
                tags$div(
                  class = "spatial-panel-morans",
                  tags$strong("Moran's I:"),
                  textOutput(panel$morans_id, inline = TRUE)
                )
              ),
              plotly::plotlyOutput(
                panel$plot_id,
                width = "100%",
                height = "420px"
              ),
              tags$div(
                class = "spatial-panel-footer",
                htmlOutput(panel$count_id),
                conditionalPanel(
                  condition = paste0(
                    "input['",
                    panel$plot_id,
                    "_persistent_selection'] != null"
                  ),
                  tags$div(
                    class = "cerebro-selection-actions",
                    actionButton(
                      panel$zoom_id,
                      "Zoom to selection",
                      icon = icon("magnifying-glass-plus"),
                      class = "btn-xs btn-default"
                    ),
                    actionButton(
                      panel$clear_id,
                      "Clear selection",
                      icon = icon("eraser"),
                      class = "btn-xs btn-default"
                    )
                  )
                )
              )
            )
          ),
          class = "cerebro-projection-gate"
        )
      )
    })
  )
})

## Register the empty Plotly shells and light card-local outputs. Reassigning an
## existing output id on a data-set change replaces it; selection changes do not
## enter this observer and therefore cannot grow the output registry.
observe({
  panels <- spatial_panel_descriptors(availableSpatial())
  lapply(panels, function(panel) {
    local({
      current <- panel
      output[[current$plot_id]] <- plotly::renderPlotly({
        plotly::plot_ly(
          type = "scattergl",
          mode = "markers",
          source = current$plot_id
        ) %>%
          plotly::layout(
            xaxis = list(
              autorange = TRUE,
              mirror = TRUE,
              showline = TRUE,
              zeroline = FALSE
            ),
            yaxis = list(
              autorange = TRUE,
              mirror = TRUE,
              showline = TRUE,
              zeroline = FALSE
            ),
            uirevision = current$key
          )
      })
      output[[current$count_id]] <- renderText({
        selection <- input[[paste0(
          current$plot_id,
          "_persistent_selection"
        )]]
        count <- if (
          is.null(selection) ||
            is.null(selection[["x"]])
        ) {
          0L
        } else {
          length(selection[["x"]])
        }
        paste0(
          "<b>Number of selected cells</b>: ",
          formatC(count, format = "f", big.mark = ",", digits = 0)
        )
      })
      output[[current$morans_id]] <- renderText({
        req(
          identical(
            input[["spatial_projection_plot_type"]],
            "ImageFeaturePlot"
          )
        )
        gene <- input[["spatial_projection_feature_to_display"]]
        req(gene, gene %in% getGeneNames())
        coords <- getSpatialData(current$name)$coordinates
        metadata <- getMetaData()
        cells <- if ("cell_barcode" %in% colnames(metadata)) {
          as.character(metadata$cell_barcode)
        } else {
          rownames(metadata)
        }
        common <- intersect(cells, rownames(coords))
        req(length(common) >= 2L)
        expression <- data_set()$getExpressionMatrix(
          cells = common,
          genes = gene
        )
        req(!is.null(expression), gene %in% rownames(expression))
        values <- as.vector(expression[gene, common])
        coords <- coords[common, , drop = FALSE]
        index <- seq_along(common)
        if (length(index) > 2000L) {
          set.seed(42)
          index <- sort(sample(index, 2000L))
        }
        score <- morans_i(
          coords[[1]][index],
          coords[[2]][index],
          values[index],
          k = 6
        )
        if (is.na(score)) {
          "not enough cells"
        } else {
          paste0(
            formatC(score, format = "f", digits = 3),
            if (length(common) > 2000L) " (2,000-cell subsample)" else ""
          )
        }
      })
      outputOptions(output, current$plot_id, suspendWhenHidden = FALSE)
    })
  })
})

spatial_projection_panel_payloads_raw <- reactive({
  panels <- spatial_projection_selected_panels()
  if (!length(panels)) {
    return(list())
  }
  req(
    spatial_projection_metadata(),
    reactive_colors(),
    spatial_projection_hover_info()
  )
  metadata <- spatial_projection_metadata()
  hover_info <- spatial_projection_hover_info()
  req(
    nrow(metadata) == length(hover_info) ||
      identical(hover_info, "none")
  )

  payloads <- lapply(panels, function(panel) {
    parameters <- spatial_projection_parameters_for(
      panel$name,
      panel$background_id
    )
    coordinates <- spatial_projection_coordinates_for(panel$name)
    list(
      panel = panel,
      data = build_spatial_projection_data(
        metadata = metadata,
        coordinates = coordinates,
        plot_parameters = parameters,
        hover_info = hover_info,
        reset_axes = isolate(
          spatial_projection_parameters_other[["reset_axes"]]
        )
      )
    )
  })
  names(payloads) <- vapply(panels, `[[`, character(1), "key")
  payloads
})

spatial_projection_panel_payloads <- debounce(
  spatial_projection_panel_payloads_raw,
  150
)

## Track the panel whose persistent selection changed most recently. The single
## observer depends on every visible panel input, so adding/removing cards does
## not register another observer.
spatial_selection_fingerprints <- new.env(parent = emptyenv())
observe({
  panels <- spatial_projection_selected_panels()
  if (!length(panels)) {
    spatial_projection_active_panel_key(NULL)
    return()
  }
  visible_keys <- vapply(panels, `[[`, character(1), "key")
  active_key <- spatial_projection_active_panel_key()
  if (is.null(active_key) || !(active_key %in% visible_keys)) {
    spatial_projection_active_panel_key(visible_keys[[1L]])
  }
  lapply(panels, function(panel) {
    selection <- input[[paste0(
      panel$plot_id,
      "_persistent_selection"
    )]]
    fingerprint <- paste(
      selection[["x"]] %||% numeric(),
      selection[["y"]] %||% numeric(),
      collapse = "|"
    )
    previous <- spatial_selection_fingerprints[[panel$key]]
    spatial_selection_fingerprints[[panel$key]] <- fingerprint
    if (!is.null(previous) && !identical(previous, fingerprint)) {
      spatial_projection_active_panel_key(panel$key)
    }
  })
})

spatial_projection_active_payload <- reactive({
  payloads <- spatial_projection_panel_payloads()
  req(length(payloads) > 0L)
  key <- spatial_projection_active_panel_key()
  if (is.null(key) || !(key %in% names(payloads))) {
    key <- names(payloads)[[1L]]
  }
  payloads[[key]]
})

## Compatibility alias for the selected-cell detail panels. They continue to
## consume one payload, now explicitly the most recently interacted card.
spatial_projection_data_to_plot <- reactive({
  spatial_projection_active_payload()$data
})

## One observer handles every card's action buttons without per-card observers.
spatial_action_counts <- new.env(parent = emptyenv())
observe({
  panels <- spatial_projection_selected_panels()
  lapply(panels, function(panel) {
    zoom_count <- input[[panel$zoom_id]] %||% 0L
    clear_count <- input[[panel$clear_id]] %||% 0L
    zoomed <- isTRUE(input[[paste0(panel$plot_id, "_zoom_state")]])
    old_zoom <- spatial_action_counts[[panel$zoom_id]] %||% 0L
    old_clear <- spatial_action_counts[[panel$clear_id]] %||% 0L
    if (zoom_count > old_zoom) {
      shinyjs::js$spatialZoomToSelection(panel$plot_id)
      spatial_projection_active_panel_key(panel$key)
    }
    if (clear_count > old_clear) {
      shinyjs::js$spatialClearSelection(panel$plot_id)
      spatial_projection_active_panel_key(panel$key)
    }
    shinyjs::toggleClass(
      id = panel$zoom_id,
      class = "is-zoomed",
      condition = zoomed
    )
    updateActionButton(
      session,
      panel$zoom_id,
      label = if (zoomed) "Reset zoom" else "Zoom to selection",
      icon = if (zoomed) {
        icon("magnifying-glass-minus")
      } else {
        icon("magnifying-glass-plus")
      }
    )
    spatial_action_counts[[panel$zoom_id]] <- zoom_count
    spatial_action_counts[[panel$clear_id]] <- clear_count
  })
})
