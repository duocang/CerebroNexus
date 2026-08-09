##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_main_parameters_UI"]] <- renderUI({
  req(data_set())
  ## This output is evaluated even while the Spatial tab is hidden
  ## (suspendWhenHidden = FALSE below). For a data set without spatial data
  ## there is nothing to configure, so bail out early instead of building the
  ## full control set (and, when spatial_images is set, the background-image
  ## picker) on every app start — that extra startup work otherwise competes
  ## with other tabs' first render.
  req(length(availableSpatial()) > 0)
  ## determine which metadata columns to include based on exclude_trivial_metadata
  exclude_trivial <- FALSE
  if (
    exists('Cerebro.options') &&
      !is.null(Cerebro.options[['exclude_trivial_metadata']])
  ) {
    exclude_trivial <- Cerebro.options[['exclude_trivial_metadata']]
  }

  ## build choices based on setting
  if (exclude_trivial == TRUE) {
    ## only include groups from getGroups()
    metadata_cols <- getGroups()
  } else {
    ## include all metadata columns except cell_barcode
    metadata_cols <- colnames(getMetaData())[
      !colnames(getMetaData()) %in% c("cell_barcode")
    ]
  }

  spatial_names <- availableSpatial()
  selected_spatial <- normalize_spatial_panel_selection(
    isolate(input[["spatial_projection_to_display"]]),
    spatial_names
  )
  if (!length(selected_spatial)) {
    selected_spatial <- spatial_names[[1L]]
  }

  tagList(
    selectizeInput(
      "spatial_projection_to_display",
      label = "Spatial data",
      choices = spatial_names,
      selected = selected_spatial,
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        placeholder = "Select one or more spatial data sets"
      )
    ),
    selectInput(
      "spatial_projection_plot_type",
      label = "Plot type",
      choices = c("ImageDimPlot", "ImageFeaturePlot", "Co-expression (RGB)"),
      selected = "ImageDimPlot"
    ),
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'ImageDimPlot'",
      selectInput(
        "spatial_projection_point_color",
        label = "Color cells by",
        choices = metadata_cols
      )
    ),
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'ImageFeaturePlot'",
      selectizeInput(
        "spatial_projection_feature_to_display",
        label = "Feature/Gene",
        choices = NULL,
        multiple = FALSE,
        options = list(
          maxOptions = 1000,
          placeholder = 'Select a gene...',
          create = FALSE,
          loadThrottle = 300
        )
      )
    ),
    ## Co-expression: one gene per RGB channel; each cell's colour blends them so
    ## spatial overlap reads as a mixed hue. Any channel may be left empty.
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'Co-expression (RGB)'",
      selectizeInput(
        "spatial_projection_coexpr_r",
        label = "Red channel gene",
        choices = NULL,
        options = list(
          maxOptions = 1000,
          placeholder = 'Gene for red...',
          create = FALSE,
          loadThrottle = 300
        )
      ),
      selectizeInput(
        "spatial_projection_coexpr_g",
        label = "Green channel gene",
        choices = NULL,
        options = list(
          maxOptions = 1000,
          placeholder = 'Gene for green...',
          create = FALSE,
          loadThrottle = 300
        )
      ),
      selectizeInput(
        "spatial_projection_coexpr_b",
        label = "Blue channel gene",
        choices = NULL,
        options = list(
          maxOptions = 1000,
          placeholder = 'Gene for blue...',
          create = FALSE,
          loadThrottle = 300
        )
      )
    ),
    uiOutput("spatial_projection_background_controls_UI")
  )
})

## Background choices live in their own dynamic output. Keeping them separate
## from the Spatial-data selectize prevents a selection change from destroying
## and recreating the selector that triggered it.
output[["spatial_projection_background_controls_UI"]] <- renderUI({
  spatial_names <- availableSpatial()
  selected_spatial <- normalize_spatial_panel_selection(
    input[["spatial_projection_to_display"]],
    spatial_names
  )
  req(length(selected_spatial) > 0L)
  panel_descriptors <- spatial_panel_descriptors(spatial_names)

  ## Configured external images are data-set-level choices. Embedded images are
  ## resolved per spatial entry so each panel only offers its own image.
  configured_background_images <- character()
  if (
    exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_images"]])
  ) {
    configured_crb_files <- Cerebro.options[["crb_file_to_load"]]
    selected_crb <- if (exists("available_crb_files")) {
      available_crb_files$selected
    } else {
      NULL
    }
    configured_background_images <- configured_spatial_images(
      Cerebro.options,
      configured_crb_files,
      selected_crb,
      names(configured_crb_files)
    )
  }

  background_controls <- lapply(selected_spatial, function(spatial_name) {
    panel <- panel_descriptors[[match(spatial_name, spatial_names)]]
    spatial_data <- tryCatch(
      getSpatialData(spatial_name),
      error = function(error) NULL
    )
    embedded_backgrounds <- spatial_embedded_backgrounds(spatial_data)
    embedded_ids <- names(embedded_backgrounds)
    choices <- character()
    if (length(embedded_backgrounds)) {
      embedded_labels <- vapply(
        embedded_backgrounds,
        `[[`,
        character(1),
        "label"
      )
      choices <- c(choices, stats::setNames(embedded_ids, embedded_labels))
    }
    if (length(configured_background_images)) {
      choices <- c(
        choices,
        setNames(
          configured_background_images,
          basename(configured_background_images)
        )
      )
    }
    raw_background <- isolate(input[[panel$background_id]])
    selected_background <- resolve_spatial_background_mode(
      raw_background,
      "custom",
      configured_background_images,
      embedded_ids
    )
    image_count <- length(choices)
    control <- if (!image_count) {
      tags$span(class = "spatial-background-unavailable", "No image available")
    } else if (image_count == 1L) {
      tags$div(
        class = "spatial-background-single",
        tags$div(
          class = "spatial-background-single-copy",
          tags$span(
            class = "spatial-background-image-icon",
            `aria-hidden` = "true"
          ),
          tags$span(
            class = "spatial-background-single-name",
            names(choices)[[1L]]
          )
        ),
        shinyWidgets::materialSwitch(
          panel$background_id,
          label = NULL,
          value = !identical(selected_background, "No Background"),
          status = "warning",
          inline = TRUE
        )
      )
    } else {
      tags$div(
        class = "spatial-background-multiple",
        selectInput(
          panel$background_id,
          label = NULL,
          choices = c("None" = "No Background", choices),
          selected = selected_background
        )
      )
    }
    tags$div(
      class = "spatial-background-row",
      tags$div(
        class = "spatial-background-row-heading",
        tags$span(class = "spatial-background-row-label", spatial_name),
        tags$span(
          class = "spatial-background-count",
          if (image_count == 1L) "1 image" else paste(image_count, "images")
        )
      ),
      control
    )
  })

  current_mode <- isolate(input[["spatial_projection_background_mode"]]) %||%
    "auto"
  mode_button <- function(mode, label) {
    tags$button(
      type = "button",
      class = paste(
        "btn btn-default spatial-background-mode-option",
        if (identical(mode, current_mode)) "is-active" else ""
      ),
      `data-spatial-background-mode` = mode,
      onclick = paste0(
        "spatialSetBackgroundMode(this, '",
        mode,
        "');"
      ),
      label
    )
  }
  customize <- shinyWidgets::dropdownButton(
    tags$div(class = "spatial-background-customize-list", background_controls),
    inputId = "spatial_projection_background_customize",
    label = "Customize…",
    circle = FALSE,
    inline = TRUE,
    width = "390px",
    margin = "0"
  )
  customize <- shiny::tagAppendAttributes(
    customize,
    class = paste(
      "spatial-background-customize",
      if (identical(current_mode, "custom")) "is-active" else ""
    ),
    `data-spatial-background-mode` = "custom",
    onclick = "spatialSetBackgroundMode(this, 'custom');"
  )

  tags$div(
    class = "spatial-background-controls",
    tags$label(class = "control-label", "Background image"),
    tags$div(
      class = "spatial-background-mode btn-group",
      mode_button("auto", "Auto"),
      mode_button("none", "None"),
      customize
    )
  )
})

serverSideGeneSelector(
  session,
  "spatial_projection_feature_to_display",
  extra_triggers = function() input[["spatial_projection_plot_type"]],
  active = function() length(availableSpatial()) > 0
)

## Co-expression channel gene pickers. Same server-side population + active gate
## as the feature selector, so their later:: callbacks don't leak into other
## tabs' tests when no spatial data is present.
##
## Use lapply, NOT a for loop: serverSideGeneSelector references the input id
## lazily, and a for loop's index variable is a single shared binding — all
## three registrations would capture its final value ("...coexpr_b"), so only
## the blue channel would get a working server-side search. lapply gives each
## iteration its own `channel_id` argument, so each selector binds correctly.
lapply(
  c(
    "spatial_projection_coexpr_r",
    "spatial_projection_coexpr_g",
    "spatial_projection_coexpr_b"
  ),
  function(channel_id) {
    serverSideGeneSelector(
      session,
      channel_id,
      extra_triggers = function() input[["spatial_projection_plot_type"]],
      active = function() length(availableSpatial()) > 0
    )
  }
)

## Render even when tab is hidden so that input values are available for
## programmatic access (e.g. shinytest2) without waiting for tab activation.
outputOptions(
  output,
  "spatial_projection_main_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "spatial_projection_background_controls_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      spatial_projection_main_parameters_info[["text"]],
      title = spatial_projection_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
spatial_projection_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)
