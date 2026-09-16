##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_main_parameters_UI"]] <- renderUI({
  req(
    identical(input[["sidebar"]], "spatial") ||
      inherits(session, "MockShinySession")
  )
  req(data_set())
  ## This output is evaluated even while the Spatial tab is hidden
  ## (suspendWhenHidden = FALSE below). For a data set without spatial data
  ## there is nothing to configure, so bail out early instead of building the
  ## full control set (and, when spatial_images is set, the background-image
  ## picker) on every app start — that extra startup work otherwise competes
  ## with other tabs' first render.
  req(length(availableSpatial()) > 0)
  colour_groups <- viewerColourGroupChoices()
  metadata_cols <- colour_groups$choices

  spatial_names <- availableSpatial()
  spatial_records <- stats::setNames(
    lapply(spatial_names, function(name) {
      tryCatch(getSpatialData(name), error = function(error) list())
    }),
    spatial_names
  )
  metadata <- getMetaData()
  sample_facets <- lapply(spatial_records, function(record) {
    spatial_metadata_facet(
      metadata,
      rownames(record[["coordinates"]]) %||% character(),
      c("sample", "sample_id", "orig.ident")
    )
  })
  sample_values <- unique(unlist(
    lapply(sample_facets, `[[`, "values"),
    use.names = FALSE
  ))
  sample_fields <- unique(unlist(
    lapply(sample_facets, `[[`, "field"),
    use.names = FALSE
  ))
  metadata_cols <- setdiff(
    metadata_cols,
    unique(c(sample_fields, "sample", "sample_id", "orig.ident"))
  )
  default_sample <- if (length(sample_values)) sample_values[[1L]] else ""
  selected_sample <- input[["spatial_projection_sample"]] %||% default_sample
  if (!selected_sample %in% sample_values) {
    selected_sample <- default_sample
  }
  if (nzchar(selected_sample)) {
    spatial_names <- spatial_names[vapply(
      sample_facets[spatial_names],
      function(facet) selected_sample %in% facet$values,
      logical(1)
    )]
  }
  current_spatial <- input[["spatial_projection_to_display"]]
  if (is.null(current_spatial) || !(current_spatial %in% spatial_names)) {
    current_spatial <- spatial_names[[1L]]
  }
  current_sd <- spatial_records[[current_spatial]]
  spatial_choices <- spatial_scene_choices(
    spatial_names,
    spatial_records[spatial_names],
    metadata
  )
  current_cells <- rownames(current_sd[["coordinates"]]) %||% character()
  if (nzchar(selected_sample)) {
    current_sample <- spatial_metadata_facet(
      metadata,
      current_cells,
      c("sample", "sample_id", "orig.ident")
    )
    current_cells <- names(current_sample$by_cell)[
      !is.na(current_sample$by_cell) &
        current_sample$by_cell == selected_sample
    ]
  }
  roi_values <- spatial_metadata_facet(
    metadata,
    current_cells,
    c("sample_roi", "roi", "roi_id", "region_of_interest")
  )$values
  selected_roi <- input[["spatial_projection_roi"]] %||%
    spatial_default_roi_selection(roi_values)
  roi_modes <- c("__all__", if (length(roi_values) > 1L) "__separate__")
  if (!selected_roi %in% c(roi_modes, roi_values)) {
    selected_roi <- spatial_default_roi_selection(roi_values)
  }
  split_columns <- intersect(
    metadata_cols,
    spatial_split_columns(metadata, current_cells, getGroups())
  )
  spatial_split_none_value <- "__none__"
  selected_split <- isolate(
    input[["spatial_projection_split_by"]]
  ) %||%
    spatial_split_none_value
  if (!selected_split %in% c(spatial_split_none_value, split_columns)) {
    selected_split <- spatial_split_none_value
  }
  tagList(
    if (length(sample_values) > 1L) {
      selectInput(
        "spatial_projection_sample",
        label = "Sample",
        choices = stats::setNames(sample_values, sample_values),
        selected = selected_sample
      )
    },
    selectInput(
      "spatial_projection_to_display",
      label = "FOV / section",
      choices = spatial_choices,
      selected = current_spatial
    ),
    if (length(roi_values)) {
      selectizeInput(
        "spatial_projection_roi",
        label = "ROI",
        choices = c(
          "All ROIs" = "__all__",
          if (length(roi_values) > 1L) {
            c("Separate ROIs" = "__separate__")
          },
          stats::setNames(roi_values, roi_values)
        ),
        selected = selected_roi
      )
    },
    selectInput(
      "spatial_projection_plot_type",
      label = "Plot type",
      choices = c("ImageDimPlot", "ImageFeaturePlot", "Co-expression (RGB)"),
      selected = "ImageDimPlot"
    ),
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'ImageDimPlot'",
      div(
        class = "spatial-image-dim-controls",
        selectInput(
          "spatial_projection_point_color",
          label = "Colour by",
          choices = colour_groups$choices,
          selected = colour_groups$selected
        ),
        selectizeInput(
          "spatial_projection_split_by",
          label = "Split by",
          choices = c("None" = spatial_split_none_value, split_columns),
          selected = selected_split
        )
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
      div(
        class = "spatial-rgb-controls",
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
      )
    )
  )
})

output[["spatial_projection_background_selector_UI"]] <- renderUI({
  req(
    identical(input[["sidebar"]], "spatial") ||
      inherits(session, "MockShinySession")
  )
  req(data_set())
  req(length(availableSpatial()) > 0)

  current_spatial <- input[["spatial_projection_to_display"]]
  if (
    is.null(current_spatial) ||
      !(current_spatial %in% availableSpatial())
  ) {
    current_spatial <- availableSpatial()[1]
  }
  current_sd <- tryCatch(
    getSpatialData(current_spatial),
    error = function(e) NULL
  )
  current_cells <- rownames(current_sd[["coordinates"]]) %||% character()
  selected_sample <- input[["spatial_projection_sample"]] %||% ""
  if (nzchar(selected_sample)) {
    current_sample <- spatial_metadata_facet(
      getMetaData(),
      current_cells,
      c("sample", "sample_id", "orig.ident")
    )
    current_cells <- names(current_sample$by_cell)[
      !is.na(current_sample$by_cell) &
        current_sample$by_cell == selected_sample
    ]
  }
  roi_values <- spatial_metadata_facet(
    getMetaData(),
    current_cells,
    c("sample_roi", "roi", "roi_id", "region_of_interest")
  )$values
  selected_roi <- input[["spatial_projection_roi"]] %||%
    spatial_default_roi_selection(roi_values)
  roi_modes <- c("__all__", if (length(roi_values) > 1L) "__separate__")
  if (!selected_roi %in% c(roi_modes, roi_values)) {
    selected_roi <- spatial_default_roi_selection(roi_values)
  }
  embedded_images <- if (is.null(current_sd)) {
    list()
  } else {
    embedded_spatial_images(current_sd, selected_roi)
  }
  dataset <- spatial_dataset_name(
    if (exists("available_crb_files")) available_crb_files$files else NULL,
    if (exists("available_crb_files")) available_crb_files$selected else NULL
  )
  background_choices <- spatial_background_choices(
    embedded_images,
    configured_spatial_images(
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      current_spatial,
      selected_roi
    )
  )
  roi_background_groups <- if (identical(selected_roi, "__separate__")) {
    spatial_roi_background_groups(
      current_sd,
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      current_spatial,
      roi_values
    )
  } else {
    list()
  }
  background_control <- if (
    identical(selected_roi, "__all__") ||
      (!length(roi_background_groups) && length(background_choices) <= 1L)
  ) {
    NULL
  } else if (length(roi_background_groups)) {
    input_id <- "spatial_projection_roi_background_images"
    selected_input <- isolate(input[[input_id]])
    if (is.null(selected_input)) {
      selected_input <- unname(vapply(
        roi_background_groups,
        function(group) {
          choice <- normalize_spatial_background_choice(NULL, group$choices)
          group$tokens[[choice]]
        },
        character(1)
      ))
    }
    selected_tokens <- intersect(
      unlist(lapply(roi_background_groups, `[[`, "tokens"), use.names = FALSE),
      selected_input %||% character()
    )
    grouped_choices <- lapply(roi_background_groups, function(group) {
      stats::setNames(unname(group$tokens), names(group$choices))
    })
    div(
      class = "spatial-roi-background-picker",
      selectizeInput(
        input_id,
        label = "Background images",
        choices = grouped_choices,
        selected = selected_tokens,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          closeAfterSelect = FALSE,
          onItemAdd = I(paste0(
            "function(value) {",
            "var group = value.replace(/-background-.*/, '');",
            "this.items.slice().forEach(function(item) {",
            "if (item !== value && item.replace(/-background-.*/, '') === group) ",
            "this.removeItem(item, true);",
            "}, this);",
            "this.refreshItems();",
            "}"
          ))
        )
      ),
      role = "group"
    )
  } else {
    selectInput(
      "spatial_projection_background_image",
      label = "Background image",
      choices = background_choices,
      selected = normalize_spatial_background_choice(
        isolate(input[["spatial_projection_background_image"]]),
        background_choices
      )
    )
  }
  background_control
})

serverSideGeneSelector(
  session,
  "spatial_projection_feature_to_display",
  extra_triggers = function() input[["spatial_projection_plot_type"]],
  active = function() {
    identical(input[["sidebar"]], "spatial") &&
      length(availableSpatial()) > 0
  }
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
      active = function() {
        identical(input[["sidebar"]], "spatial") &&
          length(availableSpatial()) > 0
      }
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
  "spatial_projection_background_selector_UI",
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
      <li><b>Sample:</b> Limit the available spatial scenes to one biological sample.</li>
      <li><b>FOV / section:</b> Select the spatial coordinate system to display.</li>
      <li><b>ROI:</b> Limit cells or spots within the selected spatial scene.</li>
      <li><b>Colour by:</b> Select one of the data set's registered grouping variables.</li>
    </ul>
    "
  )
)
