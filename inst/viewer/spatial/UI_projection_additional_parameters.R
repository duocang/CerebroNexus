##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
## Scatter controls deliberately live in a renderUI which is independent of the
## selected background image. This preserves users' point-size, opacity and
## sampling choices while moving among backgrounds.
output[["spatial_projection_scatter_parameters_UI"]] <- renderUI({
  appearance <- current_scatter_defaults()
  section_appearance <- spatialPointAppearance(
    Cerebro.options,
    viewerDatasetName(
      available_crb_files$files,
      available_crb_files$selected
    ),
    input[["spatial_projection_to_display"]]
  )
  if (!is.null(section_appearance)) {
    appearance$point_size <- section_appearance$point_size
    appearance$point_opacity <- section_appearance$point_opacity
  }
  roi_appearance <- spatialRoiSetting(
    Cerebro.options,
    viewerDatasetName(
      available_crb_files$files,
      available_crb_files$selected
    ),
    input[["spatial_projection_to_display"]],
    spatial_roi_value(input[["spatial_projection_roi"]] %||% "")
  )
  if (!is.null(roi_appearance)) {
    appearance$point_size <- roi_appearance$point_size
    appearance$point_opacity <- roi_appearance$point_opacity
  }

  tagList(
    sliderInput(
      "spatial_projection_point_size",
      label = "Point size",
      min = preferences[["cell_point_size"]][["min"]],
      max = preferences[["cell_point_size"]][["max"]],
      step = preferences[["cell_point_size"]][["step"]],
      value = appearance$point_size
    ),
    sliderInput(
      "spatial_projection_point_opacity",
      label = "Point opacity",
      min = preferences[["cell_point_opacity"]][["min"]],
      max = preferences[["cell_point_opacity"]][["max"]],
      step = preferences[["cell_point_opacity"]][["step"]],
      value = appearance$point_opacity
    )
  )
})

output[["spatial_projection_data_parameters_UI"]] <- renderUI({
  appearance <- current_scatter_defaults()
  spatial_data <- tryCatch(
    getSpatialData(input[["spatial_projection_to_display"]]),
    error = function(error) list()
  )
  boundaries <- spatial_data[["boundaries"]]
  molecules <- spatial_data[["molecules"]]
  molecule_genes <- if (
    is.list(molecules) &&
      !is.object(molecules) &&
      is.character(molecules[["genes"]])
  ) {
    molecules[["genes"]]
  } else {
    character()
  }
  molecule_scope <- nzchar(input[["spatial_projection_sample"]] %||% "") ||
    spatial_roi_is_specific(input[["spatial_projection_roi"]] %||% "")

  tagList(
    sliderInput(
      "spatial_projection_percentage_cells_to_show",
      label = "Show % of observations",
      min = preferences[["cell_percentage_cells_to_show"]][[
        "min"
      ]],
      max = preferences[["cell_percentage_cells_to_show"]][[
        "max"
      ]],
      step = preferences[["cell_percentage_cells_to_show"]][[
        "step"
      ]],
      value = appearance$percentage_cells_to_show
    ),
    if (identical(class(boundaries), "data.frame") && nrow(boundaries)) {
      checkboxInput(
        "spatial_projection_show_cell_boundaries",
        "Show cell boundaries",
        value = FALSE
      )
    },
    if (length(molecule_genes) && molecule_scope) {
      helpText(
        "Molecules are hidden while Sample or ROI filtering is active because ",
        "the exported molecule records are not assigned to cells or ROIs."
      )
    } else if (length(molecule_genes)) {
      tagList(
        checkboxInput(
          "spatial_projection_show_molecules",
          "Show molecules",
          value = FALSE
        ),
        selectInput(
          "spatial_projection_molecule_gene",
          "Molecule gene",
          choices = molecule_genes,
          selected = molecule_genes[[1L]]
        )
      )
    }
  )
})

## The image-specific controls may safely be regenerated when the selected
## image changes: their initial values come from that image's preset.
output[["spatial_projection_background_parameters_UI"]] <- renderUI({
  if (identical(input[["spatial_projection_roi"]], "__separate__")) {
    return(tags$p(
      class = "help-block",
      "Each ROI uses its saved image alignment in Separate ROIs mode."
    ))
  }
  ## Offset sliders move the background image in DATA units, so their range is
  ## sized to the current dataset's coordinate span (± the larger of x/y span).
  ## That keeps one range usable whether the coordinates run 0–5k (Xenium) or
  ## 0–9k (MERFISH). Falls back to a generous default if coordinates are absent.
  offset_limit <- 5000
  ## Coarse step so each nudge visibly moves the image; a step of 1 was
  ## imperceptible on datasets with a large coordinate span.
  offset_step <- 50
  tryCatch(
    {
      req(
        !is.null(input[["spatial_projection_to_display"]]),
        input[["spatial_projection_to_display"]] %in% availableSpatial()
      )
      sp <- getSpatialData(input[["spatial_projection_to_display"]])
      co <- sp$coordinates
      x <- co[[1]][is.finite(co[[1]])]
      y <- co[[2]][is.finite(co[[2]])]
      if (length(x) > 0 && length(y) > 0) {
        span <- max(diff(range(x)), diff(range(y)))
        if (is.finite(span) && span > 0) {
          offset_limit <- ceiling(span / 100) * 100
          offset_step <- max(50, round(span / 400))
        }
      }
    },
    error = function(e) NULL
  )

  ## Seed appearance from the exact dataset / spatial / image leaf. Because this
  ## renderUI reads the selected source-tagged image key, changing either spatial
  ## entry or image recreates every control with that image's own defaults.
  spatial_name <- input[["spatial_projection_to_display"]]
  dataset <- spatial_dataset_name(
    if (exists("available_crb_files")) available_crb_files$files else NULL,
    if (exists("available_crb_files")) available_crb_files$selected else NULL
  )
  spatial_data <- tryCatch(
    getSpatialData(spatial_name),
    error = function(e) list()
  )
  selected_roi <- input[["spatial_projection_roi"]] %||% ""
  embedded_images <- embedded_spatial_images(spatial_data, selected_roi)
  external_images <- configured_spatial_images(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name,
    selected_roi
  )
  selected_descriptor <- resolve_spatial_background(
    input[["spatial_projection_background_image"]],
    embedded_images,
    external_images
  )
  preset <- spatial_background_preset(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name,
    selected_descriptor
  )
  ## Seed MOVE and FLIP from the preset so the controls honestly reflect the
  ## shipped alignment (checkbox ticked, sliders positioned). Both are read by
  ## the JS as single-source interaction state (dataset.offsetX / dataset.flipY),
  ## so seeding the input is exactly where the preset takes effect — one factor,
  ## no double-application.
  ##
  ## SCALE is now single-source too: the slider(s) are seeded from the preset and
  ## are the only scale the JS applies (no separate dataset factor). The X and Y
  ## scale can differ; a "Lock aspect ratio" checkbox decides whether the user
  ## sees one slider (locked, X drives both) or two (unlocked). Initial lock state
  ## follows the preset: equal x/y -> locked single slider; unequal -> unlocked
  ## with both shown.
  offset_x_default <- preset$offsetX
  offset_y_default <- preset$offsetY
  flip_x_default <- preset$flipX
  flip_y_default <- preset$flipY
  scale_x_default <- preset$scaleX
  scale_y_default <- preset$scaleY
  rotation_default <- preset$rotation
  aspect_locked_default <- isTRUE(
    all.equal(scale_x_default, scale_y_default) == TRUE
  )
  slider_number_input <- function(
    id,
    label,
    min,
    max,
    value,
    step,
    full = FALSE
  ) {
    tags$div(
      class = paste(
        c(if (full) "cerebro-settings-full", "spatial-image-offset-field"),
        collapse = " "
      ),
      tags$label(`for` = id, class = "control-label", label),
      tags$div(
        class = "spatial-image-offset-row",
        tags$div(
          class = "spatial-image-offset-slider",
          sliderInput(
            id,
            label = NULL,
            min = min,
            max = max,
            value = value,
            step = step
          )
        ),
        tags$div(
          class = "spatial-image-offset-number",
          numericInput(
            paste0(id, "_num"),
            label = NULL,
            value = value,
            min = min,
            max = max,
            step = step,
            width = "100%"
          )
        )
      )
    )
  }

  tagList(
    ## Background-image adjustments. Shown only when an image is selected. Every
    ## control here is DECOUPLED from the scatter plot: it re-styles the image
    ## <div> via the independent JS channel and never re-renders the points.
    conditionalPanel(
      condition = paste0(
        "input.spatial_projection_background_image && ",
        "input.spatial_projection_background_image !== 'none'"
      ),
      slider_number_input(
        "spatial_projection_background_opacity",
        "Opacity",
        0,
        1,
        preset$opacity,
        0.05,
        full = TRUE
      ),
      ## The slider remains authoritative; the number box mirrors it for exact
      ## keyboard entry. These stay in the existing Background image card rather
      ## than introducing Position / Transform cards inside it.
      slider_number_input(
        "spatial_projection_background_offset_x",
        "Horizontal",
        -offset_limit,
        offset_limit,
        offset_x_default,
        offset_step,
        full = TRUE
      ),
      slider_number_input(
        "spatial_projection_background_offset_y",
        "Vertical",
        -offset_limit,
        offset_limit,
        offset_y_default,
        offset_step,
        full = TRUE
      ),
      tags$div(
        class = "cerebro-settings-full",
        conditionalPanel(
          condition = "input.spatial_projection_background_scale_lock",
          slider_number_input(
            "spatial_projection_background_scale",
            "Scale",
            0.2,
            3,
            scale_x_default,
            0.05
          )
        )
      ),
      tags$div(
        class = "cerebro-settings-full",
        conditionalPanel(
          condition = "!input.spatial_projection_background_scale_lock",
          slider_number_input(
            "spatial_projection_background_scale_x",
            "Scale X",
            0.2,
            3,
            scale_x_default,
            0.05
          )
        )
      ),
      tags$div(
        class = "cerebro-settings-full",
        conditionalPanel(
          condition = "!input.spatial_projection_background_scale_lock",
          slider_number_input(
            "spatial_projection_background_scale_y",
            "Scale Y",
            0.2,
            3,
            scale_y_default,
            0.05
          )
        )
      ),
      slider_number_input(
        "spatial_projection_background_rotate",
        "Rotation",
        -180,
        180,
        rotation_default,
        1,
        full = TRUE
      ),
      tags$div(
        class = "cerebro-settings-full",
        tags$div(
          class = "cerebro-settings-content spatial-image-checks",
          checkboxInput(
            "spatial_projection_background_scale_lock",
            label = "Lock aspect ratio",
            value = aspect_locked_default
          ),
          checkboxInput(
            "spatial_projection_background_flip_x",
            label = "Flip horizontally",
            value = flip_x_default
          ),
          checkboxInput(
            "spatial_projection_background_flip_y",
            label = "Flip vertically",
            value = flip_y_default
          )
        )
      ),
      tags$div(
        class = "spatial-image-actions",
        actionButton(
          "spatial_projection_background_reset",
          label = "Reset image",
          icon = icon("undo"),
          width = "100%"
        ),
        ## Turn the hand-tuned alignment into pasteable Cerebro.options code.
        actionButton(
          "spatial_projection_background_copy_preset",
          label = "Copy alignment as preset",
          icon = icon("clipboard"),
          width = "100%"
        )
      ),
      conditionalPanel(
        condition = "output.spatial_projection_background_preset_code_present",
        tags$pre(
          style = paste(
            "margin-top: 8px; padding: 8px; font-size: 11px;",
            "white-space: pre-wrap; word-break: break-word;",
            "background: #f6f8fa; border: 1px solid #d9d9d9;",
            "border-radius: 4px;"
          ),
          verbatimTextOutput(
            "spatial_projection_background_preset_code"
          )
        )
      )
    )
  )
})


## Keep controls available while the settings drawer is hidden.
outputOptions(
  output,
  "spatial_projection_scatter_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "spatial_projection_data_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "spatial_projection_background_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      spatial_projection_additional_parameters_info[["text"]],
      title = spatial_projection_additional_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
spatial_projection_additional_parameters_info <- list(
  title = "Additional parameters for projection",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Point size:</b> Controls how large the observations should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the observations.</li>
      <li><b>Show % of observations:</b> Randomly subsamples cells or spots after the active Sample, FOV / section, ROI, and group filters.</li>
    </ul>
    "
  )
)
