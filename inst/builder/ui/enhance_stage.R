## Guided Enhance stage.

builder_enhance_model <- function(id, profile, state, settings, modules) {
  manifest <- state$manifest %||% list()
  retained <- Filter(
    function(entry) {
      identical(entry$status, "valid") &&
        !identical(entry$disposition, "rejected")
    },
    unname(manifest)
  )
  auto_retained <- lapply(retained, function(entry) {
    list(
      id = entry$id,
      label = entry$summary %||% entry$id,
      enabled_pages = entry$pages %||% character(),
      cost = "No additional build cost.",
      network = "No network access.",
      prerequisite = "Already validated by the frozen content manifest.",
      replacement_policy = paste0(
        "Existing validated content is ",
        entry$disposition %||% "retained",
        "."
      ),
      skip_consequence = "Not optional: frozen valid content stays in the CRB."
    )
  })
  extras <- profile$extras %||% list()
  has_trekker <- any(vapply(
    extras,
    function(entry) {
      identical(entry$key %||% "", "trekker") && isTRUE(entry$found)
    },
    logical(1)
  ))
  spatial_sections <- unique(c(
    profile$images %||% character(),
    if (has_trekker) "trekker" else character()
  ))
  list(
    id = id,
    modules = modules,
    attachments = list(
      tables = list(
        label = "Tables for Extra material",
        enabled_pages = "extra material",
        relevant = TRUE,
        cost = "Reads and validates one bounded delimited file.",
        network = "No network access.",
        prerequisite = "Requires a readable CSV or TSV file.",
        selected = names(settings$tables %||% list()) %||% character(),
        replacement_policy = paste(
          "Replace a table only when its display name matches;",
          "otherwise append it."
        ),
        skip_consequence = paste(
          "Skipped tables will not appear in Extra material."
        )
      ),
      histology = list(
        label = "Spatial alignment",
        enabled_pages = "spatial",
        relevant = length(spatial_sections) > 0L ||
          any(vapply(
            manifest,
            function(entry) {
              (identical(entry$id, "spatial") ||
                "spatial" %in% (entry$pages %||% character())) &&
                !identical(entry$status, "not_applicable")
            },
            logical(1)
          )),
        cost = "Image decoding, encoding, and alignment.",
        network = "No network access.",
        prerequisite = "Requires spatial sections and coordinates.",
        sections = spatial_sections,
        selected = names(settings$images %||% list()) %||% character(),
        replacement_policy = "One saved image per tissue section.",
        skip_consequence = paste(
          "Sections without an image keep points-only spatial views."
        )
      )
    ),
    auto_retained = auto_retained
  )
}

builder_enhance_retain <- function(settings, kind, selected) {
  if (!is.list(settings) || !kind %in% c("tables", "images")) {
    stop("Enhance retention settings are invalid.", call. = FALSE)
  }
  selected <- as.character(selected %||% character())
  available <- settings[[kind]] %||% list()
  settings[[kind]] <- available[intersect(names(available), selected)]
  settings
}

builder_enhance_analysis_profile <- function(profile, organism) {
  profile$organism_guess <- organism
  profile
}

builder_enhance_analysis_applicability <- function(
  step,
  organism,
  blocked_reason = NULL
) {
  organism <- organism %||% ""
  intrinsic_not_applicable <- identical(step$id, "percent_mt_ribo") &&
    !organism %in% c("hg", "mm")
  list(
    relevant = !intrinsic_not_applicable,
    blocked = !intrinsic_not_applicable &&
      builder_stage_has_text(blocked_reason %||% ""),
    blocked_reason = if (intrinsic_not_applicable) NULL else blocked_reason
  )
}

builder_enhance_modules <- function(profile, settings) {
  steps <- builder_analysis_steps()
  analysis_profile <- builder_enhance_analysis_profile(
    profile,
    settings$organism
  )
  lapply(steps, function(step) {
    blocked <- try(
      builder_step_blocked(step, analysis_profile, settings$analyses),
      silent = TRUE
    )
    blocked_reason <- if (inherits(blocked, "try-error")) {
      "Prerequisites could not be evaluated."
    } else {
      blocked
    }
    applicability <- builder_enhance_analysis_applicability(
      step,
      settings$organism,
      blocked_reason
    )
    enabled_pages <- switch(
      step$id,
      percent_mt_ribo = "overview",
      most_expressed = "most expressed genes",
      marker_genes = "marker genes",
      enriched_pathways = "enriched pathways",
      character()
    )
    list(
      id = step$id,
      label = step$label,
      relevant = applicability$relevant,
      blocked = applicability$blocked,
      blocked_reason = applicability$blocked_reason,
      selected = step$id %in% settings$analyses,
      enabled_pages = enabled_pages,
      replacement_policy = paste0(
        "A newly computed ",
        step$label,
        " result replaces the same existing result."
      ),
      skip_consequence = paste0(
        "No new ",
        step$label,
        " result or matching page is added."
      ),
      consequence = step$note,
      cost = step$cost,
      network = if (isTRUE(step$network)) {
        "Network access is required."
      } else {
        "No network access."
      },
      prerequisite = if (is.null(step$needs)) {
        "No analysis dependency."
      } else {
        paste("Requires", step$needs, "first.")
      }
    )
  })
}

builder_enhance_modules_ui <- function(id, modules) {
  ns <- NS(id)
  modules <- Filter(function(module) isTRUE(module$relevant), modules)
  if (!length(modules)) {
    return(p(class = "hint", "No optional modules apply to this dataset."))
  }
  tagList(lapply(modules, function(module) {
    div(
      class = if (isTRUE(module$blocked)) {
        "enhance-module is-blocked"
      } else {
        "enhance-module"
      },
      tags$label(
        class = "enhance-module-select",
        tags$input(
          id = ns(paste0("analysis_", module$id)),
          type = "checkbox",
          class = "enhance-module-checkbox visually-hidden shiny-input-checkbox",
          checked = if (isTRUE(module$selected)) "checked",
          disabled = if (isTRUE(module$blocked)) "disabled"
        ),
        tags$span(class = "enhance-module-title", module$label),
        p(class = "consequence", module$consequence %||% ""),
        if (isTRUE(module$blocked)) {
          p(class = "blocked", module$blocked_reason %||% "Unavailable")
        }
      ),
      tags$button(
        type = "button",
        class = "enhance-info-button",
        `aria-label` = paste("More information about", module$label),
        `data-title` = module$label %||% "",
        `data-description` = module$consequence %||% "",
        `data-pages` = module$enabled_pages %||% "",
        `data-cost` = module$cost %||% "",
        `data-network` = module$network %||% "",
        `data-prerequisite` = module$prerequisite %||% "",
        `data-replacement` = module$replacement_policy %||% "",
        `data-skip` = module$skip_consequence %||% "",
        "i"
      )
    )
  }))
}

builder_tissue_image_file_ui <- function(id, record) {
  ns <- NS(id)
  source <- record$source %||% list()
  filename <- builder_safe_file_name(source$name, "Tissue image")
  detail <- paste(
    builder_file_type_label(filename, source$type),
    builder_file_human_size(source$size %||% NA_real_),
    sep = " · "
  )
  saved <- isTRUE(record$saved)
  div(
    class = "builder-file-list builder-file-list--single",
    div(
      class = "builder-file-item enhance-tissue-file-item",
      div(
        class = "enhance-tissue-file-meta",
        strong(filename),
        span(class = "hint", detail)
      ),
      div(
        class = "builder-action-row",
        span(
          class = paste(
            "builder-status",
            if (saved) "builder-status--ready" else "builder-status--attention"
          ),
          if (saved) "Ready" else "Needs saving"
        ),
        actionButton(
          ns("drop_image"),
          "Remove",
          class = "btn btn-remove-soft"
        )
      )
    )
  )
}

builder_alignment_plot_output <- function(id, label) {
  div(
    class = "spatial-alignment-plot-output",
    `aria-label` = label,
    role = "img",
    plotly::plotlyOutput(id, height = "360px")
  )
}

builder_spatial_alignment_ui <- function(id, model) {
  ns <- NS(id)
  sections <- as.character(model$sections %||% character())
  section_labels <- sections
  section_labels[sections == "trekker"] <- "Trekker physical space"
  choices <- stats::setNames(sections, section_labels)
  tagList(
    h4(model$label %||% "Spatial alignment"),
    p(
      class = "enhance-attachment-description",
      "Compare transcriptome and physical space, then align an optional tissue image."
    ),
    if (length(sections)) {
      div(
        class = "spatial-alignment-layout",
        div(
          class = "spatial-alignment-sidebar builder-controls-grid",
          selectInput(
            ns("active_section"),
            "Tissue section",
            choices = choices,
            selected = sections[[1L]]
          ),
          div(
            class = "builder-status spatial-alignment-status",
            `aria-live` = "polite",
            uiOutput(ns("alignment_status"))
          ),
          div(
            class = "enhance-tissue-file-control builder-file-picker builder-file-picker--compact",
            tags$input(
              id = ns("tissue_image_file"),
              name = ns("tissue_image_file"),
              class = "shiny-input-file enhance-tissue-file-input builder-file-input",
              type = "file",
              accept = ".png,.jpg,.jpeg",
              `tabindex` = "-1"
            ),
            tags$label(
              `for` = ns("tissue_image_file"),
              class = "enhance-tissue-file-button builder-file-trigger",
              `tabindex` = "0",
              role = "button",
              span("+ Add tissue image…")
            )
          ),
          conditionalPanel(
            condition = "output['has_image']",
            div(
              class = "spatial-alignment-controls builder-controls-grid builder-controls-grid--sliders",
              tags$fieldset(
                class = "spatial-alignment-control-group",
                tags$legend("Position"),
                sliderInput(
                  ns("img_dx"),
                  "Horizontal offset",
                  -1,
                  1,
                  0,
                  ticks = FALSE
                ),
                sliderInput(
                  ns("img_dy"),
                  "Vertical offset",
                  -1,
                  1,
                  0,
                  ticks = FALSE
                )
              ),
              tags$fieldset(
                class = "spatial-alignment-control-group",
                tags$legend("Scale & orientation"),
                sliderInput(
                  ns("img_scale"),
                  "Scale",
                  0.2,
                  3,
                  1,
                  step = 0.02,
                  ticks = FALSE
                ),
                sliderInput(
                  ns("img_rotate"),
                  "Rotation (degrees)",
                  -180,
                  180,
                  0,
                  ticks = FALSE
                ),
                checkboxInput(ns("image_flip_x"), "Flip horizontally", FALSE),
                checkboxInput(ns("image_flip_y"), "Flip vertically", FALSE)
              ),
              tags$fieldset(
                class = "spatial-alignment-control-group",
                tags$legend("Appearance"),
                sliderInput(
                  ns("image_opacity"),
                  "Image opacity",
                  0,
                  100,
                  80,
                  step = 5,
                  post = "%",
                  ticks = FALSE
                ),
                sliderInput(
                  ns("point_opacity"),
                  "Point opacity",
                  0,
                  100,
                  85,
                  step = 5,
                  post = "%",
                  ticks = FALSE
                ),
                sliderInput(
                  ns("point_size"),
                  "Point size",
                  1,
                  12,
                  5,
                  step = 1,
                  ticks = FALSE
                )
              )
            ),
            div(
              class = "spatial-alignment-actions builder-action-row",
              actionButton(
                ns("apply_align"),
                "Save alignment",
                class = "btn btn-action"
              ),
              actionButton(
                ns("apply_align_all"),
                "Apply transform to all sections",
                class = "btn btn-quiet"
              ),
              actionButton(
                ns("reset_align"),
                "Reset alignment",
                class = "btn btn-quiet"
              )
            ),
            ns = ns
          )
        ),
        div(
          class = "spatial-alignment-main",
          div(
            class = "spatial-alignment-plots builder-preview-grid",
            tags$section(
              class = "spatial-alignment-plot-card builder-subcard",
              h5("Transcriptome space"),
              uiOutput(ns("alignment_projection_label"), inline = TRUE),
              builder_alignment_plot_output(
                ns("alignment_transcriptome_plot"),
                "Transcriptome-space cell plot"
              )
            ),
            tags$section(
              class = "spatial-alignment-plot-card builder-subcard",
              h5("Spatial space"),
              uiOutput(ns("alignment_spatial_label"), inline = TRUE),
              builder_alignment_plot_output(
                ns("alignment_spatial_plot"),
                "Spatial-space cell plot"
              )
            )
          ),
          div(
            class = "spatial-alignment-legend-wrap",
            h5("Groups"),
            uiOutput(ns("alignment_legend"))
          )
        )
      )
    }
  )
}

builder_enhance_stage_ui <- function(id, model, dynamic_modules = FALSE) {
  ns <- NS(id)
  histology <- model$attachments$histology %||% list()
  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-enhance builder-card builder-section",
    h2("Enhance"),
    p(
      class = "stage-intro",
      "Optional: add analysis pages or attach supporting files. You can skip this stage."
    ),
    tags$input(
      id = ns("rendered_for"),
      type = "text",
      class = "builder-rendered-for-input",
      value = model$id,
      hidden = "hidden",
      tabindex = "-1",
      `aria-hidden` = "true"
    ),
    h3("Optional analyses"),
    div(
      class = "enhance-module-grid",
      if (isTRUE(dynamic_modules)) {
        uiOutput(ns("analysis_modules"))
      } else {
        builder_enhance_modules_ui(id, model$modules %||% list())
      }
    ),
    h3("Optional attachments"),
    div(
      class = "enhance-attachment builder-subcard",
      h4("Tables for Extra material"),
      p(
        class = "enhance-attachment-description",
        "Add optional CSV or TSV tables to the generated app’s Extra material page."
      ),
      div(
        class = "enhance-table-file-control builder-file-picker builder-file-picker--content",
        tags$input(
          id = ns("table_files"),
          name = ns("table_files"),
          class = "shiny-input-file enhance-table-file-input builder-file-input",
          type = "file",
          multiple = "multiple",
          accept = ".csv,.tsv,.txt",
          `tabindex` = "-1"
        ),
        tags$label(
          `for` = ns("table_files"),
          class = "enhance-table-file-button builder-file-trigger",
          `tabindex` = "0",
          role = "button",
          span("+ Add tables…")
        )
      ),
      uiOutput(ns("table_list"))
    ),
    if (isTRUE(histology$relevant)) {
      div(
        class = "enhance-attachment spatial-alignment-workbench builder-subcard",
        builder_spatial_alignment_ui(id, histology)
      )
    }
  )
}
