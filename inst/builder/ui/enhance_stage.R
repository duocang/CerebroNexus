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
  list(
    id = id,
    modules = modules,
    attachments = list(
      tables = list(
        label = "Supplementary tables",
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
        label = "Histology images",
        enabled_pages = "spatial",
        relevant = length(profile$images %||% character()) > 0L ||
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
        sections = profile$images %||% character(),
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

.builder_enhance_disclosure_notes <- function(item) {
  tags$details(
    class = "enhance-details",
    tags$summary("What this changes"),
    p(
      class = "pages",
      paste0(
        "Enabled page: ",
        paste(
          item$enabled_pages %||% item$pages %||% character(),
          collapse = ", "
        )
      )
    ),
    p(
      class = "replacement",
      paste0("Replacement policy: ", item$replacement_policy %||% "")
    ),
    p(
      class = "skip",
      paste0("Skip consequence: ", item$skip_consequence %||% "")
    ),
    p(class = "cost", paste0("Cost: ", item$cost %||% "not applicable")),
    p(class = "network", item$network %||% "No network access."),
    p(
      class = "prerequisite",
      paste0(
        "Prerequisite: ",
        item$prerequisite %||% item$dependency %||% "none"
      )
    )
  )
}

builder_enhance_modules_ui <- function(id, modules) {
  ns <- NS(id)
  modules <- Filter(function(module) isTRUE(module$relevant), modules)
  if (!length(modules)) {
    return(p(class = "hint", "No optional modules apply to this dataset."))
  }
  tagList(lapply(modules, function(module) {
    div(
      class = paste(
        "enhance-module",
        if (isTRUE(module$blocked)) "is-blocked" else NULL
      ),
      tags$label(
        tags$input(
          id = ns(paste0("analysis_", module$id)),
          type = "checkbox",
          class = "shiny-input-checkbox",
          checked = if (isTRUE(module$selected)) "checked",
          disabled = if (isTRUE(module$blocked)) "disabled"
        ),
        tags$span(module$label)
      ),
      if (isTRUE(module$blocked)) {
        p(class = "blocked", module$blocked_reason %||% "Unavailable")
      },
      p(class = "consequence", module$consequence %||% ""),
      .builder_enhance_disclosure_notes(module)
    )
  }))
}

builder_enhance_saved_image_ui <- function(id, section, section_count) {
  ns <- NS(id)
  div(
    style = "margin-top:.5rem;display:flex;gap:.5rem;align-items:center",
    span(
      class = "pill on",
      if (section_count > 1L) {
        paste0("Alignment saved for “", section, "”")
      } else {
        "Alignment saved"
      }
    ),
    actionButton(ns("drop_image"), "Remove", class = "btn btn-quiet")
  )
}

builder_enhance_stage_ui <- function(id, model, dynamic_modules = FALSE) {
  ns <- NS(id)
  tables <- model$attachments$tables %||% list()
  histology <- model$attachments$histology %||% list()
  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-enhance",
    h2("Enhance"),
    p(
      class = "stage-intro",
      "Optional: add analysis pages or attach supporting files. You can skip this stage."
    ),
    tags$input(
      id = ns("rendered_for"),
      type = "hidden",
      value = model$id
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
      class = "enhance-attachment",
      h4(tables$label %||% "Supplementary tables"),
      .builder_enhance_disclosure_notes(tables),
      textInput(ns("table_path"), "CSV / TSV path"),
      textInput(ns("table_name"), "Display name (optional)"),
      actionButton(ns("add_table"), "Add table", class = "btn"),
      checkboxGroupInput(
        ns("tables_to_retain"),
        "Tables to retain",
        choices = tables$selected %||% character(),
        selected = tables$selected %||% character()
      )
    ),
    if (isTRUE(histology$relevant)) {
      div(
        class = "enhance-attachment",
        h4(histology$label %||% "Histology images"),
        .builder_enhance_disclosure_notes(histology),
        checkboxGroupInput(
          ns("histology_to_retain"),
          "Saved histology to retain",
          choices = histology$selected %||% character(),
          selected = histology$selected %||% character()
        ),
        if (length(histology$sections %||% character())) {
          tagList(
            selectInput(
              ns("active_slice"),
              "Tissue section",
              choices = histology$sections,
              selected = histology$sections[[1L]]
            ),
            textInput(ns("image_path"), "PNG / JPEG path"),
            selectInput(
              ns("image_bounds_mode"),
              "Image extent",
              choices = c(
                "Cell coordinates are image pixels" = "pixels",
                "Cell coordinates use physical units" = "physical",
                "Fit to the cell bounding box" = "bbox"
              )
            ),
            numericInput(ns("image_um"), "Physical units per pixel", 1),
            numericInput(ns("image_max_px"), "Maximum image edge (px)", 1400),
            actionButton(ns("attach_image"), "Load image", class = "btn"),
            uiOutput(ns("image_state")),
            sliderInput(ns("img_dx"), "Horizontal offset", -1, 1, 0),
            sliderInput(ns("img_dy"), "Vertical offset", -1, 1, 0),
            sliderInput(ns("img_scale"), "Scale", 0.2, 3, 1),
            sliderInput(ns("img_rotate"), "Rotation (degrees)", -180, 180, 0),
            checkboxInput(ns("image_flip"), "Flip vertically", FALSE),
            checkboxInput(ns("image_flip_x"), "Flip horizontally", FALSE),
            plotly::plotlyOutput(ns("overlay_plot"), height = "360px"),
            actionButton(ns("apply_align"), "Save for this section"),
            actionButton(ns("apply_align_all"), "Apply to all sections"),
            actionButton(ns("reset_align"), "Reset alignment")
          )
        }
      )
    },
    h3("Auto-retained content"),
    if (!length(model$auto_retained %||% list())) {
      p(class = "hint", "No validated optional content is auto-retained.")
    },
    lapply(model$auto_retained %||% list(), function(content) {
      div(
        class = "enhance-retained",
        h4(content$label),
        .builder_enhance_disclosure_notes(content)
      )
    })
  )
}
