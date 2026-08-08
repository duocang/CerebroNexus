## Guided Core stage.

builder_core_assay_controls <- function(profile, settings, assay) {
  assay_profile <- profile$assay_profiles[[assay]]
  if (!is.list(assay_profile)) {
    stop("The selected assay profile is unavailable.", call. = FALSE)
  }
  select_value <- function(current, choices, default = NULL) {
    choices <- unname(as.character(choices %||% character()))
    selected <- if (
      builder_stage_has_text(current %||% "") &&
        current %in% choices
    ) {
      current
    } else if (
      builder_stage_has_text(default %||% "") &&
        default %in% choices
    ) {
      default
    } else if (length(choices)) {
      choices[[1L]]
    } else {
      character()
    }
    list(choices = choices, selected = unname(selected))
  }
  list(
    layer = select_value(
      settings$layer,
      assay_profile$layers,
      assay_profile$default_layer
    ),
    nUMI = select_value(
      settings$nUMI,
      assay_profile$nUMI_choices,
      assay_profile$nUMI
    ),
    nGene = select_value(
      settings$nGene,
      assay_profile$nGene_choices,
      assay_profile$nGene
    )
  )
}

builder_core_stage_ui <- function(id, model) {
  ns <- NS(id)
  organism_choices <- model$organism_choices %||% character()
  organism <- model$organism %||% ""
  if (
    builder_stage_has_text(organism) &&
      !organism %in% unname(organism_choices)
  ) {
    organism_choices <- c(organism_choices, organism)
  }
  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-core",
    h2("Core"),
    tags$input(
      id = ns("rendered_for"),
      type = "hidden",
      value = model$id
    ),
    textInput(ns("name"), "Dataset name", value = model$name %||% ""),
    selectizeInput(
      ns("organism"),
      "Organism",
      choices = organism_choices,
      selected = organism,
      options = list(
        create = TRUE,
        persist = TRUE,
        maxItems = 1L
      )
    ),
    selectInput(
      ns("default_group"),
      "Default group",
      choices = model$group_choices,
      selected = model$default_group
    ),
    selectInput(
      ns("default_projection"),
      "Default projection",
      choices = model$projection_choices,
      selected = model$default_projection
    ),
    if (builder_stage_has_text(model$metadata_attention %||% "")) {
      div(class = "notice warn", model$metadata_attention)
    },
    tags$details(
      tags$summary("Advanced technical settings"),
      selectInput(
        ns("assay"),
        "Assay",
        choices = model$assay_choices,
        selected = model$assay
      ),
      selectInput(
        ns("layer"),
        "Layer",
        choices = model$layer_choices,
        selected = model$layer
      ),
      selectInput(
        ns("nUMI"),
        "UMI QC column",
        choices = model$nUMI_choices,
        selected = model$nUMI
      ),
      selectInput(
        ns("nGene"),
        "Gene QC column",
        choices = model$nGene_choices,
        selected = model$nGene
      ),
      selectInput(
        ns("backend"),
        "Expression backend",
        choices = model$backend_choices,
        selected = model$backend
      )
    )
  )
}
