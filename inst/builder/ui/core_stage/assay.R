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
