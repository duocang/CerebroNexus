## Marker gene source dialogs.

builder_marker_source_choice_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "marker-source-choice",
    p(
      class = "marker-source-choice-intro",
      "Choose how this dataset should get Marker genes."
    ),
    div(
      class = "marker-source-choice-grid",
      actionButton(
        ns("marker_genes_calculate"),
        tagList(
          span(
            class = "marker-source-choice-title",
            "Calculate for all Groups"
          ),
          span(
            class = "marker-source-choice-description",
            "Run the built-in differential-expression analysis for every included grouping variable."
          )
        ),
        class = "marker-source-choice-card"
      ),
      actionButton(
        ns("marker_genes_upload"),
        tagList(
          span(
            class = "marker-source-choice-title",
            "Upload precomputed results"
          ),
          span(
            class = "marker-source-choice-description",
            "Import CSV, TSV, or XLSX tables and confirm their cluster mapping."
          )
        ),
        class = "marker-source-choice-card"
      )
    )
  )
}

builder_marker_dialog_ui <- function() {
  div(
    id = "builder-marker-dialog-backdrop",
    class = "builder-confirm-backdrop builder-marker-dialog-backdrop",
    hidden = "hidden",
    div(
      id = "builder-marker-dialog",
      class = "builder-dialog builder-marker-dialog",
      h2(id = "builder-marker-dialog-title", "Add Marker genes"),
      uiOutput("enhance-marker_dialog_body"),
      div(
        class = "builder-dialog-actions marker-dialog-actions",
        tags$button(
          id = "builder-marker-dialog-close",
          type = "button",
          class = "btn btn-quiet",
          "Cancel"
        )
      )
    )
  )
}

builder_marker_import_pending_ui <- function() {
  div(
    class = "marker-import-workbench",
    p(
      class = "hint",
      "The file mapping workbench is loading in the next implementation step."
    )
  )
}
