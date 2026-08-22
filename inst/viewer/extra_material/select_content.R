##----------------------------------------------------------------------------##
## Select category and content.
##----------------------------------------------------------------------------##

extra_material_selector <- function(input) {
  div(class = "result-selector-field", input)
}

output[["extra_material_select_category_and_content_UI"]] <- renderUI({
  div(
    class = "extra-material-selectors",
    uiOutput("extra_material_selected_category_UI"),
    uiOutput("extra_material_selected_content_UI")
  )
})

output[["extra_material_selected_category_UI"]] <- renderUI({
  categories <- getExtraMaterialCategories()
  req(length(categories) > 0L)
  selector <- extra_material_selector(selectInput(
    "extra_material_selected_category",
    label = "Material type:",
    choices = categories,
    selected = categories[[1L]],
    width = "100%"
  ))
  if (length(categories) == 1L) {
    div(style = "display: none;", selector)
  } else {
    selector
  }
})

output[["extra_material_selected_content_UI"]] <- renderUI({
  req(input[["extra_material_selected_category"]])
  if (
    input[["extra_material_selected_category"]] == "tables" &&
      checkForExtraTables()
  ) {
    groups <- extra_material_table_groups()
    file_choices <- extra_material_table_file_choices(groups)
    selected_file <- input[["extra_material_selected_file"]]
    if (
      !is.character(selected_file) ||
        length(selected_file) != 1L ||
        is.na(selected_file) ||
        !selected_file %in% unname(file_choices)
    ) {
      selected_file <- file_choices[[1L]]
    }
    selection <- extra_material_table_selection(
      groups,
      file_key = selected_file,
      load = FALSE
    )
    req(!is.null(selection))
    sheets <- extra_material_table_sheets(selection$group)
    sheet_choices <- stats::setNames(
      vapply(sheets, `[[`, character(1), "key"),
      vapply(sheets, `[[`, character(1), "label")
    )
    selected_sheet <- input[["extra_material_selected_content"]]
    if (
      !is.character(selected_sheet) ||
        length(selected_sheet) != 1L ||
        is.na(selected_sheet) ||
        !selected_sheet %in% unname(sheet_choices)
    ) {
      selected_sheet <- sheet_choices[[1L]]
    }
    selectors <- list()
    if (length(file_choices) > 1L) {
      selectors <- c(
        selectors,
        list(extra_material_selector(selectInput(
          "extra_material_selected_file",
          label = "Choose a file:",
          choices = file_choices,
          selected = selected_file,
          width = "100%"
        )))
      )
    }
    if (length(sheet_choices) > 1L) {
      selectors <- c(
        selectors,
        list(extra_material_selector(selectInput(
          "extra_material_selected_content",
          label = "Choose a table:",
          choices = sheet_choices,
          selected = selected_sheet,
          width = "100%"
        )))
      )
    }
    tagList(selectors)
  } else if (
    input[["extra_material_selected_category"]] == "plots" &&
      checkForExtraPlots()
  ) {
    extra_material_selector(selectInput(
      "extra_material_selected_content",
      label = "Choose a plot:",
      choices = getNamesOfExtraPlots(),
      width = "100%"
    ))
  }
})
