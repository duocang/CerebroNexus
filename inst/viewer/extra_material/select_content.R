##----------------------------------------------------------------------------##
## Select category and content.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to align material selectors on one row.
##----------------------------------------------------------------------------##
output[["extra_material_select_category_and_content_UI"]] <- renderUI({
  tags$div(
    style = "display: flex; gap: 32px; align-items: flex-end; flex-wrap: wrap;",
    uiOutput("extra_material_selected_category_UI"),
    uiOutput("extra_material_selected_content_UI")
  )
})

##----------------------------------------------------------------------------##
## UI element to select from which category the content should be shown.
##----------------------------------------------------------------------------##
output[["extra_material_selected_category_UI"]] <- renderUI({
  categories <- getExtraMaterialCategories()
  selectInput(
    "extra_material_selected_category",
    label = "Material type:",
    choices = categories,
    width = "320px"
  )
})

##----------------------------------------------------------------------------##
## UI element to select which content should be shown.
##----------------------------------------------------------------------------##
output[["extra_material_selected_content_UI"]] <- renderUI({
  req(input[["extra_material_selected_category"]])
  ## if selected category is `tables`
  if (
    input[["extra_material_selected_category"]] == 'tables' &&
      checkForExtraTables() == TRUE
  ) {
    groups <- extra_material_table_groups()
    file_choices <- extra_material_table_file_choices(groups)
    selected_file <- input[["extra_material_selected_file"]]
    if (
      length(selected_file) != 1L || !selected_file %in% unname(file_choices)
    ) {
      selected_file <- unname(file_choices)[[1L]]
    }
    selection <- extra_material_table_selection(
      groups,
      file_key = selected_file
    )
    req(!is.null(selection))
    sheet_choices <- stats::setNames(
      vapply(selection$group$sheets, `[[`, character(1), "key"),
      vapply(selection$group$sheets, `[[`, character(1), "label")
    )
    selected_sheet <- input[["extra_material_selected_content"]]
    if (
      length(selected_sheet) != 1L || !selected_sheet %in% unname(sheet_choices)
    ) {
      selected_sheet <- unname(sheet_choices)[[1L]]
    }

    tags$div(
      style = "display: flex; gap: 32px; align-items: flex-end; flex-wrap: wrap;",
      selectInput(
        "extra_material_selected_file",
        label = "Choose a file:",
        choices = file_choices,
        selected = selected_file,
        width = "320px"
      ),
      selectInput(
        "extra_material_selected_content",
        label = "Choose a table:",
        choices = sheet_choices,
        selected = selected_sheet,
        width = "320px"
      )
    )
    ## if selected category is `plots`
  } else if (
    input[["extra_material_selected_category"]] == 'plots' &&
      checkForExtraPlots() == TRUE
  ) {
    ##
    selectInput(
      "extra_material_selected_content",
      label = "Choose a plot:",
      choices = getNamesOfExtraPlots(),
      width = "320px"
    )
  }
})
