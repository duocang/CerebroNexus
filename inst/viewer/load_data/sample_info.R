##----------------------------------------------------------------------------##
## Sample info.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI elements that show some basic information about the loaded data set.
##----------------------------------------------------------------------------##
#
output[["load_data_sample_info_UI"]] <- renderUI({
  tagList(
    h3("Sample information"),
    ## Three stat cards laid out in one row (4/4/4 of the 12-col grid); each
    ## valueBoxOutput's own width is cleared so the enclosing column controls it.
    ## Columns stack automatically on narrow screens.
    fluidRow(
      column(
        width = 4,
        valueBoxOutput("load_data_number_of_cells", width = NULL)
      ),
      column(width = 4, valueBoxOutput("load_data_organism", width = NULL)),
      column(
        width = 4,
        valueBoxOutput("load_data_date_of_export", width = NULL)
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Value boxes that show:
## - number of cells in data set
## - organism
## - date of export
##----------------------------------------------------------------------------##

##number of cells
output[["load_data_number_of_cells"]] <- renderValueBox({
  info <- current_dataset_info()
  valueBox(
    value = formatC(
      info$cells,
      format = "f",
      big.mark = ",",
      digits = 0
    ),
    subtitle = "Cells",
    color = "light-blue",
    icon = icon("list"),
  )
})

## organism
output[["load_data_organism"]] <- renderValueBox({
  info <- current_dataset_info()
  organism <- info$organism
  if (length(organism) != 1L || is.na(organism) || !nzchar(organism)) {
    organism <- "not available"
  }
  if (organism == "hg") {
    valueBox(
      value = organism,
      subtitle = "Organism",
      color = "yellow",
      icon = icon("user")
    )
  } else {
    valueBox(
      value = organism,
      subtitle = "Organism",
      color = "yellow",
      icon = icon("paw")
    )
  }
})

## date of export
## as.character() because the date is otherwise converted to interger
output[["load_data_date_of_export"]] <- renderValueBox({
  info <- current_dataset_info()
  date <- info$date
  if (length(date) != 1L || is.na(date) || !nzchar(date)) {
    date <- "not available"
  }
  valueBox(
    value = as.character(date),
    subtitle = "Date",
    color = "green",
    icon = icon("calendar-day")
  )
})
