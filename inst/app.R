##----------------------------------------------------------------------------##
## load packages
##----------------------------------------------------------------------------##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)

##----------------------------------------------------------------------------##
## set options
##----------------------------------------------------------------------------##
custom_welcome_message <- "Welcome to CerebroNexus! This is a custom welcome message. You can change it in the app options."
Cerebro.options <<- list(
  "mode" = "closed",
  ## Keep the source demo runnable directly from inst/ without requiring an
  ## installed CerebroNexus package. Exported apps receive this value in
  ## cerebro_config.rds when createShinyApp() builds them.
  "cerebro_version" = "3.2.0",
  ## Keep one compact, runnable example in the CRAN package. Larger domain demos
  ## are distributed separately so the source package remains within CRAN's
  ## size expectations.
  "crb_file_to_load" = c(
    "PBMC example" = "extdata/v1.4/example.crb"
  ),
  "crb_pick_smallest_file" = FALSE,
  "spatial_images" = NULL,
  "cerebro_root" = ".",
  "welcome_message" = custom_welcome_message,
  "overview_default_point_size" = 1,
  "gene_expression_default_point_size" = 2,
  ## Larger default spatial points so cell-type layering reads clearly against
  ## the histology background in the demo.
  "point_size" = list("spatial_projection_point_size" = 5),
  "overview_default_point_opacity" = 0.3,
  "gene_expression_default_point_opacity" = 0.5,
  "overview_default_percentage_cells_to_show" = 100,
  "gene_expression_default_percentage_cells_to_show" = 20,
  "projections_show_hover_info" = FALSE
)

options(shiny.maxRequestSize = 800 * 1024^2)

##----------------------------------------------------------------------------##
## load server and UI functions
##----------------------------------------------------------------------------##
source("shiny/v1.4/shiny_UI.R", local = TRUE)
source("shiny/v1.4/shiny_server.R", local = TRUE)

##----------------------------------------------------------------------------##
## launch app
##----------------------------------------------------------------------------##
shiny::shinyApp(ui = ui, server = server)
