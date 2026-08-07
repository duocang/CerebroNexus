#' @title
#' Launch CerebroNexus
#'
#' @description
#' Launch the CerebroNexus Shiny application.
#'
#' @param mode Cerebro can be ran in \code{open} or \code{closed} mode, allowing
#' the user to load their own data set (\code{open}) or only show a pre-loaded
#' data set (\code{closed}, removes the "Load data" element); defaults to
#' \code{open}.
#' @param maxFileSize Maximum size of input file; defaults to \code{800}
#' (800 MB).
#' @param crb_file_to_load Path to \code{.crb} file to load on launch of
#' Cerebro. Useful when using/hosting Cerebro in \code{closed} mode. Defaults to
#' \code{NULL}.
#' @param expression_matrix_mode  Mode of expression matrix. Can be either
#' crb, h5, or BPCells. Default is crb.
#' @param expression_matrix_h5 Optional: Path to \code{.h5} file containing an expression
#' matrix created with \code{HDF5Array::writeTENxMatrix()}, with genes as
#' columns and cells as rows, contrary to the conventional format of genes as
#' rows and cells as columns. This format greatly favors performance for
#' extracting expression values for a gene (column), rather than a cell (row),
#' which is the primary action in Cerebro. Importantly, the matrix should be
#' stored with "expression" as group name (see parameters of the
#' \code{HDF5Array::writeTENxMatrix()} function). Saving the expression matrix
#' in \code{TENxMatrix} format has the benefit of a low memory footprint since
#' the expression values are directly read from disk. This is particularly
#' useful when working with very large data sets and/or when startup of the
#' Cerebro app is a priority (which is shorter because only the rest of the data
#' that needs to be loaded tends to be very small). By default, this value is
#' set to \code{NULL}, meaning that the expression matrix is expected to be part
#' of the \code{.crb} file.
#' @param expression_matrix_BPCells Optional: Path to BPCells directory created with
#' \code{BPCells::write_matrix_dir()}. This is a hopefully faster alternative to h5
#' with a similar approach.
#' @param welcome_message \code{string} with custom welcome message to display
#' in the "Load data" tab. Can contain HTML formatting, e.g.
#' \code{'<h3>Hi!</h3>'}. Defaults to \code{NULL}.
#' @param overview_default_point_size Default point size in overview. This
#' value can be changed in the UI; defaults to 5.
#' @param gene_expression_default_point_size Default point size in gene_expression. This
#' value can be changed in the UI; defaults to 5.
#' @param overview_default_point_opacity Default point opacity in
#' overview. This value can be changed in the UI; defaults to 1.0.
#' @param overview_default_percentage_cells_to_show Default percentage of
#' cells to show in overview. This value can be changed in the UI; defaults
#' to 100.
#' @param gene_expression_default_point_opacity Default point opacity in
#' gene expression. This value can be changed in the UI; defaults to 1.0.
#' @param gene_expression_default_percentage_cells_to_show Default percentage of
#' cells to show in gene expression. This value can be changed in the UI; defaults
#' to 100.
#' @param projections_show_hover_info Show hover infos in projections. This
#' setting can be changed in the UI; defaults to TRUE.
#' @param auth Optional strict authentication descriptor. \code{NULL}, the
#' default, leaves the Viewer unauthenticated. The only supported named-list
#' shape has \code{provider = "shinymanager"}; \code{credentials} must be an
#' absolute path to a readable encrypted shinymanager SQLite database; and
#' \code{passphrase_env} must name an environment variable containing at least
#' 32 bytes of high-entropy secret material. The optional whole-number
#' \code{timeout_minutes} is from 1 through 1440; \code{timeout_minutes} defaults
#' to 15.
#' The environment-held passphrase is read only for validation and provider
#' setup; it is never stored in \code{Cerebro.options} or returned artifacts.
#' Authentication gates access to the Viewer but does not provide transport
#' security, rate limiting, SSO, MFA, centralized identity-provider revocation,
#' or network policy. For \code{launchCerebro()}, the host credentials database
#' must be a regular readable/writable file outside every Shiny HTTP resource
#' directory. Its containing directory must be writable and searchable for
#' encrypted logs and SQLite journals, and all other parent directories must be
#' searchable. The database, provider, and passphrase are validated before the
#' app is returned.
#' @param ... Forwarded to the \code{shiny::shinyApp} constructor, for example
#' \code{onStart}, \code{options}, or \code{uiPattern}; these are not
#' \code{shiny::runApp} host or port arguments.
#'
#' @return
#' A Shiny application object. Authentication descriptor and database errors
#' occur before the app is returned.
#'
#' @examples
#' if ( interactive() ) {
#'   launchCerebro(
#'     mode = "open",
#'     maxFileSize = 800
#'   )
#' }
#'
#' @importFrom colourpicker colourInput
#' @import dplyr
#' @importFrom DT datatable formatPercentage formatRound formatSignif formatStyle styleColorBar styleEqual styleInterval
#' @import ggplot2
#' @importFrom grDevices col2rgb rgb
#' @importFrom msigdbr msigdbr
#' @importFrom plotly add_lines add_trace event_data layout plot_ly plotlyOutput renderPlotly toWebGL
#' @importFrom stringr str_length
#' @importFrom tidyr pivot_longer pivot_wider
#' @import scales
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @import shinydashboard
#' @importFrom shinyFiles getVolumes parseSavePath shinyFileSave shinySaveButton
#' @importFrom shinyjs inlineCSS
#' @importFrom shinyWidgets awesomeCheckbox dropdownButton materialSwitch radioGroupButtons sendSweetAlert
#'
#' @export
#'
launchCerebro <- function(
  mode = "open",
  maxFileSize = 800,
  crb_file_to_load = NULL,
  expression_matrix_mode = "crb",
  expression_matrix_h5 = NULL,
  expression_matrix_BPCells = NULL,
  welcome_message = NULL,
  overview_default_point_size = 5,
  gene_expression_default_point_size = 5,
  overview_default_point_opacity = 1,
  gene_expression_default_point_opacity = 1,
  overview_default_percentage_cells_to_show = 100,
  gene_expression_default_percentage_cells_to_show = 100,
  projections_show_hover_info = TRUE,
  auth = NULL,
  ...
) {
  ##--------------------------------------------------------------------------##
  ## Check validity of input parameters.
  ##--------------------------------------------------------------------------##
  if (mode %in% c('open', 'closed') == FALSE) {
    stop(
      "'mode' parameter must be set to either 'open' or 'closed'.",
      call. = FALSE
    )
  }
  if (
    overview_default_point_size < 0 ||
      overview_default_point_size > 20
  ) {
    stop(
      "'overview_default_point_size' parameter must be between 1 and 20",
      call. = FALSE
    )
  }
  if (
    gene_expression_default_point_opacity < 0 ||
      gene_expression_default_point_opacity > 1
  ) {
    stop(
      "'gene_expression_default_point_opacity' parameter must be between 0 and 1",
      call. = FALSE
    )
  }
  if (
    gene_expression_default_percentage_cells_to_show < 0 ||
      gene_expression_default_percentage_cells_to_show > 100
  ) {
    stop(
      "'gene_expression_default_percentage_cells_to_show' parameter must be between 0 and 100",
      call. = FALSE
    )
  }
  if (projections_show_hover_info %in% c(TRUE, FALSE) == FALSE) {
    stop(
      "'projections_show_hover_info' parameter must be set to either TRUE or FALSE.",
      call. = FALSE
    )
  }

  cerebro_root <- system.file(package = "CerebroNexus")
  viewer_auth <- .compileViewerAuth(
    auth,
    "host",
    cerebro_root = cerebro_root
  )

  ## --------------------------------------------------------------------------##
  ## Create global variable with options that need to be available inside the
  ## Shiny app.
  ## --------------------------------------------------------------------------##
  cerebro_options <- list(
    "mode" = mode,
    "cerebro_version" = as.character(
      utils::packageVersion("CerebroNexus")
    ),
    "expression_matrix_mode" = expression_matrix_mode,
    "crb_file_to_load" = crb_file_to_load,
    "expression_matrix_h5" = expression_matrix_h5,
    "expression_matrix_BPCells" = expression_matrix_BPCells,
    "welcome_message" = welcome_message,
    "cerebro_root" = cerebro_root,
    "overview_default_point_size" = overview_default_point_size,
    "overview_default_point_opacity" = overview_default_point_opacity,
    "overview_default_percentage_cells_to_show" = overview_default_percentage_cells_to_show,
    "gene_expression_default_point_size" = gene_expression_default_point_size,
    "gene_expression_default_point_opacity" = gene_expression_default_point_opacity,
    "gene_expression_default_percentage_cells_to_show" = gene_expression_default_percentage_cells_to_show,
    "projections_show_hover_info" = projections_show_hover_info
  )
  if (!is.null(viewer_auth$config)) {
    cerebro_options[[".viewer_auth"]] <- viewer_auth$config
  }

  had_cerebro_options <- exists(
    "Cerebro.options",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  previous_cerebro_options <- if (had_cerebro_options) {
    get("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  had_request_size <- "shiny.maxRequestSize" %in% names(options())
  previous_request_size <- getOption("shiny.maxRequestSize")
  launch_committed <- FALSE
  on.exit(
    {
      if (!launch_committed) {
        tryCatch(
          {
            if (had_cerebro_options) {
              assign(
                "Cerebro.options",
                previous_cerebro_options,
                envir = .GlobalEnv
              )
            } else if (
              exists(
                "Cerebro.options",
                envir = .GlobalEnv,
                inherits = FALSE
              )
            ) {
              rm("Cerebro.options", envir = .GlobalEnv)
            }
          },
          error = function(condition) NULL
        )
        tryCatch(
          {
            if (had_request_size) {
              options(shiny.maxRequestSize = previous_request_size)
            } else {
              options(shiny.maxRequestSize = NULL)
            }
          },
          error = function(condition) NULL
        )
      }
    },
    add = TRUE
  )
  assign("Cerebro.options", cerebro_options, envir = .GlobalEnv)

  ##--------------------------------------------------------------------------##
  ## Allow upload of files up to 800 MB.
  ##--------------------------------------------------------------------------##
  options(shiny.maxRequestSize = maxFileSize * 1024^2)

  ##--------------------------------------------------------------------------##
  ## Load server and UI functions.
  ##--------------------------------------------------------------------------##
  source(
    system.file(
      paste0("viewer/shiny_UI.R"),
      package = "CerebroNexus"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("viewer/shiny_server.R"),
      package = "CerebroNexus"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("viewer/auth.R"),
      package = "CerebroNexus"
    ),
    local = TRUE
  )
  viewer_app <- viewer_auth_apply(
    ui,
    server,
    config = Cerebro.options[[".viewer_auth"]],
    cerebro_root = Cerebro.options[["cerebro_root"]]
  )

  ##--------------------------------------------------------------------------##
  ## Launch Cerebro.
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '##---------------------------------------------------------------------------##\n',
      '## Launching CerebroNexus\n',
      '##---------------------------------------------------------------------------##'
    )
  )
  app <- shiny::shinyApp(
    ui = viewer_app$ui,
    server = viewer_app$server,
    ...
  )
  launch_committed <- TRUE
  rm(
    had_cerebro_options,
    previous_cerebro_options,
    had_request_size,
    previous_request_size
  )
  app
}
