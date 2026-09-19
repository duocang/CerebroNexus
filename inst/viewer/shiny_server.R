##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##

## plotting_functions.R holds only pure plot-builder functions (no reactive /
## input / output / session references), so source it ONCE at process startup
## instead of re-evaluating every definition on every new browser session. It
## lands in this file's environment (which encloses server()), so the server
## reactives still reach the functions by lexical scope. color_setup.R and
## utility_functions.R stay inside server(): the former defines a top-level
## reactive (reactive_colors); the latter's helpers close over session-scope
## reactives (data_set(), preferences, ...).
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/plotting_functions.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/color_config.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/source_cache.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/core/viewer_pack.R"),
  local = TRUE
)

## Generated Extra material tables are immutable. Share their lazy cache across
## sessions instead of reading the same sheet again for every browser tab.
.extra_material_process_cache <- new.env(parent = emptyenv())

## Keep only the compact MSigDB catalogue and resolved gene vectors across
## sessions. The complete msigdbr table is discarded after each public query.
.msigdb_process_cache <- new.env(parent = emptyenv())

## Immutable CRB prototypes are shared by all sessions in this R process.
## get_or_load_crb() always returns a shallow R6 clone, so session-local lazy
## hydration (for example one spatial FOV's molecules) cannot leak to another
## browser session while the large immutable fields and on-disk handles remain
## shared through R's copy-on-modify semantics.
.crb_process_cache <- new.env(parent = emptyenv())
.viewer_pack_process_cache <- new.env(parent = emptyenv())
.crb_launch_prototypes <- Cerebro.options[[".dataset_prototypes"]]
if (!is.list(.crb_launch_prototypes)) {
  .crb_launch_prototypes <- list()
}
if (length(.crb_launch_prototypes)) {
  for (prototype_path in names(.crb_launch_prototypes)) {
    viewerPackOpenCached(
      prototype_path,
      .crb_launch_prototypes[[prototype_path]],
      .viewer_pack_process_cache
    )
  }
}

server <- function(input, output, session) {
  source <- function(file, local = FALSE, ...) {
    dots <- list(...)
    if (identical(local, TRUE) && !length(dots)) {
      return(viewerSource(file, parent.frame()))
    }
    base::source(file, local = local, ...)
  }

  ##--------------------------------------------------------------------------##
  ## Load color setup and utility functions.
  ##--------------------------------------------------------------------------##
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/color_setup.R"),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/utility_functions.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/core/cell_view_message.R"
    ),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/core/cell_view_wire.R"
    ),
    local = TRUE
  )
  if (length(.crb_launch_prototypes)) {
    backend_plan <- Cerebro.options[[".bundle_backend_plan"]]
    configured_paths <- unname(
      Cerebro.options[["crb_file_to_load"]] %||% character()
    )
    for (prototype_path in names(.crb_launch_prototypes)) {
      effective_backend <- .configuredRuntimeBackendPlan(
        prototype_path,
        backend_plan,
        configured_paths
      )
      .cacheCrbPrototype(
        prototype_path,
        .crb_launch_prototypes[[prototype_path]],
        .runtimeBackendCacheIdentity(effective_backend)
      )
    }
  }
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/coordinated_views/config.R"
    ),
    local = TRUE
  )
  observeEvent(
    input[["cell_view_aux_request"]],
    {
      request <- input[["cell_view_aux_request"]]
      req(is.list(request), request$id, request$wire_token)
      key <- paste(request$id, request$wire_token, sep = ":")
      message <- .cerebro_cell_view_aux_pending[[key]]
      req(!is.null(message))
      rm(list = key, envir = .cerebro_cell_view_aux_pending)
      message <- cerebroCellViewResolveDeferredAux(message)
      session$sendBinaryMessage(
        "cell_view_aux_binary",
        cv_wire_pack_message(message)
      )
    },
    ignoreInit = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/clone_contract.R"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  preferences <- reactiveValues(
    cell_point_size = list(
      min = 1,
      max = 20,
      step = 1
    ),
    cell_point_opacity = list(
      min = 0.1,
      max = 1.0,
      step = 0.1
    ),
    cell_percentage_cells_to_show = list(
      min = 10,
      max = 100,
      step = 10
    ),
    use_webgl = TRUE,
    show_hover_info_in_projections = ifelse(
      exists('Cerebro.options') &&
        !is.null(Cerebro.options[['projections_show_hover_info']]),
      Cerebro.options[['projections_show_hover_info']],
      TRUE
    )
  )

  ## Outputs inside collapsed boxes may stay active while their owning sidebar
  ## page is current. Re-suspend them on every page switch so inactive modules
  ## cannot initialise expensive data or expression backends in the background.
  viewer_hidden_output_options <- list()
  outputOptions <- function(output, x, ...) {
    options <- list(...)
    owner <- viewerOutputTab(x)
    if (identical(options$suspendWhenHidden, FALSE) && !is.na(owner)) {
      viewer_hidden_output_options[[x]] <<- options
      options$suspendWhenHidden <- !identical(
        isolate(input[["sidebar"]]),
        owner
      )
    }
    do.call(shiny::outputOptions, c(list(x = output, name = x), options))
  }
  observeEvent(input[["sidebar"]], {
    tab <- input[["sidebar"]]
    ids <- names(viewer_hidden_output_options)
    for (id in ids) {
      options <- viewer_hidden_output_options[[id]]
      options$suspendWhenHidden <- !identical(viewerOutputTab(id), tab)
      do.call(
        shiny::outputOptions,
        c(list(x = output, name = id), options)
      )
    }
  })

  viewer_initial_page_tabs <- c(
    data_info = "loadData",
    projection = "overview",
    linked_views = "coordinated_views",
    groups = "groups",
    marker_genes = "markerGenes",
    most_expressed_genes = "mostExpressedGenes",
    enriched_pathways = "enrichedPathways",
    extra_material = "extra_material",
    immune_repertoire = "immune_repertoire",
    trajectory = "trajectory",
    spatial = "spatial",
    trekker = "trekker",
    hla_tcr_motifs = "hla_tcr_motifs",
    gene_expression = "geneExpression",
    gene_id_conversion = "geneIdConversion",
    color_management = "color_management",
    about = "about"
  )
  initial_page <- Cerebro.options[["initial_page"]]
  initial_tab <- if (
    is.character(initial_page) &&
      length(initial_page) == 1L &&
      initial_page %in% names(viewer_initial_page_tabs)
  ) {
    unname(viewer_initial_page_tabs[[initial_page]])
  } else {
    NULL
  }
  conditional_tabs <- unname(viewer_initial_page_tabs[c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectory",
    "spatial",
    "trekker",
    "hla_tcr_motifs"
  )])
  initial_page_applied <- reactiveVal(FALSE)
  if (!is.null(initial_tab) && !initial_tab %in% conditional_tabs) {
    session$onFlushed(
      function() {
        updateTabItems(session, "sidebar", selected = initial_tab)
        initial_page_applied(TRUE)
      },
      once = TRUE
    )
  }

  ##--------------------------------------------------------------------------##
  ## Load data set.
  ##--------------------------------------------------------------------------##

  ## reactive values holding available .crb files and the current selection.
  ## In single-file mode only 'selected' is used; when >1 files are provided via
  ## Cerebro.options$crb_file_to_load, 'files'/'names' drive the sidebar dataset
  ## switcher rendered below.
  available_crb_files <- reactiveValues(
    files = NULL,
    selected = NULL,
    names = NULL
  )
  dataset_load_requested <- reactiveVal(FALSE)
  crb_prefetch_tasks <- new.env(parent = emptyenv())

  start_crb_prefetch <- function(path) {
    path <- as.character(path[[1L]])
    if (!file.exists(path)) {
      return(invisible(NULL))
    }
    cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (
      !is.null(.crb_process_cache[[cache_key]]) ||
        !is.null(crb_prefetch_tasks[[cache_key]])
    ) {
      return(invisible(NULL))
    }
    prefetch_task <- shiny::ExtendedTask$new(function(path) {
      mirai::mirai(
        {
          connection <- file(path, open = "rb")
          on.exit(close(connection), add = TRUE)
          magic <- readBin(connection, "raw", n = 4L)
          prototype <- if (
            identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))
          ) {
            qs2::qs_read(path)
          } else {
            readRDS(path)
          }
          list(path = path, prototype = prototype)
        },
        path = path
      )
    })
    crb_prefetch_tasks[[cache_key]] <- prefetch_task
    print(glue::glue(
      "[{Sys.time()}] CRB background prefetch started: {.crbLogLabel(path)}"
    ))
    prefetch_task$invoke(path)
    invisible(NULL)
  }

  current_scatter_defaults <- reactive({
    viewerScatterDefaults(
      Cerebro.options,
      viewerDatasetName(
        available_crb_files$files,
        available_crb_files$selected
      )
    )
  })

  current_expression_scatter_defaults <- reactive({
    viewerScatterDefaults(
      Cerebro.options,
      viewerDatasetName(
        available_crb_files$files,
        available_crb_files$selected
      ),
      page = "expression"
    )
  })

  ## listen to selected 'input_file', initialize before UI element is loaded
  observeEvent(input[['input_file']], ignoreNULL = FALSE, {
    path_to_load <- viewerUploadPath(input[["input_file"]], Cerebro.options)
    if (nzchar(path_to_load)) {
      ## an uploaded file replaces the pre-configured data sets, so clear the
      ## switcher state — otherwise the dropdown keeps offering the old data
      ## sets, which no longer match what is loaded.
      available_crb_files$files <- NULL
      available_crb_files$names <- NULL
      ## take path or object from 'Cerebro.options' if it is set and points to an
      ## existing file or object
    } else if (
      exists('Cerebro.options') &&
        !is.null(Cerebro.options[["crb_file_to_load"]])
    ) {
      file_to_load <- Cerebro.options[["crb_file_to_load"]]
      ## multiple files (or a single named file) -> enable dataset switcher
      if (length(file_to_load) > 1 || !is.null(names(file_to_load))) {
        available_crb_files$files <- file_to_load
        file_names <- names(file_to_load)
        if (
          !is.null(file_names) &&
            length(file_names) == length(file_to_load)
        ) {
          available_crb_files$names <- file_names
        } else {
          available_crb_files$names <- NULL
        }

        ##--------------------------------------------------------------------##
        ## Check for a dataset specified in the URL (query string or path),
        ## e.g. '?dataset=sampleA' or '/sampleA'.
        ##--------------------------------------------------------------------##
        url_dataset <- NULL

        ## 1. query string (?dataset=...)
        query <- parseQueryString(session$clientData$url_search)
        if (!is.null(query$dataset)) {
          url_dataset <- query$dataset
        }

        ## 2. pathname (e.g. /dataset_name or /app/dataset_name). Use only the
        ## LAST path segment as the token, so the app still resolves it when
        ## mounted under a sub-path (e.g. shiny-server at /app/TCR -> "TCR").
        if (
          is.null(url_dataset) &&
            !is.null(session$clientData$url_pathname)
        ) {
          path_val <- session$clientData$url_pathname
          path_val <- gsub("/$", "", path_val) # drop trailing slash
          segments <- strsplit(path_val, "/", fixed = TRUE)[[1]]
          segments <- segments[nzchar(segments)]
          if (length(segments) > 0) {
            ## URL-decode so links with encoded names (e.g. %20) still match
            url_dataset <- utils::URLdecode(segments[length(segments)])
          }
        }

        ## try to match url_dataset against available files
        if (!is.null(url_dataset)) {
          path_to_load <- match_dataset_by_url(
            url_dataset,
            available_crb_files$files,
            available_crb_files$names
          )
          if (path_to_load != '') {
            print(glue::glue(
              "[{Sys.time()}] Dataset selected via URL: {url_dataset} -> {path_to_load}"
            ))
          }
        }

        ## if not chosen via URL: keep current selection, else pick default.
        ## crb_pick_smallest_file TRUE/NULL -> smallest file; FALSE -> first.
        if (path_to_load != '') {
          ## already set by URL logic
        } else if (!is.null(available_crb_files$selected)) {
          path_to_load <- available_crb_files$selected
        } else {
          pick_smallest <- TRUE
          if (!is.null(Cerebro.options[["crb_pick_smallest_file"]])) {
            pick_smallest <- as.logical(
              Cerebro.options[["crb_pick_smallest_file"]]
            )
          }
          if (isTRUE(pick_smallest)) {
            file_sizes <- sapply(file_to_load, function(f) {
              if (file.exists(f)) {
                file.size(f)
              } else {
                Inf ## variable/object -> infinite size, skipped
              }
            })
            path_to_load <- file_to_load[which.min(file_sizes)]
          } else {
            path_to_load <- file_to_load[1]
          }
        }
      } else {
        ## single unnamed file
        available_crb_files$files <- NULL
        available_crb_files$names <- NULL
        if (file.exists(file_to_load) || exists(file_to_load)) {
          path_to_load <- file_to_load
        }
      }
    }
    ## assign path to example file if none of the above apply
    if (length(path_to_load) == 0 || all(path_to_load == '')) {
      ## Resolve relative to cerebro_root, never via the package: the exported
      ## bundle, inst/app.R and the installed launcher all point cerebro_root at
      ## a directory that carries extdata/, so this fallback stays self-contained
      ## even when the app runs without CerebroNexus installed (a package
      ## lookup would then resolve to "" and silently break the fallback).
      path_to_load <- file.path(
        Cerebro.options[["cerebro_root"]],
        "extdata/examples/example.crb"
      )
    }
    ## set reactive value to selected file path
    if (
      is.null(available_crb_files$selected) ||
        available_crb_files$selected != path_to_load
    ) {
      available_crb_files$selected <- path_to_load
    }
  })

  ## renderUI for the dataset switcher; shown only when >1 .crb files are
  ## available, inert (returns NULL) in single-file mode.
  output[["crb_file_selector_UI"]] <- renderUI({
    if (
      !is.null(available_crb_files$files) &&
        length(available_crb_files$files) > 1
    ) {
      choices <- available_crb_files$files
      names(choices) <- if (!is.null(available_crb_files$names)) {
        available_crb_files$names
      } else {
        basename(available_crb_files$files)
      }
      selected <- available_crb_files$selected
      if (is.null(selected)) {
        selected <- choices[1]
      }
      tagList(
        ## The "Select sample dataset" title already labels this control, so the
        ## selectInput's own label would just repeat it — drop it.
        titlePanel("Select sample dataset"),
        selectInput(
          inputId = "crb_file_selector",
          label = NULL,
          choices = choices,
          selected = selected,
          width = '350px'
        )
      )
    }
  })

  ## listen to the dataset switcher and update the current selection
  observeEvent(input[['crb_file_selector']], {
    if (
      !is.null(input[['crb_file_selector']]) &&
        !is.null(available_crb_files$files)
    ) {
      if (
        is.null(available_crb_files$selected) ||
          available_crb_files$selected != input[['crb_file_selector']]
      ) {
        available_crb_files$selected <- input[['crb_file_selector']]
      }
    }
  })

  ## Keep the lightweight Data Info page independent of the full CRB. Opening
  ## another page, or selecting a dataset absent from the catalog, enables the
  ## ordinary data_set() chain.
  observeEvent(
    available_crb_files$selected,
    {
      catalog <- if (exists("Cerebro.options")) {
        Cerebro.options[[".dataset_catalog"]]
      } else {
        NULL
      }
      info <- viewerDatasetInfo(catalog, available_crb_files$selected)
      dataset_load_requested(viewerDatasetLoadRequired(
        isolate(input[["sidebar"]]),
        info,
        initial_tab,
        isolate(initial_page_applied())
      ))
      selected <- available_crb_files$selected
      if (!is.null(info) && length(selected) == 1L) {
        session$onFlushed(
          function() {
            if (session$isClosed()) {
              return()
            }
            withReactiveDomain(session, {
              current <- isolate(available_crb_files$selected)
              if (
                length(current) == 1L &&
                  identical(as.character(current), as.character(selected))
              ) {
                start_crb_prefetch(selected)
              }
            })
          },
          once = TRUE
        )
      }
    },
    ignoreNULL = FALSE,
    priority = 100
  )

  observeEvent(
    input[["sidebar"]],
    {
      dataset_load_requested(viewerDatasetLoadOnTab(
        isolate(dataset_load_requested()),
        input[["sidebar"]]
      ))
    },
    ignoreNULL = FALSE
  )

  current_dataset_info <- reactive({
    req(!is.null(available_crb_files$selected))
    selected <- available_crb_files$selected
    catalog <- if (exists("Cerebro.options")) {
      Cerebro.options[[".dataset_catalog"]]
    } else {
      NULL
    }
    info <- viewerDatasetInfo(catalog, selected)
    if (!is.null(info)) {
      return(info)
    }

    req(isTRUE(dataset_load_requested()))
    data <- data_set()
    experiment <- data$getExperiment()
    scalar <- function(value) {
      if (is.null(value) || length(value) != 1L || is.na(value)) {
        return(NA_character_)
      }
      as.character(value)
    }
    list(
      label = viewerDatasetName(available_crb_files$files, selected),
      path = selected,
      cells = .runtimeCerebroCellCount(data),
      organism = scalar(experiment$organism),
      date = scalar(experiment$date_of_export)
    )
  })

  ## create reactive value holding the current data set
  data_set <- reactive({
    req(!is.null(available_crb_files$selected))
    req(isTRUE(dataset_load_requested()))
    dataset_to_load <- available_crb_files$selected
    if (exists(dataset_to_load)) {
      print(glue::glue(
        "[{Sys.time()}] Load data set from variable: {dataset_to_load}"
      ))
      data <- get(dataset_to_load)
    } else {
      ## Route through the session cache defined in utility_functions.R.
      ## Configured bundle CRBs consume the exact backend plan validated during
      ## createShinyApp(). Uploads and older configurations fall back to the
      ## ordinary expression_backend field; serialized getters are not called.
      backend_plan <- if (exists("Cerebro.options")) {
        Cerebro.options[[".bundle_backend_plan"]]
      } else {
        NULL
      }
      configured_paths <- if (
        exists("Cerebro.options") &&
          !is.null(Cerebro.options[["crb_file_to_load"]])
      ) {
        unname(Cerebro.options[["crb_file_to_load"]])
      } else {
        character()
      }
      cache_key <- normalizePath(
        dataset_to_load,
        winslash = "/",
        mustWork = FALSE
      )
      if (is.null(.crb_process_cache[[cache_key]])) {
        prefetch_task <- crb_prefetch_tasks[[cache_key]]
        if (!is.null(prefetch_task)) {
          status <- prefetch_task$status()
          if (status %in% c("initial", "running")) {
            prefetch_task$result()
          } else if (identical(status, "success")) {
            prefetched <- prefetch_task$result()
            req(identical(prefetched$path, as.character(dataset_to_load)))
            effective_backend <- .configuredRuntimeBackendPlan(
              dataset_to_load,
              backend_plan,
              configured_paths
            )
            .cacheCrbPrototype(
              dataset_to_load,
              prefetched$prototype,
              .runtimeBackendCacheIdentity(effective_backend)
            )
            print(glue::glue(
              "[{Sys.time()}] CRB background prefetch ready: ",
              "{.crbLogLabel(dataset_to_load)}"
            ))
          } else {
            tryCatch(
              prefetch_task$result(),
              error = function(error) {
                warning(
                  "CRB background prefetch failed; loading synchronously: ",
                  conditionMessage(error),
                  call. = FALSE
                )
              }
            )
          }
        }
      }
      data <- get_or_load_crb(
        dataset_to_load,
        backend_plan,
        configured_paths
      )
    }
    expression_is_deferred <- is.environment(data) &&
      exists("expression", envir = data, inherits = FALSE) &&
      isTRUE(rlang::env_binding_are_lazy(data, "expression"))
    if (expression_is_deferred) {
      print(glue::glue(
        "[{Sys.time()}] Data loaded; expression backend will attach on first use."
      ))
    } else {
      message(data$print())
      if (!is.null(data$expression)) {
        print(glue::glue(
          "[{Sys.time()}] Format of expression data: {class(data$expression)}"
        ))
      }
    }
    viewer_pack <- if (
      is.character(dataset_to_load) &&
        length(dataset_to_load) == 1L &&
        file.exists(dataset_to_load)
    ) {
      viewerPackOpenCached(
        dataset_to_load,
        data,
        .viewer_pack_process_cache
      )
    } else {
      NULL
    }
    attr(data, "cerebro_viewer_pack") <- viewer_pack
    ## return loaded data
    return(data)
  })

  ## Large projection coordinates already exist as interleaved Float32 assets
  ## in the validated Viewer Pack. Expose only that projection directory under
  ## a session-specific resource prefix so specialist views can fetch the file
  ## directly instead of copying the same 8 MB through R and Shiny's websocket.
  viewer_projection_resources <- new.env(parent = emptyenv())
  viewer_projection_prefixes <- character()
  viewerProjectionAsset <- function(name, cell_indices = NULL) {
    pack <- viewerPackCurrent()
    if (
      !is.list(pack) ||
        !isTRUE(pack$canonical_order) ||
        (
          !is.null(cell_indices) &&
            !identical(as.integer(cell_indices), seq_len(pack$cell_count))
        )
    ) {
      return(NULL)
    }
    projection_names <- as.character(pack$manifest$projection_names)
    projection_index <- match(as.character(name), projection_names)
    if (is.na(projection_index)) {
      return(NULL)
    }
    asset_path <- file.path(
      "projections",
      sprintf("%03d.bin", projection_index)
    )
    key <- paste(pack$path, asset_path, sep = "::")
    if (exists(key, envir = viewer_projection_resources, inherits = FALSE)) {
      return(get(key, envir = viewer_projection_resources, inherits = FALSE))
    }
    assets <- pack$manifest$assets
    asset_row <- which(as.character(assets$path) == asset_path)
    dimensions <- if (length(asset_row) == 1L) {
      suppressWarnings(as.integer(strsplit(
        as.character(assets$dimensions[[asset_row]]),
        "x",
        fixed = TRUE
      )[[1L]]))
    } else {
      integer()
    }
    file <- file.path(pack$path, asset_path)
    valid <- length(asset_row) == 1L &&
      identical(as.character(assets$dtype[[asset_row]]), "float32") &&
      length(dimensions) == 2L &&
      identical(dimensions[[1L]], as.integer(pack$cell_count)) &&
      dimensions[[2L]] %in% c(2L, 3L) &&
      file.exists(file) &&
      !dir.exists(file) &&
      identical(
        as.numeric(file.info(file)$size),
        as.numeric(assets$bytes[[asset_row]])
      ) &&
      identical(
        unname(tools::md5sum(file)),
        as.character(assets$checksum[[asset_row]])
      )
    if (!valid) {
      return(NULL)
    }
    prefix <- paste0(
      "cerebro-projection-",
      gsub("[^A-Za-z0-9_-]", "", session$token),
      "-",
      length(viewer_projection_prefixes) + 1L
    )
    shiny::addResourcePath(prefix, dirname(file))
    viewer_projection_prefixes <<- c(viewer_projection_prefixes, prefix)
    descriptor <- list(
      url = paste0(prefix, "/", basename(file)),
      cells = dimensions[[1L]],
      dimensions = dimensions[[2L]],
      bytes = as.numeric(assets$bytes[[asset_row]]),
      checksum = as.character(assets$checksum[[asset_row]])
    )
    assign(key, descriptor, envir = viewer_projection_resources)
    descriptor
  }
  ## Categorical metadata codes are also stored in canonical cell order. The
  ## first Linked views frame needs one such column, which can otherwise be the
  ## largest remaining websocket vector. Keep the small level/color contract in
  ## the bundle and let the browser fetch the validated packed codes directly.
  viewer_metadata_resources <- new.env(parent = emptyenv())
  viewer_metadata_prefixes <- character()
  viewerMetadataCodesAsset <- function(name, levels = NULL) {
    pack <- viewerPackCurrent()
    if (
      !is.list(pack) ||
        !isTRUE(pack$canonical_order) ||
        !is.character(name) ||
        length(name) != 1L ||
        is.na(name) ||
        !nzchar(name)
    ) {
      return(NULL)
    }
    metadata_names <- as.character(pack$manifest$metadata_names)
    metadata_index <- match(name, metadata_names)
    if (is.na(metadata_index)) {
      return(NULL)
    }
    prefix_path <- file.path("metadata", sprintf("%03d", metadata_index))
    codes_path <- paste0(prefix_path, ".codes.bin")
    dictionary_path <- paste0(prefix_path, ".dictionary.json")
    requested_levels <- if (is.null(levels)) NULL else as.character(levels)
    key <- paste(pack$path, codes_path, paste(requested_levels, collapse = "\r"),
      sep = "::")
    if (exists(key, envir = viewer_metadata_resources, inherits = FALSE)) {
      return(get(key, envir = viewer_metadata_resources, inherits = FALSE))
    }
    assets <- pack$manifest$assets
    codes_row <- which(as.character(assets$path) == codes_path)
    dictionary_row <- which(as.character(assets$path) == dictionary_path)
    if (length(codes_row) != 1L || length(dictionary_row) != 1L) {
      return(NULL)
    }
    dtype <- as.character(assets$dtype[[codes_row]])
    dimensions <- suppressWarnings(as.integer(
      as.character(assets$dimensions[[codes_row]])
    ))
    codes_file <- file.path(pack$path, codes_path)
    dictionary_file <- file.path(pack$path, dictionary_path)
    bytes_per_code <- if (identical(dtype, "uint8")) {
      1L
    } else if (identical(dtype, "uint16")) {
      2L
    } else {
      NA_integer_
    }
    valid_files <- !is.na(bytes_per_code) &&
      length(dimensions) == 1L &&
      identical(dimensions, as.integer(pack$cell_count)) &&
      file.exists(codes_file) &&
      !dir.exists(codes_file) &&
      file.exists(dictionary_file) &&
      !dir.exists(dictionary_file) &&
      identical(
        as.numeric(file.info(codes_file)$size),
        as.numeric(assets$bytes[[codes_row]])
      ) &&
      identical(
        as.numeric(file.info(codes_file)$size),
        as.numeric(pack$cell_count) * bytes_per_code
      ) &&
      identical(
        as.numeric(file.info(dictionary_file)$size),
        as.numeric(assets$bytes[[dictionary_row]])
      ) &&
      identical(
        unname(tools::md5sum(codes_file)),
        as.character(assets$checksum[[codes_row]])
      ) &&
      identical(
        unname(tools::md5sum(dictionary_file)),
        as.character(assets$checksum[[dictionary_row]])
      )
    if (!valid_files) {
      return(NULL)
    }
    dictionary <- tryCatch(
      as.character(jsonlite::read_json(dictionary_file, simplifyVector = TRUE)),
      error = function(error) NULL
    )
    target_levels <- if (is.null(requested_levels)) {
      sort(unique(dictionary))
    } else {
      requested_levels
    }
    remap <- match(dictionary, target_levels) - 1L
    if (
      is.null(dictionary) ||
        length(dictionary) != as.integer(assets$dimensions[[dictionary_row]]) ||
        anyNA(remap)
    ) {
      return(NULL)
    }
    resource_prefix <- paste0(
      "cerebro-metadata-",
      gsub("[^A-Za-z0-9_-]", "", session$token),
      "-",
      length(viewer_metadata_prefixes) + 1L
    )
    shiny::addResourcePath(resource_prefix, dirname(codes_file))
    viewer_metadata_prefixes <<- c(
      viewer_metadata_prefixes,
      resource_prefix
    )
    descriptor <- list(
      url = paste0(resource_prefix, "/", basename(codes_file)),
      cells = as.integer(pack$cell_count),
      dtype = dtype,
      bytes = as.numeric(assets$bytes[[codes_row]]),
      checksum = as.character(assets$checksum[[codes_row]]),
      levels = I(target_levels),
      ## Viewer Pack uses 0 for missing and 1..K for dictionary entries.
      ## Linked views uses -1 for missing and 0..K-1 for its displayed levels.
      code_map = I(c(-1L, as.integer(remap)))
    )
    assign(key, descriptor, envir = viewer_metadata_resources)
    descriptor
  }
  session$onSessionEnded(function() {
    for (prefix in viewer_projection_prefixes) {
      try(shiny::removeResourcePath(prefix), silent = TRUE)
    }
    for (prefix in viewer_metadata_prefixes) {
      try(shiny::removeResourcePath(prefix), silent = TRUE)
    }
  })

  cv_saved_view_cells <- reactive({
    metadata <- getMetaData()
    if ("cell_barcode" %in% colnames(metadata)) {
      as.character(metadata$cell_barcode)
    } else {
      rownames(metadata)
    }
  })

  viewerDatasetIdentity <- reactive({
    dataset <- data_set()
    pack <- attr(dataset, "cerebro_viewer_pack", exact = TRUE)
    order_fingerprint <- if (is.list(pack)) {
      as.character(pack$manifest$cell_order_fingerprint %||% "")
    } else {
      ""
    }
    stored_fingerprint <- tryCatch(
      dataset$cell_fingerprint,
      error = function(error) NULL
    )
    if (
      is.character(stored_fingerprint) &&
        length(stored_fingerprint) == 1L &&
        !is.na(stored_fingerprint) &&
        grepl("^md5-cell-set-v1:[[:xdigit:]]{32}$", stored_fingerprint)
    ) {
      return(list(
        cell_count = getNumberOfCells(),
        fingerprint = stored_fingerprint,
        order_fingerprint = order_fingerprint
      ))
    }
    cells <- cv_saved_view_cells()
    list(
      cell_count = length(cells),
      fingerprint = cv_config_dataset_fingerprint(cells, stored_fingerprint),
      order_fingerprint = order_fingerprint
    )
  })

  cv_saved_view_dataset <- reactive({
    cells <- cv_saved_view_cells()
    list(
      cells = cells,
      fingerprint = viewerDatasetIdentity()$fingerprint
    )
  })

  observe({
    input[["sidebar"]]
    identity <- viewerDatasetIdentity()
    session$sendCustomMessage(
      "cerebro_saved_view_dataset",
      list(
        cell_count = identity$cell_count,
        cell_fingerprint = identity$fingerprint,
        cell_order_fingerprint = identity$order_fingerprint
      )
    )
  })

  # list of available trajectories
  available_trajectories <- reactive({
    req(!is.null(data_set()))
    ## collect available trajectories across all methods and create selectable
    ## options
    available_trajectories <- c()
    available_trajectory_method <- getMethodsForTrajectories()
    ## check if at least 1 trajectory method exists
    if (length(available_trajectory_method) > 0) {
      ## cycle through trajectory methods
      for (i in seq_along(available_trajectory_method)) {
        ## get current method and names of trajectories for this method
        current_method <- available_trajectory_method[i]
        available_trajectories_for_this_method <- getNamesOfTrajectories(
          current_method
        )
        ## check if at least 1 trajectory is available for this method
        if (length(available_trajectories_for_this_method) > 0) {
          ## cycle through trajectories for this method
          for (j in seq_along(available_trajectories_for_this_method)) {
            ## create selectable combination of method and trajectory name and add
            ## it to the available trajectories
            current_trajectory <- available_trajectories_for_this_method[j]
            available_trajectories <- c(
              available_trajectories,
              glue::glue("{current_method} // {current_trajectory}")
            )
          }
        }
      }
    }
    # message(str(available_trajectories))
    return(available_trajectories)
  })

  # available genes
  list_of_genes <- reactive({
    req(data_set())
    getGeneNames()
  })

  # hover info for projection.
  # Cached by (dataset path, hover toggle): selecting a different gene does
  # not re-build the per-cell hover strings because they only depend on the
  # metadata of the current dataset, not on the active gene. Unlike the
  # expression-level reactive, this chain has no gene dependency and no
  # isolate(), so the cache key stays consistent across gene switches.
  hover_info_projections <- reactive({
    # message('--> trigger "hover_info_projections"')
    if (
      !is.null(preferences[["show_hover_info_in_projections"]]) &&
        preferences[['show_hover_info_in_projections']] == TRUE
    ) {
      cells_df <- getMetaData()
      hover_info <- buildHoverInfoForProjections(cells_df)
      hover_info <- setNames(hover_info, cells_df$cell_barcode)
    } else {
      hover_info <- 'none'
    }
    # message(str(hover_info))
    return(hover_info)
  }) %>%
    cachePlot(
      preferences[["show_hover_info_in_projections"]],
      available_crb_files$selected
    )

  ## Dynamic sidebar: conditional tabs are shown or hidden based on dataset
  ## content (see toggleConditionalTab() below).
  ##--------------------------------------------------------------------------##

  ##--------------------------------------------------------------------------##
  ## Print log message when switching tab (for debugging).
  ##--------------------------------------------------------------------------##
  observe({
    print(glue::glue("[{Sys.time()}] Active tab: {input[['sidebar']]}"))
  })

  ##--------------------------------------------------------------------------##
  ## Print message when session is closed due to inactivity.
  ##--------------------------------------------------------------------------##
  observeEvent(input$timeOut, {
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste(
        "Session timeout due to",
        input$timeOut,
        "inactivity -",
        Sys.time()
      ),
      footer = NULL
    ))
    session$close()
  })

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/module/group_filters/group_filters_widget.R"
    ),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/load_data/server.R"),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/overview/server.R"),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/groups/server.R"),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/gene_expression/server.R"
    ),
    local = TRUE
  )
  ##--------------------------------------------------------------------------##
  ## Dynamic sidebar: show/hide conditional tabs based on dataset content.
  ##--------------------------------------------------------------------------##
  toggleConditionalTab <- function(tab_name, check_fn, catalog_field = NULL) {
    item_id <- paste0("sidebar_item_", tab_name)
    show_reactive <- reactive({
      if (!is.null(catalog_field)) {
        info <- current_dataset_info()
        catalog_value <- info[[catalog_field]]
        if (is.null(catalog_value) && is.list(info$capabilities)) {
          catalog_value <- info$capabilities[[catalog_field]]
        }
        if (is.logical(catalog_value) && length(catalog_value) == 1L) {
          return(isTRUE(catalog_value))
        }
      }
      req(data_set())
      result <- tryCatch(check_fn(), error = function(e) FALSE)
      if (is.logical(result)) {
        return(result)
      }
      length(result) > 0
    })
    observe({
      should_show <- show_reactive()
      shinyjs::toggle(id = item_id, condition = should_show)
      decision <- viewerInitialPageDecision(
        initial_tab,
        tab_name,
        should_show,
        isolate(initial_page_applied())
      )
      if (!is.null(decision)) {
        initial_page_applied(decision$applied)
        if (!is.null(decision$selected)) {
          updateTabItems(session, "sidebar", selected = decision$selected)
        }
      }
    })
  }

  toggleConditionalTab(
    "markerGenes",
    function() getMethodsForMarkerGenes(),
    catalog_field = "marker_genes"
  )
  toggleConditionalTab(
    "mostExpressedGenes",
    function() getGroupsWithMostExpressedGenes(),
    catalog_field = "most_expressed_genes"
  )
  toggleConditionalTab(
    "enrichedPathways",
    function() getMethodsForEnrichedPathways(),
    catalog_field = "enriched_pathways"
  )
  toggleConditionalTab(
    "extra_material",
    function() {
      length(getExtraMaterialCategories()) > 0L
    },
    catalog_field = "extra_material"
  )
  toggleConditionalTab(
    "immune_repertoire",
    function() {
      getImmuneRepertoireSummary()$available
    },
    catalog_field = "immune_repertoire"
  )
  toggleConditionalTab(
    "trajectory",
    ## Only supported methods should surface the tab; an unsupported
    ## method would otherwise render a blank tab instead of the empty state.
    function() {
      viewerSupportedTrajectoryMethods(
        getMethodsForTrajectories()
      )
    },
    catalog_field = "trajectory"
  )
  toggleConditionalTab(
    "spatial",
    function() availableSpatial(),
    catalog_field = "spatial"
  )
  ## Trekker single-cell spatial mapping: its own bespoke page (not the generic
  ## Spatial tab). Shown only when the loaded .crb carries a `trekker` slot.
  toggleConditionalTab(
    "trekker",
    function() {
      tk <- tryCatch(data_set()$getTrekker(), error = function(e) NULL)
      !is.null(tk)
    },
    catalog_field = "trekker"
  )
  toggleConditionalTab(
    "hla_tcr_motifs",
    ## Show only when the data set actually carries a TCR (TRA/TRB). HLA typing
    ## is NOT required — the motif network works without it, and the Data & QC
    ## tab is where a user would add HLA, so the page must be reachable first.
    ##
    ## The lightweight gate scans every sample; the older IR page helper stops
    ## after three and could hide a valid cohort whose TCR starts at sample four.
    function() {
      summary <- getImmuneRepertoireSummary()
      chains <- summary$chains
      if (length(chains)) {
        return(any(chains %in% c("TRA", "TRB")))
      }
      if (!summary$available) {
        return(FALSE)
      }
      tryCatch(
        viewerHasTcrRepertoire(getImmuneRepertoire()),
        error = function(e) FALSE
      )
    },
    catalog_field = "tcr_repertoire"
  )

  ## Cleanup snapshot artifacts that may have been left by test runs.
  snapshot_dir <- file.path(
    Cerebro.options[["cerebro_root"]],
    "..",
    "..",
    "tests",
    "testthat",
    "_snaps"
  )
  new_pngs <- list.files(
    snapshot_dir,
    pattern = "\\.new\\.png$",
    full.names = TRUE
  )
  if (length(new_pngs) > 0) {
    file.remove(new_pngs)
  }
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/spatial/server.R"),
    local = TRUE
  )

  deferred_viewer_server_files <- c(
    markerGenes = "marker_genes/server.R",
    geneIdConversion = "gene_id_conversion/server.R",
    color_management = "color_management/server.R",
    about = "about/server.R",
    mostExpressedGenes = "most_expressed_genes/server.R",
    enrichedPathways = "enriched_pathways/server.R",
    extra_material = "extra_material/server.R",
    immune_repertoire = "immune_repertoire/server.R",
    trajectory = "trajectory/server.R",
    trekker = "trekker/server.R",
    coordinated_views = "coordinated_views/server.R",
    hla_tcr_motifs = "hla_tcr_motifs/server.R"
  )
  server_scope <- environment()
  deferred_viewer_server_loaded <- new.env(parent = emptyenv())
  load_deferred_viewer_server <- function(server_file) {
    if (
      exists(
        server_file,
        envir = deferred_viewer_server_loaded,
        inherits = FALSE
      )
    ) {
      return(invisible(FALSE))
    }
    sys.source(
      file.path(
        Cerebro.options[["cerebro_root"]],
        "viewer",
        server_file
      ),
      envir = server_scope
    )
    assign(server_file, TRUE, envir = deferred_viewer_server_loaded)
    invisible(TRUE)
  }
  observeEvent(
    input[["coordviews_visible"]],
    {
      if (isTRUE(input[["coordviews_visible"]])) {
        load_deferred_viewer_server("coordinated_views/server.R")
      }
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input[["sidebar"]],
    {
      server_file <- unname(deferred_viewer_server_files[input[["sidebar"]]])
      if (length(server_file) && !is.na(server_file)) {
        load_deferred_viewer_server(server_file)
      }
    },
    ignoreInit = FALSE
  )

  ##--------------------------------------------------------------------------##
  ## Export reactive values for testing (shinytest2).
  ##--------------------------------------------------------------------------##
  exportTestValues(
    overview_cells_to_show = {
      if (is.null(data_set())) {
        NULL
      } else {
        overview_projection_cells_to_show()
      }
    },
    expression_levels = {
      if (is.null(data_set())) {
        NULL
      } else {
        expression_projection_expression_levels()
      }
    },
    ## Lazy-load regression guard: app startup must NOT load scRepertoire. Its
    ## ~90-package tree is exactly what deferred loading avoids, so if this is
    ## TRUE at startup the settings/tab gates have regressed into eager loading.
    ## Only a real repertoire plot render should pull scRepertoire in.
    scRepertoire_loaded = "scRepertoire" %in% loadedNamespaces(),
    ## Representative heavy deps unique to the scRepertoire tree (immApex / iNEXT
    ## are pulled in by nothing else). A prewarm-like implementation would load
    ## these too, so asserting they stay absent — with a delayed re-check in the
    ## test, past the former 1s prewarm timer — catches a deferred loader that
    ## was merely sampled before its callback ran.
    ir_heavy_deps_loaded = any(
      c("scRepertoire", "immApex", "iNEXT") %in% loadedNamespaces()
    )
  )
}
