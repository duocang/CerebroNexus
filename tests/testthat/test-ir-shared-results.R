## Immune repertoire results that depend only on a data set's files are shared
## by the sessions of a process; everything else stays per session.

ir_shared_scope <- function() {
  scope <- new.env(parent = asNamespace("shiny"))
  scope$`%||%` <- function(a, b) if (is.null(a)) b else a
  for (name in c("ir_dataset_process_key", "ir_process_memo")) {
    eval(
      viewer_r_assignment(
        viewer_test_path("immune_repertoire", "data.R"),
        name
      ),
      scope
    )
  }
  scope$ir_process_cache <- new.env(parent = emptyenv())
  scope
}

test_that("a data set's process key follows its files", {
  scope <- ir_shared_scope()
  file <- withr::local_tempfile(fileext = ".crb")
  writeLines("one", file)
  ds <- structure(list(), cerebro_dataset_path = file)
  key <- scope$ir_dataset_process_key(ds)
  expect_identical(key$path, normalizePath(file, winslash = "/"))
  expect_identical(scope$ir_dataset_process_key(ds), key)
  writeLines("a longer second version", file)
  expect_false(identical(scope$ir_dataset_process_key(ds), key))
  expect_null(scope$ir_dataset_process_key(NULL))
  expect_null(scope$ir_dataset_process_key(list()))
  expect_null(scope$ir_dataset_process_key(
    structure(
      list(),
      cerebro_dataset_path = file.path(tempdir(), "missing.crb")
    )
  ))
})

test_that("process memo computes once per key and never without one", {
  scope <- ir_shared_scope()
  runs <- 0L
  compute <- function() {
    runs <<- runs + 1L
    runs
  }
  expect_identical(scope$ir_process_memo("units", list("a", 1), compute), 1L)
  expect_identical(scope$ir_process_memo("units", list("a", 1), compute), 1L)
  expect_identical(scope$ir_process_memo("units", list("b", 1), compute), 2L)
  expect_identical(scope$ir_process_memo("other", list("a", 1), compute), 3L)
  expect_identical(scope$ir_process_memo("units", NULL, compute), 4L)
  expect_identical(scope$ir_process_memo("units", NULL, compute), 5L)
})

test_that("sharing units match the barcodes once, not once per sample", {
  scope <- new.env(parent = globalenv())
  eval(
    viewer_r_assignment(
      viewer_test_path("immune_repertoire", "data.R"),
      "ir_discover_sharing_units"
    ),
    scope
  )
  scope$ir_scr_cols <- c("barcode", "CTgene")
  calls <- 0L
  scope$match <- function(...) {
    calls <<- calls + 1L
    base::match(...)
  }
  data <- list(
    a = data.frame(barcode = c("c3", "c1"), CTgene = "TRB-a"),
    empty = data.frame(barcode = character(), CTgene = character()),
    b = data.frame(barcode = "c2", CTgene = "TRB-a")
  )
  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3", "unmatched"),
    patient = c("p1", "p2", "p1", "p3"),
    site = c("t", "t", "t", "n")
  )
  expect_identical(
    scope$ir_discover_sharing_units(data, metadata),
    c("sample", "patient")
  )
  expect_identical(calls, 1L)
})

test_that("an immune repertoire without heavy chains skips the isotype table", {
  scope <- new.env(parent = globalenv())
  eval(
    viewer_r_assignment(
      viewer_test_path("immune_repertoire", "server.R"),
      "bcr_isotype_plot"
    ),
    scope
  )
  scope$bcr_extract_isotype <- function(...) stop("isotype table was built")
  tcr <- list(
    a = data.frame(barcode = "c1", CTgene = "TRAV1.TRAJ2_TRBV3.TRBJ4"),
    b = data.frame(barcode = "c2", CTgene = NA_character_)
  )
  expect_null(scope$bcr_isotype_plot(tcr))
  bcr <- list(a = data.frame(barcode = "c1", CTgene = "IGHV1.IGHJ2.IGHG1"))
  expect_error(scope$bcr_isotype_plot(bcr), "isotype table was built")
})

test_that("Abundance lines match plot_ly's split and colour mapping", {
  scope <- new.env(parent = globalenv())
  eval(
    viewer_r_assignment(
      viewer_test_path("immune_repertoire", "visualizations.R"),
      "ir_clonal_abundance_plotly"
    ),
    scope
  )
  scope$`%>%` <- dplyr::`%>%`
  scope$cerebro_plotly_axis <- function(...) list()
  scope$cerebro_plotly_hoverlabel <- function() list()
  scope$cerebro_plotly_theme <- function() list(transparent = "rgba(0,0,0,0)")
  scope$cerebro_plotly_toolbar <- identity
  for (n_samples in c(2L, 5L, 12L)) {
    samples <- sprintf("s%02d", rev(seq_len(n_samples)))
    counts <- data.frame(
      sample = rep(samples, each = 2L),
      abundance = rep(c(1L, 3L), n_samples),
      n_clones = seq_len(2L * n_samples)
    )
    built <- plotly::plotly_build(scope$ir_clonal_abundance_plotly(
      counts
    ))$x$data
    counts$hover <- paste0(
      "<b>",
      counts$sample,
      "</b><br>",
      "Abundance: ",
      formatC(counts$abundance, format = "d"),
      "<br>",
      "Clones: ",
      formatC(counts$n_clones, format = "d")
    )
    expected <- suppressWarnings(plotly::plotly_build(plotly::plot_ly(
      counts,
      x = ~abundance,
      y = ~n_clones,
      split = ~sample,
      color = ~sample,
      type = "scatter",
      mode = "lines",
      text = ~hover,
      hoverinfo = "text"
    )))$x$data
    expect_length(built, length(expected))
    field <- function(traces, f, ...) lapply(traces, f, ...)
    expect_identical(field(built, `[[`, "name"), field(expected, `[[`, "name"))
    expect_equal(field(built, `[[`, "x"), field(expected, `[[`, "x"))
    expect_equal(field(built, `[[`, "y"), field(expected, `[[`, "y"))
    expect_identical(
      field(built, function(t) as.character(t$text)),
      field(expected, function(t) as.character(t$text))
    )
    colour <- function(t) as.character(t$line$color)
    expect_identical(field(built, colour), field(expected, colour))
  }
})

test_that("sessions opening the same data set files share IR plots", {
  render_value <- function(expr, ...) {
    shiny::reactive(substitute(expr), env = parent.frame(), quoted = TRUE)
  }
  testthat::local_mocked_bindings(
    renderPlotly = render_value,
    .package = "plotly"
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    clonalHomeostasis = function(...) {
      calls <<- calls + 1L
      ggplot2::ggplot()
    },
    .package = "scRepertoire"
  )
  file <- withr::local_tempfile(fileext = ".crb")
  writeLines("one", file)
  path <- file
  ## testServer() gives every mock session its own "app" cache, so stand in
  ## for the process's app cache with one shared object.
  app_cache <- cachem::cache_mem()
  server <- function(input, output, session) {
    scope <- environment()
    output <- new.env()
    `%>%` <- dplyr::`%>%`
    available_crb_files <- reactiveValues(selected = "A")
    data_set <- reactive(structure(list(), cerebro_dataset_path = path))
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "server.R"),
      scope,
      names = c("ir_dataset_cache_key", "ir_bindCache")
    )
    expect_identical(formals(ir_bindCache)$cache, "app")
    shared_bind_cache <- ir_bindCache
    ir_bindCache <- function(x, ...) {
      shared_bind_cache(x, ..., cache = app_cache)
    }
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "data.R"),
      scope,
      names = c(
        "ir_dataset_process_key",
        "ir_params",
        "ir_param",
        "ir_order_by",
        "IR_CLONE_SIZE_NAMES",
        "IR_CLONE_SIZE_DEFAULT",
        "ir_clone_size"
      )
    )
    req_plot_space <- function(...) NULL
    req_scRepertoire <- function() NULL
    getGroups <- function() "sample"
    ir_data <- function() list(sample = data.frame(barcode = "c1"))
    ir_render_ggplotly <- function(expr, ...) expr
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "visualizations.R"),
      scope,
      names = c("IR_PALETTE", "output$ir_plot_clonalHomeostasis")
    )
  }
  draw <- function() {
    shiny::testServer(server, {
      session$setInputs(
        ir_cloneCall = "gene",
        ir_chain = "TRB",
        ir_groupBy = ""
      )
      scope$output$ir_plot_clonalHomeostasis()
    })
  }
  draw()
  expect_identical(calls, 1L)
  draw()
  expect_identical(calls, 1L)
  writeLines("a longer second version", file)
  draw()
  expect_identical(calls, 2L)
  ## Without a file on disk, each session keeps its own plots.
  path <- NULL
  draw()
  draw()
  expect_identical(calls, 4L)
})

test_that("sharing discovery labels first-frame rows with the Pack's cells", {
  scope <- new.env(parent = globalenv())
  eval(
    viewer_r_assignment(
      viewer_test_path("immune_repertoire", "data.R"),
      "ir_sharing_unit_metadata"
    ),
    scope
  )
  first_frame <- data.frame(patient = c("p1", "p2", "p1"))
  pack <- list(canonical_order = TRUE)
  scope$viewerProjectionFirstFrameCache <- function() {
    list(meta_data = first_frame)
  }
  scope$viewerPackCurrent <- function() pack
  scope$viewerPackCellBarcodes <- function(pack) c("c1", "c2", "c3")
  scope$getMetaData <- function() stop("unexpected metadata hydration")
  metadata <- scope$ir_sharing_unit_metadata()
  expect_identical(metadata$cell_barcode, c("c1", "c2", "c3"))
  expect_identical(metadata$patient, first_frame$patient)

  hydrated <- data.frame(cell_barcode = c("c1", "c2", "c3"), patient = "p")
  scope$getMetaData <- function() hydrated
  pack$canonical_order <- FALSE
  expect_identical(scope$ir_sharing_unit_metadata(), hydrated)
  pack$canonical_order <- TRUE
  scope$viewerPackCellBarcodes <- function(pack) c("c1", "c2")
  expect_identical(scope$ir_sharing_unit_metadata(), hydrated)
  scope$viewerProjectionFirstFrameCache <- function() NULL
  scope$viewerPackCellBarcodes <- function(pack) c("c1", "c2", "c3")
  expect_identical(scope$ir_sharing_unit_metadata(), hydrated)
})
