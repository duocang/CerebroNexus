## Unit tests for helper functions defined in
## inst/viewer/utility_functions.R.
##
## These functions live in the Shiny app tree (not the package R/ namespace),
## so they are loaded by sourcing the file into a throw-away environment. They
## are pure R and require neither a running app nor Seurat.
##
## Coverage focuses on the edge cases that previously crashed the app at
## runtime (NA-only percentage columns, NULL/NA toggle inputs, a missing
## grouping column) plus the caching contract of the cachePlot() wrapper. See
## the git history of utility_functions.R for context.

## Prefer the installed copy (mirrors how test-app-inst.R locates the app),
## falling back to the source tree when running against an uninstalled
## checkout (e.g. devtools::load_all()).
utils_file <- system.file(
  "viewer",
  "utility_functions.R",
  package = "CerebroNexus"
)
if (!nzchar(utils_file) || !file.exists(utils_file)) {
  utils_file <- testthat::test_path(
    "..",
    "..",
    "inst",
    "viewer",
    "utility_functions.R"
  )
}
skip_if_not(file.exists(utils_file), "utility_functions.R not found")

utils_env <- new.env()
source(utils_file, local = utils_env)
prettifyTable <- utils_env$prettifyTable
centerOfGroups <- utils_env$centerOfGroups
cachePlot <- utils_env$cachePlot
dynamicPointSize <- utils_env$dynamicPointSize
nProjectionDimensions <- utils_env$nProjectionDimensions
capProjectionDimensions <- utils_env$capProjectionDimensions
configuredViewerContent <- utils_env$configuredViewerContent
configuredViewerPercentageCellsToShow <-
  utils_env$configuredViewerPercentageCellsToShow

test_that("projection hover info accepts standard Seurat QC columns", {
  utils_env$getGroups <- function() "sample"
  metadata <- data.frame(
    cell_barcode = c("cell-1", "cell-2"),
    nCount_RNA = c(1234, 567),
    nFeature_RNA = c(321, 210),
    sample = c("donorA", "donorB")
  )

  hover <- utils_env$buildHoverInfoForProjections(metadata)

  expect_length(hover, 2L)
  expect_match(hover[[1]], "Transcripts</b>: 1,234", fixed = TRUE)
  expect_match(hover[[1]], "Expressed genes</b>: 321", fixed = TRUE)
  expect_match(hover[[2]], "sample</b>: donorB", fixed = TRUE)
})

## ---------------------------------------------------------------------------
## centerOfGroups
## ---------------------------------------------------------------------------

test_that("centerOfGroups computes 2D medians per group", {
  result <- centerOfGroups(
    coordinates = list(c(0, 10, 2), c(0, 10, 12)),
    df = data.frame(grp = c("A", "A", "B")),
    n_dimensions = 2,
    group = "grp"
  )
  result <- as.data.frame(result)
  expect_setequal(result$group, c("A", "B"))
  expect_equal(result$x_median[result$group == "A"], 5)
  expect_equal(result$y_median[result$group == "A"], 5)
  expect_equal(result$x_median[result$group == "B"], 2)
  expect_equal(result$y_median[result$group == "B"], 12)
})

test_that("centerOfGroups returns a typed empty tibble for a missing group column", {
  result <- centerOfGroups(
    coordinates = matrix(c(1, 2, 3, 4), ncol = 2),
    df = data.frame(cluster = c("a", "b")),
    n_dimensions = 2,
    group = "does_not_exist"
  )
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("group", "x_median", "y_median", "z_median") %in% colnames(result)
  ))
})

test_that("centerOfGroups returns a typed empty tibble for a NULL group", {
  result <- centerOfGroups(
    coordinates = matrix(c(1, 2, 3, 4), ncol = 2),
    df = data.frame(cluster = c("a", "b")),
    n_dimensions = 2,
    group = NULL
  )
  expect_equal(nrow(result), 0)
})

## ---------------------------------------------------------------------------
## prettifyTable edge cases
## ---------------------------------------------------------------------------

test_that("prettifyTable does not crash on an all-NA percentage column", {
  ## Old code did `if (max(col > 1))`, which returned NA for an all-NA column
  ## and threw "missing value where TRUE/FALSE needed".
  table <- data.frame(
    gene = c("g1", "g2"),
    percent_mt = c(NA_real_, NA_real_)
  )
  expect_no_error(
    prettifyTable(
      table,
      filter = "none",
      dom = "t",
      number_formatting = TRUE,
      columns_percentage = 2
    )
  )
})

test_that("prettifyTable still rescales a 0-100 percentage column to 0-1", {
  table <- data.frame(
    gene = c("g1", "g2", "g3"),
    percent_mt = c(50, NA_real_, 20)
  )
  widget <- prettifyTable(
    table,
    filter = "none",
    dom = "t",
    number_formatting = TRUE,
    columns_percentage = 2
  )
  ## The rescaled values live in the widget's data payload.
  rescaled <- widget$x$data$percent_mt
  expect_equal(rescaled[!is.na(rescaled)], c(0.5, 0.2))
})

test_that("prettifyTable tolerates NA / NULL toggle inputs", {
  table <- data.frame(
    gene = c("g1", "g2"),
    percent_mt = c(10, 20)
  )
  ## materialSwitch can transiently pass NA / NULL during UI re-render.
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", number_formatting = NA)
  )
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", show_buttons = NULL)
  )
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", hide_long_columns = NA)
  )
})

## ---------------------------------------------------------------------------
## cachePlot: the shared bindCache wrapper used by the plot renderers.
##
## Drives a minimal server that caches a counting reactive through cachePlot,
## then asserts the caching contract: the reactive evaluates, an unchanged key
## does not recompute, a changed plot-specific key invalidates the cache, and a
## changed dataset key invalidates the cache. The last case would regress if
## the dataset key were forwarded as an already-evaluated value instead of an
## unevaluated expression, so this also guards the wrapper's cache-key scoping.
## ---------------------------------------------------------------------------

test_that("cachePlot caches by key and invalidates on key or dataset change", {
  skip_if_not_installed("shiny", "1.6.0")

  compute_count <- 0

  server <- function(input, output, session) {
    available_crb_files <- shiny::reactiveValues(selected = "datasetA")
    cached <- shiny::reactive({
      compute_count <<- compute_count + 1
      paste(input$metric, available_crb_files$selected)
    }) %>%
      cachePlot(input$metric, available_crb_files$selected)
    output$val <- shiny::renderText(cached())
  }

  shiny::testServer(server, {
    ## 1. evaluates successfully
    session$setInputs(metric = "nUMI")
    expect_equal(cached(), "nUMI datasetA")
    first <- compute_count
    expect_equal(first, 1)

    ## 2. unchanged keys do not recompute
    cached()
    expect_equal(compute_count, first)

    ## 3. changing a plot-specific key invalidates the cache
    session$setInputs(metric = "nGene")
    expect_equal(cached(), "nGene datasetA")
    expect_equal(compute_count, first + 1)

    ## returning to a previously cached key hits the cache
    session$setInputs(metric = "nUMI")
    cached()
    expect_equal(compute_count, first + 1)

    ## 4. changing the dataset key invalidates the cache
    available_crb_files$selected <- "datasetB"
    session$flushReact()
    expect_equal(cached(), "nUMI datasetB")
    expect_equal(compute_count, first + 2)
  })
})

## ---------------------------------------------------------------------------
## dynamicPointSize: default marker size from point count (+ optional canvas)
## ---------------------------------------------------------------------------

test_that("dynamicPointSize shrinks as the point count grows", {
  ## More points -> smaller default, monotonically non-increasing.
  sizes <- vapply(
    c(100, 1000, 10000, 100000, 1e6),
    function(n) dynamicPointSize(n),
    numeric(1)
  )
  expect_true(all(diff(sizes) <= 0))
  ## A small dataset should be clearly larger than a huge one.
  expect_gt(dynamicPointSize(100), dynamicPointSize(200000))
})

test_that("dynamicPointSize stays within [min, max] and snaps to step", {
  vals <- vapply(
    c(1, 10, 500, 5000, 5e5, 1e7),
    function(n) dynamicPointSize(n, min = 1, max = 20, step = 1),
    numeric(1)
  )
  expect_true(all(vals >= 1 & vals <= 20))
  expect_true(all(vals == round(vals))) # step = 1 -> integers
})

test_that("dynamicPointSize returns the fallback for missing/invalid counts", {
  expect_equal(dynamicPointSize(NULL, fallback = 3), 3)
  expect_equal(dynamicPointSize(NA, fallback = 3), 3)
  expect_equal(dynamicPointSize(0, fallback = 3), 3)
  expect_equal(dynamicPointSize(-5, fallback = 3), 3)
})

test_that("dynamicPointSize lets a larger canvas carry larger points", {
  small <- dynamicPointSize(5000, plot_width_px = 500, plot_height_px = 400)
  big <- dynamicPointSize(5000, plot_width_px = 1600, plot_height_px = 1100)
  expect_gte(big, small)
  ## The canvas correction only nudges — it never flips the point-count order.
  expect_gt(
    dynamicPointSize(200, plot_width_px = 500, plot_height_px = 400),
    dynamicPointSize(100000, plot_width_px = 1600, plot_height_px = 1100)
  )
})

## ---------------------------------------------------------------------------
## Projection dimension cap
## ---------------------------------------------------------------------------
## The projection plots dispatch on 2 vs. 3 dimensions. A projection wider than
## three matched neither branch, so no plot update was ever requested and the
## previously drawn projection stayed on screen. `addProjection()` accepts a
## projection of any width and a PCA normally carries far more than three
## components, so the width has to be capped before it reaches the dispatch.

mk_projection <- function(n_dimensions, n_cells = 6) {
  co <- as.data.frame(matrix(
    seq_len(n_cells * n_dimensions),
    nrow = n_cells
  ))
  colnames(co) <- paste0("PC_", seq_len(n_dimensions))
  rownames(co) <- paste0("cell_", seq_len(n_cells))
  co
}

test_that("nProjectionDimensions caps anything wider than three", {
  expect_equal(nProjectionDimensions(mk_projection(2)), 2)
  expect_equal(nProjectionDimensions(mk_projection(3)), 3)
  expect_equal(nProjectionDimensions(mk_projection(5)), 3)
  expect_equal(nProjectionDimensions(mk_projection(50)), 3)
})

test_that("capProjectionDimensions keeps the leading three dimensions", {
  capped <- capProjectionDimensions(mk_projection(5))
  expect_equal(ncol(capped), 3)
  expect_equal(colnames(capped), c("PC_1", "PC_2", "PC_3"))
  expect_equal(capped[["PC_3"]], mk_projection(5)[["PC_3"]])
  expect_equal(rownames(capped), rownames(mk_projection(5)))
})

test_that("capProjectionDimensions leaves 2-D and 3-D projections untouched", {
  for (n_dimensions in c(2, 3)) {
    projection <- mk_projection(n_dimensions)
    expect_equal(capProjectionDimensions(projection), projection)
  }
})

test_that("a capped projection always reaches a dispatch branch", {
  ## Guards the actual failure: the width the parameters report and the width
  ## the coordinates carry both have to land on 2 or 3, or the plot is never
  ## updated.
  for (n_dimensions in c(2, 3, 5, 50)) {
    projection <- mk_projection(n_dimensions)
    reported <- nProjectionDimensions(projection)
    expect_true(reported %in% c(2, 3))
    expect_equal(ncol(capProjectionDimensions(projection)), reported)
  }
})

test_that("the gene-expression projection caps the projection it plots", {
  ## A tab that reports a capped width but still hands over the full-width
  ## coordinates (or the reverse) puts the two back out of step, so pin both
  ## call sites per tab rather than the helper alone.
  for (tab in "gene_expression") {
    parameters <- paste(
      readLines(
        file.path(dirname(utils_file), tab, "obj_projection_parameters_plot.R"),
        warn = FALSE
      ),
      collapse = "\n"
    )
    expect_match(
      parameters,
      "nProjectionDimensions\\(getProjection\\(",
      info = tab
    )
    expect_no_match(parameters, "ncol\\(getProjection\\(", info = tab)

    coordinates <- paste(
      readLines(
        file.path(dirname(utils_file), tab, "obj_projection_coordinates.R"),
        warn = FALSE
      ),
      collapse = "\n"
    )
    expect_match(
      coordinates,
      "capProjectionDimensions\\([\\s\\S]{0,80}getProjection\\(",
      perl = TRUE,
      info = tab
    )
  }
})

test_that("the selected-cell panels carry only the identifier's two columns", {
  ## These join the projection onto the meta data to rebuild the X1-X2 selection
  ## key and then drop those two again, so every further dimension arrived in the
  ## user's table as data: a 50-component PCA contributed PC_3 through PC_50.
  ##
  ## Listed per tab rather than per file: capping the shared coordinates reactive
  ## takes a 50-column projection down to three, which looks like a fix and still
  ## leaves the third column in the table. Only the join sites can close it, and
  ## checking one tab's says nothing about the other's -- which is exactly how
  ## the Gene expression table stayed open after the Overview ones were fixed.
  sites <- list(gene_expression = "UI_table_of_selected_cells.R")
  for (tab in names(sites)) {
    for (f in sites[[tab]]) {
      source_text <- paste(
        readLines(file.path(dirname(utils_file), tab, f), warn = FALSE),
        collapse = "\n"
      )
      expect_match(
        source_text,
        "capProjectionDimensions\\([\\s\\S]{0,140}?,\\s*2\\s*\\)",
        perl = TRUE,
        info = paste(tab, f)
      )
    }
  }
})

test_that("configured Viewer content follows the selected dataset", {
  files <- c(A = "/private/a.crb", B = "/private/b.crb")
  config <- list(
    A = list(
      default_projection = "umap",
      overview_point_size = 4,
      overview_percentage_cells_to_show = 100
    ),
    B = list(
      default_projection = "pca",
      default_trajectory = list(method = "monocle2", name = "lineage"),
      overview_point_size = 8,
      overview_percentage_cells_to_show = 60
    )
  )

  expect_identical(
    configuredViewerContent(config, "/private/b.crb", files),
    config$B
  )
  expect_identical(
    configuredViewerContent(config, "/private/upload.crb", NULL),
    list()
  )
  expect_identical(configuredViewerContent(NULL, files[[1L]], files), list())
})

test_that("configured initial cell percentage is validated for Viewer use", {
  expect_identical(
    configuredViewerPercentageCellsToShow(
      list(overview_percentage_cells_to_show = 60),
      fallback = 100
    ),
    60
  )
  expect_identical(
    configuredViewerPercentageCellsToShow(
      list(overview_percentage_cells_to_show = 0),
      fallback = 100
    ),
    100
  )
  expect_identical(
    configuredViewerPercentageCellsToShow(list(), fallback = 100),
    100
  )
})
