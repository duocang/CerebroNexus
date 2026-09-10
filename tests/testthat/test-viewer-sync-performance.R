sync_perf_reactive <- function(value) {
  expression <- substitute(value)
  scope <- parent.frame()
  function() eval(expression, envir = scope)
}

run_projection_indices <- function(metadata, filters, percentage) {
  scope <- new.env(parent = globalenv())
  scope$input <- c(
    list(test_percentage_cells_to_show = percentage),
    stats::setNames(
      filters,
      paste0("test_group_filter_", names(filters))
    )
  )
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  scope$getGroups <- function() names(filters)
  scope$getGroupLevels <- function(group) unique(metadata[[group]])
  scope$viewerProjectionCellIndices("test", metadata)
}

test_that("expression rows are fetched once and aligned by cell", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility_env
  )
  expect_true(is.function(utility_env$viewerExpressionValues))

  calls <- 0L
  requested <- NULL
  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionMatrix <- function(cells, genes) {
    calls <<- calls + 1L
    requested <<- list(cells = cells, genes = genes)
    matrix(
      c(1, 2, 3, 4),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(c("A", "B"), c("c2", "c1"))
    )
  }

  values <- utility_env$viewerExpressionValues(
    data_set,
    cells = c("c1", "c2"),
    genes = c("B", "A", "B", "")
  )

  expect_identical(calls, 1L)
  expect_identical(requested$cells, c("c1", "c2"))
  expect_identical(requested$genes, c("B", "A"))
  expect_identical(values, list(B = c(4, 3), A = c(2, 1)))
})

test_that("single-gene expression uses row access with a matrix fallback", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility_env
  )

  row_calls <- 0L
  matrix_calls <- 0L
  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionRow <- function(gene, cells) {
    row_calls <<- row_calls + 1L
    stats::setNames(c(2, 1), c("c2", "c1"))[cells]
  }
  data_set$getExpressionMatrix <- function(cells, genes) {
    matrix_calls <<- matrix_calls + 1L
    matrix(c(2, 1), nrow = 1L, dimnames = list(genes, cells))
  }

  values <- utility_env$viewerExpressionValues(data_set, c("c1", "c2"), "A")
  expect_identical(row_calls, 1L)
  expect_identical(matrix_calls, 0L)
  expect_identical(values, list(A = c(1, 2)))

  data_set$getExpressionRow <- NULL
  values <- utility_env$viewerExpressionValues(data_set, c("c1", "c2"), "A")
  expect_identical(matrix_calls, 1L)
  expect_identical(values, list(A = c(2, 1)))
})

test_that("already aligned expression cells skip the string match", {
  match_lengths <- integer()
  utility_env <- new.env(parent = globalenv())
  utility_env$match <- function(x, table, ...) {
    match_lengths <<- c(match_lengths, length(x))
    base::match(x, table, ...)
  }
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility_env
  )

  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionMatrix <- function(cells, genes) {
    matrix(
      seq_len(length(cells) * length(genes)),
      nrow = length(genes),
      dimnames = list(genes, cells)
    )
  }
  utility_env$viewerExpressionValues(
    data_set,
    cells = c("c1", "c2", "c3"),
    genes = c("A", "B")
  )

  expect_false(3L %in% match_lengths)
})

test_that("projection filtering and sampling preserve original row indices", {
  metadata <- data.frame(
    cell_barcode = paste0("cell", seq_len(7L)),
    batch = factor(
      c("drop", "keep", "drop", "keep", "keep", "drop", "keep")
    ),
    state = factor(c("T", "T", "T", "B", "T", "B", "B")),
    matrix(seq_len(140L), nrow = 7L),
    check.names = FALSE
  )
  eligible <- c(2L, 4L, 5L, 7L)
  result <- run_projection_indices(
    metadata,
    list(batch = "keep", state = c("T", "B")),
    50
  )
  expect_length(result, ceiling(length(eligible) * 0.5))
  expect_true(all(result %in% eligible))
  expect_identical(anyDuplicated(result), 0L)

  expect_identical(
    run_projection_indices(
      metadata,
      list(batch = c("drop", "keep"), state = c("T", "B")),
      100
    ),
    seq_len(nrow(metadata))
  )

  expect_identical(
    run_projection_indices(
      data.frame(batch = c("keep", NA_character_)),
      list(batch = "keep"),
      100
    ),
    1L
  )
  expect_identical(
    run_projection_indices(metadata, list(batch = character()), 50),
    integer()
  )
})

test_that("projection hover text is assembled in one vectorized pass", {
  utility <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility
  )
  utility$getGroups <- function() c("sample", "cell_type")
  metadata <- data.frame(
    cell_barcode = c("cell1", "cell2"),
    nUMI = c(1200, 34567),
    nGene = c(800, 9012),
    sample = c("A", "B"),
    cell_type = c("T", "B")
  )

  expect_identical(
    as.character(utility$buildHoverInfoForProjections(metadata)),
    c(
      paste0(
        "<b>Cell</b>: cell1<br><b>Transcripts</b>: 1,200",
        "<br><b>Expressed genes</b>: 800",
        "<br><b>sample</b>: A<br><b>cell_type</b>: T"
      ),
      paste0(
        "<b>Cell</b>: cell2<br><b>Transcripts</b>: 34,567",
        "<br><b>Expressed genes</b>: 9,012",
        "<br><b>sample</b>: B<br><b>cell_type</b>: B"
      )
    )
  )
  expect_identical(
    utility$buildHoverInfoForProjections(metadata[FALSE, , drop = FALSE]),
    character()
  )
})

test_that("specialist hover consumers reuse their metadata subset", {
  cases <- list(
    overview = list(
      file = "overview/obj_projection_hover_info.R",
      data = "overview_projection_data",
      hover = "overview_projection_hover_info",
      value = integer()
    ),
    gene = list(
      file = "gene_expression/obj_projection_hover_info.R",
      data = "expression_projection_data",
      hover = "expression_projection_hover_info",
      value = c(5L, 2L)
    ),
    spatial = list(
      file = "spatial/obj_projection_hover_info.R",
      data = "spatial_projection_metadata",
      hover = "spatial_projection_hover_info",
      value = c(9L, 3L)
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    data_calls <- 0L
    helper_calls <- 0L
    requested <- NULL
    scope <- new.env(parent = globalenv())
    scope$reactive <- sync_perf_reactive
    scope$req <- shiny::req
    scope$preferences <- list(show_hover_info_in_projections = TRUE)
    displayed <- data.frame(
      cell_barcode = sprintf("cell%d", case$value),
      value = case$value
    )
    scope[[case$data]] <- function() {
      data_calls <<- data_calls + 1L
      displayed
    }
    scope$hover_info_projections <- function(cells_df) {
      helper_calls <<- helper_calls + 1L
      requested <<- cells_df
      if (!nrow(cells_df)) {
        return(stats::setNames(character(), character()))
      }
      stats::setNames(
        paste0("hover-", cells_df$value),
        cells_df$cell_barcode
      )
    }

    sys.source(viewer_test_path(case$file), envir = scope)
    result <- scope[[case$hover]]()

    expect_identical(data_calls, 1L, info = name)
    expect_identical(helper_calls, 1L, info = name)
    expect_identical(requested, displayed, info = name)
    expected <- if (length(case$value)) {
      stats::setNames(
        paste0("hover-", case$value),
        paste0("cell", case$value)
      )
    } else {
      stats::setNames(character(), character())
    }
    expect_identical(result, expected, info = name)
  }
})
