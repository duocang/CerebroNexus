## Pure behavioural tests for selecting a real Seurat v5 layer partition.
##
## The helper deliberately accepts plain cell-name vectors. This keeps the
## correctness and scale contract independent of Seurat object construction.

test_that("a normal split is one unique partition", {
  cells <- paste0("c", seq_len(12))
  memberships <- list(
    "data.sample_a" = cells[1:6],
    "data.sample_b" = cells[7:12]
  )

  result <- .find_layer_partition(cells, memberships)

  expect_identical(result$status, "unique")
  expect_setequal(result$layers, names(memberships))
  expect_length(result$solutions, 1L)
})

test_that("full, empty, and overlapping custom layers stay outside the partition", {
  cells <- paste0("c", seq_len(12))
  memberships <- list(
    "data.sample_a" = cells[1:6],
    "data.sample_b" = cells[7:12],
    "data.imputed" = cells,
    "data.corrected" = cells[c(2, 3, 8, 9)],
    "data.empty" = character()
  )

  result <- .find_layer_partition(cells, memberships)

  expect_identical(result$status, "unique")
  expect_setequal(
    result$layers,
    c("data.sample_a", "data.sample_b")
  )
  expect_false(any(
    c(
      "data.imputed",
      "data.corrected",
      "data.empty"
    ) %in%
      result$layers
  ))
})

test_that("an incomplete candidate set has no partition", {
  cells <- paste0("c", seq_len(12))
  memberships <- list(
    "data.sample_a" = cells[1:5],
    "data.sample_b" = cells[7:12]
  )

  result <- .find_layer_partition(cells, memberships)

  expect_identical(result$status, "none")
  expect_identical(result$layers, character())
  expect_length(result$solutions, 0L)
})

test_that("two exact covers are reported as ambiguous", {
  cells <- paste0("c", seq_len(8))
  memberships <- list(
    "data.sample_a" = cells[1:4],
    "data.sample_b" = cells[5:8],
    "data.batch_a" = cells[c(1, 2, 5, 6)],
    "data.batch_b" = cells[c(3, 4, 7, 8)]
  )

  result <- .find_layer_partition(cells, memberships)

  expect_identical(result$status, "ambiguous")
  expect_identical(result$layers, character())
  expect_length(result$solutions, 2L)
  expect_true(any(vapply(
    result$solutions,
    function(x) setequal(x, c("data.sample_a", "data.sample_b")),
    logical(1)
  )))
  expect_true(any(vapply(
    result$solutions,
    function(x) setequal(x, c("data.batch_a", "data.batch_b")),
    logical(1)
  )))
})

test_that("ambiguous solutions are deterministic across candidate order", {
  cells <- paste0("c", seq_len(8))
  memberships <- list(
    "data.sample_a" = cells[1:4],
    "data.sample_b" = cells[5:8],
    "data.batch_a" = cells[c(1, 2, 5, 6)],
    "data.batch_b" = cells[c(3, 4, 7, 8)]
  )

  forward <- .find_layer_partition(cells, memberships)
  reverse <- .find_layer_partition(cells, rev(memberships))

  expect_identical(forward$status, "ambiguous")
  expect_identical(forward, reverse)
})

test_that("partition resolution is independent of candidate order", {
  cells <- paste0("c", seq_len(12))
  memberships <- list(
    "data.sample_a" = cells[1:6],
    "data.sample_b" = cells[7:12],
    "data.corrected" = cells[c(2, 3, 8, 9)]
  )

  forward <- .find_layer_partition(cells, memberships)
  reverse <- .find_layer_partition(cells, rev(memberships))

  expect_identical(forward$status, reverse$status)
  expect_identical(forward$layers, reverse$layers)
  expect_identical(forward$solutions, reverse$solutions)
})

test_that("structurally invalid memberships fail rather than disappear", {
  cells <- paste0("c", seq_len(8))

  expect_error(
    .find_layer_partition(
      c(cells, cells[[1]]),
      list("data.s1" = cells[1:4], "data.s2" = cells[5:8])
    ),
    "assay_cells"
  )
  expect_error(
    .find_layer_partition(
      cells,
      list("data.s1" = c(cells[1:4], "outside"), "data.s2" = cells[5:8])
    ),
    "outside the assay"
  )
  expect_error(
    .find_layer_partition(
      cells,
      setNames(
        list(cells[1:4], cells[5:8]),
        c("data", "data")
      )
    ),
    "layer names"
  )
})

test_that("the normal partition path scales to 50000 cells", {
  skip_on_cran()

  n_cells <- 50000L
  n_layers <- 8L
  cells <- paste0("c", seq_len(n_cells))
  groups <- split(
    cells,
    rep(seq_len(n_layers), length.out = n_cells)
  )
  names(groups) <- paste0("data.sample_", seq_len(n_layers))

  elapsed <- system.time(
    result <- .find_layer_partition(cells, groups)
  )[["elapsed"]]

  expect_identical(result$status, "unique")
  expect_setequal(result$layers, names(groups))
  expect_lt(elapsed, 15)
})
