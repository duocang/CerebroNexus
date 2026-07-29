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

test_that("ambiguous examples are independent of assay-cell order", {
  cells <- paste0("c", seq_len(4))
  memberships <- list(
    "data.a" = cells[c(1, 2)],
    "data.z" = cells[c(3, 4)],
    "data.b" = cells[c(1, 3)],
    "data.y" = cells[c(2, 4)],
    "data.c" = cells[c(1, 4)],
    "data.x" = cells[c(2, 3)]
  )

  forward <- .find_layer_partition(cells, memberships)
  reverse <- .find_layer_partition(rev(cells), memberships)

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

test_that("solution caps are finite integers and never pad results", {
  cells <- paste0("c", seq_len(4))
  memberships <- list(
    "data.a" = cells[c(1, 2)],
    "data.z" = cells[c(3, 4)],
    "data.b" = cells[c(1, 3)],
    "data.y" = cells[c(2, 4)],
    "data.c" = cells[c(1, 4)],
    "data.x" = cells[c(2, 3)]
  )

  result <- .find_layer_partition(
    cells,
    memberships,
    max_solutions = 5L
  )
  expect_identical(result$status, "ambiguous")
  expect_length(result$solutions, 3L)
  expect_false(any(vapply(result$solutions, is.null, logical(1))))

  for (invalid in list(Inf, NaN, 2.5, "2", numeric())) {
    expect_error(
      .find_layer_partition(
        cells,
        memberships,
        max_solutions = invalid
      ),
      "finite integer",
      info = paste(invalid, collapse = ", ")
    )
  }
})

test_that("pathological overlap stops at deterministic work budgets", {
  cells <- paste0("c", seq_len(12L))
  memberships <- list(
    "data.a" = cells[1:8],
    "data.b" = cells[5:12],
    "data.c" = cells[c(1:4, 9:12)]
  )

  expect_error(
    .find_layer_partition(
      cells,
      memberships,
      max_conflict_work = 1L
    ),
    "budget"
  )
  expect_error(
    .find_layer_partition(
      cells,
      memberships,
      max_search_nodes = 1L,
      max_conflict_work = 1000L
    ),
    "budget"
  )

  for (argument in c("max_search_nodes", "max_conflict_work")) {
    values <- list(0, Inf, 1.5, "1", numeric())
    for (invalid in values) {
      args <- list(
        assay_cells = cells,
        memberships = memberships
      )
      args[[argument]] <- invalid
      expect_error(
        do.call(.find_layer_partition, args),
        "positive integer",
        info = paste(argument, paste(invalid, collapse = ", "))
      )
    }
  }
})

test_that("deep conflicting partitions stop before exhausting R's call stack", {
  n_layers <- 2000L
  cells <- paste0("c", seq_len(n_layers))
  singleton_partition <- as.list(cells)
  names(singleton_partition) <- paste0(
    "data.sample_",
    seq_len(n_layers)
  )

  ## A large ordinary partition must stay on the non-recursive linear path.
  result <- .find_layer_partition(cells, singleton_partition)
  expect_identical(result$status, "unique")
  expect_setequal(result$layers, names(singleton_partition))

  ## One overlapping prefix candidate activates exact-cover search. The valid
  ## solution is still roughly 2,000 layers deep, so an unguarded recursive
  ## implementation reaches R's call-stack limit before its node budget.
  memberships <- singleton_partition
  memberships[["data.noise"]] <- cells[1:2]
  expect_error(
    .find_layer_partition(cells, memberships),
    "search depth budget exceeded"
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

test_that("overlapping noise stays scalable on a large partition", {
  skip_on_cran()

  n_cells <- 50000L
  n_layers <- 8L
  cells <- paste0("c", seq_len(n_cells))
  groups <- split(
    cells,
    rep(seq_len(n_layers), length.out = n_cells)
  )
  names(groups) <- paste0("data.sample_", seq_len(n_layers))
  groups[["data.corrected"]] <- c(
    utils::head(groups[[1L]], 2500L),
    utils::head(groups[[2L]], 2500L)
  )

  elapsed <- system.time(
    result <- .find_layer_partition(cells, groups)
  )[["elapsed"]]

  expect_identical(result$status, "unique")
  expect_setequal(
    result$layers,
    paste0("data.sample_", seq_len(n_layers))
  )
  expect_lt(elapsed, 15)
})
