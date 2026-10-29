plotting_file <- viewer_test_path("plotting_functions.R")
plotting_env <- new.env(parent = globalenv())
sys.source(plotting_file, envir = plotting_env)

test_that("large violins retain distribution coverage with bounded rows", {
  table <- data.frame(
    group = factor(rep(c("A", "B"), each = 1000L)),
    value = c(rep(0, 999L), 1000, seq_len(1000L) * 2)
  )

  compact <- plotting_env$compactViolinData(
    table,
    metric = "value",
    coloring_variable = "group",
    max_points_per_group = 100L
  )

  expect_equal(as.integer(table(compact$group)), c(100L, 100L))
  expect_equal(levels(compact$group), levels(table$group))
  exact <- attr(compact, "cerebro_violin_summary")$stats
  expect_equal(
    unname(exact$mean),
    unname(vapply(
      split(table$value, table$group),
      mean,
      numeric(1)
    ))
  )
  expect_true(all(compact$value >= min(table$value)))
  expect_true(all(compact$value <= max(table$value)))

  signed <- data.frame(
    group = "signed",
    value = c(rep(-1, 999L), 999)
  )
  signed_compact <- plotting_env$compactViolinData(
    signed,
    metric = "value",
    coloring_variable = "group",
    max_points_per_group = 100L
  )
  expect_equal(
    attr(signed_compact, "cerebro_violin_summary")$stats$mean,
    0,
    tolerance = 1e-12
  )
  expect_true(all(signed_compact$value >= min(signed$value)))
})

test_that("skewed violins keep measurement scale and exact full-data boxes", {
  x <- rep(1:10, 10000)
  x[length(x)] <- 1e9
  table <- data.frame(group = "A", value = x)
  compact <- plotting_env$compactViolinData(table, "value", "group")
  summary <- attr(compact, "cerebro_violin_summary")$stats
  expect_equal(summary$mean, mean(x))
  expect_equal(summary$median, 5.5)
  expect_equal(summary$q3, 8)
  expect_equal(stats::median(compact$value), 5.5)
  expect_lt(max(compact$value), 11)
  expect_equal(summary$maximum, 1e9)
  expect_true(summary$approximate)
  packed <- CerebroNexus:::.viewerPackCompactViolinData(table, "group", "value")
  expect_identical(packed, compact)
  expect_identical(
    plotting_env$compactViolinData(packed, "value", "group"),
    packed
  )
  chart <- plotting_env$cerebroViolinData(
    table,
    "value",
    "group",
    c(A = "blue"),
    "Value",
    "integer"
  )
  ## The box comes from every observation, the density from the compact rows
  ## over the exact range.
  expect_equal(as.numeric(chart$median), 5.5)
  expect_equal(as.numeric(chart$q3), 8)
  expect_equal(as.numeric(chart$mean), mean(x))
  expect_equal(as.numeric(chart$maximum), 1e9)
  expect_equal(chart$violins[[1L]]$lo, 1)
  expect_equal(chart$violins[[1L]]$hi, 1e9)
})

test_that("empty, constant and non-finite violin groups have valid exact summaries", {
  for (x in list(
    rep(0, 5000),
    rep(-3, 5000),
    c(NA, Inf, -Inf),
    c(NA, -1, 0, 1, Inf)
  )) {
    table <- data.frame(group = rep("A", length(x)), value = x)
    compact <- plotting_env$compactViolinData(table, "value", "group")
    summary <- attr(compact, "cerebro_violin_summary")$stats
    finite <- x[is.finite(x)]
    expect_true(all(is.finite(compact$value)))
    if (length(finite)) {
      expect_equal(summary$mean, mean(finite))
      expect_equal(summary$median, stats::median(finite))
      expect_equal(summary$n, length(finite))
    } else {
      expect_equal(nrow(summary), 0)
    }
  }
})
