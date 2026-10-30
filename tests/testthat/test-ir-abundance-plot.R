## The functions of a Viewer file, without its reactives and outputs.
ir_abundance_functions <- function(...) {
  scope <- new.env(parent = globalenv())
  scope$`%>%` <- magrittr::`%>%`
  for (file in list(...)) {
    for (expr in parse(file)) {
      if (
        is.call(expr) &&
          as.character(expr[[1]])[1] %in% c("<-", "=") &&
          is.call(expr[[3]]) &&
          identical(expr[[3]][[1]], as.name("function"))
      ) {
        eval(expr, scope)
      }
    }
  }
  scope
}

ir_abundance_payload <- function(widget) {
  built <- suppressMessages(widget$preRenderHook(widget))
  plotly:::to_JSON(
    built$x[setdiff(names(built$x), c("visdat", "cur_data", "attrs"))]
  )
}

test_that("the Abundance figure is what plotly_build() would make of it", {
  skip_if_not_installed("plotly")
  scope <- ir_abundance_functions(
    viewer_test_path("plotting_functions.R"),
    viewer_test_path("immune_repertoire", "visualizations.R")
  )
  for (samples in c(1L, 2L, 3L, 40L)) {
    counts <- do.call(
      rbind,
      lapply(seq_len(samples), function(index) {
        abundance <- c(1L, 2L, 5L, 9L, 30L) * index
        data.frame(
          sample = paste0("s", index),
          abundance = abundance,
          n_clones = rev(seq_along(abundance)) * 7L
        )
      })
    )
    for (scale in c(FALSE, TRUE)) {
      for (order_by in list(NULL, "alphanumeric")) {
        args <- list(counts, scale = scale, order_by = order_by)
        expect_false(
          grepl(
            "markers",
            ir_abundance_payload(do.call(
              scope$ir_clonal_abundance_plotly,
              args
            )),
            fixed = TRUE
          ),
          info = "lines only"
        )
        expect_identical(
          as.character(ir_abundance_payload(do.call(
            scope$ir_clonal_abundance_plotly,
            c(args, prebuild = TRUE)
          ))),
          as.character(ir_abundance_payload(do.call(
            scope$ir_clonal_abundance_plotly,
            c(args, prebuild = FALSE)
          ))),
          info = paste(samples, "samples, scale", scale, "order", order_by)
        )
      }
    }
  }
})

test_that("a prebuilt figure is not built again by the renderer", {
  skip_if_not_installed("plotly")
  scope <- ir_abundance_functions(viewer_test_path("plotting_functions.R"))
  figure <- plotly::plot_ly()
  figure$x$attrs <- list()
  figure$x$data <- list(list(
    type = "scatter",
    mode = "lines",
    x = 1:3,
    y = 3:1,
    marker = list(color = "#66C2A5")
  ))
  built <- scope$cerebro_plotly_prebuilt(figure)
  expect_identical(built$preRenderHook(built), built)
  expect_identical(built$x$data[[1L]]$mode, "lines+markers")
})
