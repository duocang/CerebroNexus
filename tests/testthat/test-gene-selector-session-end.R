test_that("gene selector timers do nothing once their session has closed", {
  skip_if_not_installed("later")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      updates <- 0L
      updateSelectizeInput <- function(...) updates <<- updates + 1L
      data_set <- shiny::reactiveVal("dataset-a")
      serverSideGeneSelector(
        session,
        "gene_input",
        choices = function() c("A", "B"),
        retry = FALSE
      )
    },
    {
      session$flushReact()
      expect_identical(scope$updates, 0L)
      ## The update is scheduled with later(); a session that ends first must
      ## not be touched again, since its reactives can no longer be read.
      session$close()
      later::run_now(1)
      expect_identical(scope$updates, 0L)
    }
  )
})

test_that("an open session still receives the scheduled gene choices", {
  skip_if_not_installed("later")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      updates <- 0L
      updateSelectizeInput <- function(...) updates <<- updates + 1L
      data_set <- shiny::reactiveVal("dataset-a")
      serverSideGeneSelector(
        session,
        "gene_input",
        choices = function() c("A", "B"),
        retry = FALSE
      )
    },
    {
      session$flushReact()
      Sys.sleep(0.4)
      later::run_now(1)
      expect_identical(scope$updates, 1L)
    }
  )
})
