test_that("the default motif size follows the data set, not the filtered cohort", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      ## No Viewer Pack first frame: the default is sized from the segments.
      hla_first_frame <- reactive(NULL)
      ## 600 distinct CDR3s in the data set; a filter leaves 10 of them.
      hla_unfiltered_segments <- reactive({
        data.frame(cdr3 = paste0("CASS", seq_len(600)))
      })
      hla_segments <- reactive({
        if (isTRUE(input$filtered)) {
          data.frame(cdr3 = paste0("CASS", seq_len(10)))
        } else {
          hla_unfiltered_segments()
        }
      })
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        scope,
        names = "hla_default_min_nodes"
      )
      runs <- 0L
      observe({
        hla_default_min_nodes()
        runs <<- runs + 1L
      })
      session$userData$runs <- function() runs
    },
    {
      session$setInputs(filtered = FALSE)
      expect_identical(hla_default_min_nodes(), 4L)
      before <- session$userData$runs()
      ## A filter must neither change the default nor invalidate it: the
      ## slider is built from it, so either would reset the chosen size.
      session$setInputs(filtered = TRUE)
      expect_identical(hla_default_min_nodes(), 4L)
      expect_identical(session$userData$runs(), before)
    }
  )
})
