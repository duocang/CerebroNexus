test_that("hidden Linked selection UI drops selection and dataset dependencies", {
  target <- viewer_r_assignment(
    viewer_test_path("coordinated_views", "server.R"),
    "coordviews_selected_cells_ui"
  )
  shiny::testServer(
    function(input, output, session) {
      renderUI <- shiny::renderUI
      req <- shiny::req
      work <- new.env(parent = emptyenv())
      work$selection_reads <- work$metadata_reads <- 0L
      coordviews_selection_present <- shiny::reactive({
        input$generation
        work$selection_reads <- work$selection_reads + 1L
        FALSE
      })
      getMetaData <- viewerProjectionFirstFrameMetadata <- function() {
        work$metadata_reads <- work$metadata_reads + 1L
        stop("Empty or hidden selections must not prepare metadata")
      }
      eval(target, environment())
      output$coordviews_selected_cells_UI <- coordviews_selected_cells_ui
      shiny::outputOptions(
        output,
        "coordviews_selected_cells_UI",
        suspendWhenHidden = FALSE
      )
    },
    {
      session$setInputs(sidebar = "coordinated_views", generation = 1L)
      expect_error(
        output$coordviews_selected_cells_UI,
        class = "shiny.silent.error"
      )
      expect_identical(work$selection_reads, 1L)
      session$setInputs(sidebar = "geneExpression")
      expect_error(
        output$coordviews_selected_cells_UI,
        class = "shiny.silent.error"
      )
      session$setInputs(generation = 2L)
      expect_error(
        output$coordviews_selected_cells_UI,
        class = "shiny.silent.error"
      )
      expect_identical(work$selection_reads, 1L)
      expect_identical(work$metadata_reads, 0L)
      session$setInputs(sidebar = "coordinated_views")
      expect_error(
        output$coordviews_selected_cells_UI,
        class = "shiny.silent.error"
      )
      expect_identical(work$selection_reads, 2L)
    }
  )
})
