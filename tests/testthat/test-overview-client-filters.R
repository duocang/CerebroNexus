test_that("Projection leaves its group filters to the browser with a Viewer Pack", {
  for (packed in c(TRUE, FALSE)) {
    shiny::testServer(
      function(input, output, session) {
        scope <- environment()
        viewer_runtime_env(scope)
        metadata <- data.frame(
          cluster = factor(c("A", "B", "A", "B")),
          sample = factor(c("s", "s", "s", "s"))
        )
        viewerPackCurrent <- function() {
          if (packed) list(canonical_order = TRUE) else NULL
        }
        viewerProjectionFirstFrameMetadata <- function() metadata
        getGroups <- function() c("cluster", "sample")
        getGroupLevels <- function(group) levels(metadata[[group]])
        viewerMetadataCodesAsset <- function(name, levels) {
          list(url = paste0(name, ".codes.bin"), levels = I(levels))
        }
        current_scatter_percentage <- function() 100
        viewer_loaded_dataset_context <- function() "context"
        sent <- list()
        viewerSendClientFilters <- function(id, descriptors, context) {
          sent[length(sent) + 1L] <<- list(descriptors)
        }
        sys.source(
          viewer_test_path("overview", "obj_projection_cells_to_show.R"),
          envir = scope
        )
        frames <- 0L
        observe({
          overview_projection_cells_to_show()
          frames <<- frames + 1L
        })
        session$userData$state <- function() {
          list(
            frames = frames,
            sent = sent,
            cells = isolate(overview_projection_cells_to_show())
          )
        }
      },
      {
        session$setInputs(
          sidebar = "overview",
          overview_projection_group_filter_cluster = c("A", "B")
        )
        before <- session$userData$state()
        session$setInputs(overview_projection_group_filter_cluster = "B")
        after <- session$userData$state()
        if (packed) {
          ## Every cell stays in the frame, which is not rebuilt.
          expect_identical(after$cells, 1:4)
          expect_identical(after$frames, before$frames)
          filter <- tail(after$sent, 1)[[1]][[1]]
          expect_identical(filter$name, "cluster")
          expect_identical(as.integer(filter$allowed), 1L)
          expect_identical(filter$resource$url, "cluster.codes.bin")
        } else {
          expect_identical(after$cells, c(2L, 4L))
          expect_null(tail(after$sent, 1)[[1]])
        }
      }
    )
  }
})
