builder_spatial_wysiwyg_source <- function(file, local = parent.frame()) {
  path <- builder_spatial_test_inst_path("builder", file)
  sys.source(path, envir = local)
}

sys.source(
  builder_spatial_test_inst_path(
    "viewer",
    "core",
    "spatial_coordinate_transform.R"
  ),
  envir = environment()
)
builder_spatial_wysiwyg_source("spatial.R")
builder_spatial_wysiwyg_source("preview.R")
builder_spatial_wysiwyg_source("state/core.R")
builder_spatial_wysiwyg_source("extras.R")
builder_spatial_wysiwyg_source("worker.R")
builder_spatial_wysiwyg_source("plan/defaults.R")
builder_spatial_wysiwyg_source("spatial_alignment_server.R")

test_that("persistent spatial scrollbar is inert outside wide layout", {
  css <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "www", "builder.features.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    css,
    "\\.spatial-alignment-persistent-scrollbar \\{ display: none; \\}",
    perl = TRUE
  )
  expect_match(
    css,
    paste0(
      "@media \\(min-width: 81rem\\) \\{[\\s\\S]*",
      "\\.spatial-alignment-persistent-scrollbar \\{[\\s\\S]*",
      "display: block;"
    ),
    perl = TRUE
  )
})

test_that("spatial workbench uses a compact wide-screen gutter", {
  css <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "www", "builder.features.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    css,
    paste0(
      "@media \\(min-width: 81rem\\) \\{[\\s\\S]*",
      "\\.spatial-alignment-layout \\{[\\s\\S]*",
      "column-gap: var\\(--space-3\\);"
    ),
    perl = TRUE
  )
  expect_match(
    css,
    "\\.spatial-alignment-sidebar-body \\{[\\s\\S]*padding-right: var\\(--space-2\\);",
    perl = TRUE
  )
})

test_that("coordinate number fields fit signed decimal values", {
  css <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "www", "builder.features.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    css,
    paste0(
      "\\.spatial-coordinate-control \\{[\\s\\S]*",
      "grid-template-columns: 4rem minmax\\(0, 1fr\\) 5rem;"
    ),
    perl = TRUE
  )
})

test_that("frozen section appearance falls back to image records", {
  record <- builder_alignment_record(
    source = list(name = "histology.png"),
    source_uri = "data:image/png;base64,AA==",
    uri = "data:image/png;base64,AA==",
    base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    parameters = list(point_opacity = 0.42, point_size = 8),
    section = list(id = "fov-a", kind = "spatial")
  )
  expect_identical(
    .builder_plan_spatial_point_appearance(
      stored = list(),
      images = list("fov-a" = list("histology" = record))
    ),
    list("fov-a" = list(point_opacity = 0.42, point_size = 8))
  )
})

test_that("numeric coordinate rotation enters the canonical draft", {
  skip_if_not_installed("shiny")
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "fov-a", extras = list()),
    settings = list(
      name = "Dataset A",
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
  current_entry <- shiny::reactiveVal(entry)
  current <- shiny::reactiveVal(entry$id)

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) current_entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) invisible(updated),
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      expect_true(alignment$restore_project_settings("dataset-a"))
      session$flushReact()

      session$setInputs(`enhance-coordinate_rotation_number` = 37.5)
      session$flushReact()

      expect_identical(
        alignment$coordinate_drafts()[["dataset-a"]][["fov-a"]]$spec,
        list(schema_version = 1L, rotation_degrees = 37.5, scale = 1)
      )
    }
  )
})
