builder_example_registry_env <- new.env(parent = globalenv())
sys.source(
  builder_profile_inst_path("builder", "io.R"),
  envir = builder_example_registry_env
)

test_that("the static example directory mirrors the loadable catalog", {
  directory <- builder_example_registry_env$builder_example_directory()
  catalog <- Filter(
    function(record) isTRUE(record$gallery_visible),
    builder_example_registry_env$builder_example_catalog()
  )

  expect_identical(
    vapply(directory, `[[`, character(1), "id"),
    vapply(catalog, `[[`, character(1), "id")
  )
  expect_identical(
    vapply(directory, `[[`, character(1), "label"),
    vapply(catalog, `[[`, character(1), "label")
  )
  expect_identical(
    vapply(directory, `[[`, character(1), "detail"),
    vapply(catalog, `[[`, character(1), "detail")
  )
  expect_true(all(vapply(
    directory,
    function(record) {
      is.character(record$source) &&
        length(record$source) == 1L &&
        nzchar(record$source)
    },
    logical(1)
  )))
})

test_that("rendering the static directory cannot invoke example makers", {
  directory <- builder_example_registry_env$builder_example_directory()
  expect_false(any(vapply(
    directory,
    function(record) {
      is.function(record$make)
    },
    logical(1)
  )))

  app <- readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE)
  pre_server <- app[seq_len(
    grep("server <- function", app, fixed = TRUE)[1L] - 1L
  )]
  expect_false(any(grepl("builder_examples()", pre_server, fixed = TRUE)))
  expect_false(any(grepl("readRDS", pre_server, fixed = TRUE)))
})
