test_that("dataset loading progress spans server work and browser presentation", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(viewer_test_path("www", "viewer-shell.js"), warn = FALSE),
    collapse = "\n"
  )
  css <- paste(
    readLines(viewer_test_path("www", "custom.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server, '"cerebro_dataset_load"', fixed = TRUE)
  expect_match(
    server,
    'datasetLoadProgress(dataset_label, 8, "Preparing dataset")',
    fixed = TRUE
  )
  expect_match(
    server,
    'datasetLoadProgress(dataset_label, 42, "Opening cells and metadata")',
    fixed = TRUE
  )
  expect_match(
    server,
    'datasetLoadProgress(dataset_label, 100, "Ready", done = TRUE)',
    fixed = TRUE
  )
  expect_match(javascript, 'document.addEventListener("change"', fixed = TRUE)
  expect_match(javascript, 'input.id !== "crb_file_selector"', fixed = TRUE)
  expect_match(
    javascript,
    'window.Shiny.addCustomMessageHandler("cerebro_dataset_load", update)',
    fixed = TRUE
  )
  expect_match(javascript, '".cerebro-dataset-loading-elapsed"', fixed = TRUE)
  expect_match(css, ".cerebro-dataset-loading-card", fixed = TRUE)
  expect_match(css, "place-items: center", fixed = TRUE)
})

test_that("page benchmarks can force and identify full dataset readiness", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    server,
    'input[["cerebro_benchmark_dataset_load_request"]]',
    fixed = TRUE
  )
  expect_match(server, "dataset_load_requested(TRUE)", fixed = TRUE)
  expect_match(server, 'loaded <- data_set()', fixed = TRUE)
  expect_match(server, 'identity <- viewerDatasetIdentity()', fixed = TRUE)
  expect_match(
    server,
    '"cerebro_benchmark_dataset_ready"',
    fixed = TRUE
  )
  expect_match(server, "dataset_fingerprint", fixed = TRUE)
})


test_that("loading labels match selector labels without changing dataset identity", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  files <- c(ds1 = "/private/one.crb", ds2 = "/private/two.crb")
  labels <- c(ds2 = "Tissue atlas", ds1 = "All nuclei")
  expect_identical(runtime$viewerDatasetLabel(files, "ds1", labels), "All nuclei")
  expect_identical(runtime$viewerDatasetLabel(files, "ds2", labels), "Tissue atlas")
  expect_identical(runtime$viewerDatasetLabel(files, "ds2", c("First", "Second")), "Second")
  repeated <- c(ds1 = "Repeated", ds2 = "Repeated")
  expect_identical(runtime$viewerDatasetLabel(files, "ds2", repeated), "Repeated")
  expect_identical(unname(runtime$viewerDatasetChoices(files, repeated)), c("ds1", "ds2"))
  expect_identical(runtime$viewerDatasetPath(files, "ds2"), "/private/two.crb")
  expect_identical(runtime$viewerDatasetLabel(c(Atlas="/private/data.crb"), "Atlas"), "Atlas")
  for(selected in list(NULL, NA_character_, "missing", "__upload_1__")) {
    expect_identical(runtime$viewerDatasetLabel(files, selected, labels), "Dataset")
  }
  expect_identical(runtime$viewerDatasetLabel(NULL, "__single__"), "Dataset")
  expect_identical(runtime$viewerDatasetLabel(unname(files), files[[1]]), "Dataset")
  expect_identical(runtime$viewerDatasetLabel(files, "ds1", c(" ", "Second")), "Dataset")
})


test_that("loading transitions avoid flashes and survive consecutive switches", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_identical(system2("node", c(
    shQuote(testthat::test_path("fixtures", "dataset-loading-transition.js")),
    shQuote(viewer_test_path("www", "viewer-shell.js"))
  )), 0L)
})
