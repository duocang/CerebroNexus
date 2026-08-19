builder_dataset_switch_asset <- function(...) {
  path <- builder_profile_inst_path("builder", ...)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

builder_dataset_switch_runtime <- local({
  runtime <- NULL
  function() {
    if (!is.null(runtime)) {
      return(runtime)
    }
    runtime <<- new.env(parent = globalenv())
    withr::local_dir(builder_profile_inst_path("builder"))
    sys.source("app.R", envir = runtime)
    runtime
  }
})

test_that("Spatial preview cache keys separate datasets and sections", {
  runtime <- builder_dataset_switch_runtime()

  expect_identical(
    runtime$builder_spatial_preview_cache_key("dataset-a", "section-1"),
    "dataset-a::section-1"
  )
  expect_false(identical(
    runtime$builder_spatial_preview_cache_key("dataset-a", "section-1"),
    runtime$builder_spatial_preview_cache_key("dataset-a", "section-2")
  ))
  expect_error(
    runtime$builder_spatial_preview_cache_key("dataset-a", "")
  )
})

test_that("stale Spatial results cannot replace or clear a newer contract", {
  runtime <- builder_dataset_switch_runtime()
  key <- runtime$builder_spatial_preview_cache_key("dataset-a", "section-1")
  old_contract <- list(dataset = "dataset-a", group = "old")
  new_contract <- list(dataset = "dataset-a", group = "new")
  cache <- runtime$builder_spatial_preview_cache_begin(
    list(),
    key,
    new_contract
  )

  expect_identical(
    runtime$builder_spatial_preview_cache_store_if_match(
      cache,
      key,
      old_contract,
      list(value = "stale")
    ),
    cache
  )
  expect_identical(
    runtime$builder_spatial_preview_cache_drop_if_match(
      cache,
      key,
      old_contract
    ),
    cache
  )

  stored <- runtime$builder_spatial_preview_cache_store_if_match(
    cache,
    key,
    new_contract,
    list(value = "current")
  )
  expect_identical(stored[[key]]$status, "ready")
  expect_identical(stored[[key]]$frames$value, "current")
  expect_null(runtime$builder_spatial_preview_cache_drop_if_match(
    stored,
    key,
    new_contract
  )[[key]])
})

test_that("all Spatial worker failures release only their matching switch", {
  runtime <- builder_dataset_switch_runtime()
  key <- runtime$builder_spatial_preview_cache_key("dataset-a", "section-1")
  current_contract <- list(dataset = "dataset-a", group = "current")
  stale_contract <- list(dataset = "dataset-a", group = "stale")
  cache <- runtime$builder_spatial_preview_cache_begin(
    list(),
    key,
    current_contract
  )

  stale <- runtime$builder_spatial_preview_failure(
    cache,
    list(
      id = "dataset-a",
      section = "section-1",
      preview_cache_key = key,
      preview_contract = stale_contract,
      switch_token = 31
    )
  )
  expect_identical(stale$cache, cache)
  expect_false(stale$matched)
  expect_identical(stale$message$state, "error")
  expect_identical(stale$message$switch_token, 31)

  current <- runtime$builder_spatial_preview_failure(
    cache,
    list(
      id = "dataset-a",
      section = "section-1",
      preview_cache_key = key,
      preview_contract = current_contract,
      switch_token = 32
    )
  )
  expect_null(current$cache[[key]])
  expect_true(current$matched)

  imports <- builder_dataset_switch_asset("server", "imports.R")
  calls <- gregexpr("fail_spatial_preview(p)", imports, fixed = TRUE)[[1L]]
  expect_identical(sum(calls > 0L), 2L)
  foundation <- builder_dataset_switch_asset("server", "foundation.R")
  expect_match(
    foundation,
    "fail_spatial_preview(payload)",
    fixed = TRUE
  )
})

test_that("dataset rail pick events preserve a finite switch token", {
  runtime <- builder_dataset_switch_runtime()
  ids <- c("dataset-a", "dataset-b")

  expect_identical(
    runtime$.builder_rail_pick_event("dataset-a", ids),
    list(id = "dataset-a", switch_token = NULL)
  )
  expect_identical(
    runtime$.builder_rail_pick_event(
      list(id = "dataset-b", switch_token = 17),
      ids
    ),
    list(id = "dataset-b", switch_token = 17)
  )
  expect_null(runtime$.builder_rail_pick_event(
    list(id = "dataset-b", switch_token = Inf),
    ids
  ))
  expect_null(runtime$.builder_rail_pick_event(
    list(id = "missing", switch_token = 17),
    ids
  ))
})

test_that("Spatial preview cache and switch milestones are wired end to end", {
  foundation <- builder_dataset_switch_asset("server", "foundation.R")
  enhancements <- builder_dataset_switch_asset("server", "enhancements.R")
  alignment <- builder_dataset_switch_asset("spatial_alignment_server.R")
  imports <- builder_dataset_switch_asset("server", "imports.R")
  rail <- builder_dataset_switch_asset("ui", "dataset_rail.R")
  build <- builder_dataset_switch_asset("server", "build.R")

  expect_match(
    foundation,
    "spatial_previews <- reactiveVal(list())",
    fixed = TRUE
  )
  expect_match(
    enhancements,
    "spatial_previews = spatial_previews",
    fixed = TRUE
  )
  expect_match(alignment, "builder_spatial_preview_cache_hit(", fixed = TRUE)
  expect_match(alignment, "preview_cache_key", fixed = TRUE)
  expect_match(alignment, "active_switch_token", fixed = TRUE)
  expect_match(alignment, '"builder_dataset_switch_state"', fixed = TRUE)
  expect_match(
    imports,
    "builder_spatial_preview_cache_store_if_match(",
    fixed = TRUE
  )
  expect_match(alignment, "payload$switch_token", fixed = TRUE)
  expect_match(rail, ".builder_rail_pick_event", fixed = TRUE)
  expect_match(build, "switch_token", fixed = TRUE)
})

test_that("a cached Spatial preview settles the matching switch without enqueue", {
  skip_if_not_installed("shiny")
  runtime <- builder_dataset_switch_runtime()
  snapshot <- list(
    path = "/private/dataset-a",
    owner_token = "owner-a",
    object_md5 = strrep("a", 32L)
  )
  entry <- list(
    id = "dataset-a",
    snapshot = snapshot,
    profile = list(images = "section-a", extras = list()),
    settings = list(
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro",
      spatial_coordinate_transforms = list()
    )
  )
  contract <- list(
    dataset = entry$id,
    snapshot_identity = runtime$.builder_worker_identity(snapshot),
    section = "section-a",
    default_projection = "umap",
    group = "cluster",
    assay = NULL,
    layer = "data"
  )
  preview <- list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "section-a", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    capped = FALSE,
    transcriptome = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(-1, 1),
      y = c(-1, 1),
      group = c("A", "B")
    ),
    spatial = data.frame(
      cell_id = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B")
    )
  )
  key <- runtime$builder_spatial_preview_cache_key(entry$id, "section-a")
  cache <- runtime$builder_spatial_preview_cache_store_if_match(
    runtime$builder_spatial_preview_cache_begin(list(), key, contract),
    key,
    contract,
    preview
  )
  current <- shiny::reactiveVal(NULL)
  alignment_preview <- shiny::reactiveVal(NULL)
  spatial_coords <- shiny::reactiveVal(NULL)
  spatial_previews <- shiny::reactiveVal(cache)
  requests <- list()
  messages <- list()

  shiny::testServer(
    function(input, output, session) {
      session$sendCustomMessage <- function(type, message) {
        messages[[length(messages) + 1L]] <<- list(
          type = type,
          message = message
        )
      }
      alignment <- runtime$builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) if (identical(id, entry$id)) entry else NULL,
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) {
          requests[[length(requests) + 1L]] <<- request
          TRUE
        },
        commit_images = function(updated, images) updated,
        alignment_preview = alignment_preview,
        spatial_previews = spatial_previews,
        spatial_coords = spatial_coords
      )
    },
    {
      expect_true(alignment$request_dataset_switch(
        "dataset-a",
        function() {
          current("dataset-a")
          TRUE
        },
        switch_token = 17
      ))
      session$flushReact()

      expect_length(requests, 0L)
      expect_identical(alignment_preview(), preview)
      states <- Filter(
        function(item) identical(item$type, "builder_dataset_switch_state"),
        messages
      )
      expect_true(any(vapply(
        states,
        function(item) {
          identical(item$message$state, "spatial") &&
            identical(item$message$switch_token, 17)
        },
        logical(1)
      )))
      expect_true(any(vapply(
        states,
        function(item) {
          identical(item$message$state, "ready") &&
            identical(item$message$switch_token, 17)
        },
        logical(1)
      )))
    }
  )
})

test_that("Spatial preview requests deduplicate pending work and invalidate by contract", {
  skip_if_not_installed("shiny")
  runtime <- builder_dataset_switch_runtime()
  entry <- shiny::reactiveVal(list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    settings = list(
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro",
      spatial_coordinate_transforms = list()
    )
  ))
  current <- shiny::reactiveVal(NULL)
  alignment_preview <- shiny::reactiveVal(NULL)
  spatial_previews <- shiny::reactiveVal(list())
  requests <- list()

  shiny::testServer(
    function(input, output, session) {
      alignment <- runtime$builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) entry(),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) {
          requests[[length(requests) + 1L]] <<- request
          TRUE
        },
        commit_images = function(updated, images) updated,
        alignment_preview = alignment_preview,
        spatial_previews = spatial_previews,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      alignment$request_dataset_switch(
        "dataset-a",
        function() {
          current("dataset-a")
          TRUE
        },
        switch_token = 21
      )
      session$flushReact()
      expect_length(requests, 1L)
      expect_identical(requests[[1L]]$switch_token, 21)

      session$flushReact()
      expect_length(requests, 1L)

      updated <- entry()
      updated$settings$default_group <- "other"
      entry(updated)
      session$flushReact()
      expect_length(requests, 2L)
      expect_false(identical(
        requests[[1L]]$preview_contract,
        requests[[2L]]$preview_contract
      ))
    }
  )
})

test_that("a matching pending failure settles the newest switch generation", {
  skip_if_not_installed("shiny")
  runtime <- builder_dataset_switch_runtime()
  entry <- list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = "section-a", extras = list()),
    settings = list(
      images = list(),
      default_group = "cluster",
      default_projection = "umap",
      palette = "cerebro",
      spatial_coordinate_transforms = list()
    )
  )
  current <- shiny::reactiveVal(NULL)
  messages <- list()

  shiny::testServer(
    function(input, output, session) {
      session$sendCustomMessage <- function(type, message) {
        messages[[length(messages) + 1L]] <<- list(
          type = type,
          message = message
        )
      }
      alignment <- runtime$builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) entry,
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(updated, images) updated,
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_previews = shiny::reactiveVal(list()),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      alignment$request_dataset_switch(
        "dataset-a",
        function() {
          current("dataset-a")
          TRUE
        },
        switch_token = 41
      )
      session$flushReact()
      alignment$request_dataset_switch(
        "dataset-a",
        function() TRUE,
        switch_token = 42
      )
      alignment$fail_preview_switch("dataset-a", "section-a", 41)

      errors <- Filter(
        function(item) {
          identical(item$type, "builder_dataset_switch_state") &&
            identical(item$message$state, "error")
        },
        messages
      )
      expect_identical(errors[[length(errors)]]$message$switch_token, 42)
    }
  )
})

test_that("dataset switching gives immediate honest browser feedback", {
  js <- builder_dataset_switch_asset("www", "builder.js")
  css <- builder_dataset_switch_asset("www", "builder.components.css")

  expect_match(js, "function beginDatasetSwitch", fixed = TRUE)
  expect_match(js, "function settleDatasetSwitch", fixed = TRUE)
  expect_match(js, 'workbench.setAttribute("aria-busy", "true")', fixed = TRUE)
  expect_match(js, 'return "Switching dataset…";', fixed = TRUE)
  expect_match(js, '"This is taking longer than expected…"', fixed = TRUE)
  expect_match(js, '"builder_dataset_switch_state"', fixed = TRUE)
  expect_match(js, "datasetSwitchState.generation", fixed = TRUE)
  expect_match(js, "datasetSwitchState.removal", fixed = TRUE)
  expect_match(
    js,
    'veil.classList.remove("is-leaving")',
    fixed = TRUE
  )
  expect_match(js, "message.switch_token", fixed = TRUE)
  expect_match(js, "workbench.inert = true", fixed = TRUE)
  expect_match(css, ".builder-dataset-switch-veil", fixed = TRUE)
  expect_match(css, ".builder-dataset-switch-status", fixed = TRUE)
  expect_match(css, "@media (prefers-reduced-motion: reduce)", fixed = TRUE)
})
