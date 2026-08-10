loading_path <- builder_profile_inst_path("builder", "loading.R")
if (file.exists(loading_path)) {
  sys.source(loading_path, envir = globalenv())
}

builder_loading_call <- function(name, ...) {
  fun <- get0(name, mode = "function", inherits = TRUE)
  if (is.null(fun)) {
    return(NULL)
  }
  fun(...)
}

test_that("new imports enter the typed queue without loading data", {
  source <- list(
    kind = "file",
    staged_path = "/private/session/upload.rds",
    fingerprint = "20:fixture",
    display_name = "upload.rds"
  )
  entry <- builder_loading_call(
    "builder_import_entry",
    id = "ds1",
    label = "upload",
    source = source,
    filename = "upload.rds",
    file_type = "RDS",
    size = 20
  )

  expect_s3_class(entry, "builder_import_entry")
  expect_identical(entry$load_state, "queued")
  expect_identical(entry$progress_label, "Waiting to load")
  expect_identical(entry$generation, 1L)
  expect_null(entry$error)
  expect_null(entry$profile)
  expect_null(entry$settings)
})

test_that("failed examples stay represented only by their rail card", {
  ready <- list(list(example = "all_content"))
  queue <- builder_import_queue(max_active = 2L)
  queue <- builder_import_add(
    queue,
    builder_import_entry(
      "ds2",
      "Queued example",
      list(kind = "example", example = "queued_example")
    )
  )
  queue <- builder_import_add(
    queue,
    builder_import_entry(
      "ds3",
      "Failed example",
      list(kind = "example", example = "failed_example")
    )
  )
  queue <- builder_import_transition(queue, "ds2", "reading", 1L)
  queue <- builder_import_transition(queue, "ds3", "reading", 1L)
  queue <- builder_import_transition(
    queue,
    "ds3",
    "error",
    1L,
    error = "Could not read this object."
  )

  directory <- builder_example_directory_state(ready, queue)

  expect_setequal(
    directory$ids,
    c("all_content", "failed_example")
  )
  expect_identical(directory$loading, "queued_example")
})

test_that("ready import target follows watched and current dataset state", {
  expect_identical(
    names(formals(builder_import_ready_target)),
    c("watched", "current_id", "loaded_id")
  )
  expect_identical(
    builder_import_ready_target(
      watched = TRUE,
      current_id = "ds1",
      loaded_id = "ds2"
    ),
    "ds2"
  )
  expect_identical(
    builder_import_ready_target(
      watched = FALSE,
      current_id = "ds1",
      loaded_id = "ds2"
    ),
    "ds1"
  )
  expect_identical(
    builder_import_ready_target(
      watched = FALSE,
      current_id = NULL,
      loaded_id = "ds2"
    ),
    "ds2"
  )
})

test_that("public import errors scrub mounted and network paths", {
  mounted <- builder_import_public_error(
    "Failed to open /Volumes/Lab/patient.rds"
  )
  network <- builder_import_public_error(
    "Failed to open \\\\server\\share\\patient.rds"
  )

  expect_false(grepl("/Volumes", mounted, fixed = TRUE))
  expect_false(grepl("server", network, fixed = TRUE))
  expect_match(mounted, "selected file", fixed = TRUE)
  expect_match(network, "selected file", fixed = TRUE)
})

test_that("imports follow the bounded loading state machine", {
  entry <- builder_loading_call(
    "builder_import_entry",
    "ds1",
    "PBMC",
    list(kind = "example", example = "all_content")
  )
  queue <- builder_loading_call("builder_import_queue", max_active = 1L)
  queue <- builder_loading_call("builder_import_add", queue, entry)

  expect_s3_class(queue, "builder_import_queue")
  expect_identical(builder_import_pending_ids(queue), "ds1")

  for (state in c("reading", "inspecting", "validating", "preparing")) {
    queue <- builder_import_transition(queue, "ds1", state, generation = 1L)
    expect_identical(builder_import_find(queue, "ds1")$load_state, state)
  }
  queue <- builder_import_transition(queue, "ds1", "ready", generation = 1L)
  expect_identical(builder_import_find(queue, "ds1")$load_state, "ready")
  expect_length(builder_import_active_ids(queue), 0L)
})

test_that("invalid transitions fail and stale completions are ignored", {
  queue <- builder_import_queue()
  queue <- builder_import_add(
    queue,
    builder_import_entry(
      "ds1",
      "PBMC",
      list(kind = "example", example = "all_content")
    )
  )

  expect_error(
    builder_import_transition(queue, "ds1", "ready", generation = 1L),
    class = "builder_import_state_error"
  )

  queued <- builder_import_transition(
    queue,
    "ds1",
    "reading",
    generation = 99L
  )
  expect_identical(queued, queue)
  expect_identical(builder_import_find(queued, "ds1")$load_state, "queued")
})

test_that("error retry uses a new generation and cannot be overwritten", {
  queue <- builder_import_queue()
  queue <- builder_import_add(
    queue,
    builder_import_entry(
      "ds1",
      "PBMC",
      list(kind = "example", example = "all_content")
    )
  )
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- builder_import_transition(
    queue,
    "ds1",
    "error",
    1L,
    error = "/private/session/object.rds failed with a stack trace"
  )

  failed <- builder_import_find(queue, "ds1")
  expect_identical(failed$load_state, "error")
  expect_identical(failed$progress_label, "Could not load dataset")
  expect_false(grepl("/private/session", failed$error, fixed = TRUE))

  retried <- builder_import_retry(queue, "ds1")
  expect_identical(builder_import_find(retried, "ds1")$generation, 2L)
  expect_identical(builder_import_find(retried, "ds1")$load_state, "queued")

  stale <- builder_import_transition(
    retried,
    "ds1",
    "ready",
    generation = 1L
  )
  expect_identical(stale, retried)
})

test_that("the queue enforces one active importer and skips removed work", {
  queue <- builder_import_queue(max_active = 1L)
  for (index in seq_len(10L)) {
    queue <- builder_import_add(
      queue,
      builder_import_entry(
        paste0("ds", index),
        paste("Dataset", index),
        list(kind = "file", staged_path = paste0("/private/", index))
      )
    )
  }

  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  expect_identical(builder_import_active_ids(queue), "ds1")
  expect_error(
    builder_import_transition(queue, "ds2", "reading", 1L),
    class = "builder_import_state_error"
  )

  queue <- builder_import_remove(queue, "ds3")
  expect_false("ds3" %in% builder_import_pending_ids(queue))
  expect_identical(
    builder_import_pending_ids(queue),
    paste0("ds", c(1L, 2L, 4L:10L))
  )
})

test_that("legacy ready entries retain the established loaded default", {
  entry <- list(load_state = NULL)
  expect_identical(
    builder_loading_call("builder_import_legacy_state", entry),
    "ready"
  )
})
