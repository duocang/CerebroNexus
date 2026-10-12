builder_profile_source_runtime(globalenv())
builder_repo_source("io.R", local = globalenv())
builder_repo_source("profile.R", local = globalenv())
builder_repo_source("inspect.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())
builder_repo_source("project.R", local = globalenv())
builder_repo_source("state.R", local = globalenv())
builder_repo_source("worker.R", local = globalenv())
builder_repo_source("session.R", local = globalenv())

test_that("multiple large QS2 imports stay asynchronous and memory bounded", {
  skip_if_not(identical(
    Sys.getenv("CEREBRO_BUILDER_LARGE_IMPORT_STRESS"),
    "true"
  ))
  skip_if_not_installed("callr")
  skip_if_not_installed("ps")
  skip_if_not_installed("qs2")
  skip_if_not_installed("SeuratObject")

  payload_mib <- suppressWarnings(as.integer(Sys.getenv(
    "CEREBRO_BUILDER_LARGE_IMPORT_MIB",
    "64"
  )))
  if (is.na(payload_mib) || payload_mib < 1L) {
    payload_mib <- 64L
  }
  root <- withr::local_tempdir()
  source <- file.path(root, "large-source.qs2")
  object <- SeuratObject::pbmc_small
  set.seed(20260921L)
  object@misc$builder_large_import_stress <- sample(
    as.raw(0:255),
    payload_mib * 1024^2,
    replace = TRUE
  )
  qs2::qs_save(object, source)
  rm(object)
  gc(FALSE)
  expect_gt(file.info(source)$size, payload_mib * 1024^2 * 0.8)

  paths <- file.path(root, sprintf("large-%02d.qs2", seq_len(8L)))
  expect_true(all(file.copy(source, paths)))
  invalid <- file.path(root, "invalid.qs2")
  writeBin(as.raw(seq_len(255L)), invalid)

  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = file.path(root, "snapshots"),
    snapshot_registry = list()
  )
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))
  baseline_rss <- unname(
    ps::ps_memory_info(worker$process$as_ps_handle())[["rss"]]
  )

  wait_for_result <- function(worker, timeout = 120) {
    deadline <- Sys.time() + timeout
    heartbeat <- 0L
    repeat {
      heartbeat <- heartbeat + 1L
      polled <- builder_session_poll(worker, timeout = 10)
      worker <- polled$worker
      if (!is.null(polled$result)) {
        return(list(
          worker = worker,
          result = polled$result$value,
          heartbeat = heartbeat
        ))
      }
      if (Sys.time() >= deadline) {
        fail("Timed out waiting for a large QS2 import.")
      }
    }
  }

  peak_rss <- 0
  imported <- character()
  marker_root <- file.path(root, "transport-markers")
  dir.create(marker_root)
  for (index in seq_along(paths)) {
    id <- paste0("large-", index)
    retained_path <- file.path(root, "retained", id, basename(paths[[index]]))
    retained_marker <- file.path(marker_root, paste0(id, ".rds"))
    transport <- list(
      source = paths[[index]],
      retained_path = retained_path,
      transport_retained_path = retained_marker
    )
    dispatch_elapsed <- system.time(
      builder_session_load(
        worker,
        id,
        transport,
        .importer = builder_project_load_retained_source,
        .copy = builder_project_copy_file,
        .transport_writer = builder_project_transport_retained_write
      )
    )[["elapsed"]]
    expect_lt(dispatch_elapsed, 2)
    loaded <- wait_for_result(worker)
    worker <- loaded$worker
    expect_gt(loaded$heartbeat, 0L)
    expect_null(loaded$result$error)
    snapshot <- loaded$result$snapshot
    expect_true(
      .builder_snapshot_owned(snapshot),
      info = paste(capture.output(str(snapshot)), collapse = "\n")
    )
    expect_true(file.exists(retained_path))
    retained <- builder_project_transport_retained_read(retained_marker, id)
    expect_identical(
      retained$retained_path,
      normalizePath(retained_path, winslash = "/", mustWork = TRUE)
    )
    worker <- builder_worker_register_snapshot(
      worker,
      id,
      snapshot
    )
    imported <- c(imported, id)
    residency <- worker$process$run(function() {
      list(
        objects = ls(get(".builder_objects", envir = globalenv())),
        snapshots = ls(get(".builder_snapshots", envir = globalenv()))
      )
    })
    expect_identical(residency$objects, id)
    expect_setequal(residency$snapshots, imported)
    peak_rss <- max(
      peak_rss,
      unname(ps::ps_memory_info(worker$process$as_ps_handle())[["rss"]])
    )

    if (index == 3L) {
      builder_session_load(worker, "invalid", invalid)
      failed <- wait_for_result(worker)
      worker <- failed$worker
      expect_match(
        failed$result$error,
        "read|QS|deserialize",
        ignore.case = TRUE
      )
    }
  }

  expect_lte(
    peak_rss - baseline_rss,
    payload_mib * 1024^2 * 4 + 128 * 1024^2
  )
  expect_equal(builder_worker_cell_count(worker, imported[[1L]]), 80L)
  resident <- worker$process$run(function() {
    ls(get(".builder_objects", envir = globalenv()))
  })
  expect_identical(resident, imported[[1L]])
})
