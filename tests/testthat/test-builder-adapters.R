builder_repo_source("io.R", local = globalenv())

.builder_adapter_source_contracts <- function() {
  builder_profile_source_runtime(local = globalenv())
  builder_repo_source("inspect.R", local = globalenv())
  builder_repo_source("adapters.R", local = globalenv())
}

.builder_adapter_source_contracts()

.builder_normalize_profile_sources <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  value_names <- names(value)
  for (index in seq_along(value)) {
    if (
      length(value_names) >= index &&
        identical(value_names[[index]], "source")
    ) {
      value[index] <- list(list(normalized = TRUE))
    } else {
      value[index] <- list(.builder_normalize_profile_sources(value[[index]]))
    }
  }
  value
}

test_that("file and example adapters converge after loading", {
  object <- SeuratObject::pbmc_small
  path <- file.path(withr::local_tempdir(), "pbmc.rds")
  saveRDS(object, path)

  from_file <- builder_adapter_inspect(
    builder_seurat_file_adapter(path)
  )
  from_example <- builder_adapter_inspect(
    builder_example_adapter("pbmc_small", object)
  )

  expect_s4_class(from_file$object, "Seurat")
  expect_s4_class(from_example$object, "Seurat")
  expect_s3_class(from_file$profile, "builder_dataset_profile")
  expect_identical(from_file$profile$schema_version, 2L)
  expect_identical(
    .builder_normalize_profile_sources(from_file$profile),
    .builder_normalize_profile_sources(from_example$profile)
  )
  expect_identical(from_file$legacy_profile, from_example$legacy_profile)
  expect_identical(from_file$levels, from_example$levels)
  expect_identical(from_file$format, "RDS")
  expect_identical(from_example$format, "Built-in example")
  expect_identical(from_file$source$type, "file")
  expect_identical(from_example$source$type, "example")
})

test_that("file adapters clear a materialized stale cache without executing it", {
  object <- SeuratObject::pbmc_small
  sentinel <- file.path(withr::local_tempdir(), "sentinel")
  cache <- data.frame(
    layer = "counts",
    path = tempfile(),
    class = "matrix",
    pkg = "base",
    fxn = paste0(
      "function(x) { file.create(",
      dQuote(sentinel),
      "); matrix(0, 1, 1) }"
    ),
    assay = "RNA",
    stringsAsFactors = FALSE
  )
  object@tools[["SaveSeuratRds"]] <- cache
  path <- file.path(withr::local_tempdir(), "cached.rds")
  saveRDS(object, path)

  inspected <- builder_adapter_inspect(builder_seurat_file_adapter(path))
  expect_s4_class(inspected$object, "Seurat")
  expect_null(.builder_saved_cache(inspected$object))
  expect_false(file.exists(sentinel))
})

test_that("example adapters use the same safe cache cleanup path", {
  object <- SeuratObject::pbmc_small
  sentinel <- file.path(withr::local_tempdir(), "example-sentinel")
  object@tools[["SaveSeuratRds"]] <- data.frame(
    layer = "counts",
    path = tempfile(),
    class = "matrix",
    pkg = "base",
    fxn = paste0(
      "function(x) { file.create(",
      dQuote(sentinel),
      "); matrix(0, 1, 1) }"
    ),
    assay = "RNA",
    stringsAsFactors = FALSE
  )

  inspected <- builder_adapter_inspect(
    builder_example_adapter("cached-example", object)
  )
  expect_null(.builder_saved_cache(inspected$object))
  expect_false(file.exists(sentinel))
})

test_that("file adapters reject an incomplete cache stub without executing it", {
  skip_if_not_installed("BPCells")
  root <- withr::local_tempdir()
  source <- file.path(root, "bp")
  suppressWarnings(BPCells::write_matrix_dir(
    methods::as(
      methods::as(builder_profile_matrix(paste0("c", 1:6)), "CsparseMatrix"),
      "IterableMatrix"
    ),
    dir = source
  ))
  object <- SeuratObject::CreateSeuratObject(
    counts = BPCells::open_matrix_dir(source)
  )
  stub_path <- file.path(root, "stub.rds")
  SaveSeuratRds <- SeuratObject::SaveSeuratRds
  suppressWarnings(SaveSeuratRds(object, stub_path, move = FALSE))
  sentinel <- file.path(root, "sentinel")
  stub <- readRDS(stub_path)
  stub@tools$SaveSeuratRds$fxn <- paste0(
    "function(x) { file.create(",
    dQuote(sentinel),
    "); matrix(0, 1, 1) }"
  )
  saveRDS(stub, stub_path)

  expect_error(
    builder_adapter_inspect(builder_seurat_file_adapter(stub_path)),
    "incomplete SaveSeuratRds cache stub"
  )
  expect_false(file.exists(sentinel))
})

test_that("loaded BPCells objects can be saved, adapted and snapshotted", {
  skip_if_not_installed("BPCells")
  root <- withr::local_tempdir()
  source <- file.path(root, "bp")
  matrix <- builder_profile_matrix(paste0("c", 1:6))
  suppressWarnings(BPCells::write_matrix_dir(
    methods::as(methods::as(matrix, "CsparseMatrix"), "IterableMatrix"),
    dir = source
  ))
  object <- SeuratObject::CreateSeuratObject(
    counts = BPCells::open_matrix_dir(source)
  )
  seurat_path <- file.path(root, "saved.rds")
  SaveSeuratRds <- SeuratObject::SaveSeuratRds
  suppressWarnings(SaveSeuratRds(object, seurat_path, move = FALSE))
  loaded <- SeuratObject::LoadSeuratRds(seurat_path)
  saveRDS(loaded, seurat_path)

  inspected <- builder_adapter_inspect(
    builder_seurat_file_adapter(seurat_path)
  )
  expect_null(.builder_saved_cache(inspected$object))
  snapshot <- builder_snapshot_seurat(
    inspected$object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  expect_s4_class(builder_open_snapshot(snapshot), "Seurat")
})

test_that("adapter inputs fail closed", {
  expect_error(builder_seurat_file_adapter(tempfile()), "does not exist")
  expect_error(builder_seurat_file_adapter(tempdir()), "regular file")
  expect_error(builder_example_adapter("", SeuratObject::pbmc_small), "id")
  expect_error(builder_example_adapter("bad", list()), "Seurat")
  expect_error(builder_adapter_inspect(list()), "adapter")
})

test_that("file adapters reject sources replaced after adapter creation", {
  root <- withr::local_tempdir()
  path <- file.path(root, "source.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  adapter <- builder_seurat_file_adapter(path)
  writeBin(charToRaw("replacement"), path)

  expect_error(builder_adapter_inspect(adapter), "changed since")
})

test_that("file adapters reject sources changed while they are read", {
  root <- withr::local_tempdir()
  path <- file.path(root, "source.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  adapter <- builder_seurat_file_adapter(path)
  adapter_env <- environment(builder_adapter_inspect)
  original <- .builder_adapter_after_read
  assign(
    ".builder_adapter_after_read",
    function(adapter) writeBin(charToRaw("changed"), adapter$location),
    envir = adapter_env
  )
  on.exit(
    assign(".builder_adapter_after_read", original, envir = adapter_env),
    add = TRUE
  )

  expect_error(builder_adapter_inspect(adapter), "changed while")
})

test_that("adapter and snapshot public interfaces stay narrow", {
  expect_identical(names(formals(builder_seurat_file_adapter)), "path")
  expect_identical(names(formals(builder_example_adapter)), c("id", "object"))
  expect_identical(names(formals(builder_adapter_inspect)), "adapter")
  expect_identical(
    names(formals(builder_snapshot_seurat)),
    c("object", "snapshot_dir", "available_bytes")
  )
  expect_identical(names(formals(builder_open_snapshot)), "snapshot")
})

test_that("registration publishes a snapshot before exposing an object", {
  root <- withr::local_tempdir()
  path <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  objects <- new.env(parent = emptyenv())
  snapshots <- new.env(parent = emptyenv())
  globals <- c(
    ".builder_objects",
    ".builder_snapshots",
    ".builder_snapshot_root"
  )
  prior <- lapply(globals, function(name) {
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      list(present = TRUE, value = get(name, envir = globalenv()))
    } else {
      list(present = FALSE)
    }
  })
  names(prior) <- globals
  on.exit(
    {
      for (name in globals) {
        if (prior[[name]]$present) {
          assign(name, prior[[name]]$value, envir = globalenv())
        } else if (exists(name, envir = globalenv(), inherits = FALSE)) {
          rm(list = name, envir = globalenv())
        }
      }
    },
    add = TRUE
  )
  assign(".builder_objects", objects, envir = globalenv())
  assign(".builder_snapshots", snapshots, envir = globalenv())
  assign(".builder_snapshot_root", root, envir = globalenv())

  result <- .builder_register_adapter(
    builder_seurat_file_adapter(path),
    "ds1"
  )
  unlink(path)

  expect_s3_class(result$dataset_profile, "builder_dataset_profile")
  expect_null(result$dataset_profile$identity$cells$ids)
  expect_true(builder_axis_identity_valid(
    result$dataset_profile$identity$cells$axis_identity
  ))
  expect_s4_class(get("ds1", envir = objects), "Seurat")
  snapshot <- get("ds1", envir = snapshots)
  expect_true(dir.exists(snapshot$path))
  expect_s4_class(builder_open_snapshot(snapshot), "Seurat")
})

test_that("registration reuses an owned snapshot for an unchanged fingerprint", {
  root <- withr::local_tempdir()
  path <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  adapter <- builder_seurat_file_adapter(path)
  objects <- new.env(parent = emptyenv())
  snapshots <- new.env(parent = emptyenv())
  cache <- new.env(parent = emptyenv())
  globals <- c(
    ".builder_objects",
    ".builder_snapshots",
    ".builder_snapshot_cache",
    ".builder_snapshot_root"
  )
  prior <- lapply(globals, function(name) {
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      list(present = TRUE, value = get(name, envir = globalenv()))
    } else {
      list(present = FALSE)
    }
  })
  names(prior) <- globals
  on.exit(
    {
      for (name in globals) {
        if (prior[[name]]$present) {
          assign(name, prior[[name]]$value, envir = globalenv())
        } else if (exists(name, envir = globalenv(), inherits = FALSE)) {
          rm(list = name, envir = globalenv())
        }
      }
    },
    add = TRUE
  )
  assign(".builder_objects", objects, envir = globalenv())
  assign(".builder_snapshots", snapshots, envir = globalenv())
  assign(".builder_snapshot_cache", cache, envir = globalenv())
  assign(".builder_snapshot_root", root, envir = globalenv())

  first <- .builder_register_adapter(adapter, "ds1")
  second <- .builder_register_adapter(builder_seurat_file_adapter(path), "ds2")

  expect_false(first$cache_hit)
  expect_true(second$cache_hit)
  expect_identical(second$snapshot$path, first$snapshot$path)
  expect_s4_class(get("ds2", envir = objects), "Seurat")
})

test_that("file fingerprints change when content changes", {
  root <- withr::local_tempdir()
  path <- file.path(root, "source.rds")
  bytes <- as.raw(rep(1L, 131072L))
  writeBin(bytes, path)
  first <- builder_seurat_file_adapter(path)$fingerprint
  info <- file.info(path)

  bytes[[65537L]] <- as.raw(2L)
  writeBin(bytes, path)
  Sys.setFileTime(path, info$mtime)
  second <- builder_seurat_file_adapter(path)$fingerprint

  expect_false(identical(first, second))
})

test_that("application and worker source adapters after profile contracts", {
  app <- readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE)
  session <- readLines(
    builder_profile_inst_path("builder", "session.R"),
    warn = FALSE
  )
  worker <- readLines(
    builder_profile_inst_path("builder", "worker.R"),
    warn = FALSE
  )
  app_profile <- grep('source("profile.R"', app, fixed = TRUE)[1L]
  app_adapters <- grep('source("adapters.R"', app, fixed = TRUE)[1L]
  worker_profile <- grep(
    'source(file.path(dir, "profile.R"',
    worker,
    fixed = TRUE
  )[1L]
  worker_adapters <- grep(
    'source(file.path(dir, "adapters.R"',
    worker,
    fixed = TRUE
  )[1L]

  expect_true(app_profile < app_adapters)
  expect_true(worker_profile < worker_adapters)

  expect_true(grepl(
    paste0(
      "\\.builder_register_adapter\\s*\\(\\s*",
      "builder_seurat_file_adapter\\s*\\("
    ),
    paste(session, collapse = "\n"),
    perl = TRUE
  ))
  expect_match(
    paste(session, collapse = "\n"),
    ".builder_register_adapter(",
    fixed = TRUE
  )
  expect_match(paste(session, collapse = "\n"), ".builder_snapshots")
  expect_match(paste(session, collapse = "\n"), "builder_execute_plan")
  expect_false(grepl(
    ".builder_snapshot_release",
    paste(session, collapse = "\n"),
    fixed = TRUE
  ))
  expect_match(
    paste(worker, collapse = "\n"),
    "builder_worker_release_snapshot",
    fixed = TRUE
  )
})

.builder_wait_for_worker <- function(worker, timeout = 30) {
  deadline <- Sys.time() + timeout
  repeat {
    polled <- builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      return(list(worker = worker, result = polled$result))
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the Builder worker.")
    }
  }
}

test_that("real worker loads, fresh-builds and drops an owned snapshot", {
  skip_if_not_installed("callr")
  builder_repo_source("worker.R", local = globalenv())
  builder_repo_source("session.R", local = globalenv())
  root <- withr::local_tempdir()
  source <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, source)
  started <- builder_session_start(
    builder_profile_inst_path("builder")
  )
  expect_null(started$error)
  worker <- started$worker
  on.exit(try(worker$process$close(), silent = TRUE), add = TRUE)

  builder_session_load(worker, "ds1", source)
  loaded <- .builder_wait_for_worker(worker)
  worker <- loaded$worker
  expect_true(loaded$result$done)
  expect_null(loaded$result$value$error)
  snapshot <- loaded$result$value$snapshot
  worker <- builder_worker_register_snapshot(worker, "ds1", snapshot)
  unlink(source)
  expect_false(file.exists(source))

  live_object <- worker$process$run(function() {
    get("ds1", envir = get(".builder_objects", envir = globalenv()))
  })
  expect_s4_class(live_object, "Seurat")
  out <- file.path(root, "out")
  dir.create(out)
  source_snapshot_identity <- c(
    list(available = TRUE, snapshot = snapshot, source = list()),
    snapshot
  )
  object <- SeuratObject::pbmc_small
  plan <- structure(
    list(
      make_app = FALSE,
      app_contract_version = 0L,
      app_options = list(enabled = FALSE),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      ),
      items = list(list(
        id = "ds1",
        name = "PBMC",
        filename = "pbmc.crb",
        organism = "hg",
        assay = "RNA",
        layer = "data",
        included_groups = "groups",
        included_projections = "tsne",
        analyses = character(),
        analysis_dependency_graph = list(),
        artifact_identity = list(
          schema_version = 2L,
          cells = SeuratObject::Cells(object),
          features = rownames(object[["RNA"]]),
          group_levels = list(
            groups = sort(unique(as.character(object$groups)))
          ),
          projections = "tsne",
          source_metadata = c(
            "cell_barcode",
            "groups",
            "nCount_RNA",
            "nFeature_RNA"
          ),
          metadata = c("cell_barcode", "groups", "nUMI", "nGene"),
          spatial_sections = character()
        ),
        tables = list(),
        images = list(),
        nUMI = "nCount_RNA",
        nGene = "nFeature_RNA",
        default_group = "groups",
        metadata_policy = list(
          included = c(
            "cell_barcode",
            "groups",
            "nCount_RNA",
            "nFeature_RNA"
          )
        ),
        expression_backend = "embedded",
        sidecars = character(),
        manifest = list(),
        viewer_page_expectations = list(
          visible_conditional = character(),
          hidden_conditional = character()
        ),
        source_snapshot_identity = source_snapshot_identity
      ))
    ),
    class = c("builder_build_plan", "list")
  )
  builder_session_build(worker, plan)
  built <- .builder_wait_for_worker(worker, timeout = 60)
  worker <- built$worker
  expect_true(built$result$done)
  expect_null(built$result$value$error)
  expect_true(built$result$value$publishable)
  expect_length(built$result$value$built, 1L)
  expect_true(file.exists(built$result$value$built[[1L]]))
  expect_true(startsWith(
    built$result$value$built[[1L]],
    normalizePath(worker$snapshot_root)
  ))
  expect_false(file.exists(file.path(out, "pbmc.crb")))

  builder_session_drop(worker, "ds1")
  dropped <- .builder_wait_for_worker(worker)
  worker <- dropped$worker
  expect_identical(dropped$result$value, TRUE)
  expect_true(.builder_snapshot_owned(snapshot))
  worker <- builder_worker_release_snapshot(
    worker,
    "ds1",
    .builder_worker_identity(snapshot)
  )
  state <- worker$process$run(function() {
    snapshots <- get(".builder_snapshots", envir = globalenv())
    root <- get(".builder_snapshot_root", envir = globalenv())
    list(
      registered = exists("ds1", envir = snapshots, inherits = FALSE),
      remaining = list.files(root, all.files = TRUE, no.. = TRUE)
    )
  })
  expect_false(state$registered)
  expect_true(any(startsWith(state$remaining, "builder-build-stage-")))
  expect_length(worker$snapshot_registry, 0L)
})
