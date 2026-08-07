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
  expect_identical(
    names(formals(builder_snapshot_cleanup)),
    c("registry", "now")
  )
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
  expect_s4_class(get("ds1", envir = objects), "Seurat")
  snapshot <- get("ds1", envir = snapshots)
  expect_true(dir.exists(snapshot$path))
  expect_s4_class(builder_open_snapshot(snapshot), "Seurat")
})

test_that("application and worker source adapters after profile contracts", {
  app <- readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE)
  session <- readLines(
    builder_profile_inst_path("builder", "session.R"),
    warn = FALSE
  )
  app_profile <- grep('source("profile.R"', app, fixed = TRUE)[1L]
  app_adapters <- grep('source("adapters.R"', app, fixed = TRUE)[1L]
  worker_profile <- grep(
    'source(file.path(dir, "profile.R"',
    session,
    fixed = TRUE
  )[1L]
  worker_adapters <- grep(
    'source(file.path(dir, "adapters.R"',
    session,
    fixed = TRUE
  )[1L]

  expect_true(app_profile < app_adapters)
  expect_true(worker_profile < worker_adapters)

  expect_match(
    paste(session, collapse = "\n"),
    ".builder_register_adapter(builder_seurat_file_adapter",
    fixed = TRUE
  )
  expect_match(
    paste(session, collapse = "\n"),
    ".builder_register_adapter(",
    fixed = TRUE
  )
  expect_match(paste(session, collapse = "\n"), ".builder_snapshots")
  expect_match(paste(session, collapse = "\n"), "builder_open_snapshot")
  expect_match(paste(session, collapse = "\n"), ".builder_snapshot_release")
})

.builder_wait_for_worker <- function(rs, timeout = 30) {
  deadline <- Sys.time() + timeout
  repeat {
    result <- builder_session_poll(rs, timeout = 100)
    if (!is.null(result)) {
      return(result)
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the Builder worker.")
    }
  }
}

test_that("real worker loads, fresh-builds and drops an owned snapshot", {
  skip_if_not_installed("callr")
  builder_repo_source("session.R", local = globalenv())
  root <- withr::local_tempdir()
  source <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, source)
  started <- builder_session_start(
    builder_profile_inst_path("builder")
  )
  expect_null(started$error)
  rs <- started$session
  on.exit(try(rs$close(), silent = TRUE), add = TRUE)

  builder_session_load(rs, "ds1", source)
  loaded <- .builder_wait_for_worker(rs)
  expect_true(loaded$done)
  expect_null(loaded$value$error)
  unlink(source)
  expect_false(file.exists(source))

  rs$run(function() {
    assign(
      "ds1",
      list(poisoned_live_object = TRUE),
      envir = get(".builder_objects", envir = globalenv())
    )
    TRUE
  })
  out <- file.path(root, "out")
  dir.create(out)
  plan <- list(
    out_dir = out,
    make_app = FALSE,
    app_contract_version = 0L,
    overwrite = FALSE,
    items = list(list(
      id = "ds1",
      name = "PBMC",
      filename = "pbmc.crb",
      organism = "hg",
      assay = "RNA",
      layer = "data",
      groups = "groups",
      reductions = "tsne",
      analyses = character(),
      tables = list(),
      images = list(),
      colors = list(),
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA"
    ))
  )
  builder_session_build(rs, plan)
  built <- .builder_wait_for_worker(rs, timeout = 60)
  expect_true(built$done)
  expect_null(built$value$error)
  expect_true(file.exists(file.path(out, "pbmc.crb")))

  builder_session_drop(rs, "ds1")
  dropped <- .builder_wait_for_worker(rs)
  expect_identical(dropped$value, TRUE)
  state <- rs$run(function() {
    snapshots <- get(".builder_snapshots", envir = globalenv())
    root <- get(".builder_snapshot_root", envir = globalenv())
    list(
      registered = exists("ds1", envir = snapshots, inherits = FALSE),
      remaining = list.files(root, all.files = TRUE, no.. = TRUE)
    )
  })
  expect_false(state$registered)
  expect_length(state$remaining, 0L)
})
