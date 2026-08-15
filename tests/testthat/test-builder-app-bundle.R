builder_app_bundle_path <- builder_profile_inst_path("builder", "app_bundle.R")
if (file.exists(builder_app_bundle_path)) {
  sys.source(builder_app_bundle_path, envir = globalenv())
}

builder_app_bundle_fixture <- function(
  stage = withr::local_tempdir(.local_envir = parent.frame()),
  initial_dataset = "dataset-b",
  initial_dataset_mode = "explicit"
) {
  paths <- file.path(stage, c("dataset-a.crb", "dataset-b.crb"))
  lapply(paths, function(path) saveRDS(list(valid = TRUE), path))
  labels <- c("Dataset A", "Dataset B")
  names(paths) <- labels
  plan <- structure(
    list(
      app_contract_version = 1L,
      dataset_order = c("dataset-a", "dataset-b"),
      make_app = TRUE,
      items = list(
        list(
          id = "dataset-a",
          name = labels[[1L]],
          filename = basename(paths[[1L]]),
          colors = list(cluster = c(A = "#000000")),
          default_projection = "umap",
          default_trajectory = NULL,
          overview_point_size = 4,
          expression_backend = "embedded",
          sidecars = character()
        ),
        list(
          id = "dataset-b",
          name = labels[[2L]],
          filename = basename(paths[[2L]]),
          colors = list(cluster = c(B = "#ffffff")),
          default_projection = "pca",
          default_trajectory = list(method = "monocle2", name = "lineage"),
          overview_point_size = 7,
          expression_backend = "embedded",
          sidecars = character()
        )
      ),
      app_options = list(
        enabled = TRUE,
        show_upload_ui = FALSE,
        initial_dataset = initial_dataset,
        initial_dataset_mode = initial_dataset_mode,
        initial_page = "projection",
        welcome_message = "Welcome, team!",
        point_size = list(overview_projection_point_size = 6),
        variable_to_compare = FALSE,
        host = "127.0.0.1",
        port = 4242L,
        max_request_size = 512,
        display_mode = "showcase",
        launch_browser = FALSE
      ),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )
  list(stage = stage, paths = paths, labels = labels, plan = plan)
}

test_that("App request carries only the fixed safe login summary", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )

  expect_identical(
    request$auth,
    list(
      enabled = TRUE,
      account_count = 2L,
      timeout_minutes = 15L,
      passphrase_env = "CEREBRO_AUTH_PASSPHRASE"
    )
  )
  expect_false("accounts" %in% names(request))
  expect_false("passphrase" %in% names(request$auth))
  expect_false(builder_auth_value_contains(request, "auth-user-a-7f31"))
  expect_false(builder_auth_value_contains(request, "auth-password-a-7f31"))
})

test_that("App assembly reads authentication material without passing a secret", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  source_dir <- file.path(fixture$stage, ".builder-auth-source")
  dir.create(source_dir, mode = "0700")
  Sys.chmod(source_dir, "0700", use_umask = FALSE)
  credentials <- file.path(source_dir, "credentials.sqlite")
  writeBin(as.raw(1:8), credentials)
  Sys.chmod(credentials, "0600", use_umask = FALSE)
  env_file <- .builder_auth_write_env(
    file.path(fixture$stage, "viewer-auth.env"),
    strrep("d", 64L)
  )
  material <- list(
    source_dir = normalizePath(source_dir, winslash = "/"),
    credentials = normalizePath(credentials, winslash = "/"),
    env_file = env_file,
    descriptor = list(
      credentials = normalizePath(credentials, winslash = "/"),
      passphrase_env = "CEREBRO_AUTH_PASSPHRASE",
      timeout_minutes = 15L
    )
  )
  observed <- NULL
  create_app <- function(...) {
    observed <<- list(...)
    dir.create(observed$result_dir)
  }

  builder_build_app(
    request,
    fixture$stage,
    create_app,
    auth_material = material
  )

  expect_identical(observed$auth, material$descriptor)
  expect_identical(
    Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_),
    NA_character_
  )
  expect_false(builder_auth_value_contains(observed, strrep("d", 64L)))
})

test_that("App request rejects altered login summary fields", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  mutations <- list(
    enabled = FALSE,
    account_count = 0L,
    timeout_minutes = 30L,
    passphrase_env = "NOT_CEREBRO_AUTH_PASSPHRASE",
    extra_field = "unexpected"
  )

  for (field in names(mutations)) {
    altered <- request
    if (identical(field, "extra_field")) {
      altered$auth[[field]] <- mutations[[field]]
    } else {
      altered$auth[[field]] <- mutations[[field]]
    }
    expect_error(
      .builder_app_validate_request(altered),
      "generated-App request contract is invalid",
      info = field
    )
  }
})

builder_app_backend_fixture <- function(
  mode,
  stage = withr::local_tempdir(.local_envir = parent.frame())
) {
  fixture <- builder_app_bundle_fixture(stage = stage)
  location <- paste0(
    "dataset-a.",
    if (identical(mode, "h5")) "h5" else "bpcells"
  )
  fixture$plan$items[[1L]]$expression_backend <- mode
  fixture$plan$items[[1L]]$sidecars <- location
  sidecar <- file.path(fixture$stage, location)
  if (identical(mode, "h5")) {
    writeBin(as.raw(1:32), sidecar)
  } else {
    dir.create(file.path(sidecar, "nested"), recursive = TRUE)
    dir.create(file.path(sidecar, "empty"))
    writeBin(as.raw(1:16), file.path(sidecar, "index.bin"))
    writeBin(as.raw(17:32), file.path(sidecar, "nested", "matrix.bin"))
  }
  fixture$sidecar <- sidecar
  fixture
}

builder_fake_app <- function(request, app_dir) {
  dir.create(app_dir)
  expect_true(file.copy(
    builder_profile_inst_path("viewer"),
    app_dir,
    recursive = TRUE
  ))
  expect_true(file.copy(
    builder_profile_inst_path("extdata"),
    app_dir,
    recursive = TRUE
  ))
  dir.create(file.path(app_dir, "private-data"))
  targets <- file.path("private-data", basename(request$cerebro_data))
  for (index in seq_along(targets)) {
    file.copy(
      request$cerebro_data[[index]],
      file.path(app_dir, targets[[index]])
    )
  }
  expect_true(file.copy(
    builder_profile_inst_path("viewer", "_bundle_app.R"),
    file.path(app_dir, "app.R")
  ))
  saveRDS(
    list(
      crb_file_to_load = stats::setNames(targets, request$selector_order),
      initial_dataset = request$initial_dataset,
      initial_page = request$initial_page,
      show_upload_ui = request$show_upload_ui,
      welcome_message = request$welcome_message,
      point_size = request$point_size,
      viewer_content = request$viewer_content,
      variable_to_compare = request$variable_to_compare,
      .bundle_run_options = list(
        schema_version = 1L,
        max_request_size_bytes = request$max_request_size * 1024^2,
        shiny_app_options = list(
          port = as.integer(request$port),
          host = request$host,
          launch.browser = request$launch_browser,
          quiet = TRUE,
          display.mode = request$display_mode
        )
      ),
      colors = request$colors,
      crb_pick_smallest_file = request$crb_pick_smallest_file,
      .bundle_backend_plan = request$backend_plan
    ),
    file.path(app_dir, "cerebro_config.rds")
  )
  app_dir
}

builder_flip_file_byte <- function(path) {
  size <- file.info(path)$size
  bytes <- readBin(path, "raw", n = size)
  index <- max(1L, min(length(bytes), 16L))
  bytes[[index]] <- as.raw(bitwXor(as.integer(bytes[[index]]), 1L))
  writeBin(bytes, path)
}

builder_copy_backend_to_app <- function(fixture, app_dir) {
  target <- file.path(app_dir, "private-data", basename(fixture$sidecar))
  if (dir.exists(fixture$sidecar)) {
    dir.create(file.path(target, "empty"), recursive = TRUE)
    dir.create(file.path(target, "nested"), recursive = TRUE)
    file.copy(
      file.path(fixture$sidecar, "index.bin"),
      file.path(target, "index.bin")
    )
    file.copy(
      file.path(fixture$sidecar, "nested", "matrix.bin"),
      file.path(target, "nested", "matrix.bin")
    )
  } else {
    file.copy(fixture$sidecar, target)
  }
  target
}

test_that("App arguments come only from the frozen plan", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )

  expect_identical(names(request$cerebro_data), request$selector_order)
  expect_identical(request$selector_order, fixture$labels)
  expect_identical(request$initial_dataset, "Dataset B")
  expect_identical(request$initial_dataset_mode, "explicit")
  expect_false(request$show_upload_ui)
  expect_identical(request$contract_version, 1L)
  expect_identical(names(request$colors), fixture$labels)
  expect_identical(names(request$viewer_content), fixture$labels)
  expect_identical(
    request$viewer_content[["Dataset B"]],
    list(
      default_projection = "pca",
      default_trajectory = list(method = "monocle2", name = "lineage"),
      overview_point_size = 7
    )
  )
  expect_false(request$crb_pick_smallest_file)
  expect_identical(
    names(request),
    c(
      "contract_version",
      "stage",
      "cerebro_data",
      "crb_identities",
      "selector_order",
      "initial_dataset",
      "initial_dataset_mode",
      "initial_page",
      "show_upload_ui",
      "welcome_message",
      "point_size",
      "viewer_content",
      "variable_to_compare",
      "host",
      "port",
      "max_request_size",
      "display_mode",
      "launch_browser",
      "auth",
      "colors",
      "crb_pick_smallest_file",
      "backend_plan",
      "backend_identities",
      "content_identities"
    )
  )
  expect_identical(names(request$crb_identities), fixture$labels)
  expect_true(all(vapply(
    request$crb_identities,
    function(identity) {
      identical(identity$path, request$cerebro_data[[identity$label]]) &&
        is.character(identity$md5) &&
        grepl("^[[:xdigit:]]{32}$", identity$md5)
    },
    logical(1)
  )))
})

test_that("App requests freeze portable copied-content identities", {
  fixture <- builder_app_backend_fixture("bpcells")
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  relative_crb <- "private-data/dataset-a.crb"
  content <- request$content_identities[[relative_crb]]

  expect_identical(
    names(request$content_identities),
    names(request$backend_plan$entries)
  )
  expect_identical(content$crb$path, relative_crb)
  expect_identical(content$crb$type, "file")
  expect_identical(content$crb$size, request$crb_identities[[1L]]$size)
  expect_identical(content$crb$md5, request$crb_identities[[1L]]$md5)
  expect_identical(content$backend$type, "bpcells")
  expect_identical(content$backend$root, "private-data/dataset-a.bpcells")
  expect_true(
    "private-data/dataset-a.bpcells/empty" %in%
      names(content$backend$entries)
  )
})

test_that("App requests freeze the complete backend closure", {
  for (mode in c("h5", "bpcells")) {
    fixture <- builder_app_backend_fixture(mode)
    request <- builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    )
    relative_crb <- "private-data/dataset-a.crb"
    closure <- request$backend_identities[[relative_crb]]

    expect_identical(
      names(closure),
      c("type", "root", "root_fingerprint", "entries")
    )
    expect_identical(closure$type, mode)
    expect_identical(
      closure$root,
      normalizePath(fixture$sidecar, winslash = "/", mustWork = TRUE)
    )
    expect_true(length(closure$entries) >= 1L)
    if (identical(mode, "bpcells")) {
      expect_identical(closure$root_fingerprint$type, "directory")
      expect_identical(
        names(closure$root_fingerprint),
        c(
          "type",
          "size",
          "permissions",
          "device_id",
          "inode",
          "hard_links",
          "modification_time",
          "change_time"
        )
      )
      expect_identical(
        names(closure$entries),
        file.path(
          "dataset-a.bpcells",
          c("empty", "index.bin", "nested", "nested/matrix.bin")
        )
      )
      expect_identical(
        unname(vapply(closure$entries, `[[`, character(1), "type")),
        c("directory", "file", "directory", "file")
      )
    }
    files <- Filter(
      function(identity) identical(identity$type, "file"),
      closure$entries
    )
    expect_true(all(vapply(
      files,
      function(entry) grepl("^[0-9a-f]{32}$", entry$identity$md5),
      logical(1)
    )))
  }
})

test_that("Backend topology identities use an exact request schema", {
  fixture <- builder_app_backend_fixture("bpcells")
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  relative_crb <- "private-data/dataset-a.crb"
  closure <- request$backend_identities[[relative_crb]]
  malformed <- list(
    within(request, {
      backend_identities[[relative_crb]]$root_fingerprint$inode <- 1L
    }),
    within(request, {
      backend_identities[[relative_crb]]$root_fingerprint$size <- 1.5
    }),
    within(request, {
      backend_identities[[relative_crb]]$root_fingerprint$inode <- 1.5
    }),
    within(request, {
      backend_identities[[relative_crb]]$root_fingerprint$hard_links <- 1.5
    }),
    within(request, {
      backend_identities[[relative_crb]]$root_fingerprint$unknown <- TRUE
    }),
    within(request, {
      backend_identities[[relative_crb]]$root_fingerprint$permissions <-
        "garbage"
    }),
    within(request, {
      backend_identities[[relative_crb]]$entries <-
        rev(backend_identities[[relative_crb]]$entries)
    }),
    within(request, {
      directory <- "dataset-a.bpcells/empty"
      backend_identities[[relative_crb]]$entries[[directory]]$unknown <- TRUE
    }),
    within(request, {
      directory <- "dataset-a.bpcells/empty"
      backend_identities[[relative_crb]]$entries[[
        directory
      ]]$fingerprint$permissions <-
        "garbage"
    }),
    within(request, {
      file <- "dataset-a.bpcells/index.bin"
      backend_identities[[relative_crb]]$entries[[file]]$identity$md5 <-
        toupper(backend_identities[[relative_crb]]$entries[[file]]$identity$md5)
    }),
    within(request, {
      file <- "dataset-a.bpcells/index.bin"
      backend_identities[[relative_crb]]$entries[[file]]$identity$permissions <-
        "garbage"
    }),
    within(request, {
      file <- "dataset-a.bpcells/index.bin"
      backend_identities[[relative_crb]]$entries[[file]]$identity$inode <- 1.5
    })
  )

  expect_true(length(closure$entries) > 1L)
  for (value in malformed) {
    expect_error(.builder_app_validate_request(value), "request contract")
  }
  negative_time <- request
  negative_time$backend_identities[[
    relative_crb
  ]]$root_fingerprint$modification_time <-
    -1
  expect_no_error(.builder_app_validate_request(negative_time))
})

test_that("App assembly rejects backend closure drift before dispatch", {
  mutations <- list(
    h5_modify = function(fixture) builder_flip_file_byte(fixture$sidecar),
    bpcells_modify = function(fixture) {
      builder_flip_file_byte(file.path(fixture$sidecar, "index.bin"))
    },
    bpcells_add = function(fixture) {
      writeBin(as.raw(1:4), file.path(fixture$sidecar, "added.bin"))
    },
    bpcells_remove = function(fixture) {
      unlink(file.path(fixture$sidecar, "index.bin"))
    },
    bpcells_replace = function(fixture) {
      member <- file.path(fixture$sidecar, "index.bin")
      bytes <- readBin(member, "raw", n = file.info(member)$size)
      unlink(member)
      writeBin(bytes, member)
    },
    bpcells_add_empty_directory = function(fixture) {
      dir.create(file.path(fixture$sidecar, "added-empty"))
    },
    bpcells_remove_empty_directory = function(fixture) {
      unlink(file.path(fixture$sidecar, "empty"), recursive = TRUE)
    },
    bpcells_rename_empty_directory = function(fixture) {
      file.rename(
        file.path(fixture$sidecar, "empty"),
        file.path(fixture$sidecar, "renamed-empty")
      )
    },
    bpcells_replace_empty_directory = function(fixture) {
      directory <- file.path(fixture$sidecar, "empty")
      unlink(directory, recursive = TRUE)
      dir.create(directory)
    }
  )
  if (!identical(.Platform$OS.type, "windows")) {
    mutations$bpcells_directory_mode <- function(fixture) {
      directory <- file.path(fixture$sidecar, "nested")
      mode <- if (identical(as.character(file.info(directory)$mode), "700")) {
        "0755"
      } else {
        "0700"
      }
      Sys.chmod(
        directory,
        mode = mode,
        use_umask = FALSE
      )
    }
    mutations$bpcells_root_mode <- function(fixture) {
      mode <- if (
        identical(as.character(file.info(fixture$sidecar)$mode), "700")
      ) {
        "0755"
      } else {
        "0700"
      }
      Sys.chmod(fixture$sidecar, mode = mode, use_umask = FALSE)
    }
  }
  for (name in names(mutations)) {
    mode <- if (startsWith(name, "h5")) "h5" else "bpcells"
    fixture <- builder_app_backend_fixture(mode)
    request <- builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    )
    mutations[[name]](fixture)
    called <- FALSE

    expect_error(
      builder_build_app(
        request,
        fixture$stage,
        create_app = function(...) called <<- TRUE
      ),
      "backend closure"
    )
    expect_false(called)
  }
})

test_that("App assembly rejects backend closure drift during create_app", {
  for (mode in c("h5", "bpcells")) {
    fixture <- builder_app_backend_fixture(mode)
    request <- builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    )
    create_app <- function(...) {
      arguments <- list(...)
      builder_fake_app(request, arguments$result_dir)
      target <- if (identical(mode, "h5")) {
        fixture$sidecar
      } else {
        dir.create(file.path(fixture$sidecar, "during-empty"))
        NULL
      }
      if (!is.null(target)) {
        builder_flip_file_byte(target)
      }
    }

    expect_error(
      builder_build_app(request, fixture$stage, create_app),
      "backend closure"
    )
  }
})

test_that("Backend closure capture rejects aliases and unreadable files", {
  skip_on_os("windows")

  linked <- builder_app_backend_fixture("h5")
  alias <- tempfile(fileext = ".h5")
  withr::defer(unlink(alias))
  skip_if_not(file.link(linked$sidecar, alias), "hard links are unavailable")
  expect_error(
    builder_app_bundle_request(linked$plan, linked$paths, linked$labels),
    "hard link"
  )

  symbolic <- builder_app_backend_fixture("bpcells")
  external <- tempfile()
  writeBin(as.raw(1:4), external)
  withr::defer(unlink(external))
  expect_true(file.symlink(
    external,
    file.path(symbolic$sidecar, "linked.bin")
  ))
  expect_error(
    builder_app_bundle_request(
      symbolic$plan,
      symbolic$paths,
      symbolic$labels
    ),
    "symbolic"
  )

  unreadable <- builder_app_backend_fixture("h5")
  old_mode <- file.info(unreadable$sidecar)$mode
  Sys.chmod(unreadable$sidecar, mode = "0000", use_umask = FALSE)
  withr::defer(Sys.chmod(
    unreadable$sidecar,
    mode = old_mode,
    use_umask = FALSE
  ))
  expect_error(
    builder_app_bundle_request(
      unreadable$plan,
      unreadable$paths,
      unreadable$labels
    ),
    "readable"
  )

  unreadable_tree <- builder_app_backend_fixture("bpcells")
  old_tree_mode <- file.info(unreadable_tree$sidecar)$mode
  Sys.chmod(unreadable_tree$sidecar, mode = "0000", use_umask = FALSE)
  withr::defer(Sys.chmod(
    unreadable_tree$sidecar,
    mode = old_tree_mode,
    use_umask = FALSE
  ))
  expect_error(
    builder_app_bundle_request(
      unreadable_tree$plan,
      unreadable_tree$paths,
      unreadable_tree$labels
    ),
    "enumerated safely"
  )
})

test_that("stable identities hash each regular file only once per checkpoint", {
  root <- withr::local_tempdir()
  first <- file.path(root, "a.bin")
  second <- file.path(root, "b.bin")
  writeBin(as.raw(1:4), first)
  writeBin(as.raw(5:8), second)
  calls <- 0L
  digest <- function(path) {
    calls <<- calls + 1L
    tools::md5sum(path)
  }

  .builder_app_capture_file_identity(first, .digest_file = digest)
  expect_identical(calls, 1L)

  calls <- 0L
  .builder_app_tree_identity(root, .digest_file = digest)
  expect_identical(calls, 2L)
})

test_that("App assembly rejects CRBs changed before or during create_app", {
  before <- builder_app_bundle_fixture()
  before_request <- builder_app_bundle_request(
    before$plan,
    before$paths,
    before$labels
  )
  old_time <- file.info(before$paths[[1L]])$mtime
  builder_flip_file_byte(before$paths[[1L]])
  Sys.setFileTime(before$paths[[1L]], old_time)
  called <- FALSE

  expect_error(
    builder_build_app(
      before_request,
      before$stage,
      create_app = function(...) {
        called <<- TRUE
      }
    ),
    "changed"
  )
  expect_false(called)

  during <- builder_app_bundle_fixture()
  during_request <- builder_app_bundle_request(
    during$plan,
    during$paths,
    during$labels
  )
  create_app <- function(...) {
    arguments <- list(...)
    builder_fake_app(during_request, arguments$result_dir)
    old_time <- file.info(during$paths[[1L]])$mtime
    builder_flip_file_byte(during$paths[[1L]])
    Sys.setFileTime(during$paths[[1L]], old_time)
  }

  expect_error(
    builder_build_app(during_request, during$stage, create_app),
    "changed"
  )
})

test_that("App identities reject files with external hard-link aliases", {
  fixture <- builder_app_bundle_fixture()
  alias <- tempfile(fileext = ".crb")
  linked <- file.link(fixture$paths[[1L]], alias)
  withr::defer(unlink(alias))
  skip_if_not(linked, "hard links are unavailable")

  expect_error(
    builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    ),
    "hard link"
  )

  app_fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    app_fixture$plan,
    app_fixture$paths,
    app_fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(app_fixture$stage, "cerebro_app")
  )
  public <- file.path(app_dir, "viewer", "www")
  dir.create(public, recursive = TRUE, showWarnings = FALSE)
  expect_true(file.link(
    file.path(app_dir, "private-data", "dataset-a.crb"),
    file.path(public, "leak.bin")
  ))

  expect_error(builder_verify_app(app_dir, request), "hard link")
})

test_that("Builder and verifier share an exact request-v1 schema", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  wrong_upload <- request
  wrong_upload$show_upload_ui <- 0L
  wrong_identity <- request
  wrong_identity$crb_identities[[1L]]$md5 <- 1L
  wrong_backend <- request
  wrong_backend$backend_plan$entries[[1L]]$unknown <- TRUE
  wrong_backend_identity <- request
  wrong_backend_identity$backend_identities[[1L]]$root <- request$stage
  wrong_content_identity <- request
  wrong_content_identity$content_identities[[1L]]$crb$md5 <- strrep("0", 32L)
  uppercase_digest <- request
  uppercase_digest$crb_identities[[1L]]$md5 <- toupper(
    uppercase_digest$crb_identities[[1L]]$md5
  )
  noncanonical_path <- request
  noncanonical_path$cerebro_data[[1L]] <- file.path(
    dirname(noncanonical_path$cerebro_data[[1L]]),
    ".",
    basename(noncanonical_path$cerebro_data[[1L]])
  )
  noncanonical_path$crb_identities[[1L]]$path <-
    noncanonical_path$cerebro_data[[1L]]
  automatic_drift <- request
  automatic_drift$initial_dataset_mode <- "automatic"
  wrong_colors <- request
  wrong_colors$colors[[1L]]$cluster <- c(A = 1L)
  empty_colors <- request
  empty_colors$colors[[1L]] <- list()
  unnamed_colors <- request
  unnamed_colors$colors[[1L]]$cluster <- "#000000"
  invalid_colors <- request
  invalid_colors$colors[[1L]]$cluster <- c(A = "not-a-color")
  wrong_sidecar <- request
  first_backend <- names(wrong_sidecar$backend_plan$entries)[[1L]]
  wrong_sidecar$backend_plan$entries[[first_backend]] <- list(
    type = "h5",
    mode = "bundled",
    location = "nested/a.h5"
  )
  outside <- tempfile(fileext = ".crb")
  saveRDS(list(valid = TRUE), outside)
  withr::defer(unlink(outside))
  outside_data <- request
  outside_data$cerebro_data[[1L]] <- normalizePath(
    outside,
    winslash = "/",
    mustWork = TRUE
  )
  outside_data$crb_identities[[1L]] <- .builder_app_capture_file_identity(
    outside_data$cerebro_data[[1L]],
    outside_data$selector_order[[1L]]
  )
  invalid <- list(
    within(request, contract_version <- 2L),
    wrong_upload,
    wrong_identity,
    wrong_backend,
    wrong_backend_identity,
    wrong_content_identity,
    uppercase_digest,
    noncanonical_path,
    automatic_drift,
    wrong_colors,
    empty_colors,
    unnamed_colors,
    invalid_colors,
    wrong_sidecar,
    outside_data,
    structure(
      c(unclass(request), list(unknown = TRUE)),
      class = class(request)
    ),
    structure(
      unclass(request)[rev(names(request))],
      class = class(request)
    ),
    structure(
      unclass(request)[setdiff(names(request), "crb_identities")],
      class = class(request)
    ),
    structure(
      unclass(request)[setdiff(names(request), "backend_identities")],
      class = class(request)
    ),
    structure(
      unclass(request)[setdiff(names(request), "content_identities")],
      class = class(request)
    )
  )

  for (value in invalid) {
    expect_error(.builder_app_validate_request(value), "request contract")
    expect_error(builder_verify_app(app_dir, value), "request contract")
    expect_error(
      builder_build_app(value, fixture$stage),
      "request contract"
    )
  }

  build_fixture <- builder_app_bundle_fixture()
  build_request <- builder_app_bundle_request(
    build_fixture$plan,
    build_fixture$paths,
    build_fixture$labels
  )
  build_request$contract_version <- 2L
  expect_error(
    builder_build_app(build_request, build_fixture$stage),
    "request contract"
  )
})

test_that("App requests reject non-frozen and mismatched build evidence", {
  fixture <- builder_app_bundle_fixture()
  invalid <- list(
    unclass(fixture$plan),
    within(fixture$plan, app_contract_version <- 0L),
    within(fixture$plan, dataset_order <- rev(dataset_order))
  )
  for (plan in invalid) {
    expect_error(
      builder_app_bundle_request(plan, fixture$paths, fixture$labels)
    )
  }

  expect_error(builder_app_bundle_request(
    fixture$plan,
    unname(fixture$paths),
    fixture$labels
  ))
  expect_error(builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    rev(fixture$labels)
  ))
  expect_error(builder_app_bundle_request(
    fixture$plan,
    fixture$paths[c(1L, 1L)],
    fixture$labels
  ))
  outside <- fixture$paths
  outside[[2L]] <- tempfile(fileext = ".crb")
  saveRDS(list(valid = TRUE), outside[[2L]])
  withr::defer(unlink(outside[[2L]]))
  expect_error(
    builder_app_bundle_request(
      fixture$plan,
      outside,
      fixture$labels
    ),
    "stage"
  )

  referenced <- fixture$plan
  attr(referenced$app_options, "mutable") <- new.env(parent = emptyenv())
  expect_error(
    builder_app_bundle_request(
      referenced,
      fixture$paths,
      fixture$labels
    ),
    "reference"
  )
})

test_that("App frozen values reject executable and unbounded structures", {
  connection <- file(tempfile(), open = "w")
  withr::defer(close(connection))
  unsafe <- list(
    function() system("id"),
    quote(system("id")),
    as.name("callable"),
    new.env(parent = emptyenv()),
    connection,
    Reduce(function(value, index) list(value), seq_len(70L), init = TRUE)
  )

  expect_true(all(vapply(unsafe, .builder_app_has_reference, logical(1))))
  expect_true(.builder_app_has_reference(rep(
    list(rep(TRUE, 1000L)),
    101L
  )))
  wide_bytes <- vapply(
    seq_len(20L),
    function(index) paste0(strrep("x", 1024^2), index),
    character(1)
  )
  expect_true(.builder_app_has_reference(wide_bytes))
  expect_false(.builder_app_has_reference(list(
    schema_version = 1L,
    labels = c("A", "B"),
    enabled = FALSE
  )))
})

test_that("inert validation does not dispatch user length methods", {
  calls <- 0L
  length.builder_hostile <- function(value) {
    calls <<- calls + 1L
    stop("executed hostile method")
  }
  value <- structure(list(ok = TRUE), class = "builder_hostile")

  expect_true(.builder_app_has_reference(value))
  expect_identical(calls, 0L)
})

test_that("App requests reject nested S3 dispatch before reading items", {
  fixture <- builder_app_bundle_fixture()
  calls <- 0L
  `[[.builder_hostile_item` <- function(value, ...) {
    calls <<- calls + 1L
    stop("executed hostile method")
  }
  class(fixture$plan$items[[1L]]) <- "builder_hostile_item"

  expect_error(
    builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    ),
    "reference"
  )
  expect_identical(calls, 0L)
})

test_that("App requests strip the trusted top-level class before field reads", {
  fixture <- builder_app_bundle_fixture()
  calls <- 0L
  `$.builder_build_plan` <- function(value, ...) {
    calls <<- calls + 1L
    stop("executed hostile method")
  }

  expect_s3_class(
    builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    ),
    "builder_app_bundle_request"
  )
  expect_identical(calls, 0L)
})

test_that("App requests accept typed inert leaves from production plans", {
  fixture <- builder_app_bundle_fixture()
  claim <- structure(
    list(source = "/source/a", target = "a", artifact = "crb"),
    class = c("builder_asset_claim", "list")
  )
  manifest_entry <- structure(
    list(id = "expression"),
    class = c("builder_manifest_entry", "list")
  )
  fixture$plan$private_asset_claims <- list(claim)
  fixture$plan$manifest <- structure(
    list(expression = manifest_entry),
    class = c("builder_content_manifest", "list")
  )
  fixture$plan$created_at <- as.POSIXct(
    "2026-08-05 12:00:00",
    tz = "UTC"
  )

  expect_s3_class(
    builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    ),
    "builder_app_bundle_request"
  )
})

test_that("automatic and explicit initial selections stay distinct", {
  automatic <- builder_app_bundle_fixture(
    initial_dataset = "dataset-a",
    initial_dataset_mode = "automatic"
  )
  explicit <- builder_app_bundle_fixture(
    initial_dataset = "dataset-a",
    initial_dataset_mode = "explicit"
  )

  expect_identical(
    builder_app_bundle_request(
      automatic$plan,
      automatic$paths,
      automatic$labels
    )$initial_dataset_mode,
    "automatic"
  )
  expect_identical(
    builder_app_bundle_request(
      explicit$plan,
      explicit$paths,
      explicit$labels
    )$initial_dataset_mode,
    "explicit"
  )

  forged <- builder_app_bundle_fixture(
    initial_dataset = "dataset-b",
    initial_dataset_mode = "automatic"
  )
  expect_error(
    builder_app_bundle_request(
      forged$plan,
      forged$paths,
      forged$labels
    ),
    "automatic"
  )
})

test_that("App assembly calls only the accepted staged interface", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  observed <- NULL
  create_app <- function(...) {
    observed <<- list(...)
    builder_fake_app(request, observed$result_dir)
  }

  app_dir <- builder_build_app(request, fixture$stage, create_app)

  expect_identical(
    app_dir,
    file.path(normalizePath(fixture$stage, winslash = "/"), "cerebro_app")
  )
  expect_identical(observed$cerebro_data, request$cerebro_data)
  expect_identical(observed$result_dir, app_dir)
  expect_identical(observed$colors, request$colors)
  expect_false(observed$overwrite)
  expect_false(observed$launch_browser)
  expect_true(observed$quiet)
  expect_false(observed$verbose)
  expect_false(observed$crb_pick_smallest_file)
  expect_identical(observed$show_upload_ui, request$show_upload_ui)
  expect_identical(observed$initial_dataset, request$initial_dataset)
  expect_identical(observed$initial_page, request$initial_page)
  expect_identical(observed$welcome_message, request$welcome_message)
  expect_identical(observed$point_size, request$point_size)
  expect_identical(observed$variable_to_compare, request$variable_to_compare)
  expect_identical(observed$host, request$host)
  expect_identical(observed$port, request$port)
  expect_identical(observed$max_request_size, request$max_request_size)
  expect_identical(observed$display_mode, request$display_mode)
  expect_identical(observed$launch_browser, request$launch_browser)
})

test_that("App request and config read-back preserve every Review option", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  verification <- builder_verify_app(app_dir, request)

  expect_true(verification$valid)
  expect_identical(request$welcome_message, "Welcome, team!")
  expect_identical(request$point_size, list(overview_projection_point_size = 6))
  expect_false(request$variable_to_compare)
  expect_identical(request$host, "127.0.0.1")
  expect_identical(request$port, 4242L)
  expect_identical(request$max_request_size, 512)
  expect_identical(request$display_mode, "showcase")
  expect_false(request$launch_browser)

  config_file <- file.path(app_dir, "cerebro_config.rds")
  config <- readRDS(config_file)
  config$welcome_message <- "drifted"
  saveRDS(config, config_file)
  expect_error(builder_verify_app(app_dir, request), "welcome message")

  config$welcome_message <- request$welcome_message
  config$initial_page <- "groups"
  saveRDS(config, config_file)
  expect_error(builder_verify_app(app_dir, request), "starting page")
})

test_that("App bundle rejects NULL rendered scalar options", {
  fixture <- builder_app_bundle_fixture()
  for (field in c("point_size", "variable_to_compare")) {
    plan <- fixture$plan
    if (identical(field, "point_size")) {
      plan$app_options$point_size$overview_projection_point_size <- NULL
    } else {
      plan$app_options$variable_to_compare <- NULL
    }
    expect_error(
      builder_app_bundle_request(plan, fixture$paths, fixture$labels),
      "options are invalid"
    )
  }
})

test_that("real createShinyApp output round trips the frozen request", {
  fixture <- builder_app_bundle_fixture()
  saveRDS(Cerebro$new(), fixture$paths[[1L]])
  saveRDS(Cerebro$new(), fixture$paths[[2L]])
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )

  app_dir <- builder_build_app(request, fixture$stage)
  verification <- builder_verify_app(app_dir, request)

  expect_true(verification$valid)
  expect_identical(verification$selector_order, fixture$labels)
  expect_identical(verification$initial_dataset, "Dataset B")
  expect_false(verification$auth_enabled)
  expect_null(verification$auth_database)
})

test_that("real login App verifies its exact database and external env pair", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  saveRDS(Cerebro$new(), fixture$paths[[1L]])
  saveRDS(Cerebro$new(), fixture$paths[[2L]])
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  material <- builder_auth_create_material(
    accounts,
    fixture$stage,
    .capability = function() list(available = TRUE, reason = NULL)
  )

  app_dir <- builder_build_app(
    request,
    fixture$stage,
    auth_material = material
  )
  verification <- builder_verify_app(
    app_dir,
    request,
    auth_env_file = material$env_file
  )

  expect_true(verification$auth_enabled)
  expect_identical(
    verification$auth_database,
    file.path(app_dir, "private-data", "auth", "credentials.sqlite")
  )
  expect_false("auth_env_file" %in% names(verification))
  expect_false(builder_auth_value_contains(
    verification,
    builder_auth_read_env_file(material$env_file)
  ))
})

test_that("installed layout carries the exact App entrypoint template", {
  installed <- system.file(
    "viewer",
    "_bundle_app.R",
    package = "CerebroNexus"
  )
  source <- builder_profile_inst_path("viewer", "_bundle_app.R")

  expect_true(nzchar(installed))
  expect_true(file.exists(installed))
  expect_identical(
    readBin(installed, "raw", n = file.info(installed)$size),
    readBin(source, "raw", n = file.info(source)$size)
  )
})

test_that("App verification rejects stable copied data replacements", {
  crb_fixture <- builder_app_bundle_fixture()
  crb_request <- builder_app_bundle_request(
    crb_fixture$plan,
    crb_fixture$paths,
    crb_fixture$labels
  )
  crb_app <- builder_fake_app(
    crb_request,
    file.path(crb_fixture$stage, "cerebro_app")
  )
  builder_flip_file_byte(file.path(crb_app, "private-data", "dataset-a.crb"))
  expect_error(
    builder_verify_app(crb_app, crb_request),
    "copied content"
  )

  for (mode in c("h5", "bpcells")) {
    fixture <- builder_app_backend_fixture(mode)
    request <- builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    )
    app_dir <- builder_fake_app(
      request,
      file.path(fixture$stage, "cerebro_app")
    )
    output_backend <- builder_copy_backend_to_app(fixture, app_dir)
    target <- if (identical(mode, "h5")) {
      output_backend
    } else {
      file.path(output_backend, "nested", "matrix.bin")
    }
    builder_flip_file_byte(target)

    expect_error(
      builder_verify_app(app_dir, request),
      "copied content"
    )
    if (identical(mode, "bpcells")) {
      expect_true(file.copy(
        file.path(fixture$sidecar, "nested", "matrix.bin"),
        target,
        overwrite = TRUE
      ))
      dir.create(file.path(output_backend, "unexpected-empty"))
      expect_error(
        builder_verify_app(app_dir, request),
        "copied content"
      )
    }
  }
})

test_that("App verification rejects unrequested private artifacts", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  writeBin(as.raw(1:8), file.path(app_dir, "private-data", "rogue.bin"))

  expect_error(builder_verify_app(app_dir, request), "copied content")
})

test_that("App verification binds executable code to package templates", {
  assert_code_rejected <- function(relative, contents) {
    fixture <- builder_app_bundle_fixture()
    request <- builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    )
    app_dir <- builder_fake_app(
      request,
      file.path(fixture$stage, "cerebro_app")
    )
    target <- file.path(app_dir, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    writeLines(contents, target)
    expect_error(builder_verify_app(app_dir, request), "trusted template")
  }

  assert_code_rejected(
    "app.R",
    c(
      "shiny::addResourcePath('leak', 'private-data')",
      "shiny::shinyApp(ui = NULL, server = function(...) NULL)"
    )
  )
  assert_code_rejected(
    "viewer/shiny_UI.R",
    "ui <- shiny::fluidPage('evil')"
  )
  assert_code_rejected("viewer/www/evil.js", "window.evil = true;")
  assert_code_rejected("viewer/www/evil.css", "body { display: none; }")
  assert_code_rejected("extdata/example_gene_set.gmt", "evil\tna\tGENE")
})

test_that("App verification rejects extra Shiny root entrypoints", {
  assert_root_rejected <- function(relative, contents) {
    fixture <- builder_app_bundle_fixture()
    request <- builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    )
    app_dir <- builder_fake_app(
      request,
      file.path(fixture$stage, "cerebro_app")
    )
    target <- file.path(app_dir, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    writeLines(contents, target)
    expect_error(builder_verify_app(app_dir, request), "trusted topology")
  }

  assert_root_rejected(
    "global.R",
    "shiny::addResourcePath('leak', 'private-data')"
  )
  assert_root_rejected("www/evil.js", "window.evil = true;")
  assert_root_rejected("R/evil.R", "options(cerebro.evil = TRUE)")
})

test_that("App verification reads back a private inert bundle", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )

  verification <- builder_verify_app(app_dir, request)

  expect_s3_class(verification, "builder_app_verification")
  expect_true(verification$valid)
  expect_identical(verification$selector_order, fixture$labels)
  expect_identical(verification$initial_dataset, "Dataset B")
  expect_identical(verification$backend_plan, request$backend_plan)
  expect_false(.builder_app_has_reference(verification))
  expect_false("initial_dataset_mode" %in% names(verification))
  expect_true("diagnostic_tree_identity" %in% names(verification))
  expect_identical(
    names(verification$diagnostic_tree_identity),
    c(
      "schema_version",
      "entry_count",
      "file_count",
      "directory_count",
      "aggregate_md5"
    )
  )
  expect_false("entries" %in% names(verification$diagnostic_tree_identity))
  expect_match(
    verification$diagnostic_tree_identity$aggregate_md5,
    "^[0-9a-f]{32}$"
  )

  dir.create(file.path(app_dir, "data"))
  expect_error(builder_verify_app(app_dir, request), "legacy")
})

test_that("App verification reports a missing private root directly", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  unlink(file.path(app_dir, "private-data"), recursive = TRUE)

  expect_error(
    builder_verify_app(app_dir, request),
    "private-data directory is missing or symbolic"
  )
})

test_that("App verification rejects a tree changed during one verification", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  calls <- 0L
  identity <- function(path) {
    result <- .builder_app_tree_identity(path)
    calls <<- calls + 1L
    if (calls == 1L) {
      writeLines(
        c(
          "# changed during verification",
          "shiny::shinyApp(ui = NULL, server = function(...) NULL)"
        ),
        file.path(path, "app.R")
      )
    }
    result
  }

  expect_error(
    builder_verify_app(app_dir, request, .tree_identity = identity),
    "changed during verification"
  )
})

test_that("App verification rejects config drift and private path aliases", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  config_file <- file.path(app_dir, "cerebro_config.rds")
  config <- readRDS(config_file)
  config$initial_dataset <- "Dataset A"
  saveRDS(config, config_file)
  expect_error(builder_verify_app(app_dir, request), "initial")

  config$initial_dataset <- request$initial_dataset
  config$crb_pick_smallest_file <- TRUE
  saveRDS(config, config_file)
  expect_error(builder_verify_app(app_dir, request), "smallest")

  config$crb_pick_smallest_file <- request$crb_pick_smallest_file
  saveRDS(config, config_file)
  target <- file.path(app_dir, config$crb_file_to_load[[1L]])
  unlink(target)
  skip_on_os("windows")
  expect_true(file.symlink(fixture$paths[[1L]], target))
  expect_error(builder_verify_app(app_dir, request), "symbolic")
})

test_that("App verification requires every bundled backend sidecar", {
  fixture <- builder_app_backend_fixture("h5")
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )

  expect_error(builder_verify_app(app_dir, request), "sidecar")
  sidecar <- file.path(app_dir, "private-data", "dataset-a.h5")
  dir.create(sidecar)
  expect_error(builder_verify_app(app_dir, request), "regular file")
  unlink(sidecar, recursive = TRUE)
  expect_true(file.copy(fixture$sidecar, sidecar))
  expect_true(builder_verify_app(app_dir, request)$valid)

  fixture <- builder_app_backend_fixture("bpcells")
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  sidecar <- file.path(app_dir, "private-data", "dataset-a.bpcells")
  writeBin(as.raw(1:4), sidecar)
  expect_error(builder_verify_app(app_dir, request), "directory")
  unlink(sidecar)
  builder_copy_backend_to_app(fixture, app_dir)
  expect_true(builder_verify_app(app_dir, request)$valid)

  writeLines("not valid R code }", file.path(app_dir, "app.R"))
  expect_error(builder_verify_app(app_dir, request), "parsed")
})

test_that("App tree verification fails closed when enumeration fails", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )

  expect_error(
    .builder_app_enumerate_tree(
      app_dir,
      .list_directory = function(path) stop("denied")
    ),
    "enumerated safely"
  )
})

test_that("regular App files must be readable even if the process is privileged", {
  path <- tempfile()
  writeBin(as.raw(1:4), path)
  withr::defer(unlink(path))

  expect_error(
    .builder_app_assert_readable_file(
      path,
      .open_file = function(...) stop("denied")
    ),
    "not readable"
  )

  skip_on_os("windows")
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  app_file <- file.path(app_dir, "app.R")
  Sys.chmod(app_file, mode = "0000")
  withr::defer(Sys.chmod(app_file, mode = "0600"))

  expect_error(builder_verify_app(app_dir, request), "not readable")
})

test_that("App config is size-bounded before deserialization", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  writeBin(
    raw(32L * 1024L * 1024L),
    file.path(app_dir, "cerebro_config.rds")
  )

  expect_error(builder_verify_app(app_dir, request), "config is too large")
})

test_that("App config rejects deeply nested deserialized values", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  config_file <- file.path(app_dir, "cerebro_config.rds")
  config <- readRDS(config_file)
  config$deep <- Reduce(
    function(value, index) list(value),
    seq_len(70L),
    init = TRUE
  )
  saveRDS(config, config_file)

  expect_error(builder_verify_app(app_dir, request), "inert readable")
})

test_that("App config rejects compressed values over the object byte budget", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  config_file <- file.path(app_dir, "cerebro_config.rds")
  config <- readRDS(config_file)
  config$wide <- rep(strrep("x", 1024^2), 20L)
  saveRDS(config, config_file, compress = "xz")
  expect_lt(file.info(config_file)$size, .builder_app_config_max_bytes)

  expect_error(builder_verify_app(app_dir, request), "inert readable")
})

test_that("App verification rejects symlinks anywhere in the bundle", {
  skip_on_os("windows")
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  public_root <- file.path(app_dir, "viewer", "www")
  dir.create(public_root, recursive = TRUE, showWarnings = FALSE)
  expect_true(file.symlink(
    file.path(app_dir, "private-data", "dataset-a.crb"),
    file.path(public_root, "leak.crb")
  ))

  expect_error(builder_verify_app(app_dir, request), "symbolic")
})

test_that("the privacy allowlist covers every bundled demo data file", {
  examples <- builder_profile_inst_path("extdata", "examples")
  files <- list.files(
    examples,
    pattern = "[.](crb|h5|hdf5|rds)$",
    full.names = TRUE
  )
  relative <- file.path("extdata", "examples", basename(files))

  expect_setequal(names(.builder_app_demo_data), relative)
  ordered <- files[match(names(.builder_app_demo_data), relative)]
  expect_identical(
    unname(tools::md5sum(ordered)),
    unname(.builder_app_demo_data)
  )
})

test_that("App verification rejects private data outside exact allowed roots", {
  assert_rejected <- function(relative, directory = FALSE) {
    fixture <- builder_app_bundle_fixture()
    request <- builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    )
    app_dir <- builder_fake_app(
      request,
      file.path(fixture$stage, "cerebro_app")
    )
    target <- file.path(app_dir, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (directory) {
      dir.create(target)
    } else {
      writeBin(as.raw(1:4), target)
    }
    expect_error(builder_verify_app(app_dir, request), "outside private-data")
  }

  assert_rejected("misc/rogue.crb")
  assert_rejected("misc/rogue.h5")
  assert_rejected("misc/rogue.bpcells", directory = TRUE)
  assert_rejected("misc/leak.rds")
  assert_rejected("extdata/examples/rogue.crb")
  assert_rejected("viewer/www/leak.crb")
  assert_rejected("viewer/www/leak.rds")
  case_alias <- list(
    entries = list(list(
      path = "PRIVATE-DATA/rogue.crb",
      type = "file",
      md5 = strrep("a", 32L)
    ))
  )
  names(case_alias$entries) <- "PRIVATE-DATA/rogue.crb"
  expect_error(
    .builder_app_validate_private_locations(
      case_alias,
      root = "/app",
      .canonical_path = function(path) path
    ),
    "outside private-data"
  )
  case_config <- list(
    entries = list(list(
      path = "CEREBRO_CONFIG.RDS",
      type = "file",
      md5 = strrep("a", 32L)
    ))
  )
  names(case_config$entries) <- "CEREBRO_CONFIG.RDS"
  expect_error(
    .builder_app_validate_private_locations(
      case_config,
      root = "/app",
      .canonical_path = function(path) path
    ),
    "outside private-data"
  )

  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  demo <- file.path(app_dir, "extdata", "examples", "example.crb")
  trusted_demo <- builder_profile_inst_path(
    "extdata",
    "examples",
    "example.crb"
  )
  expect_true(nzchar(trusted_demo))
  rds_demo <- file.path(app_dir, "extdata", "examples", "pbmc_SCE.rds")
  trusted_rds_demo <- builder_profile_inst_path(
    "extdata",
    "examples",
    "pbmc_SCE.rds"
  )
  expect_true(nzchar(trusted_rds_demo))

  expect_true(builder_verify_app(app_dir, request)$valid)

  builder_flip_file_byte(rds_demo)
  expect_error(builder_verify_app(app_dir, request), "outside private-data")
  expect_true(file.copy(trusted_rds_demo, rds_demo, overwrite = TRUE))
  builder_flip_file_byte(demo)
  expect_error(builder_verify_app(app_dir, request), "outside private-data")
})
