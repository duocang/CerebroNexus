# test-coordinated-views-config.R — portable Linked views configuration.

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
config_inst <- inst_candidates[file.exists(file.path(
  inst_candidates,
  "viewer"
))][1]
config_file <- if (is.na(config_inst)) {
  ""
} else {
  file.path(config_inst, "viewer/coordinated_views/config.R")
}
config_env <- new.env(parent = baseenv())
if (nzchar(config_file) && file.exists(config_file)) {
  sys.source(config_file, envir = config_env)
}

config_cells <- c("cell-a", "cell-b", "cell-c")

valid_linked_view_config <- function() {
  list(
    schema = "cerebronexus-linked-view",
    version = 1L,
    created_at = "2026-08-20T12:00:00Z",
    dataset = list(
      cell_count = length(config_cells),
      cell_fingerprint = config_env$cv_config_cell_fingerprint(config_cells)
    ),
    selection = list(
      cells = c("cell-a", "cell-c"),
      source = "UMAP"
    ),
    view = list(
      colour = list(
        mode = "cell_type",
        gene = NULL,
        rgb_genes = character(),
        clip = 0.01
      ),
      projections = c("umap", "pca"),
      spatial_sections = "donor-a",
      active_spatial = "donor-a",
      filters = list(sample = c("donor-a", "donor-b")),
      hidden_levels = list(
        list(group = "cell_type", levels = "Doublet")
      ),
      display = list(
        percentage_cells = 75,
        point_size = 3,
        point_opacity = 0.8,
        group_labels = TRUE,
        selection_mode = "lasso",
        clone_layout = "stack"
      ),
      focus_space = "projection::umap",
      lenses = list(
        list(
          space = "projection::umap",
          viewport = list(cx = 0.5, cy = 0.4, span = 1.2),
          rotation = NULL
        ),
        list(
          space = "projection::pca",
          viewport = list(cx = 0.6, cy = 0.7, span = 0.9),
          rotation = list(rx = 0.2, ry = -0.1)
        )
      ),
      spatial_backgrounds = list(
        list(
          section = "donor-a",
          mode = "image",
          image_id = "he-main",
          opacity = 0.65,
          alignment = list(
            offset_x = 3,
            offset_y = -2,
            scale = 1.1,
            rotation = 0.05
          )
        )
      ),
      trekker = list(
        dissolve_percentage = 25,
        evidence = TRUE,
        niche_radius = 250
      )
    )
  )
}

test_that("the linked-view configuration module exists and parses", {
  expect_true(nzchar(config_file) && file.exists(config_file))
  expect_no_error(parse(file = config_file))
})

test_that("linked-view fingerprints ignore cell order", {
  fingerprint <- config_env$cv_config_cell_fingerprint
  expect_true(is.function(fingerprint))
  expect_identical(
    fingerprint(c("cell-b", "cell-a", "cell-c")),
    fingerprint(c("cell-c", "cell-b", "cell-a"))
  )
  expect_match(
    fingerprint(config_cells),
    "^md5-cell-set-v1:[0-9a-f]{32}$"
  )
})

test_that("a version-one configuration round-trips canonically", {
  normalized <- config_env$cv_config_normalize(
    valid_linked_view_config(),
    cells = config_cells
  )
  encoded <- config_env$cv_config_encode(normalized)
  decoded <- config_env$cv_config_decode(encoded, cells = config_cells)

  expect_identical(decoded$schema, "cerebronexus-linked-view")
  expect_identical(decoded$version, 1L)
  expect_identical(decoded$selection$cells, c("cell-a", "cell-c"))
  expect_identical(decoded$view$display$selection_mode, "lasso")
  expect_identical(decoded$view$lenses[[2L]]$rotation$ry, -0.1)
  expect_identical(decoded$view$spatial_backgrounds[[1L]]$image_id, "he-main")
  expect_true(endsWith(encoded, "\n"))
})

expect_config_error <- function(config, code) {
  error <- tryCatch(
    {
      config_env$cv_config_normalize(config, cells = config_cells)
      NULL
    },
    cv_config_error = identity
  )
  expect_s3_class(error, "cv_config_error")
  expect_identical(error$code, code)
}

test_that("unsupported schema versions and unknown fields are rejected", {
  config <- valid_linked_view_config()
  config$schema <- "another-schema"
  expect_config_error(config, "unsupported_schema")

  config <- valid_linked_view_config()
  config$version <- 2L
  expect_config_error(config, "unsupported_version")

  config <- valid_linked_view_config()
  config$token <- "must-not-be-accepted"
  expect_config_error(config, "unknown_field")

  config <- valid_linked_view_config()
  config$view$display$future_option <- TRUE
  expect_config_error(config, "unknown_field")
})

test_that("dataset mismatch and incomplete cohorts are rejected", {
  config <- valid_linked_view_config()
  config$dataset$cell_count <- 4L
  expect_config_error(config, "dataset_mismatch")

  config <- valid_linked_view_config()
  config$dataset$cell_fingerprint <- paste0(
    "md5-cell-set-v1:",
    paste(rep("0", 32L), collapse = "")
  )
  expect_config_error(config, "dataset_mismatch")

  config <- valid_linked_view_config()
  config$selection$cells <- c("cell-a", "missing-cell")
  expect_config_error(config, "missing_cell")

  config <- valid_linked_view_config()
  config$selection$cells <- c("cell-a", "cell-a")
  expect_config_error(config, "duplicate_item")
})

test_that("configuration bounds reject hostile or ambiguous state", {
  config <- valid_linked_view_config()
  config$view$colour$clip <- Inf
  expect_config_error(config, "invalid_type")

  config <- valid_linked_view_config()
  config$view$display$point_opacity <- 1.1
  expect_config_error(config, "out_of_range")

  config <- valid_linked_view_config()
  config$view$lenses[[2L]]$space <- config$view$lenses[[1L]]$space
  expect_config_error(config, "duplicate_item")

  config <- valid_linked_view_config()
  config$view$trekker$niche_radius <- 501
  expect_config_error(config, "out_of_range")

  config <- valid_linked_view_config()
  config$view$filters <- Reduce(
    function(value, unused) list(value),
    seq_len(12L),
    init = "cell-a"
  )
  expect_config_error(config, "too_deep")
})

test_that("cross-field references must describe one coherent workspace", {
  config <- valid_linked_view_config()
  config$view$active_spatial <- "not-selected"
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$focus_space <- "projection::missing"
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$spatial_backgrounds[[1L]]$section <- "not-selected"
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$spatial_backgrounds[[1L]]["image_id"] <- list(NULL)
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$colour$mode <- "__rgb__"
  config$view$colour$rgb_genes <- c("CD3D", "MS4A1")
  expect_config_error(config, "invalid_reference")
})

test_that("JSON decoding is bounded and reports safe error codes", {
  malformed <- tryCatch(
    config_env$cv_config_decode("{not-json", cells = config_cells),
    cv_config_error = identity
  )
  expect_s3_class(malformed, "cv_config_error")
  expect_identical(malformed$code, "invalid_json")
  expect_identical(conditionMessage(malformed), "The file is not valid JSON.")

  oversized <- paste(rep("x", 5L * 1024L * 1024L + 1L), collapse = "")
  too_large <- tryCatch(
    config_env$cv_config_decode(oversized, cells = config_cells),
    cv_config_error = identity
  )
  expect_s3_class(too_large, "cv_config_error")
  expect_identical(too_large$code, "too_large")
})

test_that("canonical output contains only the portable allowlist", {
  encoded <- config_env$cv_config_encode(config_env$cv_config_normalize(
    valid_linked_view_config(),
    cells = config_cells
  ))
  expect_no_match(encoded, "dataset_id", fixed = TRUE)
  expect_no_match(encoded, "data:image", fixed = TRUE)
  expect_no_match(encoded, "expression", fixed = TRUE)
  expect_no_match(encoded, "metadata", fixed = TRUE)
  expect_no_match(encoded, "credentials", fixed = TRUE)
  expect_no_match(encoded, "token", fixed = TRUE)
  expect_no_match(encoded, "/Users/", fixed = TRUE)
})

test_that("the server prepares a canonical snapshot with its own timestamp", {
  config <- valid_linked_view_config()
  config$created_at <- "1999-01-01T00:00:00Z"
  prepared <- config_env$cv_config_prepare(
    config,
    cells = config_cells,
    now = as.POSIXct("2026-08-20 14:35:42", tz = "UTC")
  )

  expect_identical(prepared$config$created_at, "2026-08-20T14:35:42Z")
  expect_identical(
    config_env$cv_config_decode(prepared$json, cells = config_cells),
    prepared$config
  )
})

test_that("configuration failures map to a bounded public vocabulary", {
  mismatch <- structure(
    list(
      message = "/private/path must never be shown",
      call = NULL,
      code = "dataset_mismatch"
    ),
    class = c("cv_config_error", "error", "condition")
  )
  malformed <- simpleError("parser leaked /private/path")

  expect_identical(
    config_env$cv_config_safe_message(mismatch),
    "This configuration belongs to a different cell population."
  )
  expect_identical(
    config_env$cv_config_safe_message(malformed),
    "The configuration could not be opened."
  )
})

test_that("the coordinated server exposes validated configuration transport", {
  server_file <- file.path(config_inst, "viewer/coordinated_views/server.R")
  server <- paste(readLines(server_file, warn = FALSE), collapse = "\n")

  expect_match(server, "/viewer/coordinated_views/config.R", fixed = TRUE)
  expect_match(server, "cv_config_cell_fingerprint(b$cells)", fixed = TRUE)
  expect_match(server, "coordviews_config_request", fixed = TRUE)
  expect_match(server, "coordviews_config_upload", fixed = TRUE)
  expect_match(server, "coordviews_config_upload_nonce", fixed = TRUE)
  expect_match(server, "coordviews_config_download", fixed = TRUE)
  expect_match(server, '"coordviews_config_result"', fixed = TRUE)
  expect_match(server, "downloadHandler", fixed = TRUE)
  expect_match(server, "CV_CONFIG_MAX_BYTES", fixed = TRUE)
})
