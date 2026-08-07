## Tests for the palette configured at bundle creation.
##
## `createShinyApp(colors = ...)` has always accepted a per-dataset palette,
## documented it, and written it into the generated app's configuration. Nothing
## read it back: `reactive_colors()` began from an empty list and filled it from
## the default colorset or the Color management pickers, so a deployment could
## not set its own colours and had no way to find that out.

## ---------------------------------------------------------------------------
## The pure resolver, exercised without a running app
## ---------------------------------------------------------------------------

color_config_env <- new.env(parent = globalenv())
color_config_path <- system.file(
  "viewer/color_config.R",
  package = "CerebroNexus"
)
if (!nzchar(color_config_path)) {
  color_config_path <- testthat::test_path(
    "../../inst/viewer/color_config.R"
  )
}
source(color_config_path, local = color_config_env)

resolve_configured_colors <- color_config_env$resolve_configured_colors
apply_configured_colors <- color_config_env$apply_configured_colors

files_two <- c(
  "PBMC example" = "/data/pbmc.crb",
  "My data set" = "/data/mine.crb"
)
config_two <- list(
  "PBMC example" = list(sample = c(sample_1 = "#1f77b4")),
  "My data set" = list(sample = c(a = "#111111", b = "#222222"))
)

test_that("the palette follows the data set that is loaded", {
  expect_identical(
    resolve_configured_colors(config_two, "/data/pbmc.crb", files_two),
    config_two[["PBMC example"]]
  )
  expect_identical(
    resolve_configured_colors(config_two, "/data/mine.crb", files_two),
    config_two[["My data set"]]
  )
})

test_that("a label with spaces resolves like any other", {
  ## The label is a name in a vector, not an identifier; spaces are ordinary
  ## and the demo app ships several.
  expect_length(
    resolve_configured_colors(config_two, "/data/mine.crb", files_two),
    1
  )
})

test_that("an unconfigured or uploaded data set falls back to defaults", {
  ## Uploads never appear in crb_file_to_load, so there is nothing to match.
  expect_identical(
    resolve_configured_colors(config_two, "/tmp/uploaded.crb", files_two),
    list()
  )
  expect_identical(
    resolve_configured_colors(
      list("Other" = list(sample = c(a = "#000000"))),
      "/data/pbmc.crb",
      files_two
    ),
    list()
  )
})

test_that("missing configuration is not an error", {
  expect_identical(
    resolve_configured_colors(NULL, "/data/pbmc.crb", files_two),
    list()
  )
  expect_identical(
    resolve_configured_colors(config_two, NULL, files_two),
    list()
  )
  expect_identical(
    resolve_configured_colors(config_two, "/data/pbmc.crb", NULL),
    list()
  )
  ## an unnamed file vector cannot be matched to a label
  expect_identical(
    resolve_configured_colors(config_two, "/data/pbmc.crb", unname(files_two)),
    list()
  )
})

test_that("a partial palette leaves the other levels at their defaults", {
  defaults <- c(a = "#aaaaaa", b = "#bbbbbb", c = "#cccccc")
  out <- apply_configured_colors(defaults, c(b = "#000000"))
  expect_identical(unname(out[["a"]]), "#aaaaaa")
  expect_identical(unname(out[["b"]]), "#000000")
  expect_identical(unname(out[["c"]]), "#cccccc")
  ## every current level still has exactly one colour
  expect_identical(names(out), names(defaults))
})

test_that("a configured level that no longer exists is ignored", {
  defaults <- c(a = "#aaaaaa")
  out <- apply_configured_colors(defaults, c(a = "#000000", gone = "#ffffff"))
  expect_identical(names(out), "a")
  expect_identical(unname(out[["a"]]), "#000000")
})

test_that("an empty or unnamed palette changes nothing", {
  defaults <- c(a = "#aaaaaa")
  expect_identical(apply_configured_colors(defaults, NULL), defaults)
  expect_identical(apply_configured_colors(defaults, character()), defaults)
  expect_identical(apply_configured_colors(defaults, "#000000"), defaults)
})

## ---------------------------------------------------------------------------
## What createShinyApp() accepts
## ---------------------------------------------------------------------------

demo_crb <- function() {
  path <- system.file("extdata/examples/example.crb", package = "CerebroNexus")
  if (!nzchar(path)) {
    path <- testthat::test_path("../../inst/extdata/examples/example.crb")
  }
  c("PBMC example" = path)
}

build_app <- function(colors, ...) {
  createShinyApp(
    cerebro_data = demo_crb(),
    result_dir = withr::local_tempdir(),
    launch_browser = FALSE,
    verbose = FALSE,
    colors = colors,
    ...
  )
}

test_that("the documented shape is accepted", {
  skip_if_not_installed("withr")
  expect_no_error(
    build_app(list("PBMC example" = list(sample = c(sample_1 = "#1f77b4"))))
  )
})

test_that("a palette that is not a named list per variable is refused", {
  skip_if_not_installed("withr")
  expect_error(
    build_app(list("PBMC example" = c(sample_1 = "#1f77b4"))),
    "named list"
  )
  expect_error(
    build_app(list("PBMC example" = list(c(sample_1 = "#1f77b4")))),
    "named list"
  )
})

test_that("a palette whose levels are unnamed is refused", {
  skip_if_not_installed("withr")
  expect_error(
    build_app(list("PBMC example" = list(sample = "#1f77b4"))),
    "named by group level"
  )
})

test_that("a colour R cannot read is refused rather than failing at runtime", {
  skip_if_not_installed("withr")
  expect_error(
    build_app(list(
      "PBMC example" = list(sample = c(sample_1 = "not a colour"))
    )),
    "cannot read as colours"
  )
})

test_that("a colour name as well as a hex value is accepted", {
  skip_if_not_installed("withr")
  expect_no_error(
    build_app(list("PBMC example" = list(sample = c(sample_1 = "steelblue"))))
  )
})

test_that("a palette for a data set that was not passed is called out", {
  skip_if_not_installed("withr")
  expect_warning(
    build_app(list(
      "PBMC example" = list(sample = c(sample_1 = "#1f77b4")),
      "Not passed" = list(sample = c(x = "#000000"))
    )),
    "No data set named"
  )
})

test_that("the configuration reaches the generated app unchanged", {
  skip_if_not_installed("withr")
  out <- withr::local_tempdir()
  palette <- list("PBMC example" = list(sample = c(sample_1 = "#1f77b4")))
  createShinyApp(
    cerebro_data = demo_crb(),
    result_dir = out,
    launch_browser = FALSE,
    verbose = FALSE,
    colors = palette
  )
  config <- readRDS(file.path(out, "cerebro_config.rds"))
  expect_identical(config[["colors"]], palette)
  ## and the resolver that reads it travels with the bundle
  expect_true(file.exists(file.path(out, "viewer/color_config.R")))
})

## ---------------------------------------------------------------------------
## The generated bundle actually uses the configured palette
## ---------------------------------------------------------------------------

test_that("a configured palette reaches the running app", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("withr")
  skip_on_cran()

  out <- withr::local_tempdir()
  crb <- demo_crb()
  ## a colour no default palette would produce, so a match cannot be chance
  configured <- "#0F1E2D"
  createShinyApp(
    cerebro_data = crb,
    result_dir = out,
    launch_browser = FALSE,
    verbose = FALSE,
    colors = list("PBMC example" = list(sample = c(sample_1 = configured)))
  )

  shinytest2::local_app_support(out)
  driver <- shinytest2::AppDriver$new(
    out,
    name = "configured_colors",
    load_timeout = 60000
  )
  withr::defer(driver$stop())
  driver$wait_for_idle(timeout = 30000)

  palettes <- driver$get_value(export = "group_colors")
  expect_false(is.null(palettes))
  expect_true("sample" %in% names(palettes))
  expect_identical(unname(palettes$sample[["sample_1"]]), configured)

  ## levels the configuration says nothing about keep a default, and every
  ## current level still has exactly one colour
  expect_true(all(nzchar(palettes$sample)))
  expect_false(any(is.na(palettes$sample)))
})

test_that("without configuration the defaults still apply", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("withr")
  skip_on_cran()

  out <- withr::local_tempdir()
  createShinyApp(
    cerebro_data = demo_crb(),
    result_dir = out,
    launch_browser = FALSE,
    verbose = FALSE
  )

  shinytest2::local_app_support(out)
  driver <- shinytest2::AppDriver$new(
    out,
    name = "default_colors",
    load_timeout = 60000
  )
  withr::defer(driver$stop())
  driver$wait_for_idle(timeout = 30000)

  palettes <- driver$get_value(export = "group_colors")
  expect_false(is.null(palettes))
  expect_true(all(nzchar(palettes$sample)))
})
