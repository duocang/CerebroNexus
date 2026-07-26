# test-ir-help-demo-helpers.R — pure contracts for IR help-example data.

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
local_inst <- inst_candidates[file.exists(file.path(
  inst_candidates,
  "shiny/v1.4"
))][1]
if (is.na(local_inst)) {
  local_inst <- system.file(package = "CerebroNexus")
}
help_demo_file <- file.path(
  local_inst,
  "shiny/v1.4/immune_repertoire/help_demo_helpers.R"
)
help_demo_exists <- file.exists(help_demo_file)

test_that("the bundled IR help-demo helper exists and parses", {
  expect_true(help_demo_exists)
  if (help_demo_exists) {
    expect_no_error(parse(file = help_demo_file))
  }
})

help_demo_env <- new.env(parent = globalenv())
if (help_demo_exists) {
  sys.source(help_demo_file, envir = help_demo_env)
}

test_that("help-demo routing separates local, backed, and absent examples", {
  skip_if_not(help_demo_exists)

  expect_identical(help_demo_env$ir_help_demo_kind("Isotype"), "local_bcr")
  expect_identical(
    help_demo_env$ir_help_demo_kind("Clone Sharing"),
    "local_tcr"
  )
  expect_identical(help_demo_env$ir_help_demo_kind("Abundance"), "backed")
  expect_identical(
    help_demo_env$ir_help_demo_kind("Paired Scatter"),
    "backed"
  )
  expect_identical(help_demo_env$ir_help_demo_kind("Clonal UMAP"), "none")
  expect_false(help_demo_env$ir_help_has_example("Clonal UMAP"))
  expect_true(help_demo_env$ir_help_has_example("Clone Sharing"))
})

test_that("synthetic help data is deterministic without changing caller RNG", {
  skip_if_not(help_demo_exists)

  set.seed(123)
  expected <- stats::runif(3)
  set.seed(123)
  tcr_1 <- help_demo_env$ir_make_tcr_demo_data()
  expect_equal(stats::runif(3), expected)
  tcr_2 <- help_demo_env$ir_make_tcr_demo_data()
  expect_identical(tcr_1, tcr_2)

  set.seed(456)
  expected <- stats::runif(3)
  set.seed(456)
  bcr_1 <- help_demo_env$ir_make_bcr_demo_data()
  expect_equal(stats::runif(3), expected)
  bcr_2 <- help_demo_env$ir_make_bcr_demo_data()
  expect_identical(bcr_1, bcr_2)
  expect_true(all(vapply(
    bcr_1,
    function(df) {
      all(grepl(
        "^IGHV[^.]+\\.IGHD[^.]+\\.IGHJ[^.]+\\.IGH[ADEGM]",
        df$CTgene
      ))
    },
    logical(1)
  )))
})

test_that("local seeding restores an initially absent RNG state", {
  skip_if_not(help_demo_exists)

  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else if (
        exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      ) {
        rm(".Random.seed", envir = globalenv())
      }
    },
    add = TRUE
  )

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(
    help_demo_env$ir_with_preserved_seed(9L, stats::runif(1))
  )
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("backed help examples keep an explicit load boundary", {
  help_file <- file.path(
    local_inst,
    "shiny/v1.4/immune_repertoire/help.R"
  )
  source <- paste(readLines(help_file, warn = FALSE), collapse = "\n")
  renderer <- regmatches(
    source,
    regexpr(
      "output\\$ir_demo_plot <- renderPlot\\(\\{[\\s\\S]*?\\n\\}\\)",
      source,
      perl = TRUE
    )
  )

  expect_length(renderer, 1L)
  expect_match(
    renderer,
    paste0(
      'identical\\(demo_kind, "backed"\\)[\\s\\S]{0,160}',
      "req_scRepertoire\\(\\)"
    ),
    perl = TRUE
  )
  expect_match(
    renderer,
    '"Paired Scatter"\\s*=\\s*scRepertoire::clonalScatter',
    perl = TRUE
  )
})
