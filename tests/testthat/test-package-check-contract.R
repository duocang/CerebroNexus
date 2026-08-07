source_file <- function(...) {
  testthat::test_path("..", "..", ...)
}

skip_if_not_source_tree <- function() {
  skip_if_not(
    file.exists(source_file(".Rbuildignore")),
    "static source-tree contract"
  )
}

test_that("development-only directories are excluded from package builds", {
  skip_if_not_source_tree()
  ignores <- readLines(source_file(".Rbuildignore"), warn = FALSE)
  expected <- c(
    "^\\.claude$",
    "^\\.loci$",
    "^\\.playwright-mcp$",
    "^\\.sisyphus$",
    "^\\.superpowers$"
  )

  expect_true(all(expected %in% ignores))
})

test_that("package and exported-app branding use the current identity", {
  description_path <- source_file("DESCRIPTION")
  if (!file.exists(description_path)) {
    description_path <- system.file("DESCRIPTION", package = "CerebroNexus")
  }
  description <- read.dcf(description_path)

  expect_identical(unname(description[1, "Package"]), "CerebroNexus")
  expect_match(description[1, "URL"], "mihem/CerebroNexus", fixed = TRUE)
  expect_identical(
    formals(createShinyApp)$welcome_message,
    "Welcome to CerebroNexus!"
  )
})

test_that("self-contained app vignette never purls interactive runApp calls", {
  skip_if_not_source_tree()
  lines <- readLines(
    source_file("vignettes", "create_a_self_contained_shiny_app.Rmd"),
    warn = FALSE
  )
  run_lines <- which(grepl("shiny::runApp(out_dir)", lines, fixed = TRUE))
  expect_length(run_lines, 2L)

  chunk_headers <- vapply(
    run_lines,
    function(line_number) {
      prior <- lines[seq_len(line_number)]
      prior[max(which(grepl("^```\\{r", prior)))]
    },
    character(1)
  )
  expect_true(all(grepl("purl=FALSE", chunk_headers, fixed = TRUE)))
})

test_that("summarisation has no unused unqualified ave call", {
  skip_if_not_source_tree()
  seurat_source <- paste(
    readLines(source_file("R", "seurat_utils.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("idx_first <- ave(", seurat_source, fixed = TRUE))
})

test_that("later remains declared because bundled runtime code uses it", {
  skip_if_not_source_tree()
  description <- read.dcf(source_file("DESCRIPTION"), fields = "Imports")[[1]]
  namespace <- readLines(source_file("NAMESPACE"), warn = FALSE)
  runtime_source <- paste(
    readLines(source_file("inst", "viewer", "utility_functions.R")),
    collapse = "\n"
  )

  expect_match(description, "later")
  expect_true("importFrom(later,later)" %in% namespace)
  expect_match(runtime_source, "later::later(", fixed = TRUE)
})

test_that("the authentication provider is declared and available", {
  description_path <- source_file("DESCRIPTION")
  if (!file.exists(description_path)) {
    description_path <- system.file("DESCRIPTION", package = "CerebroNexus")
  }
  description <- read.dcf(description_path, fields = "Suggests")[[1L]]
  expect_match(description, "shinymanager (>= 1.1.0)", fixed = TRUE)
  expect_true(requireNamespace("shinymanager", quietly = TRUE))
  expect_gte(utils::packageVersion("shinymanager"), "1.1.0")
})

test_that("direct browser test dependencies are declared and available", {
  description_path <- source_file("DESCRIPTION")
  if (!file.exists(description_path)) {
    description_path <- system.file("DESCRIPTION", package = "CerebroNexus")
  }
  description <- read.dcf(description_path, fields = "Suggests")[[1L]]

  expect_match(description, "chromote", fixed = TRUE)
  expect_true(requireNamespace("chromote", quietly = TRUE))
})

test_that("the Nix shell keeps Chromium Linux-only", {
  skip_if_not_source_tree()
  nix <- paste(
    readLines(source_file("default.nix")),
    collapse = "\n"
  )
  generator <- paste(
    readLines(source_file("create_env.R")),
    collapse = "\n"
  )
  expected_block <- paste(
    c(
      "  system_packages = builtins.attrValues (",
      "    {",
      "      inherit (pkgs) glibcLocales nix pandoc R;",
      "    }",
      "    // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {",
      "      inherit (pkgs) chromium;",
      "    }",
      "  );"
    ),
    collapse = "\n"
  )

  expect_identical(
    lengths(regmatches(nix, gregexpr(expected_block, nix, fixed = TRUE))),
    1L
  )
  expect_identical(
    lengths(regmatches(
      nix,
      gregexpr("inherit (pkgs) chromium;", nix, fixed = TRUE)
    )),
    1L
  )
  expect_match(
    generator,
    'stop("Expected exactly one system_packages block start")',
    fixed = TRUE
  )
  expect_match(
    generator,
    'stop("Expected exactly one system_packages block end after its start")',
    fixed = TRUE
  )
  expect_match(
    generator,
    'stop("Generated system_packages block failed postcondition")',
    fixed = TRUE
  )
})

test_that("the system package rewrite fails closed", {
  skip_if_not_source_tree()
  expressions <- parse(source_file("create_env.R"))
  is_rewrite_assignment <- vapply(
    expressions,
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], quote(`<-`)) &&
        identical(expression[[2L]], quote(rewrite_system_packages))
    },
    logical(1)
  )
  rewrite_expression <- expressions[is_rewrite_assignment]
  expect_length(rewrite_expression, 1L)
  if (length(rewrite_expression) != 1L) {
    return(invisible())
  }

  environment <- new.env(parent = baseenv())
  eval(rewrite_expression[[1L]], envir = environment)
  rewrite <- environment$rewrite_system_packages
  fixture <- c(
    "let",
    "  system_packages = builtins.attrValues {",
    "    inherit (pkgs) glibcLocales nix pandoc R;",
    "  };",
    "",
    "  shell = pkgs.mkShell {",
    "  };"
  )
  expected_block <- c(
    "  system_packages = builtins.attrValues (",
    "    {",
    "      inherit (pkgs) glibcLocales nix pandoc R;",
    "    }",
    "    // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {",
    "      inherit (pkgs) chromium;",
    "    }",
    "  );"
  )

  expect_identical(rewrite(fixture)[2:9], expected_block)
  expect_error(
    rewrite(fixture[-2]),
    "Expected exactly one system_packages block start",
    fixed = TRUE
  )
  expect_error(
    rewrite(append(fixture, fixture[2], after = 2L)),
    "Expected exactly one system_packages block start",
    fixed = TRUE
  )
  expect_error(
    rewrite(fixture[-4]),
    "Expected exactly one system_packages block end after its start",
    fixed = TRUE
  )
  expect_error(
    rewrite(append(fixture, "  };", after = 4L)),
    "Expected exactly one system_packages block end after its start",
    fixed = TRUE
  )
  expect_error(
    rewrite(fixture[c(1, 6, 7, 2:5)]),
    "Expected exactly one system_packages block end after its start",
    fixed = TRUE
  )
  expect_error(
    rewrite(c(fixture, "      inherit (pkgs) chromium;")),
    "Generated system_packages block failed postcondition",
    fixed = TRUE
  )
})
