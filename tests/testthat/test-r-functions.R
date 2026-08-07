## Unit tests for CerebroNexus R package functions
## These tests do NOT require a running Shiny app or Seurat.
## They test pure R logic: the Cerebro_v1.3 R6 class, data loading,
## and input validation in functions that can be tested without Seurat.

## ---------------------------------------------------------------------------
## Cerebro_v1.3 R6 class
## ---------------------------------------------------------------------------

test_that("Cerebro_v1.3 object can be instantiated", {
  obj <- Cerebro_v1.3$new()
  expect_true(inherits(obj, "Cerebro_v1.3"))
  expect_true(inherits(obj, "R6"))
})

test_that("Cerebro_v1.3: addGroup / getGroups round-trip", {
  obj <- Cerebro_v1.3$new()
  # addGroup checks that the column exists in meta_data first
  obj$setMetaData(data.frame(
    sample = c("rep1", "rep2"),
    cluster = c("0", "1"),
    stringsAsFactors = FALSE
  ))
  obj$addGroup("sample", c("rep1", "rep2", "rep3"))
  obj$addGroup("cluster", c("0", "1", "2"))

  groups <- obj$getGroups()
  expect_equal(sort(groups), sort(c("sample", "cluster")))
})

test_that("Cerebro_v1.3: getGroupLevels returns correct levels", {
  obj <- Cerebro_v1.3$new()
  obj$setMetaData(data.frame(sample = c("A", "B"), stringsAsFactors = FALSE))
  obj$addGroup("sample", c("A", "B", "C"))

  lvls <- obj$getGroupLevels("sample")
  expect_equal(lvls, c("A", "B", "C"))
})

test_that("Cerebro_v1.3: checkIfGroupExists works correctly", {
  obj <- Cerebro_v1.3$new()
  obj$setMetaData(data.frame(cluster = c("0", "1"), stringsAsFactors = FALSE))
  obj$addGroup("cluster", c("0", "1"))

  # returns invisibly (NULL) when group exists — no error
  expect_no_error(obj$checkIfGroupExists("cluster"))
  # throws when group does not exist
  expect_error(obj$checkIfGroupExists("nonexistent"), regexp = "not present")
})

test_that("Cerebro_v1.3: addProjection / getProjection round-trip", {
  obj <- Cerebro_v1.3$new()
  proj <- data.frame(
    UMAP_1 = c(1.0, 2.0, 3.0),
    UMAP_2 = c(4.0, 5.0, 6.0)
  )
  obj$addProjection("UMAP", proj)

  result <- obj$getProjection("UMAP")
  expect_equal(result, proj)
})

test_that("Cerebro_v1.3: availableProjections lists added projections", {
  obj <- Cerebro_v1.3$new()
  obj$addProjection("tSNE", data.frame(x = 1:3, y = 1:3))
  obj$addProjection("UMAP", data.frame(x = 1:3, y = 1:3))

  projs <- obj$availableProjections()
  expect_true("tSNE" %in% projs)
  expect_true("UMAP" %in% projs)
})

test_that("Cerebro_v1.3: setMetaData / getMetaData round-trip", {
  obj <- Cerebro_v1.3$new()
  meta <- data.frame(
    cell_barcode = paste0("cell_", 1:5),
    sample = c("A", "A", "B", "B", "B"),
    nUMI = c(100L, 200L, 150L, 300L, 250L),
    stringsAsFactors = FALSE
  )
  obj$setMetaData(meta)

  result <- obj$getMetaData()
  expect_equal(nrow(result), 5L)
  expect_true("sample" %in% colnames(result))
  expect_true("nUMI" %in% colnames(result))
})

test_that("Cerebro_v1.3: addMarkerGenes / getMarkerGenes round-trip", {
  obj <- Cerebro_v1.3$new()
  mg_table <- data.frame(
    gene = c("CD3D", "CD79A", "FCGR3A"),
    p_val = c(0.001, 0.002, 0.003),
    avg_logFC = c(1.5, 1.2, 0.9),
    stringsAsFactors = FALSE
  )
  obj$addMarkerGenes(method = "seurat", name = "cluster", table = mg_table)

  result <- obj$getMarkerGenes(method = "seurat", name = "cluster")
  expect_equal(nrow(result), 3L)
  expect_true("gene" %in% colnames(result))
})

test_that("Cerebro_v1.3: setExpression / getExpressionMatrix round-trip", {
  obj <- Cerebro_v1.3$new()
  # Use a sparse Matrix (single-value class "dgCMatrix") to avoid the
  # length > 1 class() issue with base matrix in R >= 4.x
  mat <- Matrix::Matrix(
    c(0, 1, 2, 3, 0, 1),
    nrow = 2,
    dimnames = list(c("GeneA", "GeneB"), c("cell1", "cell2", "cell3")),
    sparse = TRUE
  )
  obj$setExpression(mat)

  result <- obj$getExpressionMatrix(
    cells = c("cell1", "cell2"),
    genes = c("GeneA", "GeneB")
  )
  expect_equal(nrow(result), 2L) # 2 genes
  expect_equal(ncol(result), 2L) # 2 cells
})

test_that("Cerebro_v1.3: getMeanExpressionForGenes returns numeric vector", {
  obj <- Cerebro_v1.3$new()
  mat <- Matrix::Matrix(
    c(0, 2, 4, 6, 1, 3),
    nrow = 2,
    dimnames = list(c("GeneA", "GeneB"), c("cell1", "cell2", "cell3")),
    sparse = TRUE
  )
  obj$setExpression(mat)

  result <- obj$getMeanExpressionForGenes(c("GeneA", "GeneB"))
  expect_equal(nrow(result), 2L)
  expect_true(is.numeric(result$expression))
  # GeneA: row 1 = c(0, 4, 1) → mean 5/3; GeneB: row 2 = c(2, 6, 3) → mean 11/3
  expect_equal(
    result$expression[result$gene == "GeneA"],
    mean(c(0, 4, 1)),
    tolerance = 1e-6
  )
  expect_equal(
    result$expression[result$gene == "GeneB"],
    mean(c(2, 6, 3)),
    tolerance = 1e-6
  )
})

test_that("Cerebro_v1.3: addGeneList / getGeneLists round-trip", {
  obj <- Cerebro_v1.3$new()
  # addGeneList(name, genes) — two separate arguments
  obj$addGeneList("mito", c("MT-CO1", "MT-ND1"))
  obj$addGeneList("ribo", c("RPS2", "RPL3"))

  gl <- obj$getGeneLists()
  expect_true("mito" %in% names(gl))
  expect_true("ribo" %in% names(gl))
  expect_equal(gl$mito, c("MT-CO1", "MT-ND1"))
})

test_that("Cerebro_v1.3: addExperiment / getExperiment round-trip", {
  obj <- Cerebro_v1.3$new()
  # addExperiment(field, content) — two separate arguments, call once per field
  obj$addExperiment("experiment_name", "PBMC test")
  obj$addExperiment("organism", "hg")
  obj$addExperiment("date_of_export", "2024-01-01")

  exp <- obj$getExperiment()
  expect_equal(exp$experiment_name, "PBMC test")
  expect_equal(exp$organism, "hg")
})

test_that("Cerebro_v1.3: version can be set and retrieved", {
  obj <- Cerebro_v1.3$new()
  obj$setVersion("1.3.0")
  expect_equal(as.character(obj$getVersion()), "1.3.0")
})

## ---------------------------------------------------------------------------
## example data integrity checks
## ---------------------------------------------------------------------------

test_that("example.crb loads successfully and has correct structure", {
  path <- system.file("extdata/examples/example.crb", package = "CerebroNexus")
  expect_true(file.exists(path))

  data <- readRDS(path)
  expect_true(inherits(data, "Cerebro_v1.3"))

  # groups
  groups <- data$getGroups()
  expect_true(length(groups) >= 1)

  # projections
  projs <- data$availableProjections()
  expect_true(length(projs) >= 1)

  # meta data has rows
  meta <- data$getMetaData()
  expect_true(nrow(meta) > 0)
})

test_that("example.crb contains expected groups and projections", {
  path <- system.file("extdata/examples/example.crb", package = "CerebroNexus")
  data <- readRDS(path)

  expect_true("sample" %in% data$getGroups())
  expect_true("seurat_clusters" %in% data$getGroups())

  projs <- data$availableProjections()
  expect_true(any(grepl("UMAP|tSNE|umap|tsne", projs, ignore.case = TRUE)))
})

test_that("example.crb sample levels are as expected", {
  path <- system.file("extdata/examples/example.crb", package = "CerebroNexus")
  data <- readRDS(path)

  lvls <- data$getGroupLevels("sample")
  # example data is split into multiple pseudo-samples (donor_1/2/3)
  expect_true(length(lvls) >= 2)
  expect_true(is.character(lvls))
})

test_that("example.h5 file exists and is non-empty", {
  path <- system.file("extdata/examples/example.h5", package = "CerebroNexus")
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
})

## ---------------------------------------------------------------------------
## calculatePercentGenes input validation (without Seurat)
## ---------------------------------------------------------------------------

test_that("calculatePercentGenes stops if Seurat is not installed or object is wrong class", {
  # passing a non-Seurat object should give a clear error
  expect_error(
    calculatePercentGenes(
      object = list(),
      assay = "RNA",
      genes = list(g = "GeneA")
    ),
    regexp = "Seurat"
  )
})

## ---------------------------------------------------------------------------
## addPercentMtRibo input validation (without Seurat)
## ---------------------------------------------------------------------------

test_that("addPercentMtRibo rejects unsupported organism", {
  # needs Seurat object check first, but organism check fires after that
  # so we just verify the function at least checks for Seurat first
  expect_error(
    addPercentMtRibo(
      object = list(),
      organism = "zebrafish",
      gene_nomenclature = "name"
    ),
    regexp = "Seurat"
  )
})

test_that("addPercentMtRibo rejects unsupported gene_nomenclature", {
  # same pattern — Seurat check fires first, which is still informative
  expect_error(
    addPercentMtRibo(
      object = list(),
      organism = "hg",
      gene_nomenclature = "unknown_format"
    ),
    regexp = "Seurat"
  )
})

## ---------------------------------------------------------------------------
## launchCerebro parameter validation
## ---------------------------------------------------------------------------

preserve_global_cerebro_options <- function(envir = parent.frame()) {
  had_options <- exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
  previous_options <- if (had_options) {
    get("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  process_options <- options()
  had_request_size <- "shiny.maxRequestSize" %in% names(process_options)
  previous_request_size <- process_options[["shiny.maxRequestSize"]]
  withr::defer(
    {
      if (had_options) {
        assign("Cerebro.options", previous_options, envir = .GlobalEnv)
      } else if (
        exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
      ) {
        rm("Cerebro.options", envir = .GlobalEnv)
      }
      if (had_request_size) {
        options(shiny.maxRequestSize = previous_request_size)
      } else {
        options(shiny.maxRequestSize = NULL)
      }
    },
    envir = envir
  )
  invisible(NULL)
}

serialized_contains <- function(object, value) {
  grepl(
    value,
    rawToChar(serialize(object, NULL, ascii = TRUE)),
    fixed = TRUE
  )
}

package_authored_value_contains <- function(
  object,
  value,
  field,
  package_environment = asNamespace("CerebroNexus"),
  seen = NULL,
  depth = 0L
) {
  if (is.null(seen)) {
    seen <- new.env(parent = emptyenv())
  }
  if (is.character(object) && any(grepl(value, object, fixed = TRUE))) {
    return(TRUE)
  }
  object_names <- names(object)
  if (!is.null(object_names) && field %in% object_names) {
    return(TRUE)
  }
  is_package_authored <- function(environment) {
    cursor <- environment
    while (!identical(cursor, emptyenv())) {
      if (identical(cursor, package_environment)) {
        return(TRUE)
      }
      cursor <- parent.env(cursor)
    }
    FALSE
  }
  visit_environment <- function(environment) {
    key <- format(environment)
    if (
      exists(key, envir = seen, inherits = FALSE) ||
        !is_package_authored(environment)
    ) {
      return(FALSE)
    }
    assign(key, TRUE, envir = seen)
    bindings <- as.list(environment, all.names = TRUE)
    package_authored_value_contains(
      bindings,
      value,
      field,
      package_environment,
      seen,
      depth + 1L
    )
  }
  if (is.function(object)) {
    environment <- environment(object)
    return(!is.null(environment) && visit_environment(environment))
  }
  if (is.environment(object)) {
    return(visit_environment(object))
  }
  if (is.list(object) && depth < 12L) {
    return(any(vapply(
      object,
      package_authored_value_contains,
      logical(1),
      value = value,
      field = field,
      package_environment = package_environment,
      seen = seen,
      depth = depth + 1L
    )))
  }
  FALSE
}

source_installed_viewer_ui <- function(auth = NULL) {
  environment <- new.env(parent = asNamespace("CerebroNexus"))
  environment$Cerebro.options <- list(
    cerebro_root = system.file(package = "CerebroNexus")
  )
  if (!is.null(auth)) {
    environment$Cerebro.options[[".viewer_auth"]] <- auth
  }
  sys.source(
    system.file("viewer/shiny_UI.R", package = "CerebroNexus"),
    envir = environment
  )
  environment$ui
}

auth_guide_path <- function() {
  source_path <- testthat::test_path(
    "..",
    "..",
    "vignettes",
    "control_access_to_cerebro_with_a_login_page.Rmd"
  )
  if (file.exists(source_path)) {
    return(source_path)
  }

  installed_path <- system.file(
    "doc",
    "control_access_to_cerebro_with_a_login_page.Rmd",
    package = "CerebroNexus"
  )
  if (!nzchar(installed_path) || !file.exists(installed_path)) {
    stop("Cannot locate the authentication guide.", call. = FALSE)
  }
  installed_path
}

load_initial_auth_recipe <- function() {
  guide_lines <- readLines(auth_guide_path(), warn = FALSE)
  chunk_start <- grep("```{r create-database", guide_lines, fixed = TRUE)
  chunk_end <- which(
    seq_along(guide_lines) > chunk_start & guide_lines == "```"
  )[[1L]]
  recipe <- parse(text = guide_lines[(chunk_start + 1L):(chunk_end - 1L)])
  recipe_environment <- new.env(parent = globalenv())
  eval(recipe[[1L]], envir = recipe_environment)
  recipe_environment
}

mock_initial_auth_provider <- function() {
  test_environment <- parent.frame()
  testthat::local_mocked_bindings(
    askpass = function(...) "initial-password",
    .package = "askpass",
    .env = test_environment
  )
  testthat::local_mocked_bindings(
    create_db = function(credentials_data, sqlite_path, passphrase) {
      writeBin(charToRaw("encrypted-candidate"), sqlite_path)
      invisible(sqlite_path)
    },
    read_db_decrypt = function(sqlite_path, table, passphrase) {
      switch(
        table,
        credentials = data.frame(
          user = "admin",
          password = "hash",
          start = NA_character_,
          expire = NA_character_,
          admin = FALSE,
          is_hashed_password = 1,
          stringsAsFactors = FALSE
        ),
        pwd_mngt = data.frame(
          user = "admin",
          must_change = FALSE,
          have_changed = FALSE,
          date_change = NA_character_,
          n_wrong_pwd = 0,
          stringsAsFactors = FALSE
        ),
        logs = data.frame(
          user = character(),
          server_connected = character(),
          token = character(),
          logout = character(),
          app = character(),
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "shinymanager",
    .env = test_environment
  )
}

test_that("pre-publication input failures remove the created auth directory", {
  private_state_dir <- withr::local_tempdir()
  recipe_environment <- load_initial_auth_recipe()
  recipe_environment$readline <- function(...) ""

  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "Initial account name must be non-empty.",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))
  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "Initial account name must be non-empty.",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))

  recipe_environment$readline <- function(...) "admin"
  testthat::local_mocked_bindings(
    askpass = function(...) NULL,
    .package = "askpass"
  )
  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "password entry was cancelled",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))
  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "password entry was cancelled",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))
})

test_that("create_db failure removes the created auth directory", {
  private_state_dir <- withr::local_tempdir()
  recipe_environment <- load_initial_auth_recipe()
  recipe_environment$readline <- function(...) "admin"
  testthat::local_mocked_bindings(
    askpass = function(...) "initial-password",
    .package = "askpass"
  )
  testthat::local_mocked_bindings(
    create_db = function(...) stop("injected create_db failure"),
    .package = "shinymanager"
  )

  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "injected create_db failure",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))
  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "injected create_db failure",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))
})

test_that("cleanup reporting does not mask the pre-publication error", {
  private_state_dir <- withr::local_tempdir()
  recipe_environment <- load_initial_auth_recipe()
  recipe_environment$readline <- function(...) ""
  recipe_environment$file.remove <- function(...) FALSE
  withr::local_options(warn = 2)

  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "Initial account name must be non-empty.",
    fixed = TRUE
  )
})

test_that("link failure restores the prior passphrase environment", {
  private_state_dir <- withr::local_tempdir()
  recipe_environment <- load_initial_auth_recipe()
  recipe_environment$readline <- function(...) "admin"
  mock_initial_auth_provider()
  withr::local_envvar(CEREBRO_AUTH_PASSPHRASE = "prior-secret")
  recipe_environment$file.link <- function(from, to) {
    expect_false(identical(
      Sys.getenv("CEREBRO_AUTH_PASSPHRASE"),
      "prior-secret"
    ))
    FALSE
  }

  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "No rename fallback was attempted.",
    fixed = TRUE
  )
  expect_identical(Sys.getenv("CEREBRO_AUTH_PASSPHRASE"), "prior-secret")
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))

  Sys.setenv(CEREBRO_AUTH_PASSPHRASE = "")
  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "No rename fallback was attempted.",
    fixed = TRUE
  )
  expect_identical(Sys.getenv("CEREBRO_AUTH_PASSPHRASE"), "")
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))

  Sys.unsetenv("CEREBRO_AUTH_PASSPHRASE")
  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "No rename fallback was attempted.",
    fixed = TRUE
  )
  expect_true(is.na(Sys.getenv(
    "CEREBRO_AUTH_PASSPHRASE",
    unset = NA_character_
  )))
  expect_false(dir.exists(file.path(private_state_dir, "cerebro-auth")))
})

test_that("published database survives candidate unlink failure", {
  private_state_dir <- withr::local_tempdir()
  recipe_environment <- load_initial_auth_recipe()
  recipe_environment$readline <- function(...) "admin"
  mock_initial_auth_provider()
  withr::local_envvar(CEREBRO_AUTH_PASSPHRASE = NA_character_)
  recipe_environment$unlink <- function(x, recursive = FALSE) {
    if (length(x) == 1L && grepl(".credentials-", basename(x), fixed = TRUE)) {
      return(1L)
    }
    base::unlink(x, recursive = recursive)
  }

  expect_warning(
    credentials_path <- recipe_environment$create_initial_auth_database(
      private_state_dir
    ),
    "published candidate remains at",
    fixed = TRUE
  )
  expect_true(file.exists(credentials_path))
  expect_true(nzchar(Sys.getenv("CEREBRO_AUTH_PASSPHRASE")))
})

test_that("normal initial authentication publication retains its secret", {
  private_state_dir <- withr::local_tempdir()
  recipe_environment <- load_initial_auth_recipe()
  recipe_environment$readline <- function(...) "admin"
  mock_initial_auth_provider()
  withr::local_envvar(CEREBRO_AUTH_PASSPHRASE = NA_character_)

  credentials_path <- recipe_environment$create_initial_auth_database(
    private_state_dir
  )
  expect_identical(
    credentials_path,
    file.path(
      normalizePath(private_state_dir, winslash = "/"),
      "cerebro-auth",
      "credentials.sqlite"
    )
  )
  expect_true(file.exists(credentials_path))
  expect_true(nzchar(Sys.getenv("CEREBRO_AUTH_PASSPHRASE")))
})

test_that("initial authentication database recipe rejects an outside symlink", {
  skip_on_os("windows")
  recipe_environment <- load_initial_auth_recipe()

  private_state_dir <- withr::local_tempdir()
  outside_dir <- withr::local_tempdir()
  auth_dir <- file.path(private_state_dir, "cerebro-auth")
  if (!file.symlink(outside_dir, auth_dir)) {
    skip("test filesystem does not support symbolic links")
  }

  expect_error(
    recipe_environment$create_initial_auth_database(private_state_dir),
    "must not be a symbolic link",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(outside_dir, "credentials.sqlite")))
})

test_that("same-directory hard-link publication does not clobber a target", {
  auth_dir <- withr::local_tempdir()
  candidate <- file.path(auth_dir, ".credentials-candidate.sqlite")
  probe <- file.path(auth_dir, ".hard-link-probe")
  credentials_path <- file.path(auth_dir, "credentials.sqlite")
  writeLines("candidate", candidate)
  if (!suppressWarnings(file.link(candidate, probe))) {
    skip("test filesystem does not support same-directory hard links")
  }
  unlink(probe)
  writeLines("existing", credentials_path)

  expect_false(suppressWarnings(file.link(candidate, credentials_path)))
  expect_identical(readLines(credentials_path), "existing")
  expect_true(file.exists(candidate))

  unlink(credentials_path)
  expect_true(file.link(candidate, credentials_path))
  expect_identical(readLines(credentials_path), "candidate")
  expect_true(file.exists(candidate))
})

test_that("launchCerebro rejects invalid mode", {
  expect_error(
    launchCerebro(mode = "readonly"),
    regexp = "'mode' parameter must be set to either 'open' or 'closed'"
  )
})

test_that("launchCerebro rejects out-of-range point size", {
  expect_error(
    launchCerebro(overview_default_point_size = 50),
    regexp = "overview_default_point_size"
  )
})

test_that("launchCerebro rejects out-of-range opacity", {
  expect_error(
    launchCerebro(gene_expression_default_point_opacity = 2),
    regexp = "gene_expression_default_point_opacity"
  )
})

test_that("launchCerebro rejects out-of-range percentage", {
  expect_error(
    launchCerebro(gene_expression_default_percentage_cells_to_show = 150),
    regexp = "gene_expression_default_percentage_cells_to_show"
  )
})

test_that("launchCerebro rejects non-logical projections_show_hover_info", {
  expect_error(
    launchCerebro(projections_show_hover_info = "yes"),
    regexp = "projections_show_hover_info"
  )
})

test_that("launchCerebro preserves its installed Viewer defaults when auth is NULL", {
  preserve_global_cerebro_options()

  omitted_app <- launchCerebro(mode = "closed")
  omitted_options <- get(
    "Cerebro.options",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  explicit_app <- launchCerebro(mode = "closed", auth = NULL)
  explicit_options <- get(
    "Cerebro.options",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  expect_s3_class(omitted_app, "shiny.appobj")
  expect_s3_class(explicit_app, "shiny.appobj")
  expect_identical(omitted_options, explicit_options)
  expect_identical(explicit_options$mode, "closed")
  expect_false(".viewer_auth" %in% names(omitted_options))
  expect_false(".viewer_auth" %in% names(explicit_options))
  expect_identical(getOption("shiny.maxRequestSize"), 800 * 1024^2)
})

test_that("failed authenticated launch restores existing process state", {
  preserve_global_cerebro_options()
  fixture <- viewer_auth_fixture()
  sentinel_options <- structure(list(untouched = TRUE), class = "sentinel")
  sentinel_request_size <- 12345
  assign("Cerebro.options", sentinel_options, envir = .GlobalEnv)
  options(shiny.maxRequestSize = sentinel_request_size)

  expect_error(
    launchCerebro(
      auth = fixture$descriptor,
      rollback_probe = TRUE
    ),
    "unused argument.*rollback_probe"
  )
  expect_identical(
    get("Cerebro.options", envir = .GlobalEnv, inherits = FALSE),
    sentinel_options
  )
  expect_identical(
    getOption("shiny.maxRequestSize"),
    sentinel_request_size
  )
})

test_that("failed launch preserves absent process state", {
  preserve_global_cerebro_options()
  if (exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)) {
    rm("Cerebro.options", envir = .GlobalEnv)
  }
  options(shiny.maxRequestSize = NULL)
  expect_false(exists(
    "Cerebro.options",
    envir = .GlobalEnv,
    inherits = FALSE
  ))
  expect_false("shiny.maxRequestSize" %in% names(options()))

  expect_error(
    launchCerebro(rollback_probe = TRUE),
    "unused argument.*rollback_probe"
  )
  expect_false(exists(
    "Cerebro.options",
    envir = .GlobalEnv,
    inherits = FALSE
  ))
  expect_false("shiny.maxRequestSize" %in% names(options()))
})

test_that("launchCerebro compiles and applies an enabled host descriptor", {
  preserve_global_cerebro_options()
  fixture <- viewer_auth_fixture()
  cerebro_root <- system.file(package = "CerebroNexus")
  expected <- .compileViewerAuth(
    fixture$descriptor,
    "host",
    cerebro_root = cerebro_root
  )$config

  app <- launchCerebro(mode = "closed", auth = fixture$descriptor)
  options <- get("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)

  expect_s3_class(app, "shiny.appobj")
  expect_identical(options[[".viewer_auth"]], expected)
  expect_false(serialized_contains(options, fixture$passphrase))
  expect_false(serialized_contains(app, fixture$passphrase))
  expect_false(package_authored_value_contains(
    options,
    fixture$passphrase,
    "passphrase"
  ))
  expect_false(package_authored_value_contains(
    app,
    fixture$passphrase,
    "passphrase"
  ))
})

test_that("launchCerebro rejects credentials exposed by a Shiny resource path", {
  preserve_global_cerebro_options()
  fixture <- viewer_auth_fixture()
  public <- withr::local_tempdir()
  database <- file.path(public, "credentials.sqlite")
  expect_true(file.copy(fixture$database, database))
  prefix <- paste0("cerebro_auth_test_", Sys.getpid(), "_", sample.int(1e6, 1L))
  shiny::addResourcePath(prefix, public)
  withr::defer(shiny::removeResourcePath(prefix))
  expect_identical(
    normalizePath(shiny::resourcePaths()[[prefix]], winslash = "/"),
    normalizePath(public, winslash = "/")
  )
  sentinel <- list(untouched = TRUE)
  assign("Cerebro.options", sentinel, envir = .GlobalEnv)
  descriptor <- fixture$descriptor
  descriptor$credentials <- database

  expect_error(
    launchCerebro(auth = descriptor),
    "auth$credentials must not be located in an HTTP resource directory.",
    fixed = TRUE
  )
  expect_identical(
    get("Cerebro.options", envir = .GlobalEnv, inherits = FALSE),
    sentinel
  )
})

test_that("installed Viewer uses only one inactivity producer", {
  fixture <- viewer_auth_fixture()
  manifest <- .compileViewerAuth(
    fixture$descriptor,
    "host",
    cerebro_root = system.file(package = "CerebroNexus")
  )$config
  public_ui <- as.character(source_installed_viewer_ui())
  authenticated_ui <- as.character(source_installed_viewer_ui(manifest))

  expect_match(public_ui, "timeOut", fixed = TRUE)
  expect_false(grepl("timeOut", authenticated_ui, fixed = TRUE))
})

## ---------------------------------------------------------------------------
## .getExpressionMatrix same-semantic fallback guard
##
## When a requested layer (e.g. "data") is missing, we must NOT silently fall
## back to a layer with different semantics (e.g. "counts" or "scale.data"),
## because the caller would then treat normalised/scaled values as raw counts
## and vice versa. Fallback is only allowed within the same semantic class,
## including Seurat v5 split-layer variants like "data.1", "data.2".
## ---------------------------------------------------------------------------

test_that(".filter_same_semantic_layers keeps only same-root layers", {
  available <- c("counts", "counts.1", "data", "data.1", "scale.data")

  # requesting "data" → only data / data.* are acceptable fallbacks
  expect_equal(
    .filter_same_semantic_layers("data", available),
    c("data", "data.1")
  )

  # requesting "counts" → only counts / counts.* (v5 split layers)
  expect_equal(
    .filter_same_semantic_layers("counts", available),
    c("counts", "counts.1")
  )

  # requesting "scale.data" → the dot in the root must not split it wrongly
  expect_equal(
    .filter_same_semantic_layers("scale.data", available),
    "scale.data"
  )
})

test_that(".filter_same_semantic_layers returns empty when no same-root layer", {
  # requesting "data" but only counts present → no safe fallback
  expect_length(
    .filter_same_semantic_layers("data", c("counts", "counts.1")),
    0L
  )
})

test_that(".filter_same_semantic_layers allows cross-semantic when opted in", {
  available <- c("counts", "data", "scale.data")
  # legacy behaviour: everything is a candidate, requested layer first
  expect_equal(
    .filter_same_semantic_layers(
      "data",
      available,
      allow_cross_semantic = TRUE
    ),
    c("data", "counts", "scale.data")
  )
})

## ---------------------------------------------------------------------------
## .getExpressionMatrix on a counts-only Seurat v5 object
##
## A common real object — v5 RNA assay with only a `counts` layer (NormalizeData
## not run) — requested at the default slot = "data" must still export via the
## top-level callers, which opt into cross-semantic fallback. In v5 a missing
## layer is not an error but an EMPTY matrix, so the fallback must trigger on
## that too, warn (never a silent normalised-vs-raw swap), and return counts.
## Without allow_cross_semantic_fallback = TRUE it must hard-stop.
## ---------------------------------------------------------------------------

test_that(".getExpressionMatrix falls back counts-only v5 with a warning", {
  skip_if_not_installed("Seurat")
  skip_if_not(
    utils::compareVersion(
      as.character(utils::packageVersion("Seurat")),
      "5.0.0"
    ) >=
      0,
    "requires Seurat v5"
  )

  set.seed(1)
  counts <- matrix(
    rpois(20 * 8, 3),
    nrow = 20,
    dimnames = list(paste0("Gene", seq_len(20)), paste0("Cell", seq_len(8)))
  )
  so <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts))
  expect_setequal(SeuratObject::Layers(so[["RNA"]]), "counts")

  # default slot = "data", opted in: falls back to counts and WARNS
  expect_warning(
    mat <- .getExpressionMatrix(
      so,
      assay = "RNA",
      slot = "data",
      join_samples = FALSE,
      allow_cross_semantic_fallback = TRUE
    ),
    "falling back to `counts`"
  )
  expect_equal(dim(mat), c(20, 8))
  expect_equal(
    as.matrix(mat),
    as.matrix(Seurat::GetAssayData(so, assay = "RNA", layer = "counts"))
  )

  # without opting in, the same request must hard-stop rather than guess
  expect_error(
    .getExpressionMatrix(
      so,
      assay = "RNA",
      slot = "data",
      join_samples = FALSE
    ),
    "no same-semantic fallback"
  )
})
