spatial_contract_inst_path <- function(...) {
  relative <- file.path(...)
  path <- testthat::test_path("..", "..", "inst", relative)
  if (!file.exists(path)) {
    path <- system.file(relative, package = "CerebroNexus")
  }
  path
}

spatial_contract_core_path <- spatial_contract_inst_path(
  "viewer",
  "core",
  "spatial_coordinate_contract.R"
)
if (file.exists(spatial_contract_core_path)) {
  sys.source(spatial_contract_core_path, envir = environment())
}

test_that("the shared contract accepts every exporter coordinate alias", {
  contract <- .spx_coordinate_contract()

  for (alias in contract$x) {
    data <- stats::setNames(
      data.frame(one = 1:2, two = 3:4),
      c(alias, "y")
    )
    expect_identical(
      .spx_find_coordinate_columns(data),
      list(x = alias, y = "y"),
      info = paste("x alias", alias)
    )
  }
  for (alias in contract$y) {
    data <- stats::setNames(
      data.frame(one = 1:2, two = 3:4),
      c("x", alias)
    )
    expect_identical(
      .spx_find_coordinate_columns(data),
      list(x = "x", y = alias),
      info = paste("y alias", alias)
    )
  }
})

test_that("arbitrary numeric columns are not spatial coordinates", {
  data <- data.frame(foo = c(1, 2), bar = c(3, 4))

  expect_null(.spx_find_coordinate_columns(data))
  expect_identical(
    .spx_find_coordinate_columns(
      data,
      coord_cols = c("foo", "bar")
    ),
    list(x = "foo", y = "bar")
  )
})

test_that("the shared contract accepts every exporter barcode alias", {
  contract <- .spx_coordinate_contract()
  expected <- c("cell-a", "cell-b")

  for (alias in contract$barcode) {
    data <- stats::setNames(
      data.frame(value = expected, stringsAsFactors = FALSE),
      alias
    )
    expect_identical(
      .spx_find_barcode_column(data, expected),
      alias,
      info = paste("barcode alias", alias)
    )
  }
  expect_null(.spx_find_barcode_column(
    data.frame(sample = expected),
    expected
  ))
})

test_that("barcode alias precedence and overlap match the exporter", {
  data <- data.frame(
    cell_id = c("cell-a", "outside"),
    ID = c("cell-a", "cell-b"),
    name = c("cell-b", "cell-a"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    .spx_find_barcode_column(data, c("cell-a", "cell-b")),
    "ID"
  )
  data$cell_id <- c("cell-a", "cell-b")
  expect_identical(
    .spx_find_barcode_column(data, c("cell-a", "cell-b")),
    "cell_id"
  )
})

test_that("AsIs barcode columns retain exporter compatibility", {
  character_data <- data.frame(
    barcode = I(c("cell-a", "cell-b")),
    stringsAsFactors = FALSE
  )
  integer_data <- data.frame(barcode = I(c(101L, 102L)))

  expect_identical(
    .spx_find_barcode_column(
      character_data,
      c("cell-a", "cell-b")
    ),
    "barcode"
  )
  expect_identical(
    .spx_find_barcode_column(integer_data, c("101", "102")),
    "barcode"
  )
})

test_that("classed atomic barcodes never dispatch custom conversion", {
  touched <- FALSE
  assign(
    "as.character.spx_barcode_trap",
    function(value, ...) {
      touched <<- TRUE
      stop("untrusted method executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("as.character.spx_barcode_trap", envir = .GlobalEnv),
    add = TRUE
  )

  data <- data.frame(
    barcode = c("cell-a", "cell-b"),
    stringsAsFactors = FALSE
  )
  attr(data$barcode, "class") <- "spx_barcode_trap"

  expect_identical(
    .spx_find_barcode_column(data, c("cell-a", "cell-b")),
    "barcode"
  )
  expect_false(touched)

  data$barcode <- I(list("cell-a", "cell-b"))
  expect_null(.spx_find_barcode_column(data, c("cell-a", "cell-b")))
  expect_false(touched)
})

test_that("classed column names never dispatch custom methods", {
  touched <- FALSE
  assign(
    "as.character.spx_contract_trap",
    function(value, ...) {
      touched <<- TRUE
      stop("untrusted method executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("as.character.spx_contract_trap", envir = .GlobalEnv),
    add = TRUE
  )

  data <- data.frame(x = 1:2, y = 3:4)
  attr(data, "names") <- structure(
    c("x", "y"),
    class = "spx_contract_trap"
  )
  expect_null(.spx_find_coordinate_columns(data))
  expect_null(.spx_find_barcode_column(data, c("1", "2")))
  expect_false(touched)
})

test_that("the Builder and exporter consume the shared coordinate contract", {
  exporter_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "seurat_utils.R"
  )
  skip_if_not(
    file.exists(exporter_path),
    "source tree not present (installed-package layout)"
  )
  builder_path <- spatial_contract_inst_path(
    "builder",
    "content_spatial.R"
  )
  expect_true(file.exists(builder_path))
  exporter <- readLines(exporter_path, warn = FALSE)
  builder <- readLines(builder_path, warn = FALSE)
  for (consumer in list(exporter, builder)) {
    expect_true(any(grepl(
      ".spx_find_coordinate_columns",
      consumer,
      fixed = TRUE
    )))
    expect_true(any(grepl(
      ".spx_find_barcode_column",
      consumer,
      fixed = TRUE
    )))
  }
})

test_that("the bundled coordinate contract is byte-identical and safe", {
  source_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "spatial_coordinate_contract.R"
  )
  skip_if_not(
    file.exists(source_path),
    "R source tree not present (installed-package layout)"
  )
  expect_true(file.exists(spatial_contract_core_path))
  source_bytes <- readBin(
    source_path,
    what = "raw",
    n = file.info(source_path)$size
  )
  runtime_bytes <- readBin(
    spatial_contract_core_path,
    what = "raw",
    n = file.info(spatial_contract_core_path)$size
  )
  expect_identical(runtime_bytes, source_bytes)

  text <- paste(readLines(source_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("CerebroNexus|cerebroAppLite", text))
  expect_false(grepl(
    "library\\s*\\(|requireNamespace\\s*\\(|getFromNamespace\\s*\\(|::",
    text,
    perl = TRUE
  ))
})
