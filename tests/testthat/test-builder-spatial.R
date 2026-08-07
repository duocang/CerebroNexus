builder_spatial_test_inst_path <- function(...) {
  relative <- file.path(...)
  path <- testthat::test_path("..", "..", "inst", relative)
  if (!file.exists(path)) {
    path <- system.file(relative, package = "CerebroNexus")
  }
  path
}

builder_spatial_test_source <- function(file, local = parent.frame()) {
  path <- builder_spatial_test_inst_path("builder", file)
  if (file.exists(path)) {
    sys.source(path, envir = local)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

sys.source(
  builder_spatial_test_inst_path(
    "viewer",
    "core",
    "spatial_coordinate_contract.R"
  ),
  envir = environment()
)
builder_spatial_test_source("spatial.R")
builder_spatial_test_source("preview.R")
builder_spatial_test_source("extras.R")

test_that("real Seurat image coordinates normalize by barcode", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object()
  image <- names(object@images)[[1L]]
  raw <- SeuratObject::GetTissueCoordinates(object[[image]])

  contract <- builder_spatial_contract(object, image = image)
  coordinates <- contract$coordinates
  source_index <- match(coordinates$cell_barcode, raw$cell)

  expect_named(coordinates, c("cell_barcode", "x", "y"))
  expect_identical(coordinates$cell_barcode, SeuratObject::Cells(object))
  expect_equal(coordinates$x, raw$x[source_index])
  expect_equal(coordinates$y, raw$y[source_index])
  expect_identical(contract$preview, contract$export)

  legacy <- builder_spatial_coords(object, image)
  expect_identical(legacy[[1L]], coordinates$x)
  expect_identical(legacy[[2L]], coordinates$y)
})

test_that("metadata coordinates require explicit x and y selections", {
  cells <- c("cell-a", "cell-b", "cell-c")
  metadata <- data.frame(
    quality = c(100, 200, 300),
    x = c(1, 2, 3),
    y = c(4, 5, 6),
    row.names = cells
  )

  expect_error(
    builder_spatial_contract(metadata, cells, source = "metadata"),
    "explicit"
  )
  contract <- builder_spatial_contract(
    metadata,
    cells,
    coord_cols = c("x", "y"),
    source = "metadata"
  )
  expect_identical(contract$coordinates$x, metadata$x)
  expect_identical(contract$coordinates$y, metadata$y)
  expect_false(identical(contract$coordinates$x, metadata$quality))
  expect_error(
    builder_spatial_contract(
      metadata,
      cells,
      coord_cols = c("x", "x"),
      source = "metadata"
    ),
    "distinct"
  )

  arbitrary <- data.frame(
    quality = c(9, 8, 7),
    foo = c(11, 12, 13),
    bar = c(21, 22, 23),
    row.names = cells
  )
  explicit <- builder_spatial_contract(
    arbitrary,
    cells,
    coord_cols = c("foo", "bar"),
    source = "metadata"
  )
  expect_identical(explicit$coordinates$x, arbitrary$foo)
  expect_identical(explicit$coordinates$y, arbitrary$bar)
})

test_that("Seurat metadata subclasses fail before row-name dispatch", {
  skip_if_not_installed("SeuratObject")
  touched <- FALSE
  assign(
    "dimnames.builder_seurat_metadata_trap",
    function(value) {
      touched <<- TRUE
      stop("untrusted Seurat metadata dimnames method executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("dimnames.builder_seurat_metadata_trap", envir = .GlobalEnv),
    add = TRUE
  )

  object <- builder_content_spatial_example_object()
  metadata <- methods::slot(object, "meta.data")
  metadata$x <- seq_len(nrow(metadata))
  metadata$y <- rev(metadata$x)
  class(metadata) <- c("builder_seurat_metadata_trap", "data.frame")
  methods::slot(object, "meta.data", check = FALSE) <- metadata

  expect_error(
    builder_spatial_contract(object, coord_cols = c("x", "y")),
    "unclassed base matrix|exact base data frame"
  )
  expect_false(touched)
})

test_that("Seurat coordinate aliases win over physical column order", {
  cells <- c("cell-a", "cell-b", "cell-c")
  coordinates <- data.frame(
    quality = c(99, 98, 97),
    y = c(40, 50, 60),
    x = c(10, 20, 30),
    cell = rev(cells)
  )

  contract <- builder_spatial_contract(
    coordinates,
    cells,
    barcodes = coordinates$cell,
    source = "seurat_image"
  )
  index <- match(cells, coordinates$cell)

  expect_identical(contract$coordinate_columns, c(x = "x", y = "y"))
  expect_identical(contract$coordinates$cell_barcode, cells)
  expect_identical(contract$coordinates$x, coordinates$x[index])
  expect_identical(contract$coordinates$y, coordinates$y[index])
})

test_that("spatial subsets filter outside rows before preview and export", {
  cells <- c("cell-a", "cell-b", "cell-c")
  metadata <- data.frame(
    foo = c(20, NA_real_, 10),
    bar = c(2, Inf, 1),
    row.names = c("cell-b", "outside", "cell-a")
  )
  contract <- builder_spatial_contract(
    metadata,
    cells,
    coord_cols = c("foo", "bar"),
    source = "metadata"
  )

  expect_identical(contract$match$extra, "outside")
  expect_identical(
    contract$coordinates$cell_barcode,
    c("cell-a", "cell-b")
  )
  expect_identical(contract$coordinates$x, c(10, 20))
  expect_false("outside" %in% contract$preview$cell_barcode)
  expect_false("outside" %in% contract$export$cell_barcode)
  expect_identical(contract$preview, contract$export)
})

test_that("spatial identity and coordinate damage fails closed", {
  cells <- c("cell-a", "cell-b", "cell-c")
  valid <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = cells[1:2]
  )

  duplicate <- valid
  attr(duplicate, "row.names") <- c("cell-a", "cell-a")
  expect_error(
    builder_spatial_contract(
      duplicate,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "duplicate"
  )

  blank <- valid
  attr(blank, "row.names") <- c("cell-a", "")
  expect_error(
    builder_spatial_contract(
      blank,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "blank"
  )

  expect_error(
    builder_spatial_contract(
      valid,
      c("cell-a", "cell-a", "cell-c"),
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "duplicate"
  )

  non_finite <- valid
  non_finite$x[[2L]] <- Inf
  expect_error(
    builder_spatial_contract(
      non_finite,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "finite"
  )
})

test_that("classed numeric spatial coordinates fail without method dispatch", {
  touched <- FALSE
  assign(
    "as.numeric.builder_coordinate_trap",
    function(value, ...) {
      touched <<- TRUE
      stop("untrusted coordinate conversion executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("as.numeric.builder_coordinate_trap", envir = .GlobalEnv),
    add = TRUE
  )
  cells <- c("cell-a", "cell-b")
  coordinates <- data.frame(
    x = structure(c(1, 2), class = "builder_coordinate_trap"),
    y = c(3, 4),
    row.names = cells
  )

  expect_error(
    builder_spatial_contract(
      coordinates,
      cells,
      coord_cols = c("x", "y"),
      source = "metadata"
    ),
    "unclassed|base integer|base double"
  )
  expect_false(touched)
})

test_that("classed spatial coordinate tables fail without method dispatch", {
  touched <- FALSE
  method_names <- paste0(
    c("as.data.frame.", "dim.", "dimnames.", "row.names."),
    "builder_table_trap"
  )
  for (method_name in method_names) {
    assign(
      method_name,
      function(value, ...) {
        touched <<- TRUE
        stop("untrusted coordinate table method executed")
      },
      envir = .GlobalEnv
    )
  }
  on.exit(rm(list = method_names, envir = .GlobalEnv), add = TRUE)

  cells <- c("cell-a", "cell-b")
  matrix_table <- matrix(
    c(1, 2, 3, 4),
    nrow = 2L,
    dimnames = list(cells, c("x", "y"))
  )
  class(matrix_table) <- "builder_table_trap"
  data_frame_table <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = cells
  )
  class(data_frame_table) <- c("builder_table_trap", "data.frame")

  for (kind in c("matrix", "data_frame")) {
    touched <- FALSE
    coordinate_table <- if (identical(kind, "matrix")) {
      matrix_table
    } else {
      data_frame_table
    }
    expect_error(
      builder_spatial_contract(
        coordinate_table,
        cells,
        coord_cols = c("x", "y"),
        source = "metadata"
      ),
      "unclassed base matrix|exact base data frame",
      info = kind
    )
    expect_false(touched, info = kind)
  }
})

test_that("all supported image channel kinds normalize to RGBA", {
  gray <- matrix(c(0, 0.25, 0.75, 1), nrow = 2L)
  gray_alpha <- array(
    c(gray, matrix(c(1, 0.75, 0.5, 0.25), nrow = 2L)),
    dim = c(2L, 2L, 2L)
  )
  rgb <- array(seq(0, 1, length.out = 12L), dim = c(2L, 2L, 3L))
  rgba <- array(seq(0, 1, length.out = 16L), dim = c(2L, 2L, 4L))

  normalized <- list(
    grayscale = builder_normalize_image(gray, 20L),
    grayscale_alpha = builder_normalize_image(gray_alpha, 20L),
    rgb = builder_normalize_image(rgb, 20L),
    rgba = builder_normalize_image(rgba, 20L)
  )

  for (kind in names(normalized)) {
    got <- normalized[[kind]]
    expect_null(got$error, info = kind)
    expect_identical(dim(got$array), c(2L, 2L, 4L), info = kind)
    expect_identical(got$source_channel_kind, kind, info = kind)
  }
  for (channel in 1:3) {
    expect_equal(normalized$grayscale$array[,, channel], gray)
    expect_equal(
      normalized$grayscale_alpha$array[,, channel],
      gray_alpha[,, 1L]
    )
  }
  expect_equal(normalized$grayscale$array[,, 4L], matrix(1, 2L, 2L))
  expect_equal(
    normalized$grayscale_alpha$array[,, 4L],
    gray_alpha[,, 2L]
  )
  expect_equal(normalized$rgba$array, rgba)
})

test_that("image normalization records dimensions and bounds display size", {
  image <- array(seq(0, 1, length.out = 8L * 4L * 3L), dim = c(4L, 8L, 3L))
  got <- builder_normalize_image(image, max_display_px = 3L)

  expect_identical(got$source_dimensions, c(width = 8L, height = 4L))
  expect_identical(
    got$display_dimensions,
    c(width = got$display_width, height = got$display_height)
  )
  expect_lte(max(got$display_dimensions), 3L)
  expect_identical(dim(got$array)[3L], 4L)
})

test_that("image normalization rejects unsafe arrays and display limits", {
  cases <- list(
    non_numeric = array("x", dim = c(2L, 2L, 3L)),
    non_finite = array(c(rep(0, 11L), Inf), dim = c(2L, 2L, 3L)),
    out_of_range = array(c(rep(0, 11L), 2), dim = c(2L, 2L, 3L)),
    zero_channels = array(numeric(), dim = c(2L, 2L, 0L)),
    five_channels = array(0, dim = c(2L, 2L, 5L)),
    bad_dimensions = structure(numeric(4L), dim = c(2L, 2L, 1L, 1L))
  )
  for (name in names(cases)) {
    got <- builder_normalize_image(cases[[name]], 20L)
    expect_type(got$error, "character")
    expect_true(nzchar(got$error), info = name)
  }

  valid <- array(0, dim = c(2L, 2L, 3L))
  for (limit in list(
    0,
    -1,
    NA_real_,
    Inf,
    .Machine$integer.max + 1,
    numeric(),
    c(1, 2)
  )) {
    got <- builder_normalize_image(valid, limit)
    expect_match(got$error, "display", ignore.case = TRUE)
  }
})

test_that("PNG and JPEG read while TIFF variants give conversion guidance", {
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  directory <- withr::local_tempdir()
  rgb <- array(seq(0, 1, length.out = 27L), dim = c(3L, 3L, 3L))
  png_path <- file.path(directory, "image.png")
  jpeg_path <- file.path(directory, "image.jpeg")
  png::writePNG(rgb, png_path)
  jpeg::writeJPEG(rgb, jpeg_path)

  expect_null(builder_read_image(png_path)$error)
  expect_null(builder_read_image(jpeg_path)$error)

  for (extension in c("tif", "tiff", "ome.tif", "ome.tiff")) {
    path <- file.path(directory, paste0("image.", extension))
    file.create(path)
    got <- builder_read_image(path)
    expect_match(got$error, "convert", ignore.case = TRUE, info = extension)
    expect_match(got$error, "PNG", fixed = TRUE, info = extension)
    expect_match(got$error, "JPEG", fixed = TRUE, info = extension)
  }
})

test_that("encoding round-trips grayscale alpha through RGBA", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  gray <- matrix(c(0, 0.25, 0.75, 1), nrow = 2L)
  alpha <- matrix(c(1, 0.5, 0.25, 0), nrow = 2L)
  gray_alpha <- array(c(gray, alpha), dim = c(2L, 2L, 2L))

  encoded <- builder_encode_image(gray_alpha, max_px = 10L)
  expect_null(encoded$error)
  expect_identical(encoded$source_channel_kind, "grayscale_alpha")
  raw <- base64enc::base64decode(sub("^[^,]+,", "", encoded$uri))
  path <- withr::local_tempfile(fileext = ".png")
  writeBin(raw, path)
  decoded <- png::readPNG(path)

  expect_identical(dim(decoded), c(2L, 2L, 4L))
  for (channel in 1:3) {
    expect_equal(decoded[,, channel], gray, tolerance = 1 / 255)
  }
  expect_equal(decoded[,, 4L], alpha, tolerance = 1 / 255)

  rotated <- builder_rotate_array(gray_alpha, 90)
  expect_identical(dim(rotated)[3L], 4L)
  expect_true(any(rotated[,, 4L] < 1))
})

test_that("quarter-turn rotations preserve every RGBA pixel exactly", {
  rgba <- array(
    seq(0.01, 0.96, length.out = 2L * 3L * 4L),
    dim = c(2L, 3L, 4L)
  )
  pixel_signatures <- function(image) {
    sort(as.vector(apply(image, c(1L, 2L), paste, collapse = ":")))
  }
  rotations <- lapply(
    c(`90` = 90, `180` = 180, `270` = 270, `-90` = -90),
    function(angle) {
      builder_rotate_array(rgba, angle)
    }
  )
  expected_dimensions <- list(
    `90` = c(3L, 2L, 4L),
    `180` = c(2L, 3L, 4L),
    `270` = c(3L, 2L, 4L),
    `-90` = c(3L, 2L, 4L)
  )

  for (angle in names(rotations)) {
    rotated <- rotations[[angle]]
    expect_identical(dim(rotated), expected_dimensions[[angle]], info = angle)
    expect_identical(
      pixel_signatures(rotated),
      pixel_signatures(rgba),
      info = angle
    )
    expect_identical(
      sort(as.vector(rotated[,, 4L])),
      sort(as.vector(rgba[,, 4L])),
      info = angle
    )
  }
  expect_identical(
    rotations[["180"]],
    rgba[
      rev(seq_len(dim(rgba)[1L])),
      rev(seq_len(dim(rgba)[2L])),
      ,
      drop = FALSE
    ]
  )
  expect_identical(rotations[["-90"]], rotations[["270"]])

  labelled <- array(1, dim = c(2L, 3L, 4L))
  labelled[,, 1L] <- matrix(1:6 / 10, nrow = 2L, byrow = TRUE)
  expect_identical(
    builder_rotate_array(labelled, 90)[,, 1L],
    matrix(c(3, 6, 2, 5, 1, 4) / 10, nrow = 3L, byrow = TRUE)
  )
  expect_identical(
    builder_rotate_array(labelled, -90)[,, 1L],
    matrix(c(4, 1, 5, 2, 6, 3) / 10, nrow = 3L, byrow = TRUE)
  )

  implementation <- paste(
    deparse(body(.builder_rotate_rgba)),
    collapse = "\n"
  )
  expect_false(grepl("aperm(", implementation, fixed = TRUE))
  expect_false(grepl("return(arr[", implementation, fixed = TRUE))
})

test_that("rotation plans preserve dimensions for thin images", {
  plan <- NULL
  expect_no_error(
    plan <- builder_rotation_plan(
      width = 1L,
      height = 4000L,
      degrees = 45,
      max_edge = 200L
    )
  )
  if (is.null(plan)) {
    return(invisible())
  }

  expect_identical(
    names(plan$input_dimensions),
    c("width", "height")
  )
  expect_lte(max(plan$output_dimensions), 200L)
})

test_that("valid thin images encode through the rotation plan", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  image <- array(
    seq(0.01, 0.99, length.out = 4000L * 4L),
    dim = c(4000L, 1L, 4L)
  )

  encoded <- builder_encode_image(image, max_px = 200L, rotate = 45)

  expect_null(encoded$error)
  expect_identical(
    encoded$source_dimensions,
    c(width = 1L, height = 4000L)
  )
  expect_lte(max(encoded$display_dimensions), 200L)
})

test_that("arbitrary rotation plans bound allocation before mapping", {
  expect_true(exists("builder_rotation_plan", mode = "function"))
  if (!exists("builder_rotation_plan", mode = "function")) {
    return(invisible())
  }

  plan <- builder_rotation_plan(
    width = 4000L,
    height = 4000L,
    degrees = 45,
    max_edge = 1400L
  )
  expect_identical(
    plan$full_extent_dimensions,
    c(width = 5657L, height = 5657L)
  )
  expect_lte(max(plan$output_dimensions), 1400L)
  expect_lt(max(plan$input_dimensions), 4000L)
  expect_true(plan$prescaled)

  moderate <- array(
    seq(0.01, 0.99, length.out = 80L * 100L * 4L),
    dim = c(80L, 100L, 4L)
  )
  rotated <- builder_rotate_array(moderate, 45, max_edge = 50L)
  expect_lte(max(dim(rotated)[1:2]), 50L)
  expect_identical(dim(rotated)[3L], 4L)
  expect_true(any(rotated[,, 4L] == 0))

  implementation <- paste(deparse(body(builder_rotate_array)), collapse = "\n")
  for (full_grid in c(
    "yy <- matrix",
    "xx <- matrix",
    "src_index <- ifelse"
  )) {
    expect_false(grepl(full_grid, implementation, fixed = TRUE))
  }

  encode_implementation <- paste(
    deparse(body(builder_encode_image)),
    collapse = "\n"
  )
  normalization_calls <- gregexpr(
    "builder_normalize_image",
    encode_implementation,
    fixed = TRUE
  )[[1L]]
  expect_identical(sum(normalization_calls > 0L), 1L)
})

test_that("arbitrary rotation preserves tiny raster pixels and alpha", {
  single <- array(c(0.2, 0.4, 0.6, 0.8), dim = c(1L, 1L, 4L))
  single_rotated <- builder_rotate_array(single, 45, max_edge = 10L)
  expect_true(any(single_rotated[,, 4L] == single[,, 4L]))
  expect_true(any(apply(
    single_rotated,
    c(1L, 2L),
    function(pixel) identical(as.numeric(pixel), as.numeric(single[1, 1, ]))
  )))

  strip <- array(0, dim = c(1L, 3L, 4L))
  strip[,, 1L] <- c(0.1, 0.2, 0.3)
  strip[,, 2L] <- c(0.4, 0.5, 0.6)
  strip[,, 3L] <- c(0.7, 0.8, 0.9)
  strip[,, 4L] <- c(0.25, 0.5, 0.75)
  strip_rotated <- builder_rotate_array(strip, 45, max_edge = 10L)
  expect_setequal(
    strip_rotated[,, 4L][strip_rotated[,, 4L] > 0],
    strip[,, 4L]
  )
})

test_that("rotation prescaling samples thin images from pixel centres", {
  plan <- builder_rotation_plan(
    width = 100L,
    height = 3L,
    degrees = 45,
    max_edge = 10L
  )
  expect_identical(plan$input_dimensions[["height"]], 1L)
  thin <- array(0, dim = c(3L, 100L, 4L))
  thin[1L, , 1L] <- 0.1
  thin[2L, , 1L] <- 0.5
  thin[3L, , 1L] <- 0.9
  thin[,, 4L] <- 1
  normalized <- builder_normalize_image(
    thin,
    max_display_px = plan$input_max_edge,
    display_dimensions = plan$input_dimensions
  )

  expect_identical(
    dim(normalized$array)[1:2],
    unname(rev(plan$input_dimensions))
  )
  expect_true(all(normalized$array[,, 1L] == 0.5))
})

test_that("small nonzero rotations keep extent and encoded pixels aligned", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  image <- array(
    seq(0.01, 0.99, length.out = 2L * 4L * 4L),
    dim = c(2L, 4L, 4L)
  )
  encoded <- builder_encode_image(image, max_px = 10L, rotate = 0.005)

  expect_identical(encoded$extent_dimensions, c(width = 5L, height = 3L))
  expect_identical(encoded$display_dimensions, encoded$extent_dimensions)
  bounds <- builder_image_bounds(
    "pixels",
    list(c(0, 1), c(0, 1)),
    encoded
  )
  expect_identical(
    c(width = bounds$xmax, height = bounds$ymax),
    encoded$extent_dimensions
  )
})

test_that("encoded display limits preserve transformed source extent", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  image <- array(
    seq(0, 1, length.out = 100L * 200L * 3L),
    dim = c(100L, 200L, 3L)
  )

  encoded <- builder_encode_image(image, max_px = 20L, rotate = 45)
  expect_lte(max(encoded$display_dimensions), 20L)
  expect_identical(encoded$source_dimensions, c(width = 200L, height = 100L))
  expect_identical(encoded$extent_dimensions, c(width = 213L, height = 213L))
  expect_identical(encoded$source_channel_kind, "rgb")
  expect_identical(encoded$display_channels, 4L)
  expect_identical(encoded$display_channel_kind, "rgba")
  expect_identical(encoded$channel_kind, "rgba")

  expect_identical(
    builder_image_bounds("pixels", list(1, 1), encoded),
    list(xmin = 0, xmax = 213L, ymin = 0, ymax = 213L)
  )
  expect_identical(
    builder_image_bounds(
      "physical",
      list(1, 1),
      encoded,
      um_per_px = 0.5
    ),
    list(xmin = 0, xmax = 106.5, ymin = 0, ymax = 106.5)
  )
})

test_that("rotated image extent drives bounds and Plotly aspect", {
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  skip_if_not_installed("plotly")
  image <- array(
    seq(0.01, 0.99, length.out = 2L * 4L * 4L),
    dim = c(2L, 4L, 4L)
  )
  expected <- list(
    `90` = c(width = 2L, height = 4L),
    `45` = c(width = 5L, height = 5L)
  )

  for (angle in names(expected)) {
    encoded <- builder_encode_image(
      image,
      max_px = 2L,
      rotate = as.numeric(angle)
    )
    expect_identical(
      encoded$source_dimensions,
      c(width = 4L, height = 2L),
      info = angle
    )
    expect_identical(
      encoded$extent_dimensions,
      expected[[angle]],
      info = angle
    )
    expect_lte(max(encoded$display_dimensions), 2L)
    bounds <- builder_image_bounds(
      "pixels",
      list(c(0, 1), c(0, 1)),
      encoded
    )
    expect_identical(
      bounds,
      list(
        xmin = 0,
        xmax = expected[[angle]][["width"]],
        ymin = 0,
        ymax = expected[[angle]][["height"]]
      ),
      info = angle
    )
    built <- plotly::plotly_build(builder_overlay_plot(
      data.frame(sx = c(0, 1), sy = c(0, 1)),
      encoded$uri,
      bounds
    ))
    layout_image <- built$x$layout$images[[1L]]
    expect_equal(
      layout_image$sizex / layout_image$sizey,
      expected[[angle]][["width"]] / expected[[angle]][["height"]],
      info = angle
    )
  }
})

test_that("alignment records propagate transformed extent facts", {
  picture <- list(
    uri = "data:image/png;base64,AAAA",
    bytes = 4L,
    width = 2L,
    height = 2L,
    source_width = 4L,
    source_height = 2L,
    extent_width = 2L,
    extent_height = 4L,
    display_width = 2L,
    display_height = 2L
  )
  per_section <- list(
    section = list(
      bounds = list(xmin = 0, xmax = 2L, ymin = 0, ymax = 4L),
      cover = list(outside = 0L, total = 2L)
    )
  )
  paired <- builder_pair_sections(picture, per_section)$section

  for (field in c(
    "source_width",
    "source_height",
    "extent_width",
    "extent_height",
    "display_width",
    "display_height"
  )) {
    expect_identical(paired[[field]], picture[[field]], info = field)
  }

  app <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "app.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  session <- paste(
    readLines(
      builder_spatial_test_inst_path("builder", "session.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(app, "extent_width = a\\$enc\\$extent_width")
  expect_match(app, "extent_height = a\\$enc\\$extent_height")
  expect_match(app, "extent_width = a\\$extent_width")
  expect_match(app, "extent_height = a\\$extent_height")
  expect_match(app, "nxt\\$extent_width")
  expect_match(app, "nxt\\$extent_height")
  expect_match(session, "extent_width")
  expect_match(session, "extent_height")
})

test_that("one slide applied to every section keeps each section's own extent", {
  picture <- list(
    uri = "data:image/png;base64,AAAA",
    bytes = 4L,
    width = 300L,
    height = 240L
  )
  per_section <- list(
    A = list(
      bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
      cover = list(outside = 0L, total = 100L)
    ),
    B = list(
      bounds = list(xmin = 500, xmax = 600, ymin = 0, ymax = 80),
      cover = list(outside = 0L, total = 100L)
    ),
    C = list(
      bounds = list(xmin = 2000, xmax = 2100, ymin = 0, ymax = 80),
      cover = list(outside = 7L, total = 100L)
    )
  )

  got <- builder_pair_sections(picture, per_section)
  expect_identical(unique(vapply(got, function(x) x$uri, "")), picture$uri)
  expect_identical(
    vapply(got, function(x) x$bounds$xmin, numeric(1)),
    c(A = 0, B = 500, C = 2000)
  )
  expect_identical(
    vapply(got, function(x) x$outside, integer(1)),
    c(A = 0L, B = 0L, C = 7L)
  )
})
