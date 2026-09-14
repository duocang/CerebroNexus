sys.source(
  builder_profile_inst_path("builder", "spatial.R"),
  envir = environment()
)
sys.source(
  builder_profile_inst_path("builder", "extras.R"),
  envir = environment()
)

test_that("Builder preserves uploaded PNG and JPEG bytes", {
  skip_if_not_installed("base64enc")
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  pixels <- array(seq(0, 1, length.out = 4L * 6L * 3L), c(4L, 6L, 3L))
  files <- c(
    png = withr::local_tempfile(fileext = ".png"),
    jpeg = withr::local_tempfile(fileext = ".jpg")
  )
  png::writePNG(pixels, files[["png"]])
  jpeg::writeJPEG(pixels, files[["jpeg"]], quality = 0.91)

  for (format in names(files)) {
    path <- files[[format]]
    mime <- if (identical(format, "png")) "image/png" else "image/jpeg"
    image <- builder_read_image(path)

    expect_null(image$error, info = format)
    expect_identical(
      image$source_uri,
      paste0("data:", mime, ";base64,", base64enc::base64encode(path)),
      info = format
    )
    expect_identical(
      image$source_content_md5,
      unname(as.character(tools::md5sum(path))),
      info = format
    )
    expect_identical(image$bytes, unname(file.size(path)), info = format)
    expect_identical(
      builder_read_image_uri(image$source_uri)[c("width", "height")],
      list(width = 6L, height = 4L),
      info = format
    )
  }
})

test_that("large images keep original bytes without an R raster copy", {
  skip_if_not_installed("base64enc")
  skip_if_not_installed("png")
  path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(0, nrow = 1400L, ncol = 1500L), path)

  image <- builder_read_image(path)

  expect_null(image$error)
  expect_identical(
    image$source_dimensions,
    c(width = 1500L, height = 1400L)
  )
  expect_null(image$array)
  expect_false("max_pixels" %in% names(formals(builder_read_image)))
  expect_false("max_pixels" %in% names(formals(builder_read_image_uri)))
  expect_identical(BUILDER_IMAGE_MAX_ENCODED_BYTES, 1024^3)
})

test_that("embedded images keep source pixels and declarative transforms", {
  record <- builder_alignment_record(
    source = list(name = "section.jpg", type = "image/jpeg"),
    source_uri = "data:image/jpeg;base64,SOURCE",
    uri = "data:image/png;base64,BAKED",
    base_bounds = list(xmin = 0, xmax = 20, ymin = 10, ymax = 30),
    parameters = list(
      dx = 4,
      dy = -2,
      scale = 1.5,
      rotation = 37,
      flip_x = TRUE
    ),
    section = list(id = "fov-a", kind = "spatial")
  )

  payload <- builder_histology_image_payload(record)

  expect_identical(payload$histology_image, record$source_uri)
  expect_equal(payload$histology_image_bounds, unlist(record$base_bounds))
  expect_identical(
    payload$histology_alignment,
    builder_alignment_payload(record)
  )

  normalized <- CerebroNexus:::.normalizeEmbeddedSpatialImages(
    list(section = payload),
    data.frame(x = c(0, 20), y = c(10, 30)),
    "Spatial data"
  )
  expect_identical(
    normalized$section$histology_alignment,
    payload$histology_alignment
  )
})

test_that("Builder image upload no longer re-encodes display pixels", {
  extras <- paste(
    readLines(
      builder_profile_inst_path("builder", "extras.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  server <- paste(
    readLines(
      builder_profile_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  client <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("builder_encode_image <-", extras, fixed = TRUE))
  expect_false(grepl("builder_encode_image(", server, fixed = TRUE))
  expect_false(grepl("max_px = 1400", server, fixed = TRUE))
  expect_false(grepl("resizeTissueImageBeforeUpload", client, fixed = TRUE))
  expect_false(grepl("TISSUE_IMAGE_MAX_EDGE", client, fixed = TRUE))
})
