# Pure contracts for Viewer spatial background identity and settings.

spatial_options <- list(
  spatial_images = list(
    Atlas = list(
      sliceA = list(
        `H&E` = "spatial-assets/Atlas/sliceA/he.png",
        DAPI = list(
          path = "spatial-assets/Atlas/sliceA/dapi.png",
          bounds = c(xmin = 0, xmax = 100, ymin = 5, ymax = 95)
        )
      ),
      sliceB = c(IF = "spatial-assets/Atlas/sliceB/if.png"),
      sliceC = list()
    ),
    Other = list(
      sliceA = c(Histology = "spatial-assets/Other/sliceA/other.png")
    )
  ),
  spatial_image_settings = list(
    Atlas = list(
      sliceA = list(
        `H&E` = list(offset_x = 11, scale_x = 1.25, flip_y = TRUE),
        DAPI = list(offset_x = 22, rotation = 90)
      ),
      sliceB = list(IF = list(offset_x = 33, flip_x = TRUE))
    ),
    Other = list(sliceA = list(Histology = list(offset_x = 99)))
  )
)

test_that("configured images resolve one exact dataset and spatial leaf", {
  slice_a <- configured_spatial_images(spatial_options, "Atlas", "sliceA")

  expect_named(slice_a, c("H&E", "DAPI"))
  expect_identical(
    slice_a[["H&E"]],
    list(path = "spatial-assets/Atlas/sliceA/he.png", bounds = NULL)
  )
  expect_identical(
    slice_a$DAPI$bounds,
    c(xmin = 0, xmax = 100, ymin = 5, ymax = 95)
  )
  expect_named(
    configured_spatial_images(spatial_options, "Atlas", "sliceB"),
    "IF"
  )
  expect_identical(
    configured_spatial_images(spatial_options, "Atlas", "sliceC"),
    list()
  )
})

test_that("configured images fail closed without leaking neighbouring leaves", {
  expect_identical(
    configured_spatial_images(spatial_options, "Atlas", "missing"),
    list()
  )
  expect_identical(
    configured_spatial_images(spatial_options, "missing", "sliceA"),
    list()
  )
  expect_identical(configured_spatial_images(NULL, "Atlas", "sliceA"), list())
})

test_that("embedded images expose canonical labels and normalize singular legacy", {
  canonical <- list(
    histology_images = list(
      `H&E` = list(
        histology_image = "data:image/png;base64,HE",
        histology_image_bounds = c(xmin = 0, xmax = 10, ymin = 0, ymax = 20)
      ),
      DAPI = list(histology_image = "data:image/png;base64,DAPI")
    ),
    histology_image = "data:image/png;base64,STALE"
  )
  expect_named(embedded_spatial_images(canonical), c("H&E", "DAPI"))
  expect_identical(
    embedded_spatial_images(canonical)$DAPI$image,
    "data:image/png;base64,DAPI"
  )

  legacy <- list(
    histology_image = "data:image/png;base64,LEGACY",
    histology_image_bounds = c(xmin = 1, xmax = 2, ymin = 3, ymax = 4)
  )
  expect_named(embedded_spatial_images(legacy), "Tissue background")
  expect_identical(
    embedded_spatial_images(legacy)[["Tissue background"]]$bounds,
    legacy$histology_image_bounds
  )
  expect_identical(embedded_spatial_images(list()), list())
})

test_that("settings resolve only the exact image leaf", {
  expect_identical(
    resolve_spatial_image_setting(
      spatial_options,
      "Atlas",
      "sliceA",
      "H&E",
      "offset_x",
      0
    ),
    11
  )
  expect_identical(
    resolve_spatial_image_setting(
      spatial_options,
      "Atlas",
      "sliceA",
      "DAPI",
      "offset_x",
      0
    ),
    22
  )
  expect_identical(
    resolve_spatial_image_setting(
      spatial_options,
      "Atlas",
      "sliceB",
      "IF",
      "offset_x",
      0
    ),
    33
  )
  expect_identical(
    resolve_spatial_image_setting(
      spatial_options,
      "Atlas",
      "sliceC",
      "IF",
      "offset_x",
      0
    ),
    0
  )
  expect_identical(
    resolve_spatial_image_setting(
      spatial_options,
      "Atlas",
      "sliceA",
      "H&E",
      "rotation",
      0
    ),
    0
  )
})

test_that("background choices keep labels separate from source-tagged identity", {
  embedded <- embedded_spatial_images(list(
    histology_images = list(
      `H&E` = list(histology_image = "data:image/png;base64,HE")
    )
  ))
  external <- configured_spatial_images(spatial_options, "Atlas", "sliceA")
  choices <- spatial_background_choices(embedded, external)

  expect_identical(names(choices), c("No Background", "H&E", "H&E", "DAPI"))
  expect_identical(
    unname(choices),
    c(
      "none",
      spatial_background_key("embedded", "H&E"),
      spatial_background_key("external", "H&E"),
      spatial_background_key("external", "DAPI")
    )
  )
  expect_false(any(
    unname(choices) %in%
      c(
        "data:image/png;base64,HE",
        "spatial-assets/Atlas/sliceA/he.png"
      )
  ))
})

test_that("stale selections reset to the first current image or none", {
  slice_a_choices <- spatial_background_choices(
    list(),
    configured_spatial_images(spatial_options, "Atlas", "sliceA")
  )
  slice_b_choices <- spatial_background_choices(
    list(),
    configured_spatial_images(spatial_options, "Atlas", "sliceB")
  )
  slice_c_choices <- spatial_background_choices(list(), list())
  old <- spatial_background_key("external", "H&E")

  expect_identical(
    normalize_spatial_background_choice(old, slice_a_choices),
    old
  )
  expect_identical(
    normalize_spatial_background_choice(old, slice_b_choices),
    spatial_background_key("external", "IF")
  )
  expect_identical(
    normalize_spatial_background_choice(old, slice_c_choices),
    "none"
  )
})

test_that("selected identity resolves the matching descriptor and bounds", {
  embedded <- embedded_spatial_images(list(
    histology_images = list(
      DAPI = list(
        histology_image = "data:image/png;base64,DAPI",
        histology_image_bounds = c(xmin = 1, xmax = 9, ymin = 2, ymax = 8)
      )
    )
  ))
  external <- configured_spatial_images(spatial_options, "Atlas", "sliceA")

  expect_identical(
    resolve_spatial_background(
      spatial_background_key("embedded", "DAPI"),
      embedded,
      external
    )$bounds,
    c(xmin = 1, xmax = 9, ymin = 2, ymax = 8)
  )
  expect_identical(
    resolve_spatial_background(
      spatial_background_key("external", "DAPI"),
      embedded,
      external
    )$bounds,
    c(xmin = 0, xmax = 100, ymin = 5, ymax = 95)
  )
  expect_null(resolve_spatial_background("none", embedded, external))
})
