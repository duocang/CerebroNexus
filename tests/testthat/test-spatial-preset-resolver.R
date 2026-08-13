# Named spatial-image helpers consume only dataset -> FOV -> image manifests.

test_that("uploaded embedded images cannot inherit named-image settings", {
  settings <- list(
    spatial_image_settings = list(
      Configured = list(
        section_a = list(
          "Tissue background" = list(image_opacity = 0.4)
        )
      )
    )
  )

  expect_identical(
    resolve_spatial_image_setting(
      settings,
      NULL,
      "section_a",
      "Tissue background",
      "image_opacity",
      1
    ),
    1
  )
})

test_that("configured named-image lookup has a stable list contract", {
  settings <- list(
    spatial_images = list(
      Configured = list(
        section_a = list(
          "H&E" = "spatial-assets/configured/section_a/he.png"
        )
      )
    )
  )

  configured <- configured_spatial_images(
    settings,
    "Configured",
    "section_a"
  )
  expect_type(configured, "list")
  expect_named(configured, "H&E")
  expect_identical(
    configured[["H&E"]]$path,
    "spatial-assets/configured/section_a/he.png"
  )
  expect_identical(
    configured_spatial_images(settings, NULL, "section_a"),
    list()
  )
  expect_identical(
    configured_spatial_images(settings, "Configured", NULL),
    list()
  )
})

test_that("submitted backgrounds are normalized against the allowlist", {
  configured <- "spatial-assets/visium.png"

  expect_identical(
    normalize_spatial_background_choice(configured, configured),
    configured
  )
  expect_identical(
    normalize_spatial_background_choice("private-data/dataset.crb", configured),
    "No Background"
  )
  expect_identical(
    normalize_spatial_background_choice("../outside.png", configured),
    "No Background"
  )
  expect_identical(
    normalize_spatial_background_choice("__embedded__", configured, FALSE),
    "No Background"
  )
  expect_identical(
    normalize_spatial_background_choice("__embedded__", configured, TRUE),
    "__embedded__"
  )
})
