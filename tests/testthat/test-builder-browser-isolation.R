test_that("old privacy contract app restores its namespace fixture", {
  namespace <- asNamespace("CerebroNexus")
  marker <- ".cerebro_bundle_privacy_contract_version"
  original_value <- get(marker, envir = namespace, inherits = FALSE)
  original_locked <- bindingIsLocked(marker, namespace)
  restore_original <- function() {
    if (bindingIsLocked(marker, namespace)) {
      unlockBinding(marker, namespace)
    }
    assign(marker, original_value, envir = namespace)
    if (original_locked) {
      lockBinding(marker, namespace)
    }
  }
  on.exit(restore_original(), add = TRUE)

  app_file <- builder_profile_inst_path("builder", "app.R")
  for (locked in c(TRUE, FALSE)) {
    if (bindingIsLocked(marker, namespace)) {
      unlockBinding(marker, namespace)
    }
    assign(marker, original_value, envir = namespace)
    if (locked) {
      lockBinding(marker, namespace)
    }

    invoke_fixture <- function() {
      app <- builder_browser_old_contract_app(app_file)
      expect_true(shiny::is.shiny.appobj(app))
      expect_identical(
        get(marker, envir = namespace, inherits = FALSE),
        0L
      )
      expect_true(bindingIsLocked(marker, namespace))
    }
    invoke_fixture()

    expect_identical(
      get(marker, envir = namespace, inherits = FALSE),
      original_value
    )
    expect_identical(bindingIsLocked(marker, namespace), locked)
  }
})
