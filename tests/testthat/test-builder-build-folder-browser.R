library(shinytest2)

builder_build_folder_picker <- function(
  output_dir,
  .local_envir = parent.frame()
) {
  skip_on_os("windows")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fake_bin <- withr::local_tempdir(.local_envir = .local_envir)
  picker_name <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
    "osascript"
  } else {
    "zenity"
  }
  picker <- file.path(fake_bin, picker_name)
  writeLines(
    c(
      "#!/bin/sh",
      sprintf(
        "printf '%%s\\n' %s",
        shQuote(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
      )
    ),
    picker
  )
  Sys.chmod(picker, mode = "0755")
  withr::local_envvar(
    PATH = paste(fake_bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    .local_envir = .local_envir
  )
  invisible(output_dir)
}

test_that("native folder selection queues a Build after a flush", {
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  output_dir <- file.path(
    withr::local_tempdir(),
    "builder-native-folder-output"
  )
  builder_build_folder_picker(output_dir)

  app <- AppDriver$new(
    app_dir,
    name = "builder_native_folder_queue",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$wait_for_js(
    "document.querySelector('.example-btn[data-ex=basic_pbmc]') !== null",
    timeout = 10000
  )
  app$click(selector = ".example-btn[data-ex=basic_pbmc]")
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
      "document.getElementById('review_current_dataset') !== null && ",
      "document.getElementById('build') !== null"
    ),
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)
  app$set_inputs(make_app = FALSE)
  app$click("review_current_dataset")
  app$wait_for_js(
    paste0(
      "document.querySelector('.rail-review-status.reviewed') !== null && ",
      "!document.getElementById('build').disabled"
    ),
    timeout = 30000
  )

  app$run_js(paste0(
    "window.__builderBuildLabels = [];",
    "window.__builderBuildObserver = new MutationObserver(function () {",
    "const action = document.getElementById('build');",
    "if (action) window.__builderBuildLabels.push(action.textContent.trim());",
    "});",
    "window.__builderBuildObserver.observe(document.body, ",
    "{childList:true,subtree:true,characterData:true});"
  ))
  app$click("build")
  app$wait_for_js(
    "window.__builderBuildLabels.includes('Choose a folder…')",
    timeout = 10000
  )
  app$wait_for_idle(timeout = 10000)

  labels <- app$get_js("window.__builderBuildLabels")
  expect_true(
    "Building…" %in% labels,
    info = paste("Observed Build labels:", paste(labels, collapse = " -> "))
  )
})
