library(shinytest2)

builder_motion_escape <- function(app) {
  app$run_js(paste0(
    "document.activeElement.dispatchEvent(new KeyboardEvent('keydown',",
    "{key:'Escape',bubbles:true}));"
  ))
}

test_that("Manager and confirmation dialogs animate state and restore focus", {
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  app <- AppDriver$new(
    app_dir,
    name = "builder_transient_motion",
    width = 768,
    height = 800,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  app$wait_for_js(
    "document.querySelector('.rail-summary') !== null",
    timeout = 10000
  )
  app$run_js(paste0(
    "const row = document.createElement('div');",
    "row.className = 'ds motion-dataset-row';",
    "const remove = document.createElement('button');",
    "remove.type = 'button';",
    "remove.className = 'builder-drop';",
    "remove.dataset.ds = 'motion-fixture';",
    "remove.dataset.confirm = 'true';",
    "remove.textContent = 'Remove';",
    "row.appendChild(remove);",
    "document.querySelector('.rail').appendChild(row);"
  ))

  app$click(selector = ".rail-summary")
  app$wait_for_js(
    paste0(
      "document.querySelector('.rail.is-manager-open.is-manager-visible') !== null && ",
      "document.querySelector('.rail-manager-backdrop.is-visible') !== null && ",
      "parseFloat(getComputedStyle(document.querySelector(",
      "'.rail-manager-backdrop')).opacity) > 0.9"
    ),
    timeout = 10000
  )
  expect_gt(
    as.numeric(app$get_js(paste0(
      "parseFloat(getComputedStyle(document.querySelector(",
      "'.rail-manager-backdrop')).opacity)"
    ))),
    0.9
  )

  builder_motion_escape(app)
  app$wait_for_js(
    paste0(
      "document.querySelector('.rail').getAttribute('aria-hidden') === 'true' && ",
      "!document.querySelector('.rail').classList.contains('is-manager-open')"
    ),
    timeout = 10000
  )
  expect_identical(
    app$get_js(
      "document.querySelector('.rail-summary').getAttribute('aria-expanded')"
    ),
    "false"
  )
  expect_true(app$get_js(
    "document.activeElement.classList.contains('rail-summary')"
  ))

  app$run_js(paste0(
    "window.__builderRapidManagerStable = false;",
    "document.querySelector('.rail-summary').click();",
    "document.querySelector('.rail-manager-close').click();",
    "document.querySelector('.rail-summary').click();",
    "window.setTimeout(function () {",
    "const rail = document.querySelector('.rail');",
    "window.__builderRapidManagerStable = ",
    "rail.classList.contains('is-manager-open') && ",
    "rail.classList.contains('is-manager-visible') && ",
    "document.querySelector('.rail-summary').getAttribute('aria-expanded') === 'true';",
    "}, 400);"
  ))
  app$wait_for_js(
    "window.__builderRapidManagerStable === true",
    timeout = 10000
  )
  app$click(selector = ".builder-drop")
  app$wait_for_js(
    paste0(
      "document.querySelector('.builder-confirm-backdrop.is-visible') !== null && ",
      "document.querySelector('.builder-confirm-dialog.is-visible') !== null"
    ),
    timeout = 10000
  )
  builder_motion_escape(app)
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog') === null",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.activeElement.classList.contains('builder-drop')"
  ))

  app$run_js(paste0(
    "window.__builderDropMessages = [];",
    "window.__builderOriginalSetInputValue = Shiny.setInputValue;",
    "Shiny.setInputValue = function(name, value, options) {",
    "if (name === 'drop_ds') {",
    "window.__builderDropMessages.push(value);",
    "return;",
    "}",
    "return window.__builderOriginalSetInputValue.apply(this, arguments);",
    "};"
  ))

  app$click(selector = ".builder-drop")
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog.is-visible') !== null",
    timeout = 10000
  )
  danger_style <- app$get_js(paste0(
    "(() => { const button = document.querySelector(",
    "'.builder-confirm-dialog .btn-remove-soft');",
    "const style = getComputedStyle(button);",
    "return {background: style.backgroundColor, color: style.color}; })()"
  ))
  expect_identical(danger_style$background, "rgb(255, 241, 242)")
  expect_identical(danger_style$color, "rgb(220, 38, 38)")
  app$run_js(paste0(
    "const dialog = document.querySelector('.builder-confirm-dialog');",
    "const cancel = dialog.querySelector('.btn:not(.btn-remove-soft)');",
    "const confirm = dialog.querySelector('.btn-remove-soft');",
    "cancel.click();",
    "confirm.click();"
  ))
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog') === null",
    timeout = 10000
  )
  app$run_js(paste0(
    "window.__builderCancelThenConfirmCount = window.__builderDropMessages.length;",
    "window.__builderDropMessages = [];"
  ))

  app$click(selector = ".builder-drop")
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog.is-visible') !== null",
    timeout = 10000
  )
  app$run_js(paste0(
    "const confirm = document.querySelector(",
    "'.builder-confirm-dialog .btn-remove-soft');",
    "confirm.click();",
    "confirm.click();"
  ))
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog') === null",
    timeout = 10000
  )
  drop_counts <- app$get_js(paste0(
    "({cancelThenConfirm: window.__builderCancelThenConfirmCount,",
    "doubleConfirm: window.__builderDropMessages.length})"
  ))
  app$run_js(paste0(
    "Shiny.setInputValue = window.__builderOriginalSetInputValue;",
    "delete window.__builderOriginalSetInputValue;",
    "delete window.__builderDropMessages;",
    "delete window.__builderCancelThenConfirmCount;"
  ))
  expect_identical(as.integer(drop_counts$cancelThenConfirm), 0L)
  expect_identical(as.integer(drop_counts$doubleConfirm), 1L)

  app$run_js(paste0(
    "window.__builderOriginalSetInputValue = Shiny.setInputValue;",
    "Shiny.setInputValue = function(name, value, options) {",
    "if (name === 'drop_ds') {",
    "const row = document.querySelector('.motion-dataset-row');",
    "if (row) row.remove();",
    "return;",
    "}",
    "return window.__builderOriginalSetInputValue.apply(this, arguments);",
    "};"
  ))
  app$click(selector = ".builder-drop")
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog.is-visible') !== null",
    timeout = 10000
  )
  app$click(selector = ".builder-confirm-dialog .btn-remove-soft")
  app$wait_for_js(
    "document.querySelector('.builder-confirm-dialog') === null",
    timeout = 10000
  )
  removal_focus <- app$get_js(paste0(
    "({isBody: document.activeElement === document.body,",
    "inRail: document.querySelector('.rail').contains(document.activeElement),",
    "className: document.activeElement.className || ''})"
  ))
  app$run_js(paste0(
    "Shiny.setInputValue = window.__builderOriginalSetInputValue;",
    "delete window.__builderOriginalSetInputValue;"
  ))
  expect_false(removal_focus$isBody)
  expect_true(removal_focus$inRail)
  expect_match(removal_focus$className, "rail-manager-close", fixed = TRUE)

  app$run_js(
    "document.documentElement.style.setProperty('--duration-normal', '160ms');"
  )
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(
      name = "prefers-reduced-motion",
      value = "no-preference"
    ))
  )
  app$wait_for_js("window.__builderMotionDuration === 160", timeout = 10000)

  app$run_js(
    "document.documentElement.style.setProperty('--duration-normal', '.16s');"
  )
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(
      name = "prefers-reduced-motion",
      value = "no-preference"
    ))
  )
  app$wait_for_js("window.__builderMotionDuration === 160", timeout = 10000)

  app$run_js(
    "document.documentElement.style.setProperty('--duration-normal', '160px');"
  )
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(
      name = "prefers-reduced-motion",
      value = "no-preference"
    ))
  )
  app$wait_for_js("window.__builderMotionDuration === 180", timeout = 10000)

  app$run_js(
    "document.documentElement.style.setProperty('--duration-normal', 'invalid');"
  )
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(
      name = "prefers-reduced-motion",
      value = "no-preference"
    ))
  )
  app$wait_for_js("window.__builderMotionDuration === 180", timeout = 10000)

  app$run_js(
    "document.documentElement.style.setProperty('--duration-normal', '-1s');"
  )
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(
      name = "prefers-reduced-motion",
      value = "no-preference"
    ))
  )
  app$wait_for_js("window.__builderMotionDuration === 180", timeout = 10000)

  app$run_js(
    "document.documentElement.style.setProperty('--duration-normal', ' ');"
  )
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(
      name = "prefers-reduced-motion",
      value = "no-preference"
    ))
  )
  app$wait_for_js("window.__builderMotionDuration === 180", timeout = 10000)

  app$get_chromote_session()$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-reduced-motion", value = "reduce"))
  )
  app$wait_for_js("window.__builderMotionDuration === 0", timeout = 10000)
  expect_identical(
    app$get_js("getComputedStyle(document.querySelector('.rail')).transform"),
    "none"
  )
  builder_motion_escape(app)
  app$wait_for_js(
    "!document.querySelector('.rail').classList.contains('is-manager-open')",
    timeout = 10000
  )
})
