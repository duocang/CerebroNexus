builder_auth_browser_dir <- builder_profile_inst_path("builder")

builder_auth_browser_require <- function() {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  skip_if_not_installed("openssl")
}

builder_auth_browser_teardown <- function(app) {
  try(
    app$run_js(paste0(
      "if (window.__authOriginalSetInputValue) {",
      "Shiny.setInputValue = window.__authOriginalSetInputValue;",
      "delete window.__authOriginalSetInputValue;",
      "}"
    )),
    silent = TRUE
  )
  app$stop()
}

builder_auth_browser_restore_interceptor <- function(app) {
  app$run_js(paste0(
    "if (window.__authOriginalSetInputValue) {",
    "Shiny.setInputValue = window.__authOriginalSetInputValue;",
    "delete window.__authOriginalSetInputValue;",
    "}"
  ))
}

builder_auth_browser_picker <- function(
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

builder_auth_browser_load_example <- function(app, example = "basic_pbmc") {
  app$wait_for_js(
    sprintf(
      "document.querySelector('.example-btn[data-ex=%s]') !== null",
      example
    ),
    timeout = 10000
  )
  app$click(selector = sprintf(".example-btn[data-ex=%s]", example))
  app$wait_for_js(
    "document.querySelector('.ds-pick[aria-current=true]') !== null",
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)
}

builder_auth_browser_intercept_inputs <- function(app) {
  app$run_js(paste0(
    "window.__authInputs = [];",
    "window.__authOriginalSetInputValue = Shiny.setInputValue;",
    "Shiny.setInputValue = function(name, value, options) {",
    "if (name === 'builder_auth_accounts') {",
    "window.__authInputs.push(value === null ? null : JSON.parse(JSON.stringify(value)));",
    "}",
    "return window.__authOriginalSetInputValue.call(Shiny, name, value, options);",
    "};"
  ))
}

builder_auth_browser_hold_inputs <- function(app) {
  app$run_js(paste0(
    "window.__authInputs = [];",
    "window.__authOriginalSetInputValue = Shiny.setInputValue;",
    "Shiny.setInputValue = function(name, value, options) {",
    "if (name === 'builder_auth_accounts') {",
    "window.__authInputs.push(value === null ? null : JSON.parse(JSON.stringify(value)));",
    "return;",
    "}",
    "return window.__authOriginalSetInputValue.call(Shiny, name, value, options);",
    "};"
  ))
}

builder_auth_browser_open <- function(app) {
  app$run_js("document.querySelector('.review-app-options').open = true;")
  app$wait_for_js(
    "document.querySelector('.builder-auth-open').getClientRects().length > 0",
    timeout = 10000
  )
  app$click(selector = ".builder-auth-open")
  app$wait_for_js(
    "!document.getElementById('builder-auth-backdrop').hidden",
    timeout = 10000
  )
}

builder_auth_browser_set_row <- function(app, index, username, password) {
  app$run_js(sprintf(
    paste0(
      "(() => { const row = document.querySelectorAll('.builder-auth-row')[%d];",
      "row.querySelector('.builder-auth-username').value = %s;",
      "row.querySelector('.builder-auth-password').value = %s; })();"
    ),
    index - 1L,
    jsonlite::toJSON(username, auto_unbox = TRUE),
    jsonlite::toJSON(password, auto_unbox = TRUE)
  ))
}

test_that("Builder auth saves ordered accounts once and clears every browser copy", {
  builder_auth_browser_require()
  local_app_support(builder_auth_browser_dir)
  app <- shinytest2::AppDriver$new(
    builder_auth_browser_dir,
    name = "builder_auth_accounts",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(builder_auth_browser_teardown(app), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_auth_browser_load_example(app)
  builder_auth_browser_intercept_inputs(app)

  app$set_inputs(make_app = TRUE)
  app$wait_for_js(
    paste0(
      "document.getElementById('review-require_login') !== null && ",
      "!document.getElementById('review-require_login').disabled"
    ),
    timeout = 10000
  )
  app$run_js("document.querySelector('.review-app-options').open = true;")
  app$click(selector = "#review-require_login")
  app$wait_for_js(
    "document.querySelector('.builder-auth-open') !== null",
    timeout = 10000
  )
  builder_auth_browser_open(app)
  accounts <- builder_auth_test_accounts()
  builder_auth_browser_set_row(
    app,
    1L,
    accounts[[1L]]$username,
    accounts[[1L]]$password
  )
  app$click(selector = ".builder-auth-add")
  builder_auth_browser_set_row(
    app,
    2L,
    accounts[[2L]]$username,
    accounts[[2L]]$password
  )
  ids_before <- unlist(
    app$get_js(paste0(
      "Array.from(document.querySelectorAll('.builder-auth-row'))",
      ".map(row => row.dataset.authId)"
    )),
    use.names = FALSE
  )
  expect_identical(ids_before, c("auth-account-1", "auth-account-2"))
  app$run_js(paste0(
    "document.querySelector('.builder-auth-save').click();",
    "document.querySelector('.builder-auth-save').click();"
  ))
  app$wait_for_js(
    paste0(
      "document.getElementById('builder-auth-backdrop').hidden && ",
      "document.querySelectorAll('.builder-auth-row').length === 0 && ",
      "document.querySelectorAll('.builder-auth-row input').length === 0 && ",
      "Shiny.shinyapp.$inputValues.builder_auth_accounts === null"
    ),
    timeout = 10000
  )
  app$wait_for_idle(timeout = 10000)

  enabled_inputs <- app$get_js(
    "window.__authInputs.filter(value => value && value.enabled === true)"
  )
  expect_length(enabled_inputs, 1L)
  expect_identical(
    vapply(enabled_inputs[[1L]]$accounts, `[[`, character(1), "id"),
    ids_before
  )
  expect_identical(
    vapply(enabled_inputs[[1L]]$accounts, `[[`, character(1), "username"),
    trimws(vapply(accounts, `[[`, character(1), "username"))
  )
  dialog_html <- app$get_js(
    "document.getElementById('builder-auth-dialog').outerHTML"
  )
  sentinels <- unlist(
    lapply(accounts, function(account) {
      c(trimws(account$username), account$password)
    }),
    use.names = FALSE
  )
  expect_false(any(vapply(
    sentinels,
    grepl,
    logical(1),
    x = dialog_html,
    fixed = TRUE
  )))
  expect_false(grepl('value="auth-', dialog_html, fixed = TRUE))

  app$run_js("window.__authInputs = [];")
  app$wait_for_js(
    "document.getElementById('review-require_login').checked === true",
    timeout = 10000
  )
  app$run_js("document.querySelector('.review-app-options').open = true;")
  app$run_js(paste0(
    "window.__authCheckboxClicks = 0;",
    "window.__authBeforeChecked = ",
    "document.getElementById('review-require_login').checked;",
    "document.getElementById('review-require_login').addEventListener(",
    "'click', function() { window.__authCheckboxClicks += 1; }, {once:true});"
  ))
  app$click(selector = "#review-require_login")
  app$wait_for_idle(timeout = 10000)
  reset_state <- app$get_js(paste0(
    "(() => ({",
    "checked: document.getElementById('review-require_login').checked,",
    "beforeChecked: window.__authBeforeChecked,",
    "checkboxClicks: window.__authCheckboxClicks,",
    "checkboxConnected: document.getElementById('review-require_login').isConnected,",
    "checkboxRects: document.getElementById('review-require_login').getClientRects().length,",
    "wrapperActive: Shiny.setInputValue !== window.__authOriginalSetInputValue,",
    "inputs: window.__authInputs,",
    "lastInputIsNull: window.__authInputs.at(-1) === null,",
    "disabledBeforeReset: window.__authInputs.some((value, index) => ",
    "Boolean(value && value.enabled === false && ",
    "window.__authInputs.slice(index + 1).includes(null))),",
    "browserState: JSON.stringify(window.__authInputs),",
    "rows: document.querySelectorAll('.builder-auth-row').length,",
    "values: Array.from(document.querySelectorAll('.builder-auth-row input'))",
    ".map(input => input.value),",
    "html: document.documentElement.outerHTML",
    "}))()"
  ))
  expect_false(reset_state$checked)
  expect_true(
    reset_state$lastInputIsNull,
    info = paste("Captured auth inputs:", reset_state$browserState)
  )
  expect_true(
    reset_state$disabledBeforeReset,
    info = paste("Captured auth inputs:", reset_state$browserState)
  )
  expect_identical(reset_state$rows, 0L)
  expect_length(reset_state$values, 0L)
  expect_false(any(vapply(
    sentinels,
    grepl,
    logical(1),
    x = reset_state$html,
    fixed = TRUE
  )))
  expect_false(any(vapply(
    sentinels,
    grepl,
    logical(1),
    x = reset_state$browserState,
    fixed = TRUE
  )))
})

test_that("Builder auth accepts only the matching save acknowledgement", {
  builder_auth_browser_require()
  local_app_support(builder_auth_browser_dir)
  app <- shinytest2::AppDriver$new(
    builder_auth_browser_dir,
    name = "builder_auth_nonce_lock",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(builder_auth_browser_teardown(app), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_auth_browser_load_example(app)
  app$set_inputs(make_app = TRUE)
  app$wait_for_js(
    paste0(
      "document.getElementById('review-require_login') !== null && ",
      "!document.getElementById('review-require_login').disabled"
    ),
    timeout = 10000
  )
  app$run_js("document.querySelector('.review-app-options').open = true;")
  app$click(selector = "#review-require_login")
  app$wait_for_js("document.querySelector('.builder-auth-open') !== null")
  builder_auth_browser_open(app)
  accounts <- builder_auth_test_accounts()
  builder_auth_browser_set_row(
    app,
    1L,
    accounts[[1L]]$username,
    accounts[[1L]]$password
  )
  app$click(selector = ".builder-auth-add")
  builder_auth_browser_set_row(
    app,
    2L,
    accounts[[2L]]$username,
    accounts[[2L]]$password
  )
  builder_auth_browser_hold_inputs(app)
  app$click(selector = ".builder-auth-save")
  app$wait_for_js("window.__authInputs.length === 1", timeout = 10000)
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll(",
    "'#builder-auth-dialog input, #builder-auth-dialog button'))",
    ".every(control => control.disabled)"
  )))
  nonce <- app$get_js("window.__authInputs[0].nonce")
  app$run_js(sprintf(
    "window.__builderHandleAuthStatus({ok:true,nonce:%s});",
    jsonlite::toJSON(nonce + 1, auto_unbox = TRUE)
  ))
  expect_false(app$get_js(
    "document.getElementById('builder-auth-backdrop').hidden"
  ))
  expect_identical(
    app$get_js("document.querySelectorAll('.builder-auth-row').length"),
    2L
  )
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll(",
    "'#builder-auth-dialog input, #builder-auth-dialog button'))",
    ".every(control => control.disabled)"
  )))
  app$run_js(sprintf(
    "window.__builderHandleAuthStatus({ok:true,nonce:%s});",
    jsonlite::toJSON(nonce, auto_unbox = TRUE)
  ))
  app$wait_for_js(
    paste0(
      "document.getElementById('builder-auth-backdrop').hidden && ",
      "document.querySelectorAll('.builder-auth-row').length === 0 && ",
      "window.__authInputs.at(-1) === null"
    ),
    timeout = 10000
  )
})

test_that("Builder auth survives redraw and traps focus", {
  builder_auth_browser_require()
  local_app_support(builder_auth_browser_dir)
  output_dir <- file.path(withr::local_tempdir(), "builder-auth-output")
  builder_auth_browser_picker(output_dir)
  app <- shinytest2::AppDriver$new(
    builder_auth_browser_dir,
    name = "builder_auth_redraw_enqueue",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(builder_auth_browser_teardown(app), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_auth_browser_load_example(app)
  app$click(selector = ".example-btn[data-ex=spatial_multi_section]")
  app$wait_for_js(
    paste0(
      "document.querySelectorAll('.ds-pick').length === 2 && ",
      "document.querySelectorAll('.ds--import').length === 0"
    ),
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)
  builder_auth_browser_intercept_inputs(app)

  app$set_inputs(make_app = TRUE)
  app$wait_for_js(
    paste0(
      "document.getElementById('review-require_login') !== null && ",
      "!document.getElementById('review-require_login').disabled"
    ),
    timeout = 10000
  )
  app$run_js("document.querySelector('.review-app-options').open = true;")
  app$click(selector = "#review-require_login")
  app$wait_for_js("document.querySelector('.builder-auth-open') !== null")
  builder_auth_browser_open(app)
  accounts <- builder_auth_test_accounts()
  builder_auth_browser_set_row(
    app,
    1L,
    trimws(accounts[[1L]]$username),
    accounts[[1L]]$password
  )
  app$click(selector = ".builder-auth-add")
  builder_auth_browser_set_row(
    app,
    2L,
    accounts[[2L]]$username,
    accounts[[2L]]$password
  )
  app$click(selector = ".builder-auth-save")
  app$wait_for_js("document.getElementById('builder-auth-backdrop').hidden")
  app$wait_for_idle(timeout = 10000)
  builder_auth_browser_restore_interceptor(app)

  builder_auth_browser_open(app)
  app$click(selector = ".builder-auth-row:last-child .builder-auth-remove")
  builder_auth_browser_set_row(
    app,
    1L,
    paste0(accounts[[1L]]$username, "-edited"),
    paste0(accounts[[1L]]$password, "-edited")
  )
  app$click(selector = ".builder-auth-cancel")
  builder_auth_browser_open(app)
  restored <- app$get_js(paste0(
    "Array.from(document.querySelectorAll('.builder-auth-row')).map(row => ({",
    "id: row.dataset.authId,",
    "username: row.querySelector('.builder-auth-username').value,",
    "password: row.querySelector('.builder-auth-password').value}))"
  ))
  expect_identical(
    vapply(restored, `[[`, character(1), "id"),
    c("auth-account-1", "auth-account-2")
  )
  expect_identical(
    vapply(restored, `[[`, character(1), "username"),
    trimws(vapply(accounts, `[[`, character(1), "username"))
  )
  app$click(selector = ".builder-auth-cancel")

  app$run_js(paste0(
    "window.__authDialogNode = document.getElementById('builder-auth-dialog');",
    "window.__authWorkbenchNode = document.getElementById('workbench').firstElementChild;"
  ))
  app$click(selector = ".ds-pick:not([aria-current=true])")
  app$wait_for_js(
    paste0(
      "window.__authWorkbenchNode !== ",
      "document.getElementById('workbench').firstElementChild"
    ),
    timeout = 10000
  )
  expect_true(app$get_js(
    "window.__authDialogNode === document.getElementById('builder-auth-dialog')"
  ))
  builder_auth_browser_open(app)
  expect_identical(
    app$get_js("document.querySelector('.builder-auth-username').value"),
    trimws(accounts[[1L]]$username)
  )

  app$run_js(paste0(
    "document.querySelector('.builder-auth-save').focus();",
    "document.activeElement.dispatchEvent(new KeyboardEvent('keydown',",
    "{key:'Tab',bubbles:true}));"
  ))
  expect_true(app$get_js(
    "document.activeElement.classList.contains('builder-auth-username')"
  ))
  app$run_js(paste0(
    "document.activeElement.dispatchEvent(new KeyboardEvent('keydown',",
    "{key:'Tab',shiftKey:true,bubbles:true}));"
  ))
  expect_true(app$get_js(
    "document.activeElement.classList.contains('builder-auth-save')"
  ))
  app$run_js(paste0(
    "document.activeElement.dispatchEvent(new KeyboardEvent('keydown',",
    "{key:'Escape',bubbles:true}));"
  ))
  app$wait_for_js(
    "document.getElementById('builder-auth-backdrop').hidden",
    timeout = 10000
  )
  app$wait_for_idle(timeout = 10000)
  focus_state <- app$get_js(paste0(
    "(() => ({",
    "activeClass: document.activeElement.className,",
    "triggerCount: document.querySelectorAll('.builder-auth-open').length,",
    "triggerConnected: Boolean(document.querySelector('.builder-auth-open') && ",
    "document.querySelector('.builder-auth-open').isConnected),",
    "activeIsTrigger: document.activeElement === ",
    "document.querySelector('.builder-auth-open')",
    "}))()"
  ))
  expect_true(focus_state$triggerConnected)
  expect_true(
    focus_state$activeIsTrigger,
    info = paste("Active class after Escape:", focus_state$activeClass)
  )

  app$get_chromote_session()$set_viewport_size(width = 390, height = 844)
  app$wait_for_js("window.innerWidth === 390 && window.innerHeight === 844")
  builder_auth_browser_open(app)
  expect_lte(app$get_js("document.documentElement.scrollWidth"), 391)
  expect_lte(
    app$get_js(paste0(
      "document.getElementById('builder-auth-dialog')",
      ".getBoundingClientRect().right"
    )),
    390
  )
  app$click(selector = ".builder-auth-cancel")
})

test_that("Builder auth resets after a successful enqueue", {
  builder_auth_browser_require()
  local_app_support(builder_auth_browser_dir)
  output_dir <- file.path(withr::local_tempdir(), "builder-auth-enqueue-output")
  builder_auth_browser_picker(output_dir)
  app <- shinytest2::AppDriver$new(
    builder_auth_browser_dir,
    name = "builder_auth_enqueue_reset",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(builder_auth_browser_teardown(app), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_auth_browser_load_example(app)

  app$set_inputs(make_app = TRUE)
  app$wait_for_js(
    paste0(
      "document.getElementById('review-require_login') !== null && ",
      "!document.getElementById('review-require_login').disabled"
    ),
    timeout = 10000
  )
  app$run_js("document.querySelector('.review-app-options').open = true;")
  app$click(selector = "#review-require_login")
  app$wait_for_js(
    "document.querySelector('.builder-auth-open') !== null",
    timeout = 10000
  )
  builder_auth_browser_open(app)
  accounts <- builder_auth_test_accounts()
  builder_auth_browser_set_row(
    app,
    1L,
    accounts[[1L]]$username,
    accounts[[1L]]$password
  )
  app$click(selector = ".builder-auth-add")
  builder_auth_browser_set_row(
    app,
    2L,
    accounts[[2L]]$username,
    accounts[[2L]]$password
  )
  app$click(selector = ".builder-auth-save")
  app$wait_for_js(
    "document.getElementById('builder-auth-backdrop').hidden",
    timeout = 10000
  )
  app$wait_for_idle(timeout = 10000)

  app$click("review_current_dataset")
  app$wait_for_js(
    "document.querySelector('.rail-review-status.reviewed') !== null",
    timeout = 10000
  )
  app$wait_for_idle(timeout = 10000)
  app$wait_for_js(
    paste0(
      "document.getElementById('build') !== null && ",
      "!document.getElementById('build').disabled"
    ),
    timeout = 30000
  )
  builder_auth_browser_intercept_inputs(app)
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
  ## Enqueue starts the asynchronous build, so Shiny intentionally remains
  ## busy until the worker finishes. Wait for the auth reset contract below.
  app$wait_for_js(
    paste0(
      "window.__authInputs.at(-1) === null && ",
      "document.querySelectorAll('.builder-auth-row').length === 0 && ",
      "Shiny.shinyapp.$inputValues.builder_auth_accounts === null"
    ),
    timeout = 30000
  )
  browser_html <- app$get_js("document.documentElement.outerHTML")
  sentinels <- unlist(
    lapply(accounts, function(account) {
      c(trimws(account$username), account$password)
    }),
    use.names = FALSE
  )
  expect_false(any(vapply(
    sentinels,
    grepl,
    logical(1),
    x = browser_html,
    fixed = TRUE
  )))
})
