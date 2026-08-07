if (!exists("viewer_auth_fixture", mode = "function")) {
  source(testthat::test_path("helper-viewer-auth.R"), local = TRUE)
}

viewer_auth_wait_for_dom <- function(
  session,
  selector,
  present = TRUE,
  timeout = 30000
) {
  selector_json <- jsonlite::toJSON(selector, auto_unbox = TRUE)
  expected <- if (isTRUE(present)) "true" else "false"
  expression <- paste0(
    "new Promise(function(resolve, reject) {",
    "const selector = ",
    selector_json,
    ";",
    "const expected = ",
    expected,
    ";",
    "const deadline = Date.now() + ",
    as.integer(timeout),
    ";",
    "function poll() {",
    "const found = document.querySelector(selector) !== null;",
    "if (found === expected) { resolve(true); return; }",
    "if (Date.now() >= deadline) {",
    "reject(new Error('Timed out waiting for DOM selector: ' + selector));",
    "return;",
    "}",
    "window.setTimeout(poll, 25);",
    "}",
    "poll();",
    "})"
  )
  result <- session$Runtime$evaluate(
    expression = expression,
    awaitPromise = TRUE,
    returnByValue = TRUE,
    timeout_ = timeout + 5000
  )
  if (!is.null(result$exceptionDetails)) {
    detail <- result$exceptionDetails$exception$description
    testthat::fail(if (is.null(detail)) "DOM wait failed" else detail)
  }
  testthat::expect_true(isTRUE(result$result$value))
  invisible(TRUE)
}

viewer_auth_fill_login <- function(driver, user, password) {
  driver$wait_for_js(
    paste0(
      "document.querySelector('#auth-user_id.shiny-bound-input') !== null && ",
      "document.querySelector('#auth-user_pwd.shiny-bound-input') !== null && ",
      "document.querySelector('#auth-go_auth.shiny-bound-input') !== null"
    ),
    timeout = 30000
  )
  user_json <- jsonlite::toJSON(user, auto_unbox = TRUE)
  password_json <- jsonlite::toJSON(password, auto_unbox = TRUE)
  session <- driver$get_chromote_session()
  session$Runtime$evaluate(
    expression = paste0(
      "document.querySelector('#auth-user_id').focus();",
      "document.querySelector('#auth-user_id').select();"
    )
  )
  session$Input$insertText(text = user)
  session$Runtime$evaluate(
    expression = paste0(
      "document.querySelector('#auth-user_pwd').focus();",
      "document.querySelector('#auth-user_pwd').select();"
    )
  )
  session$Input$insertText(text = password)
  synced <- session$Runtime$evaluate(
    expression = paste0(
      "new Promise(function(resolve, reject) {",
      "const deadline = Date.now() + 5000;",
      "function poll() {",
      "const values = Shiny.shinyapp.$inputValues;",
      "if (values['auth-user_id'] === ",
      user_json,
      " && ",
      "values['auth-user_pwd:shiny.password'] === ",
      password_json,
      ") {",
      "resolve(true); return;",
      "}",
      "if (Date.now() >= deadline) {",
      "reject(new Error(JSON.stringify({",
      "keys: Object.keys(values),",
      "userMatches: values['auth-user_id'] === ",
      user_json,
      ",",
      "passwordMatches: values['auth-user_pwd:shiny.password'] === ",
      password_json,
      "})));",
      "return;",
      "}",
      "window.setTimeout(poll, 25);",
      "}",
      "poll();",
      "})"
    ),
    awaitPromise = TRUE,
    returnByValue = TRUE,
    timeout_ = 10000
  )
  if (!is.null(synced$exceptionDetails)) {
    detail <- synced$exceptionDetails$exception$description
    testthat::fail(if (is.null(detail)) "Input sync failed" else detail)
  }
  testthat::expect_true(isTRUE(synced$result$value))
  invisible(TRUE)
}

viewer_auth_login <- function(driver, user, password) {
  viewer_auth_fill_login(driver, user, password)
  driver$click(selector = "#auth-go_auth")
}

viewer_auth_login_wait_for_error <- function(
  driver,
  user,
  password,
  timeout = 30000
) {
  viewer_auth_fill_login(driver, user, password)
  expected <- "Username or password are incorrect"
  expected_json <- jsonlite::toJSON(expected, auto_unbox = TRUE)
  session <- driver$get_chromote_session()
  response <- session$Runtime$evaluate(
    expression = paste0(
      "(function() {",
      "const panel = document.querySelector('#auth-result_auth');",
      "const button = document.querySelector('#auth-go_auth');",
      "const oldNode = document.querySelector('.panel-auth .alert');",
      "const oldText = oldNode ? oldNode.innerText.trim() : null;",
      "return new Promise(function(resolve, reject) {",
      "let timer = null;",
      "const observer = new MutationObserver(check);",
      "function cleanup() { observer.disconnect(); clearTimeout(timer); }",
      "function check() {",
      "const current = document.querySelector('.panel-auth .alert');",
      "const text = current ? current.innerText.trim() : null;",
      "if (current && current !== oldNode && text === ",
      expected_json,
      ") {",
      "cleanup();",
      "resolve({oldText: oldText, newText: text, nodeReplaced: true});",
      "}",
      "}",
      "observer.observe(panel, {childList: true, subtree: true, characterData: true});",
      "timer = setTimeout(function() {",
      "cleanup();",
      "reject(new Error('Timed out waiting for a new authentication response'));",
      "}, ",
      as.integer(timeout),
      ");",
      "button.click();",
      "check();",
      "});",
      "})()"
    ),
    awaitPromise = TRUE,
    returnByValue = TRUE,
    timeout_ = timeout + 5000
  )
  if (!is.null(response$exceptionDetails)) {
    detail <- response$exceptionDetails$exception$description
    testthat::fail(
      if (is.null(detail)) "Login response wait failed" else detail
    )
  }
  response$result$value
}

viewer_auth_driver_log_text <- function(driver) {
  logs <- driver$get_logs()
  list(
    rows = nrow(logs),
    text = paste(unlist(logs, use.names = FALSE), collapse = "\n")
  )
}

test_that("real browser enforces viewer authentication and isolated sessions", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  chrome <- tryCatch(chromote::find_chrome(), error = function(error) "")
  skip_if(!nzchar(chrome), "Chrome is not available")

  fixture <- viewer_auth_fixture(envir = environment())
  descriptor <- fixture$descriptor
  descriptor$timeout_minutes <- 1L
  source_crb <- system.file(
    "extdata/examples/example.crb",
    package = "CerebroNexus"
  )
  skip_if(!nzchar(source_crb), "bundled example.crb is not available")
  app_dir <- file.path(fixture$root, "browser-app")
  createShinyApp(
    cerebro_data = c("Example" = source_crb),
    result_dir = app_dir,
    auth = descriptor,
    launch_browser = FALSE,
    verbose = FALSE
  )

  config <- readRDS(file.path(app_dir, "cerebro_config.rds"))
  bundled_crb <- file.path(app_dir, config$crb_file_to_load[[1L]])
  held_crb <- paste0(bundled_crb, ".held")
  expect_true(file.rename(bundled_crb, held_crb))
  crb_held <- TRUE
  on.exit(
    {
      if (crb_held && file.exists(held_crb)) {
        file.rename(held_crb, bundled_crb)
      }
    },
    add = TRUE
  )

  withr::local_envvar(
    stats::setNames(fixture$passphrase, fixture$env_name)
  )
  shinytest2::local_app_support(app_dir)
  driver <- shinytest2::AppDriver$new(
    app_dir,
    name = "viewer_auth_browser",
    load_timeout = 90000,
    timeout = 90000,
    height = 950,
    width = 1619
  )
  on.exit(driver$stop(), add = TRUE)

  driver$wait_for_js(
    "document.querySelector('.panel-auth') !== null",
    timeout = 30000
  )
  expect_equal(
    driver$get_js(
      "document.querySelector('.panel-auth .panel').getBoundingClientRect().width"
    ),
    448,
    tolerance = 0.5
  )
  for (selector in c("#auth-user_id", "#auth-user_pwd", "#auth-go_auth")) {
    expect_equal(
      driver$get_js(
        paste0(
          "document.querySelector('",
          selector,
          "').getBoundingClientRect().width"
        )
      ),
      384,
      tolerance = 0.5
    )
  }
  expect_false(isTRUE(driver$get_js(
    "document.querySelector('.main-sidebar') !== null"
  )))
  expect_false(isTRUE(driver$get_js(
    "document.querySelector('[id*=shinymanager_admin]') !== null"
  )))

  viewer_auth_login(driver, "viewer", "incorrect password")
  driver$wait_for_js(
    "document.querySelector('.panel-auth .alert') !== null",
    timeout = 30000
  )
  expect_identical(
    driver$get_js(
      "document.querySelector('.panel-auth .alert').innerText.trim()"
    ),
    "Username or password are incorrect"
  )
  expect_false(isTRUE(driver$get_js(
    "document.querySelector('.main-sidebar') !== null"
  )))

  expect_identical(
    driver$get_js(
      "document.querySelector('.panel-auth .alert').innerText.trim()"
    ),
    "Username or password are incorrect"
  )
  second_error <- viewer_auth_login_wait_for_error(
    driver,
    "unknown-viewer",
    "correct horse 47"
  )
  expect_true(isTRUE(second_error$nodeReplaced))
  expect_identical(
    second_error$oldText,
    "Username or password are incorrect"
  )
  expect_identical(
    second_error$newText,
    "Username or password are incorrect"
  )
  expect_identical(
    driver$get_js(
      "document.querySelector('.panel-auth .alert').innerText.trim()"
    ),
    "Username or password are incorrect"
  )
  expect_false(isTRUE(driver$get_js(
    "document.querySelector('.main-sidebar') !== null"
  )))

  driver$set_inputs(
    `auth-user_id` = "reviewer",
    `auth-user_pwd` = "review only 83",
    wait_ = FALSE
  )
  expect_true(file.rename(held_crb, bundled_crb))
  crb_held <- FALSE
  driver$click(input = "auth-go_auth")
  driver$wait_for_js(
    "document.querySelector('.main-sidebar') !== null",
    timeout = 90000
  )
  expect_false(isTRUE(driver$get_js(
    "document.querySelector('.panel-auth') !== null"
  )))
  expect_false(isTRUE(driver$get_js(
    "document.querySelector('[id*=shinymanager_admin]') !== null"
  )))
  driver$wait_for_js(
    paste0(
      "document.querySelector('#load_data_number_of_cells') !== null && ",
      "/1,?476/.test(",
      "document.querySelector('#load_data_number_of_cells').innerText",
      ")"
    ),
    timeout = 90000
  )
  cells <- driver$get_js(
    "document.querySelector('#load_data_number_of_cells').innerText"
  )
  expect_true(grepl("1,?476", cells))

  primary_url <- sub("[?].*$", "", driver$get_url())
  fresh <- driver$get_chromote_session()$new_session()
  on.exit(fresh$close(), add = TRUE)
  fresh$Page$navigate(url = primary_url)
  viewer_auth_wait_for_dom(fresh, ".panel-auth", timeout = 30000)
  expect_false(isTRUE(
    fresh$Runtime$evaluate(
      expression = "document.querySelector('.main-sidebar') !== null",
      returnByValue = TRUE
    )$result$value
  ))

  driver$click(selector = "[id='.shinymanager_logout']")
  driver$wait_for_js(
    "document.querySelector('.panel-auth') !== null",
    timeout = 30000
  )
  expect_false(isTRUE(driver$get_js(
    "document.querySelector('.main-sidebar') !== null"
  )))

  viewer_auth_login(driver, "reviewer", "review only 83")
  driver$wait_for_js(
    "document.querySelector('.main-sidebar') !== null",
    timeout = 90000
  )
  scripts <- driver$get_js(
    "Array.from(document.scripts).map(x => x.textContent).join('\\n')"
  )
  expect_false(grepl("timeOut", scripts, fixed = TRUE))
  driver$wait_for_js(
    "document.querySelector('.panel-auth') !== null",
    timeout = 90000
  )
  expect_false(isTRUE(driver$get_js(
    "document.querySelector('.main-sidebar') !== null"
  )))
  logs <- viewer_auth_driver_log_text(driver)
  expect_gt(logs$rows, 0L)
  expect_match(logs$text, "Shiny app started", fixed = TRUE)
  expect_false(grepl(fixture$passphrase, logs$text, fixed = TRUE))
})
