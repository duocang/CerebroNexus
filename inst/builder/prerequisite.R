##----------------------------------------------------------------------------##
## Gate app publication on the installed viewer's private-data contract.
##----------------------------------------------------------------------------##

builder_installed_app_contract_version <- function(namespace = NULL) {
  if (is.null(namespace)) {
    namespace <- tryCatch(
      asNamespace("CerebroNexus"),
      error = function(e) NULL
    )
  }
  if (!is.environment(namespace)) {
    return(0L)
  }

  marker <- ".cerebro_bundle_privacy_contract_version"
  if (!exists(marker, envir = namespace, inherits = FALSE)) {
    return(0L)
  }
  if (bindingIsActive(marker, namespace)) {
    return(0L)
  }
  is_lazy <- tryCatch(
    isTRUE(unname(rlang::env_binding_are_lazy(namespace, marker))),
    error = function(e) TRUE
  )
  if (is_lazy) {
    return(0L)
  }
  if (!bindingIsLocked(marker, namespace)) {
    return(0L)
  }
  version <- tryCatch(
    get(marker, envir = namespace, inherits = FALSE),
    error = function(e) NULL
  )
  if (identical(version, 1L)) 1L else 0L
}

builder_app_capability <- function(
  contract_version = builder_installed_app_contract_version()
) {
  version <- if (identical(contract_version, 1L)) 1L else 0L
  available <- identical(version, 1L)
  list(
    available = available,
    version = version,
    reason = if (available) {
      NULL
    } else {
      "Viewer app creation isn’t available in this installation. You can still build CRB files."
    }
  )
}

builder_auth_capability <- function(
  .available = function(package) requireNamespace(package, quietly = TRUE),
  .version = function(package) utils::packageVersion(package)
) {
  manager_available <- isTRUE(.available("shinymanager"))
  manager_version <- if (manager_available) {
    try(.version("shinymanager"), silent = TRUE)
  } else {
    NULL
  }
  manager_supported <- manager_available &&
    !inherits(manager_version, "try-error") &&
    isTRUE(manager_version >= base::package_version("1.1.0"))
  missing <- c(
    character(),
    if (!manager_supported) "shinymanager (>= 1.1.0)",
    if (!isTRUE(.available("openssl"))) "openssl"
  )
  list(
    available = !length(missing),
    missing = missing,
    reason = if (length(missing)) {
      paste0(
        "Login App creation requires: ",
        paste(missing, collapse = ", "),
        "."
      )
    } else {
      NULL
    }
  )
}

builder_app_control <- function(capability, current_value = NULL) {
  available <- is.list(capability) &&
    isTRUE(capability$available) &&
    identical(capability$version, 1L)
  checked <- available &&
    (is.null(current_value) || isTRUE(current_value))

  shiny::tagList(
    shiny::tags$fieldset(
      disabled = if (!available) "disabled",
      style = "border:0;padding:0;margin:0;min-width:0;",
      shiny::checkboxInput(
        "make_app",
        "Create a Viewer app",
        value = checked,
        width = "auto"
      )
    ),
    if (!available) {
      shiny::tags$div(
        class = "hint app-capability-reason",
        capability$reason
      )
    }
  )
}
