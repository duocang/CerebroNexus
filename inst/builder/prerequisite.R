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

builder_source_package_root <- function(
  candidates = c(
    Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = ""),
    file.path("..", ".."),
    file.path("..")
  )
) {
  candidates <- unique(candidates[nzchar(candidates)])
  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "DESCRIPTION")) &&
        dir.exists(file.path(candidate, "R")) &&
        length(list.files(
          file.path(candidate, "R"),
          pattern = "[.][Rr]$"
        )) >
          0L
    ) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  NULL
}

builder_source_app_contract_version <- function(
  source_root = builder_source_package_root()
) {
  if (is.null(source_root)) {
    return(NULL)
  }
  marker <- file.path(
    source_root,
    "inst",
    "builder",
    "app_bundle",
    "privacy-contract-version"
  )
  value <- tryCatch(
    readLines(marker, warn = FALSE, n = 2L),
    error = function(e) character()
  )
  if (identical(value, "1")) 1L else 0L
}

builder_activate_source_package <- function(source_root = NULL) {
  source_root <- if (is.null(source_root)) {
    builder_source_package_root()
  } else {
    builder_source_package_root(source_root)
  }
  if (is.null(source_root)) {
    return(invisible(NULL))
  }
  Sys.setenv(CEREBRO_PACKAGE_SOURCE = source_root)
  invisible(source_root)
}

builder_app_capability <- function(
  installed_contract_version = builder_installed_app_contract_version(),
  source_contract_version = builder_source_app_contract_version()
) {
  contract_version <- if (is.null(source_contract_version)) {
    installed_contract_version
  } else {
    source_contract_version
  }
  version <- if (identical(contract_version, 1L)) 1L else 0L
  available <- identical(version, 1L)
  list(
    available = available,
    version = version,
    reason = if (available) {
      NULL
    } else {
      "This Builder cannot find a secure Viewer App export runtime."
    }
  )
}

.builder_prerequisite_install_command <- function(requirements) {
  packages <- unique(sub(" \\(.*$", "", requirements))
  quoted <- paste0('"', packages, '"')
  argument <- if (length(quoted) == 1L) {
    quoted
  } else {
    paste0("c(", paste(quoted, collapse = ", "), ")")
  }
  paste0("install.packages(", argument, ")")
}

builder_runtime_capability <- function(
  .available = function(package) requireNamespace(package, quietly = TRUE)
) {
  required <- c("callr", "openssl")
  missing <- required[!vapply(required, .available, logical(1))]
  list(
    available = !length(missing),
    missing = missing,
    reason = if (length(missing)) {
      paste(
        paste0(
          "Builder cannot start because ",
          if (length(missing) == 1L) {
            "this required R package is missing: "
          } else {
            "these required R packages are missing: "
          },
          paste(missing, collapse = ", "),
          "."
        ),
        paste0(
          "Run ",
          .builder_prerequisite_install_command(missing),
          ", then start Builder again."
        )
      )
    } else {
      NULL
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
      paste(
        paste0(
          "Login is unavailable because ",
          if (length(missing) == 1L) {
            "this required R package is missing or too old: "
          } else {
            "these required R packages are missing or too old: "
          },
          paste(missing, collapse = ", "),
          "."
        ),
        paste0(
          "Run ",
          .builder_prerequisite_install_command(missing),
          ", then restart Builder."
        )
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
