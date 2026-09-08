render_viewer_guides <- function(
  vignette_dir = "vignettes",
  output_dir = "inst/viewer/www/guides",
  slugs = NULL,
  quiet = FALSE
) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render Viewer guides.")
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required to render Viewer guides.")
  }
  vignette_dir <- normalizePath(vignette_dir, mustWork = TRUE)
  if (is.null(slugs)) {
    helper_env <- new.env(parent = globalenv())
    source("inst/viewer/guides/helpers.R", local = helper_env)
    slugs <- helper_env$viewerGuideCatalogue()$slug
  }
  inputs <- file.path(vignette_dir, paste0(slugs, ".Rmd"))
  missing <- slugs[!file.exists(inputs)]
  if (length(missing)) {
    stop("Missing guide source(s): ", paste(missing, collapse = ", "))
  }
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  image_dir <- file.path(vignette_dir, "img")
  if (dir.exists(image_dir)) {
    output_image_dir <- file.path(output_dir, "img")
    dir.create(output_image_dir, recursive = TRUE, showWarnings = FALSE)
    copied <- file.copy(
      list.files(image_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
      output_image_dir,
      recursive = TRUE,
      overwrite = TRUE
    )
    if (!all(copied)) {
      stop("Failed to copy guide image assets.")
    }
  }

  previous_eval <- knitr::opts_chunk$get("eval")
  previous_eval_hook <- knitr::opts_hooks$get("eval")
  on.exit(
    {
      knitr::opts_chunk$set(eval = previous_eval)
      knitr::opts_hooks$set(eval = previous_eval_hook)
    },
    add = TRUE
  )
  knitr::opts_chunk$set(eval = FALSE)
  knitr::opts_hooks$set(eval = function(options) {
    options$eval <- FALSE
    options
  })

  failures <- character()
  for (index in seq_along(slugs)) {
    error <- tryCatch(
      {
        rmarkdown::render(
          input = inputs[[index]],
          output_format = rmarkdown::html_document(
            self_contained = FALSE,
            lib_dir = file.path(output_dir, "site_libs"),
            mathjax = NULL,
            theme = NULL,
            highlight = NULL,
            toc = TRUE,
            toc_depth = 3
          ),
          output_file = paste0(slugs[[index]], ".html"),
          output_dir = output_dir,
          quiet = quiet,
          envir = new.env(parent = globalenv())
        )
        output_file <- file.path(output_dir, paste0(slugs[[index]], ".html"))
        html <- readLines(output_file, warn = FALSE)
        html <- gsub(
          paste0(vignette_dir, "/img/"),
          "img/",
          html,
          fixed = TRUE
        )
        html <- sub("[[:blank:]]+$", "", html)
        writeLines(html, output_file, useBytes = TRUE)
        NULL
      },
      error = function(error_condition) conditionMessage(error_condition)
    )
    if (!is.null(error)) {
      failures[[slugs[[index]]]] <- error
    }
  }
  if (length(failures)) {
    stop(
      "Failed to render Viewer guide(s): ",
      paste(
        paste0(names(failures), " (", unname(failures), ")"),
        collapse = "; "
      ),
      call. = FALSE
    )
  }
  slugs
}

if (sys.nframe() == 0L) {
  render_viewer_guides()
}
