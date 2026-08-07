##----------------------------------------------------------------------------##
## Parent-side ownership of Builder stages and release publication.
##----------------------------------------------------------------------------##

builder_coordinator_prepare <- function(plan, build_id) {
  if (
    !is.list(plan) ||
      !.builder_release_text(.builder_release_or(plan$out_dir, NULL))
  ) {
    stop("A frozen plan with an output release is required.", call. = FALSE)
  }
  output_release <- .builder_release_or(plan$output_release, list())
  expected <- .builder_release_or(
    output_release$targets,
    .builder_release_or(plan$targets, character())
  )
  relative <- vapply(
    expected,
    function(path) {
      .builder_release_relative(path, plan$out_dir)
    },
    ""
  )
  expected <- sort(unique(relative), method = "radix")
  prior <- .builder_release_or(
    plan$expected_prior_identity,
    builder_release_identity(plan$out_dir)
  )
  if (!.builder_release_identity_valid(prior)) {
    stop("The expected prior release identity is invalid.", call. = FALSE)
  }
  prior_paths <- vapply(prior$entries, `[[`, character(1), "path")
  expected_roots <- unique(sub("/.*$", "", expected))
  prior_roots <- unique(sub("/.*$", "", prior_paths))
  foreign <- setdiff(prior_roots, expected_roots)
  if (length(foreign)) {
    stop(
      "The output directory contains foreign release entries: ",
      paste(foreign, collapse = ", "),
      ". They were preserved.",
      call. = FALSE
    )
  }
  if (length(prior_paths) && !isTRUE(plan$overwrite)) {
    stop(
      "Known outputs already exist. Enable Replace existing outputs.",
      call. = FALSE
    )
  }
  handle <- builder_prepare_release(
    plan$out_dir,
    build_id,
    expected_prior = prior
  )
  handle$expected_targets <- expected
  class(handle) <- c("builder_release_coordinator", class(handle))
  handle
}

.builder_coordinator_handle <- function(handle) {
  if (!inherits(handle, "builder_release_coordinator")) {
    stop("A Builder release coordinator is required.", call. = FALSE)
  }
  .builder_release_handle(handle)
}

.builder_coordinator_stage_identity <- function(handle) {
  identity <- builder_release_identity(handle$stage)
  if (!isTRUE(identity$exists)) {
    stop("The coordinator-assigned stage is missing.", call. = FALSE)
  }
  present <- vapply(identity$entries, `[[`, character(1), "path")
  if (length(handle$expected_targets)) {
    missing <- setdiff(handle$expected_targets, present)
    if (length(missing)) {
      stop(
        "The verified stage is missing planned release entries: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    expected_roots <- unique(sub("/.*$", "", handle$expected_targets))
    present_roots <- unique(sub("/.*$", "", present))
    unplanned <- setdiff(present_roots, expected_roots)
    if (length(unplanned)) {
      stop(
        "The verified stage contains unplanned release entries: ",
        paste(unplanned, collapse = ", "),
        call. = FALSE
      )
    }
  }
  identity
}

builder_coordinator_publish <- function(handle, build_result) {
  handle <- .builder_coordinator_handle(handle)
  if (
    !is.list(build_result) ||
      !identical(build_result$state, "success") ||
      !isTRUE(build_result$publishable)
  ) {
    stop("Only a verified successful build can be published.", call. = FALSE)
  }
  result_stage <- tryCatch(
    .canonicalTargetPath(build_result$stage),
    error = function(error) ""
  )
  if (!identical(result_stage, .canonicalTargetPath(handle$stage))) {
    stop(
      "The build result did not come from the assigned stage.",
      call. = FALSE
    )
  }
  built <- .builder_release_or(build_result$built, character())
  if (
    !length(built) ||
      !all(vapply(
        built,
        .pathWithin,
        logical(1),
        parent = handle$stage
      ))
  ) {
    stop("Verified build artifacts escaped the assigned stage.", call. = FALSE)
  }
  .builder_coordinator_stage_identity(handle)
  published <- builder_publish_release(handle)
  relative_built <- vapply(
    built,
    .builder_release_relative,
    "",
    root = handle$stage
  )
  build_result$built <- file.path(published$target, relative_built)
  build_result$stage <- NULL
  build_result$published <- TRUE
  build_result$publishable <- FALSE
  build_result$release <- published
  build_result
}

builder_coordinator_abort <- function(handle) {
  handle <- .builder_coordinator_handle(handle)
  builder_abort_release(handle)
}

builder_coordinator_recovery <- function(target) {
  builder_discover_recovery(target)
}
