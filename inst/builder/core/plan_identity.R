## Portable identity for the user-confirmed Builder publication plan.

.builder_plan_identity_review_fields <- c(
  "dataset_order",
  "items",
  "manifest",
  "acknowledgements"
)

.builder_plan_identity_strip_runtime <- function(value, fields) {
  if (!is.list(value)) {
    return(value)
  }
  for (field in intersect(names(value), fields)) {
    value[[field]] <- NULL
  }
  lapply(value, .builder_plan_identity_strip_runtime, fields = fields)
}

.builder_plan_review_identity <- function(plan) {
  identity <- plan[.builder_plan_identity_review_fields]
  identity$items <- lapply(identity$items, function(item) {
    item$tables <- .builder_plan_identity_strip_runtime(
      item$tables,
      c("source_path", "table", "project_asset")
    )
    item$images <- .builder_plan_identity_strip_runtime(
      item$images,
      c("source_path", "source_uri", "uri", "project_asset")
    )
    item
  })
  identity
}

.builder_publication_plan_review_identity <- function(plan) {
  identity <- .builder_plan_review_identity(plan)
  identity$items <- lapply(identity$items, function(item) {
    item$source_snapshot_identity <- NULL
    reused <- item$reused_artifact
    if (is.list(reused)) {
      item$reused_artifact <- list(
        fingerprint = reused$fingerprint %||% list(),
        members = .builder_plan_identity_strip_runtime(
          reused$members %||% list(),
          c(
            "path",
            "resolved_path",
            "source_path",
            "object_file",
            "owner_token",
            "created_at"
          )
        )
      )
    }
    item
  })
  identity
}

.builder_plan_relative_target <- function(path, root) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      !is.character(root) ||
      length(root) != 1L ||
      is.na(root) ||
      !nzchar(root)
  ) {
    stop("A portable Builder plan target is invalid.", call. = FALSE)
  }
  path <- gsub("\\", "/", path, fixed = TRUE)
  root <- sub("/+$", "", gsub("\\", "/", root, fixed = TRUE))
  prefix <- paste0(root, "/")
  if (!startsWith(path, prefix)) {
    stop(
      "A portable Builder plan target escaped its release root.",
      call. = FALSE
    )
  }
  substring(path, nchar(prefix) + 1L)
}

builder_publication_plan_payload <- function(plan) {
  if (
    !is.list(plan) ||
      !inherits(plan, "builder_build_plan") ||
      !identical(plan$readiness, "ready")
  ) {
    stop("A ready frozen BuildPlan is required.", call. = FALSE)
  }
  targets <- plan$output_release$targets %||% plan$targets %||% character()
  out_dir <- plan$output_release$directory %||% plan$out_dir %||% NULL
  if (!is.character(targets) || anyNA(targets)) {
    stop("Portable Builder plan targets are invalid.", call. = FALSE)
  }
  relative_targets <- vapply(
    targets,
    .builder_plan_relative_target,
    character(1),
    root = out_dir
  )
  app_auth <- plan$app_auth %||% list()
  list(
    schema_version = 1L,
    plan_revision = as.integer(plan$revision %||% 0L),
    review = .builder_publication_plan_review_identity(plan),
    output = list(
      overwrite = isTRUE(plan$overwrite),
      targets = unname(relative_targets),
      make_app = isTRUE(plan$make_app),
      app_contract_version = plan$app_contract_version %||% NULL,
      app_options = plan$app_options %||% list(),
      app_auth = list(
        enabled = isTRUE(app_auth$enabled),
        account_count = as.integer(app_auth$account_count %||% 0L),
        timeout_minutes = as.integer(app_auth$timeout_minutes %||% 15L)
      )
    )
  )
}

.builder_plan_canonicalize_json <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  value <- lapply(value, .builder_plan_canonicalize_json)
  value_names <- names(value)
  if (!is.null(value_names)) {
    value <- value[order(value_names, method = "radix")]
  }
  value
}

builder_publication_plan_json <- function(plan) {
  value <- jsonlite::fromJSON(
    jsonlite::toJSON(
      builder_publication_plan_payload(plan),
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA,
      pretty = FALSE
    ),
    simplifyVector = FALSE
  )
  as.character(jsonlite::toJSON(
    .builder_plan_canonicalize_json(value),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  ))
}

builder_publication_plan_digest <- function(plan) {
  path <- tempfile("builder-plan-identity-", fileext = ".json")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeLines(
    enc2utf8(builder_publication_plan_json(plan)),
    path,
    useBytes = TRUE
  )
  unname(as.character(tools::md5sum(path)))
}
