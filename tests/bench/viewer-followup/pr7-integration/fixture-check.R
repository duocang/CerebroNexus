# Offline output-body/data check, explicitly not a native-browser acceptance.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
pkgload::load_all(root, quiet = TRUE)
body_for <- function(file, id) {
  x <- as.list(parse(file.path(root, "inst/viewer/gene_expression", file)))
  Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
    is.call(e[[2]]) && identical(e[[2]][[1]], as.name("[[")) &&
    identical(e[[2]][[3]], id), x)[[1]][[3]][[2]]
}
summary_body <- body_for("UI_expression_in_selected_cells.R", "expression_in_selected_cells")
table_body <- body_for("UI_table_of_selected_cells.R", "expression_details_selected_cells")
env <- new.env(parent = globalenv())
sys.source(file.path(root, "inst/viewer/utility_functions.R"), env)
env$`%>%` <- dplyr::`%>%`
env$bind_cols <- dplyr::bind_cols
env$everything <- dplyr::everything
env$req <- shiny::req
env$input <- list()
env$expression_projection_summary_ready <- function() TRUE
env$plotExpressionSummary <- function(series, groups, colors) as.character(groups)
env$prettifyTable <- function(data, ...) data
env$expression_projection_selected_cells <- function() env$selection
old_message <- NULL
results <- lapply(seq_len(3), function(generation) {
  file <- if (generation == 2L) file.path(root, "inst/extdata/examples/example.crb") else args[[2]]
  obj <- env$.attachExternalExpression(qs2::qs_read(file), file)
  compact <- env$viewerProjectionFirstFrameCache(obj)
  env$reads <- 0L
  env$getMetaData <- function() { env$reads <- env$reads + 1L; obj$getMetaData() }
  first <- if (is.null(compact)) obj$getMetaData() else compact$meta_data
  n <- nrow(first)
  indices <- c(n, 1L, as.integer(n / 2))
  env$expression_projection_cells_to_show <- function() indices
  env$expression_projection_data <- function() first[indices, , drop = FALSE]
  env$expression_projection_coordinates <- function() data.frame(x = c(1, 1, 2), y = c(3, 3, 4))
  env$expression_summary_data <- function() list(kind = "mean", series = list(list(values = c(0, NA_real_, 9))))
  env$selection <- NULL
  for (body in list(summary_body, table_body)) {
    error <- tryCatch(eval(body, env), error = identity)
    stopifnot(inherits(error, "shiny.silent.error"))
  }
  stopifnot(env$reads == 0L)
  context <- env$viewerDatasetContext("fixture-session", if (generation == 2L) "Demo" else "Mouse", generation)
  if (!is.null(old_message)) stopifnot(is.null(env$viewerPersistentSelection(old_message, context)))
  # This models demanded aux identity; no expression backend read is needed.
  barcodes <- env$getMetaData()$cell_barcode[indices]
  message <- list(x = c(1, 2), y = c(3, 4), ids = barcodes[c(1, 3)], dataset_context = context)
  env$selection <- env$viewerPersistentSelection(message, context)
  groups <- eval(summary_body, env)
  table <- eval(table_body, env)
  stopifnot(identical(groups, c("selected", "not selected", "selected")),
    identical(table$cell_barcode, barcodes[c(1, 3)]),
    identical(table$expression_level, c(0, 9)))
  # The same published selection survives a same-dataset output revisit.
  stopifnot(identical(eval(table_body, env), table))
  old_message <<- message
  list(dataset = context$dataset_key, generation = generation, cells = n,
    indices = indices, selected = unname(table$cell_barcode), summary = groups,
    metadata_reads_before_selection = 0L, checks = "passed",
    crb_sha256 = digest::digest(file = file, algo = "sha256"))
})
jsonlite::write_json(list(environment = paste("WSL R", getRversion()),
  scope = "offline production render bodies; synthetic expression sentinels; no browser or performance claim",
  results = results), args[[3]], auto_unbox = TRUE, pretty = TRUE)
cat("PASS offline Mouse -> Demo -> Mouse, canonical subset/reorder, summary/table, pending gate and stale generation\n")
