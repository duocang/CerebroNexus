##----------------------------------------------------------------------------##
## Analyses that can be run on the object before it is exported.
##
## Each one writes into `@misc` and lights up a page that would otherwise not
## appear. They differ enormously in cost -- seconds, minutes, or a few hundred
## HTTP requests -- so each carries an honest label rather than being offered as
## an undifferentiated list of checkboxes.
##
## Pure: no Shiny. Each step is a function of (object, settings) -> object.
##----------------------------------------------------------------------------##

#' The steps on offer, in the order they have to run.
#'
#' `needs` names another step that has to have produced its result first;
#' `cost` is what the interface shows.
builder_analysis_steps <- function() {
  list(
    list(
      id = "percent_mt_ribo",
      label = "Mitochondrial / ribosomal percentages",
      cost = "seconds",
      network = FALSE,
      note = "Adds continuous QC variables to meta.data.",
      needs = NULL
    ),
    list(
      id = "most_expressed",
      label = "Most expressed genes",
      cost = "seconds",
      network = FALSE,
      note = "Finds the genes contributing the most expression in each group.",
      needs = NULL
    ),
    list(
      id = "marker_genes",
      label = "Marker genes",
      cost = "minutes",
      network = FALSE,
      note = "Runs differential expression for every group.",
      needs = NULL
    ),
    list(
      id = "enriched_pathways",
      label = "Enriched pathways (Enrichr)",
      cost = "network",
      network = TRUE,
      note = "Sends marker genes to nine Enrichr databases for every group; requires marker genes and network access.",
      needs = "marker_genes"
    )
  )
}

#' Is a step currently selectable, and if not, why.
builder_step_blocked <- function(step, profile, selected) {
  if (identical(step$id, "percent_mt_ribo")) {
    ## The gene list ships per organism; "other" has none.
    if (!profile$organism_guess %in% c("hg", "mm")) {
      return("Human and mouse only")
    }
  }
  if (!is.null(step$needs)) {
    has_already <- any(vapply(
      profile$extras,
      function(x) identical(x$key, "marker_genes") && isTRUE(x$found),
      logical(1)
    ))
    if (!(step$needs %in% selected) && !has_already) {
      return("Select Marker genes first")
    }
  }
  NULL
}

#' Run the selected steps, in order, reporting progress through `on_progress`.
#'
#' @return A list with `object` and `log` (one line per step).
builder_run_analyses <- function(
  object,
  selected,
  settings,
  on_progress = function(...) NULL
) {
  log <- character()
  if (!length(selected)) {
    return(list(object = object, log = log))
  }

  for (step in builder_analysis_steps()) {
    if (!step$id %in% selected) {
      next
    }
    on_progress(step$label)
    started <- Sys.time()

    res <- try(
      switch(
        step$id,
        percent_mt_ribo = CerebroNexus::addPercentMtRibo(
          object,
          assay = settings$assay,
          organism = settings$organism,
          gene_nomenclature = "name"
        ),
        most_expressed = CerebroNexus::getMostExpressedGenes(
          object,
          assay = settings$assay,
          groups = settings$groups
        ),
        marker_genes = CerebroNexus::getMarkerGenes(
          object,
          assay = settings$assay,
          organism = settings$organism,
          groups = settings$groups,
          verbose = FALSE
        ),
        enriched_pathways = CerebroNexus::getEnrichedPathways(object),
        object
      ),
      silent = TRUE
    )

    took <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    if (inherits(res, "try-error")) {
      ## A failed optional step must not lose the export: the data set is still
      ## worth having without that page.
      log <- c(
        log,
        paste0(
          "✗ ",
          step$label,
          ": ",
          conditionMessage(attr(res, "condition"))
        )
      )
    } else {
      object <- res
      log <- c(log, paste0("✓ ", step$label, " (", took, " s)"))
    }
  }
  list(object = object, log = log)
}
