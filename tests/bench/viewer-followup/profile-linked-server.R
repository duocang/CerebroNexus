# Test-process instrumentation only. Never loaded by the production Viewer.
bench_install <- function(scope) {
  message("LINKED_PROBE_INSTALLED"); session <- scope$session
  input <- scope$input
  active <- FALSE
  stamp <- function(name, phase, detail = NULL) {
    if (!active) return(invisible(NULL))
    cat(jsonlite::toJSON(list(name=name, phase=phase,
      at=as.numeric(Sys.time())*1000, elapsed=nanonext::mclock(), cpu=sum(proc.time()[1:2])*1000, detail=detail), auto_unbox=TRUE), "\n",
      file=Sys.getenv("VIEWER_TRACE_FILE"), append=TRUE)
  }
  wrap <- function(name, env) {
    if (!exists(name, envir=env, inherits=FALSE)) return()
    old <- get(name, envir=env)
    if (!is.function(old) || isTRUE(attr(old,"bench_wrapped"))) return()
    wrapped <- local({ f <- old; label <- name
      function(...) {
        stamp(label,"start")
        on.exit(stamp(label,"end"), add=TRUE)
        if (identical(label,"expression_projection_update_plot") && active) {
          x <- list(...)[[1L]]
          previous <- scope$expression_projection_render_state$geometry
          current <- list(dataset=x$dataset,coordinates=x$coordinates,
            plot_parameters=x$plot_parameters,cells=x$cell_indices,
            trajectory=x$trajectory,separate=isTRUE(x$separate_panels),
            request=x$render_request)
          stamp("geometry_comparison","value", list(reset_axes=x$reset_axes,
            different=names(current)[!vapply(names(current),function(n)
              identical(current[[n]],previous[[n]]),logical(1))]))
        }
        value <- f(...)
        if(identical(label,"cv_wire_pack_message")) stamp("wire_bytes","value",length(value))
        value
      }
    })
    attr(wrapped,"bench_wrapped") <- TRUE
    assign(name,wrapped,envir=env)
  }
  targets <- c("cv_prepare_progressive_supplement","cv_send_progressive_supplement","cv_build_compact_supplement","cv_build_progressive_supplement","cv_canonical_metadata","cv_build_deferred_metadata","cv_build_trajectories","cv_build_spatial","cv_build_trekker","cv_build_clone","cv_build_projections","cv_defer_assets","coordviews_color_patch","cerebroSelectionSummary","expressionProjectionFullCellSelection","viewerExpressionCells","viewerBpcellsValues","viewerExpressionRow",
    "viewerExpressionValues","expressionProjectionCachedRows","expressionProjectionCachedRow",
    "expression_projection_expression_levels","expression_projection_coordinates",
    "expression_projection_cells_to_show","expression_projection_parameters_color",
    "expression_projection_parameters_plot","expression_projection_data",
    "expression_projection_update_plot","expressionSparseColor","expressionPackedColor",
    "cerebroCellViewMessage","cerebroCellViewRender","cerebroCellViewRecolor",
    "cerebroCellViewAuxRequest","cerebroCellViewDeferredAux","cv_wire_pack_message",
    "viewerDatasetIdentity","viewerProjectionAsset","cerebroProjectionHoverColumns",
    "plotExpressionSummary","compactViolinSummary","expression_summary_data")
  shared <- environment(scope$viewerExpressionValues)
  contexts <- list(scope,shared)
  for (name in c("cv_build_progressive_supplement","cv_build_compact_supplement")) {
    if(exists(name,envir=scope,inherits=TRUE)) contexts <- c(contexts,list(environment(get(name,envir=scope,inherits=TRUE))))
  }
  for (env in unique(contexts)) {
    if(is.environment(env) && !environmentIsLocked(env)) for (name in targets) wrap(name,env)
  }
  old_loader <- scope$load_deferred_viewer_server
  scope$load_deferred_viewer_server <- function(server_file) {
    result <- old_loader(server_file)
    if(identical(server_file,"coordinated_views/server.R")) {
      envs <- list(scope)
      for(n in c("cv_build_progressive_supplement","cv_build_compact_supplement"))
        if(exists(n,envir=scope,inherits=TRUE)) envs <- c(envs,list(environment(get(n,envir=scope,inherits=TRUE))))
      for(e in unique(envs)) if(is.environment(e)&&!environmentIsLocked(e)) for(n in targets) wrap(n,e)
    }
    result
  }
  shiny::observeEvent(input$bench_profile, {
    message("LINKED_PROBE_INPUT ", input$bench_profile); active <<- isTRUE(input$bench_profile)
    if(active && identical(Sys.getenv("BENCH_SAMPLING"),"true")) Rprof(paste0(Sys.getenv("VIEWER_TRACE_FILE"),".Rprof"),interval=.005)
    else Rprof(NULL)
    session$sendCustomMessage("bench_clock",list(active=input$bench_profile,elapsed=nanonext::mclock()))
    session$sendCustomMessage("bench_profile_ack",input$bench_profile)
  },ignoreInit=TRUE,priority=10000)
  shiny::observeEvent(input$expression_genes_input, {
    stamp("gene_input","received",input$expression_genes_input)
    session$onFlushed(function() stamp("gene_input","flushed"),once=TRUE)
  },ignoreInit=TRUE,priority=9999)
  shiny::observeEvent(input$expression_projection_rendered_key, {
    stamp("primary_ack","received",input$expression_projection_rendered_key)
    session$onFlushed(function() {
      stamp("primary_ack","flushed")
      session$sendCustomMessage("bench_complete",list(key=shiny::isolate(input$expression_projection_rendered_key),at=as.numeric(Sys.time())*1000))
    },once=TRUE)
  },ignoreInit=TRUE,priority=9999)
  invisible(NULL)
}
