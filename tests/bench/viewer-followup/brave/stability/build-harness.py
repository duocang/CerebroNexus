from pathlib import Path
p=Path(__file__).resolve().parent
s=(p.parent/'memory-server.R').read_text()
s=s.replace("tests/bench/viewer-followup/brave/memory.js","tests/bench/viewer-followup/brave/stability/probes.js")
# Trace only in the test process; preserve production dispatch and output state.
s=s.replace('  .bench_scope <<- scope','''  .bench_scope <<- scope
  private <- scope$session$.__enclos_env__$private
  trace('sendMessage', where=private, print=FALSE, tracer=quote({
    packet <- list(...)
    name <- 'coordviews_selected_cells_UI'
    if ((!is.null(packet$recalculating) && identical(packet$recalculating$name,name)) ||
        (!is.null(packet$progress) && identical(packet$progress$message$id,name)) ||
        name %in% names(packet$values) || name %in% names(packet$errors))
      stamp('selected_output_message','send',list(recalculating=packet$recalculating,
        progress=packet$progress,values=names(packet$values),errors=names(packet$errors)))
  }))
  shiny::observe({
    stamp('lifecycle_inputs','observed',list(sidebar=scope$input$sidebar,
      dataset=scope$input$crb_file_selector,
      hidden=scope$session$clientData$output_coordviews_selected_cells_UI_hidden,
      indices_context=scope$input$coordviews_selection_indices$dataset_context,
      count=length(scope$input$coordviews_selection_indices$indices)))
  },priority=9999)
''')
(p/'server.R').write_text(s)
j=(p.parent/'memory.js').read_text()
j=j.replace('const ops=[],clock=[],memory=[]','const ops=[],clock=[],memory=[],lifecycle=[]')
j=j.replace('{ops,clock,memory,','{ops,clock,memory,lifecycle,')
j=j.replace("$(document).on('shiny:inputchanged',e=>{", """$(document).on('shiny:bound shiny:unbound shiny:outputinvalidated shiny:recalculating shiny:recalculated shiny:value shiny:error',e=>{
      if(e.target.id==='coordviews_selected_cells_UI')lifecycle.push({time:performance.now(),kind:e.type,id:e.target.id,visible:e.target.offsetParent!==null,context:CerebroDatasetContext.current(),nodes:document.querySelectorAll('#coordviews_selected_cells_UI').length});
    });
    Shiny.shinyapp.$socket.addEventListener('message',e=>{
      if(typeof e.data!=='string')return;
      const p=JSON.parse(e.data),id='coordviews_selected_cells_UI';
      if(p.recalculating?.name===id || p.progress?.message?.id===id || Object.hasOwn(p.values||{},id)||Object.hasOwn(p.errors||{},id))lifecycle.push({time:performance.now(),kind:'wire',packet:p});
    });
    $(document).on('shiny:inputchanged',e=>{""")
(p/'probes.js').write_text(j)
