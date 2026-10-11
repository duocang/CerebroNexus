"""Linked-only diagnostic copy; production files are never overwritten."""
from pathlib import Path
import hashlib, json
p = Path(__file__).resolve().parent
root = p.parents[3]
s = (root/'tests/bench/viewer-followup/brave/stability/paint-server.R').read_text(encoding='utf-8')
a=s.index("      for(name in c(")
b=s.index("        if(exists(name",a)
s=s[:a]+'''      for(name in c('cv_prepare_progressive_supplement',
        'cv_build_progressive_supplement','cv_build_compact_supplement',
        'cv_build_trajectories','cv_canonical_metadata','cv_cell_ids',
        'cv_build_deferred_metadata','cv_build_spatial','cv_build_trekker',
        'cv_build_clone','cv_build_clone_rows','cv_async_clone_spec',
        'cv_start_clone_task','cv_send_progressive_supplement','cv_build_bundle_safe')) {
'''+s[b:]
s=s.replace("tracer=bquote(stamp(.(name),'start')),",'''tracer=if(name=='cv_canonical_metadata') quote({
              stamp('cv_canonical_metadata','start'); force(metadata)
              stamp('metadata_argument','forced',list(rows=nrow(metadata),columns=ncol(metadata)))
            }) else bquote(stamp(.(name),'start')),''',1)
a=s.index('  targets <- c(');b=s.index('  for (name in targets)',a)
s=s[:a]+'''  targets <- c('cv_wire_pack_message','.hydrateThinCrbFields',
    'viewerProjectionFirstFrameMetadata','viewerPackTrajectoryFrame','viewerDatasetIdentity')
'''+s[b:]
a=s.index('  shiny::observeEvent(scope$input$expression_genes_input');b=s.index('\n}',a)
s=s[:a]+'''  shiny::observeEvent(scope$input$coordviews_primary_ready, {
    stamp('supplement_input','received',scope$input$coordviews_primary_ready)
  },priority=9999,ignoreInit=TRUE)
'''+s[b:]
a=s.index('  shiny::observeEvent(input$remaining_memory');b=s.index('  Rprof(',a)
s=s[:a]+s[b:]
s=s.replace("Rprof(paste0(out,'.Rprof'),interval=.005)","Rprof(paste0(out,'.Rprof'),interval=.005,gc.profiling=TRUE)")
s=s.replace("handler <- app$httpHandler",'shiny::addResourcePath("linked-diag",file.path(root,"tests/bench/viewer-followup/linked-tail"))\nhandler <- app$httpHandler')
s=s.replace('tests/bench/viewer-followup/brave/stability/paint.js','tests/bench/viewer-followup/linked-tail/probe.js')
s=s.replace("    response$content <- sub('</body>'",'''    response$content <- sub('<head>','<head><script src="linked-diag/head.js"></script>',response$content,fixed=TRUE)
    response$content <- gsub('src="[^"]*cell_views[.]js[^"]*"','src="linked-diag/cell_views.js"',response$content)
    response$content <- sub('</body>' ''')
(p/'server.R').write_text(s)
j=(root/'inst/viewer/www/cell_views.js').read_text(encoding='utf-8')
marker='  window.cerebroLinkedViewsState = Object.freeze({'
assert j.count(marker)==1
wrappers=''
for name in ['applyData','drawAll','hydrateLinkedGeometry','hydrateSparseLinked','applyHydratedSupplement','applyCloneSupplement','onBinarySupplement','reportWorkspaceReady','dispatchWorkspaceReady']:
    wrappers+=f'''  {name} = (function(original) {{ return function(arg) {{
    window.linkedDiag('{name}:start');
    try {{ return original.apply(this,arguments); }}
    finally {{ window.linkedDiag('{name}:end'); }}
  }}; }})({name});\n'''
j=j.replace(marker,wrappers+marker)
start=j.index('  function reportWorkspaceReady(');end=j.index('\n  window.cerebroLinkedViewsState',start)
part=j[start:end]
part=part.replace('    requestAnimationFrame(function () {',"    window.linkedDiag('paint:scheduled',{token:token});\n    requestAnimationFrame(function () {\n      window.linkedDiag('paint:raf1',{token:token});",1)
part=part.replace('      requestAnimationFrame(function () {',"      requestAnimationFrame(function () {\n        window.linkedDiag('paint:raf2',{token:token});",1)
j=j[:start]+part+j[end:]
(p/'cell_views.js').write_text(j,encoding='utf-8')
(p/'.gitignore').write_text('cell_views.js\n*.Rprof\n*.json\n!IDENTITY.json\ndiagnostic.rss\ndiagnostic.stages.jsonl\n')
(p/'IDENTITY.json').write_text(json.dumps({str(f.relative_to(root)):hashlib.sha256(f.read_bytes()).hexdigest() for f in [p/'head.js',p/'probe.js',p/'server.R',p/'cell_views.js',root/'inst/viewer/www/cell_views.js']},indent=2)+'\n')


