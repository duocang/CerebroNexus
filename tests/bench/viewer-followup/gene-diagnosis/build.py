from pathlib import Path
import hashlib,json,subprocess
p=Path(__file__).resolve().parent
root=p.parents[3]
source=(root/'inst/viewer/www/cell_views.js').read_text()
start=source.index('  function reportSinglePainted(')
end=source.index('  function activateSingle(',start)
s=source[start:end]
s=s.replace('    var paintedMeta', "    window.geneDiag('paint_start');\n    var paintedMeta",1)
s=s.replace('      if (singleActive', "      window.geneDiag('gpu_ready');\n      if (singleActive",1)
s=s.replace('      drawAll();', "      window.geneDiag('paint_draw_start');drawAll();window.geneDiag('paint_draw_end');",1)
s=s.replace('      if (settled === false)', "      window.geneDiag('gpu_idle_done');\n      if (settled === false)",1)
s=s.replace('      requestAnimationFrame(function () {', "      window.geneDiag('raf1_scheduled');\n      requestAnimationFrame(function () {window.geneDiag('raf1_fired');",1)
s=s.replace('        requestAnimationFrame(function () {', "        window.geneDiag('raf2_scheduled');\n        requestAnimationFrame(function () {window.geneDiag('raf2_fired');",1)
s=s.replace("          reportSelection('primary');", "          window.geneDiag('primary_report');reportSelection('primary');",1)
source=source[:start]+s+source[end:]
source=source.replace('  function recolorSingle(message) {',"  function recolorSingle(message) {window.geneDiag('recolor_start');",1)
source=source.replace('    hydrateSparseSingleColor(view.data);',"    window.geneDiag('hydrate_start');hydrateSparseSingleColor(view.data);window.geneDiag('hydrate_end');",1)
source=source.replace('  function updateSingleExpression(id, previousMeta, previousData, message) {',"  function updateSingleExpression(id, previousMeta, previousData, message) {window.geneDiag('apply_start');",1)
source=source.replace('    _layoutKey = null; resizeAll(); drawAll();',"    window.geneDiag('first_draw_start');_layoutKey = null; resizeAll(); drawAll();window.geneDiag('first_draw_end');",1)
source=source.replace('    reportSinglePainted(id, view, D);',"    window.geneDiag('apply_end',{points:D.n,geometryReused:true});reportSinglePainted(id, view, D);",1)
source=source.replace('      recolorSingle(window.CBViewWire.unpack(buffer));',"      window.geneDiag('decode_start');var decoded=window.CBViewWire.unpack(buffer);window.geneDiag('decode_end');recolorSingle(decoded);window.geneDiag('handler_end');",1)
(p/'cell_views.js').write_text(source)
r=(root/'tests/bench/viewer-followup/brave/remaining/gene-frozen-server.R').read_text()
r=r.replace('root <- normalizePath(args[[1]])','root <- normalizePath(args[[1]])\noptions(shiny.reactlog=TRUE)\nprobe_dir <- file.path(root,"tests/bench/viewer-followup/gene-diagnosis")')
r=r.replace("'plotExpressionSummary','compactViolinSummary'", "'plotExpressionSummary','compactViolinSummary','getMetaData','viewerProjectionFirstFrameMetadata','viewerPackValidateDescriptor'")
r=r.replace("tracer=bquote(stamp(.(name),'start')),", "tracer=if(name=='viewerExpressionRow') quote(stamp('viewerExpressionRow','start',list(gene=gene,n=length(cells),first=head(cells,1)))) else bquote(stamp(.(name),'start')),")
r=r.replace('  original_server(input,output,session)', '''  trace('manageInputs',where=session,print=FALSE,
    tracer=quote(stamp('manageInputs','start',list(names=names(data),gene=data[['expression_genes_input']],ack=data[['expression_projection_rendered_key']]))),
    exit=quote(stamp('manageInputs','end')))
  trace('sendBinaryMessage',where=session,print=FALSE,
    tracer=quote(stamp('binary_send','start',list(type=type,bytes=length(message)))),
    exit=quote(stamp('binary_send','end')))
  session$onFlush(function()stamp('flush','start'),once=FALSE)
  session$onFlushed(function()stamp('flush','end'),once=FALSE)
  original_server(input,output,session)''')
r=r.replace('    Rprof(NULL)', '    Rprof(NULL)\n    jsonlite::write_json(shiny::reactlog(),paste0(out,".reactlog.json"),auto_unbox=TRUE,pretty=FALSE)')
r=r.replace("Rprof(paste0(out, \".Rprof\"),interval=.005)", "Rprof(paste0(out, \".Rprof\"),interval=.01,gc.profiling=TRUE)")
r=r.replace('handler <- app$httpHandler','shiny::addResourcePath("gene-diag",probe_dir)\nhandler <- app$httpHandler')
r=r.replace("script <- paste(readLines(file.path(root,'tests/bench/viewer-followup/brave/remaining.js')),collapse='\\n')", "script <- paste(readLines(file.path(probe_dir,'footer.js')),collapse='\\n')\n    response$content <- sub('<head>','<head><script src=\"gene-diag/head.js\"></script>',response$content,fixed=TRUE)\n    response$content <- gsub('src=\"[^\"]*cell_views[.]js[^\"]*\"','src=\"gene-diag/cell_views.js\"',response$content)")
(p/'server.R').write_text(r)
identity={'head':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),'files':{f.name:hashlib.sha256(f.read_bytes()).hexdigest() for f in p.iterdir() if f.suffix in ('.R','.js','.py')}}
(p/'IDENTITY.json').write_text(json.dumps(identity,indent=2)+'\n')
