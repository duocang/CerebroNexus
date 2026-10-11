from pathlib import Path
import hashlib,json
p=Path(__file__).resolve().parent
root=p.parents[3]
s=(root/'tests/bench/viewer-followup/brave/stability/paint-server.R').read_text()
s=s.replace("Rprof(paste0(out,'.Rprof'),interval=.005)","Rprof(paste0(out,'.Rprof'),interval=.01,gc.profiling=TRUE)")
s=s.replace("handler <- app$httpHandler",'shiny::addResourcePath("gene-diag",file.path(root,"tests/bench/viewer-followup/gene-diagnosis"))\nhandler <- app$httpHandler')
s=s.replace("tests/bench/viewer-followup/brave/stability/paint.js","tests/bench/viewer-followup/gene-diagnosis/closeout.js")
s=s.replace("    response$content <- sub('</body>'",'''    response$content <- sub('<head>','<head><script src="gene-diag/head.js"></script>',response$content,fixed=TRUE)
    response$content <- gsub('src="[^"]*cell_views[.]js[^"]*"','src="gene-diag/cell_views.js"',response$content)
    response$content <- sub('</body>' ''')
(p/'closeout-server.R').write_text(s)
j=(root/'tests/bench/viewer-followup/brave/stability/paint.js').read_text()
j=j.replace("{ops,clock,memory,painted,ua:","{ops,clock,memory,painted,diag:window.geneDiagEvents,ua:")
j=j.replace("const panel=document.createElement('pre');", "const panel=document.createElement('pre');")
(p/'closeout.js').write_text(j)
(p/'closeout-identity.json').write_text(json.dumps({f.name:hashlib.sha256(f.read_bytes()).hexdigest() for f in [p/'head.js',p/'cell_views.js',p/'closeout.js',p/'closeout-server.R']},indent=2)+'\n')
