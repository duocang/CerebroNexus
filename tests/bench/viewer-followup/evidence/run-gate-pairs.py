from pathlib import Path
import subprocess,sys,json,hashlib
root=Path(r'C:/Users/wangx/Projects/CerebroNexus'); d=Path(r'C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-10-pr6-followup')
f=root/'inst/viewer/www/cell_views.js'
g=root/'inst/viewer/gene_expression/out_number_of_selected_cells.R'
try:
 for i in range(int(sys.argv[1]),int(sys.argv[2])+1):
  for arm,src in [('B','baseline'),('C','candidate')]:
   f.write_bytes((d/('browser-'+src+'.js')).read_bytes())
   g.write_bytes((d/('count-'+src+'.R')).read_bytes())
   (d/('gate'+str(i)+'-'+arm+'.source.json')).write_text(json.dumps({str(x.relative_to(root)):hashlib.sha256(x.read_bytes()).hexdigest() for x in [f,g]}),encoding='utf-8')
   cmd=['wsl','--exec','Rscript','/mnt/c/Users/wangx/Projects/CerebroNexus/tests/bench/viewer-followup/gene-first-v4.R','/mnt/c/Users/wangx/Projects/CerebroNexus','/mnt/c/Users/wangx/Projects/shinyapps_docker/apps/cerebronexus/private-data/cerebro_mouse_brain_1m_pages_clean_v1.crb','/mnt/c/Users/wangx/Projects/CerebroNexus-backups/2026-10-10-pr6-followup/gate'+str(i)+'-'+arm+'.tsv']
   print('START',i,arm,flush=True)
   result=subprocess.run(cmd)
   if result.returncode: raise RuntimeError(str(result.returncode))
finally:
 f.write_bytes((d/'browser-candidate.js').read_bytes())
 g.write_bytes((d/'count-candidate.R').read_bytes())
