"""Losslessly archive this task's finished profiles and freeze evidence hashes."""
from pathlib import Path
import gzip, hashlib, json, subprocess
p=Path(__file__).resolve().parent
root=p.parents[4]
for f in p.rglob('*.Rprof'):
    assert p in f.resolve().parents
    raw=f.read_bytes()
    packed=gzip.compress(raw,mtime=0)
    target=f.with_suffix(f.suffix+'.gz')
    if target.exists():
        assert gzip.decompress(target.read_bytes())==raw
    else:
        target.write_bytes(packed)
    assert gzip.decompress(target.read_bytes())==raw
    f.unlink()
files=[f for f in p.rglob('*') if f.is_file() and f.name!='FINAL_SHA256.json']
sources=['inst/viewer/coordinated_views/server.R','inst/viewer/trajectory/server.R',
 'inst/viewer/trajectory/projection_plot.R','inst/viewer/trajectory/selected_cells_table.R',
 'inst/viewer/www/cell_views.js']
files += [root/s for s in sources]
manifest={'source_head':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),
 'baseline':subprocess.check_output(['git','rev-parse','602f9303'],cwd=root,text=True).strip(),
 'dataset_sha256':'cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38',
 'sha256':{str(f.relative_to(root)):hashlib.sha256(f.read_bytes()).hexdigest() for f in sorted(files)}}
(p/'FINAL_SHA256.json').write_text(json.dumps(manifest,indent=2)+'\n')
print(len(files),'files hashed; profiles verified losslessly')
