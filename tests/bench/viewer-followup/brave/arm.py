"""Switch only the measured source file in this same checkout; never a worktree."""
import pathlib, subprocess, hashlib, json, sys, tempfile
root=pathlib.Path(__file__).resolve().parents[4]
file=root/'inst/viewer/coordinated_views/bundle.R'
saved=pathlib.Path(tempfile.gettempdir())/'cerebro-linked-candidate.R'
arm=sys.argv[1]
if arm=='save': saved.write_bytes(file.read_bytes());sys.exit()
if arm=='B': file.write_bytes(subprocess.check_output(['git','show','c108c7e0:inst/viewer/coordinated_views/bundle.R'],cwd=root))
elif arm=='C': file.write_bytes(saved.read_bytes())
else: raise ValueError(arm)
label=sys.argv[2]
identity={'arm':arm,'baseline':'c108c7e0','source_sha256':hashlib.sha256(file.read_bytes()).hexdigest(),'harness':{}}
for name in ['server.R','measure.js','arm.py']:
 p=pathlib.Path(__file__).parent/name;identity['harness'][name]=hashlib.sha256(p.read_bytes()).hexdigest()
(pathlib.Path(__file__).parent/'evidence'/f'{label}.identity.json').write_text(json.dumps(identity,indent=2),encoding='utf-8')
