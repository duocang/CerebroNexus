"""Read-only verification of the delivered supplement, original inputs and PDF."""
from pathlib import Path
import json,hashlib,sys,subprocess,csv
import pymupdf
out=Path(__file__).resolve().parent
if not (out/'replot.R').exists():
    out=Path('C:/Users/wangx/.codex/worktrees/benchmark-clock-smoke/CerebroNexus/tests/benchmarks/results/expression-backends/derived/20261016-scale-with-full')
repo=out.parents[5]
def sha(p):
    with p.open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
done=json.loads((out/'completion.json').read_text())
assert done['status']=='COMPLETE' and done['supplement_update']=='20261016-300k-and-human500k-99G'
for r in done['artifacts']:
    p=out/r['path']; assert p.stat().st_size==r['bytes'] and sha(p)==r['sha256'],str(p)
for r in json.loads((out/'input-sha256.json').read_text()):
    p=repo/r['path']; assert p.stat().st_size==r['bytes'] and sha(p)==r['sha256'],str(p)
original=Path('C:/Users/wangx/Downloads/CerebroNexus-results-before-full-endpoints-20261016-074602/backup-manifest.json')
record=json.loads(original.read_text())
for r in record['inventory']:
    p=Path(record['source'])/r['path']; assert p.stat().st_size==r['bytes'] and sha(p)==r['sha256'],str(p)
data=json.loads((out/'data-validation.json').read_text())
assert data['status']=='PASS' and data['successful_builds']==216 and data['access_processes']==432 and data['raw_timing_records']==16416
with (out/'access_summary.csv').open(newline='') as f: rows=list(csv.DictReader(f))
assert len([r for r in rows if r['tier_type']=='scale_supplement' and int(r['n_builds'])==5])==6
assert len([r for r in rows if r['tier_type']=='feasibility' and int(r['n_builds'])==1])==1
pdf=out/'scale_with_full_endpoints.pdf'
with pymupdf.open(pdf) as doc:
    spans=[s for page in doc for b in page.get_text('dict')['blocks'] if 'lines' in b for line in b['lines'] for s in line['spans'] if s['text'].strip()]
    assert min(s['size'] for s in spans)>=5
    alltext=' '.join(s['text'] for s in spans)
    assert all(word in alltext for word in ('300k','500k','1,306,127','1,486,324','Hollow diamonds'))
scripts=Path('C:/Users/wangx/.codex/skills/nature-figure/scripts')
for name,args in [('validate_figure.py',[str(out/'replot.R'),'--json']),('audit_figure_collisions.py',[str(pdf)])]:
    proc=subprocess.run([sys.executable,'-X','utf8',str(scripts/name),*args],capture_output=True,text=True,encoding='utf-8')
    assert proc.returncode==0,proc.stdout+proc.stderr
print('PASS: delivered hashes, original 691 files, inputs, 300k replication, 500k n=1, effective PDF fonts, source and collision audits.')
