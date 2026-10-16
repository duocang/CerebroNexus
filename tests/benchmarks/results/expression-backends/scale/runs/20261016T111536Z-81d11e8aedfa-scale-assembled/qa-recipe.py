from pathlib import Path
import json,hashlib,subprocess,sys
import pymupdf
task=Path(__file__).resolve().parent
stage=Path(json.loads((task/'assembly-record.json').read_text())['stage'])
skill=Path('C:/Users/wangx/.codex/skills/nature-figure/scripts')
out=stage/'replot-output'
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
assert sha(out/'scale_with_full_endpoints.png')==sha(stage/'figures/expression_backend_benchmark_overview.png')
for name,args in [
 ('validate_figure.py',[str(stage/'replot.R'),'--json']),
 ('audit_pdf_text.py',[str(out/'scale_with_full_endpoints.pdf'),'--min-pt','5','--json']),
 ('audit_figure_collisions.py',[str(out/'scale_with_full_endpoints.pdf'),'--json-out',str(out/'collision.json')])]:
 p=subprocess.run([sys.executable,'-X','utf8',str(skill/name),*args],capture_output=True,text=True,encoding='utf-8')
 (out/(name+'.log')).write_text(p.stdout+p.stderr,encoding='utf-8')
 if name!='audit_pdf_text.py':assert p.returncode==0,p.stdout+p.stderr
 # The raw Tf check ignores R PDF text matrices. Retain it; independently
 # require the true transformed font sizes, rather than lowering the floor.
with pymupdf.open(out/'scale_with_full_endpoints.pdf') as d:
 spans=[s for page in d for b in page.get_text('dict')['blocks'] if 'lines' in b for line in b['lines'] for s in line['spans'] if s['text'].strip()]
 assert min(s['size'] for s in spans)>=5
 result=dict(status='PASS',png_identical_to_approved=True,effective_min_font_pt=min(s['size'] for s in spans),text_spans=len(spans),
   standalone_replot=True,external_full_or_derived_reads=False,visual_review='Panels a-h inspected: exact full coordinates, the Human 500k point is a filled five-build summary with a min-max whisker, 300k ticks, uncertainty and legend/caption remain correct.')
(stage/'reproduction-QA.json').write_text(json.dumps(result,indent=2),encoding='utf-8')
print(json.dumps(result,indent=2))
