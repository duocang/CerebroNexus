from pathlib import Path
import subprocess, sys, json
import pymupdf
repo=Path('C:/Users/wangx/.codex/worktrees/benchmark-clock-smoke/CerebroNexus')
out=repo/'tests/benchmarks/results/expression-backends/derived/20261016-scale-with-full'
scripts=Path('C:/Users/wangx/.codex/skills/nature-figure/scripts')
pdf=out/'scale_with_full_endpoints.pdf'
checks=[('source-audit', ['validate_figure.py',str(out/'replot.R'),'--json']),
        ('pdf-text-audit',['audit_pdf_text.py',str(pdf),'--min-pt','5','--json']),
        ('collision-audit',['audit_figure_collisions.py',str(pdf),'--json-out',str(out/'figure.collision-audit.json'),'--overlay-pdf',str(out/'figure.collision-audit.pdf')])]
for name,args in checks:
    p=subprocess.run([sys.executable,'-X','utf8',str(scripts/args[0]),*args[1:]],capture_output=True,text=True,encoding='utf-8')
    (out/(name+'.log')).write_text(p.stdout+p.stderr,encoding='utf-8')
    if name!='collision-audit':
        obj=json.loads(p.stdout); (out/(name+'.json')).write_text(json.dumps(obj,indent=2),encoding='utf-8')
    print(name,'exit',p.returncode)
    if name=='pdf-text-audit':
        # R PDF devices use Tf=1 and scale through a text matrix. Measure effective
        # font sizes in PDF user-space instead of treating Tf alone as points.
        with pymupdf.open(pdf) as document:
            spans=[s for page in document for block in page.get_text('dict')['blocks'] if 'lines' in block
                   for line in block['lines'] for s in line['spans'] if s['text'].strip()]
            minimum=min(s['size'] for s in spans)
            assert minimum>=5 and any('Full dataset' in s['text'] for s in spans)
            effective={'status':'PASS','minimum_effective_font_pt':minimum,'text_spans':len(spans),
                       'raw_Tf_auditor_exit':p.returncode,'note':'R PDF Tf operands are scaled by text matrices; effective PyMuPDF span sizes include those transforms.'}
            (out/'pdf-text-effective.json').write_text(json.dumps(effective,indent=2),encoding='utf-8')
            print('Effective rendered PDF text:', effective)
    else:
        assert p.returncode==0, name
print('render QA PASS')
