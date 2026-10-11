"""Summarize saved evidence; never controls the browser or application."""
import json
from pathlib import Path

root = Path(__file__).parent / 'remaining'
for stem in ('memory-final', 'memory-fixed'):
    path = root / (stem + '.json')
    if not path.exists():
        continue
    evidence = json.loads(path.read_text(encoding='utf-8'))
    rows = []
    for snapshot in evidence['memory']:
        browser = snapshot['browser']
        rows.append(dict(label=snapshot['label'], dataset=snapshot['context']['dataset_key'],
            generation=snapshot['context']['generation'], rssMiB=snapshot['rss']/2**20,
            liveMiB=sum(row[1] for row in snapshot['gc']),
            browserHeapMiB=browser['heapUsed']/2**20, caches=snapshot['caches'],
            staleProbes=len(browser['staleChecks'])+1,
            staleAccepted=sum(x['accepted'] for x in browser['staleChecks'])+int(browser['wrongEpochAccepted']),
            bpcellsFDs=snapshot['bpcells_open_fds']))
    summary = dict(rows=rows, browserHeapPeakMiB=max(o.get('heapPeak',0) for o in evidence['ops'])/2**20,
        rssPeakMiB=int((root/(stem+'.rss')).read_text().split()[1])/2**20,
        fallbackRequests=sum(x['name']=='coordviews_wire_fallback' for o in evidence['ops'] for x in o.get('inputs',[])),
        visibleErrors=evidence['errors'])
    (root/(stem+'.summary.json')).write_text(json.dumps(summary,indent=2),encoding='utf-8')
    print(stem, [(x['label'],x['dataset'],round(x['rssMiB'],1),x['liveMiB'],x['staleAccepted']) for x in rows])
