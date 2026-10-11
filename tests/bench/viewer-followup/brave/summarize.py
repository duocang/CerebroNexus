import pathlib,json,statistics
root=pathlib.Path(__file__).parent/'evidence'
rows=[]
for file in sorted(root.glob('formal-?*.json')):
 if file.name.endswith(('.browser.json','.identity.json')):continue
 d=json.loads(file.read_text(encoding='utf-8-sig'));runs=d['runs'];r=runs[0]
 ready=[e['at'] for e in r['events'] if e['detail'].get('painted') and e['detail'].get('primaryReady')]
 complete=[e['at'] for e in r['events'] if e['detail'].get('painted') and e['detail'].get('complete')]
 rss=root/(file.stem+'.rss'); browser=root/(file.stem+'.browser.json')
 b=json.loads(browser.read_text(encoding='utf-8-sig')) if browser.exists() else []
 row=dict(run=file.stem,first=ready[0] if ready else None,complete=complete[0] if complete else None,prepare=r.get('linked',{}).get('transport',{}).get('supplement',{}).get('serverPrepareMs'),payload=r['bytes'],heapMiB=r.get('heapPeak',0)/2**20,RpeakMiB=float(rss.read_text().split()[1])/2**20 if rss.exists() else None,BraveTotalPeakMiB=max((s['rss'] for s in b),default=0)/2**20)
 rows.append(row)
print(json.dumps(rows,indent=2))
(root/'summary.json').write_text(json.dumps(rows,indent=2),encoding='utf-8')
