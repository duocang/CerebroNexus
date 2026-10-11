import json,pathlib
root=pathlib.Path(__file__).resolve().parent
old=root.parent/'remaining'
names=['gene-debounce-C','gene-pair2-C','gene-pair3-C','gene-final4-C','gene-final5-C']
rows=[]
for name in names:
 d=json.loads((old/(name+('.first.json' if name.endswith('5-C') else '.json'))).read_text())
 key='combined\x1dGad1\x1d\x1c\x1c'
 op=next(o for o in d['ops'] if o['name']=='expression_genes_input' and any(e['detail'].get('renderKey')==key for e in o['events']))
 ev=next(e for e in op['events'] if e['detail'].get('renderKey')==key)
 ss=[json.loads(s) for s in (old/(name+'.stages.jsonl')).read_text().splitlines()]
 received=next(s['mono'] for s in ss if s['name']=='gene_input' and s['detail']=='Gad1')
 valid=[c for c in d['clock'] if 'end' in c]
 offset=None if not valid else valid[0]['mono']-(valid[0]['start']+valid[0]['end'])/2
 stages=[dict(s,relative_ms=s['mono']-received) for s in ss if received<=s['mono']<=received+9000]
 rows.append(dict(run=name,start=op['start'],input_to_server_ms=None if offset is None else received-offset-op['start'],clock_uncertainty_ms=None if not valid else (valid[0]['end']-valid[0]['start'])/2,first_ms=ev['ms'],complete_ms=op['groupPaint'],event=ev['detail'],stages=stages,messages=op['messages'],longTasks=op.get('longTasks'),outputs=op['outputs'],inputs=op.get('inputs'),pre_input=[s for s in ss if received-3000<s['mono']<received]))
(root/'gene-five-timelines.json').write_text(json.dumps(rows,indent=2)+'\n')
for r in rows:
 print(r['run'],'input_to_server',r['input_to_server_ms'],'first',r['first_ms'],'complete',r['complete_ms'])
 print('EVENT',json.dumps(r['event']))
 print('messages',r['messages'][:12],'longtasks',r['longTasks'])
