import json,pathlib,statistics
root=pathlib.Path(__file__).parent/'remaining'
arms={'B':['gene-L1-B-review','gene-pair2-B','gene-pair3-B'],
      'C':['gene-debounce-C','gene-pair2-C','gene-pair3-C'],
      'final':['gene-debounce-C','gene-pair2-C','gene-pair3-C','gene-final4-C','gene-final5-C']}
rows={}
for name in set(sum(arms.values(),[])):
    file=root/(name+('.first.json' if name=='gene-final5-C' else '.json'))
    data=json.loads(file.read_text())
    key='combined\x1dGad1\x1d\x1c\x1c'
    eligible=[o for o in data['ops'] if o['name']=='expression_genes_input' and any(e['detail'].get('renderKey')==key for e in o['events'])]
    assert len(eligible)==1,(name,len(eligible))
    op=eligible[0];events=[e for e in op['events'] if e['detail'].get('renderKey')==key]
    assert all(e['detail']['renderedPointCount']==1000000 for e in events)
    primary=min(e['ms'] for e in events)
    stages=[json.loads(s) for s in (root/(name+'.stages.jsonl')).read_text().splitlines()]
    start=next(s['mono'] for s in stages if s['name']=='gene_input' and s['detail']=='Gad1')
    selected=[s for s in stages if s['mono']>=start]
    def at(fn,phase):return next(s['mono'] for s in selected if s['name']==fn and s['phase']==phase)
    rpeak=None if name=='gene-final5-C' else int((root/(name+'.rss')).read_text().split()[1])/2**20
    rows[name]={'first_ms':primary,'complete_ms':op['groupPaint'],'ws_bytes':sum(x['bytes'] for x in op['messages']),
      'heap_peak_MiB':op['heapPeak']/2**20,'r_peak_MiB':rpeak,'max_frame_gap_ms':op.get('maxFrameGap'),
      'input_to_rows_ms':at('expressionProjectionCachedRows','start')-start,
      'rows_ms':at('expressionProjectionCachedRows','end')-at('expressionProjectionCachedRows','start'),
      'row_ready_to_render_ms':at('expression_projection_update_plot','start')-at('expressionProjectionCachedRows','end'),
      'render_send_ms':at('expression_projection_update_plot','end')-at('expression_projection_update_plot','start'),
      'input_to_send_ms':at('expression_projection_update_plot','end')-start,
      'clock':data['clock']}
def stats(names,key):
    values=[rows[n][key] for n in names if rows[n][key] is not None]
    return {'median':statistics.median(values),'min':min(values),'max':max(values),'n':len(values)}
metrics=['first_ms','complete_ms','ws_bytes','heap_peak_MiB','r_peak_MiB','row_ready_to_render_ms','input_to_send_ms']
summary={a:{k:stats(names,k) for k in metrics} for a,names in arms.items()}
(root/'gene-summary.json').write_text(json.dumps({'rows':rows,'summary':summary},indent=2)+'\n')
print(json.dumps(summary,indent=2))
