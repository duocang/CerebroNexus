import json, pathlib, statistics
p = pathlib.Path(__file__).resolve().parent
rows = []
for f in sorted(p.glob('[BC][1-5].json')):
    first = f.with_name(f.stem + '.first.json')
    d = json.loads((first if first.exists() else f).read_text())
    page = next(o for o in d['ops'] if o['name'] == 'page' and o['value'] == '#shiny-tab-trajectory')
    op = next(o for o in d['ops'] if o['name'] == 'trajectory_point_color')
    draw = next(e for e in op['events'] if e['detail'].get('timing'))
    paints = [e for e in d['painted'] if e['detail']['id'] == 'trajectory_projection' and e['at'] > page['start']]
    ready = next(e for e in paints if e.get('pageReady') == 'true' and not e['loaderVisible'] and not e['ariaBusy'] and 981807 in e['counts'])
    rss_file = f.with_name(f.stem + '.first.rss') if first.exists() else f.with_suffix('.rss')
    rss = rss_file.read_text().split()
    rows.append(dict(run=f.stem, primary_draw_ms=op['start'] + draw['ms'] - page['start'],
        first_usable_ms=ready['at'] - page['start'], event_to_check_ms=ready['at']-ready['eventAt'],
        points=max(ready['counts']), context=ready['context'],
        last_observed_message_ms=op['start'] + max(m['ms'] for m in op['messages']) - page['start'],
        ws_bytes=sum(m['bytes'] for o in d['ops'][d['ops'].index(page):] for m in o['messages']),
        r_peak_MiB=int(rss[-1])/2**20, js_heap_peak_MiB=op['heapPeak']/2**20,
        complete_browser_capture=f.stem not in ('B2','B3'),
        errors=d['errors']))
out = {'runs': rows, 'comparisons': {}}
for label, names in [('B3',['B1','B2','B3']),('C3',['C1','C2','C3']),('C5',['C1','C2','C3','C4','C5'])]:
    subset=[r for r in rows if r['run'] in names]
    if not subset: continue
    out['comparisons'][label]={'runs':[r['run'] for r in subset]}
    for key in ['primary_draw_ms','first_usable_ms','r_peak_MiB','js_heap_peak_MiB','ws_bytes','last_observed_message_ms']:
        v=[r[key] for r in subset if key not in ('ws_bytes','js_heap_peak_MiB','last_observed_message_ms') or r['complete_browser_capture']]
        out['comparisons'][label][key]={'median':statistics.median(v),'min':min(v),'max':max(v)}
(p/'summary.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
