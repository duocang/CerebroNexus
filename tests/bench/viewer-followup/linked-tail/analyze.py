"""Summarize nested trace durations without summing them as independent costs."""
from pathlib import Path
from collections import defaultdict
import gzip
import json

p = Path(__file__).resolve().parent


def read_json(name):
    with gzip.open(p / (name + '.gz'), 'rt', encoding='utf-8') as f:
        return json.load(f)


d = read_json('first.json')
operation = d['ops'][0]
stacks = defaultdict(list)
durations = []
for line in (p / 'first.stages.jsonl').read_text(encoding='utf-8').splitlines():
    event = json.loads(line)
    if event['phase'] == 'start':
        stacks[event['name']].append(event)
    elif event['phase'] == 'end' and stacks[event['name']]:
        start = stacks[event['name']].pop()
        durations.append({
            'name': event['name'],
            'wall_ms': event['mono'] - start['mono'],
            'cpu_ms': event['cpu'] - start['cpu'],
        })

print(json.dumps({
    'first_ms': operation['events'][0]['ms'],
    'complete_ms': operation['events'][1]['ms'],
    'durations_inclusive': durations,
    'browser_events': [
        dict(relative_ms=round(e['at'] - operation['start'], 1), **e)
        for e in d['diag']
        if 0 <= e['at'] - operation['start'] <= operation['events'][1]['ms'] + 1
        and e['name'] not in ('socket_send', 'socket_receive')
    ],
}, indent=2))
