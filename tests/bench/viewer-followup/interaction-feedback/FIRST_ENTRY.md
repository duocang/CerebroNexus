# First-entry loading diagnosis: Groups and Marker genes

Date: 2026-10-10. Source: `ba7eca01`; existing branch/worktree.
User clarified that the concern is first entry. Dataset was not specified, so
the bounded reproduction used Demo (1,476 cells) in real Windows Brave, served
by the existing WSL R harness on port 8944. No backend, renderer, ready gate,
cache or loading code was changed for this diagnosis.

## Observations

Elapsed times below are from the start of the browser navigation action to
successful DOM locator observations. They include browser automation latency
and locator waits; they are not exact render timestamps, CPU profiles, painted
acknowledgements, A/B benchmarks or medians. Each page has one complete sample.
The app server/browser assets may be warm; “first” means first page entry in
that session, not a cold machine or cold R process.

| Page | Controls observed | Primary result observed | Page loader hidden observed |
|---|---:|---:|---:|
| Groups | 2,438 ms | 3,237 ms (Plotly SVG) | 5,330 ms |
| Marker genes | 2,303 ms | 2,992 ms (DataTables wrapper, 20 rows) | 5,291 ms |

At the Marker table observation the page loader remained visible, the document
was not `shiny-busy`, and there were no visible recalculating outputs in that
page. Thus its observed 2.3-second tail cannot simply be described as the table
still being computed by R. Groups similarly had an observed SVG before its
page card disappeared. DOM visibility does not by itself prove a completed
screen paint.

## Code mechanisms

- Marker reads stored results through `getMarkerGenes()`; opening this page does
  not run Seurat marker discovery. Initial controls must reach the browser before
  selected method/group inputs return; the result controls then mount the subgroup
  selector before the filtered DT result can finish. Structural shell work from
  earlier commits shortened this sequence but did not remove every round trip.
- Groups prepares its two-column composition and Plotly output. Expression metrics
  are separately requested on viewport demand. Their existence does not mean all
  metrics are eagerly computed.
- The shared non-canvas page-card completion path waits for Shiny idle, a 180 ms
  quiet period and two RAF boundaries. Its broad MutationObserver also resets
  completion on descendant changes, including attributes. This is an additional
  presentation completion path beyond receipt of the primary result.
- The two preceding commits changed loading appearance, not these data/UI
  dependencies or completion scheduling. A larger card makes an existing wait
  more prominent; it does not reduce first-entry work.

The present evidence separates primary-result availability from the loading-card
tail, but does not isolate the tail into DOM debounce vs RAF scheduling or assign
the initial delay to a specific R function. Prior Brave RAF variability is a
plausible contributor, not a newly demonstrated cause for this sample. Do not
remove paint gates or claim a faster result from simply hiding the card earlier.

Evidence: `implementation-evidence/first-entry/groups-baseline.json` and
`marker-stages.json`. `marker-baseline.json` is an earlier short observation
window ending before controls appeared; it is not a complete timing sample and
must not be averaged with the complete Marker sample. No full benchmark or
performance optimization was performed. Code tests were not rerun because no
application code changed.
