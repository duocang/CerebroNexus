# Five-page first-entry correction

Date: 2026-10-10. Baseline: `3ac132de` (diagnosis), application baseline
`ba7eca01`. Existing `卧槽/pr6_clean_perf` branch/worktree; no push.

## Changes

The shared loading card for Groups, Marker genes, Most expressed genes,
Enriched pathways and Extra material now follows the current page's actual
results instead of application-wide Shiny idle and repeatedly debounced DOM
mutations. Other page completion paths are unchanged.

- A Shiny value is necessary but insufficient: DT must emit its native draw;
  Plotly must emit afterplot; a plain Extra-material image must finish loading.
  Visible nested outputs must settle. Explicit empty/error results can finish;
  a silent `req()` condition cannot masquerade as completed content.
- The same two RAF boundaries remain after actual result completion. Cosmetic
  DOM mutations and unrelated Shiny work do not restart those frames. Incoming
  relevant values, invalidation, navigation and dataset reset cancel old work.
- Groups follows its composition plot and an explicitly shown composition table.
  Viewport-demanded expression metrics retain their independent lifecycle.
- The existing dataset context gates completion; weak output/draw sets reset on
  dataset replacement. Focused control edits during first entry reject the old
  result immediately, while unchanged/initial input binding messages do not.
- Marker and Enriched now include the subgroup selector in the result-control
  response. Remove their redundant subgroup `renderUI` outputs. The same factor
  levels, first default and filtering function remain. Toggling “show all” hides
  the mounted selector locally and retains its choice when toggled back.
- Real dataset-switch testing exposed Groups' existing afterplot callback being
  bound only once despite Plotly.purge replacing the emitter on the same DOM
  element. Its diagnostic callback now binds once per emitter. The page loader
  uses Plotly's native bubbling jQuery event, which survives emitter replacement.

The existing installed Plotly JS confirms that emitter initialization replaces
`plot.on` with a bound function, purge deletes it, and emit dispatches a jQuery
event as well as emitter callbacks. No Plotly or Shiny monkey patch was added.

No expression/metadata reader, precision, wire format, geometry, Viewer Pack,
scientific calculation or large cache changed. Most/Extra retain their existing
data-dependent selector chains; this change does not claim every initial UI
round trip has disappeared.

## Automated validation

- New isolated JS fixture executes the actual page-loader block: 33 assertions
  cover all four stored-result pages, Groups afterplot, Extra interactive/plain
  plots, real draw versus value arrival, both frames, unrelated busy work,
  cosmetic mutation, lazy metrics, control edits, late/unchanged binding,
  terminal results, revisit, navigation and dataset replacement. It also executes
  the actual Groups binding function against emitter replacement.
- The baseline fails the fixture: values without a DT draw can prematurely
  satisfy the old generic completion path. Candidate passes.
- Six focused R suites passed: groups-performance, marker-page-controls,
  result-page-controls, result-page-loader, viewer-accessibility-design and
  viewer-review-feedback. The new R test covers inline subgroup creation, factor
  levels, grouped/ungrouped content and unchanged filter results.
- Final Node fixture, JS syntax and `git diff --check` passed.

## Browser validation and evidence

Real Windows Brave, existing WSL R 4.6.1 production harness at port 8944;
1369 x 915 viewport, existing reduced-motion preference. Demo and the existing
Mouse 1M fixture. No injected browser state, throttling or fake ready signal.

Evidence is in `implementation-evidence/result-page-fix/`. Initial candidate
checks caught and corrected two Groups ordering issues: late input binding
mistaken for a user edit, then lost afterplot listeners on dataset replacement.
The final checks and their limitations are recorded with the evidence.

Demo checks include all five pages, populated results and loader dismissal;
Marker subgroup 0 (147 records) -> 1 (216 records), all subgroups (3,213), then
back to subgroup 1; Most grouping change; Enriched stored GSVA empty result;
Extra table (10 records); page revisit. Dataset replacement is checked separately
with Mouse Groups and return to Demo.

Final source validation passed after the emitter correction:

| Check | Observed result |
| --- | --- |
| Groups, Demo | Composition drawn; loading card dismissed |
| Groups, Mouse 1M | New composition drawn; new fingerprint; card dismissed |
| Groups, return to Demo | Demo composition and fingerprint restored; card dismissed |
| Marker | 147 records for subgroup 0; 216 for subgroup 1; revisit completed |
| Most expressed genes | 100 records; card dismissed |
| Enriched pathways | 38 records; card dismissed |
| Extra material | 10 records; card dismissed |

`final-checks.json` records the DOM observations; `final-*.txt` preserve page
snapshots and `final-marker.png` shows the final subgroup result. The final
browser warning/error log is empty (`final-console.json`), and the owned WSL R
session emitted no new errors during these checks (`final-r-session.txt` is its
last dataset-return excerpt). Extra interactive/PNG completion is covered by
the isolated fixture, not by this browser dataset, which provides only a table.
The final focused R loader suite passed again (18 expectations, including the
33-assertion Node fixture). No full benchmark or unrelated-page rerun was made.

`groups-candidate-stages.json` is a single intermediate UI observation (plot
observed at 2.854 s; card hidden at 4.283 s). It is not a median or an A/B result,
and is not the final emitter-replacement version. Do not use it to advertise a
speed ratio. First-entry backend work and slow browser RAF intervals can still
be visible; no fixed latency target or eliminated-tail claim is made.
