# Groups and Marker genes staged appearance

Baseline: `84ef2b15`; branch `卧槽/pr6_clean_perf`, existing checkout. No push.

## Cause and bounded fix

Removing entrance animation did not remove the dynamic UI dependency chain.
Marker genes previously mounted a parent output, then method and group outputs
(the latter waiting for the method input), then a result box, then result
controls, subgroup input, and the table. The browser showed an initially empty
page followed by these separate pieces. Groups already built its controls in one
response, but had no reserved control height; its viewport-demanded metrics output
also inserted the whole metrics box after the composition area.

- Both pages now have an initial heading and result frame with reserved layout
  space and plain preparation text. No shimmer, delayed reveal, or new ready gate.
- Groups retains the existing IntersectionObserver request and active-metric gate.
  Only the metrics box moves to static UI; its content is still demanded lazily.
- Marker initial method and grouping selectors are built together from method/group
  names. Subsequent method changes update group choices without rebuilding the
  surrounding controls; a still-valid choice is retained, otherwise the first
  valid choice is selected. Invalid methods are ignored and empty choices handled.
- Marker table frame is static. Missing-data/empty-result messages are returned
  directly instead of mounting another text-output round trip. The subgroup and
  DT result pipeline remains; neither data computation nor DT rendering is hidden
  behind an artificial all-results barrier.
- On these two pages, pending opacity is applied to results without multiplying
  through nested HTML UI wrappers. The shared loader names these pages explicitly.
- Groups reports insufficient grouping variables rather than leaving a perpetual
  control placeholder, and rejects stale groups before updating comparison choices.

No changes to marker data, filtering, number formatting, expression readers,
geometry, wire format, caches, metadata materialization, or painted/ready criteria.
This reduces visible structural staging; it does not claim that every result now
arrives simultaneously, that all layout shift is eliminated, or that backend
computation is faster. Reserved sizes are a desktop baseline with a narrow-screen
rule, not a full mobile-layout certification.

## Validation

WSL R targeted suites passed: `groups-performance`, `marker-page-controls`,
`viewer-review-feedback`. The new 21 marker assertions cover initial paired
selectors, valid-choice preservation, method change, empty choices, stale method
rejection, dataset catalog replacement, missing results, stored no-marker sentinel,
table controls, and stale-group rejection before reading result data. Final Groups
guard and expanded marker tests were rerun. JS syntax and `git diff --check` passed.

Real Windows Brave, WSL R production smoke server at localhost 8942, Demo dataset
(1,476 cells), 1369 x 915 viewport, existing reduced-motion preference:

- Initial Groups navigation exposed heading, preparation text and both result
  boxes; composition and demanded metrics subsequently rendered.
- Grouping changed sample -> cell_type; composition showed B cells, Monocytes and
  T cells. Number of expressed genes tab rendered. Returning from Marker preserved
  cell_type and the selected metric, with page loader hidden.
- Initial Marker entry exposed both populated method/group selectors and the
  table frame in the same observation, with a preparation label inside the frame.
- Marker subgroup 0 showed 147 stored records; selecting subgroup 1 produced 216
  records (first rows TCL1A / CD79A). Returning through Groups retained subgroup 1
  and its table. No screenshot is used as a duration measurement.
- Browser warning/error sample was empty; R output during these operations had no
  new application exception. Method-to-method and missing-data variants were
  verified in R tests; Demo has only one marker method/group, so those variants
  are not claimed as browser-tested.

Evidence: `implementation-evidence/groups-marker/` includes baseline Marker DOM,
entry/final DOM, screenshots, Groups revisit state and browser console. No full
benchmark, profiling, Mouse rerun, or performance A/B was performed.
