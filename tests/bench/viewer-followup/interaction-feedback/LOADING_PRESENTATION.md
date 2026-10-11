# Loading presentation and announcements

Date: 2026-10-10. Baseline: `d8105131`.
Existing `卧槽/pr6_clean_perf` branch and worktree; no push.

## Change

The shared page indicator previously floated over the main content at a viewport
offset with the same strong shadow as dataset loading. It now mounts inside a
static result container. It retains its readable 400 x 104 px size, bounded width,
28 px ring and 16 px text, with a lighter shadow and local stacking order.
Absolute positioning adds no layout height when it appears or disappears. The
existing result/control reservations remain in place; this does not eliminate
layout changes caused by the actual controls or results arriving.

Stored-result pages and Groups have static wrappers outside renderUI-owned
outputs. Canvas pages use their static host. Linked has a fixed wrapper outside
the movable shared panel grid: the loader must never travel with that grid into
a single-view page. Its existing panel-hover selector follows the new wrapper.
Other page layouts fall back to their visualization column or box body.
The card remains click-through, without a backdrop or focus trap. Dynamic
filters inside a result output retain their existing rendering lifecycle.

Dataset initialization/replacement retains the central card and real progress.
Only its stage/name region is live; the visual elapsed timer and percentage are
outside it. Identical stage messages do not rewrite the live text. Page stage
announcements live outside aria-busy panes, independently of the visual card.
The existing local-update feedback, brief opacity transition and reduced-motion
support are retained. No new animation library or entrance choreography.

No changes to request keys, generation guards, completion/paint gates, renderer,
expression reader, geometry, precision, payloads, or metadata materialization.
No performance experiment or latency claim.

## Validation

- Node fixture: 41 assertions passed. Includes result draw/paint completion,
  stale invalidation, navigation, revisit, static mounting outside renderUI,
  pre-show fallback followed by the real result container, and announcement
  clearing. `node --check` and `git diff --check` passed.
- WSL R `testthat::test_local()` passed with filter
  `result-page-loader|viewer-accessibility-design|viewer-review-feedback|dataset-loading-progress`.
- Real Windows Brave, WSL R service, existing autoreloading preview at
  `http://127.0.0.1:8943/`, Demo (1,476 cells), native 1438 x 1084 viewport.
  Existing reduced-motion preference was left enabled; computed spinner
  animation is `none`. Final preview build: `2026-10-10T22:30:34.497667`.
- Groups, Marker genes, Most expressed genes, Enriched pathways and Extra
  material: common card shown on entry, actual chart/table populated and card
  dismissed. Groups revisit did not reopen it. No visible output errors.
- Gene: card belongs to the static result host, completion dismisses it;
  revisit and CD74 local update preserve local feedback with the page card
  hidden. CD74 reached `Showing: CD74`.
- HLA: actual loading card captured; explicit no-cluster result dismissed the
  card and cleared its announcement.
- Final Linked -> Gene -> Linked: fixed loading container verified; actual
  shared grid restored to its Linked wrapper and both views rendered. The
  immediate revisit snapshot precedes async grid restoration; the subsequent
  observation verifies the mounted canvases. No readiness timing was inferred
  from the earlier snapshot.
- Browser warning/error capture was empty. DOM inspection confirms elapsed time
  is outside live regions and page announcements are outside busy panes. This is
  structural accessibility verification, not a screen-reader listening test.

Evidence: `implementation-evidence/loading-presentation/browser-checks.json`,
`hla-entry.png` (actual card), `extra-result.png`, and `linked-final.png`.
The JSON includes an intermediate Linked entry that exposed the movable-grid
placement issue; entries prefixed `final-` verify its correction. Table/HLA/Gene
presentation was checked before that isolated Linked wrapper correction.
No artificial delay or injected loading state was used for screenshots.

This is a presentation smoke test, not the earlier 1M dataset/generation
acceptance protocol, a new all-page benchmark, or a claim that all remaining
application loading/focus behavior has been redesigned.
