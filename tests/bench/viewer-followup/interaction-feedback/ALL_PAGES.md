# A-style feedback: all-page follow-up

The later 17-page structural and interaction sweep, including stored-result and
utility pages, is recorded in [FULL_SWEEP.md](FULL_SWEEP.md).

Date: 2026-10-10. Implementation baseline:
`65be53641b30f87b12fe2c6c5310002610bf511c` on `卧槽/pr6_clean_perf`.
The initial implementation did not cover enough of the selected design. This
follow-up extends the existing presentation across the six audited pages.
Existing checkout; no new branch/worktree, push, Builder work, profiling or A/B.

## Operation coverage

| Page / operation | Presentation added or retained | Completion source |
|---|---|---|
| Overview projection, colour, settings, group filters | Requested parameter shown immediately; existing plot retained; pending control border | Matching controls captured with prepared data and the existing painted render token |
| Gene genes / mean / panels / RGB | Retains the canonical-gene adapter from the initial commit | Existing canonical Gene render key |
| Gene projection, appearance, filters | Separate parameter feedback, so a gene-only key cannot certify a different projection/filter | Prepared controls and painted token |
| Linked initial entry | Distinguishes primary view from remaining annotations/additional views | Existing progressive primary-painted / complete events |
| Linked mean / panel / RGB selectors | Local requested genes; separate loaded/unavailable response, matching the full requested control values and mode | Small UI acknowledgement after the existing expression response; does not assert painted completion |
| Linked deferred colour field | Explicit loading-field message, followed by colour-mode application | Existing deferred-field application |
| Trajectory method, trajectory, colour, sampling/appearance/filter controls | Requested values remain pending until the actual frame; default/static route retained | Existing prepared/static frame token |
| Spatial dataset/plot/colour/feature/coexpression/settings/filter controls | Same parameter state; background independently reports loading/loaded/unavailable/none | Existing painted token, accepted appearance message and image token/context checks |
| HLA chain, colour, motif/filter/settings | Pending controls; explicit no-motif / guard / missing-dependency state; stale graph concealed without destroying its layout | Existing painted graph token; existing note's explicit terminal marker |
| Selection on specialist and Linked pages | Immediate selected count; deferred identity is named separately from no selection | Existing selection events, without requesting identities |
| Selected tables, group summaries, Trajectory auxiliary plots/tables, HLA tables | Independent compact updating/error text only for already demanded outputs | Existing Shiny invalidated/value/error events; 120 ms decoration delay retained |
| Navigation / revisit | Page-named loading starts on tab transition; stable status row; cached revisit does not replay first-load masking | Existing page/paint lifecycle; canvas pages never use global idle as ready |
| Dataset switch | Neutral dataset overlay retained; all new local operation/selection/response state reset | Existing dataset context/generation events |
| Hover, box/lasso/pan/zoom, focus, clear, settings drawer | Existing direct manipulation and pressed/active feedback retained; selection now has the common status row | Existing handlers; no delay or replacement animation |

Empty filtered frames say "No cells match these filters". HLA empty results
use neutral text; guard/error results use error text. A terminal result dismisses
loading without setting page-ready. Returning to an unchanged empty page does
not restart an endless spinner. A stale painted event cannot reveal a previous
graph after a terminal result; an accepted render mapping is required to recover.

## Implementation and bounds

- `cerebroFeedbackControls()` reads only an explicit allowlist of controls inside
  `isolate()`. It never enumerates their values indiscriminately (Shiny inputs
  also contain large selections) or calls metadata/expression readers. Snapshots
  are captured before debounce with prepared data. Trajectory appearance and
  HLA minimum-node values use the effective prepared/debounced values.
- A small custom UI message binds each snapshot to the existing render serial.
  The browser retains at most eight snapshots per view, rejects other dataset
  contexts, and clears only pending fields whose requested values match the
  painted token's snapshot. Message-before-paint and paint-before-message work.
- Overview excludes feedback from its existing data/frame deduplication;
  Overview/HLA can acknowledge an unchanged effective frame without redrawing.
- The existing local painted event had read `meta.render_token`, while the
  actual token is on the view payload. It now captures that token before waiting.
  The incremental-expression fixture now asserts the real latest payload token,
  rather than the old unrelated metadata token. GPU/RAF gates are unchanged.
- Trajectory's labels-only appearance message was missing dataset context and
  was rejected by the existing client freshness guard. It now carries the same
  existing context as other appearance messages; no second context system.
- Linked Selectize uses Shiny/jQuery changes, which the native change-only path
  does not reliably observe. The presentation adapter listens to the actual
  events and matches a small response snapshot for mean/panels/RGB, including
  empty and invalid selections. It adds no expression read or reactive observer.

Cost: extra small WebSocket control snapshots, proportional to selected controls
and group levels, not cell count. No new cell cache, full metadata materialization,
static resource, coordinate/expression payload field, protocol/precision change,
draw request, or auxiliary demand. The snapshot bound is not a measured heap
claim. No numerical performance improvement or <=100 ms guarantee is asserted.

## Verification

Windows Brave via browser extension, 1369 x 915 default desktop viewport;
**WSL R 4.6.1** serving the real production app on localhost 8942. No browser
state injection, fake ready event, network throttling or renderer monkey patch.
`server.R` accepts an optional fourth argument for the existing fixed Spatial
fixture, in addition to Mouse and Demo. New sessions reload session-sourced R
files and current JS. The Linked Selectize correction was tested in a fresh
browser session; the final mode-reuse/no-gene pending guard was checked in the
deterministic fixture.

Data:

- Demo: `inst/extdata/examples/example.crb`, 1,476 cells.
- Mouse: `cerebro_mouse_brain_1m_pages_clean_v1.crb`, 1,000,000 cells.
- Spatial: existing `cerebro_spatial_canonical_1m_acceptance_v1.crb`, deterministic
  1000 x 1000 canonical grid plus alignment image. Identity is unchanged in
  `../../viewer-final-protocol/final-acceptance/spatial-fixture.identity.json`.
  No new ROI/background content contract was imported.

Targeted suites (`run-tests.R`) passed: app-trajectory, cell-view-state,
dataset-loading-progress, gene-feedback, gene-progress-lifecycle,
hla-app-contract, single-expression-incremental, spatial-image-payload,
spatial-missing-coordinates, trajectory-selection-demand, view-feedback,
viewer-dataset-context, viewer-review-feedback. New JS fixture: 56 assertions
across the five specialist pages, Linked progression/selection/expression,
rapid requests, stale generation, ordering, empty/terminal recovery and appearance.
R tests verify isolated/allowlisted input reads and Trajectory appearance context.
After the final JS edits, those fixtures and syntax checks were rerun directly.

### Observed UI checks

| Scope | Observation |
|---|---|
| Demo Overview | `Colour by cell_type` immediately produced an Updating request; original plot remained; accepted frame cleared it and showed B cells / Monocytes / T cells |
| Demo HLA | Explicit no-motif result; enabling unconnected CDR3s rendered 573 nodes; leave/return preserved the network without residual loader |
| Final Demo Linked | Selectize MS4A1 request visibly changed to Updating expression, then Expression loaded; selected count 1 agreed across both plots and the common status |
| Final HLA empty revisit | No residual loader; terminal state retained; page-ready remained unset |
| Final Gene / rapid page revisit | MS4A1 canonical pending/confirmed labels remained separate from projection state; Linked -> Gene -> Linked -> Gene retained correct results, zero pending controls and hidden page loader |
| Mouse Trajectory | Colour changed from clusters to State with immediate request text; 981,807 displayed points; box selected 603 cells, first labelled identity-pending, then cohort count agreed at 603 |
| Mouse -> Spatial | Old selection and status cleared; new dataset name shown by neutral loading overlay |
| Spatial | 1,000,000 grid points and accepted alignment background; changing colour to sample showed requested value, then cleared; background state remained independent |

Browser console error/warning samples were empty for the observed sessions.
R output inspected during these operations showed no new application exception.
This is a bounded smoke result, not proof that every R warning or error path is
absent. The initial Linked adapter missed Selectize's synthetic change and could
show an empty unsolicited expression failure; both were corrected and the real
MS4A1 request/completion was repeated in the final session. No expression values,
reader, scaling, or binary transport was changed to fix those UI issues.

Evidence: `implementation-evidence/all-pages/` contains screenshots, DOM snapshots
and console samples. Earlier Gene-specific acceptance remains in `RESULTS.md`.
The numbers are gesture results, not prescribed future counts. Some immediate
screenshots still show a previous composited frame while DOM state has changed;
stable screenshots and real interactions were used, with no timing/RAF claim.

## Motion follow-up

The subsequent bounded animation cleanup is documented in [MOTION.md](MOTION.md),
including real Brave smoke evidence and the reduced-motion validation boundary.

The Groups / Marker genes structural staging follow-up is documented in
[GROUPS_MARKER.md](GROUPS_MARKER.md), with lazy-loading boundaries and targeted
browser / R verification.

## Remaining limits

This implements the common A language and principal operation paths; it does
not certify every dataset, button permutation, download, mobile layout, network
fault, or full application heap lifecycle. Existing immediate interactions were
retained rather than given artificial waiting animations. No new fault injection
was performed in the real browser; stale/error ordering is covered by fixtures.
Full reservation of every secondary panel and richer pinned-Details stage text
remain in the audit, outside this bounded implementation. Spatial ROI/content
contract integration remains a Builder topic. All closed performance topics
remain closed. No push.
