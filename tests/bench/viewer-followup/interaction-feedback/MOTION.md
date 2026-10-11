# Viewer motion cleanup

Baseline: `c7177d51740c72dc17a4fecd316c1b8b215ef3ea`.
Branch: `卧槽/pr6_clean_perf`; same checkout, no push.

## Changes

This follows the selected Minimal / Professional direction. It removes repeated
decorative motion without changing when results become usable.

| Surface | Change |
|---|---|
| Shared navigation and page/subtab revisits | Remove sidebar entrance and blanket 280 ms content fade; existing content appears immediately |
| Gene controls | Remove invalidation-driven collapse and slide, RGB entrance and 35/70 ms channel staggering; keep controls in place until their existing Shiny replacement |
| Shared Settings drawer | Replace full-width slide with 120 ms opacity; shorten cosmetic unmount delay to 140 ms; hidden drawer does not intercept pointers |
| Buttons / stat cards | Remove press scaling/translation and hover lift/shadow animation; retain colour, border and focus feedback |
| Linked selection results | Remove staggered rise on every composition/plot/table rebuild |
| Linked canvas / minimap | Remove CSS width/height interpolation; keep minimap opacity only at 120 ms |
| Linked background scope / Details skeleton | Replace expanding shadow pulse with border cue; retain static placeholder rows without repeated pulsing |
| Reduced motion | Disable CSS animations and transitions, including pseudo-elements, instead of making infinite animations run at near-zero duration |

The Gene JS removal only removes handlers for the deleted cosmetic `is-changing`
class. It does not remove a renderer RAF or painted acknowledgement. No R production
code, reader, geometry, wire protocol, payload, cache, ready gate, or data lifecycle
changed. Existing focus layout FLIP, data-coordinate transitions, and renderer
scheduling were left intact. This is not a claim to have removed every animation.
Compact loading/status and ordinary hover/focus cues remain.

## Verification

- `node --check inst/viewer/www/settings_drawer.js`: passed.
- WSL R: `testthat::test_local(filter="viewer-review-feedback", reporter="summary",
  stop_on_failure=TRUE)`: passed. Updated the pre-existing assertions that required
  the now-removed Gene entrance effects; no new mirror-only test suite.
- `git diff --check`: passed.
- Real Windows Brave, existing production smoke server at localhost 8942, R in
  WSL, Demo (1,476 cells), existing viewport 1369 x 915.
- Gene mean -> RGB: all three channel controls rendered and were accessible;
  page/control computed animation names were `none`, opacity 1, no transforms.
- Open/close/reopen Settings: controls accessible, `aria-expanded` correct,
  closing restored focus to the Settings button, no open drawer remained.
- Gene -> Overview -> Linked -> HLA -> Gene: navigation worked; HLA's explicit
  no-motif state remained; Gene retained RGB mode on return.
- Linked box selection: 3 cells; both plots, cohort summary and selected-cell
  table agreed (`Showing 1 to 3 of 3 entries`). Selection result blocks had no
  entrance animation. Canvas computed transitions were `none`.
- Captured browser error/warning sample: empty.

The actual browser reported `prefers-reduced-motion: reduce`. The smoke therefore
validates that preference; ordinary-mode 120 ms drawer CSS was inspected from the
loaded stylesheet but its visual duration was not independently exercised. No OS
preference was changed. No performance timing, A/B, profiling, or speedup claim.
No new Mouse/Spatial/Trajectory dataset run was needed for these shared cosmetic
changes; the earlier functional acceptance remains in `ALL_PAGES.md`.

Evidence: `implementation-evidence/motion/` contains Linked computed styles,
selection/table DOM, Gene drawer DOM, screenshots, and the console sample.
