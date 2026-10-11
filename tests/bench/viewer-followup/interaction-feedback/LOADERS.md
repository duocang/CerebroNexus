# Consistent Viewer loading presentation

Visual sizing and hierarchy were subsequently revised in
[LOADING_CARDS.md](LOADING_CARDS.md) after user feedback that the small indicators
were too easy to miss. The measurements below describe this initial pass.

Date: 2026-10-10. Baseline: `eaeabeddbefbd139611fe1b058591453137a7da3`.
Existing branch `卧槽/pr6_clean_perf`, existing checkout; no push.

## Findings and resolution

The previous page sweep unified result structure, but left several independent
loading designs. This change gives their existing lifecycle owners one visual
language; it does not consolidate their request or readiness state machines.

| Surface | Change |
|---|---|
| Shared entry indicator for all 17 pages | One compact line, consistent “Loading [page]…” wording; remove redundant “Loading view” heading. |
| Canvas page and Gene operation feedback | Same 16 px amber ring and 12.5 px muted text while genuinely pending. Terminal/ready text does not spin. |
| Groups and stored-result controls/results, empty DataTables | Same ring, typography and spacing; retain reserved frame heights and immediate independent output arrival. |
| Immune's existing shinycssloaders wrappers | Replace default large animated bars with the shared ring and “Loading result…” at the result's top left. Preserve wrapper hidden classes and proxy dimensions. |
| Shiny progress notifications (including Gene) | Same ring and typography, quiet border and no raised shadow; preserve actual progress bar, detail, values and close lifecycle. Override Shiny's more specific 90% font rule. |
| Secondary results | Same pending ring; explicit error class prevents an error message from looking like an ongoing load. |
| Dataset overlay | Same ring and status typography; retain dataset title, real stages, progress and elapsed time. |
| Share import/export dialog | Use its existing working status row with the shared ring; remove redundant upload text/animated stripe and separate spinner design. |

CSS owns the glyph and motion. Existing reduced-motion rules disable animation.
No backend code, renderer, expression transport, metadata materialization,
payload, cache, generation, ready/paint gate or scientific result changed.
No deliberate delay, stagger, all-results reveal barrier or performance claim.

## Validation

Real Windows Brave, extension-controlled UI; WSL R 4.6.1 running the existing
`server.R` harness on port 8944. Viewport 1369 x 915; the existing reduced-motion
preference was enabled. Production UI and CSS, no browser state injection,
network throttling or fake progress. Dataset files are the same Demo and Mouse
1M inputs listed in `FULL_SWEEP.md`.

- Dataset load/switch: Demo -> Mouse, followed by a fresh Demo session. Real
  dataset overlay displayed and dismissed. No performance timing inferred.
- Most expressed genes: control/table pending states, then 20 visible table
  rows and page loader hidden.
- Groups: static frames with pending result labels, then populated outputs.
- Marker genes: live empty controls/results both had 16 px glyphs and the
  intended labels; later 20 visible table rows and page loader hidden.
- Immune Abundance: live wrapper had a 16 x 16 px ring, no old bar pseudo-elements,
  and “Loading result…”. Completed plot contained all three sample series;
  zero visible spinner containers and page loader hidden.
- Gene: Demo CD74 and Mouse expression updates; pending feedback used the shared
  ring. Native progress was observed. An initial computed font of 11.7 px exposed
  a Shiny specificity conflict, corrected and rechecked at **12.5 px / 16 px ring**.
  Final Demo CD74 state showed the result, zero native progress notifications,
  and hidden page loader. Leave/revisit did not leave a loading indicator behind.
- Browser warning/error capture was empty. See the R log limitation below.

Passed focused `testthat::test_local()` filter:
`viewer-review-feedback|viewer-accessibility-design|dataset-loading-progress|gene-progress-lifecycle|view-feedback|gene-feedback|coordviews-config`.
The executed suites were dataset-loading-progress, gene-feedback,
gene-progress-lifecycle, view-feedback, viewer-accessibility-design and
viewer-review-feedback. The initial run exposed one fixture expecting the old
word “Opening”; update that text expectation, retain its stale-generation
assertion, then rerun successfully. Both changed JS files passed `node --check`;
`git diff --check` passed.

This is a representative check of the shared loading mechanisms, not a claim
that every analysis/parameter combination was repeated. The prior 17-page sweep
is documented separately. Share upload/download lifecycle was not rerun here;
only its CSS changed.

## Evidence and limits

Evidence: `implementation-evidence/loaders/`:

- `corrected-progress.json`: final native progress font and ring size.
- `marker-placeholders.json`: live control/result placeholder styles.
- `immune-spinner.json`, `immune-complete.json`: busy and completed plot states.
- `gene-final.json`, `console.json`: settled Gene state and browser log capture.
- `dataset-loading.png`, `mouse-progress.png`: actual loading UI screenshots.
- Other screenshots show completed/preceding scenes. Screenshot capture can lag
  DOM changes; these are not timing evidence or proof of an intermediate paint.

The R log contained one caught error, `[IR ERROR] Plot 'clonalAbundance' failed:
argument is of length zero`, before the Mouse load messages. A later fresh Demo
Abundance rendered successfully. Its exact triggering inputs and baseline
reproduction are not established, so this run is **not** an error-free R-session
acceptance. Record this as a separate reproduction item, not as a proven loader
regression or an error to suppress. The existing BPCells dense-conversion warning
also appeared on first Mouse expression use. Backend and analysis code were not
changed in this presentation-only pass.

During the initial Mouse Gene entry, an early Gad1 attempt was followed by the
existing Cd74 selection being restored while controls initialized. A subsequent
Gad1 update after initialization worked. This was not isolated against baseline;
it is not evidence of a CSS regression and remains a separate lifecycle
reproduction item. No new performance investigation was started.
