# A-style feedback implementation

**Coverage update:** the initial Gene/shared-shell slice below was commit
`65be53641b30f87b12fe2c6c5310002610bf511c`. The user's all-page follow-up is
implemented and recorded in [ALL_PAGES.md](ALL_PAGES.md); that report supersedes
the initial operation-specific scope limitation below.

Date: 2026-10-10. Baseline `0e8781afb5f94ba680f3d1dbe9f294e18917e1dd`;
branch `卧槽/pr6_clean_perf`, existing checkout, initially clean. The user chose
**A / Minimal / Professional**, then authorised implementation. No new branch,
worktree, push, Builder integration, benchmark or performance experiment.

## Implemented scope

- Gene gene/mode/RGB controls acknowledge a committed selection locally. The
  existing canvas is retained. Text distinguishes the **last confirmed frame**
  from the requested genes until an accepted painted key matches the request.
- A small, visible-Gene-only R observer maps raw selectors to the existing
  canonical render key. This handles case-insensitive names and missing genes
  without duplicating gene validation in JavaScript. It uses the non-debounced
  selector reactive, not the expression-preparation reactive. Inactive control
  snapshots are isolated. No expression or metadata is requested by the observer.
- The two existing local `cell-view-ready` events now expose their captured
  render key and dataset context. Keys are captured before asynchronous painting
  so a later payload mutation cannot relabel an earlier callback. Existing GPU,
  RAF, visibility, paint and readiness checks are unchanged.
- The existing specialist selection event exposes its already-computed pending
  identity flag. A Gene-local message can show the selected index count while
  stable identities for summaries/table are still loading. It does not trigger
  hydration and disappears when the existing identity path resolves.
- The shared first-entry loader is a compact, non-intercepting corner status,
  rather than a full-content mask. Existing first-entry/revisit/paint lifecycle
  remains unchanged. It is not an earlier-ready implementation.
- Already-invalidated selected-result/summary outputs get their own small
  updating message after the existing 120 ms delay. Value/empty validation clears
  it; a nonempty reported error retains the underlying error and gets local
  explanatory text. Primary readiness never depends on these labels.
- The existing dataset-load overlay uses an opaque neutral background without
  blur, and suppresses the redundant page-loader presentation while visible.
  Native dataset change resets its old percentage. Backend percentage semantics,
  activation and completion remain unchanged; no synthetic total progress.
- Feedback uses text/ARIA plus colour, 140 ms colour/opacity transitions, and
  existing reduced-motion support. No copied canvases or new animations of rows.

The Gene message is intentionally limited to gene/mode identity. Projection,
filter and appearance controls are **not** falsely certified by a gene-only key.
The all-page shell presentation changes apply generally; this is not a rewrite
of every page's operation-specific state machine. Skeletons (B), staged
choreography (C), and blocking plot overlays (D) were not imported. Other audit
items, including layout reservation for every secondary panel and universal
canvas terminal-error ownership, remain recorded in `AUDIT.md`.

## Verification

Run from the repository root in WSL:

```sh
Rscript tests/bench/viewer-followup/interaction-feedback/run-tests.R
```

All seven suites passed: `cell-view-state`, `dataset-loading-progress`,
`gene-feedback`, `gene-progress-lifecycle`, `single-expression-incremental`,
`viewer-dataset-context`, and `viewer-review-feedback`. The JS adapter fixture
executes the production script with DOM/event doubles and passes **23 assertions**:
immediate pending, unpainted/stale-key rejection, rapid requests, missing and
case-normalised genes, message-after-paint ordering, revisit, A/B/A generation
isolation, hidden-radio input, RGB, and deferred selection identity state.
R tests cover the real feedback observer, canonical mapping and hidden-page gate.
Incremental-expression compatibility covers numeric/NA/RGB/order/stale-paint
behaviour. Node syntax checks for the three affected scripts and `git diff
--check` passed.

One pre-existing source assertion expected Gene's removed `debounceAfterFirst`
relay. `git show 0e8781af:inst/viewer/gene_expression/obj_projection_data_to_plot.R`
confirmed that baseline already used `shiny::debounce` (the `f4e4fe3b` change).
Only this obsolete assertion was aligned; no data or debounce implementation
was modified to satisfy the test.

### Real Windows Brave

Browser: actual Windows Brave through its browser extension, default desktop
viewport (screenshots 1369 x 915). R: fresh **WSL R 4.6.1**, current working-tree
production sources, localhost port 8942. The checked-in `server.R` launches the
real app with Mouse 1M and Demo, 100% cells, without injected benchmark/probe or
renderer code. This is not the synthetic prototype on port 8941.

Observed:

| Check | Result / evidence |
|---|---|
| Demo initial Gene | No residual pending after the empty frame paints |
| MS4A1 selection | Local text `Last confirmed: No gene selected · Updating: MS4A1`, followed by actual expression colours and `Showing: MS4A1` |
| Demo box selection | 745 highlighted/summary cells and 745 table entries (`demo-selection.txt`, `demo-gene.png`) |
| Gene -> Projection -> Gene | Returned to `Showing: MS4A1`; page loader not reopened |
| Invalid added gene | Existing `Not found in this data set: NOT_A_GENE` remains visible; valid MS4A1 frame retained and no endless pending |
| Mouse 1M / Gad1 | Real 1,000,000-point frame and Gad1 expression; local pending recorded in `mouse-updating-dom.txt` |
| Mouse box selection | 361,969 summary cells and 361,969 table entries (`mouse-selection.txt`, `mouse-gene.png`) |
| Mouse -> Demo | 1,476-point frame, no selected cells, empty old table, `Showing: No gene selected` (`demo-after-mouse.txt`) |
| Demo -> Mouse | 1,000,000-point frame, zero selection and empty table (`mouse-return.txt`) |
| Final-source repeat | After the final identity label/event-key capture additions, restarted the WSL server and retested Demo input, RGB, hover, and selection |
| RGB and hover | `Showing: RGB: MS4A1, —, —`; tooltip shows barcode, transcripts, genes, sample, cluster and cell type (`final-demo-rgb-hover.png`) |
| Final selection | 733 highlighted/summary cells and 733 table entries, identity status hidden after completion; primary loader hidden (`final-demo-rgb.txt`) |
| Browser console | Error/warning sample empty in both `console.json` and `final-console.json` |

R output was inspected throughout: no Shiny/application exception observed.
It did contain a BPCells dense-conversion warning and Plotly default trace/mode
warnings. This is not a warning-free R claim; neither reader nor plot code was
changed. The implementation's initial hidden-radio parsing issue was discovered
and fixed during this smoke test, with the DOM shape added to the JS fixture.

The deferred identity message's completion/hidden state was verified in the
final browser run; its brief pending phase is covered deterministically by the
event fixture, not claimed as a captured browser screenshot. Synthetic stale
events were tested in the fixture, not injected into the real browser.

Some screenshots taken immediately after an action still showed the previous
composited image while DOM text had changed. Subsequent stable screenshots and
actual selection/table interactions were used for acceptance. No numerical
input-to-visible-feedback grade, <=100 ms claim, or RAF root-cause claim is made.
Dataset neutral styling was inspected in source; no useful screenshot of its
short intermediate phase was retained. Its activation semantics are unchanged.

## Cost, boundaries and disposition

One small control message per active Gene selector/mode change is added, sized
by requested gene names and context, **not** by cell count. This is a new small
WebSocket UI message, not a claim of zero total payload change. The binary
expression/geometry protocol and large primary/static/aux payloads are untouched.
Client retained state is a control snapshot and render-key strings, not a cell
cache. There is no new full metadata materialization, backend read, image copy,
identity reorder, precision change, ready gate, RAF change or secondary demand.
No quantitative memory or performance equivalence benchmark was performed.

This bounded A implementation and its targeted checks are complete. No observed
new functional failure remains in the exercised scope. Exhaustive all-page
acceptance, new error fault injection and responsive/mobile certification were
not performed. Gene <=3 s and the previously closed Viewer performance topics
remain closed. Spatial ROI/background/contract changes remain for Builder.

Evidence is in `implementation-evidence/`. Counts in this report describe the
actual gestures, not prescribed future selection sizes. The local browser app
is left available at `http://127.0.0.1:8942/` for review. No push was performed.
