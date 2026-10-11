# Viewer UI audit

Date: 2026-10-10. Baseline: `995ba67c2955c29380f5d4b48fd696a3ef4b0265`.
Branch: `卧槽/pr6_clean_perf`; existing worktree; initially clean.

Follow-up: [FIXES.md](FIXES.md) records the implemented corrections and real
Brave regression. It also corrects F1's incomplete producer inventory: a second
producer uses `pct` for positive-cell percentages, so a universal transcript-share
label is not safe. The findings below preserve the original audit baseline.

## Result

All 17 top-level Viewer pages were visited. Six actionable issue groups were
confirmed through rendered UI, keyboard interaction and source inspection.
The most important finding is incorrect scientific wording for the legacy
Most expressed genes `pct` metric. Other findings concern keyboard access,
accessible names and focus restoration. This is an audit, not a claim that every
control, parameter combination or scientific result has been validated.

No production code, dataset, request/paint gate, renderer, expression reader,
wire format or cache was changed. No benchmark, profile or performance experiment
was run. The established performance stop conditions remain in force.

## Environment and method

- Real Windows Brave, controlled through the browser extension UI tools.
- Desktop viewport: 1367 x 858. Narrow viewport: 390 x 844, reset after testing.
- Reduced-motion preference was already active and was not changed. Normal-motion
  animation timing and native mobile touch behavior were not separately tested.
- Existing live preview at `http://127.0.0.1:8943/` for Demo.
- Fresh WSL R service at port 8947, loading this repository with the unchanged
  `tests/bench/viewer-followup/interaction-feedback/server.R` harness. This is
  **WSL R, not Windows-native R**. No injected ready/timing instrumentation.
- Demo: `inst/extdata/examples/example.crb`, 1,476 cells.
- Fixtures in `C:/Users/wangx/Projects/shinyapps_docker/apps/cerebronexus/private-data/`:
  `cerebro_mouse_brain_1m_pages_clean_v1.crb`,
  `cerebro_spatial_canonical_1m_acceptance_v1.crb`, `demo_trekker.crb`,
  `demo_hla_tcr_dextramer.crb`.
- Reviewed page screenshots, DOM/accessible names, loaded and empty states,
  common controls, selected keyboard paths, dialogs/drawers and narrow layout.
  Fresh [Web Interface Guidelines](https://raw.githubusercontent.com/vercel-labs/web-interface-guidelines/main/command.md)
  informed the accessibility review. The design-taste skill was applied only
  contextually: this is a dense scientific analysis product, not a landing page.

## Confirmed findings

### F1 — P1: Most expressed genes explains the wrong quantity

`inst/viewer/most_expressed_genes/table.R:120`, `:124`, `:183`, `:225`

The rendered description says percentage of cells with nonzero expression, the
formula divides positive cells by group size, and the table renames `pct` to
`% of cells expressing`. The Info dialog repeats the same interpretation.
The repository producer instead calculates each gene's fraction of total
transcript counts: `R/getMostExpressedGenes.R:220-236` and `:290-306`.
Those quantities are different. Existing producer-compatible CRBs therefore
receive misleading scientific labels even when their numeric values are intact.

Reproduction: open Demo > Most expressed genes > Percent expressed; read the
description, formula and column heading, then compare the producer's `pct`
assignment. Screenshot: `evidence/findings-most-metric.png`.

Small fix: align the metric name, description, formula, column and Info text with
the stored metric contract. If multiple contracts exist, distinguish them
explicitly. Do not recalculate expression or silently reinterpret old data.

### F2 — P1: Dynamic Immune tabs are unreachable through normal keyboard navigation

`inst/viewer/www/viewer-shell.js:159`; dynamic content:
`inst/viewer/immune_repertoire/visualizations.R:48`

After opening Immune and clicking Abundance, ArrowRight leaves both focus and
selection on Abundance. All eight inactive tabs have `tabindex=-1`; the dynamic
anchors also lack `role=tab`. `syncTabs` imposes a roving tabindex without
completing the dynamic tab keyboard behavior. Tab cannot reach the other panels.
This is a reproduced keyboard-access failure, not a mouse-navigation failure.

Small fix: complete keyboard/ARIA initialization for dynamically inserted
tabsets in the shared shell, without changing the lazy content lifecycle.
Validate arrows, Home/End, selection and focus after renderUI insertion.
Do not reopen the previously fixed static HLA selected-state issue.

### F3 — P1: Material switches skip the keyboard and accessibility tree

`inst/viewer/most_expressed_genes/table.R:99`;
shared instances: `inst/viewer/utility_functions.R:5092`, `:5310`

The Most expressed genes "Show results for all subgroups" input has computed
`display:none`; its two labels/wrapper provide no keyboard-operable replacement.
Pressing Tab from Expression metric goes directly to the subgroup selector,
skipping the visible switch. The accessibility snapshot exposes its text but no
checkbox. Shared result controls use the same materialSwitch component; the
live keyboard reproduction was performed on Most expressed genes.

Small fix: retain a focusable, labelled native checkbox with visible focus and
Space behavior, styled through one shared component. Do not duplicate per-page
handlers. Recheck the shared instances on Marker/Enriched/result tables.

### F4 — P2: Linked selectors have no accessible names

`inst/viewer/coordinated_views/UI.R:187`, `:196`

The visible Colour by and Projection labels are not associated with the controls.
The generated `cv-pick-color-selectized` and `cv-pick-proj-selectized` textboxes
have no accessible name in the live snapshot. Visually adjacent text does not
name a form control for assistive technology.

Small fix: associate labels with the generated input, preserving IDs, values and
handlers; check the resulting combobox name and state after Selectize binds.

### F5 — P2: Closing Info loses keyboard focus

`inst/viewer/www/viewer-shell.js:185`;
`inst/viewer/utility_functions.R:5045`

Open Info and close with Escape. After the modal is gone, the focused element is
BODY rather than the invoking button. Reproduced in Linked and Most expressed
genes. The common modal handler supplies title/close semantics but no return path.
Settings and Share have working return behavior in the sampled interactions.

Small fix: remember the actual invoking element and restore focus after hide
when it is still connected and usable. Respect mandatory dialog semantics.

### F6 — P2: Legacy result-box and Plotly tools lack action semantics

`inst/viewer/shiny_UI.R:11`; Plotly presentation:
`inst/viewer/www/custom.css:2260`

Result-box collapse buttons expose "minus icon" instead of a meaningful action,
with no expanded/controls relationship. In Groups, Plotly Zoom in is an anchor
with only `data-title`, no href, role, tabindex attribute, title or aria-label.
A visual tooltip is not an accessible action name. The shared cell-view toolbar
already has named buttons and should not be treated as the same defect.

Small fix: supply action names and disclosure state in the shared box layer;
complete the Plotly toolbar semantics using the supported integration surface.
The attempted Plotly Enter check hit a tool selector deadline: this report
does **not** claim to have proven keyboard activation failure for that button.
Some desktop tools are about 26 x 22 px; assess target spacing before claiming a
WCAG target-size failure. A larger hit area is a usability improvement.

## Lower-priority presentation observations

- `inst/viewer/load_data/select_file.R:86`: single-dataset mode still instructs
  users to choose a dataset "below", although that selector is absent.
- Stored-result pages repeat the page title in the box and devote substantial
  first-fold space to stacked filters. This is a density/hierarchy opportunity,
  not evidence of slow computation or a reason to change data preparation.
- Demo HLA repeats the no-motif explanation in the main status and result area.
  The explicit empty state is correct; only its repetition could be simplified.

## Page coverage

| Page | Dataset / exercised state | Observation |
| --- | --- | --- |
| Data info | Demo; fixture dataset menu/switches | Loaded summary/preferences; single-dataset copy mismatch above |
| Projection | Demo | Frame and controls appear; loader cleared on later sample |
| Linked views | Demo, desktop and narrow | Two linked spaces; settings/share/info; F4 and F5 |
| Groups | Demo | Composition/metrics visible; F6 |
| Marker genes | Demo, desktop and narrow | Populated table; narrow table scroll stays inside container |
| Most expressed genes | Demo | Populated table; no-match search and clear; F1/F3/F5 |
| Enriched pathways | Demo default result | Populated table; no visible output error |
| Extra material | Demo | Stored table visible; no output error |
| Immune repertoire | Demo | Clonal projection and Abundance; keyboard failure F2 |
| Trajectory | Mouse | E18_neurogenesis frame, 981,807 plotted points, controls/legend visible |
| Spatial | Fixed 1M fixture | 1,000,000-point frame; background loaded; no document overflow |
| Trekker | Fixed fixture | Spatial and UMAP panels; 2,536 cells; no document overflow |
| HLA & TCR Motifs | Demo empty; 12,000-cell HLA fixture | Explicit empty state; populated 94-node network; Network data, HLA Associations and Data & QC loaded |
| Gene expression | Demo; narrow Settings | Initial frame/controls; responsive settings focus/inert behavior checked |
| Gene ID conversion | Demo | Populated conversion table; loader confirmed cleared on revisit |
| Colour management | Demo | Three sections visible; colour picker opens; no colour persisted |
| About | Demo | Content/links visible; no layout overflow |

## Shared behavior that passed the sampled checks

- No document-wide horizontal overflow in recorded desktop/narrow states.
  Narrow Marker table uses its own horizontal scroll container (319 px viewport,
  715 px content); this is intentional, not clipped document content.
- Narrow navigation closes after page selection; page heading receives focus.
- Narrow Settings fills the viewport and makes the background inert. Escape
  closes it, removes inert and restores the Settings trigger. Desktop Settings
  focuses its close control and restores its trigger on Escape.
- Loaded samples end without a retained page loader. Initial Projection and
  Gene ID samples were transitional and were followed by loaded samples.
- Both audit tabs returned no captured console warnings/errors. The owned WSL
  fixture service output contained dataset messages and no application errors.
  The existing 8943 server's entire historical R log was not collected.

## Evidence and limits

- `evidence/page-observations.json`: 17-page DOM observations plus revisits,
  narrow views and HLA subpages. Empty dataset fields mean Demo on port 8943.
- `evidence/interactions.json`: keyboard/modal checks, including exact DOM state.
- `evidence/console.json`: captured warning/error entries from the two audit tabs.
- `evidence/r-session.log`: collected output of the owned port-8947 WSL service.
- PNG files: page views, narrow Marker/Linked/Settings, dedicated fixture views
  and the metric finding. Some initial captures are intermediate; use the later
  DOM sample to distinguish loading from failure. No screenshot timing is a
  performance measurement.

The simple DOM label/size scan includes false positives such as Selectize's tiny
internal input and decorative icons. Only manually checked findings above are
asserted. Hidden-pane text in a DOM snapshot is not proof of visible overlap.
This is not full screen-reader certification, every Immune subtab/parameter,
upload/download validation, selection identity regression or injected server
error testing. Earlier acceptance evidence is not relabelled as a new test.
Dataset loading keyboard isolation and possible temporary loader/control overlap
were not decisively reproduced; they are not confirmed defects here.

Recommended next batch: metric wording, shared keyboard controls and modal focus,
then accessible names. Keep each fix small and test its affected interactions.
No cross-page architecture rewrite or performance work is needed for these findings.
