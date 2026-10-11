# Viewer immediate feedback audit and prototype review

Date: 2026-10-10. Baseline `312f36a837a44478a7677bce77fee2350d13ac62`, branch
`卧槽/pr6_clean_perf`, existing checkout, initially clean. Production files are
unchanged. No Builder work, performance experiment, profiling or data-layer
refactoring is included.

The main UX opportunity is to make the scope and stage of work legible. The
Viewer already has loading and progressive mechanisms, but users encounter
several visual systems: page overlays, output dimming, Shiny progress, local
pending details and dynamically inserted panels. The recommendation is a common
presentation vocabulary on top of the existing lifecycle, not another lifecycle.

## Evidence and measurement boundary

- **S: current source evidence.** All six pages and their shared shell were
  inspected at the baseline. Source-configured delays are identified as such.
- **H: historical observation.** Existing followup reports explain actual
  computation and paint waits. They are not fresh UX timing measurements.
- **P: prototype verification.** Fresh Windows Brave interaction with the
  isolated local demo establishes that its controls and scenarios work. It does
  not certify production Viewer behavior or performance.
- **U: unresolved.** Input-to-visible-feedback and primary-usable timing for
  the current production HEAD were not newly measured. The existing Brave
  Viewer tab at port 8876 contains old diagnostic controls and telemetry; its
  running source identity is not pinned to this HEAD. Its screenshot and old
  telemetry were not promoted into current evidence or used to reopen a bug.

No production operation is assigned an invented 0–100/100–200/200–500/>500 ms
grade. In particular, a **120 ms timer after output invalidation is not a
120 ms input-to-feedback measurement**: transport, R scheduling and browser
painting can precede/follow that timer. Similarly, render/ready event receipt
does not establish physical usability by itself.

For next-round targeted UI acceptance, record four distinct markers from the
same trusted operation: input, visibly rendered acknowledgement, actually usable
primary, and demanded secondary completion. Grade only the first interval:
0–100 ms excellent; 100–200 acceptable; 200–500 sluggish; >500 priority problem.
Keep cold entry, update and revisit separate. No global Shiny-idle substitute.

## Current feedback mechanisms worth preserving

Source paths in this report are relative to `inst/viewer/` unless stated otherwise.

| Mechanism | Source anchor | Current behavior |
|---|---|---|
| Output-level feedback | `www/viewer-shell.js:157–229`, `www/custom.css:2264` | Marks `aria-busy` at invalidation; after a configured 120 ms, dims populated output to 0.7 and announces updating; value/error clears it |
| First-page loader | `www/viewer-shell.js:232–405`, `www/custom.css:2062` | Shows on first tab entry; canvas pages wait for painted primary events; loaded revisits avoid it. Covers main content with a nearly opaque layer and intercepts pointers |
| Dataset switch | `www/dataset-context.js` `beginSelection`; `www/viewer-shell.js:536` | Selector change starts dataset feedback locally; context gate becomes pending. Dataset overlay and page loader are separate presentations |
| Gene progress | `gene_expression/obj_projection_expression_levels.R` progress helpers and expression reactive | Progress is render-key scoped, closes off-page, and does not reopen for an already painted key. It is initiated in R, not by a generic local control-pending state |
| Gene selector retention | `gene_expression/UI_projection_input_type.R` | Gene inputs remain mounted; search has a 300 ms load throttle. That throttle is not a delay on local selected-value display |
| Primary and aux separation | `www/cell_views.js`, `utility_functions.R` deferred aux helpers | Cell frame can precede stable identity/hover columns. Local selection can exist while stable IDs are pending |
| Linked progressive loading | `coordinated_views/server.R`, `www/cell_views.js` | Painted primary precedes requested supplement/clone stages; page loader does not intentionally wait for all secondary content |
| Trajectory secondary gates | `trajectory/UI.R:96–155` | Auxiliary sections arm after primary readiness and user scroll intent, then viewport intersection. Preserve the lazy behavior |
| HLA early/empty guards | `hla_tcr_motifs/visualizations.R:409`, `:709` | No dependencies, no segments, size guard and no motif clusters have distinct existing explanatory messages |
| Existing motion support | `www/custom.css`, `www/coordviews.css` | Existing short transitions and reduced-motion rules already exist; avoid adding a competing animation layer |

## Per-page and shared interaction audit

“Unmeasured” below is a deliberate evidence boundary. “No direct gate” means
the primary path is separated in source, not that R or the browser can never
experience contention from other work.

| Page / operation | Immediate feedback | Primary usable | Current visual issue or risk | Secondary blocks primary? | Recommended treatment |
|---|---|---|---|---|---|
| Overview first entry | Local tab loader; actual paint latency unmeasured | Existing painted cell-view event; time U | Nearly opaque whole-content loader hides useful layout and controls (S) | No selected-table gate found | Stable plot frame; one local loading owner |
| Overview projection / colour / filter | Local selector value; generic output cue after invalidation + 120 ms; U overall | New rendered frame; U. `debounceAfterFirst(...,150)` is source scheduling | No consistent selector-to-plot requested/displayed provenance (S) | No direct gate | Immediate control pending and labelled retained frame |
| Overview box selection / table | Local highlight in shared engine; stable identity may still be pending; U | Selection highlight precedes server outputs | `renderUI` table/plot appear only after selection; insertion can shift layout (S/risk) | Table follows selection | Selection count first; compact reserved secondary shell |
| Gene first entry / new gene | Mounted selectize changes locally; R progress starts later; U | Existing painted render-key acknowledgement; no new time | Selected gene and still-displayed gene are not a unified visible pending contract (S) | Summary is gated behind primary/summary readiness | Show requested Gad2 immediately; label old Gad1 until replacement |
| Gene repeated / rapid input | Existing render-key and stale-result handling; U | Latest accepted painted result | Several visual feedback owners, rather than one control/plot status (S) | No proven table-caused primary wait | Latest request owns pending label; do not reset selector from old result |
| Gene selection summary / table | Local selection first, deferred identity possible; U | Highlight first; summary/table after valid canonical identities | No-selection, identity-pending and summary-pending deserve clearer distinct visual states | No direct gate | Keep highlight; label identity or selected table as loading independently |
| Gene leave / revisit | Retained selector; hidden progress close and painted-key skip already fixed | Cached frame where eligible; U | Do not reopen the already fixed residual-progress/selector bug | No direct gate | Preserve existing behavior; no new full skeleton on revisit |
| Linked first entry | Local loading message and page loader; U | Painted primary before supplement | Users may read “ready” as meaning every view/annotation is complete (S/H) | No: progressive supplement is requested after primary | Main plot ready; remaining views/details show their own status |
| Linked colour/gene / selection | Shared engine highlights locally; some server paths demand values/metadata; U | Depends on operation; no new timing | Selection panels are dynamically inserted and staggered; UI can feel like content arrives all at once (S) | Usually secondary follows primary; contention not measured | Stable selected-cohort summary, no table row animation |
| Linked hover / pinned Details | Lightweight/deferred hover; full Details is on demand | Existing accepted pinned-tooltip behavior preserved | Same visual area can progress from partial to fuller metadata without one common stage label | Details does not gate primary | “Cell details loading” only where needed; never preload all metadata |
| Trajectory method/name / State | Local selector; source preserves primary controls; U | Static frame/State and excluded-index path; U | First-page loader still covers context despite a stable frame structure | Aux/secondary not an unconditional primary blocker | A-style update plus concise current method/name label |
| Trajectory scroll to metrics | Demand is gated by primary, scroll intent and intersection | Primary already available | Secondary sections start from a minimal gate and later gain height (S) | No, direction is primary -> secondary | Reserve compact section headers; load only on demand, not a huge eager skeleton |
| Trajectory selected table | Local selection; table appears by `renderUI`; U | Highlight before table | Table arrival changes layout; old appearance-control blocking bug is already fixed | No direct gate | Independent table status with stable identity count |
| Spatial first entry / filter / feature | Local controls; render observer resolves data before `withProgress` (S) | Existing resource-first frame and painted gate; U | Spatial progress does not cover time spent resolving `data` before it opens | No unconditional Moran prerequisite found | Local pending immediately, plot-scoped feedback; keep lazy Moran |
| Spatial background change | Independent image request/token and image state; U | Current accepted image after load | Need explicit image-loading vs feature-updating scope | Image-specific, not a reason to block unrelated primary | Small background-only status; retain current coordinate contract |
| HLA chain / scope / filters | Local controls and shell/output feedback; U | Existing graph/cell-frame path; computation may be needed | Changing graph scope is a different operation from allele recolouring but feedback does not uniformly name that difference | No general association-table gate established | “Building network” vs “Updating colours”; local secondary status |
| HLA empty / capped / unavailable | Existing explanatory notes (S) | No graph expected in these states | Notes can be separated from the blank plot region; risk of mistaking absence for loading | Not a loading wait | Put existing explanation inside stable empty/error surface; preserve wording distinctions |
| Dataset A -> B -> A | Local switch event starts feedback and invalidates context; U | New generation's painted frame | Modal dataset loader plus page loader can create a two-step loading presentation (S); not evidence of stale data leakage | Dataset validation legitimately precedes primary | Neutral frame labelled with new dataset; no retained old-dataset plot |
| Buttons and secondary panels | Press styles exist; operation-specific pending not uniform | Operation-specific | A pressed state alone does not say a backend operation was accepted | Depends on action | Compact pending text on initiating control, no extra global spinner |
| Errors across canvas pages | Dataset error unavailable message, local errors and output errors exist | Terminal error should not await readiness | Generic shell error fallback skips canvas tabs; a uniform terminal-error-to-loader contract is not evident from this path (S, needs targeted verification) | Not a computation dependency | Dedicated terminal error state; record for next round, no opportunistic fix |

## Top priorities

Ranking reflects source-supported UX impact and integration risk, not measured
latency rankings. Types: A computation; B late acknowledgement; C unnecessary
waiting; D rendering/layout presentation; E ambiguous state design.

| Rank | Page / operation | Type | Problem and user impression | Direction |
|---|---|---|---|---|
| 1 | Gene and other repeated parameter updates | B/E | Control changed but the plot has no uniform “requested vs displayed” contract: “did my choice apply?” | A pending selector + plot provenance; latest request only |
| 2 | First entry across six pages | D/E | Full-content masking removes orientation and blocks controls: “the entire page is unavailable” | B structural frame, compact primary loading |
| 3 | Spatial feature/filter updates | B | Progress is opened after data preparation in the observer: a real work interval may have no operation-local acknowledgement | Immediate local pending, existing progress remains truthful |
| 4 | Selected-cell summary/table | D/E | Conditional insertion and staged arrivals can shift layout or look like missing results | Fixed or bounded secondary shells; distinguish no selection from pending |
| 5 | Linked and Trajectory progressive content | E, prevent C regression | Primary is already separated, but one broad loading vocabulary obscures which parts are usable | C stage text; keep secondary independent; no new complete gate |
| 6 | Dataset switch | D/E | Dataset overlay then page loader creates a presentation handoff; retained old data would be dangerous | New dataset label + neutral frame through the handoff |
| 7 | Hover identity / Details | E | Partial cell information can look incomplete without explaining that details are on demand | Small local details-loading label, preserve deferred work |
| 8 | Empty/error on graph/canvas pages | E | Valid no-data/guard states and terminal errors need clearer placement and loader ownership | Separate Loading / No data / No selection / Error; verify terminal path later |

There is **no new evidence that the backend is universally fast**. B/D/E improvements
can remove uncertainty regardless of compute duration. Known real waits remain:
first expression/backend preparation, metadata or graph work, browser apply and
paint confirmation. The historical Gene diagnostic had GPU completion at
1839.5 ms and painted acknowledgement at 3429.4 ms; that gap is not all R/JS
computation. The historical Linked tail recorded primary usable at 5557.6 ms
and complete at 10620.0 ms, with secondary work after primary. These are distinct
old diagnostic samples, not current UX benchmarks or evidence to change RAF.

References: `../gene-diagnosis/CLOSEOUT.md`, `../linked-tail/RESULTS.md`, and later
`../linked-tooltip/RESULTS.md`. The later tooltip fix supersedes the earlier
functional failure. Gene progress and selector fixes, Trajectory lazy table fix,
and the last cache-lifecycle cleanup remain completed work.

## Four interactive alternatives

Open `prototype/index.html` directly, or serve that directory with a local static
server. It has no dependencies, remote fonts, telemetry or real dataset reads.
All four alternatives share the same controls and simulated durations.

| Option | Benefit | Drawback | Relative implementation cost after selection | Performance risk |
|---|---|---|---|---|
| A Minimal / Professional | Least obstruction; old result remains legible; pending input and provenance are explicit | First entry is visually sparse; subtle indicator may be missed | Low to medium: control/plot presentation binding | Low if DOM-only; do not duplicate full production canvases |
| B Skeleton / Structured | Predictable geometry and section height; first entry feels intentional | Plot skeleton may feel like a general website; excessive reserved space is undesirable | Medium: responsive shell sizing and empty-state ownership | Low with bounded CSS placeholders; avoid many shimmer layers |
| C Progressive Reveal | Best explanation of primary vs secondary availability | More labels and intermediate states; stages must come from real signals | Medium: map existing primary/aux/complete events to visible parts | Low if it only presents current events; high if implemented by changing readiness |
| D Overlay / Data app | Very explicit affected region; prior plot remains underneath | Most obstruction; makes ready content appear less usable | Low to medium: local overlay and pending affordance | Low for opacity-only overlay; avoid blur and per-frame canvas work |

Recommended discussion starting point: **A for existing-result updates, B's
bounded structural placeholder for first entry, and C's primary/secondary
wording**. This is a proposal for the user's choice, not a selected implementation.
Pure D is less suitable for inspection-heavy point clouds.

The demo includes all six page names with representative synthetic layouts;
Linked has four coordinated panes, Trajectory a branch-like cloud, Spatial a
grid and HLA a small network. These are interaction illustrations, not faithful
scientific datasets or replacements for production renderers.

## Proposed unified presentation contract

1. **Immediate acknowledgement:** update the selected value locally and show
   pending by the initiating control. Target visible response within 100 ms,
   accept 100–200 ms; measure rather than infer this from a timer.
2. **Anti-flicker:** the input acknowledgement is immediate. A larger secondary
   loading decoration may wait roughly 120–150 ms; it must not delay the result,
   readiness, input value or error. Fast completion needs no forced spinner.
3. **First entry:** reserve plot bounds and compact secondary headers. Show a
   neutral frame or light skeleton, not fake scientific points. Do not eagerly
   request metadata or below-fold metrics merely to populate a skeleton.
4. **Update within one dataset:** keep the previous accepted result and label
   its parameter. Show the new requested value separately until its accepted
   frame replaces it. No misleading new legend on old colours.
5. **Primary and secondary:** primary interaction follows the existing usable
   gate. “Statistics loading” or “Selected cells loading” never reopens the
   primary loader. Identity-pending selection is distinct from no selection.
6. **Dataset switch:** retire old result/selection immediately at pending and
   use a neutral surface labelled with the new dataset. Same-dataset retained
   result behavior must not be applied across dataset generations.
7. **Terminal states:** Loading, Updating, No data, No selection and Error have
   different text and actions. Errors offer retry where meaningful; empty states
   explain how filters/selection can change the result. Keep valid HLA guard text.
8. **Motion:** use 100–150 ms opacity for ordinary changes, 200 ms upper default;
   no bounce, table-row choreography, canvas translation or large blur. Do not
   extend a spinner just to finish its animation. Respect `prefers-reduced-motion`.
9. **Accessibility:** visible text plus `aria-busy`; polite announcements for
   meaningful state transitions, not every progress tick. Keep focus and selector
   mounting; retain explicit keyboard focus outlines and controls.
10. **Progress honesty:** named stages are preferable to percentages without a
    real denominator. Existing backend percentage stages must not be relabelled
    as end-to-end rendered completion.

The demo's 140 ms old/new cross-fade uses two small synthetic canvas surfaces
for illustration. That implementation is **not** a recommendation to allocate
a second million-cell production canvas or copy geometry. Production feasibility
must preserve the current renderer and use its existing retained frame where
possible; presentation may use DOM status/fades if true cross-fade is costly.

## Prototype validation

Windows Brave extension automation opened the isolated prototype at
`http://127.0.0.1:8941/`; this is distinct from the historical production tab.

- A/B/C/D visual states inspected and screenshots saved in `evidence/`.
- First entry, requested parameter vs old plot label, primary-ready/secondary-
  pending, error/retry, no-data, no-selection and page-return states exercised.
- Rapid Gad2 -> Slc17a7 input eventually showed only Slc17a7 as the accepted
  plot and statistics provenance. The tool's wait command timed out before its
  final observation; a subsequent DOM read verified completion. This was not a
  timing measurement or app timeout.
- Dataset switch to Demo removed the old plot/selection and showed Demo/pending.
- All six demo page controls reached primary-ready with separate secondary
  pending, including the HLA node-selection label.
- Actual pointer drag selected 353 synthetic points in one check. The selected
  count and independent pending-table message agreed. That number is not an
  expected production selection size.
- Reduced-motion control exercised; CSS also respects the OS media query.
  Cross-fade cleanup handles a reduced-motion change during a transition.
- Node syntax check passed. No prototype-origin JS error was observed in the
  captured log sample; Brave extension-origin redeclaration errors were present
  and are not attributed to the prototype. No extension settings were changed.
- Desktop screenshots were visually reviewed. Narrow-screen CSS is included;
  it is not a claim of full mobile/browser/accessibility certification.

No production UI smoke pass, R-session pass, end-to-end latency improvement or
full Viewer regression is claimed. Production files and their previous results
remain unchanged.

## Next round after the user chooses

Only after a visual direction is selected: map one presentation state per
existing context/render key; start with Gene parameter feedback, then selected
secondary shells, shared first-entry/dataset presentation, and finally page-
specific HLA/Spatial empty and error cases. Verify each affected interaction
without changing reader, geometry, caching, wire, RAF or ready/paint semantics.
The precise implementation scope will be agreed in that next round. This round
ends with the prototypes and review choice; Builder remains unstarted.
