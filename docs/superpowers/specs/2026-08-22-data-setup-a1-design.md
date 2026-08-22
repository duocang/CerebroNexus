# Data setup A1 design

## Goal

Refine the existing Data setup page into a precise, restrained editing workspace. Preserve the current Builder workflow, Shiny inputs, preview contracts, and generated CRB/Viewer behavior.

## Composition

- Keep the existing single-column workspace and warm-orange Builder identity.
- Present the page in this order: stage heading, compact dataset summary, required core settings, CRB content disclosures, Advanced settings, and the existing stage footer.
- Treat the dataset summary as one quiet information strip. Show the dataset name, cells, genes, source format, and a small set of detected-content labels without creating metric cards.
- Keep Dataset name and Organism as the only always-visible core fields.
- Use the existing disclosure component for Groups, Cell cycle when available, Projections, Trajectories when available, Analysis results, Specialized content, and Advanced settings.
- Allow one primary disclosure to remain open at a time in the normal editing flow. Preserve user-controlled opening when a validation or focus action targets a specific disclosure.
- Keep disclosure summaries useful when closed: title on the left; included count and default selection on the right.

## Typography

- Continue using the Builder system sans stack; use the mono stack only for paths, identifiers, timings, and technical values.
- Stage eyebrow: 0.7rem, 700, uppercase, warm orange.
- Stage title: `clamp(1.5rem, 2vw, 1.75rem)`, 700, tight tracking.
- Introductory copy: 0.92rem, muted, maximum readable measure.
- Section heading: 0.95rem, 700.
- Disclosure title: 0.9rem, 600.
- Disclosure summary: 0.74rem, 500, muted, right aligned.
- Field label: 0.78rem, 600, muted.
- Field value and regular controls: 0.875rem.
- Avoid additional all-caps labels, oversized counters, or competing headline styles.

## Color and surface hierarchy

- Use neutral white surfaces, `--c-bg` for the page, and `--c-surface-2` only inside expanded disclosure bodies or quiet summaries.
- Reserve `--builder-action` for the stage eyebrow, disclosure chevrons, active controls, and primary action feedback.
- Use warm-orange fills only for selected or active states; do not tint every card.
- Use semantic green, warning, and red only for actual readiness states.
- Use one-pixel neutral borders and low-elevation shadows. Hover may raise border contrast slightly but must not scale cards.
- Keep content-type colors limited to compact detected-content labels; labels use small corner radii rather than pills.

## Motion

Use the A1 precise-motion profile:

- Hover, focus, chevron rotation, and simple color changes: 120ms.
- Disclosure expansion and stage handoff: 180ms using `cubic-bezier(0.16, 1, 0.3, 1)`.
- Disclosure content enters with opacity and at most 4px vertical movement. Closing is 140ms.
- Summary text changes cross-fade over 240ms without replacing or fading the whole card.
- Completion feedback plays once for 500–700ms. It may use a restrained warm-orange ring and slight translation; it must not bounce.
- Loading and Shiny recalculation must not restart stage-entry animations or reduce the opacity of stable content.
- Shadows do not animate. Ordinary scrolling and field editing do not trigger page-level animation.
- Under `prefers-reduced-motion: reduce`, remove translation and expansion animation while retaining immediate state and color changes.

## Interaction behavior

- The full disclosure summary remains the click target and has a visible keyboard focus state.
- Opening a disclosure must not trigger expensive preview work unless that disclosure requires it. Projection previews remain bounded and cached; the shared Builder preview limit is 1,000 points.
- Editing a field updates only its local state and summary. Saving, checking, restoring, and building remain explicit actions.
- Validation opens and focuses the relevant disclosure instead of showing a detached generic error.
- Empty optional sections are not rendered. Loading placeholders retain the final component height to prevent layout jumps.
- The final-dataset transition changes `Finish checking` to `Continue to Review` with one clear completion cue.

## Responsive and accessibility behavior

- Keep the two required core fields in two columns when space permits and stack them on narrow screens.
- Disclosure titles and summaries remain separated; summaries wrap below titles rather than clipping.
- Preserve native tab order, `aria-expanded`, status announcements, and form labels.
- Focus rings use the existing Builder focus token and remain visible against warm-orange selected states.
- All text and state colors must meet the existing Builder contrast expectations.

## Scope and verification

- Reuse existing Builder controls, tokens, disclosure markup, and server contracts.
- Do not introduce a new component library, animation dependency, data conversion, or compatibility path.
- Do not change generated CRB data, Viewer App settings, external workbook behavior, Sheet contracts, or Builder project persistence.
- Static verification should cover R parsing, JavaScript syntax, CSS diff checks, and existing UI contract assertions. App, browser, and local test execution remain separate from this design specification.
