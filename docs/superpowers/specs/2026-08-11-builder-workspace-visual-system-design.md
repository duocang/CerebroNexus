# Builder Workspace Visual System Design

## Problem

The Builder stages currently use inconsistent container and action patterns. Data setup places its readiness message and primary action inside the stage card, while Review renders a separate confirmation block after the stage card. Build then wraps the entire workbench in another card. Ordinary sections, datasets, conditional settings, and page shells all use similar bordered containers, so the interface lacks a stable visual hierarchy.

The redesign must make Upload, Data setup, Review, and Build feel like one professional desktop workflow without changing Builder behavior or the bottom stage navigation.

## Direction

Use a compact, professional-tool layout. The stage shell stays flat, while major content groups sit on white lightweight panels over the gray workspace. Each panel uses the same subtle border, 14px desktop radius, and low shadow as the Viewer reference. Tinted surfaces remain reserved for conditional or exceptional states.

The governing rule is:

> A page is not a card, and an ordinary section is not a card.

## Shared stage anatomy

Data setup, Review, and Build use the same order:

1. Stage header: stage name, task-oriented title, and one sentence of context.
2. Summary strip when useful: compact facts about the active dataset or frozen plan.
3. Flat content sections separated by vertical spacing and quiet rules.
4. One stage footer inside the stage shell.

The stage footer contains a short status at the left and actions at the right. It is in normal document flow rather than sticky, because the fixed bottom workflow navigation already owns the viewport edge. On narrow screens, the status appears above full-width action buttons.

## Component semantics

The UI has four structural levels:

- `stage shell`: the active page. It has no border, radius, background, or shadow.
- `stage section`: a major group of related fields or read-only facts. It uses one white lightweight panel with a heading, optional description, subtle border, and low shadow.
- `object`: an independently identifiable item such as one of several datasets. It may use a light border and small radius.
- `state panel`: a warning, error, progress state, or conditional group such as expanded Viewer App settings. It may use a quiet tinted background.

Nested general-purpose cards are not allowed. A stage panel may contain rows or a genuinely independent object, but not another generic stage panel. Status and validation treatments retain their semantic colors.

## Visual rhythm

Use one compact spacing scale derived from existing tokens:

- stage header to body: 24px
- section to section: 32px
- section heading to content: 12px
- related form rows: 16px
- final section to stage footer: 40px
- footer top padding: 20px

Major stage panels use the existing 14px large radius and `shadow-1` on desktop, reducing to 10px on mobile. Objects and state panels inside them remain within 6–8px and must be visually quieter than the parent panel. The stage shell itself has no border or shadow, and the active workspace must not add amber elevation merely because it is current; the fixed progress navigation already communicates the active stage.

## Stage-specific design

### Upload

Keep the upload empty-state surface because it is a bounded interaction with a distinct state. Do not flatten file rows or errors that need object/state boundaries. Align its spacing and heading rhythm with the shared stage shell where practical.

### Data setup

- Remove the bordered/elevated stage wrapper.
- Add a task-oriented header and concise intro.
- Compress active-dataset identity into a summary strip rather than a separate summary card.
- Present Import and Inspect, Core content, and Optional content as three consistent lightweight panels.
- Keep borders only for genuinely independent controls or data objects inside those sections.
- Replace `Ready to review · N datasets` with the shorter `N dataset(s) ready`.
- Place the status and `Continue` in the shared stage footer.

### Review

- Remove the bordered/elevated stage wrapper.
- Keep the frozen revision, dataset count, and output count in one compact summary strip.
- For one dataset, present its facts as a flat subsection. For multiple datasets, use light object boundaries to preserve identity.
- Keep Datasets, CRB content, and Output in consistent lightweight panels.
- Delete the separate `Ready to continue?` heading and explanatory confirmation block.
- Move `Back to Data setup` and `Continue to Build` into the shared stage footer, alongside a short `CRB plan ready` status.
- The footer must be a descendant of the element carrying `data-workflow-stage="review"`.

### Build

- Remove the bordered/elevated stage wrapper.
- Present confirmed-plan facts in the shared summary strip.
- Present Output type, Destination, and non-empty Build status as consistent lightweight panels.
- Use a quiet state panel only for Viewer App settings revealed by the output choice.
- End with the shared stage footer: `Back to Review` and the primary build action when the selected output and destination are ready.

## Reusable UI boundary

Introduce or normalize a small set of UI helpers/classes rather than creating more stage-specific action layouts:

- `builder-stage-shell`
- `builder-stage-header`
- `builder-stage-summary`
- `builder-stage-section`
- `builder-stage-footer`
- `builder-stage-footer-status`
- `builder-stage-footer-actions`
- `builder-object`
- `builder-state-panel`

The existing public input IDs and server events remain unchanged. This is a structural and visual refactor, not a workflow-state rewrite. Stage-specific UI functions provide their body content and actions to the shared primitives.

## Migration order

1. Add the shared shell, section, summary, and footer primitives plus their responsive CSS.
2. Move Review confirmation inside the Review stage and replace its bespoke block with the shared footer.
3. Convert Data setup to the shared anatomy and flatten its ordinary section containers.
4. Convert Build to the shared anatomy and restrict the tinted panel to conditional Viewer App settings.
5. Align Upload spacing without removing its legitimate empty-state and file-object boundaries.
6. Remove obsolete stage-specific card, margin, and action selectors after every consumer has migrated.

The migration must preserve server behavior, input IDs, frozen-plan identity, build locking, and bottom progress navigation.

## Responsive behavior

At desktop widths, the footer status and actions share one row. At 40rem and below, they stack with the status first; buttons use the available width and retain normal control height. Long paths and dataset names wrap or truncate without widening the workspace. The footer must leave enough bottom space that the fixed stage navigation never covers it.

## Verification

Automated UI structure checks should assert:

- the active Data setup, Review, or Build page contains exactly one stage shell and one stage footer;
- the Review footer is inside its `data-workflow-stage` root;
- the removed `Ready to continue?` copy is absent;
- action input IDs and enabled/disabled behavior remain unchanged;
- no general stage shell retains the generic card class;
- every major stage section uses the shared lightweight panel treatment;
- nested generic cards are absent except explicitly allowed object/state structures.

Run focused server and UI tests for stage navigation, review confirmation, Build option locking, and authentication settings. Then run the project Builder test suite once.

Browser verification must cover Data setup, Review, and Build at desktop, tablet, and approximately 390px mobile widths. Check visual rhythm, footer containment, keyboard focus, long content wrapping, validation states, Viewer App expansion, and non-overlap with the fixed bottom navigation.

## Out of scope

- Changing the four-stage workflow or bottom progress-navigation behavior
- Changing CRB extraction, plan freezing, build execution, or authentication semantics
- A new color palette or typography system
- Reworking every legacy Builder component outside the active stage pages
