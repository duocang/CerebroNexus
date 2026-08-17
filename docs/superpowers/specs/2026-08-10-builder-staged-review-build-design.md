# Builder Staged Review and Build Design

## Goal

Make the Builder follow the user's actual sequence of work:

1. upload and inspect data;
2. configure datasets and the Viewer;
3. review one read-only project summary;
4. confirm the reviewed revision;
5. choose an output folder and build.

Build-related controls must not exist in the rendered interface before the
reviewed project has been explicitly confirmed. In particular, importing data
must never reveal the current full-width output action bar, its App checkbox,
its build summary, a disabled Build button, or a Review button.

## Problems in the current flow

The current page appends every stage into one long workbench and renders the
output action bar whenever a ready dataset or pending import exists. This
creates three conflicting models:

- loading is visually accompanied by output and Build controls;
- Review is both a section in the workbench and a separate action button;
- each dataset has its own `Looks good` action even though the release is one
  project-wide artifact.

The result exposes a later operation too early, duplicates Review navigation,
and makes confirmation appear to certify individual dataset cards rather than
the exact project that will be built.

## Chosen product model

The Builder is a four-stage workflow:

```text
Upload -> Configure -> Review -> Build
```

These are application states, not four cards in a single scrolling document.
Only the active stage is rendered in the main workbench. A semantic progress
indicator names all four stages and marks the current one, but future stages
are not shortcuts around readiness or confirmation gates.

The Builder still supports one or many datasets. Dataset-specific readiness is
shown in the dataset rail during Upload and Configure, while Review and Build
operate on the project as a whole.

## Workflow state

The session owns one explicit workflow state with the following public stages:

- `upload`: empty state or one or more imports in progress;
- `configure`: at least one imported dataset is available for editing;
- `review`: a ready frozen plan is being inspected;
- `build`: that exact frozen plan has been confirmed.

Import, build, conflict, success, and failure states remain typed substates of
their owning stage. They do not create additional top-level navigation items.

Transitions are reducer-driven rather than inferred from which controls happen
to be present:

```text
empty
  -> importing
  -> configuring
  -> reviewing
  -> confirmed / ready to build
  -> building
  -> complete or error
```

Removing the final dataset returns to Upload. Adding a dataset or changing a
configuration while Review or Build is active invalidates confirmation and
returns to Configure. A build result does not silently change the reviewed
revision.

## Stage contracts

### 1. Upload

Upload contains the empty state, local file picker, example gallery, dataset
import rail, import progress, cancellation, and import errors.

While importing, the main area shows only:

- the dataset name or filename;
- the current import phase and progress;
- a Cancel or Remove action;
- an actionable error if import fails.

No output mode, App option, output folder, review action, or build action is
rendered. The current global `actionbar` is absent rather than visually hidden.

When the first dataset finishes inspection, the workflow enters Configure.
Additional imports may continue in the rail, but Continue remains unavailable
until all imports reach a terminal successful or removed state.

### 2. Configure

Configure contains the editable Import & Inspect, Core, Viewer content,
Enhance, and project-wide App settings surfaces. It does not render the final
Review summary below them.

The dataset rail uses operational labels only:

- `Loading`
- `Needs attention`
- `Ready`

It no longer shows `Reviewing` or `Reviewed`, because confirmation is not a
per-dataset property.

A contained stage footer summarizes readiness and provides one primary
`Continue` action. This footer belongs to Configure, not to the entire
application, and contains no Build controls. Continue is enabled only when:

- all imports are complete;
- every dataset has no blocking issue;
- project-wide App settings are valid;
- a ready frozen BuildPlan can be produced.

When Continue is disabled, adjacent text names the concrete next action. It
does not use a generic count such as “Resolve 1 required setting” without a
link or location.

### 3. Review

Review is a separate, project-wide, read-only page backed exclusively by one
frozen BuildPlan. It does not read mutable draft inputs directly and does not
generate or embed a Viewer preview.

The primary information hierarchy is:

1. readiness and frozen plan revision;
2. project summary: dataset count, included Viewer pages, and artifact mode;
3. one compact card per dataset with cells, genes, defaults, and included
   content;
4. Viewer experience: initial page and dataset, uploads, welcome message,
   login summary, and point appearance policy;
5. artifacts that will be created;
6. optional, collapsed technical details.

Review provides contextual `Edit dataset settings` and `Edit App settings`
links. They return to the appropriate Configure location. Navigation alone
does not mutate the plan, but any accepted edit invalidates the prior frozen
plan and confirmation.

The page ends with one contained confirmation region:

- secondary action: `Back to settings`;
- primary action: `Looks good — continue to build`.

There is no per-dataset `Looks good`, no `Review datasets` action, and no
full-width global action bar.

### 4. Build

Build is created only after `Looks good — continue to build` confirms the
currently displayed frozen plan. It is the only stage that renders output
folder and Build controls.

The initial Build page shows:

- the confirmed plan revision;
- a read-only summary of the reviewed artifacts;
- the selected output folder, or an action to choose one;
- one primary `Build Viewer` action;
- a route back to Review.

Artifact mode and Viewer settings are not editable here. Changing them requires
returning to Configure and completing Review again.

One stable in-page status host replaces the initial controls as the protocol
moves through preflight, conflict resolution, queued, building, success, or
failure. Reactive redraws must not move progress into a separate global strip.

During a build, the host presents meaningful phases such as preparing the
release, exporting each dataset, assembling the App, and verifying the final
artifact. On success it presents the verified outputs and only the actions
supported by that result, such as Open Viewer, Show in folder, and View report.

## Confirmation identity and invalidation

Confirmation certifies an exact frozen BuildPlan, not a loose count of reviewed
datasets. The confirmation record contains an opaque plan identity derived
from every build-relevant input, including:

- ordered dataset snapshot identities and accepted dataset revisions;
- dataset export and Viewer-content settings;
- artifact mode;
- project-wide App options;
- the safe authentication summary required by the plan.

Secrets are never included in UI text, logs, fingerprints, or serialized
worker descriptors.

Before entering Build and again before queueing work, the Builder compares the
confirmation identity with a freshly frozen plan. A mismatch clears the
confirmation and returns to Configure with a concise explanation. The existing
source snapshot, compare-and-swap, output conflict, and worker protocol checks
remain authoritative; staged UI is not a replacement for release safety.

The current per-entry `reviewed_revision` model is removed from user-facing
workflow semantics. Because it is session-only state, no persistent data
migration is required.

## Component boundaries

The redesign separates four responsibilities:

1. **Workflow reducer** owns the public stage, legal transitions, and
   confirmation invalidation.
2. **Readiness projection** translates imports, dataset state, App validation,
   and BuildPlan readiness into actionable Configure guidance.
3. **Review projection** converts one frozen BuildPlan into bounded,
   user-facing, read-only content.
4. **Build protocol projection** maps the existing typed build protocol and
   result models into the stable Build-stage status host.

UI components consume these projections and emit intent events. They do not
infer readiness by querying the DOM, and JavaScript does not manufacture a
stage that the server has not authorized.

The existing build worker, release coordinator, output picker, publishing
transaction, authentication boundary, and generated-App contracts remain in
place. This change reorganizes when their controls are reachable and how their
state is presented; it does not rewrite the export pipeline.

## Error handling

- Import errors stay in Upload or Configure and identify the affected dataset.
- Configure blockers link or focus the exact setting that needs attention.
- A plan that becomes stale while Review is open returns to Configure instead
  of showing Build.
- Output conflicts are resolved before replacement begins, with Cancel,
  Replace existing files, and Choose another folder actions.
- Worker or build failures remain in the Build status host, preserve the
  confirmed revision and output location when safe, and expose Retry only when
  the typed result says retry is valid.
- A failure caused by changed configuration clears confirmation and routes back
  through Review rather than retrying an obsolete plan.

## Accessibility, focus, and responsive behavior

The progress indicator is an ordered list with one `aria-current="step"`
item. It communicates state but does not expose disabled future stages as
misleading navigation.

On an accepted transition, focus moves to the new stage heading after the
server-rendered stage is ready. Import, validation, build, and result changes
continue to use polite live regions. No handler scrolls to a hidden Build
button, and reduced-motion mode uses immediate focus and scroll behavior.

On narrow screens, the dataset rail continues to use the existing manager
pattern. The progress indicator compacts to the current stage and ordinal. The
Review confirmation and Build actions stack to full-width controls without a
fixed viewport footer.

## Motion

Stage transitions use the existing state-first motion contract: bounded
opacity or surface transitions only, no layout choreography, and zero-duration
behavior under `prefers-reduced-motion`. Build progress remains in one stable
DOM host so motion never masks server state or resets focus.

## Test strategy

### Workflow and model tests

- legal and illegal stage transitions;
- confirmation creation from a ready frozen plan;
- invalidation after every build-relevant dataset and App change;
- no invalidation for navigation without an accepted edit;
- project-wide readiness across one and many datasets;
- stale confirmation rejection before build queueing.

### UI contract tests

- Upload and loading markup contains no action bar, Review action, App checkbox,
  output folder, or Build action;
- Configure contains one Continue action and actionable blocker guidance;
- dataset rail uses Loading, Needs attention, and Ready only;
- Review is read-only, bounded, and contains exactly one Looks good action;
- Build controls render only for a matching confirmed plan;
- no hidden disabled Build input is retained as an event-wiring workaround;
- progress semantics, focus targets, and reduced-motion CSS remain valid.

### Browser tests

- empty -> import -> Configure without any early Build UI;
- multiple imports, including pending, failed, cancelled, and retried datasets;
- Configure blocker focus and recovery;
- Continue -> Review with a global summary for one and many datasets;
- Back to settings without edits, and confirmation invalidation after edits;
- Looks good -> Build, folder selection, conflict handling, and queueing;
- stable progress host through building, success, recoverable failure, and
  non-retryable failure;
- keyboard focus and responsive layouts at desktop, tablet, and mobile widths.

Existing BuildPlan, worker, publish, authentication, bundle, and generated-App
tests continue to guard the underlying release behavior.

## Scope boundaries

This design does not add an interactive Viewer preview, change the exported CRB
schema, redesign dataset analysis settings, add autosave across Builder
sessions, change authentication cryptography, or replace the existing worker
and publishing transaction. It also does not introduce user-configurable stage
skipping.

## Acceptance criteria

The redesign is complete when:

- data loading never renders build-related UI;
- Review is a distinct, project-wide, read-only stage;
- one global Looks good action confirms the exact frozen project revision;
- Build UI is absent before confirmation and present only in the Build stage;
- any accepted build-relevant edit invalidates confirmation;
- progress and results stay in one stable Build-stage host;
- focused unit, UI-contract, and real-browser regressions pass without changing
  the existing export and release safety guarantees.
