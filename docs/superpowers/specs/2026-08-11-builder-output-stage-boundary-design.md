# Builder Output-Stage Boundary Design

## Goal

Align the Builder workflow with the user's real decisions:

1. import source data;
2. decide what the CRB extracts and retains;
3. review that CRB plan;
4. decide which deliverables to build and configure the optional Viewer App;
5. choose a destination and execute the build.

The data-configuration stage must not discuss Viewer App creation or App
runtime settings. Those controls belong to Build because they configure an
optional output artifact, not the contents extracted from Seurat.

## Public workflow

The four public stages are:

```text
Upload -> Data setup -> Review -> Build
```

Only the active stage is rendered in the main workspace.

### Upload

Upload owns file and example selection, import progress, cancellation, retained
import errors, and the imported-dataset rail. It contains no CRB settings,
output-mode controls, App capability guidance, or build controls.

### Data setup

Data setup replaces the ambiguous Configure label. It owns only decisions that
change the data extracted from Seurat or retained in the CRB, including assay,
layer, metadata, Groups, projections, trajectories, expression storage,
analysis content, supplementary material, and spatial alignment.

Its footer reports CRB-plan readiness and exposes one Continue action. It does
not render `Create a Viewer app`, App capability errors, welcome text, runtime
network settings, upload policy, launch behavior, or authentication controls.

### Review

Review is a read-only projection of one frozen CRB plan. It confirms what each
CRB will contain, the dataset order relevant to the CRB release, warnings, and
the CRB artifacts. It does not claim that a Viewer App will be created and does
not show App settings.

Confirmation certifies the exact CRB plan identity. Merely navigating between
stages does not invalidate it. An accepted source-data or Data setup change
does invalidate it and makes Build unavailable until the updated CRB plan is
reviewed and confirmed.

### Build

Build owns deliverable selection, optional Viewer App configuration, output
destination, conflict handling, execution, progress, and results.

The page first presents two mutually exclusive output modes:

- `CRB files only`, selected by default;
- `CRB files + Viewer App`.

Selecting the Viewer App mode immediately renders its settings expanded in the
page. They are not hidden behind an additional disclosure or nested wizard.
The settings include:

- welcome message;
- host;
- port;
- open App after build;
- visitor-upload policy;
- starting page and starting dataset;
- login requirement;
- account setup and editing.

If Viewer App dependencies are unavailable, only the Viewer App output choice
is disabled. The reason appears beside that choice. CRB-only output remains
available, and no App capability warning appears in Data setup.

The selected output mode and App settings are frozen at Build time and become
part of the final build-request identity. Changing them does not require the
CRB Review to be repeated because they do not change CRB extraction. The
existing request validation, secret handling, release transaction, and final
artifact verification remain authoritative.

After output configuration, Build presents the destination, conflict policy,
and primary Build action. Once execution starts, output controls are locked and
the same stable DOM host presents progress, conflicts, completion, or failure.

## Stage navigation

The bottom stage strip is navigation, not a passive progress ornament. Each
stage is clickable once its entry contract is satisfied:

- Upload is always available;
- Data setup is available after at least one dataset loads successfully;
- Review is available after a valid reviewable CRB plan exists;
- Build is available after the current CRB plan is confirmed.

Future stages that have not met their entry contract are present but disabled.
During active build execution, navigation away from Build is disabled so the
user cannot mutate source or configuration state behind an active release.

Navigation alone preserves downstream state. Only accepted mutations invalidate
dependent state. Returning to Review from Build therefore preserves Build
settings, while changing Data setup invalidates the prior Review confirmation.

## Stage-navigation visual language

The chosen treatment is a restrained typographic baseline navigation:

- no checkmarks, step numbers, status labels, pills, or decorative icons;
- labels use the same type scale as surrounding secondary interface text, so
  the strip does not become visually louder than the active page;
- completed stages use neutral dark text;
- the current stage uses the existing semantic action orange, modestly heavier
  type, and a short bottom rule;
- unavailable future stages use clearly lighter neutral text and no hover or
  pointer affordance;
- a quiet baseline visually relates the stages without connector graphics.

State must not rely on color alone. Native disabled semantics, `aria-current`,
focus behavior, and the absence or presence of interactive affordances provide
the non-visual distinction. The current stage remains keyboard-focusable only
when doing so serves navigation; disabled future stages are not focus targets.

At narrow widths, labels may reduce spacing or wrap into a compact two-row
layout, but they retain the same restrained type size and do not collapse into
icons or numbered shorthand.

## State and identity boundaries

The workflow owns two distinct identities:

1. the confirmed CRB-plan identity, derived only from source snapshots and
   CRB-relevant data settings;
2. the final build-request identity, derived from the confirmed CRB plan plus
   output mode, App settings, destination policy, and safe authentication
   summary.

Authentication secrets remain outside both UI text and serializable worker
descriptors. Existing encrypted material and passphrase boundaries remain
unchanged.

This separation prevents an App-only edit from forcing a redundant CRB review
while still making the queued build an exact, immutable request.

## Error handling

- Import failures remain in Upload and name the affected dataset.
- CRB readiness failures remain in Data setup and identify the responsible
  field or dataset.
- A stale CRB confirmation routes to Review or Data setup instead of permitting
  Build.
- Missing Viewer App dependencies disable only the Viewer App output choice and
  show the actionable reason in Build.
- Invalid App fields block the Build action and show inline errors beside the
  relevant fields.
- Invalid or incomplete login accounts block Viewer App execution without
  exposing credentials in summaries or logs.
- Output conflicts remain in the stable Build host and retain the selected
  output mode and App settings.
- Build failures preserve safe retry context and never unlock mutation routes
  while the worker is still active.

## Verification

Automated coverage will verify:

- the public label is `Data setup`, with no remaining user-facing Configure
  stage label;
- Data setup never renders App creation, capability, welcome, port, or login
  controls;
- Review contains only the frozen CRB plan and no App artifact claims;
- Build defaults to CRB-only and conditionally renders expanded App settings;
- welcome, host, port, launch, uploads, starting content, and login accounts
  reach the final generated-App request unchanged;
- App-only edits preserve CRB confirmation but change build-request identity;
- CRB-relevant edits invalidate Review confirmation and disable Build;
- every permitted bottom-navigation transition and every gated transition;
- navigation without mutation preserves downstream state;
- active execution locks stage navigation;
- the navigation contains no checkmark, number, status-label, pill, or icon
  treatment and uses semantic current and disabled states;
- desktop, tablet, and phone widths preserve restrained type size, focus
  visibility, and non-overlap;
- generated CRB-only and authenticated Viewer App end-to-end paths remain
  valid.

## Non-goals

- Redesigning the generated Viewer UI.
- Changing CRB schema or Seurat extraction behavior.
- Replacing the existing build worker, release transaction, authentication
  encryption, or output verification pipeline.
- Adding deployment, public hosting, or remote-account provisioning.
