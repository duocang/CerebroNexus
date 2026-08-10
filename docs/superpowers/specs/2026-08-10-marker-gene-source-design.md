# Marker gene source workflow

## Goal

Restore the Builder to its original Marker genes behavior, then add precomputed
Marker gene imports without coupling user clicks to a checkbox value.

The finished workflow starts from the existing Marker genes card in Enhance.
When the card is not enabled, activating it opens one choice dialog:

- **Calculate for all Groups** runs the existing Builder analysis.
- **Upload precomputed results** opens the import workflow.

The generated Viewer continues to use its existing method and table selectors.
No Viewer UI changes are required.

## Reset boundary

Before reimplementation, remove the complete imported-Marker-genes feature
introduced after commit `ddcaed3e`, including:

- import UI and click interception;
- import parsing, mapping, freeze, and build integration;
- worker-loading changes;
- `readxl` and `writexl` dependency additions made solely for this feature;
- feature-specific tests and obsolete design/implementation documents.

The reset must preserve unrelated commits and the original behavior where
selecting Marker genes calculates differential expression for every configured
group.

The reset is committed and verified before the new implementation begins.

## Interaction architecture

Marker genes uses a dedicated Shiny action card, not a checkbox.

- The card activation increments `enhance-analysis_marker_genes_action`.
- The card's selected appearance is rendered from persisted dataset settings.
- `aria-pressed` communicates the rendered state to assistive technology.
- The card does not inspect or mutate a browser `checked` value.
- Other optional analyses retain their existing checkbox behavior.

When Marker genes is disabled, activating the card opens the source choice
dialog. When it is enabled, activating the card disables the active Marker
genes source and removes its draft imported records. This preserves the familiar
toggle behavior without sharing an input value between intent and state.

The card retains a separate information button. The action card and information
button are siblings; buttons are never nested.

## Source choice dialog

The dialog title is **Add Marker genes** and contains two theme-colored action
cards:

1. **Calculate for all Groups** adds `marker_genes` to the existing analysis
   plan and closes the dialog.
2. **Upload precomputed results** replaces the choice dialog with the import
   dialog without enabling the Marker genes card yet.

Cancel closes the dialog and leaves the dataset unchanged.

Calculation and import are mutually exclusive for this first release. Choosing
calculation removes draft imports; saving an import removes `marker_genes` from
the calculation plan.

## Import workflow

The import dialog collects:

- a required, trimmed method name that is unique within the dataset;
- one currently included grouping variable;
- one or more `.csv`, `.tsv`, or `.xlsx` files.

CSV and TSV files each create one source row. Every XLSX worksheet creates one
source row. Reading happens only after files are selected, with bounded size and
row checks consistent with existing Builder upload policy.

Each source row shows filename, worksheet when applicable, table dimensions,
mapping mode, resolved cluster information, and a textual status.

### Multi-cluster source

If a sheet contains multiple clusters, the user selects the column containing
cluster labels. Builder validates every non-empty label against the known levels
of the selected grouping variable. The normalized output moves this column to
the first position and renames it to the grouping variable.

### Single-cluster source

If a sheet contains one cluster without a cluster column, Builder guesses the
cluster from the worksheet name or filename stem. The guess is displayed in an
editable select control populated from known group levels. The user must confirm
or change every source row before saving.

### Validation and coverage

Unknown levels, empty tables, unreadable sheets, missing selected columns, and
duplicate single-cluster assignments remain unresolved. The dialog shows these
errors in text and disables Save while any source is unresolved.

Partial coverage is allowed. Missing known clusters produce a warning but do
not prevent saving. At least one resolved source is required.

Saving creates one imported method record and enables the Marker genes card.
The method can be removed by activating the selected card.

## Data model

Editable state lives in `settings$marker_imports`. Each method record contains:

- stable import identifier;
- method name;
- selected grouping variable;
- normalized source tables;
- source metadata needed for editing and review;
- known-level coverage and warnings;
- explicit readiness status.

Browser upload paths are never rendered or frozen. Frozen BuildPlan items contain
only validated normalized data and safe source labels.

## Build and Viewer integration

The build step merges each ready imported method into:

```r
object@misc$marker_genes[[method]][[group]]
```

Imported method names must not overwrite existing calculated or imported
methods. The selected grouping variable is the first column of every normalized
table.

The background worker explicitly loads the marker-import module before
`build.R`. Worker startup tests verify the merge function is callable inside the
actual worker process, not only in the Builder main process.

## Error handling

- File and worksheet errors stay attached to their source row.
- Invalid rows never enter frozen plans.
- A failed import does not enable Marker genes or alter the calculation plan.
- A worker missing the import module fails its startup contract before Build can
  be queued.
- Build errors include dataset and imported method names without exposing local
  paths.

## Accessibility and visual behavior

All new colors use existing Builder theme tokens. The action card has visible
hover, focus, and selected states without movement. The dialog uses headings,
labelled controls, keyboard-operable native buttons, and an `aria-live` status
region. Status is never communicated by color alone.

Reduced-motion settings suppress nonessential transitions. The dialog collapses
to one column on narrow screens.

## Verification

The implementation requires red-green coverage at these boundaries:

- the reset restores the original calculated Marker genes path;
- clicking the visible Marker genes card opens the source dialog;
- cancel leaves the card disabled;
- calculation closes the dialog and keeps the card selected;
- a selected card can be disabled without a checkbox event;
- CSV, TSV, multi-sheet XLSX, single-cluster, and multi-cluster inputs normalize;
- inferred clusters require explicit confirmation and support modification;
- unresolved rows block Save and partial coverage only warns;
- frozen plans exclude unsafe upload metadata;
- imported methods merge without overwriting existing methods;
- the real worker process loads and calls import support;
- a Basic PBMC build with no import succeeds;
- UI contracts cover theme tokens, focus, responsive layout, and textual status.
