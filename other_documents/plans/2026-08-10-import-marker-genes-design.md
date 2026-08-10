# Import precomputed Marker genes in Builder

## Goal

Let a Builder user attach precomputed marker-gene results from one or more
`.xlsx`, `.csv`, or `.tsv` files. Imported results must be published alongside,
not instead of, results calculated by Builder. The generated Viewer must expose
each imported result through its existing **Choose a method** selector without
any Viewer changes.

## Scope

This feature adds one focused workflow from **Enhance > Marker genes**:

1. The user enters a method name and selects one already configured grouping
   variable.
2. The user selects one or more workbooks or delimited files.
3. Builder inventories each delimited file and each worksheet in a workbook.
4. Builder normalizes every accepted source into one table for the selected
   grouping variable and reports the resulting coverage before build.

The first release supports Marker genes only. It does not add a generic result
importer, alter Extra material uploads, import enriched pathways, or calculate
new differential expression results.

## Existing Viewer contract

The Viewer already reads Marker genes as:

```r
marker_genes[[method]][[group]]
```

`method` populates **Choose a method** and `group` populates **Choose a
table**. A table whose first column is named after a configured grouping
variable receives the Viewer's existing subgroup filter. The Builder importer
must therefore write a plain `data.frame` at the selected group key with that
group name as its first column.

## User workflow

### Start an import

Selecting the existing **Marker genes** card first opens a choice dialog. The
user either calculates the built-in result for every configured group or opens
the upload workflow for precomputed results. The card does not change state
until that choice is confirmed, so it does not flash between selected and
unselected states.

The action requests:

- **Method name** — required, trimmed, and unique within the dataset. Builder
  never derives scientific meaning from a filename. A collision with a
  calculated or previously imported method is an error; the user must choose a
  different name.
- **Groups** — one value from the dataset's configured grouping variables.
- **Files** — one or more `.xlsx`, `.csv`, or `.tsv` files.

### Map sources

After ingestion, Builder shows one mapping row per CSV/TSV file and per Excel
worksheet. Each row is classified as either a multi-cluster or single-cluster
source.

For a multi-cluster source, the user chooses the column holding the selected
Groups' cluster labels. Builder moves that column to the first position and
names it exactly after the selected group.

For a single-cluster source, Builder guesses the cluster label from the
worksheet name (for workbooks) or filename stem (for delimited files). It shows
the guess in an editable control. Once confirmed, Builder inserts a first
column named after the selected group with that value repeated for every row.

Every edited or inferred value is matched against the selected group's known
levels. Unknown values and an absent required column leave the row unresolved;
unresolved rows cannot be attached.

### Review and publish

The Enhance and Review stages show an import summary: method, group, accepted
sources, accepted cluster labels, unresolved sources, duplicate assignments,
and coverage of known group levels. Coverage may be partial. Missing cluster
labels produce a clear warning but do not block building the method.

A source that cannot be read, has no rows, has unsafe/unsupported table data,
has an unknown cluster, or duplicates a cluster already supplied by a separate
single-cluster source cannot be accepted. The method is attachable only when at
least one normalized table is valid and no source selected for attachment is
unresolved.

## Normalized data model

Draft state stores an import record rather than raw browser upload metadata:

```r
list(
  method = "Scanpy Wilcoxon",
  group = "cell_type",
  sources = list(...),       # bounded summaries and mapping decisions
  coverage = list(...),      # known, present, missing labels
  status = "ready"
)
```

The worker owns parsed source data until the plan is frozen. Profile/state
records contain bounded metadata only. Freeze validates the immutable mapping,
then build merges the normalized tables into the worker-owned object as:

```r
object@misc$marker_genes[[method]][[group]] <- normalized_table
```

This preserves existing source methods and computed results. The normalizer
uses only plain data frames and plain atomic columns, retaining the current
table safety limits. It rejects duplicate method names and duplicate final
group-column names before an object is changed.

## Inference and validation rules

- Filename and worksheet inference compares a normalized candidate to the
  selected group's known labels. Exact matches win; no fuzzy or silent partial
  match is accepted.
- A multi-cluster source must designate a cluster column. Its non-missing
  distinct values must all be known labels.
- Single-cluster and multi-cluster sources may coexist when their cluster-label
  sets do not overlap. Overlap is an error rather than a row-order-dependent
  merge.
- At least one non-empty normalized source is required for a ready import.
- Gene and statistic column names remain unconstrained in this release. The
  Viewer deliberately supports arbitrary marker-table schemas and applies its
  established dynamic table formatting.
- Partial coverage is warning-only. The report names every missing known label.

## UI and accessibility

The mapping list identifies its source file and worksheet, shape, proposed or
chosen mapping, status, and editable control. It must not rely on filename
colour alone: errors have text, controls have labels, and status changes use a
live region. The review summary repeats warnings in text so a user need not
return to Enhance to understand a partial import.

## Failure handling

Read and parse failures are reported against the individual file or worksheet;
they do not discard unrelated, valid sources. A method remains editable after
any failure. No mutation reaches the source object until build runs from a
frozen, valid plan. A build error retains the saved frozen plan and reports the
method/group context for retry or removal.

## Tests

Unit tests cover:

- method-name validation and collisions with computed/existing methods;
- CSV, TSV, and multi-sheet XLSX inventory;
- sheet/filename exact inference and user override;
- single-cluster column injection and multi-cluster column normalization;
- unknown labels, missing mapping, duplicate labels, empty tables, and partial
  coverage;
- bounded and safe table input rejection;
- frozen plan determinism and object merge preserving existing methods.

UI/server tests cover the mapping controls, accessible status text, summary
warnings, and coexistence with the built-in Marker genes checkbox. Generated
app tests verify that an imported method appears in the unchanged Viewer
method/table selectors and can filter the normalized group column.

## Acceptance criteria

1. A user can import several CSV/TSV files and/or worksheets under one manual
   method name and one selected grouping variable.
2. A user can confirm or override every inferred single-cluster mapping.
3. Builder normalizes accepted tables to the existing Viewer marker-gene
   contract without modifying Viewer source files.
4. Imported and calculated methods coexist and are independently selectable in
   the generated Viewer.
5. Invalid mappings block attachment with actionable per-source errors;
   incomplete coverage emits a non-blocking warning.
6. The frozen plan, build report, and generated CRB make the imported method's
   provenance and coverage auditable.
