# Builder Viewer Content Design

## Goal

Give each Builder dataset one compact, explicit configuration surface for the
content that the generated Viewer exposes and the content it opens first. The
Builder must preserve ordinary safe metadata, while separately letting users
promote eligible metadata columns to Viewer groups.

## Core experience

The Core stage contains a **Viewer content** section with the subtitle:

> Choose what the generated app includes and how it opens.

Three compact cards progressively disclose their details:

- **Groups** lists every metadata column. Eligible categorical columns can be
  included; ineligible columns remain visible with a short reason. Exactly one
  included group is marked **Opens first**. Selecting a group exposes a bounded
  five-row preview and its color editor.
- **Projections** shows every exportable two-dimensional reduction, including
  PCA. Each option has a deterministic bounded scatter preview, an inclusion
  control, and a true **Opens first** choice. Preview colors follow the selected
  default group and its current palette.
- **Trajectories** appears only when trajectory content exists. Viewer-supported
  monocle2 trajectories can be included and one can open first. Unsupported
  methods remain secondary, explanatory information and cannot be selected.

The dataset-level point-size control lives with projection previews and updates
them immediately. Search, card disclosure, preview selection, and show-more
controls are transient UI state. Only content, default, color, and point-size
changes invalidate Reviewed state.

## Canonical dataset settings

Each dataset stores:

```text
included_groups
default_group
group_color_overrides
included_projections
default_projection
overview_point_size
included_trajectories
default_trajectory
```

Groups and projections use stable names. Trajectories use a method-keyed list
of stable trajectory names; the default is a two-field `method`/`name` record.
Normalization upgrades older settings without losing existing selections and
guarantees that every default belongs to its corresponding included set.

Color overrides are isolated by dataset, group, and level. Missing values use
a stable fallback label and color. The existing twelve-level collapsed editor,
search, show-all/show-fewer, and reset behavior are retained.

## Data boundaries

- Profile construction may inspect metadata and reductions once, but stores
  only bounded summaries: types, counts, missing percentage, level counts, and
  the first five display rows.
- Projection and trajectory previews are fetched from the existing isolated
  worker and are deterministically capped. They never read expression values
  or compute a new reduction.
- No full cell identifier vector or unbounded metadata column reaches the UI.
- Inclusion as a Viewer group is independent from preservation as ordinary
  metadata.

## Review and build

Review shows compact sentences for included groups, opening group, projections,
opening projection, initial point size, and trajectories. It does not reproduce
catalogs, previews, cell identifiers, or internal BuildPlan diagnostics.

Build exports only included groups, reductions, and supported trajectories;
passes the opening group through the CRB contract; and freezes opening
projection, opening trajectory, and overview point size into generated-App
configuration. Existing 5.0 versionless Viewer resolution, private-App output,
accessibility contracts, and ordinary safe metadata preservation remain intact.

## Accessibility and visual language

Controls use native checkboxes/radios with explicit labels and keyboard focus.
Cards use the existing amber visual language, readable selected states, bounded
scroll regions, and live-status text where asynchronous previews need it. No
external icon, font, frontend framework, or network resource is added.

