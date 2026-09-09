# C2 Million-Cell Viewer Validation Design

## Decision

Extend the publication-full study with one end-to-end Viewer validation for
each C2 source/backend pair. The four validations use the first build repeat
for the 1.3-million-cell mouse source and the 1.49-million-cell human source,
with both `bpcells` and `h5`.

This is a functional and diagnostic gate, not a replicated browser-performance
comparison. The existing A/B, C1, and C2 processes remain the quantitative
backend experiment.

## Scope

Each validation must exercise the production path:

1. build a standalone App with `createShinyApp()` from the C2 CRB and sidecar;
2. launch that App through `shinytest2::AppDriver`;
3. wait for the Overview Canvas to become usable;
4. hover a rendered cell and observe a visible tooltip;
5. create a box selection and observe a non-empty active cohort;
6. zoom to the selection and observe the active zoom state;
7. open Gene expression, request the frozen query plan's first gene, and
   observe a rendered expression Canvas with non-empty expression values;
8. inspect App/browser logs for errors and stop the session cleanly.

Vitessce, visual-regression screenshots, concurrent users, network shaping,
and cross-machine browser comparisons are outside this change.

## Run Grid

| source | cells | backend | build repeat |
|---|---:|---|---:|
| mouse brain E18 | 1,306,127 | bpcells | 1 |
| mouse brain E18 | 1,306,127 | h5 | 1 |
| PsychAD HBCC | 1,492,734 | bpcells | 1 |
| PsychAD HBCC | 1,492,734 | h5 | 1 |

The Viewer validation runs immediately after the selected C2 backend's access
processes and before its scratch artifact is removed. The generated App stays
under that build's scratch directory and is deleted with the build artifact.

## Measurements

`21_viewer.csv` contains one row per source/backend pair with identifiers,
status, and elapsed seconds for:

- App bundle construction;
- App launch through initial Overview Canvas readiness;
- hover completion;
- selection completion;
- zoom completion;
- gene-expression completion.

Elapsed values are raw diagnostics. Publication text must not describe four
single runs as inferential or replicated performance evidence, and no fixed
latency threshold is used. A step either reaches its observable success state
within the bounded timeout or fails.

## Interaction Contracts

The driver reuses selectors and behavior already covered by the installed-App
browser suite:

- Overview readiness: a non-mini Canvas exists under
  `#overview_projection_cell_view_host` and has non-zero dimensions.
- Hover: deterministic mouse moves scan bounded Canvas positions until the
  panel tooltip becomes visible and contains cell text.
- Selection: click the existing box tool, drag inside the Canvas, and wait for
  `#overview_projection_selection_active` to become visible with at least one
  selected cell.
- Zoom: click `#overview_projection_zoom_to_selection` and wait for its active
  state plus a visible minimap.
- Gene expression: activate the existing page, select the first gene in the
  frozen query plan, and wait for its non-mini Canvas and non-empty exported
  expression vector.

The benchmark does not add test-only hooks to production Viewer JavaScript.

## Failure and Publication Rules

The driver records `FAILED(<stage>): <message>` on an App build, launch,
readiness, interaction, log, or shutdown failure. An unexpected browser or App
error also fails the row. The publication gate requires exactly the four
scheduled rows and `status = OK` for all of them.

As with existing benchmark processes, failure preserves the previous immutable
publication result. `BENCH_KEEP=1` retains scratch output for diagnosis.

The combined study copies the raw C2 `21_viewer.csv`, exposes a study-level
`viewer_metrics.csv`, reports the four functional outcomes and raw timings in
`summary.md`, and documents that browser evidence comes from one recorded host
and browser version.

## Implementation Boundaries

- Add one benchmark driver script and the smallest shared helper needed for
  its browser actions.
- Invoke it only for C2 build repeat 1.
- Extend the existing measurement and combined-report validators instead of
  creating a separate publication workflow.
- Reuse the already declared `shinytest2`/Chromote stack; add no dependency.
- Keep A/B and C1 unchanged.

## Verification

Development verification has two layers:

1. deterministic testthat contracts cover scheduling, row validation,
   aggregation, and failure behavior without downloading large sources;
2. a small generated App exercises the same browser helper locally when Chrome
   and `shinytest2` are available.

The final scientific evidence still requires one complete
`run_publication_full.sh` acquisition on the benchmark host. Only that run can
prove all four million-cell Viewer rows succeed.
