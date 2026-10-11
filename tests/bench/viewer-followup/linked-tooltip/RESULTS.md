# Linked pinned tooltip closeout

2026-10-10. Baseline `e253450d`, same `卧槽/pr6_clean_perf` checkout.
Scope: pinned tooltip correctness only. No performance experiments or other
analytical pages were exercised.

## Fix

`hoverHtml(i, true)` iterated registered groups, including descriptors whose
per-cell `values` are deliberately null. Its direct `gg.values[i]` access threw.
The Linked-only pinned-group loop now skips a missing group or a null/undefined
values column. Loaded groups keep their existing labels and values. The ordinary
hover path already uses `catOf()` to exclude deferred descriptors.

The production patch is one guard and two comment lines. It adds no requests,
cache, materialization, ready changes, selection changes or server changes.
The existing **Details** action still requests the exact single-cell metadata
row on demand; it is the route to fields omitted from the compact tooltip.

## Targeted regression

- `linked-pinned-deferred.js`: **12 assertions passed**. The identical test on
  baseline source fails at `gg.values[i]` with a null access, establishing that
  it reproduces the reported bug. It checks ordinary/pinned content, loaded
  typed-array values, unchanged deferred state, a later-loaded column, and a
  replacement dataset with different labels.
- Existing `linked-hover-toggle.js`: **passed**. Added its two missing test
  doubles (`scheduleSingleMetadata` and `singleView`) for dependencies already
  used by the current renderer. No associated production behavior was changed.
- `node --check inst/viewer/www/cell_views.js` and `git diff --check`: passed.

## Native Brave check

Windows Brave 154.1.96.59, ordinary headed browser, 1M Mouse fixture from the
existing viewer-followup environment; fresh WSL R process, 100% cells. The
small observation panel records existing ready events/errors; the production
renderer is served directly, with no timing or readiness wrappers. No full
benchmark or formal A/B was run.

Observed initial payloads: primary 2,604 B, supplement 74,888 B, clone 601,240 B;
the existing static geometry/metadata resources remain 8 MB / 1 MB. No new
large payload or eager metadata loading is introduced. Server telemetry reports
274 ms primary preparation and 2,253 ms supplement preparation, consistent with
the established path; this single functional run is not a performance claim.

Native interaction evidence:

- 1,000,000 cells and four Linked panels render normally.
- Ordinary hover after enabling **Show cell hover**: visible group `0` without
  pinned controls or errors (`ordinary-hover.json`).
- Actual click pins a tooltip showing `0`, `Clone Expansion: No receptor`,
  **Details** and **Close**, without the null error (`pinned.json`, `pinned.png`).
- **Details** returns barcode `CGCGGTACACGGACAA-78`, cluster `0`, sample
  `10x E18 mouse brain (1M subset)`, nUMI 6,464, nGene 2,476 and current dataset
  identity. This confirms available tooltip values against the exact row
  (`details.txt`).
- Real box drag selects **163,941 cells** in all four views. Mapped trajectory
  counts 163,680 / 163,941 and receptor-bearing count 16,346 are consistent.
  Selection table shows 163,941 entries with real barcode/metadata rows
  (`selection.txt`).
- Same-dataset revisit retains the current selection and four spaces
  (`revisit.txt`).
- Dataset switch and final console results are recorded in the accompanying
  `dataset-b.json`, `dataset-a-return.json`, `events.json` and `console.json`.
  **Passed:** Mouse generation 2 -> Demo generation 3 (1,476 cells, two spaces)
  -> Mouse generation 4 (1M cells, four spaces). Both switched datasets have
  zero selected cells, no visible old tooltip, and their current dataset identity.
  Final browser error/warning log and observed JS error/rejection list are empty;
  the R session printed no Shiny error during the regression.

## Disposition

**ACCEPTED.** The pinned deferred-group error is fixed and all requested short
Linked checks pass. No evident performance regression: unchanged transport and
preparation paths, no new fetches/cache/allocation proportional to dataset size,
and the guard runs only in a user-requested pinned summary. No quantitative
speedup or formal performance equivalence is claimed. This closes the remaining
Linked correctness item; no further Linked optimization is planned.

Only the prior global performance/configuration limitations remain; this fix
does not claim a faster or more stable frame cadence, nor resumable reconnect.
