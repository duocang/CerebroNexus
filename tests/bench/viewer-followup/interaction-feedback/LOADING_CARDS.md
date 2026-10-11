# Visible, consistent loading cards

Date: 2026-10-10. Baseline: `ceed042d`.
Existing branch and worktree; no push.

The previous pass unified glyphs but made page loading too inconspicuous. The
dataset card provides the visual reference for this revision. This is CSS-only:
no new modal, focus trap, timing threshold, readiness signal or request handling.

## Presentation

- Dataset replacement retains its full-app loading surface and real progress.
  Its card shares the same surface, border, radius and shadow as page entry.
- First page entry uses a horizontally centered 400 x 104 px card, 28 px ring,
  16 px title and 24 px padding. It sits in the upper part of the content area,
  leaving navigation available. It has no backdrop and does not intercept input.
- During page entry, repeated pending text in page feedback and empty result/
  control slots is visually suppressed. Slot dimensions remain reserved. Actual
  results, explicit errors and native measured-progress notifications remain
  visible. The existing page-card lifecycle still controls this presentation.
- Local updates use the same card vocabulary at a smaller scale: a 20 px ring,
  14 px message, white surface and rounded border. Gene/page feedback reserves
  44 px both while pending and settled to avoid vertical movement.
- Immune result loading uses a centered 320 px card within its result region;
  native Shiny progress and Share working status use matching surfaces. Real
  progress bars and all existing hide/close conditions remain unchanged.
- Widths are bounded by their containers. Reduced-motion support is retained.

## Validation

Real Windows Brave, WSL R 4.6.1; existing production `server.R` harness at
127.0.0.1:8944, Demo dataset, 1369 x 915 viewport, existing reduced-motion
preference. No state injection or artificial delay to keep loaders visible.

- Gene first entry: visible card measured 400 x 104 px; pending duplicate row
  was hidden. The screenshot shows the actual card. It disappeared on completion.
- Gene CD74 update: local card measured 44 px high, 14 px text and 20 px glyph;
  global page card remained hidden. Settled state retained 44 px height, displayed
  CD74 and left zero native progress notifications.
- Groups first entry and populated result: shared page indicator appeared and
  then hid; existing controls and composition chart remained usable.
- Immune Abundance: pending result card measured 320 x 71 px with a 20 px glyph;
  completed plot hid its spinner wrapper.
- Focused suites `viewer-review-feedback`, `viewer-accessibility-design` and
  `dataset-loading-progress` passed. `git diff --check` passed.

Raw DOM measurements and screenshots are in
`implementation-evidence/loading-cards/`. `gene-entry-painted.png` is the primary
visual evidence; screenshot captures of other transitions can show the preceding
or completed frame and are not timing evidence. The prior analysis/lifecycle
reproduction items in `LOADERS.md` remain outside this CSS sizing correction.
