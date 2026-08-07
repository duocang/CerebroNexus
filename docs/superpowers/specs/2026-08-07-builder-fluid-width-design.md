# Builder fluid-width layout design

## Goal

Make the desktop Builder use the available viewport width like the Viewer,
without stretching navigation controls or reducing form readability.

## Chosen layout

- Remove the Builder shell's global `82.5rem` width ceiling.
- Use the Viewer's single page gutter: `26px` on desktop.
- Keep the dataset rail at its established fixed width; the main Builder pane
  receives all remaining space.
- Align the bottom action bar to the same viewport gutters as the shell.
- Preserve intentional inner reading-width limits for forms and prose. Spatial,
  Trekker, preview, and review grids may expand into the newly available space.
- Preserve the existing tablet and mobile breakpoints, including the mobile
  dataset manager.

## Accessibility and behavior

This is a CSS-only layout change. It does not change focus order, keyboard
behavior, Builder state, generated App output, or Viewer contracts. Horizontal
overflow must not be introduced at desktop, tablet, or mobile widths.

## Verification

- A focused CSS contract must fail while the old `82.5rem` shell/action-bar
  ceilings remain, then pass after they are removed.
- Existing responsive Builder UI tests must remain green.
- A lightweight browser check at a wide desktop viewport must confirm that the
  shell nearly spans the viewport while the rail remains fixed and the page has
  no horizontal overflow.
- Run JavaScript syntax checking and `git diff --check`; do not run broad package
  gates.
