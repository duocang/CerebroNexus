# Builder Ready and Complete Status Design

Date: 2026-08-16

## Goal

Make dataset readiness, row interaction, and successful Build completion visually
distinct without changing import scheduling, dataset selection, Build execution, or
generated App launch behavior.

## Scope

This change covers four presentation details:

1. Add a perceptible but restrained hover response to dataset rows.
2. Replace the ready-state green circle with a designed `Ready` stamp.
3. Render the successful Build pipeline with an unambiguous green completion mark.
4. Rename `Open App` to `Launch App` so the label describes its existing behavior.

It does not change the strict serial import queue, dataset lifecycle transitions,
BuildPlan contents, export behavior, or the background App launcher.

## Dataset row interaction

### Resting

A non-selected row uses the neutral surface and has no left marker. A selected row
uses the existing pale Logo-orange surface and the Logo-orange left marker.

### Hover

Hovering a non-selected row applies all of the following:

- pale Logo-orange background;
- a subtle warm border;
- one-pixel upward translation;
- a small, soft shadow.

Hover must not show the permanent left marker. That marker remains exclusive to
selection, so hover and selection cannot be confused. Hovering a selected row may
slightly strengthen its surface or shadow, but may not replace its selected colors.

Keyboard focus must receive an equally visible focus treatment through `:focus-visible`.
Reduced-motion preferences must suppress translation and animated transitions.

## Ready stamp

### Placement and content

The ready indicator sits at the right edge of the dataset pick target, replacing the
current green circle. The metadata row contains only cell count and source format;
it does not repeat `Ready`.

### Appearance

The approved treatment is a restrained double-line verification stamp:

- uppercase `READY` text;
- low-saturation success green;
- compact rectangular form with slightly rounded corners;
- two-line border effect;
- approximately two degrees of counter-clockwise rotation;
- faint green wash, not a solid green fill;
- monospaced, bold, lightly tracked lettering.

The stamp is decorative in form but remains horizontally readable. It must not become
a circle, icon-only status, or oversized badge. Its accessible name remains `Ready`.

### State separation

Lifecycle and interaction states are independent:

- loading keeps the existing progress treatment and spinner;
- ready uses the green stamp;
- error keeps the red error treatment;
- selected uses the Logo-orange surface and left marker;
- hover uses only transient surface, border, lift, and shadow feedback.

A selected ready row therefore contains one orange selection marker and one green
stamp. It must not contain a green left border or a separate green dot.

## Build completion pipeline

When Build succeeds:

- `Queued` and `Building` use small filled green dots to show passed stages;
- `Complete` uses a slightly larger green circular completion mark with a white check;
- the `Complete` label is green and emphasized;
- `Failed` stays neutral gray because that branch was not entered;
- no Logo-orange current-state ring remains after success.

Queued and active Build states retain their existing progress semantics. Failure
continues to use the error color and must not be represented as green completion.

The completion mark is paired with the visible word `Complete`; color and shape are
not the only carriers of meaning.

## Launch App wording

Rename the successful-result action from `Open App` to `Launch App`.

The implementation behavior is unchanged: Builder validates the published App,
starts it in a separate background R process using `shiny::runApp()`, and asks that
process to open the App in a browser. The action remains available only when a
generated App exists and passed verification.

## Component boundaries

- Dataset rail UI owns the stamp markup and accessible label.
- Builder component/layout CSS owns hover, selection, ready stamp, focus, and
  reduced-motion presentation.
- Build status UI owns the `Launch App` label and completion-step markup.
- Build feature CSS owns successful pipeline styling.
- Existing server handlers and launch functions remain unchanged.

## Verification

Focused contract coverage must establish:

- ready rows render `READY` and no ready-state dot;
- ready metadata does not repeat the word `Ready`;
- selected ready rows retain only the Logo-orange left marker;
- hover and focus selectors exist without changing lifecycle attributes;
- successful pipelines render passed stages and a green checked `Complete` stage;
- the result action reads `Launch App` while retaining the existing `open_app` input ID;
- result states without a verified generated App do not show `Launch App`.

Browser verification should inspect one non-selected hover row, one selected ready row,
and one successful Build result at desktop width. It should also confirm that no
interaction changes the serial import queue or launches an App before the user presses
`Launch App`.

