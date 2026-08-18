# Builder Coordinate Reset Motion Design

## Goal

Make the Coordinate settings Reset action feel smooth by animating its sliders
back to their defaults without delaying or weakening the existing reset and
persistence behavior.

## Chosen interaction

Use a 320 ms `cubic-bezier(.22, 1, .36, 1)` transition. The motion applies to
the ionRangeSlider handle, selected track, and displayed value so the control
reads as one coherent movement rather than disconnected pieces.

The Reset action continues to update the canonical Shiny values immediately.
Animation is presentation-only: it must not interpolate application state,
emit intermediate input events, or defer persistence.

## Trigger and scope

The server sends a small custom message immediately before updating the three
Coordinate settings sliders:

- Coordinate rotation
- Point opacity
- Point size

The browser marks only those controls as resetting. A short-lived CSS class
enables the transition and is removed after completion. Repeated Reset clicks
restart the same animation cleanly and cannot leave a control permanently in an
animated state.

Ordinary slider movement and programmatic updates from dataset or section
switches retain their current behavior and do not animate.

## Accessibility

Under `prefers-reduced-motion: reduce`, the controls update immediately with no
transition. Reset semantics, values, and persistence are identical in either
motion preference.

## Verification

Add static browser-contract coverage for the custom message handler, scoped
class lifecycle, 320 ms easing, and reduced-motion override. Preserve the
existing server tests that verify Reset restores rotation, point opacity, point
size, and the canvas reset token.

## Out of scope

- Animating the spatial canvas itself
- Changing default slider values
- Animating non-Reset programmatic slider updates
- Delaying server state changes until the visual animation completes
