# Builder Start Scroll-to-Top Design

## Goal

When the user starts an accepted Build, move the entire page to its maximum
upward scroll position so the Build status area is immediately visible from the
top of the page.

## Trigger

The server triggers scrolling only after `start_confirmed_build()` has passed
its operation, lock, and output-directory guards and changed `build_flow` to
`preparing`. A rejected or incomplete Build request does not move the page.

## Browser behavior

The server sends a dedicated `builder_scroll_page_top` custom message. The
browser handles it with:

```js
window.scrollTo({
  top: 0,
  behavior: reducedMotion.matches ? "auto" : "smooth",
});
```

This scrolls the document itself to the absolute top rather than merely bringing
the Build stage heading into view. It does not change focus, stage selection, or
the Build status state machine.

## Accessibility

The handler respects `prefers-reduced-motion`. Reduced-motion users receive the
same final position without animation.

## Verification

Server coverage verifies that an accepted Build sends the message after the
flow enters `preparing`, while rejected requests do not. Client contract
coverage verifies the handler uses `window.scrollTo`, `top: 0`, and the existing
reduced-motion media query.

## Out of scope

- Scrolling when the user only enters the Build stage
- Scrolling on folder selection, retry dialogs, or rejected Build attempts
- Changing focus or the sticky workflow navigation
