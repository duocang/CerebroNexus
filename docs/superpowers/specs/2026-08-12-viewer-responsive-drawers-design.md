# Viewer Responsive Drawers Design

## Scope

This change is limited to the Viewer on `feat/coordinated-views-nexus`. It
improves the mobile application navigation and replaces the draggable Linked
views More settings window with a responsive drawer. It does not modify the
Builder, integrate branches, extract a shared design system, or change any
Viewer data, filtering, image-alignment, selection, or plotting protocol.

## Goals

- Make every Viewer page reachable and recoverable at a 390 px viewport.
- Give the mobile navigation an explicit scrim, close action, and predictable
  dismissal behavior.
- Preserve every More settings control on mobile without crowding the linked
  plots.
- Keep the three-panel desktop workspace geometrically stable while settings
  are open.
- Preserve control values, plot state, selection state, active spatial space,
  and the Viewer scroll position across drawer visits.
- Keep motion short and honor `prefers-reduced-motion`.

## Responsive behavior

### More settings

Above 900 px, More settings is a fixed drawer attached to the right edge of the
Viewer viewport. It overlays the right portion of the workspace rather than
changing the Linked views grid tracks, so opening it does not resize or redraw
the linked panels. It has an always-visible title and close button and is not
draggable.

At or below 900 px, the same DOM becomes a full-viewport settings surface. Its
header stays visible, its body scrolls vertically, and the existing Background
image and Points groups remain available. The surface is a presentation change
only: the controls retain their current IDs and event behavior.

The More settings trigger remains in the Linked views control bar. Opening the
drawer preserves the current Viewer scroll position. Closing by the title-bar
button or Escape returns focus to the trigger.

### Mobile application navigation

At or below the existing 767 px AdminLTE breakpoint, the sidebar becomes a
modal side drawer with a scrim over the Viewer content. The hamburger control
uses a visible menu glyph and an accessible label. The drawer closes when the
user:

- chooses a navigation destination;
- activates its explicit close control;
- activates the scrim; or
- presses Escape.

Closing returns focus to the hamburger unless the user selected a navigation
destination, in which case focus follows the destination change. The desktop
sidebar remains unchanged.

### Overlay coordination

The mobile navigation and More settings are mutually exclusive. Opening either
surface requests that the other close first. They communicate through small
DOM events rather than calling each other's private functions. This keeps the
Viewer shell independent from Linked views internals.

## Implementation boundaries

### Linked views UI

`inst/viewer/coordinated_views/UI.R` retains the existing settings controls and
IDs. The More settings container gains drawer/dialog semantics. The draggable
grip, drag hint, and drag-specific attributes are removed; the close button is
always present.

### Linked views behavior

`inst/viewer/www/coordviews.js` keeps `setMoreOpen()` as the only state-changing
entry point. It synchronizes open classes and accessibility attributes, manages
focus, responds to Escape and shell overlay events, and dispatches an event
that asks the mobile navigation to close before settings open.

The former floating-position, pointer-drag, viewport-clamping, and bring-to-
front state is deleted. Opening settings continues not to call `resizeAll()` or
alter plot state. Shiny may replace server-rendered controls inside the drawer
without replacing or closing the drawer shell.

### Viewer shell behavior

The Viewer shell adds a real navigation scrim and a small, dedicated mobile
navigation controller. The controller adapts the existing AdminLTE push-menu
state; it does not implement a second navigation model. It owns:

- hamburger and close-button accessibility state;
- scrim visibility;
- navigation-link, scrim, close-button, and Escape dismissal;
- focus restoration; and
- the overlay-coordination event shared with Linked views.

Missing shell or Linked views elements are treated as a no-op so initial Shiny
mounting and capability-dependent pages do not raise browser errors.

### Styles

`inst/viewer/www/custom.css` owns the mobile application drawer and scrim.
`inst/viewer/www/coordviews.css` owns the More settings drawer. Both use the
existing Viewer neutral, border, shadow, amber, easing, and duration tokens.

Drawer motion is limited to 160-220 ms. Under `prefers-reduced-motion: reduce`,
position and opacity changes are immediate. Neither surface may create document
horizontal overflow at 390 px.

## Accessibility

- Triggers expose `aria-expanded` and `aria-controls`.
- Closed surfaces expose `aria-hidden="true"`; open surfaces expose the matching
  state.
- The mobile navigation and full-screen settings surface use dialog semantics.
- Each surface has an explicit accessible close button.
- Escape closes only the top-level open surface.
- Focus enters the close button on open and returns to the originating trigger
  on dismissal.
- The scrim prevents pointer access to covered content.
- Existing settings labels, IDs, and keyboard-operable controls remain intact.

## Failure and resize behavior

- If a controlled surface is absent, open and close requests return without an
  exception.
- Resizing across 900 px while More settings is open changes presentation but
  preserves open state and control values.
- Resizing across 767 px closes stale mobile navigation state when entering the
  desktop layout.
- A Shiny redraw inside More settings does not close the drawer or move focus.
- Repeated open and close requests are idempotent.

## Test strategy

Browser regression tests exercise real Viewer DOM and behavior:

- At 1619 px, More settings is a right fixed drawer, has no drag affordance,
  and leaves linked-panel geometry unchanged.
- At 768 px, the settings surface stays inside the viewport, scrolls internally,
  contains all existing controls, and creates no horizontal overflow.
- At 390 px, the menu glyph is visible; opening navigation shows a scrim; link,
  close, scrim, and Escape dismissal work.
- At 390 px, More settings fills the viewport, retains all existing controls,
  and closes via its button and Escape.
- Navigation and More settings cannot remain open simultaneously.
- Focus moves into and returns from both controlled surfaces.
- Reduced-motion CSS removes drawer transitions.

Existing Linked views tests continue to cover the controls' data behavior.
Final live-browser verification uses the generated Omnibus Viewer at desktop,
tablet, and phone widths.

## Out of scope

- Builder visual changes.
- Branch integration.
- Shared Builder/Viewer design-system extraction.
- Desktop sidebar redesign or icon-color cleanup.
- Changes to the contents or calculation of Linked views controls.
- New mobile-only feature reductions.
