# Enhance Analysis Card Interaction

## Goal

Make optional analyses feel like clear, selectable choices without exposing implementation-heavy detail in the main flow.

## Card interaction

- The whole analysis card is the selection target.
- The native checkbox remains in the DOM for Shiny state, keyboard input, and screen-reader semantics, but is visually hidden.
- The analysis title is slightly larger than it is today and remains the strongest text on the card.
- Default state uses the white surface and neutral border.
- Hover and keyboard-focus state use a pale amber surface, amber border, and restrained shadow. The transition is brief and calm.
- Selected state uses the full amber surface, a slightly darker amber border/shadow, white title text, and a pale warm description. Hovering a selected card does not make it look unselected.
- Blocked cards remain visibly unavailable and cannot be selected.

## Information action

- Every card has a small circular `i` button in its top-right corner.
- Activating the information button does not toggle the card selection.
- The button has an accessible label containing the analysis name.
- It opens one reusable accessible modal dialog with focus management, Escape-to-close, backdrop close, and focus restoration.
- The dialog presents the analysis title and short description, followed by compact labelled facts:
  - Viewer page
  - Typical cost or duration
  - Prerequisite
  - Network requirement
  - Replacement behaviour
  - What happens when skipped
- Facts use friendly labels and short values; the raw repeated prose from the former “What this changes” disclosure does not return to the cards.

## Data and behaviour

- Existing analysis identifiers, selection state, dependency rules, and build behaviour remain unchanged.
- Existing module metadata supplies the dialog content; no network dependency or external icon is introduced.
- A single modal is populated from the selected card rather than rendering one modal per card.

## Accessibility

- Cards are operable with pointer, Space, and Enter through their checkbox/label relationship.
- Focus-visible styling matches the pale amber hover state and remains distinguishable.
- Selected state is exposed through the underlying checked input and a card class derived from it.
- The information button is a separate keyboard stop.
- The modal uses `role="dialog"`, `aria-modal="true"`, a labelled title, focus trapping, Escape close, and focus restoration.
- Reduced-motion users receive state changes without animated movement.

## Testing

- UI tests verify the hidden checkbox, card label relationship, information button, and dialog content contract.
- Client tests verify that selection styling follows checkbox state and the information action does not toggle selection.
- CSS contract tests verify default, hover/focus, selected, blocked, and reduced-motion states.
- Focused Builder stage/UI/client tests and `git diff --check` are sufficient for this front-end iteration.

## Out of scope

- Changing analysis algorithms, dependencies, or build results.
- Adding external icon libraries.
- Redesigning attachment controls or Review in this iteration.
