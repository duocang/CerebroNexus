# Builder destructive action styling

## Goal

Make identical Cancel and Remove actions look and align consistently across dataset cards, regardless of card state or background color.

## Cancel placement

- Place Cancel at the bottom-right of every active or queued import card.
- Use the same alignment and spacing for preparing and waiting states.
- Keep the current cancel behavior, accessibility label, and state handling unchanged.

## Remove appearance

- Render every dataset Remove action with a solid white background, a neutral gray one-pixel border, red text, and the existing corner radius.
- Do not allow selected, active, or tinted card backgrounds to show through the button.
- Apply the same appearance in normal, selected, hover, focus, and disabled contexts, with state-specific feedback layered on top of the shared base style.

## Action layout

- Keep Move up and Move down behavior and appearance unchanged.
- Keep each card's action group right-aligned.
- Preserve the existing action order and responsive wrapping behavior.

## Scope

This change is CSS/layout only. It does not alter import cancellation, dataset removal, confirmation dialogs, queue behavior, dataset selection, or card state colors.

## Acceptance criteria

- Preparing and queued import cards show Cancel in the same bottom-right position.
- Remove looks identical on white and tinted dataset cards.
- Button labels remain fully visible at supported widths.
- Existing click, confirmation, keyboard focus, and disabled behavior remain intact.
