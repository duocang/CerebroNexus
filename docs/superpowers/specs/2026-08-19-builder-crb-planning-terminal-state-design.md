# Builder CRB Planning Terminal-State Design

## Problem

The Project result dialog enters `Step 1 of 3 · Planning` before the browser
knows that the Shiny server accepted the request. The dialog then gives the CRB
channel exclusive ownership and removes every action. Several server-side
preparation failures return without a `builder_project_crb_progress` message,
and the browser has neither an acknowledgement deadline nor a disconnect
fallback. The visible result is an indefinitely blocked Planning dialog even
though no CRB build was queued.

## Chosen design

Treat CRB preparation as a request with an explicit acknowledgement and exactly
one visible terminal outcome.

- Give each browser request an opaque string request id and keep it for the
  lifetime of that CRB operation. Progress messages echo the id so a late response cannot
  take over a later retry.
- Start a short acknowledgement timer when the browser sends the request. The
  server immediately acknowledges it with `status = "planning"`, before any
  project save, checkpoint checks, or plan construction. Any matching progress
  message cancels the acknowledgement timer.
- Route every expected server rejection through one failure helper. Capability
  rejection, missing project data, save failure, space/path/directory failure,
  and planning exceptions all send `status = "failed"` with user-facing copy.
- Freeze the request id beside the captured build plan before enqueue. The
  terminal result and registration callback use that snapshot rather than a
  mutable current input, and registration exceptions use the same failure path.
- If the input cannot be sent, acknowledgement never arrives, or Shiny
  disconnects while Planning owns the dialog, render the same terminal failure
  locally and restore a `Done` action. Do not automatically retry an operation
  whose server-side state is unknown.
- Preserve the existing rule that CRB progress owns the result dialog through
  its terminal screen, so late source-save messages still cannot overwrite it.

## Alternatives considered

1. **Server-only early-return fixes.** This closes known branches but still
   leaves the page stuck when the request is lost or the connection drops.
2. **Client-only timeout.** This restores usability but hides deterministic
   server failures and can mislabel a rejected request as a network problem.
3. **Chosen protocol closure.** Immediate acknowledgement, unified server
   failure, correlation, and a client fallback cover both sides without
   changing CRB build or artifact semantics.

## State flow

1. The user chooses `Prepare checked CRBs`; the browser allocates a request id,
   enters Planning, starts the acknowledgement timer, and sends the input.
2. The server stores the request id and immediately emits `planning`.
3. The server either emits `failed`, advances to `building` and `registering`,
   or completes with `ready`.
4. The browser ignores progress for a different request id. Matching progress
   cancels the initial timer.
5. A send failure, acknowledgement timeout, or disconnect renders a terminal
   failure with `Done`; the page is never left inert without an action.

## Error handling

- Server error text remains specific when a known guard fails.
- Unexpected plan-construction errors are caught at the CRB request boundary,
  reported through the same progress channel, and leave checkpoint cleanup to
  the existing safe cleanup helper when an output directory was created.
- Save failures report the existing save error when available.
- The browser timeout copy states that preparation did not start, avoiding a
  claim that a potentially running build was cancelled.

## Regression coverage

- The server contract requires a Planning acknowledgement and a common failure
  path for every return before `building`.
- The dirty-save callback must send failure when save returns false, including
  the already-saving path where no callback is scheduled.
- The browser contract requires a request id, one acknowledgement timer,
  correlation checks, cleanup on close/progress, and disconnect fallback.
- Existing Step 2, Step 3, terminal ownership, and Spatial scroll behavior stay
  unchanged.
