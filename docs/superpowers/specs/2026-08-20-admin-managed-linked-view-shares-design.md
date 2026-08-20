# Admin-managed linked-view sharing

## Goal

Replace browser-owned link ownership with administrator-managed, immutable
share links. A cell population may have many independent links for different
view snapshots. Reuse the Viewer's existing login system instead of
introducing a second password or authentication flow.

## Chosen interaction

The existing login-account setup gains roles. Exactly one account is the
**Administrator**; every other account is a **Viewer**. When login is first
enabled, the first account defaults to Administrator and subsequent accounts
default to Viewer. The builder requires exactly one Administrator before it can
build an authenticated Viewer.

After login:

- an Administrator sees an **Admin** sidebar item immediately below **About**;
- a Viewer does not receive or see the Admin navigation or management UI;
- visiting the application with `/admin` deep-links an Administrator to that
  page, while a Viewer is returned to the normal application with an explicit
  access-denied message;
- `/admin` is only a navigation convenience. Server-side authorization, not
  knowledge of the path, protects every management operation.

The Admin page follows the existing Viewer visual system. It contains a
**Shared views** section listing every unexpired link, grouped or filtered by
cell population. Each row shows dataset identity, creation and expiry dates,
the creating Administrator, and **Copy link** and **Revoke** actions.

## Authorization model

The encrypted `shinymanager` credentials database remains the sole source of
account identity and role. Its existing `admin` field is populated by the
builder. The authenticated server session derives an `is_admin` capability from
that server-side identity and passes the capability into the Viewer server.

Authorization is enforced twice:

1. UI capability gating omits Admin navigation and share-management controls
   for ordinary Viewers.
2. Server handlers independently reject create, list, and revoke
   operations unless the authenticated session is Administrator.

No role, password, revocation secret, or management capability is stored in
browser storage or accepted from client-supplied flags. Authentication retains
the existing session timeout and logout behavior.

When Viewer authentication is not configured, local JSON import/export and
browser-local saved views remain available, but server share creation and the
Admin page are unavailable. Opening an existing valid share link remains a
read-only operation if the deployment intentionally permits public Viewer
access.

## Link lifecycle and storage

Each create action produces an independent share record:

- **Create** stores a canonical snapshot of the Administrator's current linked
  view and returns its random, unguessable URL.
- **Copy link** copies the already-created URL and gives immediate `Copied`
  feedback; it performs no database work.
- **Revoke** invalidates only the selected URL.
- links expire after 90 days as before.

There is no product-level one-link-per-dataset limit. Multiple Administrator
sessions or browser tabs may create multiple valid snapshots for the same
dataset, and each receives a distinct token. Administrator identity is recorded
as audit metadata but is never embedded in the public URL.

The old browser-held revocation receipt and multi-row local link library are
removed. Recipients may restore and manipulate the configuration in their own
session, export it, or save it locally, but cannot mutate the stored shared
snapshot. Creating another link never edits an existing link; links are
immutable until individually revoked or expired.

## Linked-view dialog

For an Administrator, the existing management dialog keeps a compact
**Share with a link** region for the current dataset. It shows one of three
states: ready, creating, or success/error. Creating a link immediately presents
that new URL. Historical links and their revoke controls live on the Admin page
instead of expanding the linked-view dialog.

For a Viewer, the region is omitted. JSON import/export and **Saved on this
device** are unchanged. A recipient opening a link receives the normal strict
configuration and dataset validation before any visible state mutates.

## Errors and security boundaries

- Unauthorized management requests return a generic forbidden result and do
  not reveal link metadata.
- Expired, revoked, malformed, unknown, and dataset-mismatched links keep their
  existing specific restore messages.
- Create validates and canonicalizes the complete configuration before
  beginning the database transaction.
- The public token remains a 256-bit random bearer token and the configuration
  retains the existing 5 MiB limit.
- Passwords continue to be hashed by `shinymanager`; the feature adds no
  plaintext password, URL password, shared secret, or second login screen.

## Acceptance criteria

- Builder account setup visibly distinguishes one Administrator from Viewer
  accounts and refuses zero or multiple Administrators.
- Administrator login shows the Admin sidebar page; Viewer login does not.
- A direct `/admin` visit cannot bypass server authorization.
- An Administrator can create multiple independent links for the same dataset,
  list them on the Admin page, copy them, and revoke them individually.
- Revoking one link makes that URL unusable immediately without affecting any
  other link for the same dataset.
- Ordinary Viewers cannot invoke management operations through crafted Shiny
  messages.
- Recipients can restore a valid link but cannot overwrite its stored snapshot.
- Local saves and JSON import/export continue to behave as before.

## Rejected alternatives

- A separate `/admin` password was rejected because it duplicates the existing
  account system and creates two independent credential lifecycles.
- A hidden `/admin` path without role authorization was rejected because an
  unadvertised URL is not an access-control boundary.
- Browser-owned revocation receipts were rejected because ownership disappears
  when storage is cleared and does not express the application's Administrator
  role.
