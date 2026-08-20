# Linked-view share links

## Goal

Allow a user to create an unguessable, temporary link to a canonical linked-view
configuration. The recipient can restore it only in the matching cell
population. Links expire after 90 days and the browser that created one can
revoke it.

## Scope

- Server-side persistence for canonical linked-view JSON and minimal metadata.
- A cryptographically random, opaque URL token for each link.
- Browser-held revocation receipts, scoped to the application origin.
- A `Share with a link` region in the existing Import / export view dialog.
- Automatic expiry and opportunistic deletion of expired records.

The feature does not add accounts, cross-dataset barcode mapping, collaborative
editing, or public discovery. A link is a bearer capability: anyone who has it
may restore its configuration until it is revoked or expires.

## Storage and identity

The Viewer uses a small SQLite database at an explicitly configured writable
application-data path. Each record contains:

- an opaque public token generated from 32 random bytes;
- a separate 32-byte revocation secret, stored only as a hash server-side;
- canonical JSON, dataset fingerprint, creation time, expiry time, and revoked
  time if applicable.

The client receives the public token and a revocation receipt once, then keeps
the receipt in `localStorage`. The server never stores a browser or user
identity. This makes revocation possible without requiring authentication while
ensuring a copied public URL alone cannot revoke a link.

## Data flow

1. The user chooses **Create share link** with an active selection.
2. The browser sends the captured view to the existing server contract.
3. The server validates and canonicalizes it, then writes the canonical JSON
   with a 90-day UTC expiry to SQLite.
4. The browser displays and copies the resulting absolute share URL, then
   stores its revocation receipt locally.
5. Opening a share URL asks the server for the stored JSON. The server rejects
   expired, revoked, malformed, or unknown tokens. The normal configuration
   validation and dataset-fingerprint guard run before the browser applies it.
6. The creator can choose **Revoke** from its local list. The client sends the
   token plus receipt; the server compares the receipt hash and marks the record
   revoked. The public URL immediately stops working.

## User experience

The existing dialog gains a clearly separated **Share with a link** region.
It explains that the link contains view settings and selected cell barcodes,
expires in 90 days, and works only for the matching cell population. After
creation it shows a concise success panel with **Copy link** and expiry date.
The same region lists links created by this browser with **Copy** and **Revoke**
controls. A missing browser receipt means the link remains usable but cannot be
revoked from that browser.

Opening a URL with `?linked_view=<token>` does not mutate the current view until
the server and browser have both passed normal validation. Failure messages are
specific: expired/revoked links say they are no longer available; a valid link
for another cell population says so prominently.

## Limits and operational behavior

- Canonical JSON is limited by the existing 5 MiB contract limit.
- Public token and receipt are URL-safe fixed-length strings; both have strict
  input limits before database work.
- SQLite writes use short transactions; expired and revoked rows are deleted
  opportunistically during create and lookup operations.
- If the application has no configured writable share-store path, sharing is
  disabled with an actionable message. Local saves, copy, download, and import
  continue to work.
- The configured database must be shared by all application workers for links
  to work across workers and restarts; ephemeral container filesystems are not
  suitable production storage.

## Tests and acceptance

- Pure storage tests cover random-token shape, expiry, revoke authorization,
  and cleanup.
- Contract tests ensure only canonical JSON is persisted and that different
  dataset fingerprints are rejected before application.
- Browser tests cover create/copy/revoke, automatic URL restore, expiry and
  mismatch errors, and the no-store disabled state.
- Existing local saves and JSON import/export continue to work unchanged.
