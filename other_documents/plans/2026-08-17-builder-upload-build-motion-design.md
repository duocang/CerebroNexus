# Builder Upload and Build Motion Design

Date: 2026-08-17
Status: Approved

## Goal

Keep dataset upload progress visually stable during queued multi-file imports and
make the Build status fully visible without repeated page jumps.

## Upload rail

The ready dataset rail already uses an ordered snapshot reconciler, but the
import rail is still rendered as one Shiny `renderUI()` output. Every import
progress change can therefore replace all pending rows, including rows whose
visible state did not change.

The import rail will use the same authoritative snapshot pattern as the ready
rail. Each row is keyed by its stable import id and carries a fingerprint of all
visible state. The browser will reuse a row when its fingerprint is unchanged,
replace only the matching row when its visible state changes, insert new rows in
server order, and remove rows no longer present. Invalid snapshots are rejected
without partially changing the rail.

Queue progress must not take selection away from the user. Explicitly selected
ready or importing datasets remain selected while other imports advance. When
there is no explicit selection, the existing automatic focus may follow the
first active import. Completion may move from the loading workbench to the ready
dataset only when that import was already being watched.

## Build status motion

One Build click owns one bounded two-phase scroll lifecycle. The first scroll
runs after the immediate client-side `Preparing build` status is inserted. The
second runs after the first authoritative server-rendered status replaces it.
Both scrolls target the stable Build status host and move only far enough to
show the host's lower edge within the viewport. Later progress renders do not
continue to move the page.

Scrolling is smooth when motion is allowed and immediate when the operating
system requests reduced motion. The status host retains its focus and live-region
semantics.

## Failure handling

Malformed import snapshots are logged and ignored atomically. Missing Build
status elements cause the scroll lifecycle to stop without throwing. A second
Build click starts a new lifecycle and supersedes any stale pending phase.

## Verification

Contract tests cover import row fingerprints, ordered snapshots, removal of the
old whole-list `renderUI()` path, and the bounded two-phase scroll lifecycle.
Browser coverage proves that unchanged import rows retain DOM identity across a
sibling's progress update and that an explicit selection remains stable while a
queue advances. Focused tests run before the repository precheck.
