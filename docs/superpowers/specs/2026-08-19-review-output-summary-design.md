# Review Output Summary Design

## Problem

The Review page currently presents `tempdir()/cerebro-builder-output-preview`
as the output folder. This directory is an internal placeholder used to freeze
and estimate the build plan. It is not a user-selected destination and is not
where users should expect to retrieve their completed CRBs. Showing it exposes
an unstable implementation detail and gives the false impression that the
build output will remain at that path.

## Decision

Remove the `Folder` field from the Review page's Output section. Keep the three
decision-relevant fields:

- number of CRB files created;
- estimated output size;
- estimated build time.

Add the explanatory sentence: `CRB files will be available to download after
the build completes.`

## Scope

This is a presentation-only change. The temporary directory remains available
inside the frozen preview plan because planning and size estimation currently
require an output directory. Build staging, publication, verification, and
download behavior do not change.

## Error handling

No new error state is introduced. Existing Review plan errors and build errors
continue to use their current paths.

## Verification

- The Review UI does not render the `Folder` field or the preview temporary
  directory.
- The download explanation and the three remaining output fields render.
- Existing build-plan and download behavior remain unchanged.
