# Colleague Spatial Builder integration design

## Goal

Create one local delivery branch for a colleague who needs to build a Viewer app quickly, including the current coordinated Viewer, Builder, and the complete four-PR stack ending in the multi-spatial-image API.

## Branch structure

`integration/colleague-spatial-builder` starts at `feat/multi-spatial-image-api`. That branch already contains, in order, `fix/bounded-biomart-retries`, `refactor/versionless-cerebro-class`, and `feat/omnibus-seurat-pipeline`. The integration branch then merges `feat/coordinated-views-nexus` and `feat/cerebro-builder`.

## Conflict policy

- Coordinated views owns Viewer navigation and page removal.
- The multi-spatial-image API owns spatial image manifests, payloads, and image identity.
- Builder owns dataset ingestion, configuration, build, and publication.
- Deleted legacy Overview/Spatial routes stay deleted; their useful data contracts must be adapted to coordinated views instead of restoring the pages.

## Verification

Keep verification intentionally narrow: confirm all source branches are ancestors, ensure no unresolved conflict markers or whitespace errors remain, and keep the worktree clean. Do not run the full package suite in this integration pass.
