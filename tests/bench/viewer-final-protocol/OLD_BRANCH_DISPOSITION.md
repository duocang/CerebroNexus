# Old `卧槽/*` branch disposition

Audit date: 2026-10-10. Integration branch: `卧槽/pr6_clean_perf`, based on
`perf/pr6-ui-accessibility-polish` (`cbd65fe0`). This is a source and focused
correctness audit. No full Viewer benchmark was rerun, so this document makes
no new timing claim.

## Backup before retirement

`C:\Users\wangx\Projects\CerebroNexus-backups\2026-10-10-pr6-clean` contains
the verified complete five-ref Git bundle, the 31-file binary dirty patch, and
all 4,798 nonignored untracked files (including generated benchmark evidence).
`BACKUP_MANIFEST.json` records exact sizes and SHA-256 hashes. At retirement,
the live binary diff and all 4,798 archived files were compared byte-for-byte
again. The exact patch was reversed and the untracked files were moved to
`retired-checkout-files` inside the backup directory. Their contents remain
recoverable both individually and from the tar archive.

## Committed branches

| Branch | Tip | Disposition |
| --- | --- | --- |
| `卧槽/pr1_new_crb` | `41045238` | Deleted after verified backup: thin CRB v2, stable fingerprint, BPCells storage-order access, safe session clone, and lazy spatial molecules already have equivalent or better code in clean PR6. Do not replay its benchmark scripts. |
| `卧槽/pr2_viewer_ui_fixes` | `9565225b` | Deleted after verified backup: selector width, controls, extra-material labels, and long legends are already in clean PR6. |
| `卧槽/pr4_viewer_lifecycle` | `ad285b71` | Deleted after verified backup: RGB batching, HLA parser/early guard/annotation/hidden gate are already integrated; clean PR6 has a newer request-context implementation. |
| `卧槽/pr5_viewer_requests` | `ad285b71` plus dirty files | Deleted after verified backup and retiring the dirty checkout contents. Committed tip equals `pr4`; useful dirty changes were ported as listed below. |

`卧槽/pr6_clean_perf` is the retained integration branch. On the subsequent
explicit retirement request, all four old refs above were deleted after backup
verification. No `卧槽/pr3*` ref existed. The original checkout was detached
before retirement; it is now the sole registered Git worktree on clean PR6.
The temporary clean/reference checkouts were removed after acceptance and
archiving their generated test artifacts. The unrelated `worktrees/pr7`
directory was preserved.
See `FINAL_ACCEPTANCE.md` for final worktree disposition and tests.

## Dirty `pr5_viewer_requests` worktree

| Area | Decision | Evidence / reason |
| --- | --- | --- |
| Selectize private picker cleanup | **PORTED** | Three reused Linked pickers destroyed Selectize without removing its `invalid` listener; cleanup now removes that listener and readiness flags. |
| HLA dataset change | **PORTED** | The old drawn box/lasso, selected nodes, tooltip, upload value, and mounted summary hosts now retire on a pending dataset request. |
| Spatial Moran score | **PORTED** | Both the render body and `bindCache` key now gate hidden/nonfeature work before reading spatial parameters or data. The clean dataset-generation cache token is retained. |
| Spatial controls and scroll hint | **PORTED selectively** | Sidebar navigation no longer reconstructs mounted controls; hidden outputs suspend. The hint scrolls the actual `.content-wrapper`, is a button, and cleans listeners on dismissal. Plot-type gene selectors remain lazy by mode. |
| Linked palette | **PORTED selectively** | A visible-only observer sends only changed palette patches and replays one on return; progressive primary/supplement sends mark their included palette as already sent. |
| HLA Data & QC stable host and Bootstrap DT | **ALREADY PRESENT** | Clean branch has mounted HLA host, sample-count output, and consistent Bootstrap table style. |
| Dataset generation, stale result rejection, selection reset, reconnect | **ALREADY BETTER** | Clean request context, pending/ready/error phases, scoped messages, and browser invalidation supersede the dirty `viewer-resources.js` / `dataset-context.js` framework. |
| Projection/Gene/Spatial/Trajectory render wrappers and `viewerPageDebounce` | **NOT PORTED** | They target the old Plotly path. Clean PR6 uses Viewer Pack, static geometry, progressive Linked frames, and generation-scoped requests; a broad observer rewrite would duplicate or bypass these paths without a demonstrated focused gain. |
| Gene coordinate dependency reduction | **NOT PORTED** | Clean projection assets already decouple geometry from appearance and expression updates. |
| Generic resource monkey-patching, global MutationObserver | **NOT PORTED** | Larger lifecycle surface and no proved incremental benefit over clean explicit invalidation. The small Selectize leak fix was extracted. |
| HLA vector parser, generic reader/chunking, graph formatter, 16-bit quantization | **REJECTED historic experiments** | Prior evidence showed memory regression, no stable full-page gain, or unacceptable precision loss. Do not revive. |
| `tests/bench/crb` generated runs, heap snapshots, `Rprof.out` | **ARCHIVED, NOT PORTED** | Evidence is in the verified tarball; generated outputs do not belong in the clean source tree. |

## Focused checks

- `node --check` for edited browser scripts: pass.
- R parse and `git diff --check`: pass.
- `testthat` filters `hla-network-selection`, `viewer-spatial-hidden-moran`,
  `viewer-hidden-palette`, and `spatial`: pass.
- No 1M, Ren, or Spatial full benchmark was rerun for this source migration.

These checks establish the narrow correctness changes above. They do not
establish a new first-usable, memory, or payload measurement.
