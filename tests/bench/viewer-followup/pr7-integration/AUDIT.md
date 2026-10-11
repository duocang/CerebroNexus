# Selective PR7 Viewer integration

Date: 2026-10-10. Existing checkout and branch `卧槽/pr6_clean_perf`.
Baseline: `2dbd6668744f5eb6c6898771a1c2cc566b0aa6c3`, initially clean.
Frozen local PR7 source: `cb770fe9328fb7037a0008a1bb54fc2744cf86ae`.
Merge base: `cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303`.
Remote-tracking ref: `75401c75c9926bf6948ec7736ce5483fe0e4d086`.
Read-only `git ls-remote origin refs/heads/perf/pr7-builder-workspace` returned
the same `75401c75` at audit start. No fetch, branch/worktree creation, merge,
rebase, broad cherry-pick, old-branch cleanup or PR7 modification.

## Evidence precedence and scope

Read the requested initial audit/disposition/acceptance and viewer-followup
reports, including Brave remaining/stability and the final Gene closeout.
`linked-tooltip/RESULTS.md` supersedes the earlier pinned-tooltip failure in
`linked-tail/RESULTS.md`. That defect is closed at this baseline.
No Gene, Linked or Trajectory performance experiment is reopened.

The audit compares merge-base -> source, merge-base -> target, and target ->
source for each mechanism. Source is fixed even if refs subsequently move.

| Mechanism / source | Current coverage and reproduction | Decision | Minimal dependency / validation |
|---|---|---|---|
| Early Gene selection, `4d6d14d9` | Current `reportSelection` / `specialistSelectionReport` withholds IDs while pending; selected index set is reported as stable barcodes on aux arrival. | Already better; reject digit-guessing fallback. It can confuse numeric barcodes and subset-local ordinals. | Existing cell-view-state/wire, hover-demand, generation tests; native brush before/after deferred IDs. |
| Canonical summary/table barcode, `cb770fe9` | Compact first-frame metadata holds row numbers; stable selection contains barcodes. Existing summary/table compare different identities. Production-body fixture reproduces wrong/empty cohorts, including numeric barcode collision. | Port canonical source into the two gated output bodies only. Keep lazy coordinate fallback and existing selectedCellMask. | Canonical indices + existing getMetaData, already demanded by selected identity. No new helper/framework. Subset/reorder/numeric barcode and summary/table consistency. |
| Gene progress, `21025249`, `e9a28c9a` | Current expression reactive starts progress even for already-painted keys; no hidden-page close observer. | Reproduce then port narrow UI lifecycle guard, keeping visibility/request reads isolated to avoid expression invalidation. | Existing render key/progress handle; Shiny testServer and native page return. |
| Stale group summary, `21025249` | Current selected group can briefly be absent in new dataset; unchecked column is NULL. | Reproduce then add guard with default req cancellation (clear output, never retain old plot). | Existing group UI and summary; actual render-body validation test. |
| Async dataset isolation, `5c5ca28f` | Current epoch/generation/request snapshot, aux token, image token/context, painted context and invalidation already present. Source commit re-adds duplicate server context blocks; extra Gene reactiveVals are unused in its event body. | Already covered; no second framework or unused state. | Existing dataset-context and stale-selection tests. No claim of resumable reconnect. |
| ROI active dataset / rotation, `91e87d77`, `6c936a53` | Target has no ROI mode/selector contract used by these patches. Source adds ROI transforms, per-ROI pivots/settings/extents, panel identity and content/coordinate contracts. | DEFERRED as a separate feature integration, not a target bug. | Closure: ROI selector + dataset/sample scoping + metadata facets + transform context/pivots + image alignment/parameters + renderer panel mapping + contract fixtures; audit producer/export requirements separately before port. |
| Removable background, `dd3313e6` | Target retains selectInput plus explicit `none` choice; source fixes its newly changed multi-select/removal UI. | PR7-specific UI fix, no port. | Current background normalization tests; no Spatial production change or Spatial browser run. |

## Preservation

No expression reader, RGB, geometry, precision, wire, cache or ready changes.
No new primary metadata materialization, fetch or payload. Canonical barcode
reads occur after existing nonempty, generation-validated selection gates;
the deferred core already requested that metadata. Selected outputs now allocate
the correct barcode slice rather than using compact ordinal strings. No new
retained cache. Full application heap/RSS equivalence is not claimed.

Validation and final commits will be recorded in RESULTS.md. Focused baseline
reproduction is `reproduce.R`; it reads baseline files into a temporary directory
and leaves the working source untouched.
