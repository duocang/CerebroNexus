# PR184 Viewer acceptance and evidence

This is the maintained summary for the Viewer closeout. One-off investigation
scripts, logs, screenshots, traces and superseded reports have been removed
from the current tree. Production code, `tests/testthat` regression coverage,
and the reusable benchmark harnesses in this directory remain.

## Implementation and integration

- Preserve canonical cell identity, deferred metadata, geometry reuse, exact
  expression transport and dataset-generation rejection of stale results.
- Restore colored icons, unify loading feedback, and fix Gene selection,
  progress and obsolete-group-summary lifecycle handling.
- Keep Linked hover enabled by default, independently of selection/zoom and
  the Data Info hover-details preference. Maintainer commit `23ec52a3` moves
  this switch into the toolbar; merge `492a15af` includes it while preserving
  local IR wrapper cleanup `b87238b7`.
- Avoid repeated IR preparation, use exact grouped Homeostasis counts, bound
  resource retention and prevent whole delayed-matrix densification on export.
- Spatial ROI/content-contract integration remains a separate Builder topic.

## Quantified results and limits

These are specific backend operations on the loaded Ren repertoire, measured
in WSL R. They do not represent whole-page browser loading times.

| Operation | Matched baseline median | Candidate median | Evidence |
|---|---:|---:|---|
| Grouped Homeostasis construction plus ggplot build, three interleaved pairs | 5,400 ms | 762 ms | [Protocol and raw series](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/ir-grouped-homeostasis/RESULTS.md) |
| SizeDist repeated linkage change, three interleaved pairs | 2,059 ms | 105 ms | [Protocol and equality checks](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/repeated-work-fix/RESULTS.md) |

Stable Gene <=3-second first usability was not achieved. Further Gene, Linked
and Trajectory performance investigation was stopped; RAF throttling was also
observed outside Viewer and its environment-specific cause remains unresolved.
No weaker paint gate, lower expression precision or global-idle substitute was
introduced. No overall Viewer speedup factor or complete steady-state memory
proof is claimed. Windows Brave smoke evidence used WSL R services; it is
distinct from Linux Chrome regression testing and tied to its recorded source.

## Verification

At `a5dc1103`, all GitHub checks passed:
[R tests](https://github.com/mihem/CerebroNexus/actions/runs/38131736014),
[R CMD check](https://github.com/mihem/CerebroNexus/actions/runs/38131736006),
[pkgdown](https://github.com/mihem/CerebroNexus/actions/runs/38131735999).
Local source tests had 14,266 passing assertions with two conditional skips;
installed-package tests had 13,696 passing assertions with 74 conditional
skips, and R CMD check reported zero errors/warnings. These historical counts
are not presented as validation of later commits.

The integration/cleanup check after merging `23ec52a3` and preserving
`b87238b7` passed **1,281 assertions, zero failures/errors/warnings**, with one
skip for the unsupplied Mouse trajectory artifact. It covers Linked Views,
million-cell contracts and the affected IR tests. The existing test suite can be run from the repository
root with `Rscript -e 'testthat::test_local()'`. No new benchmark is needed for
removing archived investigation output.

## Historical evidence archive

The authoritative retained Git snapshot is
`a5dc1103aca8f515b5b2d23c62c8df743b2919ee`. Its historical directories remain
accessible through immutable links, including their internal relative links:

- [Follow-up reports and raw evidence](https://github.com/duocang/CerebroNexus/tree/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup)
- [Original integration protocol](https://github.com/duocang/CerebroNexus/tree/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-final-protocol)
- [Latest bounded performance closeout](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/gene-diagnosis/CLOSEOUT.md)
- [Linked tooltip fix superseding the older failure report](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/linked-tooltip/RESULTS.md)
- [CI correction and full local results](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/pr184-ci/RESULTS.md)

Before cleanup, 1,073 tracked files (22,911,273 bytes) were also archived
outside the checkout and verified byte-for-byte against the tracked files.
The local backup directory is
`CerebroNexus-backups/pr184-evidence-cleanup-20261011-133019` beside this
repository. It contains `viewer-evidence.zip`, a per-file SHA-256 manifest,
and a verified complete-history Git bundle at `b87238b7`. ZIP SHA-256:
`678fd73b5458582774aae183f32af7058cd132f1de55bb3a74f7f6370690bc77`.
Another 25 previously ignored local diagnostic files were moved into that
backup separately and checked against a per-file SHA-256 manifest.

This cleanup changes the final tree only. Published history is not rewritten;
it does not remove these objects from historical clones or revoke public access.
