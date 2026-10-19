# Viewer benchmark results and acceptance boundaries

This summary retains historical measurements, test provenance and evidence
boundaries. Reusable measurement tools remain under `tests/benchmarks/viewer/`; small data
used by installed articles are in `inst/extdata/benchmarks/`.

## Quantified results and limits

These are specific backend operations on the loaded Ren repertoire, measured
in WSL R. They do not represent whole-page browser loading times.

| Operation | Matched baseline median | Candidate median | Evidence |
|---|---:|---:|---|
| Grouped Homeostasis construction plus ggplot build, three interleaved pairs | 5,400 ms | 762 ms | [Protocol and raw series](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/ir-grouped-homeostasis/RESULTS.md) |
| SizeDist repeated linkage change, three interleaved pairs | 2,059 ms | 105 ms | [Protocol and equality checks](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/repeated-work-fix/RESULTS.md) |

Stable Gene <=3-second first usability was not achieved. RAF throttling was
also observed outside Viewer and its environment-specific cause remains unresolved.
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

The current regression suite can be run from a checkout with
`Rscript -e 'testthat::test_local()'`. Historical counts above do not certify
subsequent source changes.

## Historical evidence archive

The authoritative retained Git snapshot is
`a5dc1103aca8f515b5b2d23c62c8df743b2919ee`. Its historical directories remain
accessible through immutable links, including their internal relative links:

- [Follow-up reports and raw evidence](https://github.com/duocang/CerebroNexus/tree/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup)
- [Original integration protocol](https://github.com/duocang/CerebroNexus/tree/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-final-protocol)
- [Latest bounded performance closeout](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/gene-diagnosis/CLOSEOUT.md)
- [Linked tooltip fix superseding the older failure report](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/linked-tooltip/RESULTS.md)
- [CI correction and full local results](https://github.com/duocang/CerebroNexus/blob/a5dc1103aca8f515b5b2d23c62c8df743b2919ee/tests/bench/viewer-followup/pr184-ci/RESULTS.md)
