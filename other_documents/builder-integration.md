# Builder migration onto current master

Date: 2026-10-12. This is a reconstruction of the Builder feature on the merged
Viewer baseline, not a merge or replay of the old PR7 history.

## Frozen references and recovery

- Target master: `9b6ecc6f84f4223905b3cb2fd15ec7728571fd6f`.
- Local PR7 source: `cb770fe9328fb7037a0008a1bb54fc2744cf86ae`.
- Remote PR7 source: `75401c75c9926bf6948ec7736ce5483fe0e4d086`.
- Target/local-source merge base: `cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303`.
- Development branch: `codex/builder-on-master`.
- Recovery bundle: `C:/Users/wangx/Projects/CerebroNexus-backups/2026-10-12-builder-on-master/builder-sources.bundle`.
  `git bundle verify` succeeded; the bundle contains complete master, local PR7,
  and remote-tracking PR7 histories. `refs.txt` and `SHA256.txt` accompany it.

Master and local PR7 diverged by 43/204 commits. Local and remote PR7 are not
interchangeable: they diverged by 431/28 commits. The migration fixes the local
source above; the remote source is preserved for recovery and comparison. The
old branches are unchanged. No remote update is part of this migration.

## Integration boundaries

The shared export layer carries the content/coordinate contracts, portable
private application assembly, and spatial image declarations. The Builder
includes its four-stage workspace, isolated workers, project snapshots,
verification and publication recovery, example gallery, tests, and guide.
Viewer changes consume the Builder's ROI, spatial appearance, background and
initial projection settings.

Preserved from master:

- Canonical projection alignment, stable dataset identities and display labels.
- Deferred metadata, geometry reuse, lossless expression transport, Viewer Pack,
  generation isolation, painted readiness and the completed Gene/IR fixes.
- Existing Viewer `mirai` dependencies; Builder uses its own `callr` workers.
- Bounded dataset catalog/prototype behavior and no direct `openssl` dependency.
- Existing demo data and gene panels. The separate demo PR is not included.

Not replayed: old Viewer palette changes, duplicate context/lifecycle machinery,
old Gene fixes, benchmark evidence directories, CI sharding changes and unrelated
development history. The old shadow `state/build.R` is intentionally absent;
the local source already removed that duplicate state machine.

## Adaptations required during reconstruction

- Retain master's `dataset_labels` and positional API, append the missing
  `initial_projections` argument, and retain support for two dataset identities
  referencing one CRB.
- Carry dataset catalog information through Builder's one-use verification
  cache. An older three-field cache must not discard the merged startup catalog.
- Preserve the existing Linked Views deferred image-asset path instead of
  copying PR7's incomplete replacement (undefined image fields and an unmatched
  spatial function argument).
- Use identical raw-input MD5 digests for project/release identities. File copy
  verification uses bounded buffered copying followed by `tools::md5sum` on the
  destination; it incurs a verification read but never loads a whole file.
- Obtain authentication entropy from the OS: `/dev/urandom` on Unix and .NET's
  cryptographic generator through noninteractive PowerShell on Windows. Failure
  is fatal; there is no fallback to R's seeded statistical generator.
- Do not expose PR7's unfinished molecule/boundary toggles: the frozen source
  includes controls but does not send/draw those overlays. Exported sidecars are
  retained for subsequent Builder development.

- Keep embedded-image calibration marked as already applied. The shared
  Spatial helper must retain master's `geometryBaked` behavior, otherwise
  legacy embedded images receive their transform twice.
- Inspect worker trees with a fresh `ps` handle, verifying the owned process
  through processx before and after the PID lookup. A WSL run showed processx
  and ps start-time estimates differing by 133 ms for the same live child;
  the old conversion falsely reported that the worker had exited. Existing
  descendant verification, termination and quarantine behavior remain intact.
- Strengthen generated-App tests: select an actual fixture gene, wait for the
  initial Gene frame, then verify the requested gene's painted feedback and
  point count. The old test selected a nonexistent `Gene1` and accepted an
  allocated canvas; its parent-console attachment check also missed master's
  background prefetch worker.

- Keep the first Spatial section when a Linked supplement introduces Spatial
  for the first time. The primary-only saved workspace has no Spatial choice;
  restoring its empty list used to hide the newly arrived panel while leaving
  the picker populated. Existing explicit empty choices are still preserved
  once Spatial was available. A Node regression fails before and passes after
  this change, with stale-result and final-paint assertions retained.

## Verification

R integration tests run in WSL with R 4.6.1, not native Windows R. The Windows
OS entropy path separately passes with native R 4.5.0. No Viewer performance
claim or new benchmark target is made by this migration.

- Source package construction and installation succeeded. Installed Builder
  example/resource discovery passes `scripts/check-installed-builder.R`.
- `R CMD check --no-tests --no-manual --ignore-vignettes --no-build-vignettes`
  on the source tarball reports **Status: OK**. The focused tests below are run
  separately; this is not a claim that check ran the entire package test suite.
- The new Builder vignette renders to HTML. `node --check` succeeds for both
  `cell_views.js` and `builder.js`; changed R files pass Air formatting, and
  `git diff --check` passes. `nix-instantiate --parse default.nix` succeeds;
  a full Nix derivation was not built.
- Worker lifecycle: 281 assertions pass, including real child termination,
  active-call unwinding, restart, ownership checks, and rejection when the
  owned process exits during handle lookup.
- The fixture regeneration test passes after installing its optional `jpeg`
  dependency into an isolated test library. Package-wide dependencies and the
  user's R library were not replaced.

### Focused regression results

The latest-result ledger covers **88 test files and 1,764 named cases**:
**12,223 passing expectations, 15 skipped cases, zero failures and zero errors**.
This combines the focused runs, taking the latest result per file/test name;
it is not a claim that the entire package suite ran once on the final tree.
Two obsolete assertions for unimplemented source behavior were replaced, and
Builder-dependent export tests were moved into the integration test file.
The ledger script records those mappings and avoids counting repeated runs.

The final end-to-end run separately passes **414 expectations**: all **15**
valid combinations of embedded/H5/BPCells, plain/histology/Trekker, and CRB/App
outputs build and relocate. All **nine generated Apps** start in a hermetic
library excluding the CerebroNexus package and pass browser checks, including
actual fixture-gene painted feedback plus the relevant Linked spatial/Trekker
content. Automated browser checks use WSL Chromium/Chromote.

After the last JavaScript state correction, the supplement paint and deferred
readout tests pass again, including rejection of stale datasets and preservation
of user selection. The full artifact matrix above is rerun after that correction.

### Interactive smoke

Windows Codex in-app browser, with the R service in WSL (not a Brave run):

1. Open Builder, load all three bundled synthetic examples, and select each
   organism: Complete Viewer, four-tissue Trekker, arrow Spatial alignment.
2. Complete all three configurations, enter Review, then Build.
3. The WSL service has no native folder picker. The fallback correctly rejects
   a path outside its allowed server roots; an absolute path under the existing
   user-home root successfully creates a new release without widening access.
4. Build finishes with three CRBs and a Viewer App. The browser reports no
   console warnings/errors for this flow.
5. Launch the generated App independently, load Complete Viewer (180 cells),
   search/select EPCAM, and observe `Showing: EPCAM`, the 180-point view and
   the expression-by-cell-type plot.

The local smoke release is under
`/home/wang/cerebro-builder-integration-ui-20261012/CerebroNexus_viewer_374683`.
It is test output, not a published deployment. The existing demo preview on
port 8955 is unrelated and was left intact.

### Evidence and limits

Raw validation logs and a per-test evidence index are archived beside the Git
bundle in `validation/`. Earlier failed runs are retained as diagnostic history,
not counted as final passing results. The integration preserves mature Viewer
behavior; it does not establish new million-cell timing or memory guarantees.
Full native-Windows Builder execution, native picker dialogs, real Brave, and
full Nix builds remain outside this migration's verification scope.

## Reviewable history

1. `e84fed88` — shared export/content/coordinate contracts and compatible Viewer
   consumers, with regression coverage.
2. `87ea9eb9` — Builder workspace, worker/project/publication lifecycle, fixtures,
   dependency declarations and tests.
3. Documentation commit — user guide, project/fixture documentation and this
   provenance/verification record.

The 204 old PR7 commits are not replayed. No push, old-branch deletion, master
rewrite, or worktree creation is performed.
