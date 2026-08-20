# Current integration export equivalence design

## Goal

Regenerate and audit the Builder and scripted Shiny App export paths from the
current `integration/colleague-spatial-builder` source. Replace the obsolete
`48757fdf` delivery artifacts with outputs produced from the current integration
commit, and turn previously inferred behavior into reproducible regression
evidence.

## Scope and ownership

- Source changes, fixtures, and regression tests live only on
  `integration/colleague-spatial-builder` and are committed before handoff.
- `/Users/nuioi/Downloads/anna_lena` is a regenerable delivery workspace, not a
  source-of-truth repository.
- Before replacing delivery artifacts, record a read-only manifest containing
  paths, sizes, and hashes in temporary audit storage.
- Remove snapshot residue that is not part of the selected Git commit when
  refreshing `packages/CerebroNexus`.

## Export workflow

1. Confirm the integration worktree is clean and record its exact HEAD.
2. Build a clean source snapshot from that HEAD without `.git`, local process
   state, rendered plots, or test snapshot residue.
3. Regenerate the scripted export with `export_shiny_app.R`.
4. Regenerate the Builder export through the real Builder planning and bundle
   pipeline, not by copying or post-editing the scripted output.
5. Require both generated Apps to identify the same current source revision in
   the audit evidence.

## Controlled synthetic evidence

Synthetic fixtures must contain explicit expected values and cover:

- non-empty extra tables/material;
- multiple datasets, FOVs, and image labels, including repeated filenames whose
  semantic targets differ;
- non-identity and identity coordinate transforms with known bounds-center
  pivots and expected coordinates;
- legacy pre-spatial CRB compatibility and legacy multi-path image declarations;
- closure/environment and serialized-attribute path scanning;
- startup from a moved App directory and failure behavior from an unsupported
  working directory;
- Viewer FOV/image switching and render-layer coordinate/image alignment.

Fixtures are test data, not evidence fabrication: generation code, seeds,
expected values, and assertions are committed together.

## Verification and classification

- Compare data by semantic identifiers rather than incidental order or file
  names.
- Compare external and embedded images by the full
  dataset/FOV/label mapping and decoded bytes.
- Recompute transforms from fixture source coordinates with declared tolerance,
  axis convention, and pivot method.
- Exercise all configured FOV/image choices in a browser test and verify the
  selected semantic target, image identity, and plotted coordinates.
- Move copies of both Apps to fresh temporary directories and launch them there.
- Recursively scan ordinary files, serialized attributes, R6 fields, function
  environments, and environment parent chains for prohibited absolute paths.
- Run focused tests during implementation and one complete project check after
  commit history is stable.

Differences are classified exactly once as expected storage/input differences,
Builder product gaps, or export bugs. A current-integration delivery passes only
when both exports run independently and all core semantic assertions pass.

## Safety

- Do not alter the source PR worktree or unrelated branches.
- Do not push.
- Do not claim portability beyond the performed move-and-launch test.
- Retain audit manifests in temporary storage until final verification is
  complete; delivery artifacts themselves remain regenerable.
