# Builder output macOS metadata design

## Goal

Finder-created `.DS_Store` files must not prevent selecting, validating, or
publishing to an otherwise safe Builder output directory.

## Boundary

- Ignore a path only when its basename is exactly `.DS_Store`, at any depth.
- Continue treating every other unplanned file, including other dotfiles, as a
  foreign release entry.
- Apply the rule while computing release identity so Review/build CAS does not
  fail when Finder creates or removes `.DS_Store`.
- Reuse the same predicate in the lightweight folder-selection scan.
- Preserve `.DS_Store`; Builder does not need to delete host metadata.

## Verification

- Release identity excludes root and nested `.DS_Store` entries but retains an
  unknown dotfile.
- Coordinator preflight accepts `.DS_Store` and still rejects real foreign
  occupants.
- The real ready-CRB workflow can select an output directory containing
  `.DS_Store`.
