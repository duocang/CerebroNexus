# PR184 local integration closeout

2026-10-11. Work remains on `perf/pr6-ui-accessibility-polish` in the existing
checkout. No new branch/worktree and no push. `identity.json` fixes all source
SHAs and the verified, complete-history backup bundle and SHA-256 checksum.

## Integration boundary

- Original local head: `78d4aa097b0608854f7e0a24f6e50abcfbef8e80`.
- Old boundary: `cbd65fe0fa2ca3b65e6b68180c42f5ced8bb9303`.
- PR184 source fetched from `origin/perf/pr5-million-cell-final-optimization`:
  `78afda428c89f0d45d6958664d1490cfd43230cf`.
- Upstream master included by that source:
  `c44a5e3f9377f4416680347f32b5379d5ecc36cb`.
- Rebased implementation head: `cc50e6a1`.

The fetched PR184 source already contains the original PR6 work, the Ren
benchmark documentation (`ea4ae216`), the CI/identity fixes (`6438dd9e`), and
the merge of current upstream master. Replayed only the 12 local commits
after `cbd65fe0` onto that source. Neither the old PR0–PR5 stack nor the
original PR6 commits were replayed again.

`range-diff.txt` records the one-to-one mapping. Eleven patches are unchanged;
the first absorbs equivalent upstream fixes and resolves five test-file
conflicts. There were no production-code conflicts. Resolution preserves:

- Gene selector interaction through its actual Selectize control;
- render stubs accepting the current request context/auxiliary arguments;
- current projection generation checks, including rejection of stale redraws;
- stable dataset IDs in browser selector assertions and inputs;
- upstream waiting for actual Spatial activation and the other CI fixes.

Compared with the pre-rebase implementation, production differences are only
the fetched upstream dependency/build fixes, duplicate dataset-file validation,
public argument ordering, and explicit `utils::head` qualification in both HLA
core copies. The Viewer performance and lifecycle implementation is preserved.

## Validation

The accompanying runner separates targeted unit/integration tests from browser
tests and retains a CSV plus console log for each. Environment is Windows
checkout with WSL R 4.6.1; browser regression uses installed Linux Chrome via
shinytest2. This is not a new Windows Brave smoke, performance A/B, or full
Nix/remote CI result. Historical acceptance evidence remains in its original
directories without changing its source identities or timing conclusions.

Unit/integration run (`unit.csv`, `unit.log`): **10,244 assertions passed,
zero failures/errors/warnings, one skip**. The skip requires HDF5Array to be
absent; it is installed in this environment. All three Builder tests that
previously produced eight failed assertions now pass (14 assertions in those
three tests), confirming that the fetched CI/identity fixes resolve them.

The first browser run (`browser.csv`, `browser.log`) passed the IR file
(87 assertions) and generated-application smoke file (60 assertions). The
general app file passed 52 assertions and failed two stale animation
expectations. Both demanded `cerebro-control-enter` on Gene controls, but
the intentionally removed staggered entrance no longer exists in CSS.
Updated those expectations to `none`; enabled-control, selection, mode,
expression-result and dataset-switch assertions remain intact. No product
code was changed in response to this run. The affected app test file was
rerun in `app-recheck.csv` / `app-recheck.log`: **54 assertions passed, zero
failures/errors/skips/warnings**. Together with the unchanged IR and smoke
results, the final browser coverage is **201 passing assertions**, without
double-counting the superseded general-app run.

The browser is Google Chrome 153.0.8010.52 on Linux/WSL. No new million-cell
timing claim is made. These checks supplement, rather than relabel, the earlier
Windows Brave evidence.

## Delivery

The PR184 source commit is an ancestor of the rebased local branch. Subject to
the remote source remaining unchanged, delivering this branch to PR184 can be
a normal fast-forward update of `perf/pr5-million-cell-final-optimization`;
force-pushing is unnecessary. No remote ref or PR description has been changed.
The final commit containing this report records local integration acceptance;
it does not claim that GitHub PR184 has been updated or merged.
