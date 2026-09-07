# Builder publication evaluation

This headless harness records trial-level evidence for Builder capability
detection, frozen-plan identity, plan-to-artifact fidelity, CRB builds, handled
publication failures, and incremental reuse.

Run a fast contract check from the repository root:

```sh
Rscript validation/builder/run.R --smoke
```

Run the complete synthetic fixture matrix:

```sh
Rscript validation/builder/run.R
```

Run the paper profile from a clean worktree with an explicit caller-owned
public-data cache:

```sh
Rscript validation/builder/run.R \
  --profile publication \
  --cache /absolute/builder-public-cache \
  --output /absolute/new/publication-run
```

After the run passes, validate and promote it into the package snapshot and
regenerate all paper figures:

```sh
Rscript validation/builder/publish.R \
  --run /absolute/new/publication-run
```

Choose an explicit new result directory when required:

```sh
Rscript validation/builder/run.R --output /absolute/new/result-directory
```

The runner refuses to overwrite an existing directory. By default it writes to
`results/builder/<git-sha>_<UTC timestamp>/`; generated results are ignored by
Git. Each run contains environment and fixture JSON manifests, six CSV evidence
tables, a `failures/` directory, and `summary.md`. Raw tables are written before
the correctness gates are evaluated, and the process exits non-zero if a gate
fails.

The fixture truth is declared independently from Builder profiles. Fixture
factories reuse the repository's deterministic synthetic Seurat helpers so the
evaluation does not maintain a second biological-object implementation.

## Publication public-data sources

The publication profile adds three CC BY 4.0 public-data confirmations whose
acquisition rules are pinned in `public_data.R`:

- 10x PBMC 3k gene expression (`pbmc3k`, Cell Ranger 1.1.0);
- 10x Visium mouse-brain sagittal anterior section 1, distributed as
  `stxBrain.SeuratData` 0.1.2; and
- 10x `vdj_v1_hs_pbmc3` matched 5-prime gene expression, filtered TCR
  contigs, and filtered BCR contigs (Cell Ranger 3.1.0).

Set the publication cache to a caller-owned directory. Downloads use a
`*.part` file and are renamed only after a complete non-empty transfer. The
evaluator never deletes the cache. Evidence stores source URLs, preparation
rules, file sizes and SHA-256 digests, and prepared-object dimensions and
SHA-256 digests; it does not copy the public matrices into the package.

The Visium source requires the exact `stxBrain.SeuratData` package. Missing
required sources or packages are hard errors in the publication profile, not
silently omitted matrix cells.

Publication trials use one warm-up and five measured independent R processes
per declared build cell. The evaluator records median/IQR/min/max timing, not a
mean-bar comparison or significance test. It also builds genuine CRBs for nine
release-fault scenarios: four controlled errors and five child-process exits at
journal transitions. Each scenario must reopen the prior CRB, leave no
unaccounted transaction residue, and support a subsequent clean publication.

These results support the exact claim "recovery under injected, handled
failures." They do not establish arbitrary `SIGKILL` or power-loss atomicity,
zero-downtime replacement, concurrent-publisher correctness, or universal
performance. Peak memory remains outside this evaluation.

The publisher re-reads every CSV, validates protocol repetitions and joins,
recomputes the summary, generates seven 300-dpi evidence panels plus the SVG
evidence chain, rejects absolute local paths, and updates
`inst/extdata/builder-evaluation/current/` only after the staged snapshot is
complete.
