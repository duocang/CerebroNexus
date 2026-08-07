# Builder fixtures

Inputs for exercising the dataset builder (`launchCerebroBuilder()`) by hand.

These are **not** demo `.crb` files. Those are the builder's *output* and their
registry is [DATASETS.md](DATASETS.md). These are the Seurat objects and
background images that go *in*.

```sh
Rscript data-raw/build_builder_fixtures.R
```

Everything is synthetic and builds offline in under a minute. Output lands in
`data-raw/builder_fixtures/` and is gitignored — regenerate it, never commit it.
Point the builder's file browser at that directory.

## What each one is for

| Fixture | Exercises |
|---|---|
| `plain.rds` | no spatial data at all — the spatial card must not appear |
| `formats.rds` / `.qs2` / `.qs` | every format the builder claims to read |
| `one_section.rds` + `one_section.png` | one sample, one section — the common case, which must stay simple |
| `one_sample_three_sections.rds` + `cutA/B/C.png` | one donor cut three times: the section picker, per-section images, "apply to all" |
| `three_samples_five_sections.rds` + one PNG per section | three donors, five sections, mapping 2/1/2 — section count is not sample count and neither follows from the other |
| `pixel_coords.rds` + `pixel_coords.png` | the one case where "cell coordinates are image pixels" is literally true (600×400 image, 600×400 coordinate span) |
| `with_markers.rds` | an object that already carries marker genes — the "already in this object" pills and the analysis card both branch on this |
| `all_modalities.rds` | immune repertoire + HLA typing + Trekker on one object — the three pages that come from `@misc` rather than from a builder control |
| `de_results.csv` | a table to attach as supplementary material |
| `big.rds` (15 000 cells, ~11 MB) | large enough to watch the worker process work and confirm the page still answers |

## Notes worth keeping

**`.qd` is not offered, and cannot be.** qs2's `qdata` format does not serialise
S4 at all: `qd_save()` on a Seurat object warns *"Objects of type S4 are not
supported in qdata format"*, writes a 38-byte container, and reads back as
`NULL`. This fixture set is how that was found — the builder used to list `.qd`
among the readable formats, which told users something untrue.

**`.qs` needs the `qs` package**, which has no build for current R here, so that
fixture is skipped with a message rather than failing the run.

**Sections are placed 500 units apart** on the x axis. Two overlapping sections
are indistinguishable from one section drawn with the wrong coordinates, so
keeping them apart is what makes a mistake visible.

**The Trekker payload is copied structurally, not by key name.** A first
attempt matched the top-level keys of `demo_trekker.crb` and the page threw:
`moran` is a list of records each carrying `$gene`, not a named vector of
coefficients; a field is `list(v, max, label, desc, by_type)`, not a bare
vector; and `qc` is a flat block of 23 named scalars the page formats one at a
time. None of that is guessable, and a wrong shape raises an error rather than
degrading — which is exactly why the fixture exists.

**On a multi-section object, prefer the bounding-box extent mode.** In the
default "coordinates are image pixels" mode every section gets `[0, image
width]` regardless of where its cells sit, so every section but the one near the
origin lands entirely outside its image. The per-section coverage warning
reports it, but it is easy to walk past.
