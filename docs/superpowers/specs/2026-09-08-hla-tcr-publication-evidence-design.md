# HLA/TCR Publication Evidence Pipeline Design

**Date:** 2026-09-08

## Goal

Turn the existing real 12,000-cell HLA/TCR Cerebro object into a publication-
grade, auditable evidence pipeline. One reproducible command must regenerate
the biological results, statistical tables, scientific figures, Linked views
artifacts, and Viewer screenshots used by the paper.

Persisting an intermediate Seurat object is explicitly out of scope. The
publication artifact is the complete Cerebro object; Seurat remains a transient
implementation detail of the raw-data rebuild.

## Scientific Scope

The checked-in `demo_hla_tcr_dextramer.crb` remains the canonical daily input.
It contains real sparse transcriptome data, a real UMAP, paired alpha/beta TCR,
four independently HLA-typed donors, and per-cell dextramer reagent calls.

The publication presents two intentionally different cases:

1. **Primary biological case:** the strict TRB clonotype
   `TRBV19 / TRBJ1-5 / CASSIYSNQPQHF`. Its stable sequence definition and ten
   cell barcodes are authoritative for biological claims.
2. **Secondary product case:** the 293-cell `CTgene` clone used to demonstrate
   the Viewer clone-selection and Linked views workflow. It is not described as
   an exact sequence-defined clonotype or as an independent biological result.

The evidence may support observed expansion, sequence similarity, donor
distribution, mapping into the measured transcriptome, and restoration of a
stable cell selection. It must not claim validated peptide specificity, causal
HLA restriction, or population-level HLA association from four selected
donors.

## Two-Tier Reproducibility Model

### Tier 1: deterministic CRB publication build

This is the normal local and CI path. It is offline and starts from the tracked
CRB. It must:

- validate the CRB checksum and structural contracts;
- independently derive both cases from expression metadata and repertoire;
- generate every machine-readable artifact, table, and scientific figure;
- optionally launch the real Viewer and capture the prescribed screenshots;
- stage all outputs and publish them only after every validation passes; and
- compare regenerated outputs with the tracked publication artifacts.

### Tier 2: raw-source release rebuild

This is a release and manuscript-submission gate, not a routine CI job. It must:

- download or reuse the four official 10x donor bundles;
- verify each raw input against a pinned byte size and SHA-256 digest;
- recreate the transient Seurat object with fixed parameters and seed;
- assemble and validate a staged Cerebro object;
- run Tier 1 against that staged object; and
- report whether the reconstructed object and all derived numerical evidence
  match the checked-in publication release.

Network availability must never determine ordinary test results. A raw rebuild
failure leaves the tracked CRB and all tracked publication artifacts untouched.

## Command Interface

One top-level script owns the user-facing interface:

```bash
# Offline, CI-safe regeneration and verification from the tracked CRB.
Rscript data-raw/build_hla_tcr_publication.R --from-crb --verify

# Release gate from pinned raw inputs, including Viewer screenshots.
Rscript data-raw/build_hla_tcr_publication.R \
  --from-raw --verify --screenshots
```

The top-level script may call focused internal scripts, but users and the paper
must cite only these two commands. Existing case-builder scripts are folded
into this entry point so they cannot publish divergent answers.

## Inputs and Provenance

`data-raw/hla_tcr_dextramer_sources.csv` records one row per downloaded file:

- donor;
- modality (`tcr_contigs`, `dextramer_calls`, or `gene_expression`);
- official URL;
- expected byte size;
- expected SHA-256;
- source release (`Cell Ranger 3.0.2`); and
- licence (`CC BY 4.0`).

The raw builder must reject missing, empty, oversized, undersized, or
checksum-mismatched inputs before parsing. Downloads continue to use a `.part`
file and atomic rename.

The publication manifest records:

- raw-input manifest checksum;
- CRB SHA-256;
- R version and relevant package versions;
- fixed analysis parameters and random seed;
- cell/gene counts and donor balance;
- the exact biological definitions of both cases; and
- the filenames and SHA-256 values of all generated tabular/configuration
  artifacts.

Timestamps, temporary paths, hostnames, and usernames are excluded from
reproducible artifacts. The raw build uses a fixed artifact-release date rather
than `Sys.Date()`. UMAP is run with an explicit seed and one worker.

## Derivation and Outputs

`data-raw/build_hla_tcr_publication.R` performs one read of the CRB and derives
all publication evidence in stable barcode, donor, allele, and sequence order.
It writes into a staging directory adjacent to the final outputs.

### Durable machine-readable outputs

The existing public filenames remain stable where practical:

- `demo_hla_tcr_dextramer.case.json` — strict TRB case and claim boundaries;
- `demo_hla_tcr_dextramer.golden-clone.barcodes.tsv` — authoritative ten cells;
- `demo_hla_tcr_dextramer.linked-views.json` — strict-case Viewer state;
- `demo_hla_tcr_main_case.expectations.json` — secondary CTgene workflow;
- `demo_hla_tcr_main_case.linked-view.json` — secondary Viewer state;
- `demo_hla_tcr_publication.manifest.json` — provenance, parameters, versions,
  output checksums, and the relationship between the two cases;
- `demo_hla_tcr_publication.case-summary.csv` — one row per case;
- `demo_hla_tcr_publication.donor-summary.csv` — donor counts and HLA context;
  and
- `demo_hla_tcr_publication.motif-summary.csv` — motif membership and counts.

JSON preserves structured application contracts. CSV provides transparent,
journal-friendly supplemental tables without requiring readers to understand
the application schema.

### Scientific figures

Scientific figures are generated directly from the CRB and the derived tables:

- strict-clonotype UMAP with all cells and donor-coloured selected cells;
- strict TRB Hamming-1 motif network;
- CTgene workflow UMAP/clone context where retained by the paper; and
- HLA genotype-context summary used by the manuscript.

Scientific figures use fixed dimensions, ordering, labels, colour mapping, and
seed. Vector SVG is authoritative; PNG is generated only where the site or
paper workflow requires it. Tests validate the underlying numerical data and
figure dimensions, not platform-sensitive pixel hashes.

### Viewer screenshots

The screenshot stage launches the installed-layout Viewer with the same CRB and
imports the generated Linked views configuration. It captures named states only
after explicit readiness conditions:

- dataset information/provenance;
- real-data QC;
- strict selection in Linked views;
- HLA association context;
- TRB motif network; and
- share-selection dialog.

Each screenshot is accompanied in the manifest by the dataset fingerprint,
selection fingerprint, viewport size, page/state name, and source commit.
Screenshots are visual evidence, not numerical authority. The release gate
fails when the requested state cannot be reached, but normal logic CI does not
depend on Chrome rendering.

## Vignette and Manuscript Contract

`vignettes/hla_tcr_antigen_selected.Rmd` is the primary provenance, methods,
and biological-results article. `vignettes/hla_tcr_main_case.Rmd` is a secondary
Viewer workflow article.

Both documents read generated JSON/CSV files. Cell counts, donor counts, motif
sizes, sequence names, HLA context, and figure captions use inline R values
rather than duplicated literals. Rendering must fail when a required field or
artifact is absent.

The short `docs/hla-tcr-end-to-end-case.md` page becomes a navigation and
reproduction entry point. It links to the two vignettes and the two top-level
commands instead of maintaining a third copy of the results narrative.

## Independent Verification

Publication tests must not call the publication derivation functions to obtain
their expected biological answers. They independently read the CRB and use the
package's production HLA/TCR core to recompute critical results.

The test suite is organized into three responsibilities:

1. `test-hla-tcr-publication-data.R`
   - validates 12,000 cells, sparse expression, UMAP alignment, four balanced
     donors, paired TRA/TRB, independent HLA typing, and provenance;
   - re-derives the strict ten-cell clonotype and its motif context; and
   - re-derives the secondary 293-cell CTgene selection and motif context.
2. `test-hla-tcr-publication-artifacts.R`
   - validates manifest schemas and checksums;
   - compares JSON, TSV, CSV, and Linked views selections with independently
     derived cells and counts;
   - ensures both vignettes consume generated evidence; and
   - verifies that every referenced figure and screenshot exists with the
     declared dimensions.
3. `test-hla-tcr-publication-browser.R`
   - runs one real Viewer smoke journey;
   - compares the complete restored selection set, not arbitrary first/last
     barcodes; and
   - confirms the HLA/TCR page renders the expected real TRB motif state.

The three current branch-specific test files are replaced by these files.
Repeated literal-only assertions are removed. The CRB is loaded once per test
file, and expensive motif derivations are computed once and reused within that
file.

## Failure and Publication Semantics

- Every output is written under a staging directory first.
- The publisher validates schemas, checksums, selections, figures, and tables
  before replacing any tracked artifact.
- A failed build reports the failing stage and keeps the previous release
  byte-for-byte intact.
- `--verify` regenerates into staging and compares without modifying tracked
  outputs.
- A non-verify publication run performs explicit atomic replacements only after
  the complete staged set passes.
- Unavailable network access affects only `--from-raw`.
- Unavailable Chrome affects only `--screenshots`.

## Acceptance Criteria

The design is complete when all of the following are true:

1. Tier 1 runs offline from a clean checkout and reproduces every tracked
   numerical artifact.
2. Tier 2 rejects altered raw bytes and rebuilds a validated complete Cerebro
   object from the pinned official sources.
3. Both biological cases are independently re-derived from the CRB in tests.
4. Every number shown in the two vignettes comes from generated evidence.
5. Every scientific figure and statistical table is generated by the
   publication command.
6. The Viewer screenshot journey uses the generated configuration and records
   its data and selection fingerprints.
7. The browser test compares the full cell selection and reaches a real TRB
   motif state.
8. Ordinary CI is offline and does not download the 1.6 GB source bundle.
9. The repository precheck, package tests, package check, and pkgdown build pass.
10. No Seurat `.rds`, raw download, transient staging file, or host-specific
    path is committed.

## Explicit Non-Goals

- Persisting or publishing the transient Seurat object.
- Treating dextramer binder calls as validated antigen specificity.
- Inferring donor HLA genotype from dextramer binding.
- Claiming population association or causal HLA restriction from four donors.
- Running the raw-data rebuild in routine CI.
- Pixel-perfect screenshot regression across operating systems.
