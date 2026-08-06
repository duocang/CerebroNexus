<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="man/figures/logo-dark.svg">
    <img src="man/figures/logo.svg" alt="CerebroNexus" width="380">
  </picture>
</p>

[![R-CMD-check (upstream)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml/badge.svg)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)


CerebroNexus is a [Shiny](https://shiny.posit.co/) platform for exploring and sharing single-cell and spatial transcriptomics data, with gene-expression, immune-repertoire, trajectory, and HLA-TCR analyses. See the [full documentation](https://mihem.github.io/CerebroNexus/).

[Try the live demo](https://osmzhlab.uni-muenster.de/shiny/demo/).

*CerebroNexus began as a fork of [cerebroApp](https://github.com/romanhaa/cerebroApp) by Roman Hillje and has since evolved with substantial new features and active development by [mihem](https://github.com/mihem) and [Xuesong Wang](https://github.com/duocang).*

Automated tests run in a reproducible Nix environment.

![CerebroNexus spatial data view](man/figures/featured.png)

## 1. Installation

```r
remotes::install_github('mihem/CerebroNexus')
```

## 2. Quick Start

```r
library(CerebroNexus)

convertSeuratToCerebro(
  seurat_file = "my_seurat.rds",
  result_dir = "output",
  groups = c("sample", "cluster")
)

createShinyApp(
  cerebro_data = c("My dataset" = "output/cerebro_my_seurat.crb"),
  result_dir = "my_app"
)
```

## 3. Expression backend benchmark

CerebroNexus 4.1 includes a reproducible real-data benchmark for choosing among
the `embedded`, `bpcells`, and `h5` expression backends. On the recorded host
and workloads, BPCells exported fastest and reached the lowest ready-state RSS,
H5 produced the smallest bundles and lowest single-gene latency, and embedded
storage traded higher startup memory for a single self-contained CRB.

Read the illustrated
[Choosing an expression backend](vignettes/expression_backend_benchmark.Rmd)
guide for the complete tables, all six figures, deployment guidance, and
interpretation limits. The underlying immutable evidence is retained in the
[`20260805T214336Z-02ff3f0a971e-publication`](https://github.com/duocang/CerebroNexus/blob/benchmark-evidence-20260805-02ff3f0/tools/bench/result/runs/20260805T214336Z-02ff3f0a971e-publication/summary.md)
run, measured with CerebroNexus 3.2.0 and retained without relabelling its
historical environment.

## License

MIT, see [LICENSE.md](LICENSE.md). 
