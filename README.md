<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="man/figures/logo-dark.svg">
    <img src="man/figures/logo.svg" alt="CerebroNexus" width="380">
  </picture>
</p>

[![R-CMD-check (upstream)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml/badge.svg)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)


**CerebroNexus** (**Ce**ll**Re**port**Bro**wser **Nexus**) is a [Shiny](https://shiny.posit.co/) platform for exploring and sharing single-cell and spatial transcriptomics data, with gene-expression, immune-repertoire, trajectory, and HLA-TCR analyses. See the [full documentation](https://mihem.github.io/CerebroNexus/).

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

### Optional large examples

Large examples are downloaded and converted only when explicitly requested.
The prepared Seurat and CRB files are cached outside the package and reused on
later runs:

```r
# About 119 MB of source downloads; prepares exactly 50,000 PBMCs.
launchCerebro(extra_example_data = "50k")

# About 4.2 GB of source downloads; prepares exactly 1,000,000 mouse-brain cells.
createShinyApp(
  cerebro_data = NULL,
  extra_example_data = "1m",
  result_dir = "large_demo",
  launch_browser = FALSE
)
```

Both modes require Seurat, SeuratObject, and BPCells. Seurat's sketch workflow
creates the UMAP and clusters. The sources are public 10x Genomics datasets; no
large data file is stored in this Git repository. Allow roughly 1 GB of cache
space for 50K and 11 GB for 1M after preparation.

## License

MIT, see [LICENSE.md](LICENSE.md). 
