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

## 2. Run the complete large-data demo

```bash
git clone https://github.com/mihem/CerebroNexus.git
cd CerebroNexus
Rscript -e "remotes::install_local('.', dependencies = TRUE)"
Rscript run-demo.R
```

The first run downloads and prepares both the official 10x one-million-neuron
matrix and the Ren et al. [COVID-19 immune atlas](https://explore.data.humancellatlas.org/projects/5f607e50-ba22-4598-b1e9-f3d9d7a35dcc)
([GEO GSE158055](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE158055);
1,462,702 cells). Source and prepared files are cached outside the repository,
so later runs reuse them. The initial compressed downloads total about 18 GB.
Allow at least 100 GB of free cache space for the extracted matrices and
BPCells artifacts.
The Ren demo includes all 64 published cell populations, sample/patient
clinical metadata, and the published 220,968 paired-TCR plus 282,464 paired-BCR
cells.

The 10x marker-guided E18 neurogenesis path excludes endothelial,
microglial, and oligodendrocyte programs, orders the remaining neural lineage
with early, transitional, and late markers, and separates excitatory and
inhibitory terminal branches. It is clearly labelled as a marker-guided demo
rather than a formal trajectory-inference result. The same app also includes
the bundled PBMC, immune-repertoire, trajectory, spatial, Trekker, and HLA/TCR
demos.

## 3. Build an app for your own data

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

## License

MIT, see [LICENSE.md](LICENSE.md). 
