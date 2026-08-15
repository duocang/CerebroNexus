# Build a Cerebro data set from a Seurat object, by pointing and clicking.

Opens a local page that reads a Seurat \`.rds\`, works out what it
contains, and lets you choose what the resulting data set should show:
which metadata columns are the grouping variables, which dimensional
reductions to carry, which assay and layer the expression comes from. It
then writes the \`.crb\` and, if asked, packages it into a standalone
Shiny app.

It does the same work as calling
[`exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
yourself. The difference is that it reads the object first and only
offers what is actually there – the layers that exist, the columns that
can serve as groups, the reductions that will survive the export – so
the choices that quietly go wrong in a hand-written call are visible
before anything is written.

## Usage

``` r
launchCerebroBuilder(
  host = "127.0.0.1",
  port = NULL,
  launch_browser = TRUE,
  max_file_size = 8000
)
```

## Arguments

- host:

  Address to bind; defaults to `127.0.0.1`, i.e. this machine only.

- port:

  Port to bind; defaults to a free one chosen by Shiny.

- launch_browser:

  Open a browser; defaults to `TRUE`.

- max_file_size:

  Maximum size in MB for each local upload; defaults to 8000. The
  corresponding Shiny process option is restored when the Builder stops.

## Value

Does not return; runs until the app is stopped.

## What this is not

The object is read into this R process, so run it on your own machine or
in your own RStudio Server session. It is not meant to be deployed for
other people to upload data to: it reads whatever path it is given, and
reading an \`.rds\` runs whatever code is inside it.

## Examples

``` r
if (FALSE) { # \dontrun{
launchCerebroBuilder()
} # }
```
