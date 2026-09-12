# Save a Cerebro data file

Saves a Cerebro object as a thin CRB when it uses a BPCells sidecar. The
input object and expression sidecar are not modified.

## Usage

``` r
saveCerebro(object, file, codec = c("qs2", "rds"))
```

## Arguments

- object:

  A Cerebro object.

- file:

  Output `.crb` path.

- codec:

  Serialization codec. Defaults to `"qs2"`; use `"rds"` when direct
  compatibility with [`readRDS()`](https://rdrr.io/r/base/readRDS.html)
  is required.

## Value

The output path, invisibly.
