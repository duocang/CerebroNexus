# Read a Cerebro data file

Auto-detects legacy RDS, thin RDS, and thin qs2 CRBs. Thin CRBs are
hydrated from their sibling expression sidecar.

## Usage

``` r
readCerebro(file)
```

## Arguments

- file:

  Input `.crb` path.

## Value

A current Cerebro object.
