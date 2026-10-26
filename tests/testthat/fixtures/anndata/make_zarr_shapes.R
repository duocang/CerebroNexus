## Second step of make_zarr_fixtures.py: the shapes of small_sd_cells.zarr as a
## parquet file with WKB points, as SpatialData writes them. The coordinates are
## the ones the test expects: the point of instance id i is (10 + i, 200 - 2 * i).
library(arrow)
here <- if (nzchar(Sys.getenv("FIXTURE_DIR"))) Sys.getenv("FIXTURE_DIR") else "."
point_wkb <- function(x, y) {
  c(as.raw(1L), writeBin(1L, raw(), size = 4L, endian = "little"),
    writeBin(c(x, y), raw(), size = 8L, endian = "little"))
}
id <- 0:49
shapes <- data.frame(radius = rep(5, 50), spot_id = id)
shapes$geometry <- I(lapply(id, function(i) point_wkb(10 + i, 200 - 2 * i)))
write_parquet(shapes[, c("geometry", "radius", "spot_id")], file.path(here, "small_sd_cells.zarr", "shapes", "cells", "shapes.parquet"))
cat("shapes written\n")
