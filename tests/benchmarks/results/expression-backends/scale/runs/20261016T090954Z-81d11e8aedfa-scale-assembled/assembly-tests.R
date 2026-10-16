source('tests/benchmarks/expression-backends/scripts/benchmark.R')
source('tests/benchmarks/expression-backends/scripts/assembled_scale.R')
stage <- '/mnt/c/Users/wangx/AppData/Local/Temp/cerebro-scale-assembled-20261016/20261016T090954Z-81d11e8aedfa-scale-assembled'
bench_validate_assembled_scale(stage)
scratch <- tempfile('assembly-integrity-')
dir.create(scratch)
stopifnot(all(file.copy(list.files(stage,full.names=TRUE),scratch,recursive=TRUE)))
expect_rejected <- function() {
  result <- tryCatch({bench_validate_assembled_scale(scratch); FALSE}, error=function(e) TRUE)
  stopifnot(result)
}
path <- file.path(scratch,'10_export.csv')
x <- read.csv(path); x$total_mb[1] <- x$total_mb[1]+1
write.csv(x,path,row.names=FALSE)
expect_rejected()
cat('PASS: modified assembled measurement is rejected by inventory\n')
# Even if a caller recomputes the outer checksum, the original input values
# must still match. Merely updating a checksum must not certify edited data.
invpath <- file.path(scratch,'assembly_inventory.csv')
inv <- read.csv(invpath); row <- inv$path=='10_export.csv'
inv$bytes[row] <- file.info(path)$size
inv$sha256[row] <- digest::digest(file=path,algo='sha256')
write.csv(inv,invpath,row.names=FALSE)
expect_rejected()
cat('PASS: changed value is rejected against original acquisition after rehash\n')
# Scratch is owned by this test and is outside result directories.
unlink(scratch,recursive=TRUE)
cat('PASS: assembly positive and negative checks complete\n')
