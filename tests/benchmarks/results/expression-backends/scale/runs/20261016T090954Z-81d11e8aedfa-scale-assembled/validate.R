#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
stage <- normalizePath(if(length(args)) args[1] else getwd())
source(file.path(stage,'code/benchmark.R'))
source(file.path(stage,'code/assembled_scale.R'))
bench_validate_assembled_scale(stage)
