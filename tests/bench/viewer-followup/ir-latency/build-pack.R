args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
pack <- buildViewerPack(args[[2]], viewer_binary = "always", overwrite = FALSE)
print(validateViewerPack(args[[2]], pack, full = TRUE))
