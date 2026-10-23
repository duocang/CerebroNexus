# Attribution — CosMx SMI NSCLC demo

`run-demo.R --cosmx` builds a Viewer data set from NanoString's public CosMx SMI non-small-cell lung cancer (NSCLC) FFPE data set. **No data file is part of this repository or of the installed package**: the script downloads the sources into the user cache and builds the `.crb` and `.h5` files there.

## Source

- Data set page: <https://www.brukerspatialbiology.com/products/cosmx-spatial-molecular-imager/ffpe-dataset/nsclc-ffpe-dataset/> (formerly on nanostring.com). The page describes it as an open-source data set of eight FFPE NSCLC tissue sections from five tissues, measured with a 960-plex RNA panel on a prototype of the CosMx SMI instrument.
- Paper: He S, Bhatt R, Brown C, et al. *High-plex imaging of RNA and proteins at subcellular resolution in fixed tissue by spatial molecular imaging.* **Nature Biotechnology** 40:1794–1806 (2022). <https://doi.org/10.1038/s41587-022-01483-z>
- Files: one archive per section in the public bucket `https://nanostring-public-share.s3.us-west-2.amazonaws.com/SMI-Compressed/<section>/<section> SMI Flat data.tar.gz`. The script checks size and MD5 (NanoString publishes the MD5 sums in `SMI-Compressed/md5sums.txt`) after the download.

| Section | Bytes | MD5 |
|---|---|---|
| Lung5_Rep1 | 1,574,011,761 | `58d02b183fb2446c8c6f9b5c15ba1321` |
| Lung5_Rep2 | 1,631,848,310 | `06c17753c2f7a545beafd8e777ba756a` |
| Lung5_Rep3 | 1,410,950,200 | `26676775c1ca5970bfe522e3f8ed136a` |
| Lung6 | 1,523,509,181 | `730341a340f5978ce09beb0e36b1ae6d` |
| Lung9_Rep1 | 1,078,783,664 | `0c9ff318a6f72625e40381cc0d10504c` |
| Lung9_Rep2 | 1,769,589,563 | `016924767fb4d73e117d6a43aa45ec7f` |
| Lung12 | 1,179,692,183 | `3e2fe4ffeb60fc2ad0a8bf49c6a9f093` |
| Lung13 | 1,143,788,384 | `123aacbc67d0709a235b324b6642c129` |

Only two files of each archive are used: `<section>_exprMat_file.csv` and `<section>_metadata_file.csv`. The archive's images and the multi-gigabyte transcript table are not unpacked.

## Licence and redistribution

The data set page states **no licence**. It carries NanoString's notices about the use of the instrument and probes (including patent notices). The data are therefore treated as *available for use, redistribution terms not stated*: they are downloaded by each user from NanoString and are not committed to this repository. Check the data set page for the current terms before sharing prepared files derived from them.

## What the Viewer data set contains, and what was changed

- **Cells.** All cells of the eight sections, as segmented by NanoString; each section is one spatial entry and its cells keep the global pixel coordinates of the slide (the y axis is mirrored so the section stands upright). The segmentation outlines are not used.
- **Expression.** The 960 genes of the panel. The 20 negative-control probes are not genes; their total per cell is the column `neg_probe_counts`. The stored matrix is `log1p` of the counts per 10,000 per cell, so the Viewer's "log-normalised expression" pages apply; `nUMI` and `nGene` come from the raw counts. Transcripts that are not assigned to a cell (`cell_ID` 0) are dropped.
- **Groups.** `sample` (the section), `tissue` (the section without its replicate suffix; Lung5 has three sections, Lung9 two) and `fov` (the field of view). The flat files carry no cell-type labels.
- **Embedding.** NanoString publishes no embedding with the flat files. The `umap` projection is **computed here**: 30 principal components of the log-normalised, scaled genes, then UMAP.
