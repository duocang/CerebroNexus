##----------------------------------------------------------------------------##
## build_builder_fixtures.R
##
## Inputs for exercising the dataset builder (launchCerebroBuilder()) by hand
## plus the permanent, committed product examples used by the Builder gallery.
##
## These are NOT demo `.crb` files -- those are the builder's OUTPUT, and their
## registry is DATASETS.md. These are Seurat objects and background images that
## go IN, chosen so that between them they hit every path the builder offers:
## every readable format, spatial and non-spatial, one section and several,
## both ways of deciding an image's extent, an object that already carries
## analyses and one that carries none, a table to attach, and one object big
## enough that the worker process is doing visible work.
##
## Everything here is fully synthetic and builds offline in a few seconds. The
## point is coverage of the interface, not biological realism: a fixture that
## needs a 3.5 GB download is a fixture nobody re-generates.
##
## Manual/stress output goes to data-raw/builder_fixtures/ and is gitignored.
## The same run also deterministically overwrites the committed product fixtures
## in inst/builder/fixtures/. Run only from the repository root so neither tree
## can accidentally be written beside some unrelated working directory.
##
##   Rscript data-raw/build_builder_fixtures.R
##
##----------------------------------------------------------------------------##

.builder_fixture_arguments <- commandArgs(trailingOnly = FALSE)
.builder_fixture_file_argument <- grep(
  "^--file=",
  .builder_fixture_arguments,
  value = TRUE
)
if (length(.builder_fixture_file_argument) != 1L) {
  stop("Run this script with Rscript from the repository root.", call. = FALSE)
}
.builder_fixture_script <- normalizePath(
  sub("^--file=", "", .builder_fixture_file_argument),
  mustWork = TRUE
)
.builder_fixture_repo <- normalizePath(
  file.path(dirname(.builder_fixture_script), ".."),
  mustWork = TRUE
)
.builder_fixture_cwd <- normalizePath(getwd(), mustWork = TRUE)
.builder_fixture_description <- file.path(.builder_fixture_repo, "DESCRIPTION")
.builder_fixture_package <- if (file.exists(.builder_fixture_description)) {
  read.dcf(.builder_fixture_description, fields = "Package")[[1L]]
} else {
  ""
}
if (
  !identical(.builder_fixture_cwd, .builder_fixture_repo) ||
    !identical(.builder_fixture_package, "CerebroNexus")
) {
  stop(
    "Run this script from the CerebroNexus repository root: ",
    .builder_fixture_repo,
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
})

sys.source(file.path("inst", "builder", "io.R"), envir = environment())

.builder_fixture_main <- function() {
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) {
    caller_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit(
    {
      if (seed_exists) {
        assign(".Random.seed", caller_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  OUT <- file.path("data-raw", "builder_fixtures")
  dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
  set.seed(2026)

  say <- function(...) cat(" *", ..., "\n")

  ## ---------------------------------------------------------------------------
  ## building blocks
  ## ---------------------------------------------------------------------------

  #' A minimal but complete object: counts, groups, and a UMAP.
  make_object <- function(
    n_cells,
    n_genes = 120,
    samples = "donorA",
    organism = c("hg", "mm")
  ) {
    organism <- match.arg(organism)
    cells <- paste0("cell", seq_len(n_cells))
    ## Gene names decide what the QC and mito/ribo steps can find, so use the
    ## right nomenclature for the organism the fixture claims to be.
    prefix <- if (organism == "hg") {
      c("MT-", "RPS", "RPL")
    } else {
      c("mt-", "Rps", "Rpl")
    }
    genes <- c(
      paste0(prefix[1], c("CO1", "ND1", "CYB")),
      paste0(prefix[2], 3:6),
      paste0(prefix[3], 3:6),
      paste0("Gene", seq_len(n_genes - 11))
    )
    counts <- Matrix::Matrix(
      stats::rpois(length(genes) * n_cells, lambda = 3),
      nrow = length(genes),
      dimnames = list(genes, cells),
      sparse = TRUE
    )
    obj <- CreateSeuratObject(counts = counts)
    obj <- NormalizeData(obj, verbose = FALSE)
    obj$sample <- factor(sample(samples, n_cells, replace = TRUE))
    obj$cell_type <- factor(
      sample(c("Neuron", "Astrocyte", "Microglia"), n_cells, replace = TRUE)
    )
    obj$condition <- factor(sample(c("control", "treated"), n_cells, TRUE))
    emb <- matrix(
      stats::rnorm(n_cells * 2),
      ncol = 2,
      dimnames = list(cells, c("UMAP_1", "UMAP_2"))
    )
    obj[["umap"]] <- CreateDimReducObject(emb, key = "UMAP_")
    obj
  }

  #' Attach one tissue section covering the given cells.
  #'
  #' `origin` places the section in coordinate space; `span` is its extent. Two
  #' sections that overlap are indistinguishable from one section drawn with the
  #' wrong coordinates, so keep them apart.
  add_section <- function(
    obj,
    name,
    cells,
    origin = c(0, 0),
    span = c(100, 80)
  ) {
    coords <- data.frame(
      x = stats::runif(length(cells), 0, span[1]) + origin[1],
      y = stats::runif(length(cells), 0, span[2]) + origin[2],
      cell = cells
    )
    obj[[name]] <- CreateFOV(
      ## CreateCentroids first: a bare coordinate frame loses the cell names and
      ## the assignment fails with "Cannot add new cells with [[<-".
      coords = list(centroids = CreateCentroids(coords)),
      type = "centroids",
      assay = "RNA",
      key = paste0(tolower(name), "_")
    )
    obj
  }

  #' A background that looks like tissue rather than noise, so a wrong alignment
  #' is visible at a glance.
  write_tissue_png <- function(path, width, height, seed = 1) {
    set.seed(seed)
    gx <- matrix(rep(seq_len(width), each = height), nrow = height)
    gy <- matrix(rep(seq_len(height), times = width), nrow = height)
    ## Soft bands plus a vignette: enough structure to see rotation and flips.
    band <- 0.5 + 0.35 * sin(gy / height * 6 * pi) * cos(gx / width * 2 * pi)
    vign <- 1 -
      0.5 * (((gx - width / 2) / width)^2 + ((gy - height / 2) / height)^2) * 4
    vign[vign < 0] <- 0
    r <- pmin(1, band * vign * 1.05)
    g <- pmin(1, band * vign * 0.75)
    b <- pmin(1, band * vign * 0.95)
    png::writePNG(array(c(r, g, b), dim = c(height, width, 3)), path)
    invisible(path)
  }

  #' Save in whichever format the extension asks for, skipping cleanly when the
  #' package that writes it is not installed.
  save_as <- function(obj, path) {
    ext <- tolower(tools::file_ext(path))
    ok <- switch(
      ext,
      rds = {
        saveRDS(obj, path)
        TRUE
      },
      qs2 = if (requireNamespace("qs2", quietly = TRUE)) {
        qs2::qs_save(obj, path)
        TRUE
      } else {
        FALSE
      },
      qs = if (requireNamespace("qs", quietly = TRUE)) {
        qs::qsave(obj, path)
        TRUE
      } else {
        FALSE
      },
      FALSE
    )
    if (!ok) {
      say(sprintf(
        "skipped %s (no writer for .%s installed)",
        basename(path),
        ext
      ))
      return(invisible(NULL))
    }
    invisible(path)
  }

  ## ---------------------------------------------------------------------------
  ## 1. plain scRNA-seq -- no spatial data at all
  ## ---------------------------------------------------------------------------
  cat("\n-- 1. no spatial data --\n")
  save_as(
    make_object(400, samples = c("donorA", "donorB")),
    file.path(OUT, "plain.rds")
  )

  ## ---------------------------------------------------------------------------
  ## 2. formats -- the same object written every way the builder claims to read
  ##
  ## Not .qd: this fixture is how we found out that qs2's qdata format does not
  ## serialise S4 at all. `qd_save()` on a Seurat object warns, writes 38 bytes,
  ## and reads back as NULL -- so a .qd file cannot hold a Seurat object and the
  ## builder no longer offers it.
  ## ---------------------------------------------------------------------------
  cat("\n-- 2. one object, every readable format --\n")
  fmt_obj <- make_object(200)
  for (ext in c("rds", "qs2", "qs")) {
    save_as(fmt_obj, file.path(OUT, paste0("formats.", ext)))
  }

  ## ---------------------------------------------------------------------------
  ## 3. one sample, one tissue section
  ##
  ## The common case, and the one the interface must not complicate.
  ## ---------------------------------------------------------------------------
  cat("\n-- 3. one sample, one section --\n")
  o <- make_object(300, organism = "mm")
  o <- add_section(o, "section1", colnames(o))
  save_as(o, file.path(OUT, "one_section.rds"))
  write_tissue_png(file.path(OUT, "one_section.png"), 300, 240, seed = 1)
  say("one_section.png            300 x 240")

  ## ---------------------------------------------------------------------------
  ## 4. one sample cut into several sections
  ##
  ## Sections from one block: same donor, consecutive cuts, each its own slide.
  ## ---------------------------------------------------------------------------
  cat("\n-- 4. one sample, three sections --\n")
  o <- make_object(300, organism = "mm")
  cells <- colnames(o)
  o <- add_section(o, "cutA", cells[1:100], origin = c(0, 0))
  o <- add_section(o, "cutB", cells[101:200], origin = c(500, 0))
  o <- add_section(o, "cutC", cells[201:300], origin = c(1000, 0))
  save_as(o, file.path(OUT, "one_sample_three_sections.rds"))
  for (i in seq_len(3)) {
    write_tissue_png(
      file.path(OUT, sprintf("cut%s.png", LETTERS[i])),
      300,
      240,
      seed = 10 + i
    )
  }
  say("cutA.png / cutB.png / cutC.png   300 x 240 each")

  ## ---------------------------------------------------------------------------
  ## 5. several samples, several sections, non-trivial mapping
  ##
  ## donorA contributes two sections, donorB one, donorC two -- so the number of
  ## sections is not the number of samples and neither can be inferred from the
  ## other. Nothing in the format stores this link; only the metadata says it.
  ## ---------------------------------------------------------------------------
  cat("\n-- 5. three samples, five sections --\n")
  n <- 500
  o <- make_object(n, samples = "placeholder", organism = "mm")
  plan <- list(
    list(section = "A_rostral", sample = "donorA", n = 100),
    list(section = "A_caudal", sample = "donorA", n = 100),
    list(section = "B_rostral", sample = "donorB", n = 100),
    list(section = "C_rostral", sample = "donorC", n = 100),
    list(section = "C_caudal", sample = "donorC", n = 100)
  )
  o$sample <- factor(rep(vapply(plan, function(p) p$sample, ""), each = 100))
  o$section <- factor(rep(vapply(plan, function(p) p$section, ""), each = 100))
  cells <- colnames(o)
  for (i in seq_along(plan)) {
    idx <- ((i - 1) * 100 + 1):(i * 100)
    o <- add_section(
      o,
      plan[[i]]$section,
      cells[idx],
      origin = c((i - 1) * 500, 0)
    )
  }
  save_as(o, file.path(OUT, "three_samples_five_sections.rds"))
  for (p in plan) {
    write_tissue_png(
      file.path(OUT, paste0(p$section, ".png")),
      300,
      240,
      seed = nchar(p$section) * 7
    )
  }
  say("one PNG per section, 300 x 240")

  ## ---------------------------------------------------------------------------
  ## 6. coordinates that ARE image pixels
  ##
  ## The builder offers three ways to decide an image's extent. This fixture is
  ## the one where "the cells are already in image pixels" is literally true, so
  ## that mode can be checked against a case where it is right -- on any other
  ## fixture it silently puts the image at [0, image width].
  ## ---------------------------------------------------------------------------
  cat("\n-- 6. coordinates in image pixels --\n")
  o <- make_object(250, organism = "mm")
  o <- add_section(
    o,
    "pixels",
    colnames(o),
    origin = c(0, 0),
    span = c(600, 400)
  )
  save_as(o, file.path(OUT, "pixel_coords.rds"))
  write_tissue_png(file.path(OUT, "pixel_coords.png"), 600, 400, seed = 5)
  say(
    "pixel_coords.png           600 x 400 -- matches the coordinate span exactly"
  )

  ## ---------------------------------------------------------------------------
  ## 7. an object that already carries analyses
  ##
  ## The "already in this object" pills and the pre-export analysis card both
  ## branch on what is present, so one fixture has to arrive with results.
  ## ---------------------------------------------------------------------------
  cat("\n-- 7. arrives with marker genes --\n")
  o <- make_object(200, organism = "hg")
  markers <- do.call(
    rbind,
    lapply(levels(o$cell_type), function(ct) {
      data.frame(
        cell_type = ct,
        gene = paste0("Gene", sample.int(100, 12)),
        p_val = stats::runif(12, 0, 0.01),
        avg_log2FC = stats::rnorm(12, 1, 0.5),
        p_val_adj = stats::runif(12, 0, 0.05),
        stringsAsFactors = FALSE
      )
    })
  )
  o@misc$marker_genes <- list(cerebro_seurat = list(cell_type = markers))
  save_as(o, file.path(OUT, "with_markers.rds"))

  ## ---------------------------------------------------------------------------
  ## 8. a table to attach as supplementary material
  ## ---------------------------------------------------------------------------
  cat("\n-- 8. supplementary table --\n")
  de <- data.frame(
    gene = paste0("Gene", 1:40),
    log2FC = round(stats::rnorm(40, 0, 1.5), 3),
    p_adj = signif(stats::runif(40, 0, 0.05), 3),
    cluster = sample(c("Neuron", "Astrocyte", "Microglia"), 40, TRUE)
  )
  utils::write.csv(de, file.path(OUT, "de_results.csv"), row.names = FALSE)
  say(sprintf("de_results.csv             %d rows", nrow(de)))

  ## ---------------------------------------------------------------------------
  ## 9. large enough to watch the worker
  ##
  ## Small objects load faster than the eye can see, which makes it impossible to
  ## tell whether the work really left the Shiny process. This one takes long
  ## enough to watch the progress indicator and confirm the page still answers.
  ## ---------------------------------------------------------------------------
  cat("\n-- 9. big enough to see the worker working --\n")
  save_as(
    make_object(
      15000,
      n_genes = 400,
      samples = c("donorA", "donorB", "donorC")
    ),
    file.path(OUT, "big.rds")
  )

  ## ---------------------------------------------------------------------------
  ## 10. the other modalities -- immune repertoire, HLA typing, Trekker
  ##
  ## These do not arrive through builder controls: they are already on the object
  ## when the user points at it, and the builder's job is to carry them through
  ## without being told. Two different mechanisms, and the fixture has to cover
  ## both, because only one of them is automatic:
  ##
  ##   * immune repertoire and HLA typing are read straight off `@misc` by
  ##     exportFromSeurat();
  ##   * `@misc$trekker` is NOT -- exportFromSeurat never looks at it, so it has
  ##     to be written into the `.crb` afterwards, the same read-modify-write the
  ##     histology background uses.
  ##
  ## Shapes are copied from the shipped demos (demo_hla_tcr_dextramer.crb,
  ## demo_trekker.crb), not invented, so a fixture that passes means the real
  ## thing passes.
  ## ---------------------------------------------------------------------------
  cat("\n-- 10. immune repertoire + HLA + Trekker --\n")

  modal <- make_object(300, samples = c("donor1", "donor2"))
  cells <- colnames(modal)

  ## Immune repertoire: one data.frame per sample, keyed by barcode. scRepertoire
  ## column names -- the viewer's clonal pages look for exactly these.
  ir_for <- function(barcodes) {
    n <- length(barcodes)
    v <- sprintf("TRAV%d", sample(1:30, n, TRUE))
    j <- sprintf("TRAJ%d", sample(1:60, n, TRUE))
    cdr3 <- vapply(
      seq_len(n),
      function(i) {
        paste0(
          "CA",
          paste(
            sample(strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]], 9, TRUE),
            collapse = ""
          ),
          "F"
        )
      },
      character(1)
    )
    data.frame(
      barcode = barcodes,
      CTgene = paste(v, j, sep = "."),
      CTnt = strrep("ACG", 12),
      CTaa = cdr3,
      CTstrict = paste(v, cdr3, sep = "_"),
      stringsAsFactors = FALSE
    )
  }
  modal@misc$immune_repertoire <- lapply(
    split(cells, as.character(modal$sample)),
    ir_for
  )

  ## HLA typing: the canonical long table -- one row per sample x locus x copy.
  modal@misc$hla_typing <- do.call(
    rbind,
    lapply(
      c("donor1", "donor2"),
      function(s) {
        do.call(
          rbind,
          lapply(c("A", "B", "C"), function(locus) {
            data.frame(
              sample = s,
              donor_id = s,
              locus = paste0("HLA-", locus),
              copy = 1:2,
              allele = sprintf(
                "HLA-%s*%02d:%02d",
                locus,
                sample(1:60, 2),
                sample(1:20, 2)
              ),
              resolution = "4-digit",
              stringsAsFactors = FALSE
            )
          })
        )
      }
    )
  )
  modal@misc$hla_typing_source_type <- "synthetic"

  ## Trekker: the physical map plus the per-cell arrays its page draws.
  ##
  ## The nested shapes matter and are not guessable -- the page indexes into
  ## them. `moran` is a list of records each carrying `$gene`, not a named vector
  ## of coefficients; a field is `list(v, max, label, desc, by_type)`, not a bare
  ## vector; and `qc` is a flat block of 23 named scalars that the page formats
  ## one by one. Getting any of these wrong does not degrade the page, it throws.
  ## Every shape below was read off demo_trekker.crb.
  n_trek <- length(cells)
  types <- levels(modal$cell_type)

  trek_field <- function(label, desc) {
    list(
      ## Stored as 0-255 bytes with the real scale in `max`, which is how the
      ## demo keeps a per-cell field small.
      v = as.integer(round(stats::runif(n_trek, 0, 255))),
      max = 1,
      label = label,
      desc = desc,
      by_type = lapply(types, function(ty) {
        list(
          type = ty,
          n = sum(modal$cell_type == ty),
          median = round(stats::runif(1, 0.05, 0.9), 3),
          dispersed = round(stats::runif(1, 0.5, 1), 3)
        )
      })
    )
  }

  modal@misc$trekker <- list(
    meta = list(
      n_cells_full = n_trek,
      n_cells = n_trek,
      n_genes_obj = nrow(modal),
      unit = "um (synthetic)",
      coord_source = "build_builder_fixtures.R (synthetic)",
      r = R.version.string,
      seurat = as.character(utils::packageVersion("Seurat")),
      generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ),
    ## All 23 keys: the page formats each one, and a missing key is a crash
    ## rather than a blank.
    qc = list(
      sample_id = "synthetic",
      assay = "SyntheticU",
      tile_id = "T0001_001",
      eps = "50",
      min_sb = "5",
      total_nuclei = n_trek,
      in_lib = n_trek,
      pct_in_lib = 100,
      pct_valid_sb = 100,
      positioned = round(n_trek * 0.85),
      pct_positioned = 85,
      conf = round(n_trek * 0.62),
      pct_conf = 62.4,
      pct_2plus = 22.6,
      o_1 = round(n_trek * 0.6),
      salv_2 = 20,
      salv_3 = 8,
      pct_salv = 3.6,
      n_0 = 12,
      n_1 = round(n_trek * 0.6),
      n_2 = 40,
      n_3 = 15,
      n_4p = 5
    ),
    barcodes = cells,
    x = round(stats::runif(n_trek, 0, 1000), 1),
    y = round(stats::runif(n_trek, 0, 800), 1),
    ux = round(stats::rnorm(n_trek), 3),
    uy = round(stats::rnorm(n_trek), 3),
    clusters = as.character(modal$cell_type),
    celltype = as.character(modal$cell_type),
    fields = list(
      spatial_purity = trek_field(
        "Spatial purity",
        "Synthetic: fraction of nearest physical neighbours sharing a cell type."
      ),
      position_confidence = trek_field(
        "Position confidence",
        "Synthetic: how firmly each nucleus was placed."
      )
    ),
    conf = round(stats::runif(n_trek, 0.2, 1), 3),
    ## Records, not coefficients -- the page reads `$gene` off each one.
    moran = lapply(seq_len(8), function(i) {
      list(rank = i, gene = paste0("Gene", i), I = round(0.4 / i, 3))
    }),
    ## Each is a cell with a thumbnail. No images in a synthetic fixture, so the
    ## list is empty rather than carrying entries with a missing `img`.
    evidence = list(),
    qc_examples = list()
  )

  save_as(modal, file.path(OUT, "all_modalities.rds"))
  say(sprintf(
    "all_modalities.rds        IR %d samples, HLA %d rows, Trekker %d cells",
    length(modal@misc$immune_repertoire),
    nrow(modal@misc$hla_typing),
    n_trek
  ))

  cat("\nfixtures written to", normalizePath(OUT), "\n")
  cat("point the builder's file browser at that directory.\n")

  ## ---------------------------------------------------------------------------
  ## Permanent product examples
  ## ---------------------------------------------------------------------------
  PERMANENT <- file.path("inst", "builder", "fixtures")
  builder_write_permanent_fixtures(PERMANENT)
  cat(
    "permanent product examples written to",
    normalizePath(PERMANENT),
    "\n"
  )
}

.builder_fixture_main()
