# Trekker demo

`demo_trekker_mouse_brain_u_c.crb` is a deterministic subset of the official `Mouse_Brain_TrekkerU_C` example, not a fabricated fixture.

The build keeps up to 40 nuclei from every official cluster (706 nuclei in the current source) and 100 measured RNA genes selected from official cluster-marker results, official Moran rankings, and expression abundance.
It subsets the Location, cluster-marker, and Moran files consistently while preserving the complete run-level summary metrics.

The demo intentionally omits the optional official HTML report because that one file is about 12.9 MB.
The export and app APIs still accept the unchanged report for full datasets.

For the multi-section Viewer example, sampled nuclei are ordered spatially within each official cluster and assigned alternately to synthetic UI sections `slice1` and `slice2`. This keeps both sections balanced and spanning the complete official coordinate field; neither is a vendor-defined biological slice.
Each section has two tiny SVG layers with different colours and shapes.
Their provenance states `Demo image layer; not measured tissue imagery`; they exercise image-layer controls and contain no biological measurement.
They stay in `.trekker/images/`, not inside the CRB.

Build from the package root:

```bash
CEREBRONEXUS_TREKKER_DATA_DIR=/path/to/trekker-official \
  Rscript data-raw/build_trekker_official_demo.R
```

The script writes `inst/extdata/examples/demo_trekker_mouse_brain_u_c.crb` and its sibling `.trekker/` directory, then refuses the build if their combined size reaches 1 MiB.

## Multi-section image API

Section names are arbitrary user-facing identifiers such as `slice1`, `slice2`, or `fov-a`.
Every cell belongs to exactly one section; each section may have any number of ordered image layers.

```r
exportFromSeurat(
  object = object,
  assay = "RNA",
  slot = "counts",
  file = "brain.crb",
  experiment_name = "Brain",
  organism = "mm",
  groups = "cluster",
  trekker_data = "/path/to/official-companions",
  trekker_sections = c(cell_a = "slice1", cell_b = "slice2"),
  trekker_images = list(
    slice1 = list(DAPI = "slice1-dapi.png", IF = "slice1-if.png"),
    slice2 = list(DAPI = "slice2-dapi.png", IF = "slice2-if.png")
  ),
  trekker_image_settings = list(
    slice1 = list(DAPI = list(image_opacity = 0.55, flip_y = TRUE))
  )
)
```

`createShinyApp()` accepts the same three inputs with an outer dataset level when several CRBs are supplied.
It can add image layers or change settings after CRB creation.
Equal `(section, image label)` content is deduplicated by SHA-256; different content raises an error unless that exact identity is listed under `trekker_image_replace`.

```r
createShinyApp(
  cerebro_data = c(brain = "brain.crb"),
  result_dir = "brain-app",
  trekker_images = list(slice2 = list(IF = "replacement-if.png")),
  trekker_image_replace = list(slice2 = "IF")
)
```

Image bytes are never embedded in the CRB.
Exports place them under `<stem>.trekker/images/`; generated apps copy them into their private Trekker data directory.

Launch the bundled Viewer from the package root:

```r
shiny::runApp("inst")
```

The PBMC demo remains the default; select `Mouse brain (TrekkerU C)` from the dataset selector to open the Trekker pages.
