##----------------------------------------------------------------------------##
## make_readme_flow.R
##
## The two routes from a Seurat object to a running Cerebro app, drawn once and
## emitted in a light and a dark variant for the README's <picture> switch.
##
## One generator rather than two hand-kept files: the variants differ only in a
## palette, and a diagram whose dark version has drifted is worse than no dark
## version at all.
##
## Colours are the viewer's own tokens (inst/shiny/v1.4/www/custom.css :root) --
## amber is the acting accent, blue is structural, and the logo pair already
## uses this text pair.
##
##   Rscript data-raw/make_readme_flow.R
##
##----------------------------------------------------------------------------##

OUT <- "man/figures"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

SANS <- paste(
  "system-ui,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif"
)
MONO <- "ui-monospace,'SF Mono',Menlo,Consolas,monospace"

palettes <- list(
  light = list(
    file = "builder-flow.svg",
    text = "#1c1c1e",
    muted = "#6b6b70",
    faint = "#9a9aa0",
    border = "#dcdce0",
    surface = "#ffffff",
    neutral = "#f4f4f5",
    amber = "#f97316",
    amber_wash = "#fff4ec",
    blue = "#2f6fd6",
    blue_wash = "#eef4fb"
  ),
  dark = list(
    file = "builder-flow-dark.svg",
    text = "#e8e8ea",
    muted = "#a6a6ac",
    faint = "#7c7c84",
    border = "#3c3c42",
    surface = "#212124",
    neutral = "#2a2a2e",
    amber = "#fb923c",
    amber_wash = "#33210f",
    blue = "#6ba3f0",
    blue_wash = "#16233a"
  )
)

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

box <- function(x, y, w, h, fill, stroke, r = 10) {
  sprintf(
    '<rect x="%s" y="%s" width="%s" height="%s" rx="%s" fill="%s" stroke="%s" stroke-width="1.5"/>',
    x,
    y,
    w,
    h,
    r,
    fill,
    stroke
  )
}

txt <- function(
  x,
  y,
  s,
  fill,
  size = 14,
  family = SANS,
  weight = 400,
  anchor = "middle",
  spacing = NULL
) {
  sprintf(
    '<text x="%s" y="%s" fill="%s" font-family="%s" font-size="%s" font-weight="%s" text-anchor="%s"%s>%s</text>',
    x,
    y,
    fill,
    family,
    size,
    weight,
    anchor,
    if (is.null(spacing)) "" else sprintf(' letter-spacing="%s"', spacing),
    esc(s)
  )
}

#' A horizontal arrow. The head is an explicit triangle rather than a
#' <marker>: markers are among the things SVG sanitisers drop, and an arrow
#' that renders everywhere beats one that is tidier in the source.
arrow_h <- function(x1, x2, y, colour) {
  paste0(
    sprintf(
      '<path d="M%s %s H%s" stroke="%s" stroke-width="1.5" fill="none"/>',
      x1,
      y,
      x2 - 7,
      colour
    ),
    sprintf(
      '<path d="M%s %s l-7 -4.2 v8.4 Z" fill="%s"/>',
      x2,
      y,
      colour
    )
  )
}

#' A curved arrow, for the split out of the shared input and the merge back
#' into the shared output.
arrow_c <- function(x1, y1, x2, y2, colour) {
  cx <- (x1 + x2) / 2
  paste0(
    sprintf(
      '<path d="M%s %s C%s %s %s %s %s %s" stroke="%s" stroke-width="1.5" fill="none"/>',
      x1,
      y1,
      cx,
      y1,
      cx,
      y2,
      x2 - 7,
      y2,
      colour
    ),
    sprintf('<path d="M%s %s l-7 -4.2 v8.4 Z" fill="%s"/>', x2, y2, colour)
  )
}

render <- function(p) {
  ## Geometry. Both lanes start and end in the same place on purpose: same
  ## input, same output, different path -- that is the whole point of the
  ## picture, and it only reads if the endpoints are literally shared.
  W <- 1000
  H <- 330
  mid <- 165
  yA <- 87 # interactive lane
  yB <- 243 # two-call lane

  parts <- c(
    ## Declared rather than assumed: the labels carry a middle dot, and a
    ## standalone .svg whose encoding is guessed turns it into a CJK character.
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %s %s" width="%s" role="img" aria-labelledby="flow-title flow-desc">',
      W,
      H,
      W
    ),
    '<title id="flow-title">Two routes from a Seurat object to a Cerebro app</title>',
    paste0(
      '<desc id="flow-desc">',
      esc(paste(
        "Both routes start from Seurat objects and end at a runnable Shiny app.",
        "launchCerebroBuilder() does it in one interactive step.",
        "convertSeuratToCerebro() writes a .crb, which createShinyApp() then bundles."
      )),
      "</desc>"
    ),

    ## -- shared input ------------------------------------------------------
    box(16, 133, 156, 64, p$neutral, p$border),
    txt(94, 160, "Seurat object(s)", p$text, 15, SANS, 600),
    txt(94, 180, ".rds · .qs2 · .qs", p$faint, 12, MONO),

    ## -- lane A: the builder ----------------------------------------------
    arrow_c(172, mid, 248, yA, p$amber),
    txt(
      248,
      38,
      "ONE INTERACTIVE STEP",
      p$amber,
      11,
      SANS,
      700,
      "start",
      "1.4"
    ),
    box(248, 53, 520, 68, p$amber_wash, p$amber),
    txt(508, 82, "launchCerebroBuilder()", p$text, 16, MONO, 600),
    txt(
      508,
      103,
      "reads the object first · preview before writing · several at once",
      p$muted,
      12
    ),

    ## -- lane B: the two calls --------------------------------------------
    arrow_c(172, mid, 248, yB, p$blue),
    txt(248, 194, "TWO CALLS", p$blue, 11, SANS, 700, "start", "1.4"),
    box(248, 209, 214, 68, p$blue_wash, p$blue),
    txt(355, 240, "convertSeuratToCerebro()", p$text, 12.5, MONO, 600),
    txt(355, 259, "arguments by hand", p$muted, 11.5),

    arrow_h(462, 486, yB, p$blue),
    box(486, 227, 72, 32, p$surface, p$border, 6),
    txt(522, 247, ".crb", p$text, 13, MONO, 600),

    arrow_h(558, 582, yB, p$blue),
    box(582, 209, 186, 68, p$blue_wash, p$blue),
    txt(675, 240, "createShinyApp()", p$text, 13, MONO, 600),
    txt(675, 259, "bundle it", p$muted, 11.5),

    ## -- shared output -----------------------------------------------------
    arrow_c(768, yA, 828, mid, p$amber),
    arrow_c(768, yB, 828, mid, p$blue),
    box(828, 133, 156, 64, p$neutral, p$border),
    txt(906, 160, "Runnable app", p$text, 15, SANS, 600),
    txt(906, 180, "every sample, one link", p$faint, 12),

    "</svg>"
  )
  paste(parts, collapse = "\n")
}

for (nm in names(palettes)) {
  p <- palettes[[nm]]
  path <- file.path(OUT, p$file)
  writeLines(render(p), path)
  cat(sprintf(" * %-28s %5.1f KB\n", p$file, file.size(path) / 1024))
}
