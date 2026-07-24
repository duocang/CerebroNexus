## Render the package vignettes to formatted, self-contained HTML tutorials that
## the Shiny app serves in-app at /cerebro_docs (see inst/app.R addResourcePath).
## Code is NOT executed (formatting only), so no analysis packages are needed.
## Features: numbered sections, sticky left TOC with scroll-spy highlight, an
## orange-accent theme + branded pipeline banner (scripts/tutorial-assets/).
suppressWarnings(suppressMessages({
  library(rmarkdown)
  library(knitr)
}))

author_name <- "Xuesong Wang"
assets <- normalizePath("scripts/tutorial-assets", mustWork = TRUE)
css <- file.path(assets, "custom.css")
banner <- file.path(assets, "header.html")
collapse_js <- file.path(assets, "collapse.html")

outdir <- normalizePath("inst/shiny/v1.4/www/tutorials", mustWork = FALSE)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

vigs <- sub("\\.Rmd$", "", list.files("vignettes", pattern = "\\.Rmd$"))

ok <- character(0)
fail <- character(0)

for (v in vigs) {
  inp <- file.path("vignettes", paste0(v, ".Rmd"))
  ## rewrite the author in a temp copy (kept in vignettes/ so relative image
  ## paths still resolve); the 2026 edits are maintained by Xuesong Wang.
  raw <- readLines(inp, warn = FALSE)
  ai <- grep("^author\\s*:", raw)
  if (length(ai)) {
    raw[ai[1]] <- paste0('author: "', author_name, '"')
  }
  tmp <- file.path("vignettes", paste0(".tmp_render_", v, ".Rmd"))
  writeLines(raw, tmp)
  res <- tryCatch(
    {
      knitr::opts_chunk$set(
        eval = FALSE,
        message = FALSE,
        warning = FALSE,
        error = TRUE
      )
      knitr::opts_hooks$set(eval = function(options) {
        options$eval <- FALSE
        options
      })
      rmarkdown::render(
        input = tmp,
        output_format = rmarkdown::html_document(
          ## NOT self-contained: bootstrap/jquery/tocify/fonts (~1.1 MB) would be
          ## base64-embedded into EVERY file, so 23 tutorials ballooned to ~31 MB
          ## of duplicated library code. Instead write the libraries ONCE to a
          ## shared site_libs/ dir and reference them relatively. The app serves
          ## the whole tutorials dir, so the relative refs resolve in-app.
          self_contained = FALSE,
          lib_dir = "site_libs",
          toc = TRUE,
          toc_depth = 3,
          toc_float = list(
            collapsed = TRUE,
            smooth_scroll = TRUE,
            print = FALSE
          ),
          number_sections = TRUE,
          theme = "flatly",
          highlight = "tango",
          css = css,
          includes = rmarkdown::includes(
            before_body = banner,
            after_body = collapse_js
          )
        ),
        output_file = paste0(v, ".html"),
        ## render inside vignettes/ so figures/, ../data-raw/ and the shared
        ## site_libs/ lib_dir all resolve; the outputs are moved to `outdir`
        ## after the loop.
        output_dir = "vignettes",
        quiet = TRUE,
        envir = new.env()
      )
      "ok"
    },
    error = function(e) conditionMessage(e)
  )
  unlink(tmp)
  if (identical(res, "ok")) {
    ok <- c(ok, v)
  } else {
    fail <- c(fail, paste0(v, " :: ", substr(gsub("\n", " ", res), 1, 140)))
  }
}

## Move the rendered HTML + the shared library dir from vignettes/ into the
## served dir, and copy the (relatively-referenced) figures alongside.
for (v in ok) {
  file.rename(
    file.path("vignettes", paste0(v, ".html")),
    file.path(outdir, paste0(v, ".html"))
  )
}
if (dir.exists("vignettes/site_libs")) {
  unlink(file.path(outdir, "site_libs"), recursive = TRUE)
  file.rename("vignettes/site_libs", file.path(outdir, "site_libs"))
}
if (dir.exists("vignettes/figures")) {
  dir.create(file.path(outdir, "figures"), showWarnings = FALSE)
  file.copy(
    list.files("vignettes/figures", full.names = TRUE),
    file.path(outdir, "figures"),
    overwrite = TRUE
  )
}

## html_document(css=) only copies the file into site_libs/ when given a
## RELATIVE path; `css` above is absolute (normalizePath()), so pandoc instead
## links to it as-is -- an absolute filesystem path baked into every <link
## href>. That 404s once served over HTTP by the Shiny app (addResourcePath),
## so the whole .cb-* theme (fold carets, chip badges, code-toggle bars, the
## banner) silently never loads in the running app, even though it renders
## fine when opened straight from disk. Fix: copy custom.css into the shared
## site_libs/ dir and rewrite every rendered file's <link> to the relative
## path that actually resolves in-app.
lib_css_dir <- file.path(outdir, "site_libs")
dir.create(lib_css_dir, showWarnings = FALSE, recursive = TRUE)
file.copy(css, file.path(lib_css_dir, "custom.css"), overwrite = TRUE)
for (v in ok) {
  f <- file.path(outdir, paste0(v, ".html"))
  html <- readLines(f, warn = FALSE)
  html <- gsub(
    paste0('href="', css, '"'),
    'href="site_libs/custom.css"',
    html,
    fixed = TRUE
  )
  writeLines(html, f)
}

cat("\n==== RENDERED OK (", length(ok), ") ====\n", sep = "")
cat(paste(ok, collapse = "\n"), "\n")
cat("\n==== FAILED (", length(fail), ") ====\n", sep = "")
cat(paste(fail, collapse = "\n"), "\n")
