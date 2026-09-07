source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/guides/helpers.R"
  ),
  local = TRUE
)

guide_catalogue <- viewerGuideCatalogue()
guide_boxes <- lapply(unique(guide_catalogue$section), function(section) {
  guides <- guide_catalogue[guide_catalogue$section == section, , drop = FALSE]
  box(
    title = section,
    status = "primary",
    solidHeader = TRUE,
    width = 4,
    tags$ul(
      class = "list-unstyled",
      lapply(seq_len(nrow(guides)), function(index) {
        tags$li(
          style = "margin-bottom: 10px;",
          tags$a(
            href = viewerGuideHref(
              guides$slug[[index]],
              Cerebro.options[["cerebro_root"]],
              cerebro_www_prefix
            ),
            target = "_blank",
            rel = "noopener noreferrer",
            guides$title[[index]]
          )
        )
      })
    )
  )
})

tab_guides <- tabItem(
  tabName = "guides",
  fluidRow(
    column(
      12,
      titlePanel("Guides"),
      helpText(
        paste(
          "Open a guide in a new tab.",
          "Bundled pages are used when available; otherwise links open pkgdown."
        )
      )
    )
  ),
  do.call(fluidRow, guide_boxes)
)
