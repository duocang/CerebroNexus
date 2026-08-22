##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

tab_about <- tabItem(
  tabName = "about",
  fluidRow(
    column(12, titlePanel("About CerebroNexus")),
    column(
      12,
      htmlOutput("about"),
      actionButton("browser", "browser"),
      tags$script("$('#browser').hide();")
    )
  )
)
