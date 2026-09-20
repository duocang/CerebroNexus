##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
groups_plotly_dependencies <- local({
  empty_plot <- plotly::plot_ly(
    x = numeric(),
    y = numeric(),
    type = "bar"
  )
  dependencies <- htmltools::renderTags(empty_plot)$dependencies
  htmltools::attachDependencies(
    tags$span(style = "display: none;", `aria-hidden` = "true"),
    dependencies
  )
})

tab_groups <- tabItem(
  tabName = "groups",
  groups_plotly_dependencies,
  shinyjs::inlineCSS(
    "
    #groups_by_other_group_table .table th {
      text-align: center;
    }
    #groups_by_cell_cycle_table .table th {
      text-align: center;
    }
    "
  ),
  uiOutput("groups_controls_UI"),
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Composition by other group"),
        cerebroInfoButton("groups_by_other_group_info")
      ),
      tagList(
        plotly::plotlyOutput("groups_by_other_group_plot"),
        conditionalPanel(
          condition = "input.groups_by_other_group_show_table === true",
          DT::dataTableOutput("groups_by_other_group_table")
        )
      )
    )
  ),
  div(
    id = "groups_expression_metrics_gate",
    style = "min-height: 1px;",
    uiOutput("groups_expression_metrics_UI")
  ),
  tags$script(HTML(
    "
    (function () {
      var target = document.getElementById('groups_expression_metrics_gate');
      if (!target || target.dataset.observed) return;
      target.dataset.observed = 'true';
      var visible = false;

      function requestMetrics() {
        if (!visible || target.dataset.requested) return;
        target.dataset.requested = 'true';
        Shiny.setInputValue(
          'groups_expression_metrics_render_request',
          Date.now(),
          {priority: 'event'}
        );
        observer.disconnect();
      }

      var observer = new IntersectionObserver(function (entries) {
        visible = entries.some(function (entry) { return entry.isIntersecting; });
        requestMetrics();
      }, {rootMargin: '200px 0px'});

      observer.observe(target);
    })();
    "
  ))
)
