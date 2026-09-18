##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
tab_groups <- tabItem(
  tabName = "groups",
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
  uiOutput("groups_select_group_UI"),
  uiOutput("groups_composition_UI"),
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
      var observer = new IntersectionObserver(function (entries) {
        if (!entries.some(function (entry) { return entry.isIntersecting; })) {
          return;
        }
        Shiny.setInputValue(
          'groups_expression_metrics_render_request',
          Date.now(),
          {priority: 'event'}
        );
        observer.disconnect();
      }, {rootMargin: '200px 0px'});
      observer.observe(target);
    })();
    "
  ))
)
