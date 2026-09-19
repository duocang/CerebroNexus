##----------------------------------------------------------------------------##
## Tab: Trajectory
##----------------------------------------------------------------------------##

tab_trajectory <- tabItem(
  tabName = "trajectory",
  shinyjs::inlineCSS(
    "
    #trajectory_details_selected_cells_table .table th {
      text-align: center;
    }
    #states_by_group_table .table th {
      text-align: center;
    }
    "
  ),
  cerebroVizPageHeader(
    "Trajectory",
    "trajectory_projection_info",
    "Explore inferred cell-state transitions and pseudotime."
  ),
  fluidRow(
    class = "cerebro-viz-row cerebro-viz-top-layout",
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-toolbar-col",
      div(
        class = "cerebro-viz-toolbar",
        div(
          class = "cerebro-viz-primary",
          uiOutput("trajectory_primary_controls_UI")
        ),
        cerebroToolbarActions(
          cerebroSettingsButton(
            "trajectory_projection_more_button",
            "trajectory_projection_more"
          ),
          cerebroShareButton("trajectory_projection")
        ),
        cerebroSettingsDrawer(
          "trajectory_projection_more",
          cerebroSettingsSection(
            "Appearance",
            uiOutput("trajectory_projection_appearance_UI"),
            cerebroInfoButton(
              "trajectory_projection_additional_parameters_info"
            )
          ),
          cerebroSettingsSection(
            "Data",
            uiOutput("trajectory_projection_data_parameters_UI")
          ),
          cerebroSettingsSection(
            "Group filters",
            uiOutput("trajectory_projection_group_filters_UI"),
            cerebroInfoButton("trajectory_projection_group_filters_info")
          )
        ),
        cerebroSelectionStatus(
          "trajectory_projection",
          "trajectory_number_of_selected_cells",
          portable = FALSE
        )
      )
    ),
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-col",
      cerebroCellViewOutput("trajectory_projection")
    )
  ),
  uiOutput("trajectory_selected_cells_table_UI"),
  div(
    id = "trajectory_distribution_section_gate",
    style = "min-height: 1px;",
    uiOutput("trajectory_distribution_along_pseudotime_UI")
  ),
  div(
    id = "trajectory_states_section_gate",
    style = "min-height: 1px;",
    uiOutput("trajectory_states_by_group_UI")
  ),
  div(
    id = "trajectory_expression_section_gate",
    style = "min-height: 1px;",
    uiOutput("trajectory_expression_metrics_UI")
  ),
  tags$script(HTML(
    "
    (function () {
      var gates = [
        ['trajectory_distribution_section_gate',
          'trajectory_distribution_section_visible'],
        ['trajectory_states_section_gate',
          'trajectory_states_section_visible'],
        ['trajectory_expression_section_gate',
          'trajectory_expression_section_visible']
      ];
      var armed = false;
      var projectionReady = false;
      var scrollIntent = false;

      function armSections() {
        if (armed || !projectionReady || !scrollIntent) return;
        armed = true;
        gates.forEach(function (entry) {
          var target = document.getElementById(entry[0]);
          if (!target || target.dataset.observed) return;
          target.dataset.observed = 'true';
          var observer = new IntersectionObserver(function (entries) {
            if (!entries.some(function (item) { return item.isIntersecting; })) {
              return;
            }
            Shiny.setInputValue(entry[1], Date.now(), {priority: 'event'});
            observer.disconnect();
          }, {rootMargin: '100px 0px'});
          observer.observe(target);
        });
      }

      window.addEventListener('cerebro:specialist-state', function (event) {
        var detail = event.detail || {};
        if (detail.viewId !== 'trajectory_projection') return;
        if (detail.eventKind !== 'primary' && detail.eventKind !== 'cached') {
          return;
        }
        projectionReady = true;
        armSections();
      });
      window.addEventListener('wheel', function () {
        scrollIntent = true;
        armSections();
      }, {passive: true});
      window.addEventListener('touchmove', function () {
        scrollIntent = true;
        armSections();
      }, {passive: true});
      window.addEventListener('keydown', function (event) {
        if (!['PageDown', 'End', 'ArrowDown', ' '].includes(event.key)) return;
        scrollIntent = true;
        armSections();
      });
    })();
    "
  ))
)
