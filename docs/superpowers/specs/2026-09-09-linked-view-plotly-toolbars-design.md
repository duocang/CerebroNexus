# Linked-view Plotly Toolbars

## Goal

Resolve [#86](https://github.com/mihem/CerebroNexus/issues/86) by making the
toolbars on Groups and non-Clonal-UMAP Immune Repertoire plots use the visual
language already established by Linked views.

## Scope

- Keep Clonal UMAP unchanged because it already uses the Linked views renderer.
- Apply one shared Plotly toolbar configuration to Groups and the remaining
  interactive Immune Repertoire plots.
- Keep each plot's existing data, rendering, hover, selection, and download
  behavior unchanged.
- Do not redesign plot content, panel headers, tabs, or application navigation.

## Design

The existing `cv-panebar` is the visual reference: compact rounded buttons,
neutral gray controls, an orange active state, and a bordered vertical stack
when space is narrow. Native Plotly modebars will retain Plotly's tested actions
and SVG icons, but shared CSS will give them the same dimensions, spacing,
colors, borders, and active/disabled states.

One generic R helper will replace the Immune Repertoire-only modebar helper and
will also wrap the shared Groups Plotly builders. It will hide the Plotly logo
and remove the same clutter controls already removed from Immune Repertoire.
Plot-specific unsupported actions remain absent; the UI will not display inert
selection controls merely to make button counts identical.

Plot containers will use CSS inline-size containment. The modebar is horizontal
by default and switches to a vertical stack below 420 px, matching the existing
Linked views narrow-panel threshold. No resize JavaScript or new dependency is
needed.

## Verification

- A focused R contract test will verify that Groups and Immune Repertoire use
  the shared modebar configuration.
- A `shinytest2` browser regression will verify horizontal layout on a wide
  plot, vertical layout below 420 px, Linked views-derived button styling, and
  the absence of the Plotly logo.
- Run the focused toolbar and affected Groups/Immune Repertoire tests first,
  then the repository precheck required before the PR.
