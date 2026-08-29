# Create a self-contained CerebroNexus Shiny app folder

Bundles a CerebroNexus Shiny app into `result_dir`, copying the
`inst/viewer/` sources, the requested `.crb` data file(s), and
`extdata/`, and writes an `app.R` that sources the bundled UI/server.
The output directory can be served directly by shiny-server or run with
`shiny::runApp(result_dir)`.

## Usage

``` r
createShinyApp(
  cerebro_data,
  result_dir = NULL,
  max_request_size = 8000,
  port = 8080,
  host = "127.0.0.1",
  launch_browser = TRUE,
  quiet = FALSE,
  display_mode = "normal",
  colors = NULL,
  cerebro_options = list(exclude_trivial_metadata = TRUE),
  overwrite = TRUE,
  verbose = TRUE,
  crb_pick_smallest_file = TRUE,
  show_upload_ui = FALSE,
  initial_page = NULL,
  welcome_message = "Welcome to CerebroNexus!",
  point_size = list(overview_projection_point_size = NULL),
  variable_to_compare = NULL,
  spatial_images = NULL,
  spatial_image_settings = NULL,
  spatial_images_flip_x = NULL,
  spatial_images_flip_y = NULL,
  spatial_images_scale_x = NULL,
  spatial_images_scale_y = NULL,
  spatial_images_offset_x = NULL,
  spatial_images_offset_y = NULL,
  spatial_plot_rotation = NULL,
  auth = NULL,
  extra_tables = NULL,
  extra_tables_sheets = NULL,
  admin_account = NULL,
  admin_password_env = NULL,
  ...
)
```

## Arguments

- cerebro_data:

  Non-empty named character vector or list of `.crb` (or `.rds`) file
  paths. Names must be non-missing and unique and are used as dataset
  labels. Every path must resolve to a distinct canonical source file.

- result_dir:

  Output directory. Its basename must be portable, its path must not use
  the reserved build-lock namespace, and its final target must not be a
  symbolic link or unresolved filesystem entry.

- max_request_size:

  One finite positive numeric upload limit in MB; defaults to 8000.
  Closed Viewers cap the effective request size at 6 MB; open Viewers
  use the supplied value.

- port:

  One whole-number port from 1 through 65535; defaults to 8080.

- host:

  One non-empty character value that the generated app binds to;
  defaults to "127.0.0.1".

- launch_browser:

  One non-missing logical controlling whether to launch a browser;
  defaults to TRUE.

- quiet:

  One non-missing logical passed to
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html); defaults
  to FALSE.

- display_mode:

  Exactly one of `"auto"`, `"normal"`, or `"showcase"`; defaults to
  `"normal"`.

- colors:

  Optional named list of colour palettes per dataset.

- cerebro_options:

  Extra entries merged into `Cerebro.options` in the generated app.
  Matrix overrides must be absolute host paths and make the resulting
  app host-dependent. Native paths must resolve outside `result_dir`;
  non-native paths cannot be compared with the local app tree during the
  build. Duplicate internal entries are removed, and the keys
  `.bundle_backend_plan` and `.bundle_run_options` are written by the
  function.

- overwrite:

  If `TRUE` (default), replace `result_dir` only after a complete staged
  build succeeds. Publication failures attempt to restore the previous
  app and preserve its backup if restoration fails. If `FALSE`,
  `result_dir` must be absent or empty; a non-empty directory is
  rejected before any files are written.

- verbose:

  Print progress messages; defaults to TRUE.

- crb_pick_smallest_file:

  Forwarded to `Cerebro.options`.

- show_upload_ui:

  One non-missing logical controlling whether users may upload their own
  data; defaults to `FALSE`.

- initial_page:

  Optional initial Viewer page. Supported stable IDs are `"data_info"`,
  `"projection"`, `"linked_views"`, `"groups"`, `"marker_genes"`,
  `"most_expressed_genes"`, `"enriched_pathways"`, `"extra_material"`,
  `"immune_repertoire"`, `"trajectory"`, `"spatial"`, `"trekker"`,
  `"hla_tcr_motifs"`, `"gene_expression"`, `"gene_id_conversion"`,
  `"color_management"`, and `"about"`. A conditional page is selected
  only when it is available for the loaded dataset. Shared links take
  precedence after restoration.

- welcome_message:

  Welcome message shown in the Load Data tab.

- point_size:

  Named list with `overview_projection_point_size` (and optionally other
  keys) forwarded to `Cerebro.options`.

- variable_to_compare:

  Forwarded to `Cerebro.options`.

- spatial_images:

  Optional nested external-image manifest in
  `dataset -> spatial entry -> image label -> path` form. Dataset names
  must match `cerebro_data`; spatial names must match the corresponding
  CRB's `availableSpatial()`, and image labels must be unique and
  non-empty within an entry. A leaf may instead be a descriptor
  containing `path` and optional coordinate `bounds`. Supplied files are
  copied to opaque, safe relative paths under `spatial-assets/`. The
  legacy `c(Dataset = path)` form is accepted only when that CRB has
  exactly one spatial entry.

- spatial_image_settings:

  Optional nested settings in
  `dataset -> spatial entry -> image label -> settings` form. Settings
  may contain only `flip_x`, `flip_y`, `scale_x`, `scale_y`, `offset_x`,
  `offset_y`, and `rotation`. A leaf may target an embedded or external
  image available under that exact dataset and spatial entry. The image
  label must exist in the union of the CRB's embedded images and this
  call's `spatial_images`; unknown identities are rejected. Labels are
  user-facing names, not protocol names.

- spatial_images_flip_x:

  Legacy named per-dataset horizontal flip values. Each dataset must
  resolve to exactly one spatial image target, unless a legacy
  multi-path `spatial_images` declaration was migrated; then the value
  applies to every migrated external image.

- spatial_images_flip_y:

  Legacy named per-dataset vertical flip values. Each dataset must
  resolve to exactly one spatial image target, unless a legacy
  multi-path `spatial_images` declaration was migrated; then the value
  applies to every migrated external image.

- spatial_images_scale_x:

  Legacy named per-dataset X scale values. Each dataset must resolve to
  exactly one spatial image target, unless a legacy multi-path
  `spatial_images` declaration was migrated; then the value applies to
  every migrated external image.

- spatial_images_scale_y:

  Legacy named per-dataset Y scale values. Each dataset must resolve to
  exactly one spatial image target, unless a legacy multi-path
  `spatial_images` declaration was migrated; then the value applies to
  every migrated external image.

- spatial_images_offset_x:

  Legacy named per-dataset horizontal offsets. Each dataset must resolve
  to exactly one spatial image target, unless a legacy multi-path
  `spatial_images` declaration was migrated; then the value applies to
  every migrated external image.

- spatial_images_offset_y:

  Legacy named per-dataset vertical offsets. Each dataset must resolve
  to exactly one spatial image target, unless a legacy multi-path
  `spatial_images` declaration was migrated; then the value applies to
  every migrated external image.

- spatial_plot_rotation:

  Named list/vector; initial rotation (degrees) applied to spatial cell
  coordinates. Names must match `cerebro_data`.

- auth:

  Optional authentication settings. `NULL`, the default, leaves the
  generated Viewer public. To require a login, provide a named list with
  `credentials`, the path to an encrypted SQLite database created by
  [`shinymanager::create_db()`](https://rdrr.io/pkg/shinymanager/man/create_db.html),
  and `passphrase_env`, the name of the environment variable containing
  its passphrase. Optional `timeout_minutes` defaults to 15.

- extra_tables:

  Optional named collection of CSV, TSV, TXT, XLS, XLSX, or XLSM files
  to bundle as private Extra material tables. Workbook sheets are listed
  separately and loaded only when selected in the Viewer.

- extra_tables_sheets:

  Optional named list of Excel sheet renames keyed by `extra_tables`
  labels. Each entry maps displayed names to source sheet names;
  unmapped sheets remain available.

- admin_account:

  Optional Viewer Administrator account name. Supply it together with
  `admin_password_env` to enable Shared Link management.

- admin_password_env:

  Optional environment-variable name containing the Viewer Administrator
  password. The secret must contain at least 12 bytes and is validated
  but never written into the generated app.

- ...:

  Currently unused; reserved for future arguments.

## Value

Invisibly returns `result_dir`. If that path changes resolution during
the build, warns and returns the frozen absolute publication path.

## Details

Supports external expression backends (`bpcells`, `h5`) in addition to
the embedded mode. Each `.crb` descriptor names a portable relative file
or directory, which is copied to the same private location under
`private-data/`. The historical `data/` name is deliberately not reused:
a still-running older generated app may retain its former `/data` HTTP
resource mapping during an in-place replacement. Raw `.crb`, H5, and
BPCells artifacts are not registered as HTTP resources. Spatial
background images are the deliberate exception: files explicitly
supplied through `spatial_images` are copied verbatim under
`spatial-assets/`. The server-side renderer reads these files and embeds
them as data URIs; the directory is not registered as an HTTP resource.
Callers must provide trusted image files. Preflight requires a minimum
stable runtime API and reads the ordinary `expression_backend` field
without invoking serialized methods or its getter. The generated
configuration stores the effective per-CRB attachment plan after
applying any deployment override, and configured CRBs consume that exact
plan at runtime. Direct launches and uploads also read and validate the
ordinary field without invoking the getter. A historical CRB may omit
both field and getter; an object containing only one is rejected as an
unsupported mixed format. Dataset labels and canonical CRB sources are
both unique: two labels cannot select the same resolved input file.
Generated bundles follow the standard deployment model of one app per R
process; process-global `Cerebro.options` does not provide same-process
isolation between separately sourced apps.

Shared Links are disabled unless `CEREBRONEXUS_LINKED_VIEW_SHARE_DB` (or
`cerebro_options[["linked_view_share_db"]]`) names a writable SQLite
path outside the generated app directory. Keeping that database external
prevents `overwrite = TRUE` from deleting live links. Records are
isolated by a namespace derived from the deployment root; replicated or
relocated deployments should set
`CEREBRONEXUS_LINKED_VIEW_SHARE_NAMESPACE` to one stable app-specific
value. Links expire after 7 days without a usable Viewer Administrator
and after 90 days when `admin_account` and `admin_password_env` resolve
at runtime.

Launch settings are validated and frozen in a typed internal manifest
before target preparation. The generated `app.R` reads that manifest
instead of interpolating user values into source, and the staged source
is parsed before publication. The upload limit is installed as
`shiny.maxRequestSize` while the app is running and the previous process
option is restored when the app stops.

A configured runtime matrix override keeps its existing precedence and
skips the descriptor sidecar copy. It must be an absolute path. Native
paths are resolved component by component and rejected if they resolve
inside `result_dir`. Unresolved filesystem entries, unsafe Windows
aliases, and Windows device namespaces fail closed; a truly absent
ordinary target is allowed for later provisioning. Non-native absolute
paths intended for another host are preserved lexically and cannot be
compared with that host's app tree. Such a bundle depends on that
host-managed path and is therefore not self-contained. Treat input
`.crb`/`.rds` files as trusted serialized R objects; structural
validation is not a sandbox for untrusted RDS input.

One atomic sibling lock directory, `.<basename>-build.lock`, serializes
builds for each target. An existing lock stops the build and is never
removed automatically as "stale". Required inputs and targets are
validated before the app is assembled in a private sibling stage.
Preflight and stage-build failures leave an existing deployment in
place. Publication first renames the old app to a unique backup; if the
final rename fails, restoration is attempted. A failed restoration keeps
the backup and reports its exact path. Abrupt process death between the
two publication renames is not recovered automatically and can leave
`result_dir` temporarily absent. After confirming that the lock owner is
no longer running, verify and restore the correct backup before removing
any leftover stage and lock. On POSIX systems, the stage is mode `0700`
while data is copied, and replacement retains the existing deployment
root's permission bits. Platform-specific ACLs, ownership changes, and
security labels remain the deployment system's responsibility. All CRBs,
descriptor sidecars, spatial images and their source-path ancestors must
remain trusted and unchanged while the build is running; the target
build lock does not protect inputs.

## Examples

``` r
if (FALSE) { # \dontrun{
library(CerebroNexus)

createShinyApp(
  cerebro_data = c(
    "PBMC example" = "output/cerebro_pbmc_seurat.crb"
  ),
  result_dir = "my_app",
  port = 8080,
  host = "127.0.0.1",
  max_request_size = 8000,
  overwrite = TRUE
)
# Run with shiny::runApp("my_app") or deploy my_app/ to Shiny Server.
} # }
```
