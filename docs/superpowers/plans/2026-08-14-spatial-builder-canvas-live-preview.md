# Spatial Builder Canvas Live Preview Implementation Plan

> **For AI agents:** Required execution skill: use
> `superpowers-zh:subagent-driven-development`. Track progress with the
> checkboxes below. The user explicitly deferred all tests and validation to a
> separate task, so this plan implements and commits functionality without
> running test commands.

**Goal:** Replace the runtime Spatial Builder Plotly preview with a clean-room
Canvas 2D renderer whose coordinate, image, opacity, and point controls update
locally in the next animation frame.

**Architecture:** The worker adds sampled base coordinates before the saved
coordinate transform. R sends infrequent, versioned, JSON-safe scenes through
one custom message. A standalone browser controller owns the canvas, image
decode, fixed viewport, local controls, hover, resize, and one latest-value
`requestAnimationFrame` queue. R performs image encoding and coverage checks
only at Save.

**Technical stack:** R, Shiny custom messages, browser Canvas 2D,
ResizeObserver, requestAnimationFrame, existing Builder CSS tokens.

---

## File responsibilities

- `inst/builder/preview.R`: produce sampled base coordinates and build a
  JSON-safe Canvas scene; remove the obsolete Spatial Plotly constructor.
- `inst/builder/ui/enhance_stage.R`: render a stable canvas, tooltip, status,
  and accessible summary instead of a Plotly output.
- `inst/builder/www/builder-spatial-canvas.js`: own scene parsing, renderer
  lifecycle, local controls, image decode, geometry, drawing, hover, and stale
  work rejection.
- `inst/builder/www/builder.features.css`: size and style the canvas overlays.
- `inst/builder/app.R`: load the new browser asset.
- `inst/builder/spatial_alignment_server.R`: separate lightweight draft state
  from Save-time finalization and publish structural scenes.
- `inst/builder/www/builder.js`: remove Spatial Plotly enhancement and resize
  behavior after the new controller owns that surface.

### Task 1: Add the clean scene contract

**Files:**
- Modify: `inst/builder/preview.R`

- [ ] **Step 1: Preserve sampled untransformed spatial coordinates**

Before applying a saved coordinate transform, retain `physical_base`. After
barcode matching and shared sampling, return both current authoritative
`spatial` and raw `spatial_base` frames:

```r
physical_base <- physical
coordinate_frame <- .builder_alignment_bounds(physical_base)

if (!is.null(saved_transform)) {
  coordinate_transform <- .spx_coordinate_transform_normalize(
    saved_transform,
    physical_base
  )
  physical <- .spx_apply_coordinate_transform(physical_base, saved_transform)
}

spatial_base_full <- physical_base[
  match(common, physical_base$cell_barcode),
  c("cell_barcode", "x", "y"),
  drop = FALSE
]
spatial_base_full$group <- transcriptome_full$group

list(
  spatial = spatial_full[keep, , drop = FALSE],
  spatial_base = spatial_base_full[keep, , drop = FALSE],
  coordinate_frame = coordinate_frame,
  coordinate_transform = coordinate_transform
)
```

Return `spatial_base = NULL` from unavailable models and return the Trekker
spatial frame as its own base frame.

- [ ] **Step 2: Add a pure Canvas scene helper**

Replace `builder_alignment_plot()` with
`builder_spatial_canvas_scene(preview, colors, record, coordinate_spec,
identity, generation, reset_token)`. It must:

```r
builder_spatial_canvas_scene <- function(
  preview,
  colors = NULL,
  record = NULL,
  coordinate_spec = list(rotation_degrees = 0, scale = 1),
  identity = list(),
  generation = 0L,
  reset_token = 0L
) {
  if (!isTRUE(preview$available)) {
    return(list(
      schema_version = 1L,
      generation = generation,
      reset_token = reset_token,
      identity = identity,
      available = FALSE,
      message = preview$message %||% "Loading spatial preview…"
    ))
  }

  frame <- preview$spatial_base %||% preview$spatial
  levels <- unique(as.character(frame$group))
  palette <- builder_level_colors(levels)
  shared <- intersect(levels, names(colors %||% character()))
  palette[shared] <- colors[shared]
  group_index <- match(as.character(frame$group), levels) - 1L
  parameters <- if (is.null(record)) {
    builder_alignment_defaults()
  } else {
    .builder_alignment_parameters(record)
  }

  list(
    schema_version = 1L,
    generation = generation,
    reset_token = reset_token,
    identity = identity,
    available = TRUE,
    capped = isTRUE(preview$capped),
    coordinate_frame = preview$coordinate_frame,
    points = list(
      x = as.numeric(frame$x),
      y = as.numeric(frame$y),
      barcode = as.character(frame$cell_barcode),
      group_index = as.integer(group_index)
    ),
    groups = lapply(seq_along(levels), function(index) {
      list(
        label = levels[[index]],
        color = unname(palette[[levels[[index]]]]),
        count = sum(group_index == index - 1L)
      )
    }),
    image = if (is.null(record)) NULL else list(
      key = paste(identity$image_label %||% "", record$source$name %||% ""),
      source_uri = record$source_uri,
      base_bounds = record$base_bounds
    ),
    controls = c(
      list(coordinate_rotation = coordinate_spec$rotation_degrees %||% 0),
      parameters
    )
  )
}
```

The helper must reject or normalize non-finite structural values through the
existing R contracts before serialization.

- [ ] **Step 3: Commit the scene contract**

```bash
git add inst/builder/preview.R
git commit -m "feat(builder): define spatial canvas scene"
```

### Task 2: Build the stable Canvas UI surface

**Files:**
- Modify: `inst/builder/ui/enhance_stage.R`
- Modify: `inst/builder/www/builder.features.css`
- Modify: `inst/builder/app.R`

- [ ] **Step 1: Replace the Plotly output helper**

Create `builder_alignment_canvas_output()` and update its caller to use
`alignment_spatial_canvas`:

```r
builder_alignment_canvas_output <- function(id, label) {
  div(
    class = "spatial-alignment-plot-frame",
    tags$canvas(
      id = id,
      class = "builder-spatial-canvas",
      `data-builder-spatial-canvas` = "true",
      role = "img",
      `aria-label` = label,
      `aria-describedby` = paste0(id, "-summary")
    ),
    div(class = "builder-spatial-canvas-status", `aria-hidden` = "true"),
    div(class = "builder-spatial-canvas-tooltip", role = "tooltip", hidden = "hidden"),
    span(
      id = paste0(id, "-summary"),
      class = "visually-hidden builder-spatial-canvas-summary",
      "Spatial preview is loading."
    )
  )
}
```

- [ ] **Step 2: Add Canvas and overlay CSS**

Remove Plotly-only descendant sizing selectors and add rules equivalent to:

```css
.builder-spatial-canvas {
  display: block;
  width: 100%;
  height: 100%;
  background: var(--c-surface);
}
.builder-spatial-canvas-status,
.builder-spatial-canvas-tooltip {
  position: absolute;
  z-index: 2;
  pointer-events: none;
}
.builder-spatial-canvas-status {
  inset: 0;
  display: grid;
  place-items: center;
  padding: var(--space-4);
  color: var(--c-text-muted);
  text-align: center;
}
.builder-spatial-canvas-status:empty { display: none; }
.builder-spatial-canvas-tooltip {
  max-width: 18rem;
  padding: .45rem .6rem;
  border: 1px solid var(--c-border-2);
  border-radius: var(--radius-sm);
  background: color-mix(in srgb, var(--c-surface) 94%, transparent);
  box-shadow: var(--shadow-sm);
  color: var(--c-text);
  font-size: .78rem;
}
```

- [ ] **Step 3: Load the standalone renderer**

Add `builder-spatial-canvas.js` immediately after `builder.js` in
`inst/builder/app.R`, using the existing `asset_stamp()` function.

- [ ] **Step 4: Commit the Canvas surface**

```bash
git add inst/builder/ui/enhance_stage.R inst/builder/www/builder.features.css inst/builder/app.R
git commit -m "feat(builder): add spatial canvas surface"
```

### Task 3: Implement the browser renderer

**Files:**
- Create: `inst/builder/www/builder-spatial-canvas.js`

- [ ] **Step 1: Implement lifecycle and structural messages**

Use one IIFE, one scene-message registration flag, one cached latest scene, and
one current instance. Register the handler on `shiny:connected` and immediately
when `window.Shiny` already exists:

```js
(function () {
  "use strict";
  var latestScene = null;
  var instance = null;
  var handlerRegistered = false;

  function ensureInstance() {
    var canvas = document.querySelector("[data-builder-spatial-canvas]");
    if (instance && instance.canvas !== canvas) instance.dispose();
    if (canvas && (!instance || instance.canvas !== canvas)) {
      instance = createRenderer(canvas);
    }
    if (instance && latestScene) instance.setScene(latestScene);
  }

  function registerHandler() {
    if (handlerRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler("builder_spatial_canvas_scene", function (scene) {
      latestScene = scene;
      ensureInstance();
    });
    handlerRegistered = true;
  }
})();
```

The renderer rejects a scene when its schema is not 1, its generation is older
than the current generation, or required finite arrays and bounds are invalid.

- [ ] **Step 2: Implement local control capture and rAF coalescing**

Map the ten existing input IDs to the scene control keys. Delegated `input` and
`change` handlers read all controls from the DOM, patch the current state, and
schedule one latest-value draw:

```js
function schedule() {
  if (disposed || animationFrame !== null) return;
  animationFrame = window.requestAnimationFrame(function () {
    animationFrame = null;
    draw();
  });
}
```

Apply scene control values only for a new view key or a changed reset token.
Keep local controls for a newer structural scene of the same view and reset
token.

- [ ] **Step 3: Implement resize, viewport, and axes**

Use ResizeObserver, cap DPR at 2, and express all drawing dimensions in CSS
pixels. Freeze a viewport from the coordinate-frame half-diagonal square union
initial image base bounds plus six-percent padding. Implement nice numeric tick
steps and a single Y-inverting, aspect-preserving data-to-screen transform.

- [ ] **Step 4: Implement image decode and drawing**

Decode only `source_uri`. Increment an image token for every image/view change;
accept `load` or `decode()` completion only when generation, view key, image key,
token, and connected canvas still match. Clear the old image immediately on a
view change.

Draw the source image centered on base bounds plus offsets, with data spans
multiplied by scale, `ctx.rotate(-degrees * Math.PI / 180)`, and negative X/Y
scale values for flips.

- [ ] **Step 5: Implement point and frame drawing**

Rotate base points and coordinate-frame corners with the absolute coordinate
control around the source-frame center. Draw points in color buckets, then the
dotted original frame, solid transformed frame, orange first edge/endpoints,
and signed one-decimal rotation label.

- [ ] **Step 6: Implement local hover and accessibility**

Coalesce pointer movement into the same animation-frame queue. Find the nearest
screen point within `max(6, pointSize / 2 + 3)` CSS pixels and update the DOM
tooltip with barcode, group, and sampled group count. Never send hover or click
state to Shiny. Update the canvas ARIA label and hidden summary after every new
scene.

- [ ] **Step 7: Commit the renderer**

```bash
git add inst/builder/www/builder-spatial-canvas.js
git commit -m "feat(builder): render spatial drafts on canvas"
```

### Task 4: Move the server hot path out of R rendering

**Files:**
- Modify: `inst/builder/spatial_alignment_server.R`

- [ ] **Step 1: Make the worker contract depend only on saved transforms**

Replace the draft-dependent transform helper with one that returns
`coordinate_transforms_for(entry)`. Keep coordinate controls in
`coordinate_draft`, but do not include that reactive value in
`preview_contract_for()` or `request_preview()`.

- [ ] **Step 2: Replace reactive encoding with lightweight and final paths**

Remove `point_appearance`, `orientation`, and `encoded`. Keep `parameters()` and
define:

```r
draft_record <- shiny::reactive({
  record <- draft()
  if (is.null(record)) return(NULL)
  values <- parameters()
  record[names(values)] <- values
  record
})

finalize_current_record <- function() {
  record <- shiny::isolate(draft_record())
  image <- shiny::isolate(raw_image())
  preview <- shiny::isolate(alignment_preview())
  if (is.null(record) || is.null(image) || !isTRUE(preview$available)) return(NULL)

  encoded <- builder_encode_image(
    image$array,
    max_px = 1400,
    flip_y = record$flip_y,
    flip_x = record$flip_x,
    rotate = record$rotation
  )
  if (!is.null(encoded$error)) return(encoded)

  finalized <- builder_alignment_record(
    source = record$source,
    source_uri = record$source_uri,
    uri = encoded$uri,
    base_bounds = record$base_bounds,
    parameters = record,
    image_geometry = encoded,
    saved = FALSE,
    section = list(id = active_section(), kind = preview$section$kind)
  )
  finalized[names(encoded)[names(encoded) != "uri"]] <- encoded[names(encoded) != "uri"]
  coverage <- builder_bounds_cover(
    finalized$bounds,
    list(preview$spatial$x, preview$spatial$y)
  )
  finalized$outside <- coverage$outside
  finalized$total <- coverage$total
  finalized
}
```

Expose `current_record` as the lightweight `draft_record` reactive for the
existing return interface. Change Save to call `finalize_current_record()`.

- [ ] **Step 3: Make dirty marking encoding-free**

Change `mark_unsaved()` to compare and commit `draft_record()` without touching
URI, image geometry, or coverage. Preserve the current saved record as baseline
before the first real parameter change.

- [ ] **Step 4: Publish versioned scenes**

Add generation and reset-token reactive values plus one `publish_canvas()`
function. Build a safe identity from dataset ID/revision, section ID/kind, and
image label; do not send snapshot or upload paths. Send
`builder_spatial_canvas_scene` through
`session$sendCustomMessage("builder_spatial_canvas_scene", scene)`.

Publish:

- a structural update when a valid worker preview arrives;
- an authoritative reset after restore, upload, image Reset, rename, or view
  switch;
- an unavailable/loading scene when the active dataset or section no longer
  matches the current preview.

Do not publish from live-control dirty marking.

- [ ] **Step 5: Delete the Plotly render output**

Remove `output[["enhance-alignment_spatial_plot"]] <- plotly::renderPlotly(...)`.
Keep the existing legend and status outputs.

- [ ] **Step 6: Commit the server boundary**

```bash
git add inst/builder/spatial_alignment_server.R
git commit -m "refactor(builder): isolate spatial canvas hot path"
```

### Task 5: Remove the obsolete browser Plotly integration

**Files:**
- Modify: `inst/builder/www/builder.js`

- [ ] **Step 1: Delete Spatial Plotly enhancement functions**

Delete `plotSummaryRows`, `renderPlotSummary`, `enhancePlot`, `finiteExtent`,
`spatialPreviewAspect`, `syncSpatialPreviewAspect`, and
`syncSpatialWorkbench`.

- [ ] **Step 2: Remove dynamic Plotly enhancement**

Delete the `.js-plotly-plot` scan from `enhanceDynamicContent()`. Leave the
Spatial section-selection restoration, image picker behavior, sidebar
scrollbar, and unrelated Builder behavior intact.

- [ ] **Step 3: Commit Plotly removal**

```bash
git add inst/builder/www/builder.js
git commit -m "refactor(builder): remove spatial Plotly client hooks"
```

### Task 6: Finish the functional implementation pass

**Files:**
- Review only the files changed in Tasks 1–5.

- [ ] **Step 1: Resolve integration mismatches without running tests**

Read the changed call sites once and align exact names across R, HTML, CSS, and
JavaScript:

```text
alignment_spatial_canvas
builder_spatial_canvas_scene
builder_spatial_canvas_scene (custom-message name)
data-builder-spatial-canvas
schema_version
generation
reset_token
```

This step is a source integration pass only. Do not start the app, run tests,
run linters, or run package checks.

- [ ] **Step 2: Commit any integration-only corrections**

```bash
git add inst/builder
git commit -m "fix(builder): connect spatial canvas preview"
```

- [ ] **Step 3: Record the deferred verification scope**

Hand off that the separate verification task must replace Plotly-specific unit
contracts, add scene/geometry/browser coverage, and run focused then full
checks. Do not perform those actions in this implementation task.
