# Million-cell GPU renderer comparison

## Goal

Make the existing Overview and Gene expression 2-D scatter views usable with
one million cells, without changing their controls or adding dependencies.

## Shape

Two mutually exclusive branches start from `v4.4.3` (`69893a2b`):

- `perf/pr0-million-cell-viewer-webgl2`
- `perf/pr0-million-cell-viewer-webgpu`

They share the same official 10x 1M data preparation, demo configuration,
renderer contract, point data, shaders' geometry, and benchmark. Only the
browser GPU API differs.

The renderer draws instanced anti-aliased circles on a transparent GPU canvas
under the existing 2-D interaction canvas. Existing Canvas code keeps hover,
selection, labels, trajectories, minimaps, exports, and non-2-D/Spatial
fallbacks. No binary transport or backend expression changes belong here.

## Acceptance

- An unset `CEREBRO_1M_DEMO_CRB` leaves `inst/app.R` unchanged at runtime.
- A valid CRB plus adjacent `.bpcells` adds `10x E18 mouse brain (1M)` with
  10% initial sampling, point size 1, and opacity 0.5.
- The preparation script stores unique mouse gene symbols.
- Both renderers draw the same 1,000,000-point buffers and expose the same
  benchmark timings.
- WebGPU reports unavailable instead of falling back to WebGL2.
- The real 1M CRB opens in Overview and Gene expression; existing UI and
  interactions remain available.

