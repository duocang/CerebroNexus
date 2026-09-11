# Million-cell GPU implementation plan

1. Add failing focused tests for the optional 1M demo, symbol-based data prep,
   renderer contract, script loading, and Canvas fallback.
2. Add the cached official 10x download/preparation script and the optional
   `CEREBRO_1M_DEMO_CRB` entry in `inst/app.R`; run the focused tests.
3. Add the raw WebGL2 renderer and connect eligible 2-D Overview/Gene/Linked
   panels beneath the existing interaction canvas; verify fallback and export.
4. Commit the shared work, create the WebGPU sibling branch, then implement the
   same renderer contract with WGSL and `GPUBuffer`s.
5. Run the identical synthetic 1M renderer benchmark and real-CRB browser
   acceptance in alternating order. Record medians, p95, first upload/draw,
   interaction redraw, and memory/buffer size.
6. Run focused tests and repository precheck, update version/NEWS on each
   branch, push only to `origin`, and open two draft PRs against `upstream/master`.

