# Viewer feedback design review

Latest production audit and acceptance: [FULL_SWEEP.md](FULL_SWEEP.md) covers
all 17 Viewer pages and the shared interaction fixes.

Open [the interactive prototype](prototype/index.html) and use the four A/B/C/D
buttons. It also works directly from the filesystem; no build or package install
is needed. For a local HTTP preview from the repository root:

```powershell
python -m http.server 8941 --bind 127.0.0.1 --directory tests/bench/viewer-followup/interaction-feedback/prototype
```

Open `http://127.0.0.1:8941/`. Query `?design=A`, `B`, `C` or `D` opens a particular
visual direction. The local server serves only the prototype directory.

Try first entry, parameter update, primary-ready with secondary-pending,
dataset switch, page revisit, selection, and error/empty states. “停在当前阶段”
holds a state for visual comparison; uncheck it to let simulated work complete.
“减少动态效果” disables motion without changing the simulated work duration.
All scientific content is synthetic. The timing is intentionally fixed and is
not an app benchmark.

The complete per-page audit, Top 8, proposed state contract and evidence limits
are in [AUDIT.md](AUDIT.md). Production baseline is `312f36a8`; no production
Viewer files changed in that prototype round. The user subsequently chose A;
production implementation and targeted real-Brave results are in
[RESULTS.md](RESULTS.md), with production screenshots in
`implementation-evidence/`. The prototype remains a separate synthetic demo.

| Preview | Screenshot |
|---|---|
| A: parameter update | [A-updating.png](evidence/A-updating.png) |
| B: first entry | [B-first-entry.png](evidence/B-first-entry.png) |
| C: primary ready, secondary pending | [C-primary-ready.png](evidence/C-primary-ready.png) |
| D: plot overlay | [D-updating.png](evidence/D-updating.png) |
| Dataset switch | [dataset-switch.png](evidence/dataset-switch.png) |
| Error | [error.png](evidence/error.png) |
