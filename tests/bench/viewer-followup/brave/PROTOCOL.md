# Windows Brave focused follow-up, v1

Baseline: c108c7e0. Same checkout; no branch/worktree creation. Scope: Linked,
Trajectory, Gene selector race, dataset resource lifetime. No Gene first-load
optimization or whole-Viewer benchmark.

Windows 11 / i9-12900K / 128 GiB, Brave 154.1.96.59 via the supported extension,
normal headed browser, explicit 1619 x 950 viewport for every new tab. R remains
the existing WSL Ubuntu 24.04 R 4.6.1 / BPCells 0.3.1 / Shiny 1.14.0 environment.
Native Windows R lacks these dependencies. Fresh R and fresh tab per arm;
existing Brave process/profile, warm OS/package/static-library caches. Existing
user tabs remain open. This is NOT the previous Chrome/SwiftShader series.

Dataset and sidecar/pack identities are unchanged from ../PROTOCOL.md. Mouse CRB
SHA256: cd6f4aed684ecefc73e8136db692faf9b6a633fe3e6918e11e07771c31814e38.
Dataset fingerprint: md5-cell-set-v1:fa3fbe15b4aac9ce03ebcd5abb50acb4.
Package loaded from this checkout (4.6.4), 100% cells, hover enabled, defaults.
Server launch command and source/harness identities are recorded by arm.py.

Setup: Data info displays 1,000,000 cells; >=5 seconds without websocket traffic,
then a real UI click on Linked. Quiet is setup only, never page readiness. The
instrumentation records click time, input requests, binary decode, painted ready
events, actual point count/fingerprint, transport metrics, and errors. First
usable requires primaryReady + painted + 1M points; complete requires ready +
complete + painted. Revisit is a separate Data info -> Linked operation.
Do not alter production ready signaling. Save only after completion; no save
request during the measured operation. Raw cross-host sent_at_ms-derived
serializeTransferMs is diagnostic only: host clocks are not synchronized to a
measured bound. Browser-local binary arrival/decode/draw are retained separately.

R process tree RSS sampled every 100 ms, including fixed test-monitor overhead.
Brave total-process RSS sampled every 100 ms for 35 seconds starting before the
click; this includes existing user tabs, NOT isolated Viewer browser RSS. Report
that limitation. Page JS heap peak is also sampled (250 ms; approximate).

Exploratory linked-B1/C1 and screen rows are Level 1 only (C1 had an early save,
adding 61 bytes/3 messages). Formal prefix formal-B/C runs use the frozen wrapper
after its monitor zero-RSS shutdown fix: B1 C1 B2 C2 B3 C3. Add two pairs only if
the completion benefit is inconsistent. Do not pool exploratory timings.

Acceptance: exact old/new million-cell trajectory coordinates and order,
malformed/missing index fallback tests, actual Brave count/fingerprint, all
linked panels drawn, hover/brush/selection checks, no visible errors, stable
completion benefit, no material memory increase. First <2 seconds is a target,
not permission to change behavior or skip paint. Retain identical f64 coordinate
wire and legacy fallback. No Ren or Spatial runs for this change.
