# Builder performance design

## Goal

Reduce Builder import, inspection, snapshot, and CRB publication latency for million-cell projects while lowering temporary storage and avoidable allocation. Viewer page behavior is out of scope.

## Round 1: Builder-only work

Native local files reuse their selected serialized source after the same metadata and content-integrity checks already used for retained browser uploads. Import inspection computes metadata summaries once and reuses them for the legacy Builder profile. Existing source and snapshot MD5 values remain mandatory trust-boundary checks; only repeated scans of an unchanged verified file may be reused.

The expected million-cell effect is lower snapshot time and storage, less metadata allocation, and no CRB format change. Existing RDS, qs, and qs2 Builder inputs remain supported.

## Round 2: Thin CRB/qs2 integration

Port the PR5 CRB codec contract without porting Viewer page optimizations. Builder publishes BPCells-backed CRBs through `saveCerebro()`, verifies them through `readCerebro()`, and bundles the matching runtime reader so generated applications can hydrate Thin CRBs. Embedded legacy CRBs remain readable, and users can explicitly request RDS compatibility where the public API already exposes a codec choice.

## Measurement

Use the cached official 10x one-million-cell Seurat/BPCells fixture. Measure Round 1 Builder import/inspection/snapshot and Round 2 CRB write/decode/hydration. Record wall time, output size, temporary snapshot size, and peak RSS when the harness can collect it reliably. Run the benchmark in separate R processes and compare identical artifacts.

## Constraints

Do not change Viewer pages or copy PR5 rendering/backend work. Do not weaken path, symlink, ownership, source-change, or content-integrity checks. Do not add a second persistent Worker that duplicates a million-cell Seurat object.
