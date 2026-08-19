# Project Workspace v3 Design

## Decision

`builder-project.json` is a small authoritative index, not a serialized runtime
object. Dataset configuration, regenerable profile caches, source data, spatial
assets, and build artifacts have separate lifecycles and files.

## Problems in schema v2

- `configuration.payload` serializes nearly the full runtime entry.
- A 22 MB manifest expands to a 90 MB R entry while editable settings use less
  than 0.4 MB.
- Derived assay, manifest, identity, reduction, and metadata profiles dominate
  storage and are repeatedly copied for configuration digests.
- Every save rewrites the full JSON; one malformed payload can block the whole
  project.
- Checked state, artifact reuse, source availability, and cache validity are
  coupled even though they are independent states.

## Workspace layout

```text
project/
  builder-project.json
  datasets/<id>/config.json
  cache/<id>/profile.qs2
  sources/<id>/<source-file>
  assets/<id>/spatial/<asset-file>
  artifacts/<id>/<artifact-file>
  checkpoints/<build-id>/
```

## Authoritative manifest

The manifest stores project metadata, dataset order and identity, source
descriptors, configuration descriptors, cache descriptors, artifact
descriptors, global Builder preferences, and last UI location. It never stores
runtime profiles, snapshots, Seurat objects, preview frames, data URIs, or
inline serialized dataset entries.

## Dataset configuration

`datasets/<id>/config.json` stores only revision, settings,
acknowledgements, and spatial drafts. Spatial images are path/fingerprint
references after asset staging. Checked state records a configuration digest,
source fingerprint, and contract version.

## Profile cache

`cache/<id>/profile.qs2` stores source-derived `profile`, `dataset_profile`, and
`levels` using `qs2` serialization. It is optional and replaceable. A
descriptor binds it to a source fingerprint, cache schema, producer version,
file size, and MD5. Project open does not eagerly load every cache. Existing
schema-v3 `.rds` descriptors remain readable, but every new or regenerated
cache is written as `.qs2`.

## Identities

The configuration digest hashes only normalized settings, acknowledgements,
spatial drafts, spatial asset fingerprints, and a configuration contract
version. Checked state compares the current configuration digest with the last
checked digest. Artifact reuse separately compares the current digest with
`built_from_configuration` and verifies the artifact file.

## Lifecycle

- Import keeps the object in the Worker, writes its immutable snapshot, and
  writes a replaceable binary profile cache.
- Save writes changed small config files atomically, reuses valid cache files,
  and commits the manifest last.
- Open reads the manifest and configs first. Source reload and cache hydration
  occur per dataset rather than by expanding every runtime entry from JSON.
- Done checking applies pending drafts, hashes the small config, records the
  checked digest, switches datasets immediately, and lets preview work continue
  asynchronously.
- BuildPlan references configuration and snapshot fingerprints; it does not
  copy dataset profiles.

## Compatibility

Schema v1/v2 remain readable. Their inline payload is treated as a legacy
source for one session. The next successful save extracts the minimal config,
writes the profile cache, writes schema v3, and removes the inline payload from
the manifest. The original `.bak` remains recoverable.

## Guardrails

- New manifests cannot contain `configuration.payload`.
- Config files cannot contain runtime profiles or inline image data.
- Main manifest target is below 1 MB and warns/rejects pathological output.
- Cache failure degrades to source reload and never makes the project invalid.
- All managed paths are resolved beneath the project root.

## Expected effects

- Manifest size depends on dataset count and configuration, not source size.
- Small setting changes write KB-scale config rather than MB/GB payloads.
- Checked identity no longer copies the whole entry.
- Opening and switching become incremental; a broken cache affects one dataset.
- Large Seurat sources can remain GB-scale without creating GB-scale JSON.
