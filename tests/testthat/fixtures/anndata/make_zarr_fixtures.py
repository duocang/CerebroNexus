"""Regenerate the tiny Zarr / SpatialData fixtures used by test-spatialdata.R.

Written with the real anndata and zarr-python, so the tests read the on-disk
layout those tools produce:

    .venv/Scripts/python make_zarr_fixtures.py     # zarr stores
    Rscript make_zarr_shapes.R                     # shapes.parquet (needs R's arrow)

Fixtures were generated with anndata 0.13.4 and zarr 3.4.1.

  small_v3.zarr          a plain AnnData as a Zarr v3 store (zstd)
  small_v2.zarr          the same as a Zarr v2 store (zstd)
  small_zarr_twin.h5ad   the same data as .h5ad, to compare against
  small_sd_visium.zarr   SpatialData: two Visium-like sections, table with
                         obsm["spatial"], a hires picture per section placed by a
                         scale transformation (full-resolution pixels * 0.1)
  small_sd_cells.zarr    SpatialData: one region of cells whose coordinates are
                         only in shapes/cells (circles); the table has none
"""
import json
import shutil
import warnings
from pathlib import Path

import anndata as ad
import numpy as np
import pandas as pd
import scipy.sparse as sp
import zarr
from zarr.codecs import ZstdCodec, BytesCodec

warnings.filterwarnings("ignore")
here = Path(__file__).parent
rng = np.random.default_rng(11)



def recompress_blosc_as_zstd(store):
    """anndata writes Zarr v2 with Blosc, which the R reader does not take (it has no
    Blosc decoder); recompress every chunk with zstd and say so in the metadata."""
    import numcodecs

    blosc, zstd = numcodecs.Blosc(), numcodecs.Zstd(level=3)
    config = {"id": "zstd", "level": 3}
    for zarray in Path(store).rglob(".zarray"):
        meta = json.loads(zarray.read_text())
        if (meta.get("compressor") or {}).get("id") != "blosc":
            continue
        for chunk in zarray.parent.iterdir():
            if not chunk.name.startswith("."):
                chunk.write_bytes(bytes(zstd.encode(blosc.decode(chunk.read_bytes()))))
        meta["compressor"] = config
        zarray.write_text(json.dumps(meta, indent=2))
    consolidated = Path(store) / ".zmetadata"
    if consolidated.exists():
        data = json.loads(consolidated.read_text())
        for key, value in data["metadata"].items():
            if key.endswith(".zarray") and (value.get("compressor") or {}).get("id") == "blosc":
                value["compressor"] = config
        consolidated.write_text(json.dumps(data, indent=2))

def table(n, regions, region_of, instance_start=0, with_spatial=True, extent=None):
    counts = rng.poisson(2.0, size=(n, 30)).astype(np.float32)
    counts[counts < 2] = 0
    obs = pd.DataFrame(
        {
            "cell_type": pd.Categorical(rng.choice(["A", "B", "C"], n)),
            "region": pd.Categorical(region_of, categories=regions),
            "spot_id": np.arange(instance_start, instance_start + n),
            "n_genes_by_counts": (counts > 0).sum(axis=1).astype(int),
            "total_counts": counts.sum(axis=1),
        },
        index=[f"spot{i:03d}" for i in range(n)],
    )
    var = pd.DataFrame(index=[f"GENE{i:02d}" for i in range(30)])
    a = ad.AnnData(X=sp.csr_matrix(counts), obs=obs, var=var)
    if with_spatial:
        spatial = rng.uniform(100, 900, size=(n, 2))
        if extent is not None:   # keep every spot inside the picture of its section
            for region, (width, height) in extent.items():
                rows = np.array(region_of) == region
                spatial[rows, 0] = rng.uniform(0.05 * width, 0.95 * width, rows.sum())
                spatial[rows, 1] = rng.uniform(0.05 * height, 0.95 * height, rows.sum())
        a.obsm["spatial"] = spatial
    a.obsm["X_umap"] = rng.normal(size=(n, 2))
    a.uns["spatialdata_attrs"] = {
        "region": list(regions),
        "region_key": "region",
        "instance_key": "spot_id",
    }
    return a


def write_table(a, group_path, regions):
    ad.io.write_elem(zarr.open_group(str(group_path.parent), mode="a", zarr_format=3), group_path.name, a)
    g = zarr.open_group(str(group_path), mode="r+", zarr_format=3)
    g.attrs.update(
        {
            "spatialdata-encoding-type": "ngff:regions_table",
            "region": list(regions),
            "region_key": "region",
            "instance_key": "spot_id",
            "version": "0.2",
        }
    )


def transformation(scale, system):
    return [
        {
            "type": "scale",
            "scale": [scale, scale],
            "input": {"name": "xy", "axes": [{"name": "x", "type": "space"}, {"name": "y", "type": "space"}]},
            "output": {"name": system, "axes": [{"name": "x", "type": "space"}, {"name": "y", "type": "space"}]},
        }
    ]


def shapes_group(root, name, system, scale):
    g = root.require_group(f"shapes/{name}")
    g.attrs.update(
        {
            "encoding-type": "ngff:shapes",
            "axes": ["x", "y"],
            "coordinateTransformations": transformation(scale, system),
            "spatialdata_attrs": {"version": "0.3"},
        }
    )


def image_group(root, name, system, pixels):
    g = root.require_group(f"images/{name}")
    g.attrs.update(
        {
            "ome": {
                "omero": {"channels": [{"label": "r"}, {"label": "g"}, {"label": "b"}]},
                "version": "0.5-dev-spatialdata",
                "multiscales": [
                    {
                        "datasets": [{"path": "0", "coordinateTransformations": [{"type": "scale", "scale": [1.0, 1.0, 1.0]}]}],
                        "name": f"/images/{name}",
                        "axes": [
                            {"name": "c", "type": "channel"},
                            {"name": "y", "type": "space"},
                            {"name": "x", "type": "space"},
                        ],
                        "coordinateTransformations": [
                            {
                                "type": "identity",
                                "input": {"name": "cyx", "axes": [{"name": "c", "type": "channel"}, {"name": "y", "type": "space"}, {"name": "x", "type": "space"}]},
                                "output": {"name": system, "axes": [{"name": "c", "type": "channel"}, {"name": "y", "type": "space"}, {"name": "x", "type": "space"}]},
                            }
                        ],
                    }
                ],
            },
            "spatialdata_attrs": {"version": "0.3"},
        }
    )
    g.create_array("0", data=pixels, chunks=pixels.shape, compressors=[ZstdCodec()])


# plain AnnData as zarr v3 and v2 ---------------------------------------------
plain = table(60, ["r1"], ["r1"] * 60, with_spatial=False)
plain.obs["flag"] = pd.array([True, False, None] * 20, dtype="boolean")
plain.obs["n_flag"] = pd.array([1, 2, None] * 20, dtype="Int64")
for name, fmt in [("small_v3.zarr", 3), ("small_v2.zarr", 2)]:
    target = here / name
    shutil.rmtree(target, ignore_errors=True)
    ad.settings.zarr_write_format = fmt
    plain.write_zarr(target)
    if fmt == 2:
        recompress_blosc_as_zstd(target)
ad.settings.zarr_write_format = 3
plain.write_h5ad(here / "small_zarr_twin.h5ad")   # the same data as h5ad, to compare against

# SpatialData, Visium-like: two sections, obsm["spatial"], one picture each ----
target = here / "small_sd_visium.zarr"
shutil.rmtree(target, ignore_errors=True)
root = zarr.open_group(str(target), mode="w", zarr_format=3)
root.attrs.update({"spatialdata_attrs": {"version": "0.2", "spatialdata_software_version": "0.7.1"}})
regions = ["sec1", "sec2"]
region_of = ["sec1"] * 30 + ["sec2"] * 24
a = table(54, regions, region_of, extent={"sec1": (360, 400), "sec2": (320, 360)})
a.obs["spot_id"] = list(range(30)) + list(range(24))
write_table(a, target / "tables" / "table", regions)
for k, (name, size) in enumerate([("sec1", (40, 36)), ("sec2", (36, 32))]):
    shapes_group(root, name, name, 0.1)
    pixels = rng.integers(0, 255, size=(3, *size), dtype=np.uint8)
    image_group(root, f"{name}_hires_image", name, pixels)
zarr.consolidate_metadata(str(target))

# SpatialData, cells only: coordinates live in shapes/cells -------------------
target = here / "small_sd_cells.zarr"
shutil.rmtree(target, ignore_errors=True)
root = zarr.open_group(str(target), mode="w", zarr_format=3)
root.attrs.update({"spatialdata_attrs": {"version": "0.2", "spatialdata_software_version": "0.7.1"}})
a = table(50, ["cells"], ["cells"] * 50, with_spatial=False)
a.obs["spot_id"] = np.arange(50)[::-1].copy()      # instance ids deliberately not in row order
write_table(a, target / "tables" / "table", ["cells"])
shapes_group(root, "cells", "global", 1.0)
zarr.consolidate_metadata(str(target))
print("zarr fixtures written")
