"""Generate parser parity expectations. Development only; never imported by R.

Usage: python3 tools/reference.py source.tar.gz output_dir sample.ngb-ds3 baseline.ngb-bs3
The archive must be pyngb commit 3650627ebabbdf41d4ce70310b0fd036bb0f9f85.
Only the format modules are loaded, avoiding Polars/Arrow/SciPy dependencies.
NumPy is required. Expected channel bytes are unmodified float64 little endian,
except for the documented time conversion (minutes -> seconds).
"""
import io
import json
import pathlib
import sys
import tarfile
import types

import numpy as np

PIN = "3650627ebabbdf41d4ce70310b0fd036bb0f9f85"


def load_source(path):
    with tarfile.open(path, "r:gz") as archive:
        if not all(m.name.split("/")[0] == "pyngb-" + PIN for m in archive.getmembers()):
            raise ValueError("Expected the pinned pyngb source archive")
        sources = {
            m.name.split("/src/")[1][:-3].replace("/", "."): archive.extractfile(m).read()
            for m in archive.getmembers()
            if "/src/pyngb/" in m.name and m.name.endswith(".py")
        }
    for name in ("pyngb", "pyngb.format"):
        module = types.ModuleType(name)
        module.__path__ = []
        sys.modules[name] = module
    for name in (
        "pyngb.exceptions", "pyngb.config", "pyngb.constants", "pyngb.format.grammar",
        "pyngb.format.maps", "pyngb.format.container", "pyngb.format.document",
        "pyngb.format.extract",
    ):
        module = types.ModuleType(name)
        module.__package__ = name.rsplit(".", 1)[0]
        sys.modules[name] = module
        exec(compile(sources[name], name, "exec"), module.__dict__)
    # Execute the real channel/run validation and assembly functions against a
    # small frame adapter. It implements only the Polars methods these functions
    # use, so expected channel values still come from the upstream assembler.
    class Series:
        def __init__(self, name, values):
            self.name, self.values = name, values

    class DataFrame:
        def __init__(self, columns=None):
            self.values = columns or {}

        @property
        def columns(self):
            return list(self.values)

        @property
        def width(self):
            return len(self.values)

        @property
        def height(self):
            return len(next(iter(self.values.values()))) if self.values else 0

        def with_columns(self, series):
            return DataFrame({**self.values, series.name: series.values})

    polars = types.ModuleType("polars")
    polars.Series, polars.DataFrame = Series, DataFrame
    sys.modules["polars"] = polars
    name = "pyngb.format.channels"
    module = types.ModuleType(name)
    module.__package__ = "pyngb.format"
    sys.modules[name] = module
    exec(compile(sources[name], name, "exec"), module.__dict__)


def r_literal(value):
    if isinstance(value, dict):
        return "list(" + ",".join(json.dumps(k) + "=" + r_literal(v) for k, v in value.items()) + ")"
    if isinstance(value, list):
        constructor = "c" if value and (all(isinstance(x, (float, int)) for x in value)
                                        or all(isinstance(x, str) for x in value)) else "list"
        return constructor + "(" + ",".join(map(r_literal, value)) + ")"
    if isinstance(value, str):
        return json.dumps(value, ensure_ascii=False)
    if value is None:
        return "NULL"
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, float):
        return value.hex()
    return repr(value)


def main():
    archive, output, *paths = sys.argv[1:]
    load_source(archive)
    from pyngb.format.channels import build_dataframe, count_runs
    from pyngb.format.document import load_document
    from pyngb.format.extract import build_metadata

    output = pathlib.Path(output)
    output.mkdir(parents=True, exist_ok=True)
    for path in paths:
        doc = load_document(path, streams=(1, 2, 3))
        for stream in (1, 2, 3):
            if doc.has_defect(stream):
                raise ValueError(f"Reference fixture contains defects: {path}, stream {stream}")
        destination = output / pathlib.Path(path).name
        destination.mkdir(exist_ok=True)
        metadata = build_metadata(doc)
        (destination / "metadata.R").write_text(r_literal(metadata), encoding="utf-8")
        manifest = {}
        for run in range(count_runs(doc)):
            frame = build_dataframe(doc, run=run)
            columns = frame.values
            manifest[f"run_{run}"] = list(columns)
            for name, values in columns.items():
                (destination / f"run_{run}_{name}.bin").write_bytes(np.asarray(values, dtype="<f8").tobytes())
        (destination / "manifest.R").write_text(r_literal(manifest), encoding="utf-8")
        print(f"{pathlib.Path(path).name}: {count_runs(doc)} run(s), {len(metadata)} metadata fields")


if __name__ == "__main__":
    main()
