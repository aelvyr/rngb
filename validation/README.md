# Development validation

Recorded validation evidence: [RESULTS.md](RESULTS.md).

The original measurement files remain outside the package. No private NGB
files or generated metadata are bundled in the source distribution.

To regenerate independent expected metadata and channel bytes:

```sh
curl -fsSL 'https://codeload.github.com/GraysonBellamy/pyngb/tar.gz/3650627ebabbdf41d4ce70310b0fd036bb0f9f85' -o /tmp/pyngb-reference.tar.gz
python3 tools/reference.py /tmp/pyngb-reference.tar.gz validation/generated \
  /path/to/sample.ngb-ds3 /path/to/baseline.ngb-bs3
Rscript tools/validate_examples.R /path/to/sample.ngb-ds3 \
  /path/to/baseline.ngb-bs3 validation/generated
```

The Python generator uses the actual upstream container, grammar, document,
metadata, channel assembly and run-validation modules. A minimal frame adapter
replaces Polars storage; it does not replace any decoding or extraction logic.
NumPy is the only Python dependency. Reference generation is development tooling,
excluded from the built R package.

Validation compares every extracted metadata field (including nested program
and calibration fields) exactly, and every float64 channel byte for the sample,
embedded baseline and separate baseline. One explicit API difference: R adds
`measurement_type = "correction"` for the legacy baseline file where upstream
omits the field. Neither implementation applies calibration or baseline subtraction.

Synthetic tests exercise corrupt arrays, metadata warnings, stream integrity,
run-count/signature errors, optional stream 3 and resource limits.

For arbitrary supported fixtures after generating their reference data:

```sh
Rscript tools/validate_parity.R validation/generated /path/to/file.ngb-ds3 /path/to/file.ngb-bs3
```
