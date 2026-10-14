# Validation results — 2026-10-14

Implementation: rngb 0.1.0, native R, no Python runtime dependency.
Environment: R 4.1.1, macOS arm64.
Reference: pyngb 0.6.0, commit 3650627ebabbdf41d4ce70310b0fd036bb0f9f85.

## Supplied measurement pair

| File | Sample/data rows | Embedded baseline rows | Channels |
| --- | ---: | ---: | ---: |
| DH_260902_run01_W1C5_000_010_a.ngb-ds3 | 16,951 | 16,950 | 9 |
| DH_260902_run01_xxxx_xxx_xxx_a.ngb-bs3 | 16,950 | Not applicable | 9 |

- Every measurement channel compared byte-for-byte as little-endian float64
  against the upstream channel assembler, including time conversion to seconds.
- Sample data, embedded baseline data and standalone baseline data all passed.
- The embedded and standalone baseline data frames are exactly identical.
- All 39 sample metadata fields and 38 upstream baseline metadata fields passed
  exact recursive comparison, including temperature stages, gas settings,
  calibration coefficients/fixpoints, Unicode strings, timestamps and masses.
- R adds the documented `measurement_type = "correction"` fallback to the
  legacy baseline file, which has no upstream extracted measurement-kind field.
- Both files had zero malformed/truncated spans in streams 1–3.
- Both R reads together took approximately 0.89 seconds on this environment;
  this is a local timing observation, not a cross-platform performance guarantee.

## Additional upstream fixtures

Complete metadata and channel-byte parity passed for:

- 01_Messung_Probe_G_300_Grad_Ar.ngb-ds3
- 02_Messung_Probe_E_300_Grad_Ar.ngb-ds3
- Douglas_Fir_STA_Baseline_10K_250730_R13.ngb-bs3
- Douglas_Fir_STA_Baseline_10K_250813_R15.ngb-bs3

## Package checks

`R CMD build .` and `R CMD check --no-manual --no-vignettes rngb_0.1.0.tar.gz`
passed. Final check status: **OK**, with zero errors, warnings or notes.
Dependency repository access produced console messages because network access
was unavailable; all runtime dependencies are bundled with R and checks completed.

The package's synthetic tests passed for:

- Separate runs with unequal sample/baseline lengths; optional stream 3.
- Float32 widening, time conversion, unknown numeric channel preservation.
- Damaged measurement records (fatal) and metadata records (warning).
- Array overrun, mismatched column lengths, data preceding a channel header.
- Stray duplicate headers, mismatched run counts and measurement-kind conflicts.
- Invalid magic/section directory, missing streams and resource limits.
- UTF-16LE strings with surrogate pairs and signed int32 minimum decoding.

The source archive contains no private measurement files, generated reference
data or Python tooling. Tests establish parser parity; no comparison to a new
Proteus CSV export or calibration/baseline-subtraction implementation is claimed.
