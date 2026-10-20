# rngb

A native, base R parser for NETZSCH STA `.ngb-ds3` (sample plus correction)
and `.ngb-bs3` (baseline) files. Requires R >= 4.1; no Python, Arrow or
NETZSCH software is needed at runtime.

## Install from GitHub

Run this once in your R console:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
remotes::install_github("aelvyr/rngb", upgrade = "never")
```

Then load the package in each R session:

```r
library(rngb)
```

The repository is public, so no GitHub account or token is required.
The package contains only R code; no compiler or Python installation is needed.
To update to the latest version, rerun `remotes::install_github("aelvyr/rngb", upgrade = "never")`.

Alternatively, install from a local clone:

```sh
R CMD INSTALL .
```

## Read a file

```r
library(rngb)
sample <- read_ngb("measurement.ngb-ds3")
sample$metadata        # Named list, including nested program/calibration records
sample$data            # Raw sample measurements, one column per channel
sample$baseline        # Embedded baseline measurements, independently sampled
sample$column_metadata # Units, descriptions and processing flags

baseline <- read_ngb("baseline.ngb-bs3")
baseline$data           # The baseline measurement
is.null(baseline$baseline) # TRUE
```

The two runs may have different row counts. They are not aligned, subtracted
or concatenated. Time and program durations are in **seconds**. Mass is
**mass change in mg**, not absolute sample mass; the initial sample mass is
in `metadata$sample_mass`. DSC is **raw µV**, not calibrated heat flow.
Recorded sample temperatures are already instrument calibrated. Calibration
records are extracted as metadata without changing measurements.

Metadata includes sample/run identity, UTC date, instrument and crucible
configuration, initial/crucible/reference masses, temperature stages and gas
setpoints, PID settings, gas identity, calibration coefficients and fixpoints,
application version and timezone where available. The metadata describes the
sample in a `.ngb-ds3`; separate baseline metadata is obtained by reading its
`.ngb-bs3`. Missing metadata fields are omitted. Baseline mass sentinels are
preserved, including `-1000` in the supplied example. Measurement kind falls
back to the extension and validated run count for older files lacking its field.

Unknown numeric channel IDs are retained under hexadecimal names, with unknown
units marked `NA`. The parser checks section boundaries, record terminators,
run signatures and channel lengths. Damaged measurement streams fail explicitly;
damaged metadata streams warn. Resource limits can be set with, for example,
`read_ngb(path, limits = list(max_stream_size_mb = 100))`. ZIP streams are read
without extracting files to disk. Stream and array size limits bound memory use;
native R copies raw payloads and is not a zero-copy implementation.

The format is reverse-engineered. Compatibility has been validated against the
two supplied STA449F3A/Proteus 6.1 files and the upstream STA fixtures. Streams
4–6, dilatometer support, baseline subtraction, normalization and analysis are
outside this package's scope. This is not an official NETZSCH package.

## Validation

```sh
R CMD build .
R CMD check --no-manual rngb_0.1.0.tar.gz

# Validate the original examples without copying them into the package:
Rscript tools/validate_examples.R /path/to/sample.ngb-ds3 /path/to/baseline.ngb-bs3
```

The package tests use synthetic binary fixtures for ordinary, corrupt and
resource-limited inputs. `tools/reference.py` generates independent expected
metadata and full-channel binary values using the pinned Python source for
development parity checks. See [validation/README.md](validation/README.md).
Python is needed only to regenerate those expectations, never to use `rngb`.

This is a local development package. The maintainer email in `DESCRIPTION` is
a placeholder and must be replaced before publication.

## Attribution

Ported format knowledge and algorithms from [pyngb](https://github.com/GraysonBellamy/pyngb),
commit `3650627ebabbdf41d4ce70310b0fd036bb0f9f85`, under the MIT license.
See `LICENSE` and `NOTICE`.
