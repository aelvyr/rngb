\name{read_ngb}
\alias{read_ngb}
\title{Read NETZSCH STA NGB Measurements}
\usage{read_ngb(path, limits = list())}
\arguments{
\item{path}{A single path to an \code{.ngb-ds3} or \code{.ngb-bs3} file.}
\item{limits}{Named list of positive resource limits. Defaults:
\code{max_stream_size_mb = 1000}, \code{max_array_size_mb = 500}, and
\code{max_tables_per_stream = 10000}. Sizes are measured in MiB.}
}
\description{Parses ZIP containers and binary tables using native R, and
extracts metadata and raw measurement channels. No Python or vendor software
is required.}
\value{An S3 \code{ngb_file} list with four components:
\describe{
\item{metadata}{Named list of extracted file metadata. On a sample-plus-correction
file this describes the sample. Optional fields are omitted when absent.
\code{measurement_type} is \code{sample_correction} or \code{correction}.}
\item{data}{Data frame of raw measurements: the sample in a \code{.ngb-ds3}
file, or the baseline in a \code{.ngb-bs3} file.}
\item{baseline}{Data frame of the embedded correction run in a \code{.ngb-ds3}
file, or \code{NULL} in a \code{.ngb-bs3} file.}
\item{column_metadata}{Named list containing units, descriptions, source,
processing history and processing flags for each channel. The same metadata
applies to embedded baseline channels; their signature is checked. Unknown
channel units are \code{NA_character_}.}
}}
\details{Time and temperature-program durations are returned in seconds
(stored minutes are multiplied by 60). Mass is raw mass change in mg;
initial sample mass is separate metadata. DSC is raw microvolts. No baseline
subtraction, mass normalization or sensitivity calibration is applied.
Temperature calibration records are provenance: recorded sample temperatures
are already calibrated by the instrument.

Baseline metadata can carry a negative sample-mass sentinel (such as -1000);
it is preserved without interpreting it as a physical mass. Timestamps are
ISO 8601 UTC strings; the instrument timezone and UTC offset are separate fields.
Metadata includes sample and instrument identity, masses, temperature program,
PID settings, gas settings and temperature/DSC calibration records when present.
The parser does not extract every proprietary field or streams 4--6.

Corrupt measurement records, inconsistent channels, invalid section directories
and run-count mismatches raise \code{ngb_corrupt_error}. Resource limits raise
\code{ngb_resource_error}; both inherit from \code{ngb_error}. These conditions
include \code{stream} and \code{offset} where known. Malformed metadata records
warn and allow extraction of available fields. File-extension and input errors
are ordinary R errors.}
\examples{
\dontrun{
sample <- read_ngb("measurement.ngb-ds3")
sample$metadata$sample_mass
head(sample$data)
head(sample$baseline)
sample$column_metadata$mass
baseline <- read_ngb("baseline.ngb-bs3")
head(baseline$data)
}}
\seealso{\code{\link{print.ngb_file}}}
