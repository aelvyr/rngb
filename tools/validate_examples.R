# Run from the package root; source the R implementation directly so this
# also verifies the development checkout without installing it.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 3L)
  stop("Usage: Rscript tools/validate_examples.R sample.ngb-ds3 baseline.ngb-bs3 [reference_dir]")
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)
timing <- system.time({ sample <- read_ngb(args[1]); baseline <- read_ngb(args[2]) })
stopifnot(inherits(sample, "ngb_file"), inherits(baseline, "ngb_file"),
  identical(names(sample), c("metadata", "data", "baseline", "column_metadata")),
  nrow(sample$data) == 16951L, nrow(sample$baseline) == 16950L,
  nrow(baseline$data) == 16950L, is.null(baseline$baseline),
  identical(sample$baseline, baseline$data),
  identical(names(sample$data), c("time", "sample_temperature", "dsc_signal",
    "purge_flow_2", "protective_flow", "mass", "furnace_temperature", "cooling_power", "furnace_power")),
  sample$metadata$sample_mass == 19.777, baseline$metadata$sample_mass == -1000,
  sample$metadata$crucible_mass == 147.875,
  sample$metadata$reference_crucible_mass == 145.076,
  sample$metadata$instrument == "STA449F3A-1472-M",
  sample$metadata$date_performed == "2026-09-02T13:50:47+00:00",
  baseline$metadata$date_performed == "2026-09-02T11:49:57+00:00",
  sample$metadata$utc_offset_minutes == 120,
  length(sample$metadata$temperature_program) == 7L,
  length(sample$metadata$temperature_calibration$fixpoints) == 6L,
  length(sample$metadata$sensitivity_calibration$fixpoints) == 6L,
  sample$column_metadata$time$units == "s", sample$column_metadata$mass$units == "mg",
  sample$column_metadata$dsc_signal$units == "µV",
  !sample$column_metadata$mass$baseline_subtracted)

source("tools/parity_helpers.R")
if (length(args) == 3L) {
  for (i in 1:2) {
    result <- list(sample, baseline)[[i]]
    dir <- file.path(args[3], basename(args[i]))
    expected <- dget(file.path(dir, "metadata.R"))
    # R explicitly identifies legacy baseline files even when upstream omits
    # measurement_type; all other fields must match without exceptions.
    if (is.null(expected$measurement_type)) expected$measurement_type <- "correction"
    compare_metadata(result$metadata, expected)
    manifest <- dget(file.path(dir, "manifest.R"))
    frames <- list(result$data, result$baseline)
    for (run in seq_along(manifest)) {
      frame <- frames[[run]]
      stopifnot(identical(names(frame), manifest[[run]]))
      for (column in names(frame)) {
        path <- file.path(dir, paste0("run_", run - 1L, "_", column, ".bin"))
        raw <- readBin(path, "raw", n = file.info(path)$size)
        stopifnot(identical(writeBin(frame[[column]], raw(), size = 8L, endian = "little"), raw))
      }
    }
    cat(basename(args[i]), ": all metadata and all channel bytes match the pinned Python parser\n", sep = "")
  }
}
cat("Sample:", nrow(sample$data), "rows; baseline:", nrow(baseline$data), "rows; 9 channels\n")
cat("Embedded and external baseline: exactly identical\n")
cat("Elapsed time for both R parses:", timing[["elapsed"]], "seconds\n")
