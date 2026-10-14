# Generic full-output parity check for supported upstream and local fixtures.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("Usage: Rscript tools/validate_parity.R reference_dir file.ngb-ds3 [file.ngb-bs3 ...]")
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)
source("tools/parity_helpers.R")
for (path in args[-1]) {
  result <- read_ngb(path)
  directory <- file.path(args[1], basename(path))
  expected <- dget(file.path(directory, "metadata.R"))
  if (is.null(expected$measurement_type)) expected$measurement_type <- result$metadata$measurement_type
  compare_metadata(result$metadata, expected)
  manifest <- dget(file.path(directory, "manifest.R"))
  frames <- list(result$data, result$baseline)
  stopifnot(length(manifest) == if (is.null(result$baseline)) 1L else 2L)
  for (run in seq_along(manifest)) {
    frame <- frames[[run]]
    stopifnot(identical(names(frame), manifest[[run]]))
    for (column in names(frame)) {
      file <- file.path(directory, paste0("run_", run - 1L, "_", column, ".bin"))
      expected_raw <- readBin(file, "raw", n = file.info(file)$size)
      actual_raw <- writeBin(frame[[column]], raw(), size = 8L, endian = "little")
      if (!identical(actual_raw, expected_raw)) stop("Channel bytes differ: ", basename(path), "/", run, "/", column)
    }
  }
  cat(basename(path), ": exact metadata and channel parity\n", sep = "")
}
