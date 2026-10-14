#' Read a NETZSCH STA NGB file
#'
#' Extract raw measurements and metadata without baseline subtraction,
#' normalization or DSC calibration. Time is returned in seconds.
#' @param path Path to an .ngb-ds3 or .ngb-bs3 file.
#' @param limits Named list overriding max_stream_size_mb (1000),
#'   max_array_size_mb (500), or max_tables_per_stream (10000).
#' @return An ngb_file list with metadata, data, baseline and column_metadata.
#' @export
read_ngb <- function(path, limits = list()) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop("path must be one non-empty file path", call. = FALSE)
  if (!file.exists(path) || dir.exists(path))
    stop("File not found: ", path, call. = FALSE)
  extension <- tolower(sub("^.*\\.", "", basename(path)))
  if (!extension %in% c("ngb-ds3", "ngb-bs3"))
    stop("Supported extensions are .ngb-ds3 and .ngb-bs3", call. = FALSE)
  limits <- .ngb_limits(limits)
  streams <- .ngb_open(path, limits)
  documents <- lapply(names(streams), function(id)
    .ngb_document(streams[[id]], as.integer(id), limits))
  names(documents) <- names(streams)
  for (id in intersect(c("2", "3"), names(documents))) {
    if (length(documents[[id]]$defects))
      .ngb_error(paste("Damaged measurement stream", id),
                 stream = as.integer(id), offset = documents[[id]]$defects[[1]]$offset)
  }
  if (length(documents[["1"]]$defects))
    warning("Metadata stream contains malformed or truncated records; metadata may be incomplete",
            call. = FALSE)
  runs <- .ngb_channels(documents)
  expected <- if (extension == "ngb-ds3") 2L else 1L
  if (length(runs) != expected)
    .ngb_error(sprintf(".%s requires %d measurement run(s), found %d",
                       extension, expected, length(runs)))
  metadata <- .ngb_metadata(documents[["1"]]$tables)
  # Older Proteus files can omit the measurement-kind field. Record the kind
  # established by the extension and validated run structure in that case.
  kind <- if (expected == 2L) "sample_correction" else "correction"
  if (!is.null(metadata$measurement_type) && metadata$measurement_type != kind)
    .ngb_error("Measurement kind conflicts with file extension and run structure")
  metadata$measurement_type <- kind
  result <- list(metadata = metadata, data = runs[[1]],
                 baseline = if (expected == 2L) runs[[2]] else NULL,
                 column_metadata = .ngb_column_metadata(names(runs[[1]])))
  structure(result, class = c("ngb_file", "list"))
}

#' @export
print.ngb_file <- function(x, ...) {
  cat("NGB", x$metadata$measurement_type, "file\n")
  if (!is.null(x$metadata$sample_name)) cat("Sample:", x$metadata$sample_name, "\n")
  cat("Data:", nrow(x$data), "rows x", ncol(x$data), "channels\n")
  if (!is.null(x$baseline)) cat("Embedded baseline:", nrow(x$baseline), "rows\n")
  invisible(x)
}

.ngb_limits <- function(overrides) {
  defaults <- list(max_stream_size_mb = 1000, max_array_size_mb = 500,
                   max_tables_per_stream = 10000)
  if (!is.list(overrides) || (length(overrides) &&
      (is.null(names(overrides)) || any(!names(overrides) %in% names(defaults)) ||
       anyDuplicated(names(overrides)))))
    stop("limits must be a named list of supported resource limits", call. = FALSE)
  if (any(!vapply(overrides, function(x) is.numeric(x) && length(x) == 1L &&
                 !is.na(x) && is.finite(x) && x > 0, logical(1))))
    stop("Resource limits must be finite positive numbers", call. = FALSE)
  for (name in names(overrides)) defaults[[name]] <- overrides[[name]]
  defaults
}

.ngb_error <- function(message, stream = NULL, offset = NULL,
                       class = "ngb_corrupt_error") {
  stop(structure(list(message = message, call = NULL, stream = stream,
                      offset = offset),
                 class = c(class, "ngb_error", "error", "condition")))
}
