.ngb_channel_names <- c(`140` = "time", `141` = "sample_temperature",
  `142` = "dsc_signal", `156` = "purge_flow_1", `157` = "purge_flow_2",
  `158` = "protective_flow", `144` = "mass", `48` = "furnace_temperature",
  `49` = "cooling_power", `50` = "furnace_power", `51` = "h_foil_temperature",
  `52` = "uc_module", `53` = "environmental_pressure",
  `54` = "environmental_acceleration_x", `55` = "environmental_acceleration_y",
  `56` = "environmental_acceleration_z")
.ngb_channel_name <- function(category) {
  id <- category %% 256; name <- .ngb_channel_names[as.character(id)]
  if (is.na(name)) sprintf("%02x", as.integer(id)) else unname(name)
}
.ngb_channels <- function(documents) {
  all_runs <- list(); counts <- integer()
  for (sid in intersect(c("2", "3"), names(documents))) {
    tables <- documents[[sid]]$tables
    if (!any(vapply(tables, function(t) t$type == 0x2b22, logical(1)))) {
      if (any(vapply(tables, function(t) t$type == 0x2b23, logical(1))))
        .ngb_error("Measurement values precede any channel header", stream = as.integer(sid))
      next
    }
    runs <- list(list()); signatures <- list(numeric()); run <- 1L
    seen <- numeric(); title <- NULL; chunks <- list()
    flush <- function() {
      if (length(chunks)) {
        if (is.null(title)) .ngb_error("Measurement values precede a channel header")
        values <- unlist(chunks, use.names = FALSE)
        if (title == "time") values <- values * 60
        if (title %in% names(runs[[run]])) .ngb_error("Duplicate measurement channel")
        runs[[run]][[title]] <<- values
      }
      chunks <<- list()
    }
    for (table in tables) {
      if (table$type == 0x2b22) {
        flush()
        if (table$category %in% seen) {
          run <- run + 1L; runs[[run]] <- list(); signatures[[run]] <- numeric(); seen <- numeric()
        }
        seen <- c(seen, table$category)
        signatures[[run]] <- c(signatures[[run]], table$category)
        title <- .ngb_channel_name(table$category)
      } else if (table$type == 0x2b23) {
        arrays <- Filter(function(f) f$mode == 0x01a0 && !is.null(f$count) && f$count > 0 &&
          ((f$id == 0x0f40 && f$dtype == 5L) || (f$id == 0x0f3d && f$dtype == 4L)), table$fields)
        if (length(arrays) > 1L) .ngb_error("Segment contains multiple measurement arrays")
        if (length(arrays)) {
          if (is.null(title)) .ngb_error("Measurement values precede a channel header")
          chunks[[length(chunks) + 1L]] <- .ngb_numeric(arrays[[1]]$raw, arrays[[1]]$dtype)
        }
      }
    }
    flush()
    if (any(!vapply(signatures, identical, logical(1), signatures[[1]])))
      .ngb_error("Measurement runs disagree on their channel signature", stream = as.integer(sid))
    counts <- c(counts, length(runs)); all_runs[[sid]] <- runs
  }
  if (!length(counts)) .ngb_error("No measurement data found")
  if (length(unique(counts)) != 1L) .ngb_error("Data streams disagree on measurement-run count")
  result <- vector("list", counts[1])
  for (i in seq_len(counts[1])) {
    columns <- list()
    for (stream in all_runs) {
      if (length(intersect(names(columns), names(stream[[i]]))))
        .ngb_error("Duplicate channels across measurement streams")
      columns <- c(columns, stream[[i]])
    }
    lengths <- vapply(columns, length, integer(1))
    if (!length(lengths) || any(lengths == 0L) || length(unique(lengths)) != 1L)
      .ngb_error("Measurement channels have missing or inconsistent lengths")
    result[[i]] <- as.data.frame(columns, optional = TRUE, check.names = FALSE,
                                 stringsAsFactors = FALSE)
  }
  if (any(!vapply(result, function(frame) identical(names(frame), names(result[[1]])), logical(1))))
    .ngb_error("Measurement runs have missing or inconsistent data channels")
  result
}
.ngb_column_metadata <- function(columns) {
  units <- c(time = "s", sample_temperature = "\u00b0C", furnace_temperature = "\u00b0C",
             dsc_signal = "\u00b5V", mass = "mg", purge_flow_1 = "ml/min",
             purge_flow_2 = "ml/min", protective_flow = "ml/min",
             cooling_power = "%", furnace_power = "%", h_foil_temperature = "\u00b0C")
  descriptions <- c(time = "Elapsed measurement time",
    sample_temperature = "Recorded sample temperature (instrument calibrated)",
    furnace_temperature = "Recorded furnace temperature", mass = "Raw mass change",
    dsc_signal = "Raw DSC signal (sensitivity calibration not applied)")
  result <- lapply(columns, function(name) {
    unit <- units[name]; description <- descriptions[name]
    list(units = if (is.na(unit)) NA_character_ else unname(unit),
         description = if (is.na(description)) name else unname(description),
         source = "NGB measurement stream",
         processing_history = if (name == "time") "minutes_to_seconds" else character(),
         baseline_subtracted = FALSE, calibration_applied = FALSE)
  })
  names(result) <- columns; result
}
