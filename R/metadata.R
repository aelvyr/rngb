# Table-scoped metadata mappings adapted from pyngb, MIT (see LICENSE).
.ngb_field <- function(table, id) table$fields[[as.character(id)]]
.ngb_value <- function(table, id) .ngb_field(table, id)$value
.ngb_has <- function(table, ids) all(as.character(ids) %in% names(table$fields))
.ngb_find <- function(tables, category = NULL, type = NULL, fields = numeric()) {
  Filter(function(t) (is.null(category) || t$category == category) &&
    (is.null(type) || t$type == type) && .ngb_has(t, fields), tables)
}
.ngb_first <- function(tables, category = NULL, type = NULL, fields = numeric()) {
  found <- .ngb_find(tables, category, type, fields)
  if (length(found)) found[[1]] else NULL
}
.ngb_clean <- function(value) {
  if (!is.character(value) || length(value) != 1L) return(NULL)
  value <- trimws(value); if (nzchar(value)) value else NULL
}
.ngb_number <- function(value) {
  if (is.numeric(value) && length(value) == 1L) as.numeric(value) else NULL
}
.ngb_date <- function(value) {
  if (is.null(.ngb_number(value)) || !is.finite(value)) return(NULL)
  format(as.POSIXct(value, origin = "1970-01-01", tz = "UTC"),
         "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC")
}
.ngb_last_number <- function(table) {
  for (f in rev(table$fields)) {
    if (f$dtype %in% c(3L, 4L, 5L) && !is.null(.ngb_number(f$value))) return(f$value)
  }
  NULL
}
.ngb_stage_ids <- c(stage_type = 0x083f, temperature = 0x0e17,
                    heating_rate = 0x0e13, acquisition_rate = 0x0e14, time = 0x0e15)
.ngb_stage <- function(table) {
  if (table$type == 0x2b0c && .ngb_has(table, .ngb_stage_ids) && table$category >= 0x7530)
    table$category - 0x7530 else NULL
}
.ngb_role <- function(table) {
  id <- .ngb_value(table, 0x083f)
  if (is.null(id) || !id %in% 30:32) return(NULL)
  c("purge_1", "purge_2", "protective")[id - 29]
}
.ngb_metadata <- function(tables) {
  if (!length(tables)) return(list())
  prologues <- which(vapply(tables, function(t) t$type == 0x2afd, logical(1)))
  if (length(prologues) >= 2L) tables <- tables[seq_len(prologues[2] - 1L)]
  out <- list()
  mappings <- list(
    instrument = c(0x1775, 0x1059), project = c(0x1772, 0x083c),
    date_performed = c(0x1772, 0x083e), lab = c(0x1772, 0x0834),
    operator = c(0x1772, 0x0835), crucible_type = c(0x177e, 0x0840),
    comment = c(0x1772, 0x083d), furnace_type = c(0x177a, 0x0840),
    carrier_type = c(0x1779, 0x0840), sample_id = c(0x7530, 0x0898),
    sample_name = c(0x7530, 0x0840), material = c(0x7530, 0x0962),
    sample_mass = c(0x7530, 0x0c9e))
  for (name in names(mappings)) {
    map <- mappings[[name]]; table <- .ngb_first(tables, map[1], fields = map[2])
    value <- .ngb_value(table, map[2])
    value <- if (name == "date_performed") .ngb_date(value) else if (name == "sample_mass") {
      v <- .ngb_number(value); if (!is.null(v) && is.finite(v) && v > 0) v else NULL
    } else .ngb_clean(value)
    if (!is.null(value)) out[[name]] <- value
  }
  # Crucible masses are identified by the preceding table, not occurrence order.
  masses <- which(vapply(tables, function(t) t$category == 0x177e &&
    !is.null(.ngb_number(.ngb_value(t, 0x0c9e))), logical(1)))
  zeros <- numeric()
  for (i in masses) {
    value <- .ngb_value(tables[[i]], 0x0c9e)
    previous <- if (i > 1L) tables[[i - 1L]] else NULL
    if (.ngb_has(previous, 0x0c83)) {
      if (is.null(out$crucible_mass)) {
        out$crucible_mass <- value
        if (is.null(out$sample_mass)) out$sample_mass <- .ngb_last_number(previous)
      }
    } else if (.ngb_has(previous, 0x10c4)) {
      if (is.null(out$reference_crucible_mass)) {
        out$reference_crucible_mass <- value; out$reference_mass <- .ngb_last_number(previous)
      }
    } else if (abs(value) < 1e-12) zeros <- c(zeros, value)
  }
  if (!is.null(out$crucible_mass) && is.null(out$reference_crucible_mass) && length(zeros))
    out$reference_crucible_mass <- zeros[1]
  if (is.null(out$crucible_mass) && length(masses)) out$crucible_mass <- .ngb_value(tables[[masses[1]]], 0x0c9e)
  # Program ordinals come from categories; serialization order can differ.
  program <- list(); current <- NULL; setpoints <- list()
  for (i in seq_along(tables)) {
    t <- tables[[i]]; ordinal <- .ngb_stage(t)
    if (!is.null(ordinal)) {
      current <- paste0("stage_", ordinal)
      if (is.null(program[[current]])) {
        stage <- list()
        for (key in names(.ngb_stage_ids)) {
          f <- .ngb_field(t, .ngb_stage_ids[key])
          if (f$dtype == if (key == "stage_type") 3L else 4L) {
            value <- .ngb_number(f$value)
            if (!is.null(value)) stage[[key]] <- if (key == "time") value * 60 else value
          }
        }
        program[[current]] <- stage
      }
    } else if (!is.null(current) && t$type == 0x2b11) {
      role <- .ngb_role(t)
      if (!is.null(role) && i < length(tables)) {
        next_table <- tables[[i + 1L]]; f <- .ngb_field(next_table, 0x1047)
        if (next_table$type == 0x2b0a && !is.null(f) && f$dtype == 4L) {
          key <- paste0(role, "_mfc_flow")
          if (is.null(setpoints[[current]][[key]])) setpoints[[current]][[key]] <- f$value
        }
      }
    }
  }
  if (length(program)) {
    for (key in intersect(names(program), names(setpoints))) program[[key]] <- c(program[[key]], setpoints[[key]])
    order <- order(as.numeric(sub("stage_", "", names(program))))
    out$temperature_program <- program[order]
  }
  pid <- .ngb_find(tables, fields = c(0x0fe7, 0x0fe8, 0x0fe9))
  for (i in seq_len(min(2L, length(pid)))) {
    prefix <- c("furnace", "sample")[i]
    for (key in c("xp", "tn", "tv")) {
      value <- .ngb_number(.ngb_value(pid[[i]], c(xp = 0x0fe7, tn = 0x0fe8, tv = 0x0fe9)[key]))
      if (!is.null(value)) out[[paste0(prefix, "_", key)]] <- value
    }
  }
  # Gas identity comes from device definitions and GUID-matched gas records.
  for (i in seq_along(tables)) {
    t <- tables[[i]]
    if (t$type != 0x2b07 || !identical(.ngb_value(t, 0x104b), 2)) next
    role <- .ngb_role(t); if (is.null(role)) next
    range <- NULL; formula <- NULL; guid <- .ngb_value(t, 0x0c8f)
    if (i < length(tables)) for (j in seq.int(i + 1L, length(tables))) {
      follower <- tables[[j]]
      if (follower$type %in% c(0x2b07, 0x2b11)) break
      f <- .ngb_field(follower, 0x1048)
      if (is.null(range) && follower$type == 0x2b0a && !is.null(f) && f$dtype == 4L)
        range <- .ngb_number(f$value)
      if (is.null(formula) && follower$type == 0x2b81 && !is.null(.ngb_clean(guid)) &&
          identical(.ngb_value(follower, 0x17fc), guid)) formula <- .ngb_clean(.ngb_value(follower, 0x0c88))
      if (!is.null(range) && !is.null(formula)) break
    }
    values <- list(gas = .ngb_clean(.ngb_value(t, 0x0840)), range = range, gas_formula = formula)
    for (key in names(values)) {
      name <- paste0(role, "_mfc_", key)
      if (is.null(out[[name]]) && !is.null(values[[key]])) out[[name]] <- values[[key]]
    }
  }
  body <- Filter(function(s) identical(s$stage_type, 1), program)
  if (!length(body)) body <- Filter(function(s) any(grepl("_mfc_flow$", names(s))), program)
  for (role in c("purge_1", "purge_2", "protective")) {
    key <- paste0(role, "_mfc_flow")
    if (length(body) && all(vapply(body, function(s) !is.null(s[[key]]), logical(1)))) {
      values <- unique(vapply(body, function(s) s[[key]], numeric(1)))
      if (length(values) == 1L) out[[key]] <- values
    }
  }
  constants <- c(p0 = 0x044f, p1 = 0x0450, p2 = 0x0451, p3 = 0x0452, p4 = 0x0453, p5 = 0x04c3)
  for (t in .ngb_find(tables, category = 0x01f5)) {
    values <- lapply(constants, function(id) .ngb_number(.ngb_value(t, id)))
    values <- Filter(Negate(is.null), values)
    if (length(values)) { out$calibration_constants <- values; break }
  }
  out$temperature_calibration <- .ngb_calibration(tables, "temperature")
  out$sensitivity_calibration <- .ngb_calibration(tables, "sensitivity")
  main_start <- if (length(prologues)) prologues[1] else 1L
  tz <- .ngb_first(tables[seq.int(main_start, length(tables))], category = 0x1859)
  if (is.null(tz)) tz <- .ngb_first(tables, category = 0x1859)
  out$timezone <- .ngb_clean(.ngb_value(tz, 0x1135))
  bias <- .ngb_number(.ngb_value(tz, 0x1134))
  if (!is.null(bias)) {
    offset <- -bias; dst <- .ngb_number(.ngb_value(tz, 0x1137))
    if (identical(.ngb_value(tz, 0x1138), 2) && !is.null(dst)) offset <- offset - dst
    out$utc_offset_minutes <- offset
  }
  link <- .ngb_first(tables, 0x1770, fields = 0x0843)
  out$correction_file_path <- .ngb_clean(.ngb_value(link, 0x0843))
  strings_of <- function(ts) unlist(lapply(ts, function(t) lapply(Filter(function(f)
    f$dtype == 31L && !is.null(f$value), t$fields), function(f) f$value)), use.names = FALSE)
  strings <- strings_of(.ngb_find(tables, category = 0x0300))
  if (!length(strings)) strings <- strings_of(tables)
  version <- strings[grepl("^\\s*Version\\s+[0-9]+\\.[0-9]+\\.[0-9]+", strings, perl = TRUE)]
  if (length(version)) out$application_version <- version[1]
  licenses <- strings[grepl("\n", strings, fixed = TRUE) & !grepl("^Version", trimws(strings))]
  if (length(licenses)) out$licensed_to <- licenses[which.max(nchar(licenses))]
  model <- .ngb_find(tables, type = 0x2b17)
  for (t in model) {
    value <- .ngb_clean(.ngb_value(t, 0x0432))
    if (!is.null(value)) { out$instrument_model <- value; break }
  }
  kind <- .ngb_value(.ngb_first(tables, 0x1770, fields = 0x103a), 0x103a)
  if (!is.null(kind) && kind %in% 1:3) out$measurement_type <- c("correction", "sample", "sample_correction")[kind]
  for (t in .ngb_find(tables, type = 0x2b06)) {
    name <- .ngb_channel_name(t$category)
    if (!name %in% c("mass", "dsc_signal")) next
    key <- if (name == "mass") "mass_range" else "dsc_range"
    f <- .ngb_field(t, 0x0bbc); value <- .ngb_number(f$value)
    if (is.null(out[[key]]) && !is.null(f) && f$dtype == 4L && !is.null(value) && value > 0) out[[key]] <- value
  }
  out
}

.ngb_calibration <- function(tables, family) {
  temperature <- family == "temperature"
  mapping <- if (temperature) c(name = 0x0443, actual_c = 0x0444, measured_c = 0x0445,
    weight = 0x0446, corrected_c = 0x0447) else c(name = 0x0443, temperature_c = 0x0454,
    enthalpy = 0x0455, peak_area = 0x0456, measured_sensitivity = 0x0445,
    weight = 0x0446, fitted_sensitivity = 0x0447)
  required <- if (temperature) c(0x0444, 0x0447) else 0x0454
  excluded <- if (temperature) 0x0454 else 0x0444
  out <- list(); fixpoints <- list()
  if (temperature) {
    coeff <- .ngb_field(.ngb_first(tables, 0x01f7, fields = 0x04be), 0x04be)
    if (!is.null(coeff) && coeff$dtype == 16L && length(coeff$raw) > 0 && length(coeff$raw) %% 4L == 0L)
      out$coefficients <- .ngb_numeric(coeff$raw, 4L)
  }
  for (category in 0x7530:0x753f) {
    candidates <- Filter(function(t) !.ngb_has(t, excluded), .ngb_find(tables, category, fields = required))
    if (!length(candidates)) next
    row <- lapply(names(mapping), function(key) {
      value <- .ngb_value(candidates[[1]], mapping[key])
      if (key == "name") .ngb_clean(value) else .ngb_number(value)
    })
    names(row) <- names(mapping); row <- Filter(Negate(is.null), row)
    if (length(row)) fixpoints[[length(fixpoints) + 1L]] <- row
  }
  if (length(fixpoints)) out$fixpoints <- fixpoints
  records <- .ngb_find(tables, type = if (temperature) 0x2422 else 0x23f0, fields = 0x07d4)
  for (t in records) {
    path <- .ngb_clean(.ngb_value(t, 0x07d4)); if (is.null(path)) next
    out$record_path <- path
    map <- list(date_measured = 0x083e, gas = 0x0431, crucible_type = c(0x044c, 0x0433),
                heating_rate = 0x0435, comment = 0x083d)
    for (key in names(map)) {
      value <- NULL
      for (id in map[[key]]) { value <- .ngb_value(t, id); if (!is.null(value)) break }
      value <- if (key == "date_measured") {
        v <- .ngb_number(value); if (!is.null(v) && v > 0) .ngb_date(v) else NULL
      } else if (key == "heating_rate") {
        v <- .ngb_number(value); if (!is.null(v) && v > 0) v else NULL
      } else .ngb_clean(value)
      if (!is.null(value)) out[[key]] <- value
    }
    break
  }
  if (length(out)) out else NULL
}
