# Byte grammar adapted from pyngb (Grayson Bellamy), MIT; see LICENSE.
# All internal byte offsets are zero based, with exclusive end positions.
.ngb_bytes <- function(...) as.raw(c(...))
.ngb_anchor <- .ngb_bytes(0x18, 0xfc, 0xff, 0xff)
.ngb_header <- c(.ngb_anchor, .ngb_bytes(3, 0x80, 1))
.ngb_middle <- .ngb_bytes(0, 0, 1, 0, 0, 0, 0x0c, 0, 0x17, 0xfc, 0xff, 0xff)
.ngb_end <- .ngb_bytes(1, 0, 0, 0, 2, 0, 1, 0, 0)
.ngb_sizes <- c(`0` = 0, `2` = 2, `3` = 4, `4` = 4, `5` = 8,
                `16` = 1, `20` = 8, `72` = 16)

.ngb_slice <- function(raw, start, end) {
  if (end <= start) return(raw(0))
  raw[seq.int(start + 1, end)]
}
.ngb_matches <- function(raw, pos, pattern, end = length(raw)) {
  pos >= 0 && pos + length(pattern) <= end &&
    identical(.ngb_slice(raw, pos, pos + length(pattern)), pattern)
}
.ngb_uint <- function(raw, pos, size) {
  sum(as.integer(.ngb_slice(raw, pos, pos + size)) * 256^(seq_len(size) - 1))
}
.ngb_numeric <- function(payload, dtype) {
  if (dtype %in% c(4L, 5L)) {
    size <- if (dtype == 4L) 4L else 8L
    return(readBin(payload, "double", n = length(payload) / size,
                   size = size, endian = "little"))
  }
  if (dtype %in% c(2L, 3L)) {
    size <- if (dtype == 2L) 2L else 4L
    if (!length(payload)) return(numeric())
    bytes <- matrix(as.integer(payload), nrow = size)
    values <- colSums(bytes * 256^(seq_len(size) - 1))
    if (dtype == 3L) values[values >= 2^31] <- values[values >= 2^31] - 2^32
    return(as.numeric(values))
  }
  payload
}
.ngb_utf16 <- function(raw) {
  if (length(raw) %% 2L) return(NULL)
  codes <- .ngb_numeric(raw, 2L)
  points <- numeric(length(codes)); n <- 0L; i <- 1L
  while (i <= length(codes)) {
    code <- codes[i]
    if (code >= 0xd800 && code <= 0xdbff) {
      if (i < length(codes) && codes[i + 1L] >= 0xdc00 && codes[i + 1L] <= 0xdfff) {
        code <- 0x10000 + (code - 0xd800) * 1024 + codes[i + 1L] - 0xdc00
        i <- i + 1L
      } else { i <- i + 1L; next }
    } else if (code >= 0xdc00 && code <= 0xdfff) { i <- i + 1L; next }
    n <- n + 1L; points[n] <- code; i <- i + 1L
  }
  # R strings cannot contain embedded NULs.
  points <- points[seq_len(n)]; intToUtf8(points[points != 0])
}
.ngb_string <- function(payload) {
  if (length(payload) < 4L) return(NULL)
  proprietary <- .ngb_matches(payload, 0, .ngb_bytes(0xff, 0xfe, 0xff))
  if (proprietary) {
    size <- 2 * as.integer(payload[4L])
    if (size + 4 > length(payload)) return(NULL)
    value <- .ngb_utf16(.ngb_slice(payload, 4, 4 + size))
  } else {
    size <- .ngb_uint(payload, 0, 4)
    if (size <= 0 || size + 4 > length(payload)) return(NULL)
    bytes <- .ngb_slice(payload, 4, 4 + size)
    value <- rawToChar(bytes[bytes != as.raw(0)])
    value <- iconv(value, from = "UTF-8", to = "UTF-8", sub = NA)
    if (is.na(value)) value <- .ngb_utf16(bytes)
    if (!is.null(value)) value <- trimws(value)
  }
  if (is.null(value) || !nzchar(value)) NULL else value
}
.ngb_scalar <- function(payload, dtype) {
  if (dtype == 0L) return(NULL)
  if (dtype == 31L) return(.ngb_string(payload))
  if (dtype == 16L) return(as.numeric(as.integer(payload)))
  .ngb_numeric(payload, dtype)
}

.ngb_open <- function(path, limits) {
  listing <- tryCatch(utils::unzip(path, list = TRUE),
                      error = function(e) .ngb_error("Invalid ZIP archive"))
  required <- paste0("Streams/stream_", 1:2, ".table")
  if (!all(required %in% listing$Name)) .ngb_error("NGB archive requires streams 1 and 2")
  wanted <- paste0("Streams/stream_", 1:3, ".table")
  wanted <- wanted[wanted %in% listing$Name]
  result <- list()
  for (member in wanted) {
    rows <- which(listing$Name == member)
    if (length(rows) != 1L) .ngb_error("Duplicate ZIP stream member")
    size <- listing$Length[rows]
    if (size > limits$max_stream_size_mb * 1024^2)
      .ngb_error("ZIP stream exceeds max_stream_size_mb", class = "ngb_resource_error")
    con <- unz(path, member, open = "rb")
    payload <- tryCatch(readBin(con, "raw", n = size + 1),
                        error = function(e) .ngb_error("Cannot decompress NGB stream"),
                        finally = close(con))
    if (length(payload) != size) .ngb_error("ZIP stream size mismatch")
    id <- as.integer(sub(".*stream_([0-9]+).*", "\\1", member))
    result[[as.character(id)]] <- .ngb_container(payload, id)
  }
  result
}
.ngb_container <- function(raw, id) {
  size <- length(raw)
  if (size < 94 || !.ngb_matches(raw, 2, charToRaw("Netzsch TA file")) ||
      !.ngb_matches(raw, 28, charToRaw("_db_format_1")))
    .ngb_error("Invalid NGB stream header", stream = id)
  entries <- list(); pos <- 80
  while (pos + 14 <= size && .ngb_matches(raw, pos, .ngb_bytes(255, 255))) {
    entry <- c(id = .ngb_uint(raw, pos + 2, 2),
               offset = .ngb_uint(raw, pos + 4, 4),
               size = .ngb_uint(raw, pos + 8, 4))
    if (all(entry == 0)) break
    entries[[length(entries) + 1L]] <- entry; pos <- pos + 14
  }
  if (!length(entries)) .ngb_error("Empty section directory", stream = id)
  directory <- do.call(rbind, entries)
  ends <- directory[, "offset"] + directory[, "size"]
  if (directory[1, "offset"] < pos || utils::tail(ends, 1) != size ||
      (nrow(directory) > 1 && any(directory[-1, "offset"] != utils::head(ends, -1))) ||
      !id %in% directory[, "id"])
    .ngb_error("Invalid or non-contiguous section directory", stream = id)
  list(raw = raw, directory = directory)
}

.ngb_terminator <- function(raw, pos, end) {
  variants <- list(.ngb_end, .ngb_bytes(1, 0, 0, 0, 4, 0, 1, 0, 0, 0, 3, 0),
                   .ngb_bytes(1, 0, 0, 0, 3, 0))
  for (v in variants) if (.ngb_matches(raw, pos, v, end)) return(length(v))
  NULL
}
.ngb_record <- function(raw, pos, end, limits) {
  if (pos + 24 > end || !.ngb_matches(raw, pos, .ngb_header, end) ||
      !.ngb_matches(raw, pos + 9, .ngb_middle, end)) return(NULL)
  dtype <- as.integer(raw[pos + 22]); start <- pos + 24
  mode <- .ngb_uint(raw, pos + 22, 2); count <- NULL
  size <- .ngb_sizes[as.character(dtype)]
  if (mode == 0x0180) {
    if (dtype == 31L) {
      if (start + 4 > end) return(NULL)
      size <- if (.ngb_matches(raw, start, .ngb_bytes(255, 254, 255), end))
        4 + 2 * as.integer(raw[start + 4]) else 4 + .ngb_uint(raw, start, 4)
    } else if (dtype == 26L) {
      search_end <- min(start + 256 + length(.ngb_end), end)
      candidates <- which(.ngb_slice(raw, start, search_end) == as.raw(1)) - 1L + start
      hits <- candidates[vapply(candidates, function(p) .ngb_matches(raw, p, .ngb_end, search_end), logical(1))]
      if (!length(hits)) return(NULL)
      size <- hits[1] - start
    } else if (is.na(size)) return(NULL)
  } else if (mode == 0x01a0) {
    if (is.na(size) || start + 4 > end) return(NULL)
    count <- .ngb_uint(raw, start, 4); size <- size * count; start <- start + 4
    if (start + size + 6 > end) return(list(truncated = TRUE))
  } else return(NULL)
  finish <- start + size
  term <- .ngb_terminator(raw, finish, end)
  if (is.null(term)) return(NULL)
  if (mode == 0x01a0 && size > limits$max_array_size_mb * 1024^2)
    .ngb_error("Array exceeds max_array_size_mb", offset = pos, class = "ngb_resource_error")
  payload <- .ngb_slice(raw, start, finish)
  list(end = finish + term, id = .ngb_uint(raw, pos + 7, 2), dtype = dtype,
       mode = mode, raw = payload, count = count,
       value = if (mode == 0x0180) .ngb_scalar(payload, dtype) else NULL)
}
.ngb_bare <- function(raw, pos, end) {
  if (pos + 24 > end || !.ngb_matches(raw, pos, .ngb_header, end) ||
      !.ngb_matches(raw, pos + 9, .ngb_middle, end) ||
      .ngb_uint(raw, pos + 22, 2) != 0x0180) return(FALSE)
  size <- .ngb_sizes[as.character(as.integer(raw[pos + 22]))]
  !is.na(size) && pos + 24 + size == end
}
.ngb_gap <- function(raw, start, end, saw) {
  if (!saw && .ngb_matches(raw, start, .ngb_bytes(2, 0, 0, 0x80), end)) return("prologue")
  if (end - start == 3 && .ngb_matches(raw, start, .ngb_bytes(0, 3, 0), end)) return("table_trailer")
  anchor <- if (.ngb_matches(raw, start, c(as.raw(0), .ngb_anchor), end)) start + 1 else start
  if (.ngb_matches(raw, anchor, .ngb_anchor, end) &&
      .ngb_matches(raw, anchor + 5, .ngb_bytes(0, 1), end)) return("preamble")
  if (.ngb_bare(raw, start, end) ||
      (.ngb_matches(raw, start, .ngb_bytes(0, 3, 0), end) && .ngb_bare(raw, start + 3, end)))
    return("bare_record")
  "malformed"
}
.ngb_type_ref <- function(payload) {
  n <- length(payload)
  if (n < 10 || !(.ngb_matches(payload, 0, .ngb_bytes(1, 0x80)) ||
                  .ngb_matches(payload, 0, .ngb_bytes(255, 255))) ||
      !.ngb_matches(payload, n - 8, .ngb_bytes(2, 0, 0, 0x80)) ||
      !.ngb_matches(payload, n - 2, .ngb_bytes(0, 0))) return(NULL)
  .ngb_uint(payload, n - 4, 2)
}
.ngb_document <- function(stream, id, limits) {
  raw <- stream$raw; tables <- list(); defects <- list(); current <- NULL
  # Vectorised anchor inventory avoids rescanning multi-megabyte streams.
  candidates <- which(raw == as.raw(0x18)) - 1L
  anchors <- candidates[vapply(candidates, function(p) .ngb_matches(raw, p, .ngb_anchor), logical(1))]
  close_table <- function() {
    if (!is.null(current)) {
      tables[[length(tables) + 1L]] <<- current
      if (length(tables) > limits$max_tables_per_stream)
        .ngb_error("Table count exceeds max_tables_per_stream", stream = id,
                   class = "ngb_resource_error")
    }
  }
  for (section in seq_len(nrow(stream$directory))) {
    pos <- stream$directory[section, "offset"]
    end <- pos + stream$directory[section, "size"]; saw <- FALSE; pending <- NULL
    while (pos < end) {
      rec <- if (!is.null(pending)) pending else .ngb_record(raw, pos, end, limits)
      pending <- NULL
      if (!is.null(rec$truncated)) {
        defects[[length(defects) + 1L]] <- list(offset = pos, kind = "truncated"); break
      }
      if (!is.null(rec)) {
        saw <- TRUE
        type <- if (rec$dtype == 26L && rec$mode == 0x0180) .ngb_type_ref(rec$raw) else NULL
        if (!is.null(type)) {
          close_table()
          current <- list(category = rec$id, type = type, fields = list())
        } else if (is.null(current)) {
          defects[[length(defects) + 1L]] <- list(offset = pos, kind = "orphan")
        } else {
          key <- as.character(rec$id)
          if (is.null(current$fields[[key]])) current$fields[[key]] <- rec
          else if (rec$dtype != 0L) warning("Duplicate field; keeping first occurrence", call. = FALSE)
        }
        pos <- rec$end; next
      }
      if (saw && .ngb_matches(raw, pos, .ngb_bytes(0, 3, 0), end) &&
          (pos + 3 == end || .ngb_matches(raw, pos + 3, .ngb_anchor, end))) {
        pos <- pos + 3; next
      }
      gap_end <- end
      probes <- anchors[anchors > pos & anchors < end]
      for (probe in probes) {
        candidate <- .ngb_record(raw, probe, end, limits)
        if (!is.null(candidate) && is.null(candidate$truncated)) {
          gap_end <- probe; pending <- candidate; break
        }
      }
      kind <- .ngb_gap(raw, pos, gap_end, saw)
      if (kind == "malformed") defects[[length(defects) + 1L]] <- list(offset = pos, kind = kind)
      pos <- gap_end
    }
  }
  close_table()
  list(tables = tables, defects = defects)
}
