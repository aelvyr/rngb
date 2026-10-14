library(rngb)

# Independent fixture encoder: construct the grammar, including embedded runs,
# rather than using the reader to write its own expected values.
b <- function(...) as.raw(c(...))
uint <- function(value, size) as.raw(floor(value / 256^(0:(size - 1L))) %% 256)
header <- b(0x18, 0xfc, 0xff, 0xff, 3, 0x80, 1)
middle <- b(0, 0, 1, 0, 0, 0, 0x0c, 0, 0x17, 0xfc, 0xff, 0xff)
ending <- b(1, 0, 0, 0, 2, 0, 1, 0, 0)
scalar <- function(id, type, payload, terminator = ending)
  c(header, uint(id, 2), middle, b(type, 0x80, 1), payload, terminator)
array <- function(id, type, values, count = length(values)) {
  size <- if (type == 4) 4L else 8L
  c(header, uint(id, 2), middle, b(type, 0xa0, 1), uint(count, 4),
    writeBin(as.double(values), raw(), size = size, endian = "little"), ending)
}
text_field <- function(id, text) {
  payload <- charToRaw(enc2utf8(text))
  scalar(id, 31, c(uint(length(payload), 4), payload))
}
table <- function(category, type, ...) {
  open <- scalar(category, 26, c(b(1, 0x80, 2, 0, 0, 0x80), uint(type, 2), b(0, 0)))
  c(open, ..., b(0, 3, 0))
}
prologue <- c(b(2, 0, 0, 0x80), raw(60))
metadata <- function(kind = 3L, mass = 19.777) c(prologue,
  table(0x1770, 0x2afc, scalar(0x103a, 3, uint(kind, 4))),
  table(0x1775, 0x2b17, text_field(0x1059, "STA test")),
  table(0x7530, 0x2afb, text_field(0x0840, "Sample Zürich"),
        scalar(0x0c9e, 5, writeBin(mass, raw(), size = 8L, endian = "little"))))
channel <- function(category, values, type = 5L, count = length(values)) c(
  table(category, 0x2b22),
  table(0x7530, 0x2b23, array(if (type == 4) 0x0f3d else 0x0f40, type, values, count)))
run <- function(time, mass, ...) c(channel(0x178c, time), channel(0x1790, mass), ...)
container <- function(id, body, bad_magic = FALSE, bad_size = FALSE) {
  h <- raw(94)
  h[3:17] <- charToRaw(if (bad_magic) "Invalid TA file" else "Netzsch TA file")
  h[29:40] <- charToRaw("_db_format_1")
  h[81:94] <- c(b(255, 255), uint(id, 2), uint(94, 4),
                uint(length(body) + as.integer(bad_size), 4), b(0, 0))
  c(h, body)
}
make_file <- function(streams, extension = "ngb-ds3") {
  work <- tempfile("ngb-fixture-"); dir.create(work); dir.create(file.path(work, "Streams"))
  on.exit(unlink(work, recursive = TRUE))
  members <- character()
  for (id in names(streams)) {
    name <- file.path("Streams", paste0("stream_", id, ".table"))
    writeBin(streams[[id]], file.path(work, name)); members <- c(members, name)
  }
  path <- tempfile(fileext = paste0(".", extension))
  old <- setwd(work); on.exit(setwd(old), add = TRUE)
  status <- utils::zip(path, members, flags = "-q")
  if (status != 0) stop("Cannot create test archive")
  path
}
files <- character()
fixture <- function(...) {
  path <- make_file(...); files <<- c(files, path); path
}
expect_error <- function(expr, class = "ngb_corrupt_error") {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, class))
  invisible(error)
}

sample_run <- run(c(0, 1, 2), c(0, -1, -2))
baseline_run <- run(c(0, 2), c(0, -0.5))
path <- fixture(list(`1` = container(1, metadata()),
                     `2` = container(2, c(prologue, sample_run, baseline_run))))
x <- read_ngb(path)
stopifnot(identical(x$data$time, c(0, 60, 120)),
          identical(x$data$mass, c(0, -1, -2)),
          identical(x$baseline$time, c(0, 120)),
          identical(x$baseline$mass, c(0, -0.5)),
          x$metadata$sample_name == "Sample Zürich", x$metadata$sample_mass == 19.777,
          x$metadata$measurement_type == "sample_correction",
          x$column_metadata$mass$description == "Raw mass change",
          !x$column_metadata$mass$calibration_applied,
          identical(names(x), c("metadata", "data", "baseline", "column_metadata")))

# Single-run baseline and optional stream 3; f32 widening is lossless.
base <- fixture(list(`1` = container(1, metadata(1)),
  `2` = container(2, c(prologue, baseline_run)),
  `3` = container(3, c(prologue, channel(0x7530, c(25, 30), 4L)))), "ngb-bs3")
y <- read_ngb(base)
stopifnot(is.null(y$baseline), y$metadata$measurement_type == "correction",
          identical(y$data$furnace_temperature, c(25, 30)))

# Unknown channels retain a stable name, with no guessed unit.
unknown <- fixture(list(`1` = container(1, metadata(1)),
  `2` = container(2, c(prologue, run(c(0, 1), c(0, 0), channel(0x17ab, c(8, 9)))))), "ngb-bs3")
u <- read_ngb(unknown)
stopifnot(identical(u$data$ab, c(8, 9)), is.na(u$column_metadata$ab$units))

# Data grammar damage is fatal; metadata damage warns and degrades.
broken <- fixture(list(`1` = container(1, metadata(1)),
  `2` = container(2, c(prologue, sample_run, b(99, 98, 97)))), "ngb-bs3")
expect_error(read_ngb(broken))
warned <- FALSE
damaged_meta <- fixture(list(`1` = container(1, c(metadata(1), b(99, 98, 97))),
                             `2` = container(2, c(prologue, baseline_run))), "ngb-bs3")
z <- withCallingHandlers(read_ngb(damaged_meta), warning = function(w) {
  warned <<- TRUE; invokeRestart("muffleWarning")
})
stopifnot(warned, identical(z$data$mass, c(0, -0.5)))

truncated <- fixture(list(`1` = container(1, metadata(1)),
  `2` = container(2, c(prologue, channel(0x178c, c(0, 1), count = 1000)))), "ngb-bs3")
expect_error(read_ngb(truncated))
unequal <- fixture(list(`1` = container(1, metadata(1)),
  `2` = container(2, c(prologue, run(c(0, 1, 2), c(0, 1))))), "ngb-bs3")
expect_error(read_ngb(unequal))
orphan <- fixture(list(`1` = container(1, metadata(1)),
  `2` = container(2, c(prologue, table(0x7530, 0x2b23, array(0x0f40, 5, c(1, 2))), baseline_run))), "ngb-bs3")
expect_error(read_ngb(orphan))
stray <- fixture(list(`1` = container(1, metadata()),
  `2` = container(2, c(prologue, sample_run, channel(0x178c, c(0, 1))))))
expect_error(read_ngb(stray))
wrong_count <- fixture(list(`1` = container(1, metadata()),
  `2` = container(2, c(prologue, sample_run, baseline_run)),
  `3` = container(3, c(prologue, channel(0x7530, c(25, 30, 35))))))
expect_error(read_ngb(wrong_count))
wrong_kind <- fixture(list(`1` = container(1, metadata(1)),
  `2` = container(2, c(prologue, sample_run, baseline_run))))
expect_error(read_ngb(wrong_kind))
one_run_ds3 <- fixture(list(`1` = container(1, metadata()), `2` = container(2, c(prologue, sample_run))))
expect_error(read_ngb(one_run_ds3))
bad_header <- fixture(list(`1` = container(1, metadata(1), bad_magic = TRUE),
  `2` = container(2, c(prologue, baseline_run))), "ngb-bs3")
expect_error(read_ngb(bad_header))
bad_directory <- fixture(list(`1` = container(1, metadata(1), bad_size = TRUE),
  `2` = container(2, c(prologue, baseline_run))), "ngb-bs3")
expect_error(read_ngb(bad_directory))
missing <- fixture(list(`1` = container(1, metadata(1))), "ngb-bs3")
expect_error(read_ngb(missing))
expect_error(read_ngb(path, list(max_stream_size_mb = 0.00001)), "ngb_resource_error")
expect_error(read_ngb(path, list(max_array_size_mb = 0.000001)), "ngb_resource_error")
expect_error(read_ngb(path, list(max_tables_per_stream = 1)), "ngb_resource_error")
expect_error(read_ngb(path, list(max_stream_size_mb = NULL)), "error")
expect_error(read_ngb(path, list(unknown_limit = 1)), "error")
expect_error(read_ngb(NA_character_), "error")
expect_error(read_ngb(tempfile()), "error")

# Test UTF-16LE (including a surrogate pair) and null stripping through decoding.
decode <- getFromNamespace(".ngb_string", "rngb")
utf16 <- c(b(255, 254, 255, 4), uint(0x00dc, 2), uint(0xd83d, 2), uint(0xde00, 2), uint(0, 2))
stopifnot(identical(decode(utf16), "Ü😀"))
number <- getFromNamespace(".ngb_numeric", "rngb")
stopifnot(identical(number(b(0, 0, 0, 128), 3L), -2147483648))

unlink(files)
cat("Synthetic parser, corruption, encoding and resource-limit checks passed\n")
