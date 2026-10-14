compare_metadata <- function(actual, expected, path = "metadata") {
  if (is.list(expected)) {
    if (!is.list(actual)) stop("Not a list at ", path)
    if (!is.null(names(expected))) {
      if (!setequal(names(actual), names(expected))) stop("Metadata keys differ at ", path)
      for (key in names(expected)) compare_metadata(actual[[key]], expected[[key]], paste(path, key, sep = "$"))
    } else {
      if (length(actual) != length(expected)) stop("Metadata list lengths differ at ", path)
      for (i in seq_along(expected)) compare_metadata(actual[[i]], expected[[i]], paste0(path, "[[", i, "]]"))
    }
  } else if (is.numeric(expected)) {
    if (!is.numeric(actual) || !identical(as.numeric(actual), as.numeric(expected)))
      stop("Numeric metadata differs at ", path)
  } else if (!identical(actual, expected)) stop("Metadata differs at ", path)
}
