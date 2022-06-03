# extract simplicial complex from TDA package filtration
ensure_cmplx <- function(x) {
  if (! is.null(names(x)) &&
      all(names(x) == c("cmplx", "values", "increasing", "coordinates"))) {
    warning("Taking `cmplx` element as the simplicial complex.")
    x <- x$cmplx
  }
  x
}

# ensure that input is a list of numeric vectors
ensure_list <- function(x) {
  stopifnot(
    typeof(x) == "list",
    all(unique(vapply(x, typeof, "")) %in% c("integer", "double")),
    all(unlist(x) %% 1 == 0)
  )
  x
}

# sort an undirected edge list matrix by columns in order
sort_el <- function(x) {
  # put lower indices on left
  x <- cbind(pmin(x[, 1L], x[, 2L]), pmax(x[, 1L], x[, 2L]))
  # sort rows by from & to indices
  x[order(x[, 1L], x[, 2L]), ]
}
