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
    all(unique(vapply(x, typeof, "")) %in% c("integer", "numeric")),
    all(unlist(x) %% 1 == 0)
  )
  x
}
