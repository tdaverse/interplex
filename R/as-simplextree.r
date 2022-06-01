#' @title Convert objects to class "simplextree"
#' 
#' @description This generic function...
#' 
#' @param x An R object to be coerced. See Details.
#' @param ... Additional arguments passed to methods.
#' @return An object of class "simplextree".
#' @example inst/examples/as-simplextree.r
#' @export
as_simplextree <- function(x, ...) UseMethod("as_simplextree")

#' @rdname as_simplextree
#' @export
as_simplextree.default <- function(x, ...) {
  
  # extract simplicial complex from TDA package filtration
  if (! is.null(names(x)) &&
      all(names(x) == c("cmplx", "values", "increasing", "coordinates"))) {
    warning("Taking `cmplx` element as the simplicial complex.")
    x <- x$cmplx
  }
  
  # ensure that input is a list of numeric vectors
  stopifnot(
    typeof(x) == "list",
    all(unique(vapply(x, typeof, "")) %in% c("integer", "numeric")),
    all(unlist(x) %% 1 == 0)
  )
  
  # insert all simplices into a new simplicial complex
  res <- simplextree::simplex_tree()
  res$insert(x)
  res
}

#' @rdname as_simplextree
#' @export
as_simplextree.igraph <- function(x, ...) {
  
  # insert all edges into a new simplicial complex
  res <- simplextree::simplex_tree()
  el <- apply(igraph::as_edgelist(x), 1L, function(y) y, simplify = FALSE)
  res$insert(el)
  res
}

#' @rdname as_simplextree
#' @export
as_simplextree.network <- function(x, ...) {
  
  # convert to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_simplextree(x)
}
