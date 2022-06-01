#' @title Convert simplicial complex objects to lists in the TDA package style
#' 
#' @description This generic function...
#' 
#' @param x An R object to be coerced. See Details.
#' @param ... Additional arguments passed to methods.
#' @return A list of integer vectors, each encoding one simplex.
#' @example inst/examples/as-cmplx.r
#' @export
as_cmplx <- function(x, ...) UseMethod("as_cmplx")

#' @rdname as_cmplx
#' @export
as_cmplx.default <- function(x, ...) {
  x <- ensure_cmplx(x)
  x <- ensure_list(x)
  
  # return list
  x
}

#' @rdname as_cmplx
#' @export
as_cmplx.Rcpp_SimplexTree <- function(x, ...) {
  
  # extract list of fixed-dimension simplex matrices
  simps <- x$as_list()
  # convert matrices to lists
  simps <- lapply(simps, function(row) apply(row, 1L, identity, simplify = FALSE))
  # unlist to list of simplices
  unlist(simps, recursive = FALSE)
}

#' @rdname as_cmplx
#' @export
as_cmplx.simplextree <- function(x, ...) {
  as_cmplx.Rcpp_SimplexTree(x, ...)
}

#' @rdname as_cmplx
#' @export
as_cmplx.igraph <- function(x, ...) {
  
  # generate a list of edge vectors
  edges <- apply(igraph::as_edgelist(x), 1L, function(y) y, simplify = FALSE)
  # concatenate with a list of vertex vectors
  c(as.integer(igraph::V(x)), edges)
}

#' @rdname as_cmplx
#' @export
as_cmplx.network <- function(x, ...) {
  
  # convert to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_cmplx(x)
}
