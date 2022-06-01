#' @title Convert objects to class 'igraph'
#' 
#' @description This generic function...
#' 
#' @param x An R object to be coerced. See Details.
#' @param ... Additional arguments passed to methods.
#' @return An object of class 'igraph'.
#' @example inst/examples/as-igraph.r
#' @export
as_igraph <- function(x, ...) UseMethod("as_igraph")

#' @rdname as_igraph
#' @export
as_igraph.default <- function(x, ...) {
  x <- ensure_cmplx(x)
  x <- ensure_list(x)
  
  # subset to 0- and 1-simplices
  x <- x[sapply(x, length) <= 2L]
  # create graph from vertex and edge data
  igraph::graph(
    edges = unlist(x[sapply(x, length) == 2L]),
    n = max(unlist(x[sapply(x, length) == 1L])),
    directed = FALSE
  )
}

#' @rdname as_igraph
#' @export
as_igraph.Rcpp_SimplexTree <- function(x, ...) {
  
  # create graph from vertex and edge data
  res <- igraph::graph(
    edges = as.vector(t(x$edges)),
    n = max(x$vertices),
    directed = FALSE
  )
  
  # delete vertices missing from `x`
  # -+- need to do this -+-
  
  res
}

#' @rdname as_igraph
#' @export
as_igraph.simplextree <- function(x, ...) {
  as_igraph.Rcpp_SimplexTree(x, ...)
}

#' @rdname as_igraph
#' @export
as_igraph.igraph <- function(x, ...) x

#' @rdname as_igraph
#' @export
as_igraph.network <- function(x, ...) {
  # defer to intergraph
  intergraph::asIgraph(x, ...)
}
