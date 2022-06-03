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
  # store vertex IDs
  x_vid <- sort(unique(unlist(x)))
  # create graph from vertex and edge data
  res <- igraph::graph(
    edges = match(unlist(x[sapply(x, length) == 2L]), x_vid),
    n = max(match(unlist(x[sapply(x, length) == 1L]), x_vid)),
    directed = FALSE
  )
  # add vertex IDs as an attribute
  igraph::set_vertex_attr(res, "index", value = x_vid)
}

#' @rdname as_igraph
#' @export
as_igraph.Rcpp_SimplexTree <- function(x, ...) {
  
  # store vertex IDs
  x_vid <- as.integer(x$vertices)
  # create graph from vertex and edge data
  res <- igraph::graph(
    edges = as.vector(rbind(
      match(x$edges[, 1L], x_vid),
      match(x$edges[, 2L], x_vid)
    )),
    n = length(x$vertices),
    directed = FALSE
  )
  # add vertex IDs as an attribute
  igraph::set_vertex_attr(res, "index", value = x_vid)
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
