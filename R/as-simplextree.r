#' @title Convert objects to class 'simplextree'
#' 
#' @description This generic function...
#' 
#' @param x An R object to be coerced. See Details.
#' @param ... Additional arguments passed to methods.
#' @return An object of class 'simplextree'.
#' @example inst/examples/as-simplextree.r
#' @export
as_simplextree <- function(x, ...) UseMethod("as_simplextree")

#' @rdname as_simplextree
#' @export
as_simplextree.default <- function(x, ...) {
  x <- ensure_cmplx(x)
  x <- ensure_list(x)
  
  # insert all simplices into a new simplicial complex
  res <- simplextree::simplex_tree()
  res$insert(x)
  res
}

#' @rdname as_simplextree
#' @export
as_simplextree.Rcpp_SimplexTree <- function(x, ...) x

#' @rdname as_simplextree
#' @export
as_simplextree.simplextree <- function(x, ...) {
  as_simplextree.Rcpp_SimplexTree(x, ...)
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
