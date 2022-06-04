#' @title Convert objects to class 'simplextree'
#'
#' @description This generic function...
#'
#' @param x An R object to be coerced. See Details.
#' @param index Integer-valued vertex attribute to be used as 0-simplex indices.
#'   Ignored if `NULL` (the default).
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
as_simplextree.igraph <- function(x, index = NULL, ...) {
  if (! is.null(index)) ensure_index(x, index)
  
  # generate vertex list
  vl <- if (is.null(index)) igraph::V(x) else igraph::vertex_attr(x, index)
  # generate edge list
  el <- apply(
    igraph::as_edgelist(x, names = FALSE),
    1L, identity, simplify = FALSE
  )
  if (! is.null(index)) el <- lapply(el, function(e) vl[e])
  vl <- as.list(vl)
  
  # initialize simplicial complex
  res <- simplextree::simplex_tree()
  # insert vertices and edges
  res$insert(vl)
  res$insert(el)
  
  res
}

#' @rdname as_simplextree
#' @export
as_simplextree.network <- function(x, index = NULL, ...) {
  
  # convert to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_simplextree(x, index = index)
}
