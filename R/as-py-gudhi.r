#' @title Coerce objects to GUDHI simplex trees
#' 
#' @description This generic function...
#' 
#' @param x An R object to be coerced. See Details.
#' @param index Integer-valued vertex attribute to be used as 0-simplex indices.
#'   Ignored if `NULL` (the default).
#' @param ... Additional arguments passed to methods.
#' @return A list of integer vectors, each encoding one simplex.
#' @example inst/examples/ex-as-py-gudhi.r
#' @author Jason Cory Brunson
#' @author Yara Skaf
#' @export
as_py_gudhi <- function(x, ...) UseMethod("as_py_gudhi")

#' @rdname as_py_gudhi
#' @export
as_py_gudhi.default <- function(x, ...) {
  x <- ensure_cmplx(x)
  x <- ensure_list(x)
  # import GUDHI
  # -+- Is it possible to detect already imported, and as what? -+-
  gd <- reticulate::import("gudhi")
  
  # insert all simplices into a new simplex tree
  res <- gd$SimplexTree()
  for (s in x) res$insert(as.list(s))
  res
}

#' @rdname as_py_gudhi
#' @export
as_py_gudhi.Rcpp_SimplexTree <- function(x, ...) {
  # import GUDHI
  gd <- reticulate::import("gudhi")
  
  # insert maximal simplices into a new simplex tree
  res <- gd$SimplexTree()
  simplextree_version <- utils::packageVersion("simplextree")
  if (simplextree_version >= "1.0.1") {
    # traverse insertion over maximal simplices
    simplextree::traverse(
      simplextree::maximal(x),
      function(s) res$insert(as.list(s))
    )
  } else if (simplextree_version == "0.9.1") {
    # loop insertion over serialization
    for (s in x$serialize()) res$insert(as.list(s))
  } else {
    stop(
      "No method available for simplextree v",
      utils::packageVersion("simplextree")
    )
  }
  
  res
}

#' @rdname as_py_gudhi
#' @export
as_py_gudhi.simplextree <- function(x, ...) {
  as_py_gudhi.Rcpp_SimplexTree(x, ...)
}

#' @rdname as_py_gudhi
#' @export
as_py_gudhi.igraph <- function(x, index = NULL, ...) {
  if (! is.null(index)) ensure_index(x, index)
  # import GUDHI
  gd <- reticulate::import("gudhi")
  
  # generate vertex list
  vl <- if (is.null(index)) igraph::V(x) else igraph::vertex_attr(x, index)
  # generate edge list
  el <- apply(
    igraph::as_edgelist(x, names = FALSE),
    1L, identity, simplify = FALSE
  )
  if (! is.null(index)) el <- lapply(el, function(e) vl[e])
  vl <- as.list(vl)
  
  # insert vertices and edges into a new simplex tree
  res <- gd$SimplexTree()
  for (s in c(vl, el)) res$insert(as.list(s))
  res
}

#' @rdname as_py_gudhi
#' @export
as_py_gudhi.network <- function(x, index = NULL, ...) {
  
  # coerce to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_py_gudhi(x, index = index)
}
