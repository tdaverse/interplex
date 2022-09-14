#' @title Coerce objects to Python GUDHI simplex trees
#'
#' @description Coerce objects to 'SimplexTree' objects in Python GUDHI,
#'   accessed via [the reticulate package][reticulate::reticulate].
#'
#' @details
#'
#' `as_py_gudhi_simplextree()` is a generic function with specific methods for
#' different simplicial complex S3 classes. It returns an object of class
#' 'gudhi.simplex_tree.SimplexTree', which is a
#' [reticulate][reticulate::reticulate] accessor to a Python object of class
#' 'SimplexTree' implemented in GUDHI.
#'
#' @template sec-classes-methods
#'
#' @param x An R object to be coerced. See Details.
#' @param index Integer-valued vertex attribute to be used as 0-simplex indices.
#'   Ignored if `NULL` (the default).
#' @param ... Additional arguments passed to methods.
#' @return A simplex tree instantiated in Python GUDHI accessed through
#'   reticulate.
#' @example inst/examples/ex-as-py-gudhi.r
#' @author Jason Cory Brunson
#' @author Yara Skaf
#' @export
as_py_gudhi_simplextree <- function(x, ...) UseMethod("as_py_gudhi_simplextree")

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.default <- function(x, ...) {
  stop("Cannot coerce class '", class(x)[[1L]], "' to a GUDHI 'SimplexTree'")
}

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.list <- function(
    x, check_structure = TRUE, drop_values = FALSE, ...
) {
  # `x` should be structured as a filtration returned by `TDA::*Filtration()`
  if (check_structure) check_tda_filtration(x)
  
  # import GUDHI
  # TODO: Detect whether and under what name GUDHI is already imported.
  gd <- reticulate::import("gudhi")
  
  # insert all simplices into a new simplex tree
  res <- gd$SimplexTree()
  if (drop_values) {
    for (s in x) res$insert(as.list(s))
  } else {
    x_iter <- seq_along(x$cmplx)
    if (! x$increasing) x_iter <- rev(x_iter)
    for (i in x_iter)
      res$insert(simplex = as.list(x$cmplx[[i]]), filtration = x$values[[i]])
  }
  res
}

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.Rcpp_SimplexTree <- function(x, ...) {
  # import GUDHI
  gd <- reticulate::import("gudhi")
  
  # insert maximal simplices into a new simplex tree
  res <- gd$SimplexTree()
  .simplextree_version <- utils::packageVersion("simplextree")
  if (.simplextree_version >= "1.0.1") {
    # traverse insertion over maximal simplices
    simplextree::traverse(
      simplextree::maximal(x),
      function(s) res$insert(as.list(s))
    )
  } else if (.simplextree_version == "0.9.1") {
    # loop insertion over serialization
    for (s in x$serialize()) res$insert(as.list(s))
  } else {
    stop("No method available for {simplextree} v", .simplextree_version)
  }
  
  res
}

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.Rcpp_Filtration <- function(x, ...) {
  # import GUDHI
  gd <- reticulate::import("gudhi")
  
  # insert simplices into a new simplex tree in order of value (weight)
  res <- gd$SimplexTree()
  .simplextree_version <- utils::packageVersion("simplextree")
  if (.simplextree_version >= "1.0.1") {
    for (i in seq_along(x$simplices))
      res$insert(
        simplex = as.list(x$simplices[[i]]),
        filtration = x$weights[[i]]
      )
  } else {
    stop("No method available for simplextree v", .simplextree_version)
  }
  
  res
}

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.igraph <- function(x, index = NULL, value = NULL, ...) {
  # check compatibility of 'index' and 'value' attributes
  # (does not check that filtration respects faces)
  if (! is.null(index)) check_index(x, index)
  if (! is.null(value)) check_value(x, value)
  
  # import GUDHI
  gd <- reticulate::import("gudhi")
  
  # generate vertex vector (for use by following code)
  vl <- if (is.null(index)) igraph::V(x) else igraph::vertex_attr(x, index)
  # generate edge list
  el <- apply(
    igraph::as_edgelist(x, names = FALSE),
    1L, identity, simplify = FALSE
  )
  if (! is.null(index)) el <- lapply(el, function(e) vl[e])
  # coerce to vertex list
  vl <- as.list(vl)
  
  # insert vertices and edges into a new simplex tree
  res <- gd$SimplexTree()
  if (is.null(value)) {
    for (s in c(vl, el)) res$insert(as.list(s))
  } else {
    for (i in seq_along(vl))
      res$insert(
        simplex = as.list(vl[[i]]),
        filtration = igraph::vertex_attr(x, value)[[i]]
      )
    for (i in seq_along(el))
      res$insert(
        simplex = as.list(el[[i]]),
        filtration = igraph::edge_attr(x, value)[[i]]
      )
  }
  res
}

#' @rdname as_py_gudhi_simplextree
#' @export
as_py_gudhi_simplextree.network <- function(
    x, index = NULL, value = NULL, ...
) {
  
  # coerce to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_py_gudhi_simplextree(x, index = index, value = value)
}
