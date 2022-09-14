#' @title Coerce objects to class 'Rcpp_SimplexTree'
#'
#' @description Coerce objects to 'Rcpp_SimplexTree' objects, as implemented in
#'   [the simplextree package][simplextree::simplextree-package].
#'
#' @details
#'
#' `as_rcpp_simplextree()` is a generic function with specific methods for
#' different simplicial complex S3 classes. It returns an object of class
#' ['Rcpp_SimplexTree'][simplextree::Rcpp_SimplexTree], which is an [Rcpp
#' Module][Rcpp::Module] that exposes an instance of a C++ instance of a simplex
#' tree.
#'
#' @template sec-classes-methods
#'
#' @param x An R object to be coerced. See Details.
#' @param index Integer-valued vertex attribute to be used as 0-simplex indices.
#'   Ignored if `NULL` (the default).
#' @param ... Additional arguments passed to methods.
#' @return An instance of a simplex tree, exposed as an Rcpp Module with class
#'   'Rcpp_SimplexTree'.
#' @example inst/examples/ex-as-simplextree.r
#' @export
as_rcpp_simplextree <- function(x, ...) UseMethod("as_rcpp_simplextree")

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.default <- function(x, ...) {
  stop("Cannot coerce class '", class(x)[[1L]], "' to an 'Rcpp_SimplexTree'")
}

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.list <- function(
    x, check_structure = TRUE, drop_values = FALSE, ...
) {
  # `x` should be structured as a filtration returned by `TDA::*Filtration()`
  if (check_structure) check_tda_filtration(x)
  
  # insert all simplices into a new simplex tree
  res <- simplextree::simplex_tree()
  .simplextree_version <- utils::packageVersion("simplextree")
  if (! drop_values && ! .simplextree_version >= "1.0.1") {
    warning(
      "Filtrations are not supported in {simplextree} v",
      .simplextree_version
    )
    drop_values <- TRUE
  } else if (! drop_values) {
    warning("Filtrations are not yet supported for {simplextree}")
    drop_values <- TRUE
  }
  if (drop_values) {
    res$insert(x$cmplx)
  } else {
    x_iter <- seq_along(x$cmplx)
    if (! x$increasing) x_iter <- rev(x_iter)
    # TODO: Construct a filtration in {simplextree}.
  }
  res
}

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.Rcpp_SimplexTree <- function(x, ...) x

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.gudhi.simplex_tree.SimplexTree <- function(
    x, drop_values = FALSE, ...
) {
  
  # initialize simplex tree
  res <- simplextree::simplex_tree()
  # iteratively insert simplices (and filtration values)
  if (drop_values) {
    reticulate::iterate(x$get_simplices(), function(s) res$insert(s[[1L]]))
  } else {
    warning("Filtrations are not yet supported for {simplextree}")
    reticulate::iterate(x$get_filtration(), function(s) res$insert(s[[1L]]))
  }
  # return simplex tree
  res
}

as_rcpp_simplextree_gudhi_complex <- function(
    x, drop_values = FALSE, ...
) {
  
  # create simplex tree in Python GUDHI
  x <- x$create_simplex_tree()
  
  # invoke 'gudhi.simplex_tree.SimplexTree' method
  as_rcpp_simplextree(x, drop_values = drop_values, ...)
}

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.gudhi.rips_complex.RipsComplex <-
  as_rcpp_simplextree_gudhi_complex

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.gudhi.alpha_complex.AlphaComplex <-
  as_rcpp_simplextree_gudhi_complex

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.igraph <- function(x, index = NULL, value = NULL, ...) {
  # check compatibility of 'index' and 'value' attributes
  # (does not check that filtration respects faces)
  if (! is.null(index)) check_index(x, index)
  if (! is.null(value)) check_value(x, value)
  
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
  
  # initialize simplicial complex
  res <- simplextree::simplex_tree()
  # insert vertices and edges
  res$insert(vl)
  res$insert(el)
  if (! is.null(value)) {
    # assign edge filtration values
    # TODO: assign vertex filtration values
    res <- simplextree::flag(
      res,
      igraph::edge_attr(x, value)[igraph::get.edge.ids(x, t(res$edges))]
    )
  }
  
  res
}

#' @rdname as_rcpp_simplextree
#' @export
as_rcpp_simplextree.network <- function(x, index = NULL, value = NULL, ...) {
  
  # coerce to an igraph object
  x <- intergraph::asIgraph(x, ...)
  
  # invoke 'igraph' method
  as_rcpp_simplextree(x, index = index, value = value)
}
