#' @title **interplex** package
#'
#' @description This is a helper package to convert simplicial complexes between
#'   different data structures.
#'   

#' @details
#'
#' This package helps interface between different topological data analytic
#' packages and workflows by converting simplicial complex objects stored using
#' different data structures. Each conversion is designed to retain as much
#' annotation as possible, whether of simplices or of the complex.
#'
#' The package supports conversions between simplicial complexes stored using
#' the following data structures:

#' * a complete list of simplices,
#'   as stored in `cmplx` values of filtration objects
#'   in [the TDA package][TDA::TDA-package]
#' * an object of class 'Rcpp_SimplexTree' or 'simplextree'
#'   as implemented
#'   in [the simplextree package][simplextree::simplextree-package]
#' * an 'igraph' object,
#'   as implemented
#'   in [the igraph package][igraph]
#' * a 'network' object,
#'   as implemented
#'   in [the network package][network::network-package]

#'
#' When converting from a simplicial complex structure to a graph structure,
#' only the 1-skeleton is converted; simplices of dimension > 1 are discarded.

#' @docType package
#' @name interplex
NULL
