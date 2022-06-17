#' 
#' The generic coercers recognize the following S3 classes during method
#' dispatch: 'Rcpp_SimplexTree', 'gudhi.simplex_tree.SimplexTree', 'igraph', and
#' 'network'. In case `x` matches none of these classes, the default method
#' requires `x` to be a list of integer vectors representing simplices, or else
#' a list with a named element `cmplx` having this structure, as used in
#' [TDA::TDA-package].
#' 
