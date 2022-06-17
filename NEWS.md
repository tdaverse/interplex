# interplex 0.1.0

This first release includes converters between the following data structures:

* complete lists of simplices, used by the {TDA} package
* simplex tree instances of class 'Rcpp_SimplexTree',
  provided by the {simplextree} package
* simplex trees as implemented in Python GUDHI through the {reticulate} interface
* objects of class 'igraph', provided by the {igraph} package
* objects of class 'network', provided by the {network} package

Coercion among the graph/network classes is done using methods from the {intergraph} package. Simplicial complexes are only directly coerced from the 'igraph' class.
