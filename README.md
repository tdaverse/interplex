# interplex

This is an R package to facilitate conversion between different structures for simplicial complex data.

## supported structures

{interplex} includes converters between the following data structures:

* complete lists of simplices, used by the [{TDA}](https://cran.r-project.org/web/packages/TDA/index.html) package
* simplex tree instances of class 'Rcpp_SimplexTree' or 'simplextree',
  provided by the [{simplextree}](https://github.com/peekxc/simplextree) package
* objects of class 'igraph', provided by the [{igraph}](https://igraph.org/r/) package
* objects of class 'network', provided by the [{network}](https://github.com/statnet/network) package

Coercion among the graph/network classes is done using methods from the [{intergraph}](https://mbojan.github.io/intergraph/) package. Simplicial complexes are only directly coerced between the 'igraph' class.
