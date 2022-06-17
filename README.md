# interplex

This is an R package to facilitate conversion between different structures for simplicial complex data.

## usage

### installation

Until {interplex} is submitted to CRAN, install from GitHub as follows:

```r
remotes::install_github("corybrunson/interplex")
```

### supported structures

{interplex} includes converters between the following data structures:

* complete lists of simplices, used by the [{TDA}](https://cran.r-project.org/package=TDA) package
* simplex tree instances of class 'Rcpp_SimplexTree',
  provided by the [{simplextree}](https://github.com/peekxc/simplextree) package
* simplex trees in [Python GUDHI](https://gudhi.inria.fr/python/latest/) imported using the [{reticulate}](https://rstudio.github.io/reticulate/) package
* objects of class 'igraph', provided by the [{igraph}](https://igraph.org/r/) package
* objects of class 'network', provided by the [{network}](https://github.com/statnet/network) package

Coercion among the graph/network classes is done using methods from the [{intergraph}](https://mbojan.github.io/intergraph/) package. Simplicial complexes are only directly coerced between the 'igraph' class.

## acknowledgments

This package was designed and developed in part through discussions with Matt Piekenbrock and Raoul Wadhwa.

### resources

Development of this package benefitted from the use of equipment and the
support of colleagues at [UF Health](https://ufhealth.org/).

### contribute

Contributions in any form are more than welcome!
See the
[CONTRIBUTING](https://github.com/corybrunson/interplex/blob/main/CONTRIBUTING.md)
file for guidance, and please respect the [Code of
Conduct](https://github.com/corybrunson/interplex/blob/main/CODE_OF_CONDUCT.md).
