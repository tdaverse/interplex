## Test environments

* local OS X install, R 4.1.3 (via `devtools::check()`)
* R-hub (via `rhub::check_for_cran()`)
* Win-Builder (devel, current, and previous; via `devtools::check_win_*()`)

### R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

### R-hub

There were no ERRORs or WARNINGs.

There were two NOTEs. One was due to this being a new submission.

The other "[f]ound the following files/directories:" and listed "'lastMiKTeXException'". A web search led me to [{rhub} issue #503](https://github.com/r-hub/rhub/issues/503), which suggests that the issue can be ignored.

In addition to several surnames, the check also flagged the unrecognized words "GUDHI", which is a Python package; "TDA", the standard initialism for "topological data analysis"; and "simplicial", pluralized "simplices", which is a standard term in discrete topology.

### Win-Builder

There were no ERRORs or WARNINGs.

There was one NOTE, due to this being a new submission.

The check flagged the same spellings as the R-hub check.

## Version dependencies

The method `as_py_gudhi.Rcpp_SimplexTree()` operates differently depending on the version of {simplextree} installed. This has not been flagged by documentation and checks but may be important for the maintainers to know.

## Reverse dependencies

This is a new submission with no reverse dependencies.
