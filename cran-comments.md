## Test environments

* local OS X install, R 4.2.1
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`
* R-hub
  * `rhub::check_for_cran()`
  * `rhub::check_for_cran(platforms = "macos-highsierra-release-cran")`

### R CMD check results

Using the vanilla check, there were no ERRORs, WARNINGs, or NOTEs.

The check with only Depends/Imports and exceptions was recently required by the CRAN maintainers. When run, conditional examples that use {network} functions resulted in an ERROR with the following message:
```
  Warning in ensure_cmplx(x) :
    Taking `cmplx` element as the simplicial complex.
  Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
    there is no package called ‘lattice’
  Calls: as_network ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
  Execution halted
```
Deeper experiments and research failed to clarify this problem.

### Win-Builder

There were no ERRORs or WARNINGs.

There was one NOTE, due to this being a new submission.

The check flagged the same spellings as the R-hub check.

### R-hub

There were no ERRORs or WARNINGs.

There were two NOTEs. One was due to this being a new submission.

The other "[f]ound the following files/directories:" and listed "'lastMiKTeXException'". A web search led me to [{rhub} issue #503](https://github.com/r-hub/rhub/issues/503), which suggests that the issue can be ignored.

In addition to several surnames, the check also flagged the unrecognized words "GUDHI", which is a Python package; "TDA", the standard initialism for "topological data analysis"; and "simplicial", pluralized "simplices", which is a standard term in discrete topology.

## Version dependencies

The method `as_py_gudhi_simplextree.Rcpp_SimplexTree()` operates differently depending on the version of {simplextree} installed. This has not been flagged by documentation and checks but may be important for the maintainers to know.

## Reverse dependencies

This is a new submission with no reverse dependencies.
