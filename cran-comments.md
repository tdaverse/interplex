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
  * R-hub checks on several platforms as follows:
  ```
  rhub::check_for_cran(platforms = c(
    "debian-gcc-release",
    "linux-x86_64-rocker-gcc-san",
    "ubuntu-gcc-release"
  ))
  ```

### R CMD check results

There were no ERRORs, WARNINGs, or NOTEs using either check.

### Win-Builder

There were no ERRORs or WARNINGs.

There was one NOTE:
```
New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  ...

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-02-06 as issues were not corrected
    in time.
```
All flagged words have been checked; they are proper names or technical terms.
The remaining components of the NOTE have to do with this submission replacing an archived version.

### R-hub

There were no ERRORs or WARNINGs.

There were three NOTEs, varying slightly by platform. One was due to this being a new submission.

Another note "Found the following files/directories" and listed "''NULL''".
Based on [{rhub} issue #560](https://github.com/r-hub/rhub/issues/560), this is likely a problem with R-hub and can be ignored.

The other "[f]ound the following files/directories:" and listed "'lastMiKTeXException'". A web search led me to [{rhub} issue #503](https://github.com/r-hub/rhub/issues/503), which suggests that the issue can be ignored.

Finally, these checks also flagged the same unrecognized words as the Win-Builder checks.

## Version dependencies

The method `as_py_gudhi_simplextree.Rcpp_SimplexTree()` operates differently depending on the version of {simplextree} installed. This has not been flagged by documentation and checks but may be important for the maintainers to know.

## Reverse dependencies

This is a "new" submission (of an archived package) with no reverse dependencies.
