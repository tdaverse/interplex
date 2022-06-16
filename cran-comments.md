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

The check also flagged the unrecognized word "simplicial", pluralized "simplices".
This is a standard term in discrete topology.

### Win-Builder

There were no ERRORs or WARNINGs.

There was one NOTE, due to this being a new submission.

The check flagged the same spellings as the R-hub check.

## Reverse dependencies

This is a new submission with no reverse dependencies.
