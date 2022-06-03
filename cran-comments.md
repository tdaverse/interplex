## Test environments

* local OS X install, R 4.1.3 (via `devtools::check()`)
* R-hub (via `rhub::check_for_cran()`)
* Win-Builder (devel, current, and previous; via `devtools::check_win_*()`)

### R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

### R-hub and Win-Builder

There were no ERRORs or WARNINGs.

The only NOTE mentioned that this is a new submission and flagged the unrecognized word "simplicial", pluralized "simplices".
This is a standard term in discrete topology.

The R-hub builder flagged a PREPERROR that i could not pin down, but the log flagged only the one NOTE.

## Downstream dependencies

This is a new submission with no dependencies.
