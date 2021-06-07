## Test environments
* local OS X install, R 3.6.1  
* win-builder

## R CMD check results  
There were no ERRORs or WARNINGs. There was 1 NOTE when `rsvg` was not installed (as expected).

## Downstream dependencies
There are currently no downstream dependencies.

* I reduced the number of tests that run on CRAN with `testthat::skip_on_cran()`.
