## Test environments
* local OS X install, R 3.6.1  
* ubuntu 16.04.6 (on travis-ci), R 3.6.3, 4.0.0 and devel
* win-builder

## R CMD check results  
There were no ERRORs or WARNINGs. There was 1 NOTE when `rsvg` was not installed (as expected).

## Downstream dependencies
There are currently no downstream dependencies.

## Release notes
Please be aware, that the performance drop in the upcoming `dplyr v1.0.0` for `summarize()` and similar functions seem to have a big impact on `cvms`, and that this might affect the testing time significantly. Given that the `tidyverse` folks intend to fix this in `v1.1.0` (as I understand it), I will not be skipping more unittests on CRAN unless specifically requested by CRAN.
