## Test environments
* local OS X install, R 3.6.1  
* ubuntu 16.04.6 (on travis-ci), R 3.5.3, 3.6.1 and devel
* win-builder

## R CMD check results  
There were no ERRORs, WARNINGs or NOTEs.

The Windows errors were likely due to some of the packages being slightly differently compiled there, so I decreased the test tolerance when not on unix systems.

## Downstream dependencies
There are currently no downstream dependencies.
