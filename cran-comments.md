## Test environments
* local OS X install, R 3.6.0  
* ubuntu 14.04.5 (on travis-ci), R 3.5.3, 3.6.1 and devel (2019-08-05 r76918)
* win-builder


## R CMD check results  
There were no ERRORs or WARNINGs. There was 1 NOTE:

Missing or unexported object: ‘tidyr::nest_legacy’

This is for compatibility with the upcoming tidyr version, that makes changes to nest and unnest, 
as per request by Hadley Wickham.

## Downstream dependencies
There are currently no downstream dependencies.
