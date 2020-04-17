## Test environments
* local OS X install, R 3.6.1  
* ubuntu 16.04.6 (on travis-ci), R 3.5.3, 3.6.1 and devel
* win-builder

## R CMD check results  
There were no ERRORs or WARNINGs. There was 1 NOTE when 'rsvg' was not installed (as expected).

## Downstream dependencies
There are currently no downstream dependencies.


## Fixes for 1.0.0 errors
* Moved ggimage and rsvg to Suggests. rsvg is not used *directly* by cvms but by ggimage. I added checks to make sure ggimage isn't called when either ggimage or rsvg isn't installed. I also skip tests when it is not available.
* Fixed the failing unit test by improving the input checks in evaluate().
* Stripped numbers from error messages in unit tests that fail on platforms without support for long doubles. The specific numbers are not that important in those tests.
* Skips a lot of the unit tests to stay within time limits. This seems an unfortunate step but I've tried to maintain tests of the basic building blocks. All the tests will still be tested locally and on Travis CI.
