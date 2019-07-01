## Test environments
* local OS X install, R 3.6.0  
* win-builder

## R CMD check results  
There were no ERRORs, WARNINGs or NOTEs.  

## Downstream dependencies
There are currently no downstream dependencies, as it is a new package.

## Resubmission notes
* Replaced replace dontrun with donttest.  
* Removed need for <<-, even though it wasn't used to modify the global env. Note, that 
it is still used in a script in data-raw/ which shouldn't be a problem.
* Added Benjamin Hugh Zachariae as author in DESCRIPTION.

