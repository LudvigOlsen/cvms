# cvms
**Cross-Validation for Model Selection**  

R package: A set of functions for cross-validating gaussian and binomial regression models.  


**Notice:** Under development. Needs testing.  
Check Oldscript folder for working script and the old tutorial.  


By Ludvig R. Olsen and Benjamin Zachariae  
Cognitive Science, Aarhus University  
Started in Oct. 2016  

Main functions:  
* cross_validate()
* cross_validate_list()  
  

## Installation:

You need to manually install the package splitters (which will be renamed in the near future).  

Run these lines to install cvms and splitters:  

install.packages("devtools")   
devtools::install_github("LudvigOlsen/R-splitters")  
devtools::install_github("LudvigOlsen/R-cross_validate")  

  
## News:  
* Implemented splitters::fold() for creating balanced folds  
* Replaced main for loop with llply for speed  
  
  
Please read using_cross_validation.pdf in "Oldscript/" for further information on how to use the functions.  
