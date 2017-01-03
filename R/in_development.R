# in development - move or remove

load_dataset <- function(){
  data <- XLConnect::readWorksheet(XLConnect::loadWorkbook("../ClinicalData.xlsx"),sheet=1)

  data$ID <- as.factor(data$ID)
  data$Diagnosis <- as.factor(data$Diagnosis)
  data$Education <- as.integer(data$Education)
  data = data[data$Gender != 'F/M',]

  return(data)

}


# Read https://www.r-bloggers.com/handling-class-imbalance-with-r-and-caret-caveats-when-using-the-auc/
# AUPRC
