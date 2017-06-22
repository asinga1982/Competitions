                 # Functions
# Function to calculate F scores
calc.F <- function(dataset)
{  
  xx <- dataset
  retrieved <- sum(xx$Pred)
  precision <- length(which(xx$Act==1 & xx$Pred==1)) / retrieved
  recall <- length(which(xx$Act==1 & xx$Pred==1)) / sum(xx$Act)
  Fmeasure <- 2 * precision * recall / (precision + recall)
  print(precision)
  print(recall)
  
  Fmeasure
}
