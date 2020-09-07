
modelPerformance <- function (model, testData, testDataClass) {

## Installing required packages and loading libraries
  
  requiredPackages <- c("ROCR", "caret", "e1071")
  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) install.packages(newPackages, dependencies = TRUE)
   
  library(e1071)
  library(caret)
  library(ROCR)
  
  
## Model performance metrics
  
  predictionLabels <- predict(model, testData, type = "response")
  predictionLabelsProbabilities <- ifelse(predictionLabels > 0.5, 1, 0)
  confusionMatrix <- table(Predicted = predictionLabelsProbabilities, Actual = testDataClass)
  cat("The confusion matrix is\n") 
  print(confusionMatrix)
  cat("The sensitivity of the model is", (sensitivity(confusionMatrix))*100, "%", "\n")
  cat("The specificity of the model is", (specificity(confusionMatrix))*100, "%", "\n")


## ROC curve

  predictionResults <- prediction(predictionLabelsProbabilities, testDataClass)
  performanceResults <- performance(predictionResults, "tpr", "fpr")
  plot(performanceResults, colorize = TRUE)


## Statistical Significance of the model
  overallP <- with(model,
                    pchisq(null.deviance-deviance,
                           df.null-df.residual,
                           lower.tail = FALSE))
  cat("The statistical significance for the model is", overallP, "\n")
  cat("The confidence level for this model is",
      ((1-overallP)*100), "percent")

}