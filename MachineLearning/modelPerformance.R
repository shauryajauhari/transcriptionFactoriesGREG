## Author : Shaurya Jauhari
## Last Reviewed: September 24th, 2020.
## Description: This function takes model, test-data, and test-data class (3 arguments) as input 
## and engenders a comprehensive set of 7 performance metrics, as below:
## (i) Confusion Matrix
## (ii) Sensitivity
## (iii) Specificity
## (iv) ROC-Curve
## (v) Area Under Curve(AUC) of the ROC-Curve
## (vi) Statistical significance for the model
## (vii) Confidence level for the model

## The last two metrics are applicable to linear models only.

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
  misClassError <- 1- sum(diag(confusionMatrix))/sum(confusionMatrix)
  cat("The misclassification error of the model is", misClassError*100, "%", "\n")
  cat("The sensitivity of the model is", (sensitivity(confusionMatrix))*100, "%", "\n")
  cat("The specificity of the model is", (specificity(confusionMatrix))*100, "%", "\n")


## ROC curve

  predictionResults <- prediction(predictionLabelsProbabilities, testDataClass)
  performanceResults <- performance(predictionResults, "tpr", "fpr")
  plot(performanceResults, main = "ROC Curve", colorize = TRUE)
  abline(a = 0, b = 1)
  
## AUC
  
  aucFind <- performance(predictionResults, measure = "auc")
  aucFind <- aucFind@y.values[[1]]
  cat("The area under curve is", aucFind, "\n")

## Statistical Significance of the model (linear models only)
  
  ifelse(class(model) %in% c("glm","lm"), overallP <- 1- (pchisq(eval(parse(text= paste0(deparse(substitute(model)), "$null.deviance"))) - eval(parse(text= paste0(deparse(substitute(model)), "$deviance"))), 
                                                                 eval(parse(text= paste0(deparse(substitute(model)), "$df.null"))) - eval(parse(text= paste0(deparse(substitute(model)), "$df.residual"))),
                                                                 lower.tail = FALSE)), )
  
  cat("The statistical significance for the model is", overallP, "\n")
  cat("The confidence level for the model is",((1-overallP)*100), "%")
    
}