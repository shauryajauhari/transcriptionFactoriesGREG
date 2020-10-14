## Author : Shaurya Jauhari
## Last Reviewed: October 14th, 2020.
## Description: This function takes a feature-file, a chromosome-name, and a feature-name as arguments and 
## adds random values between the maximum value and twice the maximum value of the read-coverages of that 
## feature. This distorts proportion of that feature to the overall classification model and might be useful
## for cross-validation.


dataSimulation <- function(dataFile, chrName, featureName){
  
  ## Check for file availability
  
  if(file.exists(dataFile))
  {
    standardData <- read.table(dataFile, header = TRUE)
  }
  else
  {
    return("Invalid file.")
  }
  
  ## Extracting chromosome-specific data
  
  stopifnot(chrName %in% unique(standardData$chr))
  chrData <- standardData[standardData$chr == chrName,]
  
  ## Locating feature data to be tampered
  
  stopifnot(featureName %in% colnames(chrData))
  selectedFeature <- chrData[ , featureName] 
  
  
  ### SIMULATION ###
  ## The idea is to add exorbitant values to the base values to the selected feature (read-coverages)
  ## that is the most influential variable to the model. For the same, we plan on adding random values
  ## between the original maximum value of the coverage and twice that number.
  
  selectedFeature <- runif(length(selectedFeature), min = max(selectedFeature), max = 2 * max(selectedFeature))
  chrData[ , featureName] <- selectedFeature
  return(chrData)
}