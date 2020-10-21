## Author : Shaurya Jauhari
## Last Reviewed: October 21st, 2020.
## Description: This function outputs (in rounded percentage) the proportion of rows in the data that are instances of non-zero values.
## Even if there is a single zero value in a row, that row will not be counted. The idea is to determine the sparsity in the data;
## sparse is a data set where most of the item values are zero. 
## It takes two arguments. First is the path of the data file, and second is the columns to omit (if any).
## The columns to omit must be provided as a list, combining multiple elements with a "c()".

determineNonZeroProportion <- function(dataFilePath, columnsToOmit)
{
  
  if(file.exists(dataFilePath))
  {
    # Read file
    dataFile <- read.table(dataFilePath, header = TRUE)
    
    # Refining Data | Removing irrelevant columns
    # Bypassing, if not 
    ifelse(missing(columnsToOmit), refinedDataFile <- dataFile, refinedDataFile <- dataFile[, -columnsToOmit])
    
    # Calculating metric
    corezero <- apply (refinedDataFile, 1, function(row) all(row!=0))
    remainder <- refinedDataFile[corezero,]
    cat("The percentage of (perfect) non-zero rows in the data is", round((nrow(remainder)/nrow(refinedDataFile))*100), "%")
  }
  else
  {
    return("File does not exist.")
  }
}