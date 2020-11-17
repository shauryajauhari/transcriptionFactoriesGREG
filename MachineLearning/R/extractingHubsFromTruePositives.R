extractingHubsFromTruePositives <- function (filePath){
  if(file.exists(filePath))
  {
    dataFile <- read.table(filePath, header = TRUE)
    dataFileUnique <- unique(dataFile)
    return(dataFileUnique[dataFileUnique$Class == "Hub" & dataFileUnique$Prediction == "Hub", c(1:3)])
  }
  else
  {
    return("File not found!")
  }
}