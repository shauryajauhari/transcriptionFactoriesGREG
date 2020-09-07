visualizeWithGviz <- function (featureFile, chrName, chrIndex, startIndex, endIndex, classIndex, modelPredictionProbabilities) {
  
  
  ## Install and Load the package for visualization: Gviz
  if (!require(Gviz)) install.packages('Gviz', dependencies = TRUE)
  library(Gviz)
  
  standardData <- read.table(featureFile, header = TRUE) ## full data
  fineData <- standardData[, c(as.numeric(chrIndex), as.numeric(startIndex), as.numeric(endIndex), as.numeric(classIndex))]
  names(fineData) <- c ("chr", "start", "end", "Class")
  ## Extracting data for particular chromosome
  
  dataNonHub <- fineData[fineData[as.numeric(chrIndex)] == as.character(chrName) & fineData[4] == "Non-Hub", ]
  dataHub <- fineData[fineData[as.numeric(chrIndex)] == as.character(chrName) & fineData[4] == "Hub", ]
  
  ## Plotting Hubs
  
  annotationTrackHub <- AnnotationTrack(range = dataHub, 
                                        name = "Hubs", 
                                        genome = "hg38", 
                                        chromosome = as.character(chrName))
  itrack <- IdeogramTrack(genome = "hg38", chromosome = as.character(chrName))
  genomeTrack <- GenomeAxisTrack()
  plotTracks(list(annotationTrackHub, itrack, genomeTrack), 
             background.panel = "#FFFEDB", 
             background.title = "darkblue",
             stacking = "dense")
  dev.off()
  
  ## Plotting Non-Hubs
  
  annotationTrackNonHub <- AnnotationTrack(range = dataNonHub, 
                                           name = "Non-Hubs", 
                                           genome = "hg38", 
                                           chromosome = as.character(chrName))
  itrack <- IdeogramTrack(genome = "hg38", chromosome = as.character(chrName))
  plotTracks(list(annotationTrackNonHub, itrack),
             background.panel = "#FFFEDB", 
             background.title = "darkblue",
             stacking = "dense")
  dev.off()
  
  ## Merging Actual and Predicted Classes
  
  dataPredicted <- cbind(fineData, modelPredictionProbabilities)
  names(dataPredicted)[names(dataPredicted) == "modelPredictionProbabilities"] <- "Prediction"
  
  ## Define positive and negative class definitions
  
  dataPredicted$Prediction <- ifelse(dataPredicted$Prediction == 1, "Non-Hub", "Hub")            
  dataPredicitedHub <- dataPredicted[dataPredicted[as.numeric(chrIndex)] == chrName & dataPredicted$Prediction=="Hub", ]
  
  ## Plotting Predicted Hubs
  
  annotationTrackHub <- AnnotationTrack(range = dataHub, 
                                        name = "Hubs", 
                                        genome = "hg38", 
                                        chromosome = as.character(chrName))
  annotationTrackPredictedHub <- AnnotationTrack(range = dataPredicitedHub, 
                                                 name = "Predicted Hubs", 
                                                 genome = "hg38", 
                                                 chromosome = as.character(chrName))
  itrack <- IdeogramTrack(genome = "hg38", chromosome = as.character(chrName))
  genomeTrack <- GenomeAxisTrack()
  plotTracks(list(annotationTrackPredictedHub,annotationTrackHub, itrack, genomeTrack), 
             background.panel = "#FFFEDB", 
             background.title = "darkblue",
             stacking = "dense")
  
  ## True Positives
  
  dataPredicitedHub[dataPredicitedHub$Class == dataPredicitedHub$Prediction , ]
  
}
  
  
  
  
  
  
  
  
  
  
  




