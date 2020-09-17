visualizeWithGviz <- function (featureFile, chrName, chrIndex, startIndex, endIndex, classIndex, modelPredictionProbabilities) {
  
  
  ## Install (if absent) and Load the package for visualization: Gviz
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", dependencies = TRUE)
  BiocManager::install("Gviz")
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
  png(file = paste0(as.character(chrName), "Hubs", ".png"))
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
  png(file = paste0(as.character(chrName), "Non-Hubs", ".png"))
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
  png(file = paste0(as.character(chrName), "PredictedHubs", ".png"))
  plotTracks(list(annotationTrackPredictedHub,annotationTrackHub, itrack, genomeTrack), 
             background.panel = "#FFFEDB", 
             background.title = "darkblue",
             stacking = "dense")
  dev.off()

  ## True Positives
  
  dataPredicitedHub <- dataPredicitedHub[dataPredicitedHub$Class == dataPredicitedHub$Prediction , ]
  write.table(dataPredicitedHub, 
              file = paste0(as.character(chrName), "TruePositives.txt"),
              sep = "\t", 
              row.names = FALSE, 
              quote = FALSE) 
  
  
  ## True Negatives
  
  dataPredicitedNonHub <- dataPredicted[dataPredicted[as.numeric(chrIndex)] == chrName & dataPredicted$Prediction=="Non-Hub", ]
  dataPredicitedNonHub <- dataPredicitedNonHub[dataPredicitedNonHub$Class == dataPredicitedNonHub$Prediction , ]
  write.table(dataPredicitedNonHub, 
              file = paste0(as.character(chrName), "TrueNegatives.txt"),
              sep = "\t", 
              row.names = FALSE, 
              quote = FALSE) 
  
}
  
  
  
  
  
  
  
  
  
  
  




