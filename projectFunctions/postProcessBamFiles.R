# The following function covers "whole 9 yards"!. It takes any cell-type (as defined in "siftedData.xlsx"),
# and calculates the normalized read counts for every feature (again, as defined in "siftedData.xlsx"). The
# file is saved in the home directory of that cell-type.

postProcessBamFiles <- function(cell)
  {
  
  # Listing of all "readCounts.tab" from each feature.
  
  covFiles <- list.files(path=paste0("./GREG/", cell, "/"), pattern = ".tab", recursive = TRUE)

  # Loading those files to a list of dataframes.
  
dataCovFiles <- lapply(covFiles, function(path) {
  read.delim( file = paste0("./GREG/", cell, "/", path), header = TRUE, sep = "\t")
})

  # Naming list indices for intuitive access.

names(dataCovFiles) <- sub("\\/.*", "", covFiles)

  # Calculating the average of read counts from all replicates present.

avgReads <- lapply(dataCovFiles, function(avgDf){
  return(cbind(avgDf, rowMeans(avgDf[,-c(1:3)]))) # leaving out the first three columns, i.e. chr, start, end.
})

  # Trimming the dataframes for relevant columns only.

selectAvgReads <- lapply(avgReads, function(d){
  subset(d, select = c("X..chr.", "X.start.", "X.end.", "rowMeans(avgDf[, -c(1:3)])"))
})

  # Merge all dataframes first and then remove the "chr", "start", and "end" columns.

install.packages("plyr")
library(plyr)

combinedAvgReads <- join_all(selectAvgReads, by=c("X..chr.", "X.start.", "X.end."), type='left')

  # Since we no longer need the bins' attributes, we omit them here. Now we're left with the read counts for
  # all features, against each given feature.

combinedAvgReads <- combinedAvgReads[,-c(1:3)]
colnames(combinedAvgReads) <- names(dataCovFiles)

  # Replacing NAs induced by combining dataframes by 0.
combinedAvgReads[is.na(combinedAvgReads)] <- 0

  # Normalizing reads by BPM.
normAvgReads <- as.data.frame(sapply(combinedAvgReads, bpmNormalize))

  # Save file and write as output.

write.table(normAvgReads, file = paste0("./GREG/", cell, "/normalizedReads.txt") , sep = "\t", row.names = FALSE)
}

