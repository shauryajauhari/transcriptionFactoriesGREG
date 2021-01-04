## Author : Shaurya Jauhari
## Last Reviewed: January 4th, 2021.
## Description: This function takes true positive data for a cell-type and add the corresponding binIDs and attributes
## of genomic ranges from GREG.
## It takes as input two references: 
## 1. intervalsMasterReferenceGREG.txt (code mentioned implicitly for referral)
## 2. GREG_cluster_v3.RData (GREG cluster results)

extractingHubsFromTruePositives <- function (filePath, masterIntervalsFilePath){
  
  # --- Checking file existence --- #
  if(file.exists(filePath))
  {
    # --- Reading file from disk --- #
    dataFile <- read.table(filePath, header = TRUE)
    
    # --- Removing redundancy, if exists --- #
    dataFileUnique <- unique(dataFile)
    
    # --- Sorting the data on the basis of chromosome number and start index, if not already ---#
    dataFileUnique <- dataFileUnique[order(dataFileUnique$chr, dataFileUnique$start),]
    
    # --- Extracting "Hubs" only --- #
    dataFileUnique <- dataFileUnique[dataFileUnique$Class == "Hub" & dataFileUnique$Prediction == "Hub", ]
    
    # --- Creating a new column that holds Hub IDs - Hub1, Hub2 ... ---#
    dataFileUnique$HubID <- "Hub"
    
    # --- Assigning Hub IDs sequentially ---#
    for (i in 1: nrow(dataFileUnique)) {dataFileUnique$HubID[i] <- paste0(dataFileUnique$HubID[i],i)}
    
    # -------------------------------------------------------------- #
    # --- Allocating genomic ranges defined in GREG for each hub --- #
    # -------------------------------------------------------------- #
    
    # --- Locating the cross-reference for Bin-IDs in GREG format --- #
    #######################################################################################################################################
    # The following chunk of code returns the object that we'll be using for the cross-reference. 
    # "targetBED" is actually a pre-defined object (in another workflow), that holds 2Kb intervals under "chr", "start", and "end" indices.
    
    # ## Create an instance
    # intervals <- targetBED
    # 
    # ## Extract length of each chromosome type.
    # levs <- levels(intervals$chrom)
    # 
    # for (item in levs)
    # {
    #   occur =0
    #   for(len in 1:nrow(intervals))
    #   {
    #     if (intervals$chrom[len]== item)
    #     {
    #       occur = occur + 1
    #       intervals$binIds[len] <- paste0("Bin", as.character(occur))
    #     }
    #   }
    #   cat("The chromosome", item, "has", occur, "occurences. \n")
    # }
    # 
    # ## Save file
    # intervals$binsGREGformat <- paste0(intervals$chr,":",intervals$binIds)
    # write.table(intervals, file = "intervalsMasterReferenceGREG.txt", sep = "\t", row.names = FALSE, quote = FALSE)
    #######################################################################################################################################
    
    # --- Loading instance --- #
    masterIntervalsBinIDS <- read.table(as.character(masterIntervalsFilePath), header = T)

    # --- Assign Bin-IDs for the given Hubs --- #
    resData <- merge(dataFileUnique, masterIntervalsBinIDS, by.x = c("chr", "start", "end") , by.y = c("chr", "start", "end")) 
    
    # --- Extracting Long Range relationships (IDs; these are chr-chr interactions larger than IMb in GREG) --- #
    # --- Loading pre-processed data --- #
    resData$LRIDs <- LR_relationships_per_bin[resData$binsGREGformat]
    
    # --- Mapping the genomic ranges --- #
    buffer <- vector( mode = "list", length = length(resData$LRIDs))
    for (i in 1:length(resData$LRIDs))
    {
      for (j in 1:length(resData$LRIDs[[i]]))
      {
        buffer[[i]][[j]] <- Long_Range_relationships[Long_Range_relationships$LR_id == resData$LRIDs[[i]][[j]], c ("A_label", "A_Start", "A_End", "B_label", "B_Start", "B_End")]
      }
    }
    
    # --- Assign as a new column --- #
    resData$GenomicRanges <- buffer
    
    # --- Naming list elements for easy access --- #
    for (i in 1:length(resData$LRIDs))
    {
      names(resData$GenomicRanges[[i]]) <- resData$LRIDs[[i]]
    }
    
    # --- Output --- #
    return(resData[, c("HubID", "chr", "start", "end", "binsGREGformat", "LRIDs", "GenomicRanges")])
    
  }
  else
  {
    # --- Throw an error for an inaccessible file --- #
    return("File not found!")
  }
}