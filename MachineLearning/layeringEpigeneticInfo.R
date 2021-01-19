

load("../data/GREG_cluster_v3.RData")
source("./R/extractingHubsFromTruePositives.R")

a549LR <- extractingHubsFromTruePositives("./results/truePositives/a549LR.txt", "../intervalsMasterReferenceGREG19.txt")
a549RF <- extractingHubsFromTruePositives("./results/truePositives/a549RF.txt", "../intervalsMasterReferenceGREG19.txt")
k562LR <- extractingHubsFromTruePositives("./results/truePositives/k562LR.txt", "../intervalsMasterReferenceGREG19.txt")
k562RF <- extractingHubsFromTruePositives("./results/truePositives/k562RF.txt", "../intervalsMasterReferenceGREG19.txt")
imr90RF <- extractingHubsFromTruePositives("./results/truePositives/imr90RF.txt", "../intervalsMasterReferenceGREG19.txt")
imr90LR <- extractingHubsFromTruePositives("./results/truePositives/imr90LR.txt", "../intervalsMasterReferenceGREG19.txt")
mcf7LR <- extractingHubsFromTruePositives("./results/truePositives/mcf7LR.txt", "../intervalsMasterReferenceGREG19.txt")
mcf7RF <- extractingHubsFromTruePositives("./results/truePositives/mcf7RF.txt", "../intervalsMasterReferenceGREG19.txt")
helaRF <- extractingHubsFromTruePositives("./results/truePositives/helaRF.txt", "../intervalsMasterReferenceGREG19.txt")
helaLR <- extractingHubsFromTruePositives("./results/truePositives/helaLR.txt", "../intervalsMasterReferenceGREG19.txt")
h1escLR <- extractingHubsFromTruePositives("./results/truePositives/h1escLR.txt", "../intervalsMasterReferenceGREG19.txt")
h1escRF <- extractingHubsFromTruePositives("./results/truePositives/h1escRF.txt", "../intervalsMasterReferenceGREG19.txt")



require(plyr)
conservedHubs <- join_all(list(a549LR, a549RF, mcf7LR, mcf7RF, imr90LR, imr90RF, helaLR, helaRF, h1escLR, h1escRF, k562LR, k562RF), 
                          by = 'chr', 
                          type = 'inner')


save(a549LR, a549RF, mcf7LR, mcf7RF, imr90LR, imr90RF, k562LR, k562RF, helaLR, helaRF, h1escLR, h1escRF, file = "allCellsHubs.RData")



masterIntervalsBinIDS <- read.table("../intervalsMasterReferenceGREG19.txt", header = T)
masterIntervalsBinIDS <- masterIntervalsBinIDS[, -4]


Long_Range_relationships[Long_Range_relationships$LR_id==temp$LRIDs[[1]][[1]],]

del <- vector( mode = "list", length = length(temp$LRIDs))

for (i in 1:length(temp$LRIDs))
{
  for (j in 1:length(temp$LRIDs[[i]]))
  {
    del[[i]][[j]] <- Long_Range_relationships[Long_Range_relationships$LR_id == temp$LRIDs[[i]][[j]], c ("A_label", "A_Start", "A_End", "B_label", "B_Start", "B_End")]
    # names(del[[i]]) <- temp$LRIDs[[i]]
  }
}



########### SUCCESSFUL EXECUTION #########

bins <- names(LR_relationships_per_bin)

LRIDs <- vector(mode = "list", length = length(LR_relationships_per_bin)) 

for (i in 1:length(LR_relationships_per_bin))
{
  for (j in 1:length(LR_relationships_per_bin[[i]]))
  {
    LRIDs[[i]][j] <- LR_relationships_per_bin[[i]][j]
  }
}

anotherList <- cbind(bins, LRIDs)
anotherList <- as.data.frame(anotherList)

library(dplyr)

names(anotherList) <- c("binsGREGformat", "LRIDs")

anotherList$binsGREGformat <- as.character(anotherList$binsGREGformat)

final <- inner_join(masterIntervalsBinIDS, anotherList, by = "binsGREGformat")


### make a collection of all ML derived hubs from all cell-types; select unique  #####

everything <- rbind.data.frame(a549LR, a549RF, mcf7LR, mcf7RF, imr90LR, imr90RF, k562LR, k562RF, helaLR, helaRF, h1escLR, h1escRF)
everything <- unique(everything)


mapped <- inner_join(final, everything, by = "binsGREGformat")
mapped <- mapped[, c("chr.x", "start.x", "end.x", "binsGREGformat", "LRIDs.x", "GenomicRanges")]
colnames(mapped) <- c("chr", "start", "end", "binsGREGformat", "LRIDs", "GenomicRanges")

mapped <- unique(mapped)

############ MAPPING TFs and LNCRNAs ##############

temp <- vector( mode = "list", length = length(subnetwork_per_LR))

for (i in 1: length(subnetwork_per_LR))
{
  temp[[i]][[1]]<- vector(mode = "list", length = length(subnetwork_per_LR[[i]]))
  temp[[i]][[1]] <- cbind(names(subnetwork_per_LR[[i]]))
  
  temp[[i]][[2]] <- cbind(subnetwork_per_LR[[i]][[1]]$TFs)
  
  temp[[i]][[3]] <- cbind(subnetwork_per_LR[[i]][[1]]$lncRNAs)
  
}

names(temp) <- names(subnetwork_per_LR)

# --- Create new columns --- #
mapped$TFs <- vector (mode = "list", length = nrow(mapped))
mapped$lncRNAs <- vector (mode = "list", length = nrow(mapped))


# --- Populate the columns --- #

# --- Working Subset --- #
for(i in 1:length(test1$LRIDs))
{
  for (j in 1:length(test2))
  {
    if (names(test2[j]) %in% test1$LRIDs[[i]])
    {
      try(test1$TFs[[i]] <- as.character(test2[[j]][[2]]), silent = TRUE)
      try(test1$lncRNAs[[i]] <- as.character(test2[[j]][[3]]), silent = TRUE)
    }
  }
}


###

for(i in 1:length(mapped$LRIDs))
  {
    for (j in 1:length(temp))
      {
        if (names(temp[j]) %in% mapped$LRIDs[[i]])
          {
            try(mapped$TFs[[i]] <- as.character(temp[[j]][[2]]), silent = TRUE)
            try(mapped$lncRNAs[[i]] <- as.character(temp[[j]][[3]]), silent = TRUE)
          }
      }
  }





