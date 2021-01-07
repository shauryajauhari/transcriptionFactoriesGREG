## Load results from GREG

load("./data/GREG_cluster_v3.RData")
load("./MachineLearning/epigeneticInfoBins.RData")

## Subset information: cell-types and LRIDs only

onlyData <- Long_Range_relationships[, c(1,6)]

## Rename columns

names(onlyData) <- c("LRIDs", "celltype")

## Merge data frames based on LRIDs (common column)
jointSet <- merge(temp, mapped, by="LRIDs")

## Sifting cell-specific bin-data

cells <- unique(jointSet$celltype)

for(cell in cells)
{
  assign(paste0(cell,"Data"), jointSet[jointSet$celltype == cell, 6])
}

fullData <- list(A549 <- A549Data, 
                 H1ESC <- H1ESCData, 
                 HeLa <- HELAData, 
                 IMR90 <- IMR90Data, 
                 K562 <- K562Data, 
                 MCF7 <- MCF7Data)

## Creating a Venn Diagram
'/
//'
## Installing and Loading Packages

if (!require(devtools)) install.packages("devtools")
devtools::install_github("yanlinlin82/ggvenn")
library(ggvenn)

ggvenn(fullData)
