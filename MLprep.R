scoreTable <- read.table("./GREG/A549/normalizedReads.txt", header = TRUE) ## importing scores

LRhubs <- read.table("./GREG/A549/A549LRs.txt", header = TRUE) ## class:hub
bins <- read.table("./GREG/A549/binsRegions.txt", header = TRUE) ## all regions
colnames(bins) <- c("chr", "start", "end") ## renaming columns



## combining regions to scores

masterTable <- cbind(bins, scoreTable)

## Hubs

library(dplyr)
hubs <- inner_join(masterTable, LRhubs)

hubs$Class <- "Hub"


## Non-hubs

nonHubs <- anti_join(masterTable, LRhubs)
nonHubs$Class <- "Non-Hub"

## Combining data for both classes into a superset.

megaTable <- full_join(hubs, nonHubs)
megaTable <- megaTable[with(megaTable, order(chr, start)), ] ## sorting the data

