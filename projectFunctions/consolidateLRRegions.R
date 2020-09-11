
consolidateLRRegions <- function(cell){

files <<- list.files("../results/", cell, "/LR-hubs/", pattern = "\\.bed$") # Extracting BED files from the input directory; searches for sub-sub folders too.
files <<- substr(files,1,nchar(files)-4) # Clipping file extension to retrieve sample names only.

LRregions <- list() ## Creating a consolidated region-set for the long range interactions.

for(i in 1:length(files))
{
  LRregions[[i]] <- read.table(paste0("../results/", cell, "/LR-hubs/",paste0(eval(parse(text="files[i]")),".bed")), sep = "\t")
  LRregions[[i]] <- LRregions[[i]][,1:3]
  colnames(LRregions[[i]]) <- c("chr", "start", "end")
  LRregions[[i]] <- LRregions[[i]][order(LRregions[[i]]$chr),]
}

LRregionsAll <- do.call("rbind", LRregions) ## combining all regions under one dataframe
write.table(LRregionsAll, file = paste0("../GREG/", cell, "/", cell, "LRs.txt" , row.names = FALSE, col.names = TRUE) ## Save file

} 


