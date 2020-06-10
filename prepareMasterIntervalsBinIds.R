## Read table
intervals <- read.table("hg19_2k_bins.bed", sep = "\t", header = FALSE)

## Define columns
colnames(intervals) <- c("chr", "start", "end")

## Extract length of each chromosome type.
levs <- levels(intervals$chr)

for (item in levs)
{
  occur =0
  for(len in 1:nrow(intervals))
    {
      if (intervals$chr[len]== item)
        {
          occur = occur + 1
          intervals$binIds[len] <- paste0("Bin", as.character(occur))
        }
    }
  cat("The chromosome", item, "has", occur, "occurences. \n")
}

## Save file
intervals$binsGREGformat <- paste0(intervals$chr,":",intervals$binIds)
write.table(intervals, file = "intervalsMasterReferenceGREG.txt", sep = "\t", 
            row.names = FALSE, quote = FALSE)
