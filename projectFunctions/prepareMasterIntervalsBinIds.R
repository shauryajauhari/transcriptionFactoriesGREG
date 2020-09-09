## Author : Shaurya Jauhari
## Last Reviewed: September 9th, 2020.
## Description: This function takes a regular, 3-column BED file (with specific intervals; without header) and assigns the following:
## (i) a Bin-ID, which is a unique id for each chromosome-specific bin; Bin1, Bin2, Bin3, ...
## (ii) a column with a concatenation for chromosome and Bin-ID; chr1:Bin1, chr1:Bin2, chr2:Bin1, ...
## (iii) Finally, structures a header to the file; "chr", "start", "end", "binIds", and	"binsGREGformat"
## ** For more information on GREG, visit https://doi.org/10.1093/database/baz162

prepareMasterIntervalsBinIds <- function(bedFile)
{
  ## Read table
  intervals <- read.table(bedFile, sep = "\t", header = FALSE)
  
  ## Define columns
  colnames(intervals) <- c("chr", "start", "end")
  
  ## Extract length of each chromosome type.
  levs <- levels(as.factor(intervals$chr))
  
  for (item in levs)
  {
    occur = 0
    for(len in 1:nrow(intervals))
    {
      if (intervals$chr[len] == item)
      {
        occur = occur + 1
        intervals$binIds[len] <- paste0("Bin", as.character(occur))
      }
    }
    cat("The chromosome", item, "has", occur, "occurences. \n")
  }
  
  ## Save file
  intervals$binsGREGformat <- paste0(intervals$chr,":",intervals$binIds)
  write.table(intervals, file = paste0( "../", "intervalsMasterReference", as.character(sub("\\..*", "", row.names(file.info(bedFile)))),".txt"), sep = "\t", 
              row.names = FALSE, quote = FALSE)
}

