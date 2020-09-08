checkingBAMDownload <- function(cell, feature)
{
  
  cells <- c("A549", "H1ESC", "HELA", "IMR90", "K562", "MCF7")
  features <- c("CTCF", "EP300", "H3K27me3", "H3K36me3", "H3K4me1", "H3K4me2", "H3K4me3", "H3K9ac", "H3K9me3", "RAD21", "RNAPol2", "RNAPol3", "RNA-Seq", "YY1")
  
  library(readxl) 
  # masterData <- read_excel(paste0(getwd(),"/siftedData.xlsx"))


  if(cell %in% cells & feature %in% features)
  {
    
    ## Define a counter variable
    count = 0
  
    ## List the BAM files  
    count <- length(list.files(paste0(getwd(),"/GREG/",cell, "/", feature, "/")))
  
    ## Total Number of files as listed in the master table
    totalFiles <- length(unlist(strsplit(masterData$`Download Link`[masterData$`Cell Type`== cell & masterData$Feature == feature][[1]], ",")))
  
    ## Condition Checking | downloaded files versus the listed files
    ifelse(count == totalFiles, return("Successfully Downloaded!"), return("Missing files. Download Error. Please check!"))
    
  }
  
  else
  {
    return("Invalid cell-type or feature.")
  }

}