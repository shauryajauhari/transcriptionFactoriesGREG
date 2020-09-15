## Author : Shaurya Jauhari
## Last Reviewed: September 15th, 2020.
## Description: Download BAM files

downloadBAMfiles <- function(cell, feature)
{
  ## Check if any or both arguments are missing.
  
  if(missing(cell) | missing(feature))
  {
    return("Argument missing.")
  }
  
  
  ## Install packages (if absent)
  
  requiredPackages <- c("curl", "readxl")
  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) install.packages(newPackages)

  ## Loading package libraries
 
   library(curl)
  library(readxl) 
  
  
  ## Loading metadata table
  
  masterData <- read_excel(paste0(getwd(),"/siftedData.xlsx"))
  
  
  ## Loading definitions
  
  cells <- c("A549", "H1ESC", "HELA", "IMR90", "K562", "MCF7")
  features <- c("CTCF", "EP300", "H3K27me3", "H3K36me3", "H3K4me1", "H3K4me2", "H3K4me3", "H3K9ac", "H3K9me3", "RAD21", "RNAPol2", "RNAPol3", "RNA-Seq", "YY1")
  
  
  if(cell %in% cells & feature %in% features)
  {
    for (lenList in 1:length(unlist(strsplit(masterData$`Download Link`[masterData$`Cell Type`== cell & masterData$Feature == feature][[1]], ","))))
    {
      curl_download(url = trimws(unlist(strsplit(masterData$`Download Link`[masterData$`Cell Type`== cell & masterData$Feature == feature][[1]], ","))[lenList]),
                    destfile = print(paste0(paste0(getwd(), "/GREG/", cell), paste0("/", feature), 
                                 paste0("/", basename(unlist(strsplit(masterData$`Download Link`[masterData$`Cell Type`== cell
                                                                               & masterData$Feature == feature][[1]], ","))[lenList])))),
                    quiet = FALSE)
    }
  }
  else
  {
    return("Invalid cell-type or feature.")
  }
}