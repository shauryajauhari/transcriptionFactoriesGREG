checkingBAMDownload <- function(cell, feature)
{

  if(cell %in% cells & feature %in% features)
  {
    
    ## Define a counter variable
    count = 0
  
    ## List the BAM files  
    count <- length(list.files(paste0(paste0("./GREG/",cell), paste0("/", feature, "/"))))
  
    ## Total Number of files as listed in the master table
    totalFiles <- length(unlist(strsplit(masterData$`Download Link`[masterData$`Cell Type`== cell & masterData$Feature == feature][[1]], ",")))
  
    ## Condition Checking | downloaded files versus the listed files
    ifelse(count == totalFiles, return("Successfully Downloaded!"), return("Missing files. Download Error. Please check!"))
    
  }
  
  else
  {
    print("Invalid cell-type or feature.")
  }

}