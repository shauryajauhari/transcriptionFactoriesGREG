checkingDataMatrixFormat <- function(cell){
  
  cells <- c("A549", "H1ESC", "HELA", "IMR90", "K562", "MCF7")
  
  if(cell %in% cells)
  {
    ## Load coverage data file
    covFile <- read.table(paste0(getwd(), "/GREG/", cell, "/normalizedReads.txt"), header = TRUE)
    
    ## Column count
    numberColumns <- length(covFile)
    
    ## Number of folders for features
    numberFolders <- length(list.dirs(path = paste0(getwd(), "/GREG/", cell, "/")))
    
    ## Condition checking | have all features been covered?
    ifelse(numberColumns == numberFolders, return("All good!"), return("Ouch! Did you miss something?"))
  }
  
  else
  {
    return("Invalid cell-type.")
  }

}