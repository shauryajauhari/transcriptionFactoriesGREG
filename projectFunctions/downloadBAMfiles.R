downloadBAMfiles <- function(cell, feature)
{
  ## Install "curl" package (if absent) and load library
  if (!require(curl)) install.packages("curl", dependencies = TRUE)
  library(curl)
  
  if(cell %in% cells & feature %in% features)
  {
    for (lenList in 1:length(unlist(strsplit(masterData$`Download Link`[masterData$`Cell Type`== cell & masterData$Feature == feature][[1]], ","))))
    {
      curl_download(trimws(unlist(strsplit(masterData$`Download Link`[masterData$`Cell Type`== cell & masterData$Feature == feature][[1]], ","))[lenList]), 
                    print(paste0(paste0("./GREG/",cell), paste0("/", feature), 
                                 paste0("/", basename(unlist(strsplit(masterData$`Download Link`[masterData$`Cell Type`== cell
                                                                               & masterData$Feature == feature][[1]], ","))[lenList])))))
    }
  }
  else
  {
    print("Invalid cell-type or feature.")
  }
}