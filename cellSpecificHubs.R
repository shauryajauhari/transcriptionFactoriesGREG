## Load results from GREG

load("./data/GREG_cluster_v3.RData")
load("./MachineLearning/epigeneticInfoBins.RData")

## Subset information: cell-types and LRIDs only

onlyData <- Long_Range_relationships[, c(1,6)]

## Rename columns

names(onlyData) <- c("LRIDs", "celltype")

## Merge data frames based on LRIDs (common column)
jointSet <- merge(onlyData, mapped, by="LRIDs")

## Sifting cell-specific bin-data

cells <- unique(jointSet$celltype)

for(cell in cells)
{
  assign(paste0(cell,"Data"), jointSet[jointSet$celltype == cell, 6])
}

fullData <- list(A549Data, H1ESCData, HELAData, IMR90Data, K562Data, MCF7Data)
names(fullData) <- c("A549", "H1ESC", "HeLa", "IMR90", "K562", "MCF7")

## Plotting cell-wise frequency
freq <- vector(mode = "list", length = length(fullData))
for(i in 1:length(fullData)) freq[i] <- length(fullData[[i]])

barplot(as.numeric(freq),
main = "Number of Hubs in each Cell",
xlab = "Cell",
ylab = "Number of Hubs",
names.arg = c("A549", "H1ESC", "HeLa", "IMR90", "K562", "MCF7"),
col = "darkred",
horiz = FALSE)

## basic barplot
forPlot <- data.frame(c("A549", "H1ESC", "HeLa", "IMR90", "K562", "MCF7"),
                       as.numeric(freq))  


requiredPackages <- c("plotly", "ggplot2")
newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages, 
                                        dependencies = TRUE,
                                        repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(ggplot2))

## using Plotly

fig <- plot_ly(x = c("A549", "H1ESC", "HeLa", "IMR90", "K562", "MCF7") ,                                  
        y = as.numeric(freq),
        type = "bar", color = I("orange"))
        
fig <- fig %>% layout(title = "Hub Frequencies in Cells",
                      yaxis = list(title = 'Number of Hubs'),
                      xaxis = list(title = 'Cell'))
fig

## Using ggplot2

ggplot(forPlot, aes(x = c("A549", "H1ESC", "HeLa", "IMR90", "K562", "MCF7"),
                    y = as.numeric(freq))) +        
  geom_bar(stat = "identity")


## Creating a Venn Diagram
## Installing and Loading Packages

requiredPackage <- "Vennerable"
newPackage <- requiredPackage[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(newPackage)) install_github("js229/Vennerable")
suppressPackageStartupMessages(library(Vennerable))


## Plot

myVenn <- Venn(fullData)
plot(myVenn, doWeights = FALSE)


## Counting bin interactions

sourceBin <- vector(mode="list", length = length(subnetwork_per_LR))
destinationBin <- vector(mode="list", length = length(subnetwork_per_LR))

for(i in 1:length(subnetwork_per_LR))
{
  destinationBin[[i]] <- vector(mode = "list", length = length(subnetwork_per_LR[[i]]))
  for(j in 1:length(subnetwork_per_LR[[i]]))
  {
    sourceBin[[i]][[j]] <- names(subnetwork_per_LR[[i]][j])
    destinationBin[[i]][[j]] <- subnetwork_per_LR[[i]][[j]][[6]]
  }
}

## clubbing all source and destination bins together

masterFrame <- vector(mode="list", length = length(subnetwork_per_LR))

for(i in 1:length(subnetwork_per_LR))
  {
    masterFrame[[i]] <- data.frame(cbind(sourceBin[[i]]), cbind(destinationBin[[i]]))
}


allBins <- do.call("rbind", masterFrame)
names(allBins) <- c("SourceBin", "CorrespondingBins")



## Counting Interactions in same chromosome and different chromosomes

for(i in 1:nrow(allBins))
{
  ## define counter variables
  ## refreshing values to zero for each new row
  sameCount <- 0
  differentCount <- 0
  
  for(j in 1:length(allBins$CorrespondingBins[[i]])){
    ifelse(gsub("\\:.*", "", allBins$SourceBin[[i]]) == 
             gsub("\\:.*", "", allBins$CorrespondingBins[[i]][[j]]), 
           sameCount <- sameCount + 1,
           differentCount <- differentCount + 1)
  }
  ## assign new columns holding counts for each bin
  
  allBins$WithinChromosomeInteractions[[i]] <- sameCount
  allBins$OutsideChromosomeInteractions[[i]] <- differentCount
}

## saving object

saveRDS(object = allBins, file = "allBinsInteractionCount.rds")

## stacked barplot for number of interactions per bin

fig1 <- plot_ly(allBins, x = ~SourceBin, y = ~WithinChromosomeInteractions, 
                type = 'bar', name = 'Within Chromosome Interactions')
fig1 <- fig1 %>% add_trace(y = ~OutsideChromosomeInteractions, name = 'Outside Chromosome Interactions')
fig1 <- fig1 %>% layout(yaxis = list(title = 'Frequency'), barmode = 'stack')

fig1
