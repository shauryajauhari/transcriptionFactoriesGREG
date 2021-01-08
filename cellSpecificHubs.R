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

## using Plotly
library(plotly)
fig <- plot_ly(x = c("A549", "H1ESC", "HeLa", "IMR90", "K562", "MCF7") ,                                  
        y = as.numeric(freq),
        type = "bar", color = I("orange"))
        
fig <- fig %>% layout(title = "Hub Frequencies in Cells",
                      yaxis = list(title = 'Number of Hubs'),
                      xaxis = list(title = 'Cell'))
fig

##using ggplot2
library(ggplot2)
ggplot(forPlot, aes(x = c("A549", "H1ESC", "HeLa", "IMR90", "K562", "MCF7"),
                    y = as.numeric(freq))) +        
  geom_bar(stat = "identity")


## Creating a Venn Diagram
## Installing and Loading Packages

library(devtools)
install_github("js229/Vennerable")
library(Vennerable)

myVenn <- Venn(fullData)
plot(myVenn, doWeights = FALSE)
