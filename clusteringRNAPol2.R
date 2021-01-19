setwd("~/Desktop/transcriptionFactoriesGREG")

a549 <- read.table("./MachineLearning/data/A549forML.txt", header = TRUE)

## factoring classes

a549Hub <- a549[a549$Class=="Hub", ]
a549NonHub <- a549[a549$Class=="Non-Hub", ]

## sub-factoring on the basis of RNAPol2 values

a549HubOverMedian <- a549Hub[a549Hub$RNAPol2 >= median(a549Hub$RNAPol2), ] 
a549HubUnderMedian <- a549Hub[a549Hub$RNAPol2 < median(a549Hub$RNAPol2), ] 
a549NonHubOverMedian <- a549NonHub[a549NonHub$RNAPol2 >= median(a549NonHub$RNAPol2), ] 
a549NonHubUnderMedian <- a549NonHub[a549NonHub$RNAPol2 < median(a549NonHub$RNAPol2), ] 


## bar plots

plotLengths <- c(nrow(a549HubOverMedian), nrow(a549HubUnderMedian), 
                    nrow(a549NonHubOverMedian), nrow(a549NonHubUnderMedian))

library(plotly)

fig <- plot_ly(
  x = c("Hubs with High RNAPol2", "Hubs with Low RNAPol2", "Non-Hubs with High RNAPol2", 
        "Non-Hubs with Low RNAPol2"),
  y = plotLengths,
  name = "Counts",
  type = "bar"
)

fig <- fig %>% layout(title = "Group-Cardinality in A549",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Count"))

fig


## measures of central tendency

range(a549$RNAPol2)
median(a549$RNAPol2)
mean(a549$RNAPol2)


getMode <- function(x) {
  cats <- unique(x)
  cats[which.max(tabulate(match(x, cats)))]
}

getMode(a549$RNAPol2)


## RNAPol2 distribution

library(ggplot2)
ggplot(a549) + 
  geom_bar(aes(RNAPol2), position = "dodge") + 
  ggtitle("Distribution of RNAPol2  in A549 cell-data") +
xlab("RNAPol2 Coverage Values") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5))
