# Install and Load Package
install.packages(c("factoextra", "psych"), 
                 dependencies = TRUE, 
                 repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")

library(factoextra)

# Load Data
a549data <- read.table("./data/A549forML.txt", header = TRUE)


# Prune data for variables only
dataOnly <- a549data[, -c(1:3)]

# Normalize the data to fit the range of [0,1]
source("./R/minMaxNormalization.R")
dataOnly[,-13] <- minMaxNormalization(dataOnly[,-13])

# Transpose Data
#t_dataOnly <- t(dataOnly[1:3267, ])

# Execute PCA
pcaRun <- prcomp(dataOnly, center = TRUE) # scale. = FALSE because we have already normalized the data

# Visualization of Results
fviz_eig(pcaRun)
fviz_pca_ind(pcaRun,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)            


library(psych)            
pairs.panels(dataOnly[,-13],
             gap = 0,
             bg = c("red", "green", "blue")[dataOnly$Class],
             pch = 21)           
             