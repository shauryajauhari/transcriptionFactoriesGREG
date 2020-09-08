context("BAM file downloads") 

## Loading background information
#masterData <- read_excel("siftedData.xlsx")
cells <- c("IMR90", "MCF7", "HELA", "K562", "H1ESC", "A549")
features <- c("CTCF", "EP300", "H3K27me3", "H3K36me3", "H3K4me1", "H3K4me2", 
             "H3K4me3", "H3K9ac", "H3K9me3", "RAD21", "RNAPol2", "RNAPol3", "RNA-Seq", "YY1")


test_that("Download exists", {expect_output(checkBAMDownload("A549", "CTCF"), TRUE) })
test_that("Download exists", {expect_output(checkBAMDownload("A549", "NANA"), FALSE) })