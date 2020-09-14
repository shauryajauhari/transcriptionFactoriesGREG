context("Structuring scaled BED format") 

test_that("Successful execution", {expect_match(prepareMasterIntervalsBinIds("hg19_2k_bins.bed"), "File saved successfully!") })
test_that("Unsuccessful execution", {expect_match(prepareMasterIntervalsBinIds("notMe.bed"), "File not found. Please check!") })
