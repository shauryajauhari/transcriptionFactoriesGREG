context("Structuring scaled BED format") 

test_that("Successful execution", {expect_silent(prepareMasterIntervalsBinIds("hg19_2k_bins.bed")) })
test_that("Unsuccessful execution", {expect_silent(prepareMasterIntervalsBinIds("notMe.bed")) })
