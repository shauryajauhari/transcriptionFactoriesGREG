context("Coverages from BAM files") 

test_that("Download exists", {expect_is(postProcessBAMFiles("A549"), "data.frame") })