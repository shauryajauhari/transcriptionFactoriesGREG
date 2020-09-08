context("BAM file downloads") 

test_that("Download exists", {expect_output(checkingBAMDownload("A549", "CTCF"), "Successfully Downloaded!") })
test_that("Download exists", {expect_output(checkingBAMDownload("A549", "NANA"), "Invalid cell-type or feature.") })