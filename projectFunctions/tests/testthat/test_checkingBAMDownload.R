context("BAM file downloads") 

## Check this after you have successfully downloaded the BAM files 
test_that("Download exists", {expect_identical(checkingBAMDownload("A549", "CTCF"), "Successfully Downloaded!") })

test_that("Invalid input", {expect_identical(checkingBAMDownload("A549", "NANA"), "Invalid cell-type or feature.") })
test_that("Download absent", {expect_identical(checkingBAMDownload("A549", "YY1"), "Missing files. Download Error. Please check!") })