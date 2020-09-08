context("BAM file downloads") 
#test_that("Inputs are strings", {expect_type(downloadBAMfiles(cell, feature), "character", "character") }) 

test_that("Output", {expect_output(downloadBAMfiles(cell, feature), ) }) 