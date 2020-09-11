context("Coverages from BAM files") 

test_that("Download exists", {expect_silent(postProcessBAMFiles("A549")) })
test_that("Invalid cell-type error", {expect_error(postProcessBAMFiles("LULU"), "Invalid cell-type.") })
