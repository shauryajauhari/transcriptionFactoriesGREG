context("Bin Intervals") 

test_that("Invalid input", {expect_identical(checkingBins("LULU"), "Invalid cell-type.") })
test_that("Pass", {expect_identical(checkingBins("A549"), "All good!") })