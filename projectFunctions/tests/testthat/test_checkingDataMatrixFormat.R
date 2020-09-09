context("Counts per feature") 

test_that("Invalid input", {expect_identical(checkingDataMatrixFormat("LULU"), "Invalid cell-type.") })
test_that("Pass", {expect_identical(checkingDataMatrixFormat("A549"), "All good!") })
test_that("Download absent", {expect_identical(checkingDataMatrixFormat("MCF7"), "Ouch. Did you miss something!") })