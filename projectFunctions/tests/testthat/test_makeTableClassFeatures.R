context("Bin Intervals") 

test_that("Invalid input", {expect_identical(makeTableClassFeatures("LULU"), "Invalid cell-type.") })
test_that("Pass", {expect_silent(makeTableClassFeatures("A549")) })