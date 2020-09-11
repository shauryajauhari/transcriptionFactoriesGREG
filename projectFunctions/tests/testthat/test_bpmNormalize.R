context("BPM Normalization")

test_that("Result is numeric", {expect_is(bpmNormalize(13.0), "numeric") })
test_that("Invalid input", {expect_error(bpmNormalize("NA")) })