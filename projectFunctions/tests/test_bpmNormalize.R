context("BPM Normalization")

test_that("Result is a float", 
          {
            expect_is(bpmNormalize(13.0), "numeric") 
          }) 