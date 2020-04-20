library(neet)

## Check outputs

test_that("Correct output format", {
  expect_neet(regularisedMaxent(),
              "numeric")
})
