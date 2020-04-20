library(neet)

## Check outputs

test_that("Correct model output format", {
  expect_neet(fit_pres_bg_model(),
              "MaxEnt")
})
