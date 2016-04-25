context("createPairwise.R")

test_that("Correct inputs for createPairwise",{
  expect_error(createPairwise("c", 1), "ids should be numeric")
  expect_error(createPairwise(1, "c"), "number_per should be numeric")
})
