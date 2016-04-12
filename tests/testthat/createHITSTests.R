context("createHITS")

test_that("createHITS throws an error for non-numeric inputs",{
  expect_error(readInData("g"), "All arguments need to be numeric.")
  expect_error(readInData(TRUE), "All arguments need to be numeric.")
})
