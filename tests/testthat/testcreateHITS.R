context("createHITS")

test_that("createHITS throws an error for non-numeric inputs",{
  expect_error(createHITS("g"), "All arguments need to be numeric.")
  expect_error(createHITS(TRUE), "All arguments need to be numeric.")
})
