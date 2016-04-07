context("readInData")

test_that("readInData throws an error for non-numeric inputs",{
  expect_error(readInData("g"), "batchNumbers needs to be a vector of numerics")
  expect_error(readInData(TRUE), "batchNumbers needs to be a vector of numerics")
})
test_that("readInData throws an error for non-vector inputs", {
  expect_error(readInData(matrix(1,2,3,4)), "batchNumbers needs to be a vector of numerics")
  expect_error(readInData(data.frame(1,2,3,4,5,6)), "batchNumbers needs to be a vector of numerics")
})
test_that("readInData puts out a correct dataframe",{
  expect_true(is.data.frame(readInData(204)))
  expect_true(ncol(readInData(204)) == 7)
})
