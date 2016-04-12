context("batchStatus")

test_that("batchStatus throws an error for non-numeric inputs",{
  expect_error(batchStatus("g"), "batch_id needs to be a vector of numerics")
  expect_error(batchStatus(TRUE), "batch_id needs to be a vector of numerics")
})
test_that("batchStatus throws an error for non-vector inputs", {
  expect_error(batchStatus(matrix(1,2,3,4)), "batch_id needs to be a vector of numerics")
  expect_error(batchStatus(data.frame(1,2,3,4,5,6)), "batch_id needs to be a vector of numerics")
})
test_that("batchStatus puts out a correct dataframe",{
  expect_true(is.data.frame(batchStatus(204)))
  expect_true(ncol(batchStatus(204)) == 5)
  expect_true(nrow(batchStatus(204:205))==2)
})
