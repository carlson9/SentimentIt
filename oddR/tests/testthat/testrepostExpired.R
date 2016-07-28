context("repostExpired")

test_that("repostExpired throws an error for non-numeric inputs",{
  expect_error(repostExpired("g"), "batch_id needs to be a vector of numerics")
  expect_error(repostExpired(TRUE), "batch_id needs to be a vector of numerics")
})
test_that("repostExpired throws an error for non-vector inputs", {
  expect_error(repostExpired(matrix(1,2,3,4)), "batch_id needs to be a vector of numerics")
  expect_error(repostExpired(data.frame(1,2,3,4,5,6)), "batch_id needs to be a vector of numerics")
})

