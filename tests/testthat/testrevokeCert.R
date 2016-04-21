context("revokeCert")
# Inputs a radnom worker ID as the constant.
test_that("revokeCert throws an error for non-character inputs",{
  expect_error(revokeCert(3
