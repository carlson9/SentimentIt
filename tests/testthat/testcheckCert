context("checkCert")

test_that("checkCert throws an error for non-character inputs and blank inputs",{
  expect_error(checkCert(3, "abc"), "certification must be a set of characters, not just a number")
  expect_error(checkCert(3, ""), "you must input something for the worker id")
  expect_error(checkCert("", "abd"), "you must input something for the certification")
  expect_error(checkCert("abc", 3), "the worker id must be a set of characters, not just a number")
})
