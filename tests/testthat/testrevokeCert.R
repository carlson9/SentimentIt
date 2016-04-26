context("revokeCert")

test_that("revokeCert throws an error for non-character inputs",{
  expect_error(revokeCert(3, "abc"), "certification must be a set of characters, not just a number")
  expect_error(revokeCert(3, ""), "you must input something for the worker id")
  expect_error(revokeCert("", "abd"), "you must input something for the certification")
  expect_error(revokeCert("abc", 3), "the worker id must be a set of characters, not just a number")
})
  
