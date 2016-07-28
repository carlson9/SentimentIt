context("createCert")

test_that("createCert throws an error for non-character inputs and blank inputs",{
  expect_error(createCert(3, "abc"), "certification must be a set of characters, not just a number")
  expect_error(createCert(3, ""), "you must input something for the worker id")
  expect_error(createCert("", "abd"), "you must input something for the certification")
  expect_error(createCert("abc", 3), "the worker id must be a set of characters, not just a number")
})
