context("givetakeCert")

test_that("givetakeCert throws an error for non-character inputs, blank inputs, and the same certifications",{

  expect_error(givetakeCert(3, "abd", "abc"), "certification must be a set of characters, not just a number")
  expect_error(givetakeCert("abd", 3, "abc"), "certification must be a set of characters, not just a number")
  expect_error(givetakeCert(3, "", "abd"), "you must input something for the certification")
  expect_error(givetakeCert("", 3, "abd"), "you must input something for the certification")
  expect_error(givetakeCert("abd", "abd", "abc"), "the two certifications must be different")
  expect_error(givetakeCert("abc", "abd", 3), "the worker id must be a set of characters, not just a number")
  expect_error(givetakeCert("abc", "abd", ""), "you must input something for the worker id")
})
