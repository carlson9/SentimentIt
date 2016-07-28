context("createHITSBatch")

test_that("createHITSBatch throws an error for non-character certifications, blank inputs, and non-numeric batches",{
  expect_error(createHITSBatch(3, 3, "abc", ...), "certification must be a set of characters, not just a number")
  expect_error(createHITSBatch(3, "Abd", 3, ...), "certification must be a set of characters, not just a number")
  expect_error(createHITSBatch(3, "", "abd", ...), "you must input something for the certification")
  expect_error(createHITSBatch(3, "abd", "", ...), "you must input something for the certification")
  expect_error(createHITSBatch(3, "abd", "abd", ...), "the two certifications must be different")
  expect_error(createHITSBatch("a", "ad", "Ac", ...), "the batch input must be numeric")
  expect_error(createHITSBatch("", "ad", "Ac", ...), "the batch input must be non-blank")
})
