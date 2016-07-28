context("readText")

test_that("readText throws an error for a blank path from which data will be drawn",{
  expect_error(readText("", ...), "you must input something for the the path from which data will be taken")
})
