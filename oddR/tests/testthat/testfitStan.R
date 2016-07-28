content("fitStan")

test_that("Proper inputs for fitStan", {
  wrong_data <- data.frame(matrix(1:100, ncol=5))

  expect_error(fitStan(wrong_data), "data dimension mismatches")
})

test_that("Outout for fitStan is class rstan",{
  load("~/SentimentIt/data/fit_stanDataExample.rda")

  expect_true(class(fitStan(ExampleData))=="stanfit")
})
