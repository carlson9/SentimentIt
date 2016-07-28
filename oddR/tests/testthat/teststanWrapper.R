content("stanWrapper")

test_that("Proper inputs for fitStanHier", {
  wrong_data <- data.frame(matrix(1:100, ncol=5))
  correct_data <- data.frame(matrix(1:154, ncol=7))
  correct_hier_data <- data.frame(matrix(1:120, ncol=3))
  colnames(correct_hier_data) <- c("col1", "col2", "col3")

  expect_error(stanWrapper(wrong_data, correct_hier_data, "col1"), "data dimension mismatches")

  expect_error(stanWrapper(correct_data, correct_hier_data, "col_wr"),
               "hierarchy_var should be a name of a column in hierarchy_data")
})

test_that("Outout for fitStanHier is class rstan",{
  load("~/SentimentIt/data/fit_stanDataExample.rda")

  expect_true(is.list(stanWrapper(data=ExampleData_hier1, hierarchy_data=ExampleData_hier2, hierarchy_var="countries")))
})
