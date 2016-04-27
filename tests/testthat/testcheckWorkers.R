content("checkWorkers.R")

test_that("Proper inputs for checkWorkers", {
  load("data/fitStan_object.RData")
  wrong_fit <- c("a", "b", "c")
  correct_data <- data.frame(matrix(1:126, ncol=7))
  colnames(correct_data) <- c("batch_id", "comparison_id", "document_id", "result", "hit_id", "worker_id", "completed_at")
  wrong_data1 <- data.frame(matrix(1:100, ncol=5))
  wrong_data2 <- data.frame(matrix(1:126, ncol=7))
  colnames(wrong_data2) <- c("a", "b", "c", "d", "e", "f", "g")

  expect_error(checkWorkers(wrong_fit, correct_data), "fit should be a stan object")
  expect_error(checkWorkers(fit, wrong_data1), "data dimension mismatches")
  expect_error(checkWorkers(fit, wrong_data2), "worker_id is not in data")
  expect_error(checkWorkers(fit, correct_data, cut_point="c"), "cut_point should be numeric")
  expect_error(checkWorkers(fit, correct_data, cut_point=1, cut_proportion=1.2), "cut_proportion should be in the range between 0 to 1")
})
