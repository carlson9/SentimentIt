context("createBatches")

test_that("createBatches produces batch_ids of correct length",{
  expect_true(length(createBatches(hit_setting_id=2, num_batches=3))==3)
  expect
})
test_that("createBatches throws an error for non-numeric inputs",{
  expect_error(createBatches("g"), "hit_setting_id needs to be a numeric")
  expect_error(batchBatches(TRUE), "hit_setting_id needs to be a numeric")
})