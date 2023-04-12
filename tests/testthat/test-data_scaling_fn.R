test_that("scaling works", {
  test_vec <- seq(1:10)
  scaled_vec <- data_scaling(test_vec)
  max_test <- max(scaled_vec)
  min_test <- min(scaled_vec)
  expect_equal(min_test, 1)
  expect_equal(max_test, 2)
})

