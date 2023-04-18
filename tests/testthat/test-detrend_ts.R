test_that("detrending", {
  data(simTransComms)

  test.data <- simTransComms$community1[,2:7]
  eg.detrend1 <- detrend_ts(test.data, method = "linear")
  eg.detrend2 <- detrend_ts(test.data, method = "gaussian")
  eg.detrend3 <- detrend_ts(test.data, method = "loess")

  expect_equal(dim(eg.detrend1),dim(test.data))
  expect_equal(dim(eg.detrend2),dim(test.data))
  expect_equal(dim(eg.detrend3),dim(test.data))

  expect_false(any(is.na(eg.detrend1)))
  expect_false(any(is.na(eg.detrend2)))
  expect_false(any(is.na(eg.detrend3)))

  expect_error(detrend_ts(test.data[,1], method = "linear"))

  })
