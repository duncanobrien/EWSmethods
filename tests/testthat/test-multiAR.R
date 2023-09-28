test_that("multiAR behaviour", {

  data(simTransComms)
  test.data <- simTransComms$community1[1:25,2:7]
  eg.multiAR1 <- multiAR(data = test.data, winsize = 50, dt = 1)

  expect_equal(dim(eg.multiAR1),c(NROW(test.data) - round(NROW(test.data)*0.5) + 1,2))
  expect_true(all(is.numeric(eg.multiAR1[,2])))
  expect_false(all(is.na(eg.multiAR1[,2])))

})
