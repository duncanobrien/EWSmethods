test_that("uniAR behaviour", {

  data(simTransComms)
  test.data <- simTransComms$community1[1:25,c(2,5)]
  eg.uniAR1 <- uniAR(data = test.data, winsize = 50, dt = 1)

  expect_equal(dim(eg.uniAR1),c(NROW(test.data) - round(NROW(test.data)*0.5) + 1,2))
  expect_true(all(is.numeric(eg.uniAR1[,2])))
  expect_false(all(is.na(eg.uniAR1[,2])))

})
