test_that("uniJI behaviour", {
  data(simTransComms)
  test.data <- simTransComms$community3[1:50,2:7]
  eg.uniJI1 <- uniJI(data = test.data[,1:2], winsize = 50)

  expect_equal(dim(eg.uniJI1),c(NROW(test.data) - round(NROW(test.data)*0.5) + 1,2))
  expect_true(all(is.numeric(eg.uniJI1[,2])))
  expect_true(all(eg.uniJI1[,2] >= 0))

  test.data2 <- data.frame("time" = seq(50),"ts" = c(rep(0,40),rnorm(10)))

  expect_warning(eg.uniJI2 <-uniJI(data = test.data2, winsize = 50))
  expect_true(any(is.na(eg.uniJI2$smap_J)))

  })
