test_that("multiJI behaviour", {

  data(simTransComms)
  test.data <- simTransComms$community3[1:25,2:7]
  eg.multiJI1 <- multiJI(data = test.data, winsize = 50)

  expect_equal(dim(eg.multiJI1),c(NROW(test.data) - round(NROW(test.data)*0.5) + 1,2))
  expect_true(all(is.numeric(eg.multiJI1[,2])))
  expect_true(all(eg.multiJI1[,2] >= 0))

  test.data2 <- matrix(nrow = 25,ncol = 5)

  test.data2 <- sapply(1:5, function(x){
    test.data2[,x] <- rnorm(25,mean = 20, sd = 3)
  })
  test.data2 <- cbind(seq(1:25),test.data2)
  test.data2[,2] <- c(rep(0,20),rnorm(5))

  expect_warning(eg.multiJI2 <- multiJI(data = test.data2, winsize = 50))
})
