test_that("FI behaviour", {

  data(simTransComms)
  test.data <- simTransComms$community1[1:50,2:7]
  sost1 <- t(apply(test.data[,2:6], MARGIN = 2, FUN = sd))
  eg.fi1 <- FI(data = test.data, winsize = 50,sost = sost1, TL = 90)

  expect_equal(dim(eg.fi1$FI),c( NROW(test.data) - round(NROW(test.data)*0.5) + 1,2))
  expect_true(all(is.numeric(eg.fi1$FI[,2])))
  expect_true(all(eg.fi1$FI[,2] >= 0))

  sost3 <- t(sost1)
  expect_error(FI(data = test.data, winsize = 50,sost = sost2, TL = 90))

  eg.fi2 <- FI(data = test.data, winsize = 50,sost = sost1, TL = 75)

  expect_true(sd(eg.fi2$FI$FI) > sd(eg.fi1$FI$FI))

  })
