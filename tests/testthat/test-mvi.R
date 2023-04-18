test_that("mvi behaviour", {

  data(simTransComms)
  test.data <- simTransComms$community1[1:50,2:7]

  eg.mvi <- mvi(test.data, winsize = 50)
  expect_equal(dim(eg.mvi),c(NROW(test.data) - round(NROW(test.data)*0.5) + 1,2))
  expect_true(all(is.numeric(eg.mvi)))
  expect_true(all(eg.mvi[,2] >= 0))

  })
