test_that("uniEWS works", {

  data(simTransComms)
  test.data <- simTransComms$community3[1:50,2:3]
  test.data[,"trait"] <- rnorm(50,5,1)

  eg.uniEWS1 <- uniEWS(test.data[,1:2], method = "rolling",
                           winsize = 50,
                           metrics = sample(c("cv", "acf", "ar1", "dr", "rr", "skew","kurt","SD"),size = 3,replace = F))
  eg.uniEWS2 <- uniEWS(test.data, method = "expanding",
                           burn_in = 25,
                          trait = test.data$trait,
                           metrics = c(sample(c("cv", "acf", "ar1", "dr", "rr", "skew","kurt","SD"),size = 2,replace = F),"trait"))

  expect_error(uniEWS(test.data, method = "rolling",
                        winsize = 50,
                        metrics = "rubbish"))
  expect_error(uniEWS(test.data, method = "expanding",
                        burn_in = 25,
                        metrics = "rubbish"))

  expect_equal(dim(eg.uniEWS1$EWS$raw)[1],NROW(test.data) - round(NROW(test.data)*0.5) + 1)
  expect_equal(dim(eg.uniEWS2$EWS)[1],(NROW(test.data)-25+1)*7) #7 selected here rather than three as 7 unique combinations of 3 metrics
  expect_true("uniEWS" %in% class(eg.uniEWS1) & "uniEWS" %in% class(eg.uniEWS2) &
                "rollEWS" %in% class(eg.uniEWS1) & "expEWS" %in% class(eg.uniEWS2))

  expect_no_error(plot(eg.uniEWS1))
  expect_no_error(plot(eg.uniEWS2))
})
