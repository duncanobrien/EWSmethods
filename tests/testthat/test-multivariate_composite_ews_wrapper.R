test_that("mulitEWS works", {

  data(simTransComms)
  test.data <- simTransComms$community3[1:50,2:7]

  eg.multiEWS1 <- multiEWS(test.data, method = "rolling",
                     winsize = 50,
                     metrics = sample(c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"),size = 3,replace = F))
  eg.multiEWS2 <- multiEWS(test.data, method = "expanding",
                           burn_in = 25,
                           metrics = sample(c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"),size = 3,replace = F))

  expect_error(multiEWS(test.data, method = "rolling",
                        winsize = 50,
                        metrics = "rubbish"))
  expect_error(multiEWS(test.data, method = "expanding",
                        burn_in = 25,
                        metrics = "rubbish"))

  expect_equal(dim(eg.multiEWS1$EWS$raw)[1],NROW(test.data) - round(NROW(test.data)*0.5) + 1)
  expect_equal(dim(eg.multiEWS2$EWS$raw)[1],(NROW(test.data)-25+1)*3)
  expect_true("multiEWS" %in% class(eg.multiEWS1) & "multiEWS" %in% class(eg.multiEWS2) &
                "rollEWS" %in% class(eg.multiEWS1) & "expEWS" %in% class(eg.multiEWS2))

  expect_no_error(plot(eg.multiEWS1))
  expect_no_error(plot(eg.multiEWS2))
})
