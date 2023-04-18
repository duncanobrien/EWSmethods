test_that("Permuted rolling EWS works", {

  test.data <- simTransComms$community2[1:25,2:7]

  unregister_dopar <- function(){
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }

  cl <- parallel::makeCluster(2)

  doParallel::registerDoParallel(cl)

  eg.multi1 <- perm_rollEWS(
     data = test.data,
     winsize = 50,
      variate = "multi",
      metrics =  sample(c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"),size = 3,replace = F),
      perm.meth = "red.noise",
      iter = 50)
  base::closeAllConnections()
  unregister_dopar()
  rm(cl)

  expect_warning(eg.uni1 <- perm_rollEWS(
    data = test.data[,1:2],
    winsize = 50,
    variate = "uni",
    metrics =  sample(c("cv", "acf", "ar1", "dr", "rr", "skew","kurt","SD"),size = 3,replace = F),
    perm.meth = "sample",
    iter = 50))

  expect_error(uniEWS(test.data, method = "rolling",
                      winsize = 50,
                      metrics = "rubbish"))
  expect_error(uniEWS(test.data, method = "expanding",
                      burn_in = 25,
                      metrics = "rubbish"))

  expect_equal(dim(eg.multi1$EWS$raw)[1],NROW(test.data) - round(NROW(test.data)*0.5) + 1 )
  expect_equal(dim(eg.uni1$EWS$raw)[1],NROW(test.data) - round(NROW(test.data)*0.5) + 1 )
  expect_true("multiEWS" %in% class(eg.multi1) & "uniEWS" %in% class(eg.uni1) &
                "rollEWS" %in% class(eg.multi1) & "rollEWS" %in% class(eg.uni1))

  expect_no_error(plot(eg.multi1))
  expect_no_error(plot(eg.uni1))

  expect_identical(rownames(eg.uni1$EWS$cor),c("tau","perm_pvalue"))
  expect_identical(rownames(eg.multi1$EWS$cor),c("tau","perm_pvalue"))
})
