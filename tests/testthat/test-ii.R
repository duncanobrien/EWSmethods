test_that("embedding and information imbalance works", {

  data(simTransComms)
  E <- 2
  N <- NROW(simTransComms$community1[1:25,])
  embed_x <- embed_ts(simTransComms$community1[1:25,2:3], E = E, tau = 1)
  embed_y <- embed_ts(simTransComms$community1[1:25,c(2,6)], E = E, tau = 1)
  expect_equal(dim(embed_x),dim(embed_y))
  expect_true(NROW(embed_x) + NROW(embed_y) ==  (N*2) - (2*E))

  eg_ii <- II(embed_x,embed_y,alpha = 0.5, tau = 1)
  expect_true(is.numeric(eg_ii) & length(eg_ii) == 1)
  expect_error(II(embed_x[,1],embed_y,alpha = 0.5, tau = 1))

  alphas <- seq(0,1,0.2)
  eg_ii2 <- tuneII(embed_x,embed_y,alphas = alphas, tau = 1)
  expect_true(NROW(eg_ii2) == length(alphas) & NCOL(eg_ii2) == 2)

})

