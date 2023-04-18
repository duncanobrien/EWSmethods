test_that("deseason behaviour", {

  spp_data <- matrix(nrow = 5*12, ncol = 5)

  seasonal_cycle <-  20*sin(2*pi*(1:5*12)/12)
  spp_data <- sapply(1:dim(spp_data)[2], function(x){

    spp_data[,x] <- ts(rnorm(5*12,mean = 20, sd = 3) + seasonal_cycle, freq = 12, start = c(2000, 1)) #add seasonal cycle to random noise

  })
  test.data <- cbind("time" = base::seq(base::as.Date('2000/01/01'),
                                        base::as.Date('2004/12/01'), by = "month"),
                     as.data.frame(spp_data))

    eg.deseason1 <- deseason_ts(test.data, increment  = "month", method = "average", order = "ymd")
    eg.deseason2 <- deseason_ts(test.data,  increment  = "month",method =  "stl", order = "ymd")
    eg.deseason3 <- deseason_ts(test.data,  increment  = "month", method = "decompose", order = "ymd")

    expect_equal(dim(eg.deseason1),dim(test.data))
    expect_equal(dim(eg.deseason2),dim(test.data))
    expect_equal(dim(eg.deseason3),dim(test.data))

    expect_false(any(is.na(eg.deseason1)))
    expect_false(any(is.na(eg.deseason2)))
    expect_true(all(is.na(head(eg.deseason3[,-1])) & is.na(head(eg.deseason3[,-1])) & !all(is.na(eg.deseason3[10:16,-1]))))

    expect_error(deseason_ts(test.data[,1], method = "average"))
    expect_error(deseason_ts(test.data, increment  = "month", method = "average"))
    expect_error(deseason_ts(test.data, method = "average", order = "ym"))

})
