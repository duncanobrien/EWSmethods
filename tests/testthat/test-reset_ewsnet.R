test_that("EWSNet model weights can be downloaded and removed", {
  testthat::skip_if_offline(host = "r-project.org")

  try1 <- tryCatch(ewsnet_reset(remove_weights = FALSE, auto = TRUE, weights_path = tempdir()),
                   error = function(e) conditionMessage(e),
                   warning = function(w) conditionMessage(w),
                   message = function(m) conditionMessage(m))

  if(inherits(try1,"character")){
    skip("Internet failure")
  }
  #target_folder <- paste(c(system.file("python", package = "EWSmethods"),"weights"),collapse = "/")
  expect_length(fs::dir_ls(path = tempdir(), regexp = "*\\.h5$",recurse = T),50)

  ewsnet_reset(remove_weights = TRUE, auto = TRUE, weights_path = tempdir())
  expect_length(fs::dir_ls(path = tempdir(), regexp = "*\\.h5$",recurse = T),0)
  })
