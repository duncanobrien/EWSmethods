test_that("EWSNet model weights can be downloaded and removed", {
  ewsnet_reset(remove_weights = FALSE, auto = TRUE)
  target_folder <- paste(c(system.file("python", package = "EWSmethods"),"weights"),collapse = "/")
  expect_length(fs::dir_ls(path = target_folder, regexp = "*\\.h5$",recurse = T),50)

  ewsnet_reset(remove_weights = TRUE, auto = TRUE)
  expect_length(fs::dir_ls(path = target_folder, regexp = "*\\.h5$",recurse = T),0)
  })
