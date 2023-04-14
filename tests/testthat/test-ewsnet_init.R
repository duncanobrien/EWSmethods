test_that("EWSNet can be correctly initialised and uninstalled", {
  ewsnet_init("test_env", auto = TRUE)
  expect_match(reticulate::py_config()$python,"test_env")
  expect_true("tensorflow" %in% reticulate::py_list_packages()$package)

  if(interactive()){
  ewsnet_init("test_env", conda_refresh = TRUE, auto = FALSE)
  }else{
    reticulate::miniconda_uninstall()
  }
  conda <- try(reticulate::conda_list())
  expect_true(inherits(conda, "try-error"))
})
