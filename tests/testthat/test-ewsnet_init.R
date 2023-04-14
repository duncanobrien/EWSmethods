test_that("EWSNet can be correctly initialised and uninstalled", {
  ewsnet_init("test_env", auto = TRUE)
  expect_match(reticulate::py_config()$python,"test_env")
  expect_true("tensorflow" %in% reticulate::py_list_packages()$package)

  reticulate::miniconda_uninstall()
  #conda <- try(reticulate::conda_list())
  #expect_true(inherits(conda, "try-error"))
  expect_true(!("test_env" %in% reticulate::conda_list()))

})
